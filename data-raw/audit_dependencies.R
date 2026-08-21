# =============================================================================
# Auditoría de dependencias del paquete
# =============================================================================
#
# Este script ayuda a comprobar qué paquetes externos aparecen en el código
# del paquete y cuáles se utilizan únicamente durante el desarrollo.
#
# La distinción es importante:
#
# - los paquetes utilizados por funciones de R/ deben declararse normalmente
#   en Imports (o, en casos concretos, Suggests);
# - los paquetes utilizados únicamente en data-raw/ no necesitan estar
#   disponibles para el usuario final del paquete.
#
# El script busca llamadas explícitas del tipo paquete::funcion y llamadas a
# library(), require() o requireNamespace().
#
# No modifica DESCRIPTION automáticamente. La decisión de añadir o eliminar
# dependencias debe hacerse después de revisar los resultados.
# =============================================================================


# -----------------------------------------------------------------------------
# Función auxiliar
# -----------------------------------------------------------------------------

extraer_dependencias <- function(ruta) {

  archivos <- list.files(
    ruta,
    pattern = "\\.[Rr]$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(archivos) == 0L) {
    return(character(0))
  }

  codigo <- unlist(
    lapply(
      archivos,
      readLines,
      warn = FALSE
    ),
    use.names = FALSE
  )

  # Llamadas explícitas: paquete::funcion o paquete:::funcion
  coincidencias_namespace <- regmatches(
    codigo,
    gregexpr(
      "\\b[A-Za-z][A-Za-z0-9.]*:::{0,1}[A-Za-z][A-Za-z0-9._]*",
      codigo,
      perl = TRUE
    )
  )

  coincidencias_namespace <- unlist(
    coincidencias_namespace,
    use.names = FALSE
  )

  paquetes_namespace <- sub(
    ":::{0,1}.*$",
    "",
    coincidencias_namespace
  )

  # library(paquete), require(paquete) y requireNamespace("paquete")
  patron_carga <- paste0(
    "\\b(?:library|require|requireNamespace)\\s*\\(\\s*",
    "[\"']?([A-Za-z][A-Za-z0-9.]*)[\"']?"
  )

  coincidencias_carga <- regmatches(
    codigo,
    gregexpr(
      patron_carga,
      codigo,
      perl = TRUE
    )
  )

  coincidencias_carga <- unlist(
    coincidencias_carga,
    use.names = FALSE
  )

  paquetes_carga <- sub(
    patron_carga,
    "\\1",
    coincidencias_carga,
    perl = TRUE
  )

  sort(
    unique(
      c(
        paquetes_namespace,
        paquetes_carga
      )
    )
  )
}


# -----------------------------------------------------------------------------
# Dependencias según la zona del proyecto
# -----------------------------------------------------------------------------

dependencias_R <- extraer_dependencias("R")
dependencias_data_raw <- extraer_dependencias("data-raw")
dependencias_tests <- extraer_dependencias("tests")


# -----------------------------------------------------------------------------
# Dependencias declaradas actualmente en DESCRIPTION
# -----------------------------------------------------------------------------

descripcion <- read.dcf("DESCRIPTION")

separar_dependencias <- function(x) {

  if (is.na(x) || !nzchar(x)) {
    return(character(0))
  }

  x <- unlist(
    strsplit(
      x,
      ","
    )
  )

  x <- trimws(x)

  # Elimina restricciones de versión:
  # "testthat (>= 3.0.0)" -> "testthat"
  x <- sub(
    "\\s*\\(.*\\)$",
    "",
    x
  )

  sort(unique(x))
}

imports_declarados <- separar_dependencias(
  descripcion[1, "Imports"]
)

suggests_declarados <- separar_dependencias(
  descripcion[1, "Suggests"]
)


# -----------------------------------------------------------------------------
# Resumen
# -----------------------------------------------------------------------------

cat("\nDEPENDENCIAS DETECTADAS EN R/\n")
cat("--------------------------------\n")
print(dependencias_R)

cat("\nDEPENDENCIAS DETECTADAS EN data-raw/\n")
cat("---------------------------------------\n")
print(dependencias_data_raw)

cat("\nDEPENDENCIAS DETECTADAS EN tests/\n")
cat("------------------------------------\n")
print(dependencias_tests)

cat("\nIMPORTS DECLARADOS\n")
cat("--------------------\n")
print(imports_declarados)

cat("\nSUGGESTS DECLARADOS\n")
cat("---------------------\n")
print(suggests_declarados)


# -----------------------------------------------------------------------------
# Comparaciones orientativas
# -----------------------------------------------------------------------------

cat("\nIMPORTS NO DETECTADOS EXPLÍCITAMENTE EN R/\n")
cat("-------------------------------------------\n")
print(
  setdiff(
    imports_declarados,
    dependencias_R
  )
)

cat("\nPAQUETES DETECTADOS EN R/ PERO NO DECLARADOS EN IMPORTS/SUGGESTS\n")
cat("---------------------------------------------------------------\n")
print(
  setdiff(
    dependencias_R,
    c(imports_declarados, suggests_declarados)
  )
)
