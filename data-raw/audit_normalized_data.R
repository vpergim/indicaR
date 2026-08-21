# ============================================================================
# Auditoría global de los datasets normalizados
# ============================================================================
#
# Este script comprueba que todos los datasets fuente pueden transformarse
# correctamente mediante `normalizar_dataset()` y que el resultado cumple
# el contrato de datos definido para el paquete.
#
# OBJETIVOS
# ---------
#
#   1. Normalizar todos los datasets disponibles.
#
#   2. Comprobar que ninguno produce errores.
#
#   3. Validar las variables temporales:
#
#        - periodo
#        - anyo
#        - trimestre
#        - mes
#
#   4. Validar que `valor` sea siempre numeric.
#
#   5. Comprobar la coherencia de las variables territoriales.
#
#   6. Comprobar las columnas derivadas de dimensiones como:
#
#        - edad_min
#        - edad_max
#        - edad_tipo
#        - niveleducativo_4
#        - tamanyomuni_min
#        - tamanyomuni_max
#        - prod_codigo
#
#   7. Detectar posibles filas completamente duplicadas.
#
# Este script NO modifica los Excel ni los datasets normalizados.
#
# REQUISITOS
# ----------
#
# La forma recomendada de preparar previamente la sesión es:
#
#   source("data-raw/setup_dev.R")
#
# ============================================================================


# ============================================================================
# 1. Normalizar todos los datasets
# ============================================================================

lista_normalizada <- lapply(
  names(lista_xlsx),
  function(id) {

    message(
      "Normalizando: ",
      id
    )

    normalizar_dataset(
      id_dataset = id,
      datos = lista_xlsx[[id]]
    )
  }
)

names(lista_normalizada) <- names(lista_xlsx)


# ============================================================================
# 2. Comprobaciones generales
# ============================================================================

stopifnot(
  length(lista_normalizada) == length(lista_xlsx),
  identical(
    names(lista_normalizada),
    names(lista_xlsx)
  )
)


# ============================================================================
# 3. Resumen estructural de los datasets normalizados
# ============================================================================

resumen_normalizacion <- data.frame(
  id_dataset = names(lista_normalizada),

  n_filas = vapply(
    lista_normalizada,
    nrow,
    integer(1)
  ),

  n_columnas = vapply(
    lista_normalizada,
    ncol,
    integer(1)
  ),

  stringsAsFactors = FALSE
)


# ============================================================================
# 4. Validación de columnas obligatorias
# ============================================================================
#
# Estas columnas deben existir en TODOS los datasets normalizados.
#

columnas_obligatorias <- c(
  "id_dataset",
  "variable",
  "operacion",
  "organismo",
  "frecuencia",
  "periodo",
  "anyo",
  "trimestre",
  "mes",
  "valor"
)


columnas_obligatorias_faltantes <- do.call(
  rbind,
  lapply(
    names(lista_normalizada),
    function(id) {

      faltantes <- setdiff(
        columnas_obligatorias,
        names(lista_normalizada[[id]])
      )

      if (length(faltantes) == 0L) {
        return(NULL)
      }

      data.frame(
        id_dataset = id,
        columna_faltante = faltantes,
        stringsAsFactors = FALSE
      )
    }
  )
)


# ============================================================================
# 5. Validación de tipos de datos
# ============================================================================

auditoria_tipos <- do.call(
  rbind,
  lapply(
    names(lista_normalizada),
    function(id) {

      x <- lista_normalizada[[id]]

      data.frame(
        id_dataset = id,

        id_dataset_character =
          is.character(x$id_dataset),

        frecuencia_character =
          is.character(x$frecuencia),

        periodo_character =
          is.character(x$periodo),

        anyo_integer =
          is.integer(x$anyo),

        trimestre_integer =
          is.integer(x$trimestre),

        mes_integer =
          is.integer(x$mes),

        valor_numeric =
          is.numeric(x$valor),

        stringsAsFactors = FALSE
      )
    }
  )
)

rownames(auditoria_tipos) <- NULL


tipos_invalidos <- subset(
  auditoria_tipos,
  !id_dataset_character |
    !frecuencia_character |
    !periodo_character |
    !anyo_integer |
    !trimestre_integer |
    !mes_integer |
    !valor_numeric
)


# ============================================================================
# 6. Validación de frecuencia y periodo
# ============================================================================

auditoria_frecuencia <- do.call(
  rbind,
  lapply(
    names(lista_normalizada),
    function(id) {

      x <- lista_normalizada[[id]]

      frecuencia <- unique(
        x$frecuencia
      )

      if (length(frecuencia) != 1L) {

        return(
          data.frame(
            id_dataset = id,
            problema = "Más de una frecuencia en el dataset",
            stringsAsFactors = FALSE
          )
        )
      }


      problema <- NULL


      # ----------------------------------------------------------------------
      # Frecuencia anual
      # ----------------------------------------------------------------------

      if (frecuencia == "A") {

        if (!all(grepl("^[0-9]{4}$", x$periodo))) {
          problema <- "Formato anual inválido"
        }

        if (!all(is.na(x$trimestre))) {
          problema <- "Dataset anual con trimestre informado"
        }

        if (!all(is.na(x$mes))) {
          problema <- "Dataset anual con mes informado"
        }
      }


      # ----------------------------------------------------------------------
      # Frecuencia trimestral
      # ----------------------------------------------------------------------

      if (frecuencia == "T") {

        if (!all(grepl("^[0-9]{4}T[1-4]$", x$periodo))) {
          problema <- "Formato trimestral inválido"
        }

        if (!all(x$trimestre %in% 1:4)) {
          problema <- "Trimestre fuera del rango 1-4"
        }

        if (!all(is.na(x$mes))) {
          problema <- "Dataset trimestral con mes informado"
        }
      }


      # ----------------------------------------------------------------------
      # Frecuencia mensual
      # ----------------------------------------------------------------------

      if (frecuencia == "M") {

        if (!all(
          grepl(
            "^[0-9]{4}M(0[1-9]|1[0-2])$",
            x$periodo
          )
        )) {
          problema <- "Formato mensual inválido"
        }

        if (!all(x$mes %in% 1:12)) {
          problema <- "Mes fuera del rango 1-12"
        }

        if (!all(is.na(x$trimestre))) {
          problema <- "Dataset mensual con trimestre informado"
        }
      }


      if (is.null(problema)) {
        return(NULL)
      }


      data.frame(
        id_dataset = id,
        problema = problema,
        stringsAsFactors = FALSE
      )
    }
  )
)


# ============================================================================
# 7. Validación de variables territoriales
# ============================================================================

auditoria_territorial_final <- do.call(
  rbind,
  lapply(
    names(lista_normalizada),
    function(id) {

      x <- lista_normalizada[[id]]

      if (!"tipo_territorio" %in% names(x)) {
        return(NULL)
      }

      tipos <- sort(
        unique(
          x$tipo_territorio[
            !is.na(x$tipo_territorio)
          ]
        )
      )

      data.frame(
        id_dataset = id,
        tipo_territorio = tipos,
        stringsAsFactors = FALSE
      )
    }
  )
)


tipos_territoriales_validos <- c(
  "nacional",
  "ccaa",
  "provincia",
  "ccaa_provincia",
  "agregado",
  "especial"
)


tipos_territoriales_invalidos <- subset(
  auditoria_territorial_final,
  !tipo_territorio %in% tipos_territoriales_validos
)


# ============================================================================
# 8. Validación de edad
# ============================================================================

auditoria_edad <- do.call(
  rbind,
  lapply(
    names(lista_normalizada),
    function(id) {

      x <- lista_normalizada[[id]]

      if (!"edad" %in% names(x)) {
        return(NULL)
      }

      necesarias <- c(
        "edad_min",
        "edad_max",
        "edad_tipo"
      )

      faltantes <- setdiff(
        necesarias,
        names(x)
      )

      if (length(faltantes) > 0L) {

        return(
          data.frame(
            id_dataset = id,
            problema = paste0(
              "Faltan columnas de edad: ",
              paste(faltantes, collapse = ", ")
            ),
            stringsAsFactors = FALSE
          )
        )
      }


      intervalo_invalido <- !is.na(x$edad_min) &
        !is.na(x$edad_max) &
        x$edad_min > x$edad_max


      if (any(intervalo_invalido)) {

        return(
          data.frame(
            id_dataset = id,
            problema = "Intervalo de edad inválido",
            stringsAsFactors = FALSE
          )
        )
      }

      NULL
    }
  )
)


# ============================================================================
# 9. Validación de nivel educativo
# ============================================================================

auditoria_niveleducativo <- do.call(
  rbind,
  lapply(
    names(lista_normalizada),
    function(id) {

      x <- lista_normalizada[[id]]

      if (!"niveleducativo" %in% names(x)) {
        return(NULL)
      }

      if (!"niveleducativo_4" %in% names(x)) {

        return(
          data.frame(
            id_dataset = id,
            problema = "Falta `niveleducativo_4`",
            stringsAsFactors = FALSE
          )
        )
      }

      if (
        any(
          !is.na(x$niveleducativo) &
          is.na(x$niveleducativo_4)
        )
      ) {

        return(
          data.frame(
            id_dataset = id,
            problema = "Nivel educativo sin grupo común de 4 categorías",
            stringsAsFactors = FALSE
          )
        )
      }

      NULL
    }
  )
)


# ============================================================================
# 10. Validación de tamaño municipal
# ============================================================================

auditoria_tamanyomuni <- do.call(
  rbind,
  lapply(
    names(lista_normalizada),
    function(id) {

      x <- lista_normalizada[[id]]

      if (!"tamanyomuni" %in% names(x)) {
        return(NULL)
      }

      necesarias <- c(
        "tamanyomuni_min",
        "tamanyomuni_max"
      )

      faltantes <- setdiff(
        necesarias,
        names(x)
      )

      if (length(faltantes) > 0L) {

        return(
          data.frame(
            id_dataset = id,
            problema = paste0(
              "Faltan columnas de tamaño municipal: ",
              paste(faltantes, collapse = ", ")
            ),
            stringsAsFactors = FALSE
          )
        )
      }


      intervalo_invalido <- !is.na(x$tamanyomuni_min) &
        !is.na(x$tamanyomuni_max) &
        x$tamanyomuni_min > x$tamanyomuni_max


      if (any(intervalo_invalido)) {

        return(
          data.frame(
            id_dataset = id,
            problema = "Intervalo de tamaño municipal inválido",
            stringsAsFactors = FALSE
          )
        )
      }

      NULL
    }
  )
)


# ============================================================================
# 11. Validación de códigos de producto
# ============================================================================

auditoria_prod <- do.call(
  rbind,
  lapply(
    names(lista_normalizada),
    function(id) {

      x <- lista_normalizada[[id]]

      if (!"prod" %in% names(x)) {
        return(NULL)
      }

      if (!"prod_codigo" %in% names(x)) {

        return(
          data.frame(
            id_dataset = id,
            problema = "Falta `prod_codigo`",
            stringsAsFactors = FALSE
          )
        )
      }

      if (
        any(
          !is.na(x$prod_codigo) &
          !x$prod_codigo %in% 1:9
        )
      ) {

        return(
          data.frame(
            id_dataset = id,
            problema = "`prod_codigo` fuera del rango 1-9",
            stringsAsFactors = FALSE
          )
        )
      }

      NULL
    }
  )
)


# ============================================================================
# 12. Filas completamente duplicadas
# ============================================================================
#
# Una fila idéntica en todas sus columnas puede indicar una duplicación
# introducida durante la importación o transformación.
#
# No consideramos aquí duplicaciones conceptuales parciales: únicamente filas
# completamente idénticas.
#

duplicados_normalizados <- do.call(
  rbind,
  lapply(
    names(lista_normalizada),
    function(id) {

      x <- lista_normalizada[[id]]

      dup <- duplicated(x)

      if (!any(dup)) {
        return(NULL)
      }

      data.frame(
        id_dataset = id,
        n_duplicados = sum(dup),
        stringsAsFactors = FALSE
      )
    }
  )
)


# ============================================================================
# 13. Resumen final
# ============================================================================

message("")
message("==============================================")
message("AUDITORÍA GLOBAL DE NORMALIZACIÓN")
message("==============================================")

message(
  "Datasets normalizados: ",
  length(lista_normalizada)
)

message(
  "Columnas obligatorias faltantes: ",
  if (is.null(columnas_obligatorias_faltantes)) {
    0L
  } else {
    nrow(columnas_obligatorias_faltantes)
  }
)

message(
  "Datasets con tipos incorrectos: ",
  nrow(tipos_invalidos)
)

message(
  "Problemas de frecuencia/periodo: ",
  if (is.null(auditoria_frecuencia)) {
    0L
  } else {
    nrow(auditoria_frecuencia)
  }
)

message(
  "Tipos territoriales inválidos: ",
  nrow(tipos_territoriales_invalidos)
)

message(
  "Problemas en edad: ",
  if (is.null(auditoria_edad)) {
    0L
  } else {
    nrow(auditoria_edad)
  }
)

message(
  "Problemas en nivel educativo: ",
  if (is.null(auditoria_niveleducativo)) {
    0L
  } else {
    nrow(auditoria_niveleducativo)
  }
)

message(
  "Problemas en tamaño municipal: ",
  if (is.null(auditoria_tamanyomuni)) {
    0L
  } else {
    nrow(auditoria_tamanyomuni)
  }
)

message(
  "Problemas en producto: ",
  if (is.null(auditoria_prod)) {
    0L
  } else {
    nrow(auditoria_prod)
  }
)

message(
  "Datasets con filas duplicadas: ",
  if (is.null(duplicados_normalizados)) {
    0L
  } else {
    nrow(duplicados_normalizados)
  }
)

message("==============================================")
