# ============================================================================
# Importación de los datos fuente
# ============================================================================
#
# Este script lee los ficheros Excel originales utilizados para construir
# los datos internos del paquete.
#
# Los nombres de los ficheros y los rangos que deben importarse se obtienen
# del objeto interno `metadata`.
#
# IMPORTANTE
# ----------
# Este script forma parte del proceso de construcción de los datos y NO se
# ejecuta cuando un usuario utiliza el paquete.
#
# Los ficheros Excel originales se encuentran fuera del proyecto y no se
# distribuyen con el paquete.
#
# El resultado de este script es:
#
#   lista_xlsx
#
# una lista nombrada en la que cada elemento contiene uno de los datasets
# originales importados desde Excel.
#
# ============================================================================


# ----------------------------------------------------------------------------
# 1. Directorio que contiene los Excel originales
# ----------------------------------------------------------------------------
#
# Esta es una ruta LOCAL de desarrollo.
#
# Si en el futuro el proyecto se construye desde otro ordenador, esta ruta
# deberá modificarse o parametrizarse.
#

path_raw <- "../../Dropbox/INDICADORES GVA"


# ----------------------------------------------------------------------------
# 2. Comprobar que existe el directorio de datos
# ----------------------------------------------------------------------------

if (!dir.exists(path_raw)) {
  stop(
    paste0(
      "No se encuentra el directorio de datos fuente:\n",
      normalizePath(
        path_raw,
        winslash = "/",
        mustWork = FALSE
      )
    ),
    call. = FALSE
  )
}


# ----------------------------------------------------------------------------
# 3. Construir las rutas y rangos de lectura
# ----------------------------------------------------------------------------
#
# `metadata` contiene una fila por dataset.
#
# Para cada uno necesitamos:
#
#   - id_dataset: identificador único;
#   - archivo: ruta al Excel;
#   - rango: rango de celdas que contiene los datos.
#

info_archivos <- data.frame(
  id_dataset = metadata$id_dataset,
  archivo = file.path(
    path_raw,
    paste0(metadata$id_dataset, ".xlsx")
  ),
  rango = paste0(
    metadata$celda_inicio,
    ":",
    metadata$celda_fin
  ),
  stringsAsFactors = FALSE
)


# ----------------------------------------------------------------------------
# 4. Comprobar que existen todos los ficheros
# ----------------------------------------------------------------------------

archivos_no_encontrados <- info_archivos$archivo[
  !file.exists(info_archivos$archivo)
]

if (length(archivos_no_encontrados) > 0L) {

  stop(
    paste(
      c(
        "No se han encontrado los siguientes ficheros:",
        archivos_no_encontrados
      ),
      collapse = "\n"
    ),
    call. = FALSE
  )
}


# ----------------------------------------------------------------------------
# 5. Leer los Excel
# ----------------------------------------------------------------------------
#
# Utilizamos lapply() porque queremos generar una lista con un elemento
# por dataset.
#
# Los nombres de la lista serán los identificadores de `metadata`.
#

lista_xlsx <- lapply(
  seq_len(nrow(info_archivos)),
  function(i) {

    id <- info_archivos$id_dataset[i]
    archivo <- info_archivos$archivo[i]
    rango <- info_archivos$rango[i]

    message(
      "Leyendo: ",
      id,
      " | rango: ",
      rango
    )

    readxl::read_xlsx(
      path = archivo,
      range = rango
    )
  }
)

names(lista_xlsx) <- info_archivos$id_dataset


# ----------------------------------------------------------------------------
# 6. Comprobaciones de integridad
# ----------------------------------------------------------------------------

stopifnot(
  length(lista_xlsx) == nrow(metadata),
  identical(
    names(lista_xlsx),
    metadata$id_dataset
  )
)


# ----------------------------------------------------------------------------
# 7. Resumen
# ----------------------------------------------------------------------------

message(
  "Importación finalizada: ",
  length(lista_xlsx),
  " datasets leídos."
)
