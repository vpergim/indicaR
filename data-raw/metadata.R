# ============================================================================
# Construcción del metadata interno del paquete
# ============================================================================
#
# Este script importa y prepara el archivo _METADATA.xlsx utilizado durante
# la construcción de los datos del paquete.
#
# IMPORTANTE:
# - Este script NO se ejecuta cuando un usuario utiliza el paquete.
# - Solo debe ejecutarlo el desarrollador cuando cambie _METADATA.xlsx.
# - El archivo Excel original no se distribuirá con el paquete.
#
# El objeto resultante, `metadata`, se almacenará como dato interno del
# paquete mediante usethis::use_data(..., internal = TRUE).
#
# ============================================================================


# ----------------------------------------------------------------------------
# 1. Ruta al archivo fuente
# ----------------------------------------------------------------------------

metadata_path <- file.path(
  "../../Dropbox/INDICADORES GVA",
  "_METADATA.xlsx"
)


# ----------------------------------------------------------------------------
# 2. Lectura de la hoja principal
# ----------------------------------------------------------------------------

metadata_full <- readxl::read_xlsx(
  path = metadata_path,
  sheet = "metadata"
)

metadata <- metadata_full %>%
  select(
  c("NOMBRE ARCHIVO",
    "VARIABLE",
    "OPERACIÓN",
    "ORGANISMO",
    "PERIODO",
    "CELDA INICIO",
    "CELDA FIN"
    )
)


# ----------------------------------------------------------------------------
# 3. Normalización de nombres de variables
# ----------------------------------------------------------------------------
#
# Los nombres originales del Excel contienen espacios y mayúsculas.
# Los convertimos a nombres internos consistentes para facilitar su uso
# posterior en las funciones de construcción de datos.
#

metadata <- metadata |>
  dplyr::transmute(
    id_dataset = `NOMBRE ARCHIVO`,
    variable = VARIABLE,
    operacion = `OPERACIÓN`,
    organismo = ORGANISMO,
    periodo = PERIODO,
    celda_inicio = `CELDA INICIO`,
    celda_fin = `CELDA FIN`
  )


# ----------------------------------------------------------------------------
# 4. Comprobaciones mínimas de integridad
# ----------------------------------------------------------------------------

stopifnot(
  !anyNA(metadata$id_dataset),
  !anyDuplicated(metadata$id_dataset)
)


# ----------------------------------------------------------------------------
# 5. Guardado como dato interno del paquete
# ----------------------------------------------------------------------------
#
# internal = TRUE hace que el objeto se almacene en R/sysdata.rda.
#
# Por tanto:
#
#   indicaR::metadata
#
# NO estará disponible para el usuario.
#
# Las funciones internas del paquete sí podrán utilizarlo.
#
#
# usethis::use_data(
#   metadata,
#   internal = TRUE,
#   overwrite = TRUE
# )
