# ============================================================================
# Preparación de una sesión de desarrollo
# ============================================================================
#
# Este script carga los componentes necesarios para trabajar con los datos
# durante el desarrollo del paquete.
#
# UTILIZACIÓN
# -----------
#
# Al iniciar una nueva sesión de R/RStudio:
#
#   source("data-raw/setup_dev.R")
#
# Después quedarán disponibles, entre otros:
#
#   - metadata
#   - lista_xlsx
#   - estructura_datasets
#   - cod_ccaa_prov
#   - diccionario_territorial
#
# y las funciones internas del paquete.
#
# IMPORTANTE
# ----------
# Este script es exclusivamente una utilidad para el DESARROLLO.
# No forma parte de la API pública del paquete.
#
# ============================================================================


message("Preparando sesión de desarrollo...")


# ============================================================================
# 1. Metadata
# ============================================================================

source(
  "data-raw/metadata.R"
)


# ============================================================================
# 2. Cargar funciones del paquete
# ============================================================================

devtools::load_all()


# ============================================================================
# 3. Importar Excel originales
# ============================================================================

source(
  "data-raw/import_raw_data.R"
)


# ============================================================================
# 4. Construir estructura de datasets
# ============================================================================

estructura_datasets <- parse_id_dataset(
  metadata$id_dataset
)


# ============================================================================
# 5. Cargar referencia territorial
# ============================================================================

cod_ccaa_prov <- readRDS(
  "data-raw/sources/cod_ccaa_prov.rds"
)


# ============================================================================
# 6. Construir diccionario territorial
# ============================================================================

diccionario_territorial <- crear_diccionario_territorial(
  cod_ccaa_prov
)


# ============================================================================
# 7. Resumen
# ============================================================================

message(
  "Sesión preparada correctamente."
)

message(
  "Datasets cargados: ",
  length(lista_xlsx)
)

message(
  "Datasets en metadata: ",
  nrow(metadata)
)
