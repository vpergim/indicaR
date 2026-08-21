# ============================================================================
# Construcción de los datos internos del paquete
# ============================================================================

source("data-raw/setup_dev.R")
source("data-raw/build_maps.R")

# ============================================================================
# 1. Construir datasets normalizados
# ============================================================================

datos_indicadores <- lapply(
  names(lista_xlsx),
  function(id_dataset) {
    normalizar_dataset(
      lista_xlsx[[id_dataset]],
      id_dataset = id_dataset
    )
  }
)

names(datos_indicadores) <- names(lista_xlsx)

usethis::use_data(
  datos_indicadores,
  metadata,
  mapa_ccaa,
  mapa_provincias,
  mapa_canarias_linea,
  mapa_canarias_recuadro,
  internal = TRUE,
  overwrite = TRUE
)
