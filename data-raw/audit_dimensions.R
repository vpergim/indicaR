# ============================================================================
# Auditoría de dimensiones descriptivas normalizadas
# ============================================================================
#
# Este script:
#
#   1. normaliza los 90 datasets mediante `normalizar_dataset()`;
#   2. identifica las dimensiones descriptivas existentes;
#   3. recopila los valores distintos observados en cada dimensión;
#   4. permite detectar inconsistencias de nomenclatura entre fuentes.
#
# El script NO modifica los datos fuente.
#
# REQUISITOS
# ----------
# La sesión debe haberse preparado previamente mediante:
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
# 2. Comprobación básica
# ============================================================================

stopifnot(
  length(lista_normalizada) == length(lista_xlsx),
  identical(
    names(lista_normalizada),
    names(lista_xlsx)
  )
)


# ============================================================================
# 3. Dimensiones no estructurales
# ============================================================================
#
# Excluimos:
#
#   - metadata del indicador;
#   - variables territoriales;
#   - variables temporales;
#   - valor.
#
# El resto se consideran dimensiones descriptivas candidatas a auditoría.
#

columnas_no_dimension <- c(
  "id_dataset",
  "variable",
  "operacion",
  "organismo",
  "frecuencia",
  "territorio",
  "tipo_territorio",
  "cod_ccaa",
  "ccaa",
  "cod_provincia",
  "provincia",
  "periodo",
  "anyo",
  "trimestre",
  "mes",
  "valor"
)


dimensiones_observadas <- sort(
  unique(
    unlist(
      lapply(
        lista_normalizada,
        function(x) {
          setdiff(
            names(x),
            columnas_no_dimension
          )
        }
      )
    )
  )
)

dimensiones_observadas


# ============================================================================
# 4. Obtener valores distintos por dimensión
# ============================================================================

valores_por_dimension <- lapply(
  dimensiones_observadas,
  function(dimension) {

    datasets_con_dimension <- names(
      Filter(
        function(x) {
          dimension %in% names(x)
        },
        lista_normalizada
      )
    )

    valores <- sort(
      unique(
        unlist(
          lapply(
            lista_normalizada[datasets_con_dimension],
            function(x) {
              as.character(
                x[[dimension]]
              )
            }
          )
        )
      )
    )

    data.frame(
      dimension = dimension,
      valor = valores,
      stringsAsFactors = FALSE
    )
  }
)

valores_por_dimension <- do.call(
  rbind,
  valores_por_dimension
)

rownames(valores_por_dimension) <- NULL


# ============================================================================
# 5. Número de valores distintos por dimensión
# ============================================================================

resumen_dimensiones <- aggregate(
  valor ~ dimension,
  data = valores_por_dimension,
  FUN = length
)

names(resumen_dimensiones)[
  names(resumen_dimensiones) == "valor"
] <- "n_valores"

resumen_dimensiones <- resumen_dimensiones[
  order(
    resumen_dimensiones$dimension
  ),
]

rownames(resumen_dimensiones) <- NULL
