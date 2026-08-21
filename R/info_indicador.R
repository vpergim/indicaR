#' Información de un indicador
#'
#' Muestra información descriptiva y estructural de un indicador disponible
#' en el paquete. Es útil para conocer su contenido antes de consultar los datos.
#'
#' @param id_dataset Cadena de texto con el identificador del indicador.
#'   Los identificadores disponibles pueden consultarse con
#'   [listar_indicadores()].
#'
#' @return Una lista con los siguientes elementos:
#'
#' \describe{
#'   \item{metadata}{
#'     Tabla de una fila con la información descriptiva del indicador,
#'     incluyendo su variable, operación estadística, organismo, período
#'     disponible y, cuando existe, comentarios y URL de la fuente.
#'   }
#'
#'   \item{n_observaciones}{
#'     Número total de observaciones almacenadas para el indicador.
#'   }
#'
#'   \item{cobertura}{
#'     Lista con el primer año disponible (`anyo_min`), el último año
#'     disponible (`anyo_max`) y la frecuencia temporal (`frecuencia`). Los
#'     valores de `frecuencia` son A - anual, T - trimestral y M - mensual.
#'   }
#'
#'   \item{dimensiones}{
#'     Tabla con las columnas disponibles además del núcleo común del
#'     indicador. Incluye el nombre de cada dimensión y el número de valores
#'     distintos disponibles en ella.
#'
#'     Puede incluir tanto dimensiones originales como variables derivadas
#'     creadas durante la normalización.
#'   }
#' }
#'
#' @seealso [listar_indicadores()]
#'
#' @examples
#' listar_indicadores()
#'
#' info_indicador("TERRITORIO_SEXO_EDAD_A_01")
#'
#' @export
info_indicador <- function(id_dataset) {

  # --------------------------------------------------------------------------
  # Validación del identificador
  # --------------------------------------------------------------------------

  if (!is.character(id_dataset) || length(id_dataset) != 1L || is.na(id_dataset)) {
    stop(
      "`id_dataset` debe ser una cadena de texto de longitud 1.",
      call. = FALSE
    )
  }

  if (!id_dataset %in% metadata$id_dataset) {
    stop(
      sprintf("No existe ningún indicador con id_dataset = '%s'.", id_dataset),
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Recuperar metadata y datos
  # --------------------------------------------------------------------------

  meta <- metadata[metadata$id_dataset == id_dataset, , drop = FALSE]

  datos <- datos_indicadores[[id_dataset]]


  # --------------------------------------------------------------------------
  # Identificar dimensiones
  # --------------------------------------------------------------------------

  columnas_nucleo <- c(
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

  dimensiones <- setdiff(
    names(datos),
    columnas_nucleo
  )

  resumen_dimensiones <- data.frame(
    dimension = dimensiones,
    n_valores = vapply(
      dimensiones,
      function(x) {
        length(unique(datos[[x]][!is.na(datos[[x]])]))
      },
      integer(1)
    ),
    row.names = NULL
  )


  # --------------------------------------------------------------------------
  # Resultado
  # --------------------------------------------------------------------------

  list(
    metadata = meta |>
      dplyr::select(
        id_dataset,
        variable,
        comentarios,
        operacion,
        organismo,
        periodo,
        url
      ),

    n_observaciones = nrow(datos),

    cobertura = list(
      anyo_min = if (all(is.na(datos$anyo))) NA_integer_ else min(datos$anyo, na.rm = TRUE),
      anyo_max = if (all(is.na(datos$anyo))) NA_integer_ else max(datos$anyo, na.rm = TRUE),
      frecuencia = unique(datos$frecuencia)
    ),

    dimensiones = resumen_dimensiones
  )
}
