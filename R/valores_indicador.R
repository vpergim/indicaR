#' Valores disponibles de una dimensión
#'
#' Devuelve los valores disponibles para una columna concreta de un indicador.
#'
#' Esta función permite conocer qué valores pueden utilizarse como filtros en
#' [consultar_indicador()].
#'
#' @param id_dataset Cadena de texto con el identificador del indicador.
#'   Los identificadores disponibles pueden consultarse con
#'   [listar_indicadores()].
#'
#' @param dimension Cadena de texto con el nombre de la columna que se desea
#'   inspeccionar. Las dimensiones disponibles pueden consultarse con
#'   [info_indicador()].
#'
#' @param buscar Opcionalmente, cadena de texto utilizada para buscar entre
#'   los valores disponibles de una dimensión de tipo texto. La búsqueda no
#'   distingue entre mayúsculas y minúsculas y se realiza de forma literal.
#'
#' @return Un vector con los valores distintos disponibles en la dimensión
#'   solicitada.
#'
#' @details
#' La función valida explícitamente tanto el identificador del indicador como
#' el nombre de la dimensión. Si alguno no existe, genera un error.
#'
#' Los valores se devuelven tal como están almacenados en los datos
#' normalizados del paquete.
#'
#' En dimensiones con muchos valores, `buscar` permite localizar categorías
#' sin truncar la lista original de valores disponibles.
#'
#' @examples
#' valores_indicador(
#'   "TERRITORIO_SEXO_EDAD_A_01",
#'   "sexo"
#' )
#'
#' valores_indicador(
#'   "TERRITORIO_SEXO_EDAD_A_01",
#'   "tipo_territorio"
#' )
#'
#' valores_indicador(
#'   "TERRITORIO_SEXO_EDAD_A_01",
#'   "territorio",
#'   buscar = "val"
#' )
#'
#' @seealso
#' [listar_indicadores()],
#' [info_indicador()],
#' [consultar_indicador()]
#'
#' @export
valores_indicador <- function(id_dataset, dimension, buscar = NULL) {

  # --------------------------------------------------------------------------
  # Validar identificador
  # --------------------------------------------------------------------------

  if (
    !is.character(id_dataset) ||
    length(id_dataset) != 1L ||
    is.na(id_dataset) ||
    !nzchar(id_dataset)
  ) {
    stop(
      "`id_dataset` debe ser una cadena de texto no vacía de longitud 1.",
      call. = FALSE
    )
  }

  if (!id_dataset %in% names(datos_indicadores)) {
    stop(
      sprintf(
        "No existe ningún indicador con id_dataset = '%s'.",
        id_dataset
      ),
      call. = FALSE
    )
  }

  # --------------------------------------------------------------------------
  # Validar dimensión
  # --------------------------------------------------------------------------

  if (
    !is.character(dimension) ||
    length(dimension) != 1L ||
    is.na(dimension) ||
    !nzchar(dimension)
  ) {
    stop(
      "`dimension` debe ser una cadena de texto no vacía de longitud 1.",
      call. = FALSE
    )
  }

  datos <- datos_indicadores[[id_dataset]]

  if (!dimension %in% names(datos)) {
    stop(
      sprintf(
        "La dimensión '%s' no está disponible para '%s'.",
        dimension,
        id_dataset
      ),
      call. = FALSE
    )
  }

  # --------------------------------------------------------------------------
  # Validar búsqueda
  # --------------------------------------------------------------------------

  if (
    !is.null(buscar) &&
    (
      !is.character(buscar) ||
      length(buscar) != 1L ||
      is.na(buscar) ||
      !nzchar(buscar)
    )
  ) {
    stop(
      "`buscar` debe ser NULL o una cadena de texto no vacía de longitud 1.",
      call. = FALSE
    )
  }

  # --------------------------------------------------------------------------
  # Obtener valores disponibles
  # --------------------------------------------------------------------------

  valores <- unique(
    datos[[dimension]][!is.na(datos[[dimension]])]
  )

  valores <- sort(valores)

  if (!is.null(buscar)) {

    if (!is.character(valores)) {
      stop(
        sprintf(
          "`buscar` solo puede utilizarse con dimensiones de tipo texto; '%s' no lo es.",
          dimension
        ),
        call. = FALSE
      )
    }

    valores <- valores[
      grepl(
        tolower(buscar),
        tolower(valores),
        fixed = TRUE
      )
    ]
  }

  valores
}
