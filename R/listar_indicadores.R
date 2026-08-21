#' Listar indicadores disponibles
#'
#' Devuelve una tabla con los indicadores disponibles en el paquete y su
#' información descriptiva básica.
#'
#' Esta función está pensada como punto de entrada para descubrir qué
#' indicadores pueden consultarse y obtener el `id_dataset` necesario para
#' utilizar otras funciones de la API, como [info_indicador()].
#'
#' @return Un `data.frame` con una fila por indicador y las siguientes columnas:
#'
#' \describe{
#'   \item{id_dataset}{
#'     Identificador único del indicador dentro del paquete. Es la clave que
#'     debe utilizarse en funciones como [info_indicador()] y, posteriormente,
#'     en las funciones de consulta de datos.
#'   }
#'
#'   \item{variable}{
#'     Nombre o descripción de la variable estadística representada por el
#'     indicador.
#'   }
#'
#'   \item{operacion}{
#'     Operación o fuente estadística de la que procede el indicador.
#'   }
#'
#'   \item{organismo}{
#'     Organismo responsable de la información estadística.
#'   }
#'
#'   \item{periodo}{
#'     Período de referencia disponible según la metadata de la fuente.
#'     Para conocer la cobertura temporal calculada a partir de los datos
#'     almacenados puede utilizarse [info_indicador()].
#'   }
#' }
#'
#' @seealso [info_indicador()]
#'
#' @examples
#' listar_indicadores()
#'
#' @export
listar_indicadores <- function() {
  metadata |>
    dplyr::select(
      id_dataset,
      variable,
      operacion,
      organismo,
      periodo
    )
}

