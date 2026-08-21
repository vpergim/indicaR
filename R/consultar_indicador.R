#' Consultar un indicador
#'
#' Devuelve los datos de un indicador y permite filtrarlos por cualquiera
#' de las dimensiones disponibles en el dataset normalizado.
#'
#' Los filtros se especifican mediante argumentos con nombre. El nombre debe
#' coincidir con una columna disponible en el indicador y los valores
#' solicitados deben existir en dicha columna.
#'
#' @param id_dataset Cadena de texto con el identificador del indicador.
#'   Los identificadores disponibles pueden consultarse con
#'   [listar_indicadores()].
#'
#' @param ... Filtros aplicados a las columnas del indicador. Cada filtro debe
#'   indicarse como `columna = valor`. Se pueden proporcionar varios valores
#'   mediante un vector, por ejemplo `anyo = c(2023, 2024)`.
#'
#' @return Un `data.frame` con las observaciones que cumplen los filtros.
#'
#' @details
#' Los nombres y valores de los filtros se validan explícitamente. Si se
#' solicita una columna inexistente o un valor que no está presente en el
#' indicador, la función genera un error en lugar de ignorarlo.
#'
#' Cuando, después de aplicar los filtros solicitados, siguen presentes varios
#' tipos de territorio, debe indicarse explícitamente `tipo_territorio`.
#' Esto evita mezclar inadvertidamente niveles territoriales distintos.
#'
#' Las dimensiones disponibles para cada indicador pueden consultarse con
#' [info_indicador()].
#'
#' @examples
#' consultar_indicador(
#'   "TERRITORIO_SEXO_EDAD_A_01",
#'   anyo = 2024,
#'   sexo = "Mujeres",
#'   tipo_territorio = "ccaa"
#' )
#'
#' @seealso [listar_indicadores()], [info_indicador()]
#'
#' @export
consultar_indicador <- function(id_dataset, ...) {

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

  datos <- datos_indicadores[[id_dataset]]
  filtros <- list(...)


  # --------------------------------------------------------------------------
  # Validar nombres de filtros
  # --------------------------------------------------------------------------

  if (length(filtros) > 0L) {

    nombres_filtros <- names(filtros)

    if (
      is.null(nombres_filtros) ||
      any(is.na(nombres_filtros)) ||
      any(!nzchar(nombres_filtros))
    ) {
      stop(
        "Todos los filtros deben proporcionarse con nombre.",
        call. = FALSE
      )
    }

    if (anyDuplicated(nombres_filtros)) {
      stop(
        "No puede especificarse dos veces el mismo filtro.",
        call. = FALSE
      )
    }

    filtros_desconocidos <- setdiff(
      nombres_filtros,
      names(datos)
    )

    if (length(filtros_desconocidos) > 0L) {

      columnas_disponibles <- setdiff(
        names(datos),
        c(
          "id_dataset",
          "variable",
          "operacion",
          "organismo",
          "frecuencia",
          "valor"
        )
      )

      stop(
        sprintf(
          paste0(
            "Filtros no disponibles para '%s': %s.\n",
            "Columnas disponibles para filtrar:\n- %s"
          ),
          id_dataset,
          paste(filtros_desconocidos, collapse = ", "),
          paste(columnas_disponibles, collapse = "\n- ")
        ),
        call. = FALSE
      )
    }


    # ------------------------------------------------------------------------
    # Validar valores solicitados
    # ------------------------------------------------------------------------

    for (nombre in nombres_filtros) {

      valores <- filtros[[nombre]]

      if (is.null(valores) || length(valores) == 0L) {
        stop(
          sprintf(
            "El filtro '%s' no puede estar vacío.",
            nombre
          ),
          call. = FALSE
        )
      }

      disponibles <- unique(datos[[nombre]])

      no_validos <- is.na(
        match(valores, disponibles)
      )

      if (any(no_validos)) {

        valores_no_validos <- valores[no_validos]

        stop(
          sprintf(
            "Valores no disponibles para el filtro '%s': %s.",
            nombre,
            paste(
              as.character(valores_no_validos),
              collapse = ", "
            )
          ),
          call. = FALSE
        )
      }
    }


    # ------------------------------------------------------------------------
    # Aplicar filtros
    # ------------------------------------------------------------------------

    conservar <- rep(TRUE, nrow(datos))

    for (nombre in nombres_filtros) {
      conservar <- conservar &
        datos[[nombre]] %in% filtros[[nombre]]
    }

    datos <- datos[conservar, , drop = FALSE]
  }


  # --------------------------------------------------------------------------
  # Comprobar resultado
  # --------------------------------------------------------------------------

  if (nrow(datos) == 0L) {
    stop(
      "La combinación de filtros solicitada no devuelve observaciones.",
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Evitar mezcla accidental de niveles territoriales
  # --------------------------------------------------------------------------

  if ("tipo_territorio" %in% names(datos)) {

    tipos_presentes <- unique(
      datos$tipo_territorio[!is.na(datos$tipo_territorio)]
    )

    if (length(tipos_presentes) > 1L) {
      stop(
        sprintf(
          paste0(
            "La consulta contiene varios tipos de territorio: %s. ",
            "Especifique `tipo_territorio` explícitamente."
          ),
          paste(tipos_presentes, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }


  datos
}
