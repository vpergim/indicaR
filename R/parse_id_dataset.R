#' Descomponer el identificador de un dataset
#'
#' Interpreta los identificadores utilizados para nombrar los ficheros
#' fuente del proyecto.
#'
#' Los identificadores siguen, con carácter general, la estructura:
#'
#' \preformatted{
#' AMBITO_DIMENSION1_DIMENSION2_..._FRECUENCIA_NUMERO
#' }
#'
#' Por ejemplo:
#'
#' \preformatted{
#' TERRITORIO_SEXO_EDAD_T_01
#' }
#'
#' se interpreta como:
#'
#' - ámbito territorial: `TERRITORIO`
#' - dimensiones adicionales: `SEXO`, `EDAD`
#' - frecuencia: `T`
#' - número de variable: `01`
#'
#' Esta función es interna y se utiliza para construir el diccionario
#' estructural de los datasets disponibles en el paquete.
#'
#' @param id Vector de caracteres con identificadores de datasets.
#'
#' @return Un `data.frame` con una fila por identificador y las columnas:
#'   `id_dataset`, `ambito`, `dimensiones`, `frecuencia` y
#'   `numero_variable`.
#'
#' @keywords internal
#' @noRd
parse_id_dataset <- function(id) {

  # --------------------------------------------------------------------------
  # Comprobaciones de entrada
  # --------------------------------------------------------------------------

  if (!is.character(id)) {
    stop("`id` debe ser un vector de caracteres.", call. = FALSE)
  }

  if (anyNA(id)) {
    stop("`id` no puede contener valores NA.", call. = FALSE)
  }

  if (any(id == "")) {
    stop("`id` no puede contener cadenas vacías.", call. = FALSE)
  }


  # --------------------------------------------------------------------------
  # Función auxiliar para interpretar un único identificador
  # --------------------------------------------------------------------------

  parse_one <- function(x) {

    # Separar el identificador utilizando "_" como delimitador.
    partes <- strsplit(
      x,
      split = "_",
      fixed = TRUE
    )[[1]]


    # Un identificador debe contener, como mínimo:
    #
    #   AMBITO_FRECUENCIA_NUMERO
    #
    # Ejemplo:
    #
    #   TERRITORIO_A_01
    #
    if (length(partes) < 3L) {
      stop(
        sprintf(
          "El identificador '%s' no cumple la estructura esperada.",
          x
        ),
        call. = FALSE
      )
    }


    n <- length(partes)


    # ------------------------------------------------------------------------
    # Extraer los componentes estructurales
    # ------------------------------------------------------------------------

    ambito <- partes[1L]

    frecuencia <- partes[n - 1L]

    numero_variable <- partes[n]


    # Las dimensiones son todos los elementos comprendidos entre el ámbito
    # territorial y los dos componentes finales.
    #
    # Ejemplo:
    #
    #   TERRITORIO_SEXO_EDAD_T_01
    #
    # produce:
    #
    #   c("SEXO", "EDAD")
    #
    # En datasets sin dimensiones adicionales, como TERRITORIO_A_01,
    # se devuelve character(0).
    dimensiones <- if (n > 3L) {
      partes[2L:(n - 2L)]
    } else {
      character(0)
    }


    # ------------------------------------------------------------------------
    # Validar la frecuencia
    # ------------------------------------------------------------------------

    frecuencias_validas <- c(
      "A", # anual
      "T", # trimestral
      "M"  # mensual
    )

    if (!frecuencia %in% frecuencias_validas) {
      stop(
        sprintf(
          paste0(
            "El identificador '%s' contiene una frecuencia no válida: '%s'. ",
            "Se esperaba A, T o M."
          ),
          x,
          frecuencia
        ),
        call. = FALSE
      )
    }


    # ------------------------------------------------------------------------
    # Validar el número de variable
    # ------------------------------------------------------------------------

    if (!grepl("^[0-9]+$", numero_variable)) {
      stop(
        sprintf(
          paste0(
            "El identificador '%s' contiene un número de variable ",
            "no válido: '%s'."
          ),
          x,
          numero_variable
        ),
        call. = FALSE
      )
    }


    # ------------------------------------------------------------------------
    # Construir el resultado
    # ------------------------------------------------------------------------

    data.frame(
      id_dataset = x,
      ambito = ambito,
      dimensiones_txt = paste(
        dimensiones,
        collapse = "_"
      ),
      frecuencia = frecuencia,
      numero_variable = numero_variable,
      stringsAsFactors = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # Aplicar el parser a todos los identificadores recibidos
  # --------------------------------------------------------------------------

  resultado <- lapply(
    id,
    parse_one
  )

  resultado <- do.call(
    rbind,
    resultado
  )

  rownames(resultado) <- NULL


  # --------------------------------------------------------------------------
  # Añadir las dimensiones como list-column
  # --------------------------------------------------------------------------
  #
  # Conservamos dos representaciones:
  #
  # dimensiones_txt
  #   Una versión legible, por ejemplo "SEXO_EDAD".
  #
  # dimensiones
  #   Una lista con c("SEXO", "EDAD"), que será más útil posteriormente
  #   para trabajar programáticamente con las dimensiones.
  #

  resultado$dimensiones <- lapply(
    resultado$id_dataset,
    function(x) {

      partes <- strsplit(
        x,
        split = "_",
        fixed = TRUE
      )[[1]]

      n <- length(partes)

      if (n > 3L) {
        partes[2L:(n - 2L)]
      } else {
        character(0)
      }
    }
  )


  resultado
}
