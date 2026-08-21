#' Gráfico de columnas apiladas
#'
#' Genera un gráfico de columnas apiladas para representar la distribución
#' de una variable categórica. Opcionalmente, permite comparar grupos mediante
#' facetas.
#'
#' Las categorías pueden ordenarse explícitamente y representarse mediante
#' una paleta de colores personalizada.
#'
#' @param datos Un `data.frame` en formato largo con los datos que se desean
#'   representar.
#' @param x Cadena de texto con el nombre de la variable del eje X.
#' @param categoria Cadena de texto con el nombre de la variable que define
#'   los segmentos de las columnas.
#' @param valor Cadena de texto con el nombre de la variable numérica.
#'   Por defecto, `"valor"`.
#' @param faceta Cadena de texto con el nombre de la variable utilizada para
#'   crear facetas. Si es `NULL`, se genera un único panel.
#' @param niveles Vector opcional con el orden de las categorías. Si se
#'   especifica, debe contener todos los valores presentes en `categoria`.
#' @param colores Vector nombrado opcional con los colores de las categorías.
#'   Los nombres deben coincidir con sus valores.
#' @param titulo Título del gráfico. Por defecto, `NULL`.
#' @param subtitulo Subtítulo del gráfico. Por defecto, `NULL`.
#' @param accuracy Precisión utilizada para las etiquetas numéricas.
#'   Por defecto, `0.1`.
#' @param minimo_etiqueta Valor mínimo que debe alcanzar un segmento para
#'   mostrar su etiqueta. Por defecto, `2`.
#' @param color_texto Color de las etiquetas numéricas. Por defecto, `"black"`.
#' @param tamanyo_texto Tamaño de las etiquetas numéricas. Por defecto, `3`.
#' @param ancho_columna Anchura de las columnas. Por defecto, `0.8`.
#' @param filas_leyenda Número de filas de la leyenda. Por defecto, `1`.
#'
#' @return Un objeto de clase `ggplot`.
#'
#' @details
#' Cada combinación de `x`, `categoria` y, si se utiliza, `faceta` debe
#' identificar una única observación.
#'
#' @examples
#' datos_ejemplo <- data.frame(
#'   territorio = rep(
#'     c("Comunitat Valenciana", "España"),
#'     each = 24
#'   ),
#'   anyo = rep(
#'     rep(2018:2023, each = 4),
#'     times = 2
#'   ),
#'   respuesta = rep(
#'     c(
#'       "Muy satisfecho/a",
#'       "Bastante satisfecho/a",
#'       "Poco satisfecho/a",
#'       "Nada satisfecho/a"
#'     ),
#'     times = 12
#'   ),
#'   valor = c(
#'     # Comunitat Valenciana
#'     11.5, 47.7, 27.4, 13.4,
#'     13.0, 44.7, 35.2,  7.1,
#'      9.2, 50.0, 34.0,  6.8,
#'     12.4, 46.1, 31.8,  9.7,
#'     14.8, 43.6, 33.1,  8.5,
#'     16.6, 40.6, 32.5, 10.3,
#'
#'     # España
#'     10.7, 46.2, 33.5,  9.6,
#'      9.9, 44.0, 37.0,  9.1,
#'     11.3, 45.5, 32.8, 10.4,
#'     12.1, 43.8, 34.2,  9.9,
#'     13.2, 42.6, 33.7, 10.5,
#'     14.0, 40.2, 34.9, 10.9
#'   )
#' )
#' niveles_ejemplo <- c(
#'   "Muy satisfecho/a",
#'   "Bastante satisfecho/a",
#'   "Poco satisfecho/a",
#'   "Nada satisfecho/a"
#' )
#'
#' colores_ejemplo <- c(
#'   "Muy satisfecho/a" = "#5AB4AC",
#'   "Bastante satisfecho/a" = "#ACD9D5",
#'   "Poco satisfecho/a" = "#EBD9B2",
#'   "Nada satisfecho/a" = "#D8B365"
#' )
#'
#' p <- grafico_columnas_apiladas(
#'   datos_ejemplo,
#'   x = "anyo",
#'   categoria = "respuesta",
#'   faceta = "territorio",
#'   niveles = niveles_ejemplo,
#'   colores = colores_ejemplo
#' )
#'
#' p
#'
#' p + ggplot2::labs(
#'   title = "Satisfacción",
#'   subtitle = "Datos ilustrativos"
#' )
#'
#' @export
grafico_columnas_apiladas <- function(
    datos,
    x,
    categoria,
    valor = "valor",
    faceta = NULL,
    niveles = NULL,
    colores = NULL,
    titulo = NULL,
    subtitulo = NULL,
    accuracy = 0.1,
    minimo_etiqueta = 2,
    color_texto = "black",
    tamanyo_texto = 3,
    ancho_columna = 0.8,
    filas_leyenda = 1
) {

  # ---------------------------------------------------------------------------
  # Validaciones básicas
  # ---------------------------------------------------------------------------

  if (!is.data.frame(datos)) {
    stop(
      "`datos` debe ser un data.frame.",
      call. = FALSE
    )
  }

  validar_nombre_columna <- function(argumento, nombre_argumento) {

    if (!is.character(argumento) ||
        length(argumento) != 1L ||
        is.na(argumento) ||
        !nzchar(argumento)) {
      stop(
        paste0(
          "`", nombre_argumento,
          "` debe ser una cadena de texto con el nombre de una columna."
        ),
        call. = FALSE
      )
    }
  }

  validar_nombre_columna(x, "x")
  validar_nombre_columna(categoria, "categoria")
  validar_nombre_columna(valor, "valor")

  if (!is.null(faceta)) {
    validar_nombre_columna(faceta, "faceta")
  }

  columnas_requeridas <- c(
    x,
    categoria,
    valor,
    faceta
  )

  columnas_faltantes <- setdiff(
    columnas_requeridas,
    names(datos)
  )

  if (length(columnas_faltantes) > 0L) {
    stop(
      paste0(
        "Columnas no disponibles en `datos`: ",
        paste(columnas_faltantes, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (nrow(datos) == 0L) {
    stop(
      "`datos` no contiene observaciones para representar.",
      call. = FALSE
    )
  }

  if (!is.numeric(datos[[valor]])) {
    stop(
      paste0(
        "La columna `",
        valor,
        "` debe ser numérica."
      ),
      call. = FALSE
    )
  }

  # Cada segmento de cada columna debe corresponder a una única observación.
  claves <- datos[c(x, categoria, faceta)]

  if (anyDuplicated(claves)) {
    stop(
      paste0(
        "Existen varias observaciones para una misma combinación de `",
        paste(c(x, categoria, faceta), collapse = "`, `"),
        "`. Filtre previamente las dimensiones adicionales."
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Orden de las categorías
  # ---------------------------------------------------------------------------

  valores_categoria <- unique(
    as.character(datos[[categoria]])
  )

  valores_categoria <- valores_categoria[
    !is.na(valores_categoria)
  ]

  if (!is.null(niveles)) {

    niveles_faltantes <- setdiff(
      valores_categoria,
      niveles
    )

    if (length(niveles_faltantes) > 0L) {
      stop(
        paste0(
          "`niveles` no contiene todas las categorías presentes en los datos: ",
          paste(niveles_faltantes, collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }

    datos[[categoria]] <- factor(
      datos[[categoria]],
      levels = niveles
    )
  }

  # ---------------------------------------------------------------------------
  # Paleta
  # ---------------------------------------------------------------------------

  if (!is.null(colores)) {

    if (is.null(names(colores)) ||
        any(!nzchar(names(colores)))) {
      stop(
        "`colores` debe ser un vector nombrado.",
        call. = FALSE
      )
    }

    colores_faltantes <- setdiff(
      valores_categoria,
      names(colores)
    )

    if (length(colores_faltantes) > 0L) {
      stop(
        paste0(
          "Faltan colores para las categorías: ",
          paste(colores_faltantes, collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Etiquetas
  # ---------------------------------------------------------------------------

  formateador <- scales::label_number(
    accuracy = accuracy,
    decimal.mark = ",",
    big.mark = "."
  )

  datos$.etiqueta_grafico <- ifelse(
    datos[[valor]] >= minimo_etiqueta,
    formateador(datos[[valor]]),
    ""
  )

  # ---------------------------------------------------------------------------
  # Gráfico
  # ---------------------------------------------------------------------------

  p <- ggplot2::ggplot(
    datos,
    ggplot2::aes(
      x = as.factor(.data[[x]]),
      y = .data[[valor]],
      fill = .data[[categoria]]
    )
  ) +
    ggplot2::geom_col(
      alpha = 0.8,
      width = ancho_columna
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = .data[[".etiqueta_grafico"]]
      ),
      position = ggplot2::position_stack(
        vjust = 0.5
      ),
      size = tamanyo_texto,
      colour = color_texto
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        nrow = filas_leyenda
      )
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = NULL,
      title = titulo,
      subtitle = subtitulo
    )

  if (!is.null(colores)) {
    p <- p +
      ggplot2::scale_fill_manual(
        values = colores
      )
  }

  if (!is.null(faceta)) {
    p <- p +
      ggplot2::facet_wrap(
        ggplot2::vars(.data[[faceta]])
      )
  }

  p <- p +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        size = 11
      ),
      legend.title = ggplot2::element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(
        size = 12,
        face = "bold"
      )
    )

  p
}
