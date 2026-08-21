#' Comparación territorial de un indicador
#'
#' Genera un gráfico horizontal para comparar el valor de un indicador entre
#' distintas categorías, normalmente territorios, en un único período.
#'
#' Permite destacar determinadas categorías mediante colores específicos y
#' ordenar los valores de mayor a menor o de menor a mayor.
#'
#' @param datos Un `data.frame` con los datos que se desean representar.
#' @param categoria Cadena de texto con el nombre de la variable categórica.
#'   Por defecto, `"territorio"`.
#' @param valor Cadena de texto con el nombre de la variable numérica.
#'   Por defecto, `"valor"`.
#' @param colores_destacados Vector nombrado opcional con colores para
#'   categorías concretas. Los nombres deben coincidir con los valores de
#'   `categoria`.
#' @param color_resto Color utilizado para las categorías no incluidas en
#'   `colores_destacados`. Por defecto, `"gray50"`.
#' @param titulo Título del gráfico. Por defecto, `NULL`.
#' @param subtitulo Subtítulo del gráfico. Por defecto, `NULL`.
#' @param accuracy Precisión utilizada para formatear las etiquetas numéricas.
#'   Se pasa a [scales::label_number()]. Por defecto, `0.1`.
#' @param porcentaje Si es `TRUE`, añade el símbolo `%` a las etiquetas de los
#'   valores. Por defecto, `TRUE`.
#' @param margen Factor utilizado para ampliar el eje horizontal y dejar espacio
#'   para las etiquetas. Por defecto, `1.1`.
#' @param orden_desc Si es `TRUE`, ordena las categorías de mayor a menor.
#'   Si es `FALSE`, las ordena de menor a mayor.
#'
#' @return Un objeto de clase `ggplot`.
#'
#' @details
#' Cada categoría debe aparecer una sola vez en `datos`. La función no agrega
#' ni resume observaciones automáticamente.
#'
#' @examples
#' datos_ejemplo <- data.frame(
#'   territorio = c(
#'     "Illes Balears",
#'     "Ceuta",
#'     "Extremadura",
#'     "Melilla",
#'     "Cataluña",
#'     "Andalucía",
#'     "País Vasco",
#'     "Navarra",
#'     "Aragón",
#'     "Cantabria",
#'     "Madrid",
#'     "Comunitat Valenciana",
#'     "Murcia",
#'     "España",
#'     "Castilla-La Mancha",
#'     "La Rioja",
#'     "Galicia",
#'     "Asturias",
#'     "Castilla y León",
#'     "Canarias"
#'   ),
#'   valor = c(
#'     82.9, 81.7, 79.5, 79.2, 78.6,
#'     78.3, 77.8, 77.6, 76.8, 76.5,
#'     75.8, 75.7, 75.6, 75.5, 74.8,
#'     73.3, 70.0, 68.0, 66.4, 64.6
#'   )
#' )
#'
#' colores_ejemplo <- c(
#'   "Comunitat Valenciana" = "#000000",
#'   "España" = "#AA151B"
#' )
#'
#' p <- grafico_comparacion_territorial(
#'   datos_ejemplo,
#'   colores_destacados = colores_ejemplo
#' )
#'
#' p
#'
#' p +
#'   ggplot2::labs(
#'     title = "Indicador de ejemplo",
#'     subtitle = "Comparación territorial",
#'     caption = "Fuente: elaboración propia."
#'   ) +
#'   ggplot2::theme(
#'     plot.title.position = "plot",
#'     plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = 16),
#'     plot.subtitle = ggplot2::element_text(size = 12),
#'     plot.caption = ggplot2::element_text(face = "italic", color = "grey30")
#'   )
#'
#'
#' @export
grafico_comparacion_territorial <- function(
    datos,
    categoria = "territorio",
    valor = "valor",
    colores_destacados = NULL,
    color_resto = "gray50",
    titulo = NULL,
    subtitulo = NULL,
    accuracy = 0.1,
    porcentaje = TRUE,
    margen = 1.1,
    orden_desc = TRUE
) {

  # ---------------------------------------------------------------------------
  # Validaciones
  # ---------------------------------------------------------------------------

  if (!is.data.frame(datos)) {
    stop(
      "`datos` debe ser un data.frame.",
      call. = FALSE
    )
  }

  if (!is.character(categoria) ||
      length(categoria) != 1L ||
      is.na(categoria) ||
      !nzchar(categoria)) {
    stop(
      "`categoria` debe ser una cadena de texto con el nombre de una columna.",
      call. = FALSE
    )
  }

  if (!is.character(valor) ||
      length(valor) != 1L ||
      is.na(valor) ||
      !nzchar(valor)) {
    stop(
      "`valor` debe ser una cadena de texto con el nombre de una columna.",
      call. = FALSE
    )
  }

  columnas_faltantes <- setdiff(
    c(categoria, valor),
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

  if (anyDuplicated(datos[[categoria]])) {
    stop(
      paste0(
        "La columna `",
        categoria,
        "` contiene categorías repetidas. ",
        "Cada categoría debe tener una única observación antes de generar ",
        "el gráfico."
      ),
      call. = FALSE
    )
  }

  if (!is.numeric(accuracy) ||
      length(accuracy) != 1L ||
      is.na(accuracy) ||
      accuracy <= 0) {
    stop(
      "`accuracy` debe ser un número positivo.",
      call. = FALSE
    )
  }

  if (!is.logical(porcentaje) ||
      length(porcentaje) != 1L ||
      is.na(porcentaje)) {
    stop(
      "`porcentaje` debe ser TRUE o FALSE.",
      call. = FALSE
    )
  }

  if (!is.numeric(margen) ||
      length(margen) != 1L ||
      is.na(margen) ||
      margen <= 1) {
    stop(
      "`margen` debe ser un número mayor que 1.",
      call. = FALSE
    )
  }

  if (!is.logical(orden_desc) ||
      length(orden_desc) != 1L ||
      is.na(orden_desc)) {
    stop(
      "`orden_desc` debe ser TRUE o FALSE.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Preparación de los datos
  # ---------------------------------------------------------------------------

  datos_plot <- datos[
    !is.na(datos[[categoria]]) &
      !is.na(datos[[valor]]),
    ,
    drop = FALSE
  ]

  if (nrow(datos_plot) == 0L) {
    stop(
      "No quedan observaciones válidas después de eliminar valores NA.",
      call. = FALSE
    )
  }

  if (orden_desc) {
    orden <- order(
      datos_plot[[valor]],
      decreasing = TRUE
    )
  } else {
    orden <- order(
      datos_plot[[valor]],
      decreasing = FALSE
    )
  }

  datos_plot <- datos_plot[orden, , drop = FALSE]

  # Se fija el orden mediante factor para que ggplot2 respete exactamente
  # la clasificación calculada anteriormente.
  datos_plot[[categoria]] <- factor(
    datos_plot[[categoria]],
    levels = datos_plot[[categoria]]
  )

  # ---------------------------------------------------------------------------
  # Colores
  # ---------------------------------------------------------------------------

  colores <- rep(
    color_resto,
    nrow(datos_plot)
  )

  if (!is.null(colores_destacados)) {

    if (is.null(names(colores_destacados)) ||
        any(!nzchar(names(colores_destacados)))) {
      stop(
        "`colores_destacados` debe ser un vector nombrado.",
        call. = FALSE
      )
    }

    coincidencias <- match(
      as.character(datos_plot[[categoria]]),
      names(colores_destacados)
    )

    tiene_color <- !is.na(coincidencias)

    colores[tiene_color] <- colores_destacados[
      coincidencias[tiene_color]
    ]
  }

  datos_plot$.color_grafico <- colores

  # ---------------------------------------------------------------------------
  # Escala horizontal
  # ---------------------------------------------------------------------------

  val_min <- min(datos_plot[[valor]])
  val_max <- max(datos_plot[[valor]])

  if (val_max <= 0) {

    x_min <- val_min * margen
    x_max <- 0

  } else if (val_min >= 0) {

    x_min <- 0
    x_max <- val_max * margen

  } else {

    x_min <- val_min * margen
    x_max <- val_max * margen
  }

  posicion_eje <- if (val_max <= 0) {
    "right"
  } else {
    "left"
  }

  # ---------------------------------------------------------------------------
  # Etiquetas numéricas
  # ---------------------------------------------------------------------------

  formateador <- scales::label_number(
    accuracy = accuracy,
    decimal.mark = ",",
    big.mark = "."
  )

  etiquetas <- formateador(
    datos_plot[[valor]]
  )

  if (porcentaje) {
    etiquetas <- paste0(
      etiquetas,
      " %"
    )
  }

  datos_plot$.etiqueta_grafico <- etiquetas

  # ---------------------------------------------------------------------------
  # Gráfico
  # ---------------------------------------------------------------------------

  p <- ggplot2::ggplot(
    datos_plot,
    ggplot2::aes(
      x = .data[[valor]],
      y = .data[[categoria]],
      colour = .data[[".color_grafico"]]
    )
  ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        xend = .data[[valor]],
        yend = .data[[categoria]]
      ),
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      shape = 21,
      size = 2.5,
      stroke = 1,
      fill = "white"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = .data[[".etiqueta_grafico"]]
      ),
      size = 3,
      colour = "black",
      hjust = if (val_max > 0) {
        -0.4
      } else {
        1.3
      }
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = titulo,
      subtitle = subtitulo
    ) +
    ggplot2::scale_x_continuous(
      limits = c(x_min, x_max),
      expand = c(0, 0),
      labels = scales::label_number(
        decimal.mark = ",",
        big.mark = "."
      )
    ) +
    ggplot2::scale_y_discrete(
      position = posicion_eje
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5
      ),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        size = 9
      )
    )

  p
}
