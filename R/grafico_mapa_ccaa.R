#' Mapa por comunidades autónomas
#'
#' Representa valores por comunidad autónoma mediante un mapa coroplético de
#' España. Los datos se relacionan con la cartografía mediante el código de
#' comunidad autónoma.
#'
#' Canarias se muestran desplazadas junto a la Península. El usuario puede
#' añadir una línea, un recuadro o ningún elemento auxiliar alrededor de ellas.
#'
#' @param datos Un `data.frame` con los datos que se desean representar.
#' @param codigo Cadena de texto con el nombre de la columna que contiene el
#'   código de comunidad autónoma. Por defecto, `"cod_ccaa"`.
#' @param valor Cadena de texto con el nombre de la variable numérica que se
#'   desea representar. Por defecto, `"valor"`.
#' @param titulo Título del gráfico. Por defecto, `NULL`.
#' @param titulo_leyenda Título de la leyenda. Por defecto, `NULL`.
#' @param paleta Nombre de una paleta secuencial de RColorBrewer. Por defecto,
#'   `"PuBu"`.
#' @param n_clases Número de intervalos utilizados para clasificar los valores.
#'   Por defecto, `5`.
#' @param accuracy Precisión de las etiquetas mostradas sobre el mapa.
#'   Por defecto, `0.1`.
#' @param canarias Elemento utilizado para señalar la posición desplazada de
#'   Canarias. Puede ser `"linea"`, `"recuadro"` o `"ninguno"`.
#' @param incluir_ceuta_melilla Si es `TRUE`, mantiene Ceuta y Melilla en el
#'   mapa. Por defecto, `FALSE`.
#'
#' @return Un objeto de clase `ggplot`.
#'
#' @details
#' Cada comunidad autónoma debe tener como máximo una observación. La función
#' no agrega observaciones ni calcula valores territoriales.
#'
#' @examples
#' datos_ejemplo <- data.frame(
#'   cod_ccaa = sprintf("%02d", 1:17),
#'   valor = c(
#'     78.3, 76.8, 68.0, 82.9, 64.6, 76.5,
#'     66.4, 74.8, 78.6, 75.7, 79.5, 70.0,
#'     75.8, 75.6, 73.3, 77.6, 77.8
#'   )
#' )
#'
#' p <- grafico_mapa_ccaa(
#'   datos_ejemplo
#' )
#' p
#'
#' \dontrun{
#' listado <- listar_indicadores()
#' indicador <- "TERRITORIO_A_05"
#' info <- info_indicador(indicador)
#' info$metadata[[2]]
#'
#' datos <- consultar_indicador(
#'   indicador,
#'   tipo_territorio = "ccaa",
#'   anyo = 2024
#' )
#'
#' grafico_mapa_ccaa(
#'   datos,
#'   titulo = "Tasa bruta de natalidad (2024)"
#' ) +
#'   ggplot2::theme(
#'     plot.title = ggplot2::element_text(face = "bold")
#'   )
#' }
#'
#' @import sf
#' @export
grafico_mapa_ccaa <- function(
    datos,
    codigo = "cod_ccaa",
    valor = "valor",
    titulo = NULL,
    titulo_leyenda = NULL,
    paleta = "PuBu",
    n_clases = 5L,
    accuracy = 0.1,
    canarias = c("linea", "recuadro", "ninguno"),
    incluir_ceuta_melilla = FALSE
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

  if (!is.character(codigo) ||
      length(codigo) != 1L ||
      is.na(codigo) ||
      !nzchar(codigo)) {
    stop(
      "`codigo` debe ser una cadena de texto con el nombre de una columna.",
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
    c(codigo, valor),
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

  if (!is.numeric(n_clases) ||
      length(n_clases) != 1L ||
      is.na(n_clases) ||
      n_clases < 2 ||
      n_clases != as.integer(n_clases)) {
    stop(
      "`n_clases` debe ser un número entero mayor o igual que 2.",
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

  if (!is.logical(incluir_ceuta_melilla) ||
      length(incluir_ceuta_melilla) != 1L ||
      is.na(incluir_ceuta_melilla)) {
    stop(
      "`incluir_ceuta_melilla` debe ser TRUE o FALSE.",
      call. = FALSE
    )
  }

  canarias <- match.arg(canarias)

  # ---------------------------------------------------------------------------
  # Preparación de códigos
  #
  # Los códigos se convierten a texto de dos posiciones para hacer compatible
  # la entrada del usuario con `codauto` de la cartografía interna.
  # ---------------------------------------------------------------------------

  datos_plot <- datos[
    !is.na(datos[[codigo]]),
    ,
    drop = FALSE
  ]

  codigos <- as.character(datos_plot[[codigo]])

  codigos_numericos <- suppressWarnings(
    as.integer(codigos)
  )

  if (anyNA(codigos_numericos)) {
    stop(
      paste0(
        "La columna `",
        codigo,
        "` contiene códigos autonómicos no válidos."
      ),
      call. = FALSE
    )
  }

  datos_plot$.codauto_grafico <- sprintf(
    "%02d",
    codigos_numericos
  )

  if (anyDuplicated(datos_plot$.codauto_grafico)) {
    stop(
      paste0(
        "Existen varias observaciones para una misma comunidad autónoma. ",
        "Filtre previamente las dimensiones adicionales antes de generar ",
        "el mapa."
      ),
      call. = FALSE
    )
  }

  codigos_validos <- as.character(mapa_ccaa$codauto)

  codigos_desconocidos <- setdiff(
    datos_plot$.codauto_grafico,
    codigos_validos
  )

  if (length(codigos_desconocidos) > 0L) {
    stop(
      paste0(
        "Códigos de comunidad autónoma no reconocidos: ",
        paste(codigos_desconocidos, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Unión con la cartografía interna
  # ---------------------------------------------------------------------------

  indice <- match(
    as.character(mapa_ccaa$codauto),
    datos_plot$.codauto_grafico
  )

  mapa_plot <- mapa_ccaa

  mapa_plot$.valor_grafico <- datos_plot[[valor]][indice]

  # Ceuta (18) y Melilla (19) se excluyen por defecto para reproducir el diseño
  # nacional utilizado como referencia, pero pueden conservarse explícitamente.
  if (!incluir_ceuta_melilla) {
    mapa_plot <- mapa_plot[
      !as.character(mapa_plot$codauto) %in% c("18", "19"),
      ,
      drop = FALSE
    ]
  }

  # ---------------------------------------------------------------------------
  # Clasificación de los valores
  # ---------------------------------------------------------------------------

  valores_validos <- mapa_plot$.valor_grafico[
    !is.na(mapa_plot$.valor_grafico)
  ]

  if (length(valores_validos) == 0L) {
    stop(
      "No existen valores disponibles para representar en el mapa.",
      call. = FALSE
    )
  }

  val_min <- min(valores_validos)
  val_max <- max(valores_validos)

  if (val_min == val_max) {
    stop(
      paste0(
        "Todos los valores disponibles son iguales. ",
        "No es posible construir intervalos para el mapa."
      ),
      call. = FALSE
    )
  }

  brks <- seq(
    val_min,
    val_max,
    length.out = n_clases + 1L
  )

  formateador_intervalos <- scales::label_number(
    accuracy = accuracy,
    decimal.mark = ",",
    big.mark = "."
  )

  etiquetas_intervalos <- paste0(
    formateador_intervalos(brks[-length(brks)]),
    " - ",
    formateador_intervalos(brks[-1L])
  )

  mapa_plot$.clase_grafico <- cut(
    mapa_plot$.valor_grafico,
    breaks = brks,
    labels = etiquetas_intervalos,
    include.lowest = TRUE
  )

  # ---------------------------------------------------------------------------
  # Etiquetas territoriales
  # ---------------------------------------------------------------------------

  formateador_valores <- scales::label_number(
    accuracy = accuracy,
    decimal.mark = ",",
    big.mark = "."
  )

  mapa_plot$.etiqueta_grafico <- formateador_valores(
    mapa_plot$.valor_grafico
  )

  # ---------------------------------------------------------------------------
  # Construcción del mapa
  # ---------------------------------------------------------------------------

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = mapa_plot,
      ggplot2::aes(
        fill = .data[[".clase_grafico"]]
      ),
      colour = "grey70",
      linewidth = 0.1
    )

  if (canarias == "linea") {
    p <- p +
      ggplot2::geom_sf(
        data = mapa_canarias_linea,
        colour = "grey70",
        linewidth = 0.2
      )
  }

  if (canarias == "recuadro") {
    p <- p +
      ggplot2::geom_sf(
        data = mapa_canarias_recuadro,
        colour = "grey70",
        linewidth = 0.2
      )
  }

  p <- p +
    ggplot2::geom_sf_label(
      data = mapa_plot[
        !is.na(mapa_plot$.valor_grafico),
        ,
        drop = FALSE
      ],
      ggplot2::aes(
        label = .data[[".etiqueta_grafico"]]
      ),
      fun.geometry = sf::st_centroid,
      label.size = NA,
      alpha = 0.5,
      size = 3
    ) +
    ggplot2::scale_fill_brewer(
      name = titulo_leyenda,
      palette = paleta,
      guide = ggplot2::guide_legend(
        direction = "horizontal",
        title.position = "top",
        title.hjust = 0.5,
        label.hjust = 0.5,
        label.position = "bottom",
        keywidth = 3,
        keyheight = 0.5
      )
    ) +
    ggplot2::labs(
      title = titulo
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5
      ),
      legend.position = "inside",
      legend.position.inside = c(0.7, 0.1)
    )

  p
}
