#' Gráfico de líneas para series temporales
#'
#' Genera un gráfico de líneas a partir de datos en formato largo.
#'
#' Puede representar una única serie o varias series diferenciadas mediante
#' una variable de agrupación. Los datos pueden proceder de
#' [consultar_indicador()] o de cualquier otro `data.frame` compatible.
#'
#' @param datos Un `data.frame` con los datos que se desean representar.
#' @param x Cadena de texto con el nombre de la variable utilizada en el eje X.
#'   Por defecto, `"anyo"`.
#' @param y Cadena de texto con el nombre de la variable utilizada en el eje Y.
#'   Por defecto, `"valor"`.
#' @param grupo Cadena de texto con el nombre de la variable que identifica
#'   distintas series. Si es `NULL`, se representa una única serie.
#' @param colores Vector de colores opcional. Si `grupo` no es `NULL`, puede
#'   utilizarse un vector nombrado cuyos nombres correspondan a los valores de
#'   la variable de agrupación. Si es `NULL`, se utiliza la escala de colores
#'   predeterminada de `ggplot2`.
#' @param titulo Título del gráfico. Por defecto, `NULL`.
#' @param subtitulo Subtítulo del gráfico. Por defecto, `NULL`.
#' @param etiqueta_x Etiqueta del eje X. Por defecto, `NULL`.
#' @param etiqueta_y Etiqueta del eje Y. Por defecto, `NULL`.
#' @param breaks_x Valores que deben mostrarse como cortes del eje X. Si es
#'   `NULL` y `x` es numérica, se calculan automáticamente mediante
#'   [scales::breaks_pretty()].
#'
#' @return Un objeto de clase `ggplot` que puede modificarse posteriormente
#'   mediante la gramática habitual de `ggplot2`.
#'
#' @details
#' Los datos deben contener una única observación por punto y serie. Si quedan
#' dimensiones sin filtrar, la función informa de cuáles son y de sus valores
#' disponibles.
#'
#' @examples
#'
#' datos_ejemplo <- data.frame(
#'   territorio = rep(
#'     c("Comunitat Valenciana", "España"),
#'     each = 18
#'   ),
#'   anyo = rep(2004:2021, times = 2),
#'   valor = c(
#'     # Comunitat Valenciana
#'     79.8, 79.7, 80.4, 80.5, 80.9, 81.3,
#'     81.6, 81.8, 81.9, 82.4, 82.4, 82.2,
#'     82.6, 82.5, 82.6, 83.0, 82.4, 82.2,
#'
#'     # España
#'     80.3, 80.3, 80.9, 81.0, 81.3, 81.7,
#'     82.1, 82.2, 82.3, 82.8, 82.9, 82.7,
#'     83.1, 83.1, 83.2, 83.6, 82.3, 83.1
#'   )
#' )
#'
#' colores_ejemplo <- c(
#'   "Comunitat Valenciana" = "#000000",
#'   "España" = "#AA151B"
#' )
#'
#' p <- grafico_lineas(
#'   datos_ejemplo,
#'   grupo = "territorio",
#'   colores = colores_ejemplo,
#'   titulo = "Indicador de ejemplo"
#' )
#'
#' p
#'
#' # El resultado es un objeto ggplot y puede seguir personalizándose.
#' p +
#'   ggplot2::labs(
#'     subtitle = "Comunitat Valenciana y España",
#'     caption = "Datos ilustrativos"
#'   )
#'
#' @export
grafico_lineas <- function(
    datos,
    x = "anyo",
    y = "valor",
    grupo = NULL,
    colores = NULL,
    titulo = NULL,
    subtitulo = NULL,
    etiqueta_x = NULL,
    etiqueta_y = NULL,
    breaks_x = NULL
) {

  # ---------------------------------------------------------------------------
  # Validaciones básicas de entrada
  # ---------------------------------------------------------------------------

  if (!is.data.frame(datos)) {
    stop(
      "`datos` debe ser un data.frame.",
      call. = FALSE
    )
  }

  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(
      "`x` debe ser una cadena de texto con el nombre de una columna.",
      call. = FALSE
    )
  }

  if (!is.character(y) || length(y) != 1L || is.na(y) || !nzchar(y)) {
    stop(
      "`y` debe ser una cadena de texto con el nombre de una columna.",
      call. = FALSE
    )
  }

  if (!is.null(grupo) &&
      (!is.character(grupo) ||
       length(grupo) != 1L ||
       is.na(grupo) ||
       !nzchar(grupo))) {
    stop(
      "`grupo` debe ser NULL o una cadena de texto con el nombre de una columna.",
      call. = FALSE
    )
  }

  columnas_requeridas <- c(x, y, grupo)
  columnas_requeridas <- columnas_requeridas[!is.na(columnas_requeridas)]

  columnas_faltantes <- setdiff(columnas_requeridas, names(datos))

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

  # ---------------------------------------------------------------------------
  # Validación de la estructura de las series
  #
  # Para dibujar una línea debe existir como máximo una observación para cada
  # combinación de posición en el eje X y serie. Si existen varias observaciones,
  # significa que los datos contienen alguna dimensión adicional sin resolver.
  #
  # La función no agrega ni selecciona categorías automáticamente. Decidir si
  # debe utilizarse un total, una categoría concreta o varias series corresponde
  # a la fase de consulta de los datos.
  # ---------------------------------------------------------------------------

  columnas_serie <- c(x, grupo)
  claves_serie <- datos[columnas_serie]

  if (anyDuplicated(claves_serie)) {

    # Identificamos qué columnas cambian dentro de una misma combinación de
    # eje X + grupo. Esto permite orientar al usuario hacia las dimensiones
    # que todavía debe filtrar o incorporar explícitamente a la representación.
    #
    # Se convierten las claves a factores para poder formar grupos mediante
    # interaction() sin depender del tipo original de las columnas.
    factores_clave <- lapply(
      claves_serie,
      function(z) factor(z, exclude = NULL)
    )

    id_serie <- do.call(
      interaction,
      c(
        factores_clave,
        list(drop = TRUE, lex.order = TRUE)
      )
    )

    filas_por_clave <- split(
      seq_len(nrow(datos)),
      id_serie,
      drop = TRUE
    )

    filas_por_clave <- filas_por_clave[
      lengths(filas_por_clave) > 1L
    ]

    # Excluimos las variables que ya definen el gráfico y la medida representada.
    columnas_candidatas <- setdiff(
      names(datos),
      c(x, y, grupo)
    )

    columnas_que_varian <- columnas_candidatas[
      vapply(
        columnas_candidatas,
        function(columna) {

          any(
            vapply(
              filas_por_clave,
              function(filas) {

                valores <- datos[[columna]][filas]
                valores <- valores[!is.na(valores)]

                length(unique(valores)) > 1L
              },
              logical(1)
            )
          )
        },
        logical(1)
      )
    ]

    # Construimos un diagnóstico legible mostrando, además del nombre de cada
    # columna que provoca la ambigüedad, los valores que contiene.
    #
    # Esto permite distinguir rápidamente si el usuario debe filtrar una
    # categoría (por ejemplo, una edad concreta) o si desea utilizar esa
    # dimensión para generar varias series.
    if (length(columnas_que_varian) > 0L) {

      detalle_dimensiones <- vapply(
        columnas_que_varian,
        function(columna) {

          valores <- unique(datos[[columna]])
          valores <- valores[!is.na(valores)]
          valores <- sort(valores)

          paste0(
            "- ",
            columna,
            ": ",
            paste(valores, collapse = ", ")
          )
        },
        character(1)
      )

      detalle_columnas <- paste0(
        "\n\nDimensiones que todavía varían:\n",
        paste(detalle_dimensiones, collapse = "\n")
      )

    } else {

      detalle_columnas <- ""
    }

    recomendacion <- paste0(
      "\n\nAntes de generar el gráfico puede:\n",
      "1. Filtrar una categoría concreta con `consultar_indicador()`.\n",
      "2. Representar las categorías como series diferentes, si desea ",
      "compararlas.\n",
      "3. Utilizar una categoría de total o agregado si el indicador la ",
      "proporciona.\n\n",
      "No se calcula automáticamente un total a partir de las categorías, ",
      "porque la forma correcta de agregarlas depende de la definición ",
      "estadística del indicador."
    )

    if (is.null(grupo)) {
      stop(
        paste0(
          "Existen varias observaciones para un mismo valor de `",
          x,
          "`.",
          detalle_columnas,
          recomendacion
        ),
        call. = FALSE
      )
    }

    stop(
      paste0(
        "Existen varias observaciones para una misma combinación de `",
        x,
        "` y `",
        grupo,
        "`.",
        detalle_columnas,
        recomendacion
      ),
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Construcción del gráfico
  #
  # Se utiliza .data[[...]] para permitir que los nombres de las columnas
  # se proporcionen como texto sin recurrir a evaluación no estándar.
  # ---------------------------------------------------------------------------

  if (is.null(grupo)) {

    p <- ggplot2::ggplot(
      datos,
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]]
      )
    ) +
      ggplot2::geom_line(
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        shape = 21,
        size = 2.5,
        stroke = 1,
        fill = "white"
      )

  } else {

    p <- ggplot2::ggplot(
      datos,
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]],
        colour = .data[[grupo]],
        group = .data[[grupo]]
      )
    ) +
      ggplot2::geom_line(
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        shape = 21,
        size = 2.5,
        stroke = 1,
        fill = "white"
      )

    if (!is.null(colores)) {
      p <- p +
        ggplot2::scale_colour_manual(
          values = colores
        )
    }
  }

  # ---------------------------------------------------------------------------
  # Escala del eje X
  #
  # Solo se aplica scale_x_continuous() cuando la variable es numérica.
  # Esto permite que en el futuro la misma función pueda utilizarse también
  # con variables temporales no numéricas sin forzar una escala incompatible.
  # ---------------------------------------------------------------------------

  if (is.numeric(datos[[x]])) {

    if (is.null(breaks_x)) {
      breaks_x <- scales::breaks_pretty(n = 7)
    }

    p <- p +
      ggplot2::scale_x_continuous(
        breaks = breaks_x
      )
  }

  # ---------------------------------------------------------------------------
  # Etiquetas y estilo común
  # ---------------------------------------------------------------------------

  p <- p +
    ggplot2::labs(
      title = titulo,
      subtitle = subtitulo,
      x = etiqueta_x,
      y = etiqueta_y,
      colour = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        margin = ggplot2::margin(r = 15, unit = "pt")
      ),
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  p
}
