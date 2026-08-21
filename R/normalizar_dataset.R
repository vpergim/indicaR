#' Normalizar un dataset de indicadores
#'
#' Transforma uno de los datasets importados desde Excel desde su formato
#' original ancho (wide) a un formato largo (long) homogéneo.
#'
#' La estructura del dataset se deduce automáticamente a partir de su
#' identificador mediante `[parse_id_dataset()]`.
#'
#' La función realiza las siguientes operaciones:
#'
#' 1. Identifica las columnas descriptivas a partir de `id_dataset`.
#' 2. Asigna nombres canónicos a las dimensiones.
#' 3. Convierte las columnas temporales a formato largo.
#' 4. Normaliza la representación del periodo.
#' 5. Extrae `anyo`, `trimestre` y `mes`.
#' 6. Convierte `valor` a tipo numérico.
#' 7. Incorpora información descriptiva procedente de `metadata`.
#'
#' Esta función es interna. Los usuarios del paquete no deberían trabajar
#' directamente con ella.
#'
#' @param id_dataset Cadena de caracteres con el identificador del dataset,
#'   por ejemplo `"TERRITORIO_SEXO_EDAD_T_01"`.
#'
#' @param datos Data frame o tibble con el dataset original importado desde
#'   Excel.
#'
#' @param metadata_tbl Tabla de metadatos del paquete. Por defecto utiliza
#'   el objeto interno `metadata`.
#'
#' @return Un data frame en formato long. Contiene:
#'
#'   - metadatos básicos del indicador;
#'   - las dimensiones específicas del dataset;
#'   - `periodo`;
#'   - `anyo`;
#'   - `trimestre`;
#'   - `mes`;
#'   - `valor`.
#'
#' @keywords internal
#' @noRd
normalizar_dataset <- function(
    id_dataset,
    datos,
    metadata_tbl = metadata
) {

  # ==========================================================================
  # 1. Comprobaciones de entrada
  # ==========================================================================

  if (!is.character(id_dataset) || length(id_dataset) != 1L) {
    stop(
      "`id_dataset` debe ser una única cadena de caracteres.",
      call. = FALSE
    )
  }

  if (is.na(id_dataset) || id_dataset == "") {
    stop(
      "`id_dataset` no puede ser NA ni una cadena vacía.",
      call. = FALSE
    )
  }

  if (!is.data.frame(datos)) {
    stop(
      "`datos` debe ser un data frame o tibble.",
      call. = FALSE
    )
  }


  # ==========================================================================
  # 2. Interpretar el identificador del dataset
  # ==========================================================================
  #
  # Ejemplo:
  #
  #   TERRITORIO_SEXO_EDAD_T_01
  #
  # se interpreta como:
  #
  #   ámbito       = TERRITORIO
  #   dimensiones  = SEXO, EDAD
  #   frecuencia   = T
  #

  estructura <- parse_id_dataset(id_dataset)

  ambito <- estructura$ambito[[1]]
  dimensiones <- estructura$dimensiones[[1]]
  frecuencia <- estructura$frecuencia[[1]]


  # ==========================================================================
  # 3. Determinar las columnas descriptivas
  # ==========================================================================
  #
  # El número de columnas descriptivas viene determinado por:
  #
  #   1 columna de ámbito territorial
  #   +
  #   número de dimensiones adicionales
  #
  # Ejemplo:
  #
  #   TERRITORIO_SEXO_EDAD_T_01
  #
  # tiene:
  #
  #   TERRITORIO
  #   SEXO
  #   EDAD
  #
  # es decir, tres columnas descriptivas.
  #

  n_dimensiones <- 1L + length(dimensiones)

  if (ncol(datos) <= n_dimensiones) {
    stop(
      sprintf(
        paste0(
          "El dataset '%s' no contiene columnas temporales después ",
          "de las dimensiones descriptivas."
        ),
        id_dataset
      ),
      call. = FALSE
    )
  }


  # ==========================================================================
  # 4. Construir nombres canónicos para las dimensiones
  # ==========================================================================
  #
  # Para el ámbito territorial utilizamos:
  #
  #   CCAA -> ccaa
  #
  # mientras que los restantes ámbitos se representan mediante:
  #
  #   territorio
  #
  # Esto permite utilizar una denominación genérica cuando un dataset
  # contiene mezclados total nacional, comunidades autónomas y/o provincias.
  #

  nombre_ambito <- if (ambito == "CCAA") {
    "ccaa"
  } else {
    "territorio"
  }


  # Las dimensiones adicionales se convierten simplemente a minúsculas.
  #
  # Ejemplos:
  #
  #   SEXO             -> sexo
  #   EDAD             -> edad
  #   NIVELEDUCATIVO   -> niveleducativo
  #   TIPOVIVIENDA     -> tipovivienda
  #

  nombres_dimensiones <- tolower(dimensiones)

  nombres_canonicos <- c(
    nombre_ambito,
    nombres_dimensiones
  )


  # Comprobación defensiva: después de normalizar no deberían generarse
  # nombres duplicados.
  if (anyDuplicated(nombres_canonicos)) {
    stop(
      sprintf(
        "El dataset '%s' genera nombres de dimensiones duplicados.",
        id_dataset
      ),
      call. = FALSE
    )
  }


  # ==========================================================================
  # 5. Renombrar las columnas descriptivas
  # ==========================================================================
  #
  # El renombrado se realiza POR POSICIÓN y no utilizando la cabecera
  # original del Excel.
  #
  # Esto es intencionado.
  #
  # Por ejemplo, un fichero TERRITORIOPROV puede tener como primera cabecera
  # física "TERRITORIO" o "TERRITORIOPROV". Ambas situaciones ya han sido
  # auditadas y sabemos que representan la misma dimensión conceptual.
  #

  datos_normalizados <- datos

  names(datos_normalizados)[seq_len(n_dimensiones)] <-
    nombres_canonicos


  # Garantizar que las dimensiones sean de tipo character.
  datos_normalizados[nombres_canonicos] <-
    lapply(
      datos_normalizados[nombres_canonicos],
      as.character
    )


  # ==========================================================================
  # 6. Identificar las columnas temporales originales
  # ==========================================================================
  #
  # Esta identificación debe realizarse ANTES de añadir columnas derivadas
  # como `cod_ccaa`, `cod_provincia` o `tipo_territorio`.
  #
  # De lo contrario, esas nuevas columnas podrían interpretarse erróneamente
  # como periodos.
  #

  posiciones_periodo <- seq.int(
    from = n_dimensiones + 1L,
    to = ncol(datos_normalizados)
  )

  nombres_periodo <- names(datos_normalizados)[
    posiciones_periodo
  ]


  # ==========================================================================
  # 7. Homogeneizar la dimensión territorial
  # ==========================================================================

  if (!exists("diccionario_territorial")) {
    stop(
      paste0(
        "No existe `diccionario_territorial`. ",
        "Debe construirse antes de normalizar los datasets."
      ),
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # 7.1. Datasets de comunidades autónomas
  # --------------------------------------------------------------------------

  if (ambito == "CCAA") {

    territorio_normalizado <- homogeneizar_ccaa(
      datos_normalizados$ccaa,
      diccionario_territorial
    )

    if (any(!territorio_normalizado$encontrada)) {

      desconocidas <- unique(
        territorio_normalizado$ccaa_original[
          !territorio_normalizado$encontrada
        ]
      )

      stop(
        sprintf(
          "El dataset '%s' contiene CCAA no reconocidas: %s",
          id_dataset,
          paste(desconocidas, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    datos_normalizados$ccaa <-
      territorio_normalizado$ccaa

    datos_normalizados$cod_ccaa <-
      territorio_normalizado$cod_ccaa
  }


  # --------------------------------------------------------------------------
  # 7.2. Datasets TERRITORIO
  # --------------------------------------------------------------------------
  #
  # Estos datasets contienen:
  #
  #   - total nacional;
  #   - comunidades autónomas.
  #
  # Nunca se interpretan nombres ambiguos como provincias.
  #

  if (ambito == "TERRITORIO") {

    territorio_normalizado <- normalizar_territorio(
      x = datos_normalizados$territorio,
      diccionario = diccionario_territorial
    )


    # ------------------------------------------------------------------------
    # Comprobar territorios no identificados
    # ------------------------------------------------------------------------

    if (any(!territorio_normalizado$encontrada)) {

      desconocidos <- unique(
        territorio_normalizado$territorio_original[
          !territorio_normalizado$encontrada
        ]
      )

      desconocidos <- desconocidos[
        !is.na(desconocidos)
      ]

      stop(
        sprintf(
          paste0(
            "El dataset '%s' contiene territorios no reconocidos: %s"
          ),
          id_dataset,
          paste(
            desconocidos,
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }


    # ------------------------------------------------------------------------
    # Incorporar variables territoriales normalizadas
    # ------------------------------------------------------------------------

    datos_normalizados$territorio <-
      territorio_normalizado$territorio

    datos_normalizados$tipo_territorio <-
      territorio_normalizado$tipo_territorio

    datos_normalizados$cod_ccaa <-
      territorio_normalizado$cod_ccaa

    datos_normalizados$ccaa <-
      territorio_normalizado$ccaa

    datos_normalizados$cod_provincia <-
      territorio_normalizado$cod_provincia

    datos_normalizados$provincia <-
      territorio_normalizado$provincia
  }


  # --------------------------------------------------------------------------
  # 7.3. Datasets TERRITORIOPARCIAL
  # --------------------------------------------------------------------------

  if (ambito == "TERRITORIOPARCIAL") {

    territorio_normalizado <- normalizar_territorioparcial(
      x = datos_normalizados$territorio,
      diccionario = diccionario_territorial
    )

    # ------------------------------------------------------------------------
    # Comprobar territorios no identificados
    # ------------------------------------------------------------------------

    if (any(!territorio_normalizado$encontrada)) {

      desconocidos <- unique(
        territorio_normalizado$territorio_original[
          !territorio_normalizado$encontrada
        ]
      )

      desconocidos <- desconocidos[
        !is.na(desconocidos)
      ]

      stop(
        sprintf(
          paste0(
            "El dataset '%s' contiene territorios no reconocidos: %s"
          ),
          id_dataset,
          paste(
            desconocidos,
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }

    # ------------------------------------------------------------------------
    # Incorporar variables territoriales normalizadas
    # ------------------------------------------------------------------------

    datos_normalizados$territorio <-
      territorio_normalizado$territorio

    datos_normalizados$tipo_territorio <-
      territorio_normalizado$tipo_territorio

    datos_normalizados$cod_ccaa <-
      territorio_normalizado$cod_ccaa

    datos_normalizados$ccaa <-
      territorio_normalizado$ccaa

    datos_normalizados$cod_provincia <-
      territorio_normalizado$cod_provincia

    datos_normalizados$provincia <-
      territorio_normalizado$provincia
  }


  # --------------------------------------------------------------------------
  # 7.4. Datasets TERRITORIOPROV
  # --------------------------------------------------------------------------

  if (ambito == "TERRITORIOPROV") {

    if (!exists("cod_ccaa_prov")) {
      stop(
        paste0(
          "No existe `cod_ccaa_prov`. ",
          "Debe cargarse antes de normalizar datasets TERRITORIOPROV."
        ),
        call. = FALSE
      )
    }

    territorio_normalizado <- normalizar_territorioprov(
      x = datos_normalizados$territorio,
      diccionario = diccionario_territorial,
      cod_ccaa_prov = cod_ccaa_prov
    )


    # ------------------------------------------------------------------------
    # Comprobar territorios no identificados
    # ------------------------------------------------------------------------

    if (any(!territorio_normalizado$encontrada)) {

      desconocidos <- unique(
        territorio_normalizado$territorio_original[
          !territorio_normalizado$encontrada
        ]
      )

      desconocidos <- desconocidos[
        !is.na(desconocidos)
      ]

      stop(
        sprintf(
          paste0(
            "El dataset '%s' contiene territorios no reconocidos: %s"
          ),
          id_dataset,
          paste(
            desconocidos,
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }


    # ------------------------------------------------------------------------
    # Incorporar variables territoriales normalizadas
    # ------------------------------------------------------------------------

    datos_normalizados$territorio <-
      territorio_normalizado$territorio

    datos_normalizados$tipo_territorio <-
      territorio_normalizado$tipo_territorio

    datos_normalizados$cod_ccaa <-
      territorio_normalizado$cod_ccaa

    datos_normalizados$ccaa <-
      territorio_normalizado$ccaa

    datos_normalizados$cod_provincia <-
      territorio_normalizado$cod_provincia

    datos_normalizados$provincia <-
      territorio_normalizado$provincia
  }

  # --------------------------------------------------------------------------
  # 7.5. Datasets CV
  # --------------------------------------------------------------------------

  if (ambito == "CV") {

    territorio_normalizado <- normalizar_cv(
      x = datos_normalizados$territorio,
      diccionario = diccionario_territorial
    )

    if (any(!territorio_normalizado$encontrada)) {
      stop(
        sprintf(
          "El dataset '%s' contiene territorios CV no reconocidos.",
          id_dataset
        ),
        call. = FALSE
      )
    }

    datos_normalizados$territorio <-
      territorio_normalizado$territorio

    datos_normalizados$tipo_territorio <-
      territorio_normalizado$tipo_territorio

    datos_normalizados$cod_ccaa <-
      territorio_normalizado$cod_ccaa

    datos_normalizados$ccaa <-
      territorio_normalizado$ccaa

    datos_normalizados$cod_provincia <-
      territorio_normalizado$cod_provincia

    datos_normalizados$provincia <-
      territorio_normalizado$provincia
  }

  # --------------------------------------------------------------------------
  # 7.6. Datasets CV
  # --------------------------------------------------------------------------

  if (ambito == "CVPROV") {

    territorio_normalizado <- normalizar_cvprov(
      x = datos_normalizados$territorio,
      diccionario = diccionario_territorial,
      cod_ccaa_prov = cod_ccaa_prov
    )

    if (any(!territorio_normalizado$encontrada)) {
      stop(
        sprintf(
          "El dataset '%s' contiene territorios CVPROV no reconocidos.",
          id_dataset
        ),
        call. = FALSE
      )
    }

    datos_normalizados$territorio <-
      territorio_normalizado$territorio

    datos_normalizados$tipo_territorio <-
      territorio_normalizado$tipo_territorio

    datos_normalizados$cod_ccaa <-
      territorio_normalizado$cod_ccaa

    datos_normalizados$ccaa <-
      territorio_normalizado$ccaa

    datos_normalizados$cod_provincia <-
      territorio_normalizado$cod_provincia

    datos_normalizados$provincia <-
      territorio_normalizado$provincia
  }


  # ==========================================================================
  # 8. Homogeneizar dimensiones descriptivas
  # ==========================================================================
  #
  # Las dimensiones se normalizan únicamente cuando están presentes en el
  # dataset.
  #
  # Cada dimensión dispone de su propia función de normalización, lo que
  # permite mantener reglas explícitas y detectar categorías nuevas.
  # ==========================================================================


  # --------------------------------------------------------------------------
  # 8.1. Sexo
  # --------------------------------------------------------------------------

  if ("sexo" %in% names(datos_normalizados)) {

    datos_normalizados$sexo <- normalizar_sexo(
      datos_normalizados$sexo
    )
  }

  # --------------------------------------------------------------------------
  # 8.2. Contrato
  # --------------------------------------------------------------------------

  if ("contrato" %in% names(datos_normalizados)) {

    datos_normalizados$contrato <- normalizar_contrato(
      datos_normalizados$contrato
    )
  }

  # --------------------------------------------------------------------------
  # 8.3. Jornada
  # --------------------------------------------------------------------------

  if ("jornada" %in% names(datos_normalizados)) {

    datos_normalizados$jornada <- normalizar_jornada(
      datos_normalizados$jornada
    )
  }

  # --------------------------------------------------------------------------
  # 8.4. Ocupación
  # --------------------------------------------------------------------------

  if ("ocupacion" %in% names(datos_normalizados)) {

    datos_normalizados$ocupacion <- normalizar_ocupacion(
      datos_normalizados$ocupacion
    )
  }

  # --------------------------------------------------------------------------
  # 8.5. Nacionalidad
  # --------------------------------------------------------------------------

  if ("nacionalidad" %in% names(datos_normalizados)) {

    datos_normalizados$nacionalidad <- normalizar_nacionalidad(
      datos_normalizados$nacionalidad
    )
  }

  # --------------------------------------------------------------------------
  # 8.6. Orden de nacimiento
  # --------------------------------------------------------------------------

  if ("ordennacimiento" %in% names(datos_normalizados)) {

    datos_normalizados$ordennacimiento <- normalizar_ordennacimiento(
      datos_normalizados$ordennacimiento
    )
  }

  # --------------------------------------------------------------------------
  # 8.7. Sector 2
  # --------------------------------------------------------------------------

  if ("sect2" %in% names(datos_normalizados)) {

    datos_normalizados$sect2 <- normalizar_sect2(
      datos_normalizados$sect2
    )
  }

  # --------------------------------------------------------------------------
  # 8.8. Sector 3
  # --------------------------------------------------------------------------

  if ("sect3" %in% names(datos_normalizados)) {

    datos_normalizados$sect3 <- normalizar_sect3(
      datos_normalizados$sect3
    )
  }

  # --------------------------------------------------------------------------
  # 8.9. Tamaño de empresa
  # --------------------------------------------------------------------------

  if ("tamanyoemp" %in% names(datos_normalizados)) {

    datos_normalizados$tamanyoemp <- normalizar_tamanyoemp(
      datos_normalizados$tamanyoemp
    )
  }

  # --------------------------------------------------------------------------
  # 8.10. Tipo de vivienda
  # --------------------------------------------------------------------------

  if ("tipovivienda" %in% names(datos_normalizados)) {

    datos_normalizados$tipovivienda <- normalizar_tipovivienda(
      datos_normalizados$tipovivienda
    )
  }

  # --------------------------------------------------------------------------
  # 8.11. Secciones
  # --------------------------------------------------------------------------

  if ("secciones" %in% names(datos_normalizados)) {

    datos_normalizados$secciones <- normalizar_secciones(
      datos_normalizados$secciones
    )
  }

  # --------------------------------------------------------------------------
  # 8.12. Gastos
  # --------------------------------------------------------------------------

  if ("gastos" %in% names(datos_normalizados)) {

    datos_normalizados$gastos <- normalizar_gastos(
      datos_normalizados$gastos
    )
  }

  # --------------------------------------------------------------------------
  # 8.13. Actividades
  # --------------------------------------------------------------------------

  if ("actividades" %in% names(datos_normalizados)) {

    datos_normalizados$actividades <- normalizar_actividades(
      datos_normalizados$actividades
    )
  }

  # --------------------------------------------------------------------------
  # 8.14. Destino
  # --------------------------------------------------------------------------

  if ("destino" %in% names(datos_normalizados)) {

    datos_normalizados$destino <- normalizar_destino(
      datos_normalizados$destino
    )
  }

  # --------------------------------------------------------------------------
  # 8.15. Nivel educativo
  # --------------------------------------------------------------------------

  if ("niveleducativo" %in% names(datos_normalizados)) {

    nivel_normalizado <- normalizar_niveleducativo(
      datos_normalizados$niveleducativo
    )

    datos_normalizados$niveleducativo <-
      nivel_normalizado$niveleducativo

    datos_normalizados$niveleducativo_4 <-
      nivel_normalizado$niveleducativo_4
  }

  # --------------------------------------------------------------------------
  # 8.16. Edad
  # --------------------------------------------------------------------------

  if ("edad" %in% names(datos_normalizados)) {

    edad_normalizada <- normalizar_edad(
      datos_normalizados$edad
    )

    datos_normalizados$edad <-
      edad_normalizada$edad

    datos_normalizados$edad_min <-
      edad_normalizada$edad_min

    datos_normalizados$edad_max <-
      edad_normalizada$edad_max

    datos_normalizados$edad_tipo <-
      edad_normalizada$edad_tipo
  }

  # --------------------------------------------------------------------------
  # 8.17. Tamaño de municipio
  # --------------------------------------------------------------------------

  if ("tamanyomuni" %in% names(datos_normalizados)) {

    tamanyomuni_normalizado <- normalizar_tamanyomuni(
      datos_normalizados$tamanyomuni
    )

    datos_normalizados$tamanyomuni <-
      tamanyomuni_normalizado$tamanyomuni

    datos_normalizados$tamanyomuni_min <-
      tamanyomuni_normalizado$tamanyomuni_min

    datos_normalizados$tamanyomuni_max <-
      tamanyomuni_normalizado$tamanyomuni_max
  }

  # --------------------------------------------------------------------------
  # 8.18. Producto
  # --------------------------------------------------------------------------

  if ("prod" %in% names(datos_normalizados)) {

    prod_normalizado <- normalizar_prod(
      datos_normalizados$prod
    )

    datos_normalizados$prod <-
      prod_normalizado$prod

    datos_normalizados$prod_codigo <-
      prod_normalizado$prod_codigo
  }







  # ------------------------------------------------------------------------
  # REVISAR LA NUMERACIÓN DESPUÉS DE HOMOGENEIZAR TODAS LAS VARIABLES
  # ------------------------------------------------------------------------


  # ==========================================================================
  # 8. Validar nuevamente los nombres temporales
  # ==========================================================================
  #
  # Aunque los datasets ya se auditan previamente, la función se protege
  # frente a entradas incorrectas si se utiliza aisladamente.
  #

  patron_periodo <- switch(
    frecuencia,

    A = "^[0-9]{4}$",

    T = "^[0-9]{4}T[1-4]$",

    M = "^[0-9]{4}M(0?[1-9]|1[0-2])$"
  )

  periodos_validos <- grepl(
    patron_periodo,
    nombres_periodo
  )

  if (any(!periodos_validos)) {

    invalidos <- nombres_periodo[
      !periodos_validos
    ]

    stop(
      sprintf(
        paste0(
          "El dataset '%s' contiene columnas temporales no válidas: %s"
        ),
        id_dataset,
        paste(
          invalidos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }


  # ==========================================================================
  # 9. Transformar de formato ancho a formato largo
  # ==========================================================================
  #
  # Ejemplo:
  #
  #   territorio    sexo      2023T1   2023T2
  #   Valencia      Mujeres      51.2     52.4
  #
  # pasa a:
  #
  #   territorio    sexo      periodo   valor
  #   Valencia      Mujeres   2023T1     51.2
  #   Valencia      Mujeres   2023T2     52.4
  #
  # Convertimos inicialmente `valor` a character para poder detectar de forma
  # controlada cualquier contenido no numérico procedente de los Excel.
  #

  datos_long <- tidyr::pivot_longer(
    datos_normalizados,
    cols = tidyselect::all_of(nombres_periodo),
    names_to = "periodo_original",
    values_to = "valor",
    values_transform = list(
      valor = as.character
    )
  )


  # ==========================================================================
  # 10. Comprobar y convertir la variable valor
  # ==========================================================================
  #
  # El contrato de datos del paquete establece que `valor` debe ser numeric.
  #
  # Algunas fuentes utilizan códigos textuales para representar ausencia de
  # información. En los datasets auditados se ha detectado:
  #
  #   ".." -> dato no disponible / no informado
  #
  # Este código se transforma explícitamente en NA_real_.
  #
  # Cualquier otro contenido no numérico seguirá considerándose inesperado y
  # provocará un error, para evitar conversiones silenciosas que puedan ocultar
  # problemas en los datos fuente.
  #

  valor_original <- datos_long$valor


  # --------------------------------------------------------------------------
  # 10.1. Normalizar espacios
  # --------------------------------------------------------------------------

  valor_limpio <- trimws(
    valor_original
  )


  # --------------------------------------------------------------------------
  # 10.2. Identificar códigos conocidos de ausencia
  # --------------------------------------------------------------------------

  codigos_na <- c(
    "",
    "..",
    ".",
    ","
  )

  es_codigo_na <- !is.na(valor_limpio) &
    valor_limpio %in% codigos_na


  # --------------------------------------------------------------------------
  # 10.3. Convertir los valores restantes a numeric
  # --------------------------------------------------------------------------

  valor_convertir <- valor_limpio

  valor_convertir[es_codigo_na] <- NA_character_

  valor_numerico <- suppressWarnings(
    as.numeric(valor_convertir)
  )


  # --------------------------------------------------------------------------
  # 10.4. Detectar contenidos no numéricos inesperados
  # --------------------------------------------------------------------------
  #
  # Un valor es problemático cuando:
  #
  #   - no era NA originalmente;
  #   - no pertenece a los códigos de ausencia conocidos;
  #   - no puede convertirse a numeric.
  #

  valor_invalido <- !is.na(valor_limpio) &
    !es_codigo_na &
    is.na(valor_numerico)


  if (any(valor_invalido)) {

    valores_problematicos <- sort(
      unique(
        valor_limpio[valor_invalido]
      )
    )

    stop(
      sprintf(
        paste0(
          "El dataset '%s' contiene valores no numéricos no reconocidos ",
          "en las columnas temporales: %s"
        ),
        id_dataset,
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }


  # --------------------------------------------------------------------------
  # 10.5. Asignar el resultado
  # --------------------------------------------------------------------------

  datos_long$valor <- valor_numerico


  # ==========================================================================
  # 11. Extraer el año
  # ==========================================================================

  datos_long$anyo <- as.integer(
    substr(
      datos_long$periodo_original,
      1L,
      4L
    )
  )


  # ==========================================================================
  # 12. Extraer trimestre y mes
  # ==========================================================================

  if (frecuencia == "A") {

    datos_long$trimestre <- NA_integer_
    datos_long$mes <- NA_integer_

  } else if (frecuencia == "T") {

    datos_long$trimestre <- as.integer(
      sub(
        "^.*T",
        "",
        datos_long$periodo_original
      )
    )

    datos_long$mes <- NA_integer_

  } else if (frecuencia == "M") {

    datos_long$trimestre <- NA_integer_

    datos_long$mes <- as.integer(
      sub(
        "^.*M",
        "",
        datos_long$periodo_original
      )
    )
  }


  # ==========================================================================
  # 13. Construir el periodo canónico
  # ==========================================================================
  #
  # Anual:
  #
  #   2024 -> 2024
  #
  # Trimestral:
  #
  #   2024T3 -> 2024T3
  #
  # Mensual:
  #
  #   2024M3  -> 2024M03
  #   2024M03 -> 2024M03
  #

  if (frecuencia == "A") {

    datos_long$periodo <- sprintf(
      "%04d",
      datos_long$anyo
    )

  } else if (frecuencia == "T") {

    datos_long$periodo <- sprintf(
      "%04dT%d",
      datos_long$anyo,
      datos_long$trimestre
    )

  } else if (frecuencia == "M") {

    datos_long$periodo <- sprintf(
      "%04dM%02d",
      datos_long$anyo,
      datos_long$mes
    )
  }


  # Ya no necesitamos conservar el nombre temporal original.
  datos_long$periodo_original <- NULL


  # ==========================================================================
  # 14. Incorporar metadata del indicador
  # ==========================================================================

  fila_metadata <- metadata_tbl[
    metadata_tbl$id_dataset == id_dataset,
    ,
    drop = FALSE
  ]

  if (nrow(fila_metadata) != 1L) {
    stop(
      sprintf(
        paste0(
          "Se esperaba exactamente una fila de metadata para '%s', ",
          "pero se encontraron %s."
        ),
        id_dataset,
        nrow(fila_metadata)
      ),
      call. = FALSE
    )
  }


  datos_long$id_dataset <- id_dataset
  datos_long$variable <- fila_metadata$variable[[1]]
  datos_long$operacion <- fila_metadata$operacion[[1]]
  datos_long$organismo <- fila_metadata$organismo[[1]]
  datos_long$frecuencia <- frecuencia


  # ==========================================================================
  # 15. Ordenar las columnas
  # ==========================================================================
  #
  # El orden final será:
  #
  #   metadatos
  #   dimensiones
  #   tiempo
  #   valor
  #

  columnas_territoriales <- if (ambito == "CCAA") {

    c(
      "ccaa",
      "cod_ccaa"
    )

  } else if (
    ambito %in% c(
      "CV",
      "CVPROV",
      "TERRITORIO",
      "TERRITORIOPARCIAL",
      "TERRITORIOPROV"
    )
  ) {

    c(
      "territorio",
      "tipo_territorio",
      "cod_ccaa",
      "ccaa",
      "cod_provincia",
      "provincia"
    )

  } else {

    "territorio"
  }


  # Las dimensiones adicionales excluyen la dimensión territorial principal.
  columnas_dimensiones_adicionales <- nombres_dimensiones


  # Nivel educativo: clasificación agregada adicional.
  if ("niveleducativo" %in% nombres_dimensiones) {

    columnas_dimensiones_adicionales <- c(
      columnas_dimensiones_adicionales,
      "niveleducativo_4"
    )
  }


  # Edad: representación estructurada del intervalo.
  if ("edad" %in% nombres_dimensiones) {

    columnas_dimensiones_adicionales <- c(
      columnas_dimensiones_adicionales,
      "edad_min",
      "edad_max",
      "edad_tipo"
    )
  }


  # Tamaño municipio
  if ("tamanyomuni" %in% nombres_dimensiones) {

    columnas_dimensiones_adicionales <- c(
      columnas_dimensiones_adicionales,
      "tamanyomuni_min",
      "tamanyomuni_max"
    )
  }

  # Producto (9 productos de Comercio Exterior)
  if ("prod" %in% nombres_dimensiones) {

    columnas_dimensiones_adicionales <- c(
      columnas_dimensiones_adicionales,
      "prod_codigo"
    )
  }


  columnas_finales <- c(
    "id_dataset",
    "variable",
    "operacion",
    "organismo",
    "frecuencia",
    columnas_territoriales,
    columnas_dimensiones_adicionales,
    "periodo",
    "anyo",
    "trimestre",
    "mes",
    "valor"
  )

  datos_long <- datos_long[
    columnas_finales
  ]


  # ==========================================================================
  # 16. Comprobaciones finales
  # ==========================================================================

  stopifnot(
    is.character(datos_long$id_dataset),
    is.character(datos_long$periodo),
    is.integer(datos_long$anyo),
    is.integer(datos_long$trimestre),
    is.integer(datos_long$mes),
    is.numeric(datos_long$valor)
  )


  # ==========================================================================
  # 17. Resultado
  # ==========================================================================

  datos_long
}
