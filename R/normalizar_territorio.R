#' Normalizar texto para comparaciones territoriales
#'
#' Genera una clave textual simplificada para comparar nombres de
#' comunidades autónomas, provincias y otros territorios procedentes de
#' fuentes heterogéneas.
#'
#' La transformación:
#'
#' - convierte el texto a minúsculas;
#' - elimina acentos y otros signos diacríticos;
#' - elimina espacios y signos de puntuación;
#' - conserva únicamente caracteres alfanuméricos.
#'
#' Esta función NO determina equivalencias territoriales por sí misma.
#' Únicamente genera una clave comparable.
#'
#' Por ejemplo:
#'
#' \preformatted{
#' "PAÍS VASCO"          -> "paisvasco"
#' "País Vasco"          -> "paisvasco"
#' "Castilla-La Mancha"  -> "castillalamancha"
#' }
#'
#' @param x Vector de caracteres.
#'
#' @return Vector de caracteres con las claves normalizadas.
#'
#' @keywords internal
#' @noRd
normalizar_clave_territorial <- function(x) {

  if (!is.character(x)) {
    x <- as.character(x)
  }

  # Transliteration a ASCII.
  #
  # Con esto evitamos mantener manualmente sustituciones para á, é, í,
  # ó, ú, ü, ñ, etc.
  x_ascii <- iconv(
    x,
    from = "",
    to = "ASCII//TRANSLIT"
  )

  x_ascii <- tolower(x_ascii)

  # Eliminar cualquier carácter que no sea alfanumérico.
  x_ascii <- gsub(
    "[^a-z0-9]",
    "",
    x_ascii
  )

  x_ascii
}


#' Crear el diccionario territorial del paquete
#'
#' Construye un diccionario de correspondencias entre las distintas
#' denominaciones territoriales que pueden aparecer en las fuentes y
#' los códigos y nombres canónicos utilizados por el paquete.
#'
#' La referencia oficial utilizada es `cod_ccaa_prov`, que contiene:
#'
#' - `CPRO`: código de provincia;
#' - `NPRO`: nombre canónico de provincia;
#' - `CCA`: código de comunidad autónoma;
#' - `NCA`: nombre canónico de comunidad autónoma.
#'
#' Además se incorpora una tabla explícita de alias para denominaciones
#' frecuentes que no pueden resolverse únicamente mediante limpieza
#' ortográfica.
#'
#' @param cod_ccaa_prov Tabla de correspondencias entre provincias y
#'   comunidades autónomas.
#'
#' @return Data frame con el diccionario territorial.
#'
#' @keywords internal
#' @noRd
crear_diccionario_territorial <- function(cod_ccaa_prov) {

  # ==========================================================================
  # 1. Comprobaciones
  # ==========================================================================

  columnas_necesarias <- c(
    "CPRO",
    "NPRO",
    "CCA",
    "NCA"
  )

  faltan <- setdiff(
    columnas_necesarias,
    names(cod_ccaa_prov)
  )

  if (length(faltan) > 0L) {
    stop(
      paste0(
        "`cod_ccaa_prov` no contiene las columnas necesarias: ",
        paste(faltan, collapse = ", ")
      ),
      call. = FALSE
    )
  }


  # ==========================================================================
  # 2. Comunidades autónomas canónicas
  # ==========================================================================

  ccaa <- unique(
    cod_ccaa_prov[
      c(
        "CCA",
        "NCA"
      )
    ]
  )

  names(ccaa) <- c(
    "cod_ccaa",
    "nombre_canonico"
  )

  ccaa$tipo_territorio <- "ccaa"

  ccaa$cod_provincia <- NA_character_

  ccaa$clave <- normalizar_clave_territorial(
    ccaa$nombre_canonico
  )


  # ==========================================================================
  # 3. Alias de comunidades autónomas
  # ==========================================================================
  #
  # Esta tabla contiene únicamente equivalencias semánticas que no se
  # resuelven mediante la normalización ortográfica.
  #
  # El alias se vincula al código, NO directamente al nombre final.
  # De esta forma `cod_ccaa_prov` sigue siendo la fuente de verdad para
  # determinar la denominación canónica.
  #

  alias_ccaa <- data.frame(
    alias = c(
      "ANDALUCIA",
      "ARAGON",
      "ASTURIAS",
      "PRINCIPADO DE ASTURIAS",
      "BALEARES",
      "ILLES BALEARS",
      "CANARIAS",
      "CANTABRIA",
      "CASTILLA Y LEON",
      "CASTILLA LA MANCHA",
      "CASTILLA-LA MANCHA",
      "CATALUNYA",
      "CATALUÑA",
      "COMUNITAT VALENCIANA",
      "COMUNIDAD VALENCIANA",
      "EXTREMADURA",
      "GALICIA",
      "MADRID",
      "COMUNIDAD DE MADRID",
      "MURCIA",
      "REGION DE MURCIA",
      "NAVARRA",
      "COMUNIDAD FORAL DE NAVARRA",
      "PAIS VASCO",
      "EUSKADI",
      "LA RIOJA",
      "CEUTA",
      "MELILLA",
      "NAVARRA, COMUNIDAD FORAL DE",
      "MURCIA, REGIÓN DE",
      "ASTURIAS, PRINCIPADO DE",
      "MADRID, COMUNIDAD DE",
      "BALEARS, ILLES",
      "RIOJA, LA"
    ),
    cod_ccaa = c(
      "01",
      "02",
      "03",
      "03",
      "04",
      "04",
      "05",
      "06",
      "07",
      "08",
      "08",
      "09",
      "09",
      "10",
      "10",
      "11",
      "12",
      "13",
      "13",
      "14",
      "14",
      "15",
      "15",
      "16",
      "16",
      "17",
      "18",
      "19",
      "15",
      "14",
      "03",
      "13",
      "04",
      "17"
    ),
    stringsAsFactors = FALSE
  )

  alias_ccaa$clave <- normalizar_clave_territorial(
    alias_ccaa$alias
  )

  alias_ccaa <- merge(
    alias_ccaa[
      c(
        "clave",
        "cod_ccaa"
      )
    ],
    ccaa[
      c(
        "cod_ccaa",
        "nombre_canonico"
      )
    ],
    by = "cod_ccaa",
    all.x = TRUE,
    sort = FALSE
  )

  alias_ccaa$tipo_territorio <- "ccaa"
  alias_ccaa$cod_provincia <- NA_character_


  # ==========================================================================
  # 4. Provincias canónicas
  # ==========================================================================

  provincias <- cod_ccaa_prov[
    c(
      "CPRO",
      "NPRO",
      "CCA"
    )
  ]

  names(provincias) <- c(
    "cod_provincia",
    "nombre_canonico",
    "cod_ccaa"
  )

  provincias$tipo_territorio <- "provincia"

  provincias$clave <- normalizar_clave_territorial(
    provincias$nombre_canonico
  )


  # ==========================================================================
  # 5. Alias de provincias
  # ==========================================================================
  #
  # Algunas fuentes utilizan denominaciones provinciales distintas de las
  # incluidas en `cod_ccaa_prov`.
  #
  # Estos alias se asignan mediante el código provincial oficial. El nombre
  # canónico final seguirá procediendo de `cod_ccaa_prov`.
  #

  alias_provincias <- data.frame(
    alias = c(
      "A Coruña",
      "Álava",
      "Alicante",
      "Castellón",
      "Las Palmas",
      "Valencia"
    ),
    cod_provincia = c(
      "15",
      "01",
      "03",
      "12",
      "35",
      "46"
    ),
    stringsAsFactors = FALSE
  )

  alias_provincias$clave <- normalizar_clave_territorial(
    alias_provincias$alias
  )

  alias_provincias <- merge(
    alias_provincias[
      c(
        "clave",
        "cod_provincia"
      )
    ],
    provincias[
      c(
        "cod_provincia",
        "cod_ccaa",
        "nombre_canonico"
      )
    ],
    by = "cod_provincia",
    all.x = TRUE,
    sort = FALSE
  )

  alias_provincias$tipo_territorio <- "provincia"


  # ==========================================================================
  # 6. Total nacional
  # ==========================================================================
  #
  # Se incorpora explícitamente porque aparece en numerosos datasets pero
  # evidentemente no pertenece a `cod_ccaa_prov`.
  #

  nacional <- data.frame(
    clave = normalizar_clave_territorial(
      c(
        "TOTAL NACIONAL",
        "TOTAL ESPAÑA",
        "ESPAÑA"
      )
    ),
    tipo_territorio = "nacional",
    cod_ccaa = NA_character_,
    cod_provincia = NA_character_,
    nombre_canonico = "Total nacional",
    stringsAsFactors = FALSE
  )


  # ==========================================================================
  # 7. Categorías territoriales especiales
  # ==========================================================================
  #
  # Algunas fuentes incluyen agregados territoriales que no corresponden a
  # una comunidad autónoma ni a una provincia.
  #

  especiales <- data.frame(
    clave = normalizar_clave_territorial(
      c(
        "EXTRA-REGIO",
        "OTRAS COMUNIDADES"
      )
    ),
    tipo_territorio = c(
      "especial",
      "agregado"
    ),
    cod_ccaa = NA_character_,
    cod_provincia = NA_character_,
    nombre_canonico = c(
      "Extra-Regio",
      "Otras comunidades"
    ),
    stringsAsFactors = FALSE
  )


  # ==========================================================================
  # 8. Unir las distintas fuentes del diccionario
  # ==========================================================================

  columnas_diccionario <- c(
    "clave",
    "tipo_territorio",
    "cod_ccaa",
    "cod_provincia",
    "nombre_canonico"
  )

  diccionario <- rbind(
    ccaa[columnas_diccionario],
    alias_ccaa[columnas_diccionario],
    provincias[columnas_diccionario],
    alias_provincias[columnas_diccionario],
    nacional[columnas_diccionario],
    especiales[columnas_diccionario]
  )


  # ==========================================================================
  # 9. Eliminar duplicados exactos
  # ==========================================================================

  diccionario <- unique(
    diccionario
  )


  # ==========================================================================
  # 10. Detectar claves ambiguas
  # ==========================================================================
  #
  # Es crítico comprobar que una misma clave no corresponda a dos territorios
  # distintos.
  #
  # Por ejemplo, no queremos que una regla basada en "valencia" pueda
  # significar silenciosamente tanto Comunitat Valenciana como la provincia
  # de Valencia.
  #

  destinos <- paste(
    diccionario$tipo_territorio,
    diccionario$cod_ccaa,
    diccionario$cod_provincia,
    sep = "|"
  )

  n_destinos <- tapply(
    destinos,
    diccionario$clave,
    function(x) length(unique(x))
  )

  claves_ambiguas <- names(
    n_destinos[n_destinos > 1L]
  )

  if (length(claves_ambiguas) > 0L) {

    # No detenemos aquí la construcción porque determinadas coincidencias
    # entre CCAA y provincia pueden ser reales, por ejemplo Madrid o Murcia.
    #
    # La resolución se realizará teniendo en cuenta el ámbito esperado
    # del dataset.
    attr(
      diccionario,
      "claves_ambiguas"
    ) <- claves_ambiguas
  }


  # ==========================================================================
  # 11. Resultado
  # ==========================================================================

  rownames(diccionario) <- NULL

  diccionario
}


#' Homogeneizar nombres de comunidades autónomas
#'
#' Sustituye distintas denominaciones de comunidades autónomas por los
#' nombres canónicos presentes en `cod_ccaa_prov` y devuelve además su
#' código.
#'
#' La correspondencia es exacta después de generar la clave normalizada.
#' No se utiliza coincidencia difusa ni búsqueda parcial.
#'
#' @param x Vector de nombres de comunidades autónomas.
#'
#' @param diccionario Diccionario generado mediante
#'   `[crear_diccionario_territorial()]`.
#'
#' @return Data frame con:
#'
#' - `ccaa_original`;
#' - `cod_ccaa`;
#' - `ccaa`;
#' - `encontrada`.
#'
#' @keywords internal
#' @noRd
homogeneizar_ccaa <- function(x, diccionario) {

  x <- as.character(x)

  clave <- normalizar_clave_territorial(x)

  referencia <- diccionario[
    diccionario$tipo_territorio == "ccaa",
    c(
      "clave",
      "cod_ccaa",
      "nombre_canonico"
    )
  ]

  # Puede haber varios alias que produzcan exactamente la misma clave.
  # Nos basta una correspondencia por clave siempre que el destino sea
  # idéntico.
  referencia <- unique(referencia)

  # Comprobación de seguridad.
  destinos_por_clave <- tapply(
    referencia$cod_ccaa,
    referencia$clave,
    function(z) length(unique(z))
  )

  ambiguas <- names(
    destinos_por_clave[destinos_por_clave > 1L]
  )

  if (length(ambiguas) > 0L) {
    stop(
      paste0(
        "El diccionario contiene claves de CCAA ambiguas: ",
        paste(ambiguas, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  referencia <- referencia[
    !duplicated(referencia$clave),
  ]

  pos <- match(
    clave,
    referencia$clave
  )

  data.frame(
    ccaa_original = x,
    cod_ccaa = referencia$cod_ccaa[pos],
    ccaa = referencia$nombre_canonico[pos],
    encontrada = !is.na(pos),
    stringsAsFactors = FALSE
  )
}

#' Clasificar nombres territoriales
#'
#' Identifica una colección de nombres territoriales como total nacional,
#' comunidad autónoma o provincia utilizando el diccionario territorial
#' construido por `[crear_diccionario_territorial()]`.
#'
#' La coincidencia se realiza mediante claves normalizadas exactas.
#'
#' Cuando una misma clave puede representar más de un nivel territorial
#' (por ejemplo, "Madrid" puede referirse a la comunidad autónoma o a la
#' provincia), la función devuelve una fila por posible correspondencia.
#'
#' Esta función se utiliza principalmente para auditar las denominaciones
#' observadas en los datasets fuente antes de automatizar su normalización.
#'
#' @param x Vector de nombres territoriales.
#'
#' @param diccionario Diccionario generado mediante
#'   `[crear_diccionario_territorial()]`.
#'
#' @return Data frame con las columnas:
#'
#' - `territorio_original`;
#' - `clave`;
#' - `tipo_territorio`;
#' - `cod_ccaa`;
#' - `cod_provincia`;
#' - `nombre_canonico`;
#' - `encontrada`.
#'
#' @keywords internal
#' @noRd
clasificar_territorio <- function(x, diccionario) {

  x <- as.character(x)

  claves <- normalizar_clave_territorial(x)

  entrada <- data.frame(
    territorio_original = x,
    clave = claves,
    stringsAsFactors = FALSE
  )

  resultado <- merge(
    entrada,
    diccionario,
    by = "clave",
    all.x = TRUE,
    sort = FALSE
  )

  resultado$encontrada <- !is.na(
    resultado$tipo_territorio
  )

  resultado <- resultado[
    c(
      "territorio_original",
      "clave",
      "tipo_territorio",
      "cod_ccaa",
      "cod_provincia",
      "nombre_canonico",
      "encontrada"
    )
  ]

  rownames(resultado) <- NULL

  resultado
}


#' Normalizar territorios en datasets TERRITORIOPROV
#'
#' Clasifica los territorios presentes en datasets cuyo ámbito es
#' `TERRITORIOPROV`.
#'
#' Estos datasets pueden contener:
#'
#' - total nacional;
#' - comunidades autónomas;
#' - provincias;
#' - comunidades autónomas uniprovinciales.
#'
#' En las comunidades uniprovinciales, una única fila representa
#' simultáneamente el nivel autonómico y provincial. Estas observaciones
#' se clasifican como `"ccaa_provincia"`.
#'
#' La condición de comunidad uniprovincial NO se codifica manualmente.
#' Se deduce de `cod_ccaa_prov`: una CCAA es uniprovincial cuando aparece
#' asociada a un único código provincial.
#'
#' @param x Vector de nombres territoriales.
#'
#' @param diccionario Diccionario generado mediante
#'   `[crear_diccionario_territorial()]`.
#'
#' @param cod_ccaa_prov Tabla maestra con las columnas `CPRO`, `NPRO`,
#'   `CCA` y `NCA`.
#'
#' @return Data frame con una fila por elemento de `x` y las columnas:
#'
#' - `territorio_original`;
#' - `territorio`;
#' - `tipo_territorio`;
#' - `cod_ccaa`;
#' - `ccaa`;
#' - `cod_provincia`;
#' - `provincia`;
#' - `encontrada`.
#'
#' @keywords internal
#' @noRd
normalizar_territorioprov <- function(
    x,
    diccionario,
    cod_ccaa_prov
) {

  # ==========================================================================
  # 1. Preparar entrada
  # ==========================================================================

  x <- as.character(x)

  claves <- normalizar_clave_territorial(x)


  # ==========================================================================
  # 2. Construir referencias canónicas
  # ==========================================================================

  referencia_ccaa <- unique(
    cod_ccaa_prov[
      c(
        "CCA",
        "NCA"
      )
    ]
  )

  names(referencia_ccaa) <- c(
    "cod_ccaa",
    "ccaa"
  )


  referencia_prov <- unique(
    cod_ccaa_prov[
      c(
        "CPRO",
        "NPRO",
        "CCA"
      )
    ]
  )

  names(referencia_prov) <- c(
    "cod_provincia",
    "provincia",
    "cod_ccaa"
  )


  # ==========================================================================
  # 3. Identificar comunidades uniprovinciales
  # ==========================================================================
  #
  # Contamos cuántas provincias tiene asociada cada CCAA.
  #

  n_provincias <- table(
    cod_ccaa_prov$CCA
  )

  codigos_uniprovinciales <- names(
    n_provincias[n_provincias == 1L]
  )


  # ==========================================================================
  # 4. Preparar resultado vacío
  # ==========================================================================

  resultado <- data.frame(
    territorio_original = x,
    territorio = NA_character_,
    tipo_territorio = NA_character_,
    cod_ccaa = NA_character_,
    ccaa = NA_character_,
    cod_provincia = NA_character_,
    provincia = NA_character_,
    encontrada = FALSE,
    stringsAsFactors = FALSE
  )


  # ==========================================================================
  # 5. Resolver cada territorio
  # ==========================================================================
  #
  # Se procesa elemento a elemento porque necesitamos resolver explícitamente
  # la jerarquía CCAA/provincia.
  #

  for (i in seq_along(x)) {

    clave_i <- claves[i]

    # ------------------------------------------------------------------------
    # Ignorar territorios ausentes
    # ------------------------------------------------------------------------
    #
    # Un valor NA o vacío no representa ningún territorio y no debe
    # compararse con el diccionario.
    #

    if (
      is.na(clave_i) ||
      clave_i == ""
    ) {
      next
    }



    # ------------------------------------------------------------------------
    # 5.1. Buscar todas las coincidencias del diccionario
    # ------------------------------------------------------------------------

    candidatos <- diccionario[
      diccionario$clave == clave_i,
      ,
      drop = FALSE
    ]

    if (nrow(candidatos) == 0L) {
      next
    }


    # ------------------------------------------------------------------------
    # 5.2. Total nacional
    # ------------------------------------------------------------------------

    candidato_nacional <- candidatos[
      candidatos$tipo_territorio == "nacional",
      ,
      drop = FALSE
    ]

    if (nrow(candidato_nacional) > 0L) {

      resultado$territorio[i] <-
        candidato_nacional$nombre_canonico[1]

      resultado$tipo_territorio[i] <- "nacional"

      resultado$encontrada[i] <- TRUE

      next
    }


    # ------------------------------------------------------------------------
    # 5.3. Categorías especiales o agregadas
    # ------------------------------------------------------------------------

    candidato_especial <- candidatos[
      candidatos$tipo_territorio %in% c(
        "especial",
        "agregado"
      ),
      ,
      drop = FALSE
    ]

    if (nrow(candidato_especial) > 0L) {

      resultado$territorio[i] <-
        candidato_especial$nombre_canonico[1]

      resultado$tipo_territorio[i] <-
        candidato_especial$tipo_territorio[1]

      resultado$encontrada[i] <- TRUE

      next
    }


    # ------------------------------------------------------------------------
    # 5.4. Intentar interpretar primero como CCAA
    # ------------------------------------------------------------------------
    #
    # Esto es importante en TERRITORIOPROV.
    #
    # Cuando aparece "MADRID", la fila de la fuente representa la CCAA.
    # Si además la CCAA es uniprovincial, la misma observación representa
    # también la provincia.
    #

    candidato_ccaa <- candidatos[
      candidatos$tipo_territorio == "ccaa",
      ,
      drop = FALSE
    ]

    if (nrow(candidato_ccaa) > 0L) {

      cod_ccaa_i <- candidato_ccaa$cod_ccaa[1]

      nombre_ccaa <- referencia_ccaa$ccaa[
        match(
          cod_ccaa_i,
          referencia_ccaa$cod_ccaa
        )
      ]


      resultado$cod_ccaa[i] <- cod_ccaa_i
      resultado$ccaa[i] <- nombre_ccaa
      resultado$territorio[i] <- nombre_ccaa


      # ----------------------------------------------------------------------
      # CCAA uniprovincial
      # ----------------------------------------------------------------------

      if (cod_ccaa_i %in% codigos_uniprovinciales) {

        provincia_i <- referencia_prov[
          referencia_prov$cod_ccaa == cod_ccaa_i,
          ,
          drop = FALSE
        ]

        resultado$tipo_territorio[i] <- "ccaa_provincia"

        resultado$cod_provincia[i] <-
          provincia_i$cod_provincia[1]

        resultado$provincia[i] <-
          provincia_i$provincia[1]

      } else {

        resultado$tipo_territorio[i] <- "ccaa"
      }


      resultado$encontrada[i] <- TRUE

      next
    }


    # ------------------------------------------------------------------------
    # 5.5. Interpretar como provincia
    # ------------------------------------------------------------------------

    candidato_prov <- candidatos[
      candidatos$tipo_territorio == "provincia",
      ,
      drop = FALSE
    ]

    if (nrow(candidato_prov) > 0L) {

      cod_provincia_i <-
        candidato_prov$cod_provincia[1]

      provincia_i <- referencia_prov[
        referencia_prov$cod_provincia == cod_provincia_i,
        ,
        drop = FALSE
      ]

      cod_ccaa_i <- provincia_i$cod_ccaa[1]

      nombre_ccaa <- referencia_ccaa$ccaa[
        match(
          cod_ccaa_i,
          referencia_ccaa$cod_ccaa
        )
      ]


      resultado$territorio[i] <-
        provincia_i$provincia[1]

      resultado$tipo_territorio[i] <- "provincia"

      resultado$cod_ccaa[i] <- cod_ccaa_i

      resultado$ccaa[i] <- nombre_ccaa

      resultado$cod_provincia[i] <-
        cod_provincia_i

      resultado$provincia[i] <-
        provincia_i$provincia[1]

      resultado$encontrada[i] <- TRUE
    }
  }


  # ==========================================================================
  # 6. Resultado
  # ==========================================================================

  resultado
}

#' Normalizar territorios en datasets TERRITORIO
#'
#' Clasifica y homogeneiza los nombres territoriales presentes en datasets
#' cuyo ámbito es `TERRITORIO`.
#'
#' Según la nomenclatura definida para el proyecto, estos datasets contienen:
#'
#' - total nacional;
#' - comunidades autónomas.
#'
#' No contienen provincias.
#'
#' Esto es especialmente importante para las comunidades autónomas
#' uniprovinciales. Por ejemplo, `"MADRID"` puede coincidir tanto con la
#' Comunidad de Madrid como con la provincia de Madrid en el diccionario
#' general. En un dataset `TERRITORIO`, la interpretación correcta es siempre
#' la comunidad autónoma.
#'
#' La función utiliza:
#'
#' - `homogeneizar_ccaa()` para las comunidades autónomas;
#' - el diccionario territorial para reconocer el total nacional.
#'
#' No utiliza coincidencias difusas.
#'
#' @param x Vector de nombres territoriales.
#'
#' @param diccionario Diccionario generado mediante
#'   `[crear_diccionario_territorial()]`.
#'
#' @return Data frame con una fila por elemento de `x` y las columnas:
#'
#' - `territorio_original`;
#' - `territorio`;
#' - `tipo_territorio`;
#' - `cod_ccaa`;
#' - `ccaa`;
#' - `cod_provincia`;
#' - `provincia`;
#' - `encontrada`.
#'
#' @keywords internal
#' @noRd
normalizar_territorio <- function(
    x,
    diccionario
) {

  # ==========================================================================
  # 1. Preparar entrada
  # ==========================================================================

  x <- as.character(x)

  claves <- normalizar_clave_territorial(x)


  # ==========================================================================
  # 2. Preparar resultado
  # ==========================================================================

  resultado <- data.frame(
    territorio_original = x,
    territorio = NA_character_,
    tipo_territorio = NA_character_,
    cod_ccaa = NA_character_,
    ccaa = NA_character_,
    cod_provincia = NA_character_,
    provincia = NA_character_,
    encontrada = FALSE,
    stringsAsFactors = FALSE
  )


  # ==========================================================================
  # 3. Identificar total nacional
  # ==========================================================================

  referencia_nacional <- diccionario[
    diccionario$tipo_territorio == "nacional",
    ,
    drop = FALSE
  ]

  claves_nacional <- unique(
    referencia_nacional$clave
  )

  es_nacional <- !is.na(claves) &
    claves %in% claves_nacional


  if (any(es_nacional)) {

    resultado$territorio[es_nacional] <- "Total nacional"

    resultado$tipo_territorio[es_nacional] <- "nacional"

    resultado$encontrada[es_nacional] <- TRUE
  }


  # ==========================================================================
  # 4. Identificar comunidades autónomas
  # ==========================================================================
  #
  # Todo valor no nacional debe corresponder, por definición del ámbito
  # TERRITORIO, a una comunidad autónoma.
  #

  es_candidata_ccaa <- !es_nacional &
    !is.na(x) &
    trimws(x) != ""


  if (any(es_candidata_ccaa)) {

    ccaa_normalizadas <- homogeneizar_ccaa(
      x[es_candidata_ccaa],
      diccionario
    )

    resultado$territorio[es_candidata_ccaa] <-
      ccaa_normalizadas$ccaa

    resultado$tipo_territorio[es_candidata_ccaa] <-
      ifelse(
        ccaa_normalizadas$encontrada,
        "ccaa",
        NA_character_
      )

    resultado$cod_ccaa[es_candidata_ccaa] <-
      ccaa_normalizadas$cod_ccaa

    resultado$ccaa[es_candidata_ccaa] <-
      ccaa_normalizadas$ccaa

    resultado$encontrada[es_candidata_ccaa] <-
      ccaa_normalizadas$encontrada
  }


  # ==========================================================================
  # 5. Resultado
  # ==========================================================================

  resultado
}

#' Normalizar territorios en datasets TERRITORIOPARCIAL
#'
#' Clasifica y homogeneiza los nombres territoriales presentes en datasets
#' cuyo ámbito es `TERRITORIOPARCIAL`.
#'
#' Según la nomenclatura del proyecto, estos datasets contienen:
#'
#' - total nacional;
#' - algunas comunidades autónomas;
#' - una categoría agregada de otras comunidades.
#'
#' No contienen provincias.
#'
#' Por tanto, nombres ambiguos como `"MADRID"` se interpretan siempre como
#' comunidad autónoma.
#'
#' @param x Vector de nombres territoriales.
#'
#' @param diccionario Diccionario generado mediante
#'   `[crear_diccionario_territorial()]`.
#'
#' @return Data frame con una fila por elemento de `x` y las columnas:
#'
#' - `territorio_original`;
#' - `territorio`;
#' - `tipo_territorio`;
#' - `cod_ccaa`;
#' - `ccaa`;
#' - `cod_provincia`;
#' - `provincia`;
#' - `encontrada`.
#'
#' @keywords internal
#' @noRd
normalizar_territorioparcial <- function(
    x,
    diccionario
) {

  # ==========================================================================
  # 1. Preparar entrada
  # ==========================================================================

  x <- as.character(x)

  claves <- normalizar_clave_territorial(x)


  # ==========================================================================
  # 2. Preparar resultado
  # ==========================================================================

  resultado <- data.frame(
    territorio_original = x,
    territorio = NA_character_,
    tipo_territorio = NA_character_,
    cod_ccaa = NA_character_,
    ccaa = NA_character_,
    cod_provincia = NA_character_,
    provincia = NA_character_,
    encontrada = FALSE,
    stringsAsFactors = FALSE
  )


  # ==========================================================================
  # 3. Identificar total nacional
  # ==========================================================================

  referencia_nacional <- diccionario[
    diccionario$tipo_territorio == "nacional",
    ,
    drop = FALSE
  ]

  claves_nacional <- unique(
    referencia_nacional$clave
  )

  es_nacional <- !is.na(claves) &
    claves %in% claves_nacional

  if (any(es_nacional)) {

    resultado$territorio[es_nacional] <- "Total nacional"

    resultado$tipo_territorio[es_nacional] <- "nacional"

    resultado$encontrada[es_nacional] <- TRUE
  }


  # ==========================================================================
  # 4. Identificar agregado "OTRAS COMUNIDADES"
  # ==========================================================================

  referencia_agregado <- diccionario[
    diccionario$tipo_territorio == "agregado",
    ,
    drop = FALSE
  ]

  claves_agregado <- unique(
    referencia_agregado$clave
  )

  es_agregado <- !is.na(claves) &
    claves %in% claves_agregado

  if (any(es_agregado)) {

    posiciones <- which(es_agregado)

    for (i in posiciones) {

      candidato <- referencia_agregado[
        referencia_agregado$clave == claves[i],
        ,
        drop = FALSE
      ]

      resultado$territorio[i] <-
        candidato$nombre_canonico[1]

      resultado$tipo_territorio[i] <- "agregado"

      resultado$encontrada[i] <- TRUE
    }
  }


  # ==========================================================================
  # 5. Identificar comunidades autónomas
  # ==========================================================================
  #
  # Todo valor que no sea nacional ni agregado debe corresponder a una CCAA.
  #

  es_candidata_ccaa <- !es_nacional &
    !es_agregado &
    !is.na(x) &
    trimws(x) != ""

  if (any(es_candidata_ccaa)) {

    ccaa_normalizadas <- homogeneizar_ccaa(
      x[es_candidata_ccaa],
      diccionario
    )

    resultado$territorio[es_candidata_ccaa] <-
      ccaa_normalizadas$ccaa

    resultado$tipo_territorio[es_candidata_ccaa] <-
      ifelse(
        ccaa_normalizadas$encontrada,
        "ccaa",
        NA_character_
      )

    resultado$cod_ccaa[es_candidata_ccaa] <-
      ccaa_normalizadas$cod_ccaa

    resultado$ccaa[es_candidata_ccaa] <-
      ccaa_normalizadas$ccaa

    resultado$encontrada[es_candidata_ccaa] <-
      ccaa_normalizadas$encontrada
  }


  # ==========================================================================
  # 6. Resultado
  # ==========================================================================

  resultado
}

#' Normalizar territorios en datasets CV
#'
#' Normaliza la dimensión territorial de datasets cuyo ámbito es `CV`.
#'
#' Estos datasets contienen exclusivamente información de la
#' Comunitat Valenciana.
#'
#' La denominación original se homogeneiza al nombre canónico presente en
#' `cod_ccaa_prov` y se asigna el código autonómico correspondiente.
#'
#' @param x Vector de nombres territoriales.
#'
#' @param diccionario Diccionario generado mediante
#'   `[crear_diccionario_territorial()]`.
#'
#' @return Data frame con una fila por elemento de `x`.
#'
#' @keywords internal
#' @noRd
normalizar_cv <- function(
    x,
    diccionario
) {

  resultado_ccaa <- homogeneizar_ccaa(
    x,
    diccionario
  )

  resultado <- data.frame(
    territorio_original = resultado_ccaa$ccaa_original,
    territorio = resultado_ccaa$ccaa,
    tipo_territorio = ifelse(
      resultado_ccaa$encontrada,
      "ccaa",
      NA_character_
    ),
    cod_ccaa = resultado_ccaa$cod_ccaa,
    ccaa = resultado_ccaa$ccaa,
    cod_provincia = NA_character_,
    provincia = NA_character_,
    encontrada = resultado_ccaa$encontrada,
    stringsAsFactors = FALSE
  )

  resultado
}


#' Normalizar territorios en datasets CVPROV
#'
#' Normaliza la dimensión territorial de datasets cuyo ámbito es `CVPROV`.
#'
#' Estos datasets contienen:
#'
#' - Comunitat Valenciana;
#' - Alicante/Alacant;
#' - Castellón/Castelló;
#' - Valencia/València.
#'
#' La CCAA se clasifica como `"ccaa"` y las provincias como `"provincia"`.
#'
#' @param x Vector de nombres territoriales.
#'
#' @param diccionario Diccionario generado mediante
#'   `[crear_diccionario_territorial()]`.
#'
#' @param cod_ccaa_prov Tabla maestra de códigos territoriales.
#'
#' @return Data frame con una fila por elemento de `x`.
#'
#' @keywords internal
#' @noRd
normalizar_cvprov <- function(
    x,
    diccionario,
    cod_ccaa_prov
) {

  x <- as.character(x)

  clasificacion <- clasificar_territorio(
    x = x,
    diccionario = diccionario
  )

  resultado <- data.frame(
    territorio_original = x,
    territorio = NA_character_,
    tipo_territorio = NA_character_,
    cod_ccaa = NA_character_,
    ccaa = NA_character_,
    cod_provincia = NA_character_,
    provincia = NA_character_,
    encontrada = FALSE,
    stringsAsFactors = FALSE
  )

  referencia_ccaa <- unique(
    cod_ccaa_prov[
      c(
        "CCA",
        "NCA"
      )
    ]
  )

  names(referencia_ccaa) <- c(
    "cod_ccaa",
    "ccaa"
  )

  referencia_prov <- cod_ccaa_prov[
    c(
      "CPRO",
      "NPRO",
      "CCA"
    )
  ]

  names(referencia_prov) <- c(
    "cod_provincia",
    "provincia",
    "cod_ccaa"
  )

  for (i in seq_along(x)) {

    candidatos <- clasificacion[
      clasificacion$territorio_original == x[i] &
        clasificacion$encontrada,
      ,
      drop = FALSE
    ]

    if (nrow(candidatos) == 0L) {
      next
    }

    candidato_ccaa <- candidatos[
      candidatos$tipo_territorio == "ccaa",
      ,
      drop = FALSE
    ]

    if (nrow(candidato_ccaa) > 0L) {

      cod_ccaa_i <- candidato_ccaa$cod_ccaa[1]

      nombre_ccaa <- referencia_ccaa$ccaa[
        match(
          cod_ccaa_i,
          referencia_ccaa$cod_ccaa
        )
      ]

      resultado$territorio[i] <- nombre_ccaa
      resultado$tipo_territorio[i] <- "ccaa"
      resultado$cod_ccaa[i] <- cod_ccaa_i
      resultado$ccaa[i] <- nombre_ccaa
      resultado$encontrada[i] <- TRUE

      next
    }

    candidato_prov <- candidatos[
      candidatos$tipo_territorio == "provincia",
      ,
      drop = FALSE
    ]

    if (nrow(candidato_prov) > 0L) {

      cod_provincia_i <- candidato_prov$cod_provincia[1]

      provincia_i <- referencia_prov[
        referencia_prov$cod_provincia == cod_provincia_i,
        ,
        drop = FALSE
      ]

      cod_ccaa_i <- provincia_i$cod_ccaa[1]

      nombre_ccaa <- referencia_ccaa$ccaa[
        match(
          cod_ccaa_i,
          referencia_ccaa$cod_ccaa
        )
      ]

      resultado$territorio[i] <- provincia_i$provincia[1]
      resultado$tipo_territorio[i] <- "provincia"
      resultado$cod_ccaa[i] <- cod_ccaa_i
      resultado$ccaa[i] <- nombre_ccaa
      resultado$cod_provincia[i] <- cod_provincia_i
      resultado$provincia[i] <- provincia_i$provincia[1]
      resultado$encontrada[i] <- TRUE
    }
  }

  resultado
}
