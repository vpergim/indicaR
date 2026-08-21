#' Normalizar la dimensión sexo
#'
#' Homogeneiza los valores de la dimensión `sexo` utilizados por los
#' distintos datasets del paquete.
#'
#' Actualmente las fuentes contienen únicamente:
#'
#' - `"Hombres"`
#' - `"Mujeres"`
#'
#' La función utiliza correspondencias explícitas y exactas después de una
#' normalización textual básica. No se utilizan coincidencias difusas.
#'
#' Esta estrategia permite:
#'
#' - detectar nuevas categorías inesperadas;
#' - evitar correcciones silenciosas;
#' - mantener documentadas las equivalencias admitidas por el paquete.
#'
#' @param x Vector de caracteres con valores de sexo.
#'
#' @return Vector de caracteres con los valores normalizados.
#'
#' @keywords internal
#' @noRd
normalizar_sexo <- function(x) {

  # ==========================================================================
  # 1. Preparar entrada
  # ==========================================================================

  x <- as.character(x)


  # ==========================================================================
  # 2. Generar una clave comparable
  # ==========================================================================
  #
  # Reutilizamos la función de normalización textual ya empleada para los
  # territorios. Para estos valores:
  #
  #   "Hombres" -> "hombres"
  #   "Mujeres" -> "mujeres"
  #

  clave <- normalizar_clave_territorial(x)


  # ==========================================================================
  # 3. Diccionario explícito
  # ==========================================================================

  diccionario <- c(
    hombres = "Hombres",
    mujeres = "Mujeres"
  )


  # ==========================================================================
  # 4. Resolver valores
  # ==========================================================================

  resultado <- unname(
    diccionario[clave]
  )


  # ==========================================================================
  # 5. Detectar valores no reconocidos
  # ==========================================================================
  #
  # Los NA originales se conservan como NA.
  #
  # Cualquier otro valor desconocido provoca un error para evitar que una
  # nueva categoría introducida por una fuente pase inadvertida.
  #

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(resultado)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `sexo` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }


  # ==========================================================================
  # 6. Conservar NA y cadenas vacías como NA
  # ==========================================================================

  resultado[
    is.na(x) |
      trimws(x) == ""
  ] <- NA_character_


  # ==========================================================================
  # 7. Resultado
  # ==========================================================================

  resultado
}

#' Normalizar la dimensión contrato
#'
#' Homogeneiza los valores de la dimensión `contrato`.
#'
#' Actualmente las fuentes contienen:
#'
#' - `"Indefinido"`
#' - `"Temporal"`
#'
#' Cualquier categoría nueva o no reconocida provoca un error, para evitar
#' modificaciones silenciosas en la clasificación utilizada por las fuentes.
#'
#' @param x Vector de caracteres con valores de tipo de contrato.
#'
#' @return Vector de caracteres con los valores normalizados.
#'
#' @keywords internal
#' @noRd
normalizar_contrato <- function(x) {

  x <- as.character(x)

  clave <- normalizar_clave_territorial(x)

  diccionario <- c(
    indefinido = "Indefinido",
    temporal = "Temporal"
  )

  resultado <- unname(
    diccionario[clave]
  )

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(resultado)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `contrato` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resultado[
    is.na(x) |
      trimws(x) == ""
  ] <- NA_character_

  resultado
}

#' Normalizar la dimensión jornada
#'
#' Homogeneiza los valores de la dimensión `jornada`.
#'
#' Actualmente las fuentes contienen:
#'
#' - `"Jornada a tiempo completo"`
#' - `"Jornada a tiempo parcial"`
#'
#' Cualquier categoría nueva o no reconocida provoca un error para evitar
#' cambios silenciosos en las clasificaciones de origen.
#'
#' @param x Vector de caracteres con valores de tipo de jornada.
#'
#' @return Vector de caracteres con los valores normalizados.
#'
#' @keywords internal
#' @noRd
normalizar_jornada <- function(x) {

  x <- as.character(x)

  clave <- normalizar_clave_territorial(x)

  diccionario <- c(
    jornadaatiempocompleto = "Jornada a tiempo completo",
    jornadaatiempoparcial = "Jornada a tiempo parcial"
  )

  resultado <- unname(
    diccionario[clave]
  )

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(resultado)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `jornada` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resultado[
    is.na(x) |
      trimws(x) == ""
  ] <- NA_character_

  resultado
}

#' Normalizar la dimensión ocupación
#'
#' Homogeneiza los valores de la dimensión `ocupacion`.
#'
#' Actualmente las fuentes contienen:
#'
#' - `"Alta"`
#' - `"Media"`
#' - `"Baja"`
#'
#' Cualquier categoría nueva o no reconocida provoca un error, para evitar
#' modificaciones silenciosas en las clasificaciones utilizadas por las
#' fuentes originales.
#'
#' @param x Vector de caracteres con valores de ocupación.
#'
#' @return Vector de caracteres con los valores normalizados.
#'
#' @keywords internal
#' @noRd
normalizar_ocupacion <- function(x) {

  x <- as.character(x)

  clave <- normalizar_clave_territorial(x)

  diccionario <- c(
    alta = "Alta",
    media = "Media",
    baja = "Baja"
  )

  resultado <- unname(
    diccionario[clave]
  )

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(resultado)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `ocupacion` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resultado[
    is.na(x) |
      trimws(x) == ""
  ] <- NA_character_

  resultado
}

#' Normalizar la dimensión nacionalidad
#'
#' Homogeneiza los valores de la dimensión `nacionalidad`.
#'
#' Las fuentes pueden utilizar distintos niveles de desagregación. Actualmente
#' se han identificado las siguientes categorías:
#'
#' - `"Española"`
#' - `"Extranjera"`
#' - `"Extranjera: Unión Europea"`
#' - `"Extranjera: No pertenecientes a la Unión Europea"`
#'
#' Las cuatro categorías se conservan de forma diferenciada. La función no
#' agrega ni desagrega categorías.
#'
#' Cualquier valor nuevo o no reconocido provoca un error para permitir su
#' revisión explícita antes de incorporarlo al paquete.
#'
#' @param x Vector de caracteres con valores de nacionalidad.
#'
#' @return Vector de caracteres con los valores normalizados.
#'
#' @keywords internal
#' @noRd
normalizar_nacionalidad <- function(x) {

  # ==========================================================================
  # 1. Preparar entrada
  # ==========================================================================

  x <- as.character(x)


  # ==========================================================================
  # 2. Generar clave comparable
  # ==========================================================================

  clave <- normalizar_clave_territorial(x)


  # ==========================================================================
  # 3. Diccionario explícito
  # ==========================================================================

  diccionario <- c(
    espanola =
      "Española",

    extranjera =
      "Extranjera",

    extranjeraunioneuropea =
      "Extranjera: Unión Europea",

    extranjeranopertenecientesalaunioneuropea =
      "Extranjera: No pertenecientes a la Unión Europea"
  )


  # ==========================================================================
  # 4. Resolver valores
  # ==========================================================================

  resultado <- unname(
    diccionario[clave]
  )


  # ==========================================================================
  # 5. Detectar categorías desconocidas
  # ==========================================================================

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(resultado)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `nacionalidad` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }


  # ==========================================================================
  # 6. Conservar ausencias
  # ==========================================================================

  resultado[
    is.na(x) |
      trimws(x) == ""
  ] <- NA_character_


  # ==========================================================================
  # 7. Resultado
  # ==========================================================================

  resultado
}

#' Normalizar la dimensión orden de nacimiento
#'
#' Homogeneiza los valores de la dimensión `ordennacimiento`.
#'
#' Actualmente las fuentes contienen:
#'
#' - `"Primero"`
#' - `"Segundo"`
#' - `"Tercero"`
#' - `"Cuarto y más"`
#'
#' Cualquier categoría nueva o no reconocida provoca un error para evitar
#' cambios silenciosos en las clasificaciones utilizadas por las fuentes.
#'
#' @param x Vector de caracteres con valores de orden de nacimiento.
#'
#' @return Vector de caracteres con los valores normalizados.
#'
#' @keywords internal
#' @noRd
normalizar_ordennacimiento <- function(x) {

  x <- as.character(x)

  clave <- normalizar_clave_territorial(x)

  diccionario <- c(
    primero = "Primero",
    segundo = "Segundo",
    tercero = "Tercero",
    cuartoymas = "Cuarto y más"
  )

  resultado <- unname(
    diccionario[clave]
  )

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(resultado)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `ordennacimiento` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resultado[
    is.na(x) |
      trimws(x) == ""
  ] <- NA_character_

  resultado
}

#' Normalizar la dimensión sector 2
#'
#' Homogeneiza los valores de la dimensión `sect2`.
#'
#' Actualmente las fuentes contienen:
#'
#' - `"Secundaria"`
#' - `"Terciaria"`
#'
#' Cualquier categoría nueva o no reconocida provoca un error para evitar
#' cambios silenciosos en la clasificación de origen.
#'
#' @param x Vector de caracteres con valores de sector 2.
#'
#' @return Vector de caracteres con los valores normalizados.
#'
#' @keywords internal
#' @noRd
normalizar_sect2 <- function(x) {

  x <- as.character(x)

  clave <- normalizar_clave_territorial(x)

  diccionario <- c(
    secundaria = "Secundaria",
    terciaria = "Terciaria"
  )

  resultado <- unname(
    diccionario[clave]
  )

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(resultado)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `sect2` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resultado[
    is.na(x) |
      trimws(x) == ""
  ] <- NA_character_

  resultado
}

#' Normalizar la dimensión sector 3
#'
#' Homogeneiza los valores de la dimensión `sect3`.
#'
#' Actualmente las fuentes contienen:
#'
#' - `"Agricultura"`
#' - `"Construcción"`
#' - `"Industria"`
#' - `"Servicios"`
#'
#' Cualquier categoría nueva o no reconocida provoca un error para evitar
#' cambios silenciosos en la clasificación utilizada por las fuentes.
#'
#' @param x Vector de caracteres con valores de sector 3.
#'
#' @return Vector de caracteres con los valores normalizados.
#'
#' @keywords internal
#' @noRd
normalizar_sect3 <- function(x) {

  x <- as.character(x)

  clave <- normalizar_clave_territorial(x)

  diccionario <- c(
    agricultura = "Agricultura",
    construccion = "Construcción",
    industria = "Industria",
    servicios = "Servicios"
  )

  resultado <- unname(
    diccionario[clave]
  )

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(resultado)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `sect3` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resultado[
    is.na(x) |
      trimws(x) == ""
  ] <- NA_character_

  resultado
}

#' Normalizar la dimensión tamaño de empresa
#'
#' Homogeneiza los valores de la dimensión `tamanyoemp`.
#'
#' Actualmente las fuentes contienen:
#'
#' - `"Micro"`
#' - `"Pequeña"`
#' - `"Mediana"`
#' - `"Grande"`
#'
#' Cualquier categoría nueva o no reconocida provoca un error para evitar
#' cambios silenciosos en la clasificación utilizada por las fuentes.
#'
#' @param x Vector de caracteres con valores de tamaño de empresa.
#'
#' @return Vector de caracteres con los valores normalizados.
#'
#' @keywords internal
#' @noRd
normalizar_tamanyoemp <- function(x) {

  x <- as.character(x)

  clave <- normalizar_clave_territorial(x)

  diccionario <- c(
    micro = "Micro",
    pequena = "Pequeña",
    mediana = "Mediana",
    grande = "Grande"
  )

  resultado <- unname(
    diccionario[clave]
  )

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(resultado)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `tamanyoemp` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resultado[
    is.na(x) |
      trimws(x) == ""
  ] <- NA_character_

  resultado
}

#' Normalizar la dimensión tipo de vivienda
#'
#' Homogeneiza los valores de la dimensión `tipovivienda`.
#'
#' Actualmente las fuentes contienen:
#'
#' - `"Vivienda libre"`
#' - `"Vivienda nueva"`
#' - `"Vivienda protegida"`
#' - `"Vivienda usada"`
#' - `"Viviendas: Total"`
#'
#' Las categorías se conservan diferenciadas tal y como aparecen
#' conceptualmente en la fuente.
#'
#' Cualquier categoría nueva o no reconocida provoca un error, para evitar
#' modificaciones silenciosas en la clasificación utilizada por las fuentes.
#'
#' @param x Vector de caracteres con valores de tipo de vivienda.
#'
#' @return Vector de caracteres con los valores normalizados.
#'
#' @keywords internal
#' @noRd
normalizar_tipovivienda <- function(x) {

  x <- as.character(x)

  clave <- normalizar_clave_territorial(x)

  diccionario <- c(
    viviendalibre = "Vivienda libre",
    viviendanueva = "Vivienda nueva",
    viviendaprotegida = "Vivienda protegida",
    viviendausada = "Vivienda usada",
    viviendastotal = "Viviendas: Total"
  )

  resultado <- unname(
    diccionario[clave]
  )

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(resultado)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `tipovivienda` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resultado[
    is.na(x) |
      trimws(x) == ""
  ] <- NA_character_

  resultado
}

#' Normalizar la dimensión secciones
#'
#' Homogeneiza los valores de la dimensión `secciones`.
#'
#' Actualmente las fuentes contienen cuatro categorías correspondientes a
#' secciones de actividad económica:
#'
#' - `"B Industrias extractivas"`
#' - `"C Industria manufacturera"`
#' - `"D Suministro de energía eléctrica, gas, vapor y aire acondicionado"`
#' - `"E Suministro de agua, actividades de saneamiento, gestión de residuos y descontaminación"`
#'
#' Las etiquetas se conservan completas.
#'
#' Cualquier categoría nueva o no reconocida provoca un error para evitar
#' cambios silenciosos en la clasificación de origen.
#'
#' @param x Vector de caracteres con valores de sección.
#'
#' @return Vector de caracteres con los valores normalizados.
#'
#' @keywords internal
#' @noRd
normalizar_secciones <- function(x) {

  x <- as.character(x)

  clave <- normalizar_clave_territorial(x)

  diccionario <- c(
    bindustriasextractivas =
      "B Industrias extractivas",

    cindustriamanufacturera =
      "C Industria manufacturera",

    dsuministrodeenergiaelectricagasvaporyaireacondicionado =
      "D Suministro de energía eléctrica, gas, vapor y aire acondicionado",

    esuministrodeaguaactividadesdesaneamientogestionderesiduosydescontaminacion =
      paste0(
        "E Suministro de agua, actividades de saneamiento, ",
        "gestión de residuos y descontaminación"
      )
  )

  resultado <- unname(
    diccionario[clave]
  )

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(resultado)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `secciones` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resultado[
    is.na(x) |
      trimws(x) == ""
  ] <- NA_character_

  resultado
}

#' Normalizar la dimensión gastos
#'
#' Homogeneiza los valores de la dimensión `gastos`.
#'
#' Actualmente las fuentes contienen:
#'
#' - `"Gasto medio diario por persona"`
#' - `"Gasto medio por persona"`
#' - `"Gasto total"`
#'
#' Cualquier categoría nueva o no reconocida provoca un error para evitar
#' cambios silenciosos en la clasificación utilizada por las fuentes.
#'
#' @param x Vector de caracteres con valores de tipo de gasto.
#'
#' @return Vector de caracteres con los valores normalizados.
#'
#' @keywords internal
#' @noRd
normalizar_gastos <- function(x) {

  x <- as.character(x)

  clave <- normalizar_clave_territorial(x)

  diccionario <- c(
    gastomediodiarioporpersona =
      "Gasto medio diario por persona",

    gastomedioporpersona =
      "Gasto medio por persona",

    gastototal =
      "Gasto total"
  )

  resultado <- unname(
    diccionario[clave]
  )

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(resultado)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `gastos` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resultado[
    is.na(x) |
      trimws(x) == ""
  ] <- NA_character_

  resultado
}

#' Normalizar la dimensión actividades
#'
#' Homogeneiza los valores de la dimensión `actividades`.
#'
#' Actualmente las fuentes contienen:
#'
#' - `"47 Comercio al por menor, excepto de vehículos de motor y motocicletas"`
#' - `"Comercio al por menor sin Estaciones de Servicio (47 sin 473)"`
#'
#' Las etiquetas se conservan completas, ya que representan ámbitos
#' diferentes dentro de la actividad comercial.
#'
#' Cualquier categoría nueva o no reconocida provoca un error para evitar
#' cambios silenciosos en las clasificaciones utilizadas por las fuentes.
#'
#' @param x Vector de caracteres con valores de actividad.
#'
#' @return Vector de caracteres con los valores normalizados.
#'
#' @keywords internal
#' @noRd
normalizar_actividades <- function(x) {

  x <- as.character(x)

  clave <- normalizar_clave_territorial(x)

  diccionario <- c(
    `47comercioalpormenorexceptodevehiculosdemotorymotocicletas` =
      "47 Comercio al por menor, excepto de vehículos de motor y motocicletas",

    comercioalpormenorsinestacionesdeservicio47sin473 =
      "Comercio al por menor sin Estaciones de Servicio (47 sin 473)"
  )

  resultado <- unname(
    diccionario[clave]
  )

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(resultado)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `actividades` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resultado[
    is.na(x) |
      trimws(x) == ""
  ] <- NA_character_

  resultado
}

#' Normalizar la dimensión destino
#'
#' Homogeneiza los valores de la dimensión `destino`.
#'
#' Actualmente las fuentes contienen:
#'
#' - `"Bienes de consumo"`
#' - `"Bienes de consumo duradero"`
#' - `"Bienes de consumo no duradero"`
#' - `"Bienes de equipo"`
#' - `"Bienes intermedios"`
#' - `"Energía"`
#' - `"Total industria"`
#' - `"Total industria sin energía"`
#'
#' Las categorías se conservan diferenciadas tal y como aparecen
#' conceptualmente en las fuentes.
#'
#' Cualquier categoría nueva o no reconocida provoca un error para evitar
#' cambios silenciosos en la clasificación de origen.
#'
#' @param x Vector de caracteres con valores de destino económico.
#'
#' @return Vector de caracteres con los valores normalizados.
#'
#' @keywords internal
#' @noRd
normalizar_destino <- function(x) {

  x <- as.character(x)

  clave <- normalizar_clave_territorial(x)

  diccionario <- c(
    bienesdeconsumo =
      "Bienes de consumo",

    bienesdeconsumoduradero =
      "Bienes de consumo duradero",

    bienesdeconsumonoduradero =
      "Bienes de consumo no duradero",

    bienesdeequipo =
      "Bienes de equipo",

    bienesintermedios =
      "Bienes intermedios",

    energia =
      "Energía",

    totalindustria =
      "Total industria",

    totalindustriasinenergia =
      "Total industria sin energía"
  )

  resultado <- unname(
    diccionario[clave]
  )

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(resultado)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `destino` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resultado[
    is.na(x) |
      trimws(x) == ""
  ] <- NA_character_

  resultado
}

#' Normalizar la dimensión nivel educativo
#'
#' Homogeneiza los valores de la dimensión `niveleducativo` y construye,
#' además, una clasificación común de cuatro grupos.
#'
#' Las fuentes utilizan dos niveles de desagregación:
#'
#' 1. Una clasificación agregada de cuatro grupos.
#' 2. Una clasificación detallada de siete grupos.
#'
#' La función conserva siempre el máximo detalle disponible en la columna
#' `niveleducativo` y añade una segunda columna, `niveleducativo_4`, que
#' permite comparar datasets construidos con ambas clasificaciones.
#'
#' La correspondencia común de cuatro grupos es:
#'
#' - Educación primaria e inferior
#' - Primera etapa de Educación Secundaria y similar
#' - Segunda etapa de Educación Secundaria y Educación Postsecundaria
#'   no Superior
#' - Educación superior
#'
#' En la clasificación detallada:
#'
#' - `Analfabetos`
#' - `Estudios primarios incompletos`
#' - `Educación primaria`
#'
#' se agrupan como `Educación primaria e inferior`.
#'
#' Asimismo:
#'
#' - `Segunda etapa de educación secundaria, con orientación general`
#' - `Segunda etapa de educación secundaria con orientación profesional
#'   (incluye educación postsecundaria no superior)`
#'
#' se agrupan como `Segunda etapa de Educación Secundaria y Educación
#' Postsecundaria no Superior`.
#'
#' Cualquier valor nuevo o no reconocido provoca un error para evitar
#' clasificaciones silenciosas.
#'
#' @param x Vector de caracteres con valores de nivel educativo.
#'
#' @return Data frame con dos columnas:
#'
#' - `niveleducativo`: categoría normalizada conservando el máximo detalle;
#' - `niveleducativo_4`: clasificación común de cuatro grupos.
#'
#' @keywords internal
#' @noRd
normalizar_niveleducativo <- function(x) {

  # ==========================================================================
  # 1. Preparar entrada
  # ==========================================================================

  x <- as.character(x)

  clave <- normalizar_clave_territorial(x)


  # ==========================================================================
  # 2. Normalización del nivel educativo detallado
  # ==========================================================================
  #
  # El diccionario admite tanto las etiquetas de la clasificación detallada
  # como las etiquetas compactadas que aparecen en la clasificación agregada.
  #
  # Los valores compactados se convierten a etiquetas legibles.
  #

  diccionario_detalle <- c(

    # ------------------------------------------------------------------------
    # Clasificación agregada de cuatro grupos
    # ------------------------------------------------------------------------

    educacionprimariaeinferior =
      "Educación primaria e inferior",

    primeraetapadeeducacionsecundariaysimilar =
      "Primera etapa de Educación Secundaria y similar",

    segundaetapadeeducacionsecundariayeducacionpostsecundarianosuperior =
      paste0(
        "Segunda etapa de Educación Secundaria y ",
        "Educación Postsecundaria no Superior"
      ),

    educacionsuperior =
      "Educación superior",


    # ------------------------------------------------------------------------
    # Clasificación detallada
    # ------------------------------------------------------------------------

    analfabetos =
      "Analfabetos",

    estudiosprimariosincompletos =
      "Estudios primarios incompletos",

    educacionprimaria =
      "Educación primaria",

    segundaetapadeeducacionsecundariaconorientacionprofesionalincluyeeducacionpostsecundarianosuperior =
      paste0(
        "Segunda etapa de educación secundaria con orientación profesional ",
        "(incluye educación postsecundaria no superior)"
      ),

    segundaetapadeeducacionsecundariaconorientaciongeneral =
      "Segunda etapa de educación secundaria, con orientación general"
  )


  niveleducativo <- unname(
    diccionario_detalle[clave]
  )


  # ==========================================================================
  # 3. Detectar valores no reconocidos
  # ==========================================================================

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(niveleducativo)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `niveleducativo` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }


  # ==========================================================================
  # 4. Construir clasificación común de cuatro grupos
  # ==========================================================================

  niveleducativo_4 <- rep(
    NA_character_,
    length(niveleducativo)
  )


  # --------------------------------------------------------------------------
  # 4.1. Educación primaria e inferior
  # --------------------------------------------------------------------------

  niveleducativo_4[
    niveleducativo %in% c(
      "Analfabetos",
      "Estudios primarios incompletos",
      "Educación primaria",
      "Educación primaria e inferior"
    )
  ] <- "Educación primaria e inferior"


  # --------------------------------------------------------------------------
  # 4.2. Primera etapa de Educación Secundaria
  # --------------------------------------------------------------------------

  niveleducativo_4[
    niveleducativo ==
      "Primera etapa de Educación Secundaria y similar"
  ] <- "Primera etapa de Educación Secundaria y similar"


  # --------------------------------------------------------------------------
  # 4.3. Segunda etapa y postsecundaria no superior
  # --------------------------------------------------------------------------

  niveleducativo_4[
    niveleducativo %in% c(
      paste0(
        "Segunda etapa de Educación Secundaria y ",
        "Educación Postsecundaria no Superior"
      ),
      paste0(
        "Segunda etapa de educación secundaria con orientación profesional ",
        "(incluye educación postsecundaria no superior)"
      ),
      "Segunda etapa de educación secundaria, con orientación general"
    )
  ] <- paste0(
    "Segunda etapa de Educación Secundaria y ",
    "Educación Postsecundaria no Superior"
  )


  # --------------------------------------------------------------------------
  # 4.4. Educación superior
  # --------------------------------------------------------------------------

  niveleducativo_4[
    niveleducativo == "Educación superior"
  ] <- "Educación superior"


  # ==========================================================================
  # 5. Comprobación de integridad
  # ==========================================================================
  #
  # Todo valor reconocido de `niveleducativo` debe poder asignarse también
  # a uno de los cuatro grupos comunes.
  #

  sin_grupo_4 <- !is.na(niveleducativo) &
    is.na(niveleducativo_4)

  if (any(sin_grupo_4)) {

    stop(
      paste0(
        "Existen valores de `niveleducativo` sin correspondencia en ",
        "`niveleducativo_4`: ",
        paste(
          sort(
            unique(
              niveleducativo[sin_grupo_4]
            )
          ),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }


  # ==========================================================================
  # 6. Conservar ausencias
  # ==========================================================================

  es_ausencia <- is.na(x) |
    trimws(x) == ""

  niveleducativo[es_ausencia] <- NA_character_
  niveleducativo_4[es_ausencia] <- NA_character_


  # ==========================================================================
  # 7. Resultado
  # ==========================================================================

  data.frame(
    niveleducativo = niveleducativo,
    niveleducativo_4 = niveleducativo_4,
    stringsAsFactors = FALSE
  )
}

#' Normalizar la dimensión edad
#'
#' Homogeneiza las etiquetas de edad y extrae una representación estructurada
#' de los intervalos observados en las fuentes.
#'
#' Las fuentes utilizan distintas clasificaciones de edad: edades puntuales,
#' intervalos cerrados, intervalos abiertos superiores y grupos de menores
#' de una determinada edad.
#'
#' La función NO agrega ni transforma una clasificación en otra. Conserva el
#' nivel de detalle disponible y únicamente normaliza variantes de escritura
#' que representan exactamente el mismo intervalo.
#'
#' Ejemplos:
#'
#' \preformatted{
#' "25 años"             -> edad_min = 25, edad_max = 25
#' "De 25 a 34 años"     -> edad_min = 25, edad_max = 34
#' "25 - 34 años"        -> edad_min = 25, edad_max = 34
#' "25 y más años"       -> edad_min = 25, edad_max = NA
#' "Menores de 25 años"  -> edad_min = 0,  edad_max = 24
#' "Menos de 25 años"    -> edad_min = 0,  edad_max = 24
#' }
#'
#' `edad_tipo` puede tomar los valores:
#'
#' - `"puntual"`
#' - `"intervalo"`
#' - `"abierta_superior"`
#' - `"abierta_inferior"`
#'
#' Cualquier formato nuevo o no reconocido provoca un error para evitar
#' interpretaciones silenciosas.
#'
#' @param x Vector de caracteres con categorías de edad.
#'
#' @return Data frame con:
#'
#' - `edad`: etiqueta normalizada;
#' - `edad_min`: edad mínima incluida;
#' - `edad_max`: edad máxima incluida, o NA si no existe límite superior;
#' - `edad_tipo`: tipo de intervalo.
#'
#' @keywords internal
#' @noRd
normalizar_edad <- function(x) {

  # ==========================================================================
  # 1. Preparar entrada
  # ==========================================================================

  x <- as.character(x)

  x_limpio <- trimws(x)


  # ==========================================================================
  # 2. Preparar resultado
  # ==========================================================================

  n <- length(x_limpio)

  edad <- rep(
    NA_character_,
    n
  )

  edad_min <- rep(
    NA_integer_,
    n
  )

  edad_max <- rep(
    NA_integer_,
    n
  )

  edad_tipo <- rep(
    NA_character_,
    n
  )


  # ==========================================================================
  # 3. Edades puntuales
  # ==========================================================================
  #
  # Ejemplos:
  #
  #   0 años
  #   25 años
  #   90 años
  #

  es_puntual <- grepl(
    "^[0-9]+ años$",
    x_limpio
  )

  if (any(es_puntual)) {

    valor <- as.integer(
      sub(
        " años$",
        "",
        x_limpio[es_puntual]
      )
    )

    edad[es_puntual] <- paste0(
      valor,
      " años"
    )

    edad_min[es_puntual] <- valor
    edad_max[es_puntual] <- valor
    edad_tipo[es_puntual] <- "puntual"
  }


  # ==========================================================================
  # 4. Intervalos cerrados: "De X a Y años"
  # ==========================================================================

  es_intervalo_de <- grepl(
    "^De [0-9]+ a [0-9]+ años$",
    x_limpio
  )

  if (any(es_intervalo_de)) {

    limites <- regmatches(
      x_limpio[es_intervalo_de],
      gregexpr(
        "[0-9]+",
        x_limpio[es_intervalo_de]
      )
    )

    minimo <- vapply(
      limites,
      function(z) as.integer(z[1]),
      integer(1)
    )

    maximo <- vapply(
      limites,
      function(z) as.integer(z[2]),
      integer(1)
    )

    edad[es_intervalo_de] <- paste0(
      "De ",
      minimo,
      " a ",
      maximo,
      " años"
    )

    edad_min[es_intervalo_de] <- minimo
    edad_max[es_intervalo_de] <- maximo
    edad_tipo[es_intervalo_de] <- "intervalo"
  }


  # ==========================================================================
  # 5. Intervalos escritos como "X - Y años"
  # ==========================================================================
  #
  # Estas etiquetas representan el mismo concepto que "De X a Y años" y se
  # convierten al formato canónico utilizado por el paquete.
  #

  es_intervalo_guion <- grepl(
    "^[0-9]+[[:space:]]*-[[:space:]]*[0-9]+ años$",
    x_limpio
  )

  if (any(es_intervalo_guion)) {

    limites <- regmatches(
      x_limpio[es_intervalo_guion],
      gregexpr(
        "[0-9]+",
        x_limpio[es_intervalo_guion]
      )
    )

    minimo <- vapply(
      limites,
      function(z) as.integer(z[1]),
      integer(1)
    )

    maximo <- vapply(
      limites,
      function(z) as.integer(z[2]),
      integer(1)
    )

    edad[es_intervalo_guion] <- paste0(
      "De ",
      minimo,
      " a ",
      maximo,
      " años"
    )

    edad_min[es_intervalo_guion] <- minimo
    edad_max[es_intervalo_guion] <- maximo
    edad_tipo[es_intervalo_guion] <- "intervalo"
  }


  # ==========================================================================
  # 6. Intervalos abiertos superiores: "X y más años"
  # ==========================================================================

  es_abierta_superior <- grepl(
    "^(De )?[0-9]+ y más años$",
    x_limpio
  )

  if (any(es_abierta_superior)) {

    minimo <- as.integer(
      sub(
        "^.*?([0-9]+) y más años$",
        "\\1",
        x_limpio[es_abierta_superior]
      )
    )

    edad[es_abierta_superior] <- paste0(
      minimo,
      " y más años"
    )

    edad_min[es_abierta_superior] <- minimo
    edad_max[es_abierta_superior] <- NA_integer_
    edad_tipo[es_abierta_superior] <- "abierta_superior"
  }


  # ==========================================================================
  # 7. Intervalos inferiores: menores de X años
  # ==========================================================================
  #
  # Las dos expresiones observadas:
  #
  #   "Menores de 25 años"
  #   "Menos de 25 años"
  #
  # representan el mismo intervalo y se normalizan a la primera.
  #
  # Como las edades se expresan en años completos, "menores de 25" se
  # representa mediante:
  #
  #   edad_min = 0
  #   edad_max = 24
  #

  es_abierta_inferior <- grepl(
    "^(Menores de|Menos de) [0-9]+ años$",
    x_limpio
  )

  if (any(es_abierta_inferior)) {

    limite <- as.integer(
      sub(
        "^(Menores de|Menos de) ([0-9]+) años$",
        "\\2",
        x_limpio[es_abierta_inferior]
      )
    )

    edad[es_abierta_inferior] <- paste0(
      "Menores de ",
      limite,
      " años"
    )

    edad_min[es_abierta_inferior] <- 0L
    edad_max[es_abierta_inferior] <- limite - 1L
    edad_tipo[es_abierta_inferior] <- "abierta_inferior"
  }


  # ==========================================================================
  # 8. Detectar categorías no reconocidas
  # ==========================================================================

  es_ausencia <- is.na(x_limpio) |
    x_limpio == ""

  no_reconocidos <- !es_ausencia &
    is.na(edad_tipo)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x_limpio[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `edad` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }


  # ==========================================================================
  # 9. Comprobaciones de coherencia
  # ==========================================================================

  intervalo_invalido <- !is.na(edad_min) &
    !is.na(edad_max) &
    edad_min > edad_max

  if (any(intervalo_invalido)) {

    stop(
      "Se han generado intervalos de edad con límite inferior mayor que el superior.",
      call. = FALSE
    )
  }


  # ==========================================================================
  # 10. Resultado
  # ==========================================================================

  data.frame(
    edad = edad,
    edad_min = edad_min,
    edad_max = edad_max,
    edad_tipo = edad_tipo,
    stringsAsFactors = FALSE
  )
}

#' Normalizar la dimensión tamaño de municipio
#'
#' Homogeneiza los valores de la dimensión `tamanyomuni` y extrae los límites
#' numéricos de población asociados a cada intervalo.
#'
#' La función conserva la etiqueta categórica original en formato canónico y
#' añade:
#'
#' - `tamanyomuni_min`: límite inferior incluido;
#' - `tamanyomuni_max`: límite superior incluido, o NA cuando el intervalo
#'   no tiene límite superior.
#'
#' Ejemplos:
#'
#' \preformatted{
#' "Menos de 101"      -> min = 0,      max = 100
#' "De 101 a 500"      -> min = 101,    max = 500
#' "De 1.001 a 2.000"  -> min = 1001,   max = 2000
#' "Más de 500.000"    -> min = 500001, max = NA
#' }
#'
#' Cualquier categoría nueva o no reconocida provoca un error.
#'
#' @param x Vector de caracteres con categorías de tamaño de municipio.
#'
#' @return Data frame con:
#'
#' - `tamanyomuni`
#' - `tamanyomuni_min`
#' - `tamanyomuni_max`
#'
#' @keywords internal
#' @noRd
normalizar_tamanyomuni <- function(x) {

  # ==========================================================================
  # 1. Preparar entrada
  # ==========================================================================

  x <- as.character(x)

  x_limpio <- trimws(x)


  # ==========================================================================
  # 2. Función auxiliar para convertir números con separador de miles
  # ==========================================================================
  #
  # Ejemplos:
  #
  #   "1.001"   -> 1001
  #   "500.000" -> 500000
  #

  convertir_entero <- function(z) {

    as.integer(
      gsub(
        "\\.",
        "",
        z
      )
    )
  }


  # ==========================================================================
  # 3. Preparar resultado
  # ==========================================================================

  n <- length(x_limpio)

  tamanyomuni <- rep(
    NA_character_,
    n
  )

  tamanyomuni_min <- rep(
    NA_integer_,
    n
  )

  tamanyomuni_max <- rep(
    NA_integer_,
    n
  )


  # ==========================================================================
  # 4. Intervalos cerrados: "De X a Y"
  # ==========================================================================

  es_intervalo <- grepl(
    "^De [0-9\\.]+ a [0-9\\.]+$",
    x_limpio
  )

  if (any(es_intervalo)) {

    numeros <- regmatches(
      x_limpio[es_intervalo],
      gregexpr(
        "[0-9\\.]+",
        x_limpio[es_intervalo]
      )
    )

    minimo <- vapply(
      numeros,
      function(z) convertir_entero(z[1]),
      integer(1)
    )

    maximo <- vapply(
      numeros,
      function(z) convertir_entero(z[2]),
      integer(1)
    )

    tamanyomuni[es_intervalo] <- x_limpio[es_intervalo]

    tamanyomuni_min[es_intervalo] <- minimo
    tamanyomuni_max[es_intervalo] <- maximo
  }


  # ==========================================================================
  # 5. Intervalo inferior: "Menos de X"
  # ==========================================================================

  es_menos <- grepl(
    "^Menos de [0-9\\.]+$",
    x_limpio
  )

  if (any(es_menos)) {

    limite <- convertir_entero(
      sub(
        "^Menos de ",
        "",
        x_limpio[es_menos]
      )
    )

    tamanyomuni[es_menos] <- x_limpio[es_menos]

    tamanyomuni_min[es_menos] <- 0L

    tamanyomuni_max[es_menos] <- limite - 1L
  }


  # ==========================================================================
  # 6. Intervalo superior: "Más de X"
  # ==========================================================================

  es_mas <- grepl(
    "^Más de [0-9\\.]+$",
    x_limpio
  )

  if (any(es_mas)) {

    limite <- convertir_entero(
      sub(
        "^Más de ",
        "",
        x_limpio[es_mas]
      )
    )

    tamanyomuni[es_mas] <- x_limpio[es_mas]

    # "Más de 500.000" implica 500.001 o más.
    tamanyomuni_min[es_mas] <- limite + 1L

    tamanyomuni_max[es_mas] <- NA_integer_
  }


  # ==========================================================================
  # 7. Detectar categorías no reconocidas
  # ==========================================================================

  es_ausencia <- is.na(x_limpio) |
    x_limpio == ""

  no_reconocidos <- !es_ausencia &
    is.na(tamanyomuni)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x_limpio[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `tamanyomuni` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }


  # ==========================================================================
  # 8. Comprobación de coherencia
  # ==========================================================================

  intervalo_invalido <- !is.na(tamanyomuni_min) &
    !is.na(tamanyomuni_max) &
    tamanyomuni_min > tamanyomuni_max

  if (any(intervalo_invalido)) {

    stop(
      paste0(
        "Se han generado intervalos de `tamanyomuni` con límite inferior ",
        "mayor que el superior."
      ),
      call. = FALSE
    )
  }


  # ==========================================================================
  # 9. Resultado
  # ==========================================================================

  data.frame(
    tamanyomuni = tamanyomuni,
    tamanyomuni_min = tamanyomuni_min,
    tamanyomuni_max = tamanyomuni_max,
    stringsAsFactors = FALSE
  )
}

#' Normalizar la dimensión producto
#'
#' Homogeneiza los valores de la dimensión `prod`.
#'
#' Los cuatro datasets procedentes de DataComex utilizan una clasificación
#' común de nueve categorías identificadas como:
#'
#' - `"producto1"`
#' - `"producto2"`
#' - ...
#' - `"producto9"`
#'
#' En esta fase no se asignan denominaciones descriptivas adicionales porque
#' no se dispone todavía de una correspondencia documental validada entre
#' estos códigos internos y etiquetas de producto más detalladas.
#'
#' La función conserva `prod` como identificador categórico y genera además
#' `prod_codigo`, un entero entre 1 y 9 que permite ordenar y filtrar las
#' categorías de forma inequívoca.
#'
#' Cualquier categoría nueva o no reconocida provoca un error.
#'
#' @param x Vector de caracteres con categorías de producto.
#'
#' @return Data frame con:
#'
#' - `prod`: código categórico normalizado;
#' - `prod_codigo`: código entero entre 1 y 9.
#'
#' @keywords internal
#' @noRd
normalizar_prod <- function(x) {

  # ==========================================================================
  # 1. Preparar entrada
  # ==========================================================================

  x <- as.character(x)

  clave <- normalizar_clave_territorial(x)


  # ==========================================================================
  # 2. Diccionario explícito
  # ==========================================================================

  diccionario <- stats::setNames(
    paste0(
      "producto",
      1:9
    ),
    paste0(
      "producto",
      1:9
    )
  )

  prod <- unname(
    diccionario[clave]
  )


  # ==========================================================================
  # 3. Detectar valores no reconocidos
  # ==========================================================================

  no_reconocidos <- !is.na(x) &
    trimws(x) != "" &
    is.na(prod)

  if (any(no_reconocidos)) {

    valores_problematicos <- sort(
      unique(
        x[no_reconocidos]
      )
    )

    stop(
      paste0(
        "Se han encontrado valores de `prod` no reconocidos: ",
        paste(
          valores_problematicos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }


  # ==========================================================================
  # 4. Extraer código numérico
  # ==========================================================================

  prod_codigo <- suppressWarnings(
    as.integer(
      sub(
        "^producto",
        "",
        prod
      )
    )
  )


  # ==========================================================================
  # 5. Conservar ausencias
  # ==========================================================================

  es_ausencia <- is.na(x) |
    trimws(x) == ""

  prod[es_ausencia] <- NA_character_
  prod_codigo[es_ausencia] <- NA_integer_


  # ==========================================================================
  # 6. Resultado
  # ==========================================================================

  data.frame(
    prod = prod,
    prod_codigo = prod_codigo,
    stringsAsFactors = FALSE
  )
}

# --------------------------------------------------------------------------
# Sector 1
# --------------------------------------------------------------------------
#
# `sect1` se conserva sin recodificación.
#
# Sus valores representan códigos de agrupaciones de secciones de actividad
# económica. El guion bajo indica un intervalo de secciones:
#
#   B_E -> B, C, D y E
#   G_I -> G, H e I
#   R_U -> R, S, T y U
#
# Algunas categorías representan agrupaciones no consecutivas, por ejemplo:
#
#   B, D_E -> B, D y E
#
# Por tanto, estos códigos tienen significado propio y no deben modificarse
# mediante limpieza sintáctica.

