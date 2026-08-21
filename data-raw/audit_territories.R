# ============================================================================
# Auditoría de nombres territoriales
# ============================================================================
#
# Este script comprueba que todos los nombres territoriales observados en los
# Excel fuente pueden ser identificados mediante el diccionario territorial
# del paquete.
#
# OBJETIVO
# --------
# Detectar:
#
#   - errores tipográficos en los Excel;
#   - nuevas denominaciones de CCAA o provincias;
#   - nuevos agregados territoriales;
#   - alias todavía no incluidos en el diccionario.
#
# IMPORTANTE
# ----------
# Una denominación desconocida NO debe añadirse automáticamente al
# diccionario. Primero debe comprobarse manualmente si se trata de:
#
#   - una errata en el Excel;
#   - un alias legítimo;
#   - una categoría territorial nueva.
#
# REQUISITOS
# ----------
# Antes de ejecutarlo deben existir:
#
#   - lista_xlsx
#   - estructura_datasets
#   - crear_diccionario_territorial()
#   - clasificar_territorio()
#
# La forma recomendada de preparar la sesión es:
#
#   source("data-raw/setup_dev.R")
#
# ============================================================================


# ============================================================================
# 1. Construir el diccionario territorial
# ============================================================================

cod_ccaa_prov <- readRDS(
  "data-raw/sources/cod_ccaa_prov.rds"
)

diccionario_territorial <- crear_diccionario_territorial(
  cod_ccaa_prov
)


# ============================================================================
# 2. Seleccionar datasets con información territorial
# ============================================================================

ambitos_mixtos <- c(
  "CVPROV",
  "TERRITORIO",
  "TERRITORIOPARCIAL",
  "TERRITORIOPROV"
)

ids_territorio <- estructura_datasets$id_dataset[
  estructura_datasets$ambito %in% ambitos_mixtos
]


# ============================================================================
# 3. Obtener todas las denominaciones territoriales observadas
# ============================================================================

territorios_observados <- sort(
  unique(
    unlist(
      lapply(
        lista_xlsx[ids_territorio],
        function(x) as.character(x[[1]])
      )
    )
  )
)


# ============================================================================
# 4. Clasificar las denominaciones
# ============================================================================

revision_territorios <- clasificar_territorio(
  territorios_observados,
  diccionario_territorial
)


# ============================================================================
# 5. Detectar territorios no identificados
# ============================================================================

territorios_no_identificados <- subset(
  revision_territorios,
  !encontrada
)

if (nrow(territorios_no_identificados) > 0L) {

  message(
    "ATENCIÓN: existen territorios sin identificar."
  )

  print(
    territorios_no_identificados
  )

} else {

  message(
    "Todos los territorios han sido identificados correctamente."
  )
}


# ============================================================================
# 6. Detectar denominaciones ambiguas
# ============================================================================
#
# Algunos nombres pueden corresponder legítimamente a una CCAA y a una
# provincia, por ejemplo:
#
#   Madrid
#   Murcia
#   Navarra
#
# Esto no constituye necesariamente un error.
#

coincidencias_territoriales <- aggregate(
  tipo_territorio ~ territorio_original,
  data = subset(
    revision_territorios,
    encontrada
  ),
  FUN = length
)

territorios_ambiguos <- subset(
  coincidencias_territoriales,
  tipo_territorio > 1L
)


# ============================================================================
# 7. Utilidad para localizar una errata concreta
# ============================================================================
#
# Si `territorios_no_identificados` contiene un valor que parece una errata,
# puede localizarse en los Excel utilizando el siguiente patrón.
#
# Ejemplo:
#
#   errata <- "madird"
#
# El valor debe expresarse como clave normalizada.
#

buscar_errata_territorial <- function(errata) {

  names(
    Filter(
      function(x) {

        any(
          normalizar_clave_territorial(
            as.character(x[[1]])
          ) == errata,
          na.rm = TRUE
        )
      },
      lista_xlsx
    )
  )
}


# Ejemplo de uso:
#
# buscar_errata_territorial("madird")


# ============================================================================
# 8. Validación específica de datasets TERRITORIOPROV
# ============================================================================
#
# Comprueba que todos los datasets cuyo ámbito es TERRITORIOPROV pueden
# clasificarse completamente en los niveles:
#
#   - nacional
#   - ccaa
#   - ccaa_provincia
#   - provincia
#
# La clasificación se realiza sobre los valores únicos de la primera columna,
# para evitar repetir comprobaciones cuando existen otras dimensiones
# adicionales, por ejemplo SEXO.
# ============================================================================

ids_territorioprov <- estructura_datasets$id_dataset[
  estructura_datasets$ambito == "TERRITORIOPROV"
]


auditoria_territorioprov <- lapply(
  ids_territorioprov,
  function(id) {

    territorios <- unique(
      as.character(
        lista_xlsx[[id]][[1]]
      )
    )

    clasificacion <- normalizar_territorioprov(
      x = territorios,
      diccionario = diccionario_territorial,
      cod_ccaa_prov = cod_ccaa_prov
    )

    clasificacion$id_dataset <- id

    clasificacion[
      c(
        "id_dataset",
        "territorio_original",
        "territorio",
        "tipo_territorio",
        "cod_ccaa",
        "ccaa",
        "cod_provincia",
        "provincia",
        "encontrada"
      )
    ]
  }
)

auditoria_territorioprov <- do.call(
  rbind,
  auditoria_territorioprov
)

rownames(auditoria_territorioprov) <- NULL


# ============================================================================
# 9. TERRITORIOPROV no identificados
# ============================================================================

territorioprov_no_identificados <- subset(
  auditoria_territorioprov,
  !encontrada
)


# ============================================================================
# 10. Tipos territoriales observados por dataset
# ============================================================================

resumen_territorioprov <- as.data.frame(
  with(
    auditoria_territorioprov,
    table(
      id_dataset,
      tipo_territorio,
      useNA = "ifany"
    )
  )
)

resumen_territorioprov <- subset(
  resumen_territorioprov,
  Freq > 0
)

names(resumen_territorioprov)[
  names(resumen_territorioprov) == "Freq"
] <- "n_territorios"


# ============================================================================
# 11. Validación específica de datasets TERRITORIO
# ============================================================================
#
# Los datasets cuyo ámbito es TERRITORIO deberían contener, según la
# nomenclatura definida para el proyecto:
#
#   - total nacional;
#   - comunidades autónomas.
#
# En esta auditoría comprobamos:
#
#   1. que todos los valores territoriales pueden identificarse;
#   2. qué tipos territoriales aparecen realmente;
#   3. si existen provincias u otras categorías inesperadas;
#   4. si todos los datasets contienen total nacional.
#
# La comprobación se realiza sobre los valores únicos de la primera columna,
# para evitar repeticiones cuando existen dimensiones adicionales como
# SEXO, EDAD, SECT1, etc.
#
# ============================================================================


# ============================================================================
# 11.1. Identificar datasets TERRITORIO
# ============================================================================

ids_territorio_simple <- estructura_datasets$id_dataset[
  estructura_datasets$ambito == "TERRITORIO"
]


# ============================================================================
# 11.2. Clasificar sus territorios
# ============================================================================

auditoria_territorio <- lapply(
  ids_territorio_simple,
  function(id) {

    territorios <- unique(
      as.character(
        lista_xlsx[[id]][[1]]
      )
    )

    clasificacion <- clasificar_territorio(
      x = territorios,
      diccionario = diccionario_territorial
    )

    clasificacion$id_dataset <- id

    clasificacion[
      c(
        "id_dataset",
        "territorio_original",
        "clave",
        "tipo_territorio",
        "cod_ccaa",
        "cod_provincia",
        "nombre_canonico",
        "encontrada"
      )
    ]
  }
)

auditoria_territorio <- do.call(
  rbind,
  auditoria_territorio
)

rownames(auditoria_territorio) <- NULL


# ============================================================================
# 11.3. Territorios no identificados
# ============================================================================

territorio_no_identificados <- subset(
  auditoria_territorio,
  !encontrada
)


# ============================================================================
# 11.4. Resumen de tipos territoriales por dataset
# ============================================================================

resumen_territorio <- as.data.frame(
  with(
    auditoria_territorio,
    table(
      id_dataset,
      tipo_territorio,
      useNA = "ifany"
    )
  )
)

resumen_territorio <- subset(
  resumen_territorio,
  Freq > 0
)

names(resumen_territorio)[
  names(resumen_territorio) == "Freq"
] <- "n_territorios"


# ============================================================================
# 11.5. Detectar tipos territoriales inesperados
# ============================================================================
#
# Para un dataset TERRITORIO esperamos únicamente:
#
#   - nacional
#   - ccaa
#
# Las CCAA uniprovinciales pueden aparecer ambiguas en el diccionario
# general, pero conceptualmente aquí deben interpretarse como CCAA.
#

tipos_esperados_territorio <- c(
  "nacional",
  "ccaa"
)

tipos_territorio_inesperados <- subset(
  auditoria_territorio,
  encontrada &
    !tipo_territorio %in% tipos_esperados_territorio
)


# ============================================================================
# 11.6. Comprobar presencia de total nacional
# ============================================================================

datasets_con_nacional <- unique(
  auditoria_territorio$id_dataset[
    auditoria_territorio$tipo_territorio == "nacional"
  ]
)

datasets_sin_nacional <- setdiff(
  ids_territorio_simple,
  datasets_con_nacional
)

# ============================================================================
# 12. Validación de la normalización del ámbito TERRITORIO
# ============================================================================

auditoria_territorio_normalizado <- lapply(
  ids_territorio_simple,
  function(id) {

    territorios <- unique(
      as.character(
        lista_xlsx[[id]][[1]]
      )
    )

    resultado <- normalizar_territorio(
      x = territorios,
      diccionario = diccionario_territorial
    )

    resultado$id_dataset <- id

    resultado
  }
)

auditoria_territorio_normalizado <- do.call(
  rbind,
  auditoria_territorio_normalizado
)

rownames(auditoria_territorio_normalizado) <- NULL


# Territorios que no han podido resolverse.
territorio_normalizado_no_identificado <- subset(
  auditoria_territorio_normalizado,
  !encontrada
)


# Tipos distintos de los permitidos.
territorio_normalizado_tipo_invalido <- subset(
  auditoria_territorio_normalizado,
  encontrada &
    !tipo_territorio %in% c(
      "nacional",
      "ccaa"
    )
)

# ============================================================================
# 13. Validación específica de datasets TERRITORIOPARCIAL
# ============================================================================
#
# Los datasets TERRITORIOPARCIAL contienen:
#
#   - total nacional;
#   - algunas comunidades autónomas;
#   - una categoría agregada "OTRAS COMUNIDADES".
#
# No deberían contener provincias.
# ============================================================================


ids_territorioparcial <- estructura_datasets$id_dataset[
  estructura_datasets$ambito == "TERRITORIOPARCIAL"
]


auditoria_territorioparcial <- lapply(
  ids_territorioparcial,
  function(id) {

    territorios <- unique(
      as.character(
        lista_xlsx[[id]][[1]]
      )
    )

    clasificacion <- clasificar_territorio(
      x = territorios,
      diccionario = diccionario_territorial
    )

    clasificacion$id_dataset <- id

    clasificacion[
      c(
        "id_dataset",
        "territorio_original",
        "clave",
        "tipo_territorio",
        "cod_ccaa",
        "cod_provincia",
        "nombre_canonico",
        "encontrada"
      )
    ]
  }
)

auditoria_territorioparcial <- do.call(
  rbind,
  auditoria_territorioparcial
)

rownames(auditoria_territorioparcial) <- NULL


# ============================================================================
# 13.1. Territorios no identificados
# ============================================================================

territorioparcial_no_identificados <- subset(
  auditoria_territorioparcial,
  !encontrada
)


# ============================================================================
# 13.2. Resumen de tipos territoriales
# ============================================================================

resumen_territorioparcial <- as.data.frame(
  with(
    auditoria_territorioparcial,
    table(
      id_dataset,
      tipo_territorio,
      useNA = "ifany"
    )
  )
)

resumen_territorioparcial <- subset(
  resumen_territorioparcial,
  Freq > 0
)

names(resumen_territorioparcial)[
  names(resumen_territorioparcial) == "Freq"
] <- "n_territorios"


# ============================================================================
# 13.3. Tipos territoriales inesperados
# ============================================================================
#
# Los únicos tipos esperados son:
#
#   nacional
#   ccaa
#   agregado
#

tipos_esperados_territorioparcial <- c(
  "nacional",
  "ccaa",
  "agregado"
)

tipos_territorioparcial_inesperados <- subset(
  auditoria_territorioparcial,
  encontrada &
    !tipo_territorio %in% tipos_esperados_territorioparcial
)

# ============================================================================
# 14. Validación específica de datasets CV y CVPROV
# ============================================================================
#
# CV
# --
# Se espera que represente únicamente información de Comunitat Valenciana.
#
# CVPROV
# ------
# Se espera que contenga Comunitat Valenciana y sus provincias:
#
#   - Alicante/Alacant
#   - Castellón/Castelló
#   - Valencia/València
#
# Esta auditoría comprueba la composición real antes de definir la lógica
# definitiva de normalización.
# ============================================================================


# ============================================================================
# 14.1. Datasets CV
# ============================================================================

ids_cv <- estructura_datasets$id_dataset[
  estructura_datasets$ambito == "CV"
]

auditoria_cv <- lapply(
  ids_cv,
  function(id) {

    territorios <- unique(
      as.character(
        lista_xlsx[[id]][[1]]
      )
    )

    data.frame(
      id_dataset = id,
      territorio_original = territorios,
      stringsAsFactors = FALSE
    )
  }
)

auditoria_cv <- do.call(
  rbind,
  auditoria_cv
)

rownames(auditoria_cv) <- NULL


# ============================================================================
# 14.2. Datasets CVPROV
# ============================================================================

ids_cvprov <- estructura_datasets$id_dataset[
  estructura_datasets$ambito == "CVPROV"
]

auditoria_cvprov <- lapply(
  ids_cvprov,
  function(id) {

    territorios <- unique(
      as.character(
        lista_xlsx[[id]][[1]]
      )
    )

    clasificacion <- clasificar_territorio(
      x = territorios,
      diccionario = diccionario_territorial
    )

    clasificacion$id_dataset <- id

    clasificacion[
      c(
        "id_dataset",
        "territorio_original",
        "clave",
        "tipo_territorio",
        "cod_ccaa",
        "cod_provincia",
        "nombre_canonico",
        "encontrada"
      )
    ]
  }
)

auditoria_cvprov <- do.call(
  rbind,
  auditoria_cvprov
)

rownames(auditoria_cvprov) <- NULL


# ============================================================================
# 14.3. Valores CV observados
# ============================================================================

valores_cv <- auditoria_cv


# ============================================================================
# 14.4. CVPROV no identificados
# ============================================================================

cvprov_no_identificados <- subset(
  auditoria_cvprov,
  !encontrada
)


# ============================================================================
# 14.5. Resumen de tipos en CVPROV
# ============================================================================

resumen_cvprov <- as.data.frame(
  with(
    auditoria_cvprov,
    table(
      id_dataset,
      tipo_territorio,
      useNA = "ifany"
    )
  )
)

resumen_cvprov <- subset(
  resumen_cvprov,
  Freq > 0
)

names(resumen_cvprov)[
  names(resumen_cvprov) == "Freq"
] <- "n_territorios"

