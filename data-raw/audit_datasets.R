# ============================================================================
# Auditoría de los datasets fuente
# ============================================================================
#
# Este script comprueba la coherencia estructural de los Excel originales
# antes de iniciar su transformación y normalización.
#
# El script NO modifica los datos.
#
# Comprueba:
#
#   1. Coherencia entre las dimensiones declaradas en `id_dataset`
#      y las primeras columnas de cada Excel.
#
#   2. Diferencias en las denominaciones de las columnas estructurales.
#
#   3. Validez de las columnas temporales según la frecuencia:
#
#        A -> YYYY
#        T -> YYYYT[1-4]
#        M -> YYYYM1 ... YYYYM12
#             o YYYYM01 ... YYYYM12
#
#   4. Ausencia de periodos duplicados dentro de cada dataset.
#
# ============================================================================
# 0. Requisitos
# ============================================================================
#
# Antes de ejecutar este script deben existir:
#
#   - metadata
#   - lista_xlsx
#   - parse_id_dataset()
#
# Estos objetos se generan/cargan previamente mediante:
#
#   source("data-raw/metadata.R")
#   devtools::load_all()
#   source("data-raw/import_raw_data.R")
#
# ============================================================================


if (!exists("lista_xlsx")) {
  stop(
    "No existe el objeto `lista_xlsx`. Deben cargarse primero los Excel.",
    call. = FALSE
  )
}

if (!exists("metadata")) {
  stop(
    "No existe el objeto `metadata`.",
    call. = FALSE
  )
}

if (!exists("parse_id_dataset")) {
  stop(
    "No existe la función `parse_id_dataset()`.",
    call. = FALSE
  )
}
# ============================================================================
# 1. Comprobaciones previas
# ============================================================================

if (!exists("lista_xlsx")) {
  stop(
    "No existe el objeto `lista_xlsx`. Deben cargarse primero los Excel.",
    call. = FALSE
  )
}

if (!exists("metadata")) {
  stop(
    "No existe el objeto `metadata`.",
    call. = FALSE
  )
}


# ============================================================================
# 2. Interpretación de los identificadores
# ============================================================================

estructura_datasets <- parse_id_dataset(
  metadata$id_dataset
)


# ============================================================================
# 3. Auditoría de columnas estructurales
# ============================================================================

audit_one_dataset <- function(id_dataset,
                              ambito,
                              dimensiones,
                              datos) {

  # Componentes estructurales que esperamos encontrar al comienzo del dataset.
  #
  # Ejemplo:
  #
  #   TERRITORIO_SEXO_EDAD_T_01
  #
  # implica tres columnas estructurales:
  #
  #   TERRITORIO
  #   SEXO
  #   EDAD
  #
  estructura_esperada <- c(
    ambito,
    dimensiones
  )


  # Número esperado de columnas descriptivas antes de que comiencen
  # las columnas temporales.
  n_estructura <- length(
    estructura_esperada
  )


  # Comprobación defensiva.
  if (ncol(datos) < n_estructura) {
    stop(
      sprintf(
        paste0(
          "El dataset '%s' tiene menos columnas (%s) de las necesarias ",
          "según su identificador (%s)."
        ),
        id_dataset,
        ncol(datos),
        n_estructura
      ),
      call. = FALSE
    )
  }


  # Tomamos las primeras columnas según la estructura declarada
  # por el identificador.
  columnas_reales <- names(datos)[
    seq_len(n_estructura)
  ]


  # Construimos una fila por dimensión para poder comparar fácilmente
  # el nombre esperado con el nombre encontrado en el Excel.
  data.frame(
    id_dataset = id_dataset,
    posicion = seq_len(n_estructura),
    dimension_id = estructura_esperada,
    columna_excel = columnas_reales,
    stringsAsFactors = FALSE
  )
}


auditoria_estructura <- lapply(
  seq_len(nrow(estructura_datasets)),
  function(i) {

    id <- estructura_datasets$id_dataset[i]

    audit_one_dataset(
      id_dataset = id,
      ambito = estructura_datasets$ambito[i],
      dimensiones = estructura_datasets$dimensiones[[i]],
      datos = lista_xlsx[[id]]
    )
  }
)

auditoria_estructura <- do.call(
  rbind,
  auditoria_estructura
)

rownames(auditoria_estructura) <- NULL


# ============================================================================
# 4. Equivalencias entre dimensiones y columnas Excel
# ============================================================================

# Nos interesa especialmente conocer relaciones como:
#
#   dimension_id   columna_excel
#   ------------   -------------
#   EDAD           GRUPOS EDAD
#   TERRITORIO     TERRITORIO
#   SEXO           SEXO
#
# El resumen elimina repeticiones para que podamos revisar únicamente las
# combinaciones distintas encontradas en los 90 datasets.
#

equivalencias_columnas <- unique(
  auditoria_estructura[
    c(
      "dimension_id",
      "columna_excel"
    )
  ]
)

equivalencias_columnas <- equivalencias_columnas[
  order(
    equivalencias_columnas$dimension_id,
    equivalencias_columnas$columna_excel
  ),
]

rownames(equivalencias_columnas) <- NULL


# ============================================================================
# 5. Dimensiones que requieren revisión
# ============================================================================

#
# Una misma dimensión declarada en el identificador puede aparecer con
# distintos nombres de columna en los Excel.
#
# Esto puede deberse a:
#
#   1. diferencias meramente tipográficas;
#   2. distintas denominaciones de un mismo concepto;
#   3. un error en la cabecera del Excel;
#   4. un error en el identificador del fichero.
#
# En esta fase NO corregimos automáticamente ninguna discrepancia.
# Únicamente identificamos los casos que requieren revisión.
# ============================================================================


# ----------------------------------------------------------------------------
# Contar cuántos nombres de columna distintos tiene cada dimensión
# ----------------------------------------------------------------------------

nombres_por_dimension <- aggregate(
  columna_excel ~ dimension_id,
  data = equivalencias_columnas,
  FUN = function(x) length(unique(x))
)

names(nombres_por_dimension)[
  names(nombres_por_dimension) == "columna_excel"
] <- "n_nombres_excel"


# ----------------------------------------------------------------------------
# Seleccionar dimensiones con más de una denominación real
# ----------------------------------------------------------------------------

dimensiones_a_revisar <- nombres_por_dimension[
  nombres_por_dimension$n_nombres_excel > 1L,
]

dimensiones_a_revisar <- dimensiones_a_revisar[
  order(dimensiones_a_revisar$dimension_id),
]


# ----------------------------------------------------------------------------
# Obtener el detalle de los datasets afectados
# ----------------------------------------------------------------------------

detalle_revision <- auditoria_estructura[
  auditoria_estructura$dimension_id %in%
    dimensiones_a_revisar$dimension_id,
  c(
    "id_dataset",
    "posicion",
    "dimension_id",
    "columna_excel"
  )
]

detalle_revision <- detalle_revision[
  order(
    detalle_revision$dimension_id,
    detalle_revision$columna_excel,
    detalle_revision$id_dataset
  ),
]

rownames(detalle_revision) <- NULL


# ============================================================================
# 6. Auditoría de columnas temporales
# ============================================================================

#
# Comprueba que las columnas situadas después de las dimensiones estructurales
# cumplen el patrón temporal esperado según la frecuencia declarada en el
# identificador del dataset.
#
# No modifica ningún objeto.
# ============================================================================


auditar_periodos <- function(id_dataset, estructura, datos) {

  fila <- estructura[
    estructura$id_dataset == id_dataset,
  ]

  # Número de columnas descriptivas:
  #
  #   ámbito territorial + dimensiones adicionales
  n_dimensiones <- 1L + length(
    fila$dimensiones[[1]]
  )

  # Las restantes columnas deberían ser periodos.
  columnas_periodo <- names(datos)[
    seq.int(
      from = n_dimensiones + 1L,
      to = ncol(datos)
    )
  ]

  frecuencia <- fila$frecuencia


  # --------------------------------------------------------------------------
  # Patrón esperado según frecuencia
  # --------------------------------------------------------------------------

  patron <- switch(
    frecuencia,

    A = "^[0-9]{4}$",

    T = "^[0-9]{4}T[1-4]$",

    # De momento no imponemos un formato mensual concreto:
    # queremos descubrir cómo están codificados realmente.
    M = NULL
  )


  # --------------------------------------------------------------------------
  # Resultado
  # --------------------------------------------------------------------------

  if (frecuencia %in% c("A", "T")) {

    validas <- grepl(
      patron,
      columnas_periodo
    )

  } else {

    validas <- rep(
      NA,
      length(columnas_periodo)
    )
  }


  data.frame(
    id_dataset = id_dataset,
    frecuencia = frecuencia,
    columna_periodo = columnas_periodo,
    valida = validas,
    stringsAsFactors = FALSE
  )
}


auditoria_periodos <- do.call(
  rbind,
  lapply(
    names(lista_xlsx),
    function(id) {

      auditar_periodos(
        id_dataset = id,
        estructura = estructura_datasets,
        datos = lista_xlsx[[id]]
      )
    }
  )
)

rownames(auditoria_periodos) <- NULL

periodos_anuales_trimestrales_invalidos <- subset(
  auditoria_periodos,
  frecuencia %in% c("A", "T") &
    !valida
)


# ============================================================================
# 7. Validación específica de periodos mensuales
# ============================================================================

auditoria_mensual <- subset(
  auditoria_periodos,
  frecuencia == "M"
)

auditoria_mensual$valida <- grepl(
  "^[0-9]{4}M(0[1-9]|1[0-2])$",
  auditoria_mensual$columna_periodo
)

periodos_mensuales_invalidos <- subset(
  auditoria_mensual,
  !valida
)


# ============================================================================
# 8. Comprobación de periodos duplicados
# ============================================================================

duplicados_periodo <- do.call(
  rbind,
  lapply(
    split(
      auditoria_periodos,
      auditoria_periodos$id_dataset
    ),
    function(x) {

      dup <- duplicated(x$columna_periodo)

      if (!any(dup)) {
        return(NULL)
      }

      x[dup, c(
        "id_dataset",
        "frecuencia",
        "columna_periodo"
      )]
    }
  )
)


# ============================================================================
# 9. Comprobación final
# ============================================================================

#source("data-raw/metadata.R")
#devtools::load_all()
#source("data-raw/import_raw_data.R")
#source("data-raw/audit_datasets.R")

equivalencias_columnas

dimensiones_a_revisar
detalle_revision

periodos_anuales_trimestrales_invalidos
periodos_mensuales_invalidos

duplicados_periodo

# ============================================================================
# 10. Verificación columnas y estudios
# ============================================================================

columnas_por_dataset <- lapply(
  names(lista_xlsx),
  function(id) {
    data.frame(
      id_dataset = id,
      columna = names(lista_xlsx[[id]]),
      stringsAsFactors = FALSE
    )
  }
)

columnas_por_dataset <- do.call(
  rbind,
  columnas_por_dataset
)

rownames(columnas_por_dataset) <- NULL

subset(
  columnas_por_dataset,
  grepl(
    "TAMA",
    #"ESTUD|FORMAC|NIVEL",
    columna,
    ignore.case = TRUE
  )
)
