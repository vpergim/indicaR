# ============================================================================
# Auditoría de la normalización de datasets
# ============================================================================
#
# Este script comprueba el funcionamiento de `normalizar_dataset()` utilizando
# datasets representativos de las tres frecuencias temporales existentes:
#
#   - anual;
#   - trimestral;
#   - mensual.
#
# OBJETIVO
# --------
# Detectar problemas introducidos por:
#
#   - cambios en los Excel fuente;
#   - nuevos códigos de valores ausentes;
#   - modificaciones en las columnas temporales;
#   - cambios en `normalizar_dataset()`.
#
# Este script NO modifica los datos originales.
#
# REQUISITOS
# ----------
# Antes de ejecutarlo deben existir:
#
#   - metadata
#   - lista_xlsx
#   - normalizar_dataset()
#
# La forma recomendada de preparar la sesión es:
#
#   source("data-raw/setup_dev.R")
#
# ============================================================================


# ============================================================================
# 1. Dataset anual
# ============================================================================

test_anual <- normalizar_dataset(
  id_dataset = "CCAA_SEXO_A_01",
  datos = lista_xlsx[["CCAA_SEXO_A_01"]]
)


# Comprobaciones básicas de estructura.

stopifnot(
  unique(test_anual$frecuencia) == "A",
  all(is.na(test_anual$trimestre)),
  all(is.na(test_anual$mes)),
  is.integer(test_anual$anyo),
  is.numeric(test_anual$valor)
)


# ============================================================================
# 2. Dataset trimestral
# ============================================================================

test_trimestral <- normalizar_dataset(
  id_dataset = "TERRITORIO_SEXO_EDAD_T_01",
  datos = lista_xlsx[["TERRITORIO_SEXO_EDAD_T_01"]]
)


stopifnot(
  unique(test_trimestral$frecuencia) == "T",
  all(test_trimestral$trimestre %in% c(1L, 2L, 3L, 4L)),
  all(is.na(test_trimestral$mes)),
  is.integer(test_trimestral$anyo),
  is.numeric(test_trimestral$valor)
)


# ============================================================================
# 3. Dataset mensual
# ============================================================================

test_mensual <- normalizar_dataset(
  id_dataset = "TERRITORIO_M_01",
  datos = lista_xlsx[["TERRITORIO_M_01"]]
)


stopifnot(
  unique(test_mensual$frecuencia) == "M",
  all(test_mensual$mes %in% 1:12),
  all(is.na(test_mensual$trimestre)),
  is.integer(test_mensual$anyo),
  is.numeric(test_mensual$valor)
)


# ============================================================================
# 4. Comprobación del formato canónico del periodo
# ============================================================================
#
# Todos los periodos mensuales deben quedar normalizados como:
#
#   YYYYMM -> no
#   YYYYM1 -> no
#   YYYYM01 -> sí
#

stopifnot(
  all(
    grepl(
      "^[0-9]{4}M(0[1-9]|1[0-2])$",
      test_mensual$periodo
    )
  )
)


# ============================================================================
# 5. Resumen informativo
# ============================================================================

message(
  "Auditoría de normalización completada correctamente."
)

message(
  "NA en anual: ",
  sum(is.na(test_anual$valor))
)

message(
  "NA en trimestral: ",
  sum(is.na(test_trimestral$valor))
)

message(
  "NA en mensual: ",
  sum(is.na(test_mensual$valor))
)
