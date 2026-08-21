# Valores disponibles de una dimensión

Devuelve los valores disponibles para una columna concreta de un
indicador.

## Uso

``` r
valores_indicador(id_dataset, dimension, buscar = NULL)
```

## Argumentos

- id_dataset:

  Cadena de texto con el identificador del indicador. Los
  identificadores disponibles pueden consultarse con
  [`listar_indicadores()`](https://vpergim.github.io/indicaR/reference/listar_indicadores.md).

- dimension:

  Cadena de texto con el nombre de la columna que se desea inspeccionar.
  Las dimensiones disponibles pueden consultarse con
  [`info_indicador()`](https://vpergim.github.io/indicaR/reference/info_indicador.md).

- buscar:

  Opcionalmente, cadena de texto utilizada para buscar entre los valores
  disponibles de una dimensión de tipo texto. La búsqueda no distingue
  entre mayúsculas y minúsculas y se realiza de forma literal.

## Valor

Un vector con los valores distintos disponibles en la dimensión
solicitada.

## Detalles

Esta función permite conocer qué valores pueden utilizarse como filtros
en
[`consultar_indicador()`](https://vpergim.github.io/indicaR/reference/consultar_indicador.md).

La función valida explícitamente tanto el identificador del indicador
como el nombre de la dimensión. Si alguno no existe, genera un error.

Los valores se devuelven tal como están almacenados en los datos
normalizados del paquete.

En dimensiones con muchos valores, `buscar` permite localizar categorías
sin truncar la lista original de valores disponibles.

## Ver también

[`listar_indicadores()`](https://vpergim.github.io/indicaR/reference/listar_indicadores.md),
[`info_indicador()`](https://vpergim.github.io/indicaR/reference/info_indicador.md),
[`consultar_indicador()`](https://vpergim.github.io/indicaR/reference/consultar_indicador.md)

## Ejemplos

``` r
valores_indicador(
  "TERRITORIO_SEXO_EDAD_A_01",
  "sexo"
)
#> [1] "Hombres" "Mujeres"

valores_indicador(
  "TERRITORIO_SEXO_EDAD_A_01",
  "tipo_territorio"
)
#> [1] "ccaa"     "nacional"

valores_indicador(
  "TERRITORIO_SEXO_EDAD_A_01",
  "territorio",
  buscar = "val"
)
#> [1] "Comunidad Valenciana"
```
