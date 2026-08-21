# Consultar un indicador

Devuelve los datos de un indicador y permite filtrarlos por cualquiera
de las dimensiones disponibles en el dataset normalizado.

## Usage

``` r
consultar_indicador(id_dataset, ...)
```

## Arguments

- id_dataset:

  Cadena de texto con el identificador del indicador. Los
  identificadores disponibles pueden consultarse con
  [`listar_indicadores()`](https://vpergim.github.io/indicaR/reference/listar_indicadores.md).

- ...:

  Filtros aplicados a las columnas del indicador. Cada filtro debe
  indicarse como `columna = valor`. Se pueden proporcionar varios
  valores mediante un vector, por ejemplo `anyo = c(2023, 2024)`.

## Value

Un `data.frame` con las observaciones que cumplen los filtros.

## Details

Los filtros se especifican mediante argumentos con nombre. El nombre
debe coincidir con una columna disponible en el indicador y los valores
solicitados deben existir en dicha columna.

Los nombres y valores de los filtros se validan explícitamente. Si se
solicita una columna inexistente o un valor que no está presente en el
indicador, la función genera un error en lugar de ignorarlo.

Cuando, después de aplicar los filtros solicitados, siguen presentes
varios tipos de territorio, debe indicarse explícitamente
`tipo_territorio`. Esto evita mezclar inadvertidamente niveles
territoriales distintos.

Las dimensiones disponibles para cada indicador pueden consultarse con
[`info_indicador()`](https://vpergim.github.io/indicaR/reference/info_indicador.md).

## See also

[`listar_indicadores()`](https://vpergim.github.io/indicaR/reference/listar_indicadores.md),
[`info_indicador()`](https://vpergim.github.io/indicaR/reference/info_indicador.md)

## Examples

``` r
consultar_indicador(
  "TERRITORIO_SEXO_EDAD_A_01",
  anyo = 2024,
  sexo = "Mujeres",
  tipo_territorio = "ccaa"
)
#> # A tibble: 38 × 21
#>    id_dataset variable operacion organismo frecuencia territorio tipo_territorio
#>    <chr>      <chr>    <chr>     <chr>     <chr>      <chr>      <chr>          
#>  1 TERRITORI… POBLACI… AGENDA20… INE       A          Andalucía  ccaa           
#>  2 TERRITORI… POBLACI… AGENDA20… INE       A          Andalucía  ccaa           
#>  3 TERRITORI… POBLACI… AGENDA20… INE       A          Aragón     ccaa           
#>  4 TERRITORI… POBLACI… AGENDA20… INE       A          Aragón     ccaa           
#>  5 TERRITORI… POBLACI… AGENDA20… INE       A          Principad… ccaa           
#>  6 TERRITORI… POBLACI… AGENDA20… INE       A          Principad… ccaa           
#>  7 TERRITORI… POBLACI… AGENDA20… INE       A          Illes Bal… ccaa           
#>  8 TERRITORI… POBLACI… AGENDA20… INE       A          Illes Bal… ccaa           
#>  9 TERRITORI… POBLACI… AGENDA20… INE       A          Canarias   ccaa           
#> 10 TERRITORI… POBLACI… AGENDA20… INE       A          Canarias   ccaa           
#> # ℹ 28 more rows
#> # ℹ 14 more variables: cod_ccaa <chr>, ccaa <chr>, cod_provincia <chr>,
#> #   provincia <chr>, sexo <chr>, edad <chr>, edad_min <int>, edad_max <int>,
#> #   edad_tipo <chr>, periodo <chr>, anyo <int>, trimestre <int>, mes <int>,
#> #   valor <dbl>
```
