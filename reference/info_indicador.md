# Información de un indicador

Muestra información descriptiva y estructural de un indicador disponible
en el paquete. Es útil para conocer su contenido antes de consultar los
datos.

## Usage

``` r
info_indicador(id_dataset)
```

## Arguments

- id_dataset:

  Cadena de texto con el identificador del indicador. Los
  identificadores disponibles pueden consultarse con
  [`listar_indicadores()`](https://vpergim.github.io/indicaR/reference/listar_indicadores.md).

## Value

Una lista con los siguientes elementos:

- metadata:

  Tabla de una fila con la información descriptiva del indicador,
  incluyendo su variable, operación estadística, organismo, período
  disponible y, cuando existe, comentarios y URL de la fuente.

- n_observaciones:

  Número total de observaciones almacenadas para el indicador.

- cobertura:

  Lista con el primer año disponible (`anyo_min`), el último año
  disponible (`anyo_max`) y la frecuencia temporal (`frecuencia`). Los
  valores de `frecuencia` son A - anual, T - trimestral y M - mensual.

- dimensiones:

  Tabla con las columnas disponibles además del núcleo común del
  indicador. Incluye el nombre de cada dimensión y el número de valores
  distintos disponibles en ella.

      Puede incluir tanto dimensiones originales como variables derivadas
      creadas durante la normalización.

## See also

[`listar_indicadores()`](https://vpergim.github.io/indicaR/reference/listar_indicadores.md)

## Examples

``` r
listar_indicadores()
#> # A tibble: 90 × 6
#>    id_dataset                   variable comentarios operacion organismo periodo
#>    <chr>                        <chr>    <chr>       <chr>     <chr>     <chr>  
#>  1 CCAA_SEXO_A_01               ESPERAN… NA          Indicado… INE       1975-2…
#>  2 CCAA_SEXO_A_02               ESPERAN… NA          Indicado… INE       1975-2…
#>  3 CCAA_SEXO_EDAD_A_01          TASA DE… En el item… Tablas d… INE       1991-2…
#>  4 CCAA_SEXO_EDAD_A_02          ESPERAN… En el item… Tablas d… INE       1991-2…
#>  5 CCAA_SEXO_EDAD_NIVELEDUCATI… ESPERAN… NA          Indicado… INE       2016-2…
#>  6 CV_SECT1_TAMANYOEMP_A_01     EMPRESA… NA          DIRCE     INE       2020-2…
#>  7 CVPROV_TIPOVIVIENDA_M_01     COMPRAV… NA          Estadíst… INE       2007M0…
#>  8 TERRITORIO_A_01              INTENSI… Se descarg… Encuesta… INE       2014-2…
#>  9 TERRITORIO_A_02              GASTOS … Se descarg… Encuesta… INE       2014-2…
#> 10 TERRITORIO_A_03              TOTAL C… En el item… Encuesta… INE       2015-2…
#> # ℹ 80 more rows

info_indicador("TERRITORIO_SEXO_EDAD_A_01")
#> $metadata
#> # A tibble: 1 × 7
#>   id_dataset              variable comentarios operacion organismo periodo url  
#>   <chr>                   <chr>    <chr>       <chr>     <chr>     <chr>   <chr>
#> 1 TERRITORIO_SEXO_EDAD_A… POBLACI… NA          AGENDA20… INE       2015-2… http…
#> 
#> $n_observaciones
#> [1] 880
#> 
#> $cobertura
#> $cobertura$anyo_min
#> [1] 2015
#> 
#> $cobertura$anyo_max
#> [1] 2025
#> 
#> $cobertura$frecuencia
#> [1] "A"
#> 
#> 
#> $dimensiones
#>          dimension n_valores
#> 1       territorio        20
#> 2  tipo_territorio         2
#> 3         cod_ccaa        19
#> 4             ccaa        19
#> 5    cod_provincia         0
#> 6        provincia         0
#> 7             sexo         2
#> 8             edad         2
#> 9         edad_min         2
#> 10        edad_max         2
#> 11       edad_tipo         1
#> 
```
