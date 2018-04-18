Preparación de datos para visualizaciones de GCMs
================
Guillermo A. Durán

Paso a paso de la preparación de los datos para las visualizaciones de GCMs.

Paquetes utilizados:

``` r
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 2.2.1     ✔ purrr   0.2.4
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.4
    ## ✔ tidyr   0.8.0     ✔ stringr 1.3.0
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

``` r
library(rgdal)
```

    ## Loading required package: sp

    ## rgdal: version: 1.2-18, (SVN revision 718)
    ##  Geospatial Data Abstraction Library extensions to R successfully loaded
    ##  Loaded GDAL runtime: GDAL 2.2.1, released 2017/06/23
    ##  Path to GDAL shared files: /usr/share/gdal/2.2
    ##  GDAL binary built with GEOS: TRUE 
    ##  Loaded PROJ.4 runtime: Rel. 4.9.3, 15 August 2016, [PJ_VERSION: 493]
    ##  Path to PROJ.4 shared files: (autodetected)
    ##  Linking to sp version: 1.2-7

1.  Lectura de tablas de GCMs

Generar nómbres de todos los archivos del directorio

``` r
archivos <- list.files("proyecciones_chorotega", full.names = T)
```

Crear tabla juntando todos los archivos individuales

``` r
tbl <- map(archivos, read.table, sep="", header = TRUE) %>% bind_rows()
```

Hacer la tabla "tidy" como tbl\_year:

``` r
tbl_tas_year <- tbl %>% filter(Variable == "tas") %>% group_by(Year, Model, Scenario, Longitude, Latitude) %>% summarise(tas_mean = mean(Value))
tbl_pr_year <- tbl %>% filter(Variable == "pr") %>% group_by(Year, Model, Scenario, Longitude, Latitude) %>% summarise(pr_year = sum(Value))
```

Hacer dos tablas, una con los datos por cada celda y otra con los datos de toda el área:

``` r
tbl_year <- tbl_tas_year %>% inner_join(tbl_pr_year, by = c("Year", "Model", "Scenario", "Longitude", "Latitude"))

tbl_year_all <- tbl_year %>% group_by(Year, Model, Scenario) %>% summarise(tas_m = mean(tas_mean), pr_y = mean(pr_year))
saveRDS(tbl_year_all, "anual_CIGEFI_TodoChorotega.rds")
```

1.  Creación de tabla con identificadores geográficos

Leer el shapefile con un indicador de cada celda:

``` r
library(rgdal)
gridcells <- readOGR(dsn = ".", layer = "Celdas_ubicaciones")
```

    ## OGR data source with driver: ESRI Shapefile 
    ## Source: "/home/cigefi/Guillermo/GCMsChorotega", layer: "Celdas_ubicaciones"
    ## with 614 features
    ## It has 7 fields
    ## Integer64 fields read as strings:  id

Usar ID de grilla del shape para la tabla de valores climáticos. (Ojo con valores invertidos en datos de CIGEFI)

``` r
tabla_shape <- gridcells@data
tbl_year_id <- left_join(tbl_year, tabla_shape, by = c("Latitude" = "Lon", "Longitude" = "Lat"))
tbl_year_id <- tbl_year_id %>% select(-xmin, -xmax, -ymin, -ymax)
```

Salvar tabla:

``` r
fwrite(tbl_year_id, file = "anual_CIGEFI_ID.csv")
```

Crear tablas para los GCMs que tienen ambos escenarios:

``` r
tbl_year_clean <- tbl_year_id %>% filter(Model != "ccsm4_r3i1p1" & Model != "cesm1_cam5_r3i1p1")

tbl_year_all_clean <- tbl_year_clean %>% group_by(Year, Model, Scenario) %>% summarise(tas_m = mean(tas_mean), pr_y = mean(pr_year))
```

Salvar tablas:

``` r
fwrite(tbl_year_clean, file = "anual_CIGEFI_ID.csv")

saveRDS(tbl_year_all_clean, file = "anual_CIGEFI_TodoChorotega.rds")
```

1.  Creación de tablas con valores mensuales

``` r
tbl_month_pr <- tbl %>% filter(Variable == "pr") %>% mutate(pr_month = Value) 
tbl_month_tas <- tbl %>% filter(Variable == "tas") %>% mutate(tas_month = Value)
tbl_month <- tbl_month_pr %>% inner_join(tbl_month_tas, by = c("Year", "Month", "Model", "Scenario", "Longitude", "Latitude")) %>% select(Year, Month, Model, Scenario, Longitude, Latitude, tas_month, pr_month)
```

Calcular los valores mensuales para cada 10 años: (notese que se están eliminando los datos de 1979 para poder hacer los gráficos por décadas)

``` r
tbl_month_10years <- tbl_month %>%
  filter(Year >= 1980) %>% 
  group_by(Year %/% 10, Month, Longitude, Latitude, Model, Scenario) %>% 
  summarise(tas_mean = mean(tas_month), pr_mean = mean(pr_month)) %>% rename(period = `Year%/%10`)

periods_10years <- tbl_month_10years %>% group_by(as.integer(period)) %>% summarise(n())

years10 <- as.data.frame(seq(1980, 2090, 10))
years10 <-years10 %>% rename(ini_year = `seq(1980, 2090, 10)`)
years10 <- cbind(years10, periods_10years)
years10 <- years10 %>% rename(period = `as.integer(period)`)

tbl_month_10years_ini_year <- left_join(tbl_month_10years, years10, by="period") %>% ungroup() %>%
  select(-`n()`, -period)

tbl_month_10years_id <- left_join(tbl_month_10years_ini_year, tabla_shape, by = c("Latitude" = "Lon", "Longitude" = "Lat"))
tbl_month_10years_id <- tbl_month_10years_id %>% ungroup() %>% select(-Longitude, -Latitude, -xmin, -xmax, -ymin, -ymax)
```

Salvar tabla:

``` r
fwrite(tbl_month_10years_id, file = "mensual_CIGEFI.csv")
```

Calculo de valores mensuales para toda el área:

``` r
tbl_month_10years_all <- tbl_month %>%
  filter(Year >= 1980) %>% 
  group_by(Year %/% 10, Month, Model, Scenario) %>% 
  summarise(tas_mean = mean(tas_month), pr_mean = mean(pr_month)) %>% rename(period = `Year%/%10`)

periods_10years_all <- tbl_month_10years_all %>% group_by(as.integer(period)) %>% summarise(n()) %>% rename(period = `as.integer(period)`)

years10 <- as.data.frame(seq(1980, 2090, 10))
years10 <-years10 %>% rename(ini_year = `seq(1980, 2090, 10)`)

years10_all <- cbind(years10, periods_10years_all)

tbl_month_10years_all_ini_year <- left_join(tbl_month_10years_all, years10_all, by="period") %>% ungroup() %>%
  select(-`n()`, -period)
```

Salvar tabla:

``` r
saveRDS(tbl_month_10years_all_ini_year, "mensual_CIGEFI_TodoChorotega.rds")
```

1.  Calculos de percentiles para datos históricos (En caso de utilizarse como en la versión 1.0) Percentiles anuales:

``` r
tbl_percentiles <- tbl_year %>% 
  filter(Year < 2000) %>% 
  group_by(Longitude, Latitude) %>%
  summarise("tas_95pctl"=quantile(tas_mean, probs=0.95),
            "tas_5pctl"=quantile(tas_mean, probs=0.05),
            "pr_95pctl"=quantile(pr_year, probs=0.95),
            "pr_5pct"=quantile(pr_year, probs=0.05)
            )
tbl_percentiles <- left_join(tbl_percentiles, tabla_shape, by = c("Latitude" = "Lon", "Longitude" = "Lat"))


percentiles_CIGEFI_TodoChorotega <- tbl_year_all_clean %>% 
  filter(Year < 2000) %>%
  ungroup() %>% 
  summarise("tas_95pctl"=quantile(tas_m, probs=0.95),
            "tas_5pctl"=quantile(tas_m, probs=0.05),
            "pr_95pctl"=quantile(pr_y, probs=0.95),
            "pr_5pct"=quantile(pr_y, probs=0.05)
            )
```

Percentiles mensuales:

``` r
tbl_month_lt2000 <- tbl_month %>% filter(Year < 2000)

tbl_percentiles_mes <- tbl_month_lt2000 %>%  
  group_by(Month, Longitude, Latitude) %>%
  summarise("tas_95pctl"=quantile(tas_month, probs=0.95),
            "tas_5pctl"=quantile(tas_month, probs=0.05),
            "pr_95pctl"=quantile(pr_month, probs=0.95),
            "pr_5pct"=quantile(pr_month, probs=0.05))

tbl_percentiles_mes <- left_join(tbl_percentiles_mes, tabla_shape, by = c("Latitude" = "Lon", "Longitude" = "Lat"))

tbl_percentiles_mes_all <- tbl_month_lt2000 %>% group_by(Month) %>% 
  summarise("tas_95pctl"=quantile(tas_month, probs=0.95),
            "tas_5pctl"=quantile(tas_month, probs=0.05),
            "pr_95pctl"=quantile(pr_month, probs=0.95),
            "pr_5pct"=quantile(pr_month, probs=0.05))
```

Salvar tabla:

``` r
saveRDS(tbl_percentiles_mes, "percentiles_CIGEFI_mensual.rds")
saveRDS(tbl_percentiles_mes_all, "percentiles_CIGEFI_mensual_TodoChorotega.rds")
```
