library(tidyverse)
library(leaflet)
library(microbenchmarkCore)
library(data.table)

archivos <- list.files("proyecciones_chorotega", full.names = T)

#este no
tbl <- rbindlist(lapply(archivos, fread), idcol = "origen")
tbl[, origen := factor(origen, labels = basename(archivos))]

#este si
tbl <- map(archivos, read.table, sep="", header = TRUE) %>% 
  setNames(archivos) %>% 
  bind_rows(.id = "grp")


tbl_tas_year <- tbl %>% filter(Variable == "tas") %>% group_by(Year, Model, Scenario, Longitude, Latitude) %>% summarise(tas_mean = mean(Value))
tbl_pr_year <- tbl %>% filter(Variable == "pr") %>% group_by(Year, Model, Scenario, Longitude, Latitude) %>% summarise(pr_year = sum(Value))

tbl_year <- tbl_tas_year %>% inner_join(tbl_pr_year, by = c("Year", "Model", "Scenario", "Longitude", "Latitude"))

tbl_year_all <- tbl_year %>% group_by(Year, Model, Scenario) %>% summarise(tas_m = mean(tas_mean), pr_y = mean(pr_year))

saveRDS(tbl_year, "anual_CIGEFI.rds")
saveRDS(tbl_year_all, "anual_CIGEFI_TodoChorotega.rds")


test <- tbl %>% filter(Scenario == "rpc85" & Variable == "pr") %>% 
  group_by(Year, Model) %>% summarise(pr_mean = max(Value)) %>% filter(Year >= 2030 & Year <= 2060)

test <- tbl_year %>% filter(Year >= 2030 & Year <= 2060 & Scenario == "rpc45") %>% group_by(Year, Model) %>% summarise(tas_mean = mean(tas_mean))

ggplot(test, aes(Year, tas_mean)) + geom_point(data=test, aes(colour = Model)) + stat_smooth(data=test, method="loess", level=0.8, se=F)


gridcells <- readLines("CeldasDatos_ubicaciones.geojson") %>% paste(collapse = "\n")

leaflet() %>% addGeoJSON(gridcells, weight = 0.5, fillOpacity = 0.2, color = "#444444") %>% 
  addTiles() %>%
  setView(lng=-85.375, lat=10.625, zoom = 9)
                                               

library(rgdal)
gridcells <- readOGR(dsn = ".", layer = "Celdas_ubicaciones")

leaflet() %>% 
  addPolygons(data = gridcells, 
              layerId = gridcells$id, 
              group = "click.list",
              weight = 0.5, 
              fillOpacity = 0.2, 
              color = "#444444") %>% 
  addTiles() %>%
  setView(lng=-85.375, lat=10.625, zoom = 9)


#usar ID de grilla del shape para la tabla de valores climáticos. Ojo con valores invertidos en datos de CIGEFI
head(gridcells@data)
tabla_shape <- gridcells@data
tbl_year_id <- left_join(tbl_year, tabla_shape, by = c("Latitude" = "Lon", "Longitude" = "Lat"))
tbl_year_id_gt2000 <- tbl_year_id %>% filter(Year >= 2000)
saveRDS(tbl_year_id_gt2000, "anual_CIGEFI_ID.rds")

test <- tbl_year_id %>% filter(Year >= 2030 & Year <= 2060 & Scenario == "rpc45") %>% group_by(Year, Model) %>% summarise(tas_mean = mean(tas_mean))

ggplot(test, aes(Year, tas_mean)) + geom_point(data=test, aes(colour = Model)) + stat_smooth(data=test, method="loess", level=0.8, se=F)

#probando con data.table
library(data.table)
dtbl_year_id_gt2000 <- data.table(tbl_year_id_gt2000)
test <- dtbl_year_id_gt2000[Year >= 2030 & Year <= 2060 & Scenario == "rpc45"]

microbenchmark(dtbl_year_id_gt2000[Year >= 2030 & Year <= 2060 & Scenario == "rpc45" & id == 890])

microbenchmark(tbl_year_id_gt2000 %>% filter(Year >= 2030 & Year <= 2060 & Scenario == "rpc45" & id ==890))


#revision de variables
library(stringr)
tbl$archivo <- str_sub(tbl$grp, 24, -1)
counts <- tbl %>% group_by(Model, Scenario, archivo) %>% count(Model)

#calcular percentiles por celda usando datos históricos
tbl_percentiles <- tbl_year %>% 
  filter(Year < 2000) %>% 
  group_by(Longitude, Latitude) %>%
  summarise("tas_95pctl"=quantile(tas_mean, probs=0.95),
            "tas_5pctl"=quantile(tas_mean, probs=0.05),
            "pr_95pctl"=quantile(pr_year, probs=0.95),
            "pr_5pct"=quantile(pr_year, probs=0.05)
            )
tbl_percentiles <- left_join(tbl_percentiles, tabla_shape, by = c("Latitude" = "Lon", "Longitude" = "Lat"))

saveRDS(tbl_percentiles, "percentiles_CIGEFI.rds")

percentiles_CIGEFI_TodoChorotega <- anual_CIGEFI_TodoChorotega %>% 
  filter(Year < 2000) %>% 
  group_by() %>% 
  summarise("tas_95pctl"=quantile(tas_m, probs=0.95),
            "tas_5pctl"=quantile(tas_m, probs=0.05),
            "pr_95pctl"=quantile(pr_y, probs=0.95),
            "pr_5pct"=quantile(pr_y, probs=0.05)
            )
saveRDS(percentiles_CIGEFI_TodoChorotega, "percentiles_CIGEFI_TodoChorotega.rds")
