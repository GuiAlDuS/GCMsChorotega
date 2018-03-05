library(tidyverse)
library(leaflet)
library(microbenchmarkCore)

archivos <- list.files("proyecciones_chorotega", full.names = T)

tbl <- lapply(archivos, read.table, sep="", head = T) %>% bind_rows()

tbl_tas_year <- tbl %>% filter(Variable == "tas") %>% group_by(Year, Model, Scenario, Longitude, Latitude) %>% summarise(tas_mean = mean(Value))
tbl_pr_year <- tbl %>% filter(Variable == "pr") %>% group_by(Year, Model, Scenario, Longitude, Latitude) %>% summarise(pr_year = sum(Value))

tbl_year <- tbl_tas_year %>% inner_join(tbl_pr_year, by = c("Year", "Model", "Scenario", "Longitude", "Latitude"))

tbl_year_all <- tbl_year %>% group_by(Year, Model, Scenario) %>% summarise(tas_m = mean(tas_mean), pr_y = mean(pr_year))

saveRDS(tbl_year, "anual_CIGEFI.rds")
saveRDS(tbl_year_all, "anual_CIGEFI_TodoChorotega.rds")

tbl %>% group_by(Model) %>% count(Model)

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


#usar ID de grilla del shape para la tabla de valores climáticos
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
