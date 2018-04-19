library(tidyverse)
library(leaflet)
library(microbenchmark)
library(data.table)

archivos <- list.files("proyecciones_chorotega", full.names = T)

#este no
tbl <- rbindlist(lapply(archivos, fread, header="auto"))

tbl <- rbindlist(lapply(archivos, fread), idcol = "origen")
tbl[, origen := factor(origen, labels = basename(archivos))]

#este si
tbl <- map(archivos, read.table, sep="", header = TRUE) %>% 
  setNames(archivos) %>% 
  bind_rows(.id = "grp")

#version sin id
tbl <- map(archivos, read.table, sep="", header = TRUE) %>% bind_rows()
saveRDS(tbl, "datoscrudos.rds")

dbWriteTable(con, "tbl_chorotega", tbl)

tbl_tas_year <- tbl %>% filter(Variable == "tas") %>% group_by(Year, Model, Scenario, Longitude, Latitude) %>% summarise(tas_mean = mean(Value))
tbl_pr_year <- tbl %>% filter(Variable == "pr") %>% group_by(Year, Model, Scenario, Longitude, Latitude) %>% summarise(pr_year = sum(Value))

tbl_year <- tbl_tas_year %>% inner_join(tbl_pr_year, by = c("Year", "Model", "Scenario", "Longitude", "Latitude"))
saveRDS(tbl_year, "anual_CIGEFI.rds")

tbl_year_all <- tbl_year %>% group_by(Year, Model, Scenario) %>% summarise(tas_m = mean(tas_mean), pr_y = mean(pr_year))
saveRDS(tbl_year_all, "anual_CIGEFI_TodoChorotega.rds")

#tbl_year <- readRDS("anual_CIGEFI_ID.rds")
#fwrite(tbl_year, file = "anual_CIGEFI_ID.csv")

test <- tbl %>% filter(Scenario == "rpc85" & Variable == "pr") %>% 
  group_by(Year, Model) %>% summarise(pr_mean = max(Value)) %>% filter(Year >= 2030 & Year <= 2060)

test <- tbl_year %>% filter(Year >= 2030 & Year <= 2060 & Scenario == "rpc45") %>% group_by(Year, Model) %>% summarise(tas_mean = mean(tas_mean))

ggplot(test, aes(Year, tas_mean)) + geom_line(data=test, aes(colour = Model)) + stat_smooth(data=test, method="loess", level=0.8, se=F)


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
tbl_year_id <- tbl_year_id %>% select(-xmin, -xmax, -ymin, -ymax)

fwrite(tbl_year_id, file = "anual_CIGEFI_ID.csv")

#tbl_year_id_gt2000 <- tbl_year_id %>% filter(Year >= 2000)
#saveRDS(tbl_year_id_gt2000, "anual_CIGEFI_ID.rds")

test <- tbl_year %>% filter(Year >= 2030 & Year <= 2060 & Scenario == "rpc45") %>% group_by(Year, Model) %>% summarise(tas_mean = mean(tas_mean))

ggplot(test, aes(Year, tas_mean)) + geom_line(data=test, aes(colour = Model)) + stat_smooth(data=test, method="loess", level=0.8, se=F)

#probando con data.table
library(data.table)
dtbl_year_id <- data.table(tbl_year_id)
test <- dtbl_year_id[Year >= 2030 & Year <= 2060 & Scenario == "rpc45"]

microbenchmark(dtbl_year_id[Year >= 2030 & Year <= 2060 & Scenario == "rpc45" & id == 890])

microbenchmark(tbl_year_id %>% filter(Year >= 2030 & Year <= 2060 & Scenario == "rpc45" & id ==890))


#revision de variables
#library(stringr)
#tbl$archivo <- str_sub(tbl$grp, 24, -1)
#counts <- tbl %>% group_by(Model, Scenario, archivo) %>% count(Model)

#calcular percentiles por celda usando datos históricos
tbl_year <- readRDS("anual_CIGEFI.rds")

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


percentiles_CIGEFI_TodoChorotega <- tbl_year_all_clean %>% 
  filter(Year < 2000) %>% 
  ungroup() %>% 
  summarise("tas_95pctl"=quantile(tas_m, probs=0.95),
            "tas_5pctl"=quantile(tas_m, probs=0.05),
            "pr_95pctl"=quantile(pr_y, probs=0.95),
            "pr_5pct"=quantile(pr_y, probs=0.05)
            )
saveRDS(percentiles_CIGEFI_TodoChorotega, "percentiles_CIGEFI_TodoChorotega.rds")

#creación de tablas para variabilidad mensual.
library(odbc)
con <- dbConnect(odbc::odbc(), "PostgreSQL_SIG")
tbl<- tbl(con, "tbl_chorotega")

tbl <- readRDS("datoscrudos.rds")

tbl_month_pr <- tbl %>% filter(Variable == "pr") %>% mutate(pr_month = Value) 
tbl_month_tas <- tbl %>% filter(Variable == "tas") %>% mutate(tas_month = Value)
tbl_month <- tbl_month_pr %>% inner_join(tbl_month_tas, by = c("Year", "Month", "Model", "Scenario", "Longitude", "Latitude")) %>% select(Year, Month, Model, Scenario, Longitude, Latitude, tas_month, pr_month)
#fwrite(collect(tbl_month), file = "mensual_CIGEFI_raw.csv")

saveRDS(tbl_month, "mensual_CIGEFI_raw.rds")

tbl_month <- readRDS("mensual_CIGEFI_raw.rds")

#tbl_month_gt2000 <- tbl_month %>% filter(Year >= 2000)
head(tbl_month_gt2000)

tbl_month_10years <- tbl_month %>%
  filter(Year >= 1980) %>% 
  group_by(Year %/% 10, Month, Longitude, Latitude, Model, Scenario) %>% 
  summarise(tas_mean = mean(tas_month), pr_mean = mean(pr_month)) %>% rename(period = `Year%/%10`)
#head(tbl_month_gt2000_5years)
#tbl_month_gt2000_10years <- collect(tbl_month_gt2000_10years)

periods_10years <- tbl_month_10years %>% group_by(as.integer(period)) %>% summarise(n())
#head(periods_5years)
#periods_10years <- collect(periods_10years)

years10 <- as.data.frame(seq(1980, 2090, 10))
years10 <-years10 %>% rename(ini_year = `seq(1980, 2090, 10)`)
years10 <- cbind(years10, periods_10years)
years10 <- years10 %>% rename(period = `as.integer(period)`)

tbl_month_10years_ini_year <- left_join(tbl_month_10years, years10, by="period") %>% ungroup() %>%
  select(-`n()`, -period)

tbl_month_10years_id <- left_join(tbl_month_10years_ini_year, tabla_shape, by = c("Latitude" = "Lon", "Longitude" = "Lat"))
tbl_month_10years_id <- tbl_month_10years_id %>% ungroup() %>% select(-Longitude, -Latitude, -xmin, -xmax, -ymin, -ymax)

#saveRDS(tbl_month_gt2000_10years_id, "mensual_CIGEFI.rds")
fwrite(tbl_month_10years_id, file = "mensual_CIGEFI.csv")

#para todas las celdas:
tbl_month_10years_all <- tbl_month %>%
  filter(Year >= 1980) %>% 
  group_by(Year %/% 10, Month, Model, Scenario) %>% 
  summarise(tas_mean = mean(tas_month), pr_mean = mean(pr_month)) %>% rename(period = `Year%/%10`)

#tbl_month_gt2000_5years_all_R <- collect(tbl_month_gt2000_5years_all)

periods_10years_all <- tbl_month_10years_all %>% group_by(as.integer(period)) %>% summarise(n()) %>% rename(period = `as.integer(period)`)

years10 <- as.data.frame(seq(1980, 2090, 10))
years10 <-years10 %>% rename(ini_year = `seq(1980, 2090, 10)`)

years10_all <- cbind(years10, periods_10years_all)

tbl_month_10years_all_ini_year <- left_join(tbl_month_10years_all, years10_all, by="period") %>% ungroup() %>%
  select(-`n()`, -period)

saveRDS(tbl_month_10years_all_ini_year, "mensual_CIGEFI_TodoChorotega.rds")

#calcular 5 y 95 percentiles para datos históricos
tbl_month_lt2000 <- tbl_month %>% filter(Year < 2000)

tbl_percentiles_mes <- tbl_month_lt2000 %>%  
  group_by(Month, Longitude, Latitude) %>%
  summarise("tas_95pctl"=quantile(tas_month, probs=0.95),
            "tas_5pctl"=quantile(tas_month, probs=0.05),
            "pr_95pctl"=quantile(pr_month, probs=0.95),
            "pr_5pct"=quantile(pr_month, probs=0.05))

tbl_percentiles_mes <- left_join(tbl_percentiles_mes, tabla_shape, by = c("Latitude" = "Lon", "Longitude" = "Lat"))
saveRDS(tbl_percentiles_mes, "percentiles_CIGEFI_mensual.rds")

tbl_percentiles_mes_all <- tbl_month_lt2000 %>% group_by(Month) %>% 
  summarise("tas_95pctl"=quantile(tas_month, probs=0.95),
            "tas_5pctl"=quantile(tas_month, probs=0.05),
            "pr_95pctl"=quantile(pr_month, probs=0.95),
            "pr_5pct"=quantile(pr_month, probs=0.05))

saveRDS(tbl_percentiles_mes_all, "percentiles_CIGEFI_mensual_TodoChorotega.rds")


#prueba con geom_linerange

mensual_GCMsChorotega <- data.table(readRDS("mensual_CIGEFI_TodoChorotega.rds"))
percentiles_mesChorotega <- readRDS("percentiles_CIGEFI_mensual_TodoChorotega.rds")

sel_mes <- mensual_GCMsChorotega[Scenario == "rpc45" & ini_year >= 2030 & ini_year <= 2050]

ggplot() + geom_jitter(data = sel_mes, aes(x = Month, y = pr_mean, colour = Model), width = 0.2) +
  geom_smooth(data=sel_mes, aes(x = Month, y = pr_mean), method="loess", level=0.5, se=F) +
  geom_linerange(data=percentiles_mesChorotega, aes(x=Month, 
                 ymin=pr_5pct, 
                 ymax=pr_95pctl), linetype="dashed") +
  scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
  labs(x = "Mes", y = "Lluvia (mm)") + 
  labs(title = paste("Total de lluvia mensual"))

#prueba con boxplots transparentes y puntos encima
ggplot() + 
  geom_jitter(data = sel_mes, aes(x = Month, y = pr_mean, colour = Model), width = 0.15) +
  geom_boxplot(data = sel_mes, aes(x = Month, y = pr_mean, group = Month), outlier.colour=NA, fill=NA) +
  geom_linerange(data=percentiles_mesChorotega, aes(x=Month, ymin=pr_5pct, ymax=pr_95pctl), size = 10, alpha = 0.3) +
  scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
  labs(x = "Mes", y = "Lluvia (mm)") + 
  scale_colour_discrete(name="Modelos") +
  labs(title = paste("Total de lluvia mensual"))

  #-----#
mensual_GCMs <- fread("mensual_CIGEFI.csv")
percentiles_mes <- readRDS("percentiles_CIGEFI_mensual.rds")

sel_mes <- mensual_GCMs[Scenario == "rpc45" & ini_year >= 2030 & ini_year <= 2050 & id == 103]
sel_percentiles_mes <- percentiles_mes %>% dplyr::filter(103 == id)

ggplot() + geom_jitter(data = sel_mes, aes(x = Month, y = pr_mean, colour = Model)) +
  geom_smooth(data=sel_mes, aes(x = Month, y = pr_mean), method="loess", level=0.5, se=F) +
  geom_linerange(data=sel_percentiles_mes, aes(x=Month, ymin=pr_5pct, ymax=pr_95pctl), linetype="dashed") +
  scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
  labs(x = "Mes", y = "Lluvia (mm)") + 
  scale_colour_discrete(name="Experimental\nCondition") +
  labs(title = paste("Total de lluvia mensual"))

#tabla con todos los datos desde 1979 hasta 2100 para los modelos con datos para ambos escenarios
tbl_year_clean <- tbl_year_id %>% filter(Model != "ccsm4_r3i1p1" & Model != "cesm1_cam5_r3i1p1")
tbl_year_clean %>% group_by(Model) %>% tally()
fwrite(tbl_year_clean, file = "anual_CIGEFI_ID.csv")

tbl_year_all_clean <- tbl_year_clean %>% group_by(Year, Model, Scenario) %>% summarise(tas_m = mean(tas_mean), pr_y = mean(pr_year))
saveRDS(tbl_year_all_clean, file = "anual_CIGEFI_TodoChorotega.rds")

#cuadro para percentiles anuales
percentilesChorotega <- readRDS("percentiles_CIGEFI_TodoChorotega.rds")
anual_GCMs <- fread("anual_CIGEFI_ID.csv")

seleccion <- anual_GCMs %>% filter(Year >= 2030 & Year <= 2060 & Scenario == "rpc45") %>% group_by(Year, Model) %>% summarise(tas_mean = mean(tas_mean))

percentiles <- data.frame(2030, 2060, percentilesChorotega$tas_5pctl, percentilesChorotega$tas_95pctl)
names(percentiles) <- c("xmini", "xmaxi", "ymini", "ymaxi")

ggplot() + 
  geom_line(data = seleccion, aes(x = Year, y = tas_mean, colour = Model)) +
  geom_rect(data = data.frame(percentiles), 
            aes(xmin = xmini, xmax = xmaxi, ymin = ymini, ymax = ymaxi), 
            fill = "grey", alpha = 0.3)

#prueba percentiles para celdas
percentiles <- data.table(readRDS("percentiles_CIGEFI.rds"))
anual_GCMs <- fread("anual_CIGEFI_ID.csv")






### calculo de rango por año para valores antes del 2000
# calcular max y min por año
tbl_year <- readRDS("anual_CIGEFI.rds")

#para celdas
library(rgdal)
gridcells <- readOGR(dsn = ".", layer = "Celdas_ubicaciones")
tabla_shape <- gridcells@data


max_min_year_lt2000 <- tbl_year %>%
  filter(Year < 2000) %>% 
  group_by(Longitude, Latitude) %>% 
  summarise("tasmax_y" = max(tas_mean),
            "tasmin_y" = min(tas_mean),
            "prmax_y" = max(pr_year),
            "prmin_y" = min(pr_year))

min_max_year_lt2000_id <- left_join(max_min_year_lt2000, tabla_shape, by = c("Latitude" = "Lon", "Longitude" = "Lat"))
min_max_year_lt2000_id <- min_max_year_lt2000_id %>% select(-xmin, -xmax, -ymin, -ymax)

saveRDS(min_max_year_lt2000_id,"extremos_CIGEFI_anual.rds")

#para todo Chorotega
max_min_year_lt2000_Ch <- tbl_year %>%
  filter(Year < 2000) %>% 
  group_by(Year, Model) %>%
  summarise("tas_mean" = mean(tas_mean),
            "pr_year" = mean(pr_year)) %>% 
  group_by() %>% 
  summarise("tasmax_y" = max(tas_mean),
            "tasmin_y" = min(tas_mean),
            "prmax_y" = max(pr_year),
            "prmin_y" = min(pr_year))

saveRDS(max_min_year_lt2000_Ch,"extremos_CIGEFI_anual_Chorotega.rds")



### calculo de rango por mes para valores antes del 2000
#calcular max y min por década para cada mes
tbl_month <- readRDS("mensual_CIGEFI_raw.rds")

#para celdas
max_min_mes_lt2000 <- tbl_month %>% 
  filter(Year < 2000) %>% 
  group_by(Year %/% 10, Month, Longitude, Latitude, Model, Scenario) %>% 
  summarise("tasm" = mean(tas_month),
            "prm" = mean(pr_month)) %>% 
  group_by(Month, Model, Longitude, Latitude) %>% 
  summarise("tasm" = mean(tasm),
            "prm" = mean(prm)) %>% 
  group_by(Month, Longitude, Latitude) %>% 
  summarise("tasmax" = max(tasm),
            "tasmin" = min(tasm),
            "prmax" = max(prm),
            "prmin" = min(prm))

max_min_mes_lt2000_id <- left_join(max_min_mes_lt2000, tabla_shape, by = c("Latitude" = "Lon", "Longitude" = "Lat"))
max_min_mes_lt2000_id <- max_min_mes_lt2000_id %>% select(-xmin, -xmax, -ymin, -ymax)

saveRDS(max_min_mes_lt2000_id,"extremos_CIGEFI_mensual.rds")

#para todo Chorotega
max_min_mes_lt2000_Ch <- tbl_month %>% 
  filter(Year < 2000) %>% 
  group_by(Year %/% 10, Month, Model, Scenario) %>% 
  summarise("tasm" = mean(tas_month),
            "prm" = mean(pr_month)) %>% 
  group_by(Month, Model) %>% 
  summarise("tasm" = mean(tasm),
            "prm" = mean(prm)) %>% 
  group_by(Month) %>% 
  summarise("tasmax" = max(tasm),
            "tasmin" = min(tasm),
            "prmax" = max(prm),
            "prmin" = min(prm))

saveRDS(max_min_mes_lt2000_Ch,"extremos_CIGEFI_mensual_Chorotega.rds")
