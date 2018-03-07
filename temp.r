library(tidyverse)
library(leaflet)
library(microbenchmarkCore)
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

tbl_year_all <- tbl_year %>% group_by(Year, Model, Scenario) %>% summarise(tas_m = mean(tas_mean), pr_y = mean(pr_year))

saveRDS(tbl_year, "anual_CIGEFI.rds")

tbl_year <- readRDS("anual_CIGEFI_ID.rds")
fwrite(tbl_year, file = "anual_CIGEFI_ID.csv")

saveRDS(tbl_year_all, "anual_CIGEFI_TodoChorotega.rds")


test <- tbl %>% filter(Scenario == "rpc85" & Variable == "pr") %>% 
  group_by(Year, Model) %>% summarise(pr_mean = max(Value)) %>% filter(Year >= 2030 & Year <= 2060)

test <- tbl_year %>% filter(Year >= 2030 & Year <= 2060 & Scenario == "rpc45") %>% group_by(Year, Model) %>% summarise(tas_mean = mean(tas_mean))

ggplot(test, aes(Year, tas_mean)) + geom_point(data=test, aes(colour = Model)) + stat_smooth(data=test, method="loess", level=0.8, se=F)


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

tbl_month_gt2000 <- tbl_month %>% filter(Year >= 2000)
head(tbl_month_gt2000)

tbl_month_gt2000_10years <- tbl_month_gt2000 %>% 
  group_by(Year %/% 10, Month, Longitude, Latitude, Model, Scenario) %>% 
  summarise(tas_mean = mean(tas_month), pr_mean = mean(pr_month)) %>% rename(period = `Year%/%10`)
head(tbl_month_gt2000_5years)
tbl_month_gt2000_10years <- collect(tbl_month_gt2000_10years)

periods_10years <- tbl_month_gt2000_10years %>% group_by(as.integer(period)) %>% summarise(n())
head(periods_5years)
periods_10years <- collect(periods_10years)

years10 <- as.data.frame(seq(2000, 2090, 10))
years10 <-years10 %>% rename(ini_year = `seq(2000, 2090, 10)`)
years10 <- cbind(years10, periods_10years)
years10 <- years10 %>% rename(period = `as.integer(period)`)

tbl_month_gt2000_10years_ini_year <- left_join(tbl_month_gt2000_10years, years10, by="period") %>% ungroup() %>%
  select(-`n()`, -period)

tbl_month_gt2000_10years_id <- left_join(tbl_month_gt2000_10years_ini_year, tabla_shape, by = c("Latitude" = "Lon", "Longitude" = "Lat"))
tbl_month_gt2000_10years_id <- tbl_month_gt2000_10years_id %>% ungroup() %>% select(-Longitude, -Latitude, -xmin, -xmax, -ymin, -ymax)

saveRDS(tbl_month_gt2000_10years_id, "mensual_CIGEFI.rds")
fwrite(tbl_month_gt2000_10years_id, file = "mensual_CIGEFI.csv")

#para todas las celdas:
tbl_month_gt2000_10years_all <- tbl_month_gt2000 %>% 
  group_by(Year %/% 10, Month, Model, Scenario) %>% 
  summarise(tas_mean = mean(tas_month), pr_mean = mean(pr_month)) %>% rename(period = `Year%/%10`)

tbl_month_gt2000_5years_all_R <- collect(tbl_month_gt2000_5years_all)

periods_10years_all <- tbl_month_gt2000_10years_all %>% group_by(as.integer(period)) %>% summarise(n()) %>% rename(period = `as.integer(period)`)
years10_all <- cbind(years10, periods_10years_all)

tbl_month_gt2000_10years_all_ini_year <- left_join(tbl_month_gt2000_10years_all, years10_all, by="period") %>% ungroup() %>%
  select(-`n()`, -period)

saveRDS(tbl_month_gt2000_10years_all_ini_year, "mensual_CIGEFI_TodoChorotega.rds")

#calcular 5 y 95 percentiles para datos históricos
tbl_month_lt2000 <- collect( tbl_month %>% filter(Year < 2000))
  
tbl_percentiles_mes <- tbl_percentiles_mes %>%  
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

ggplot() + geom_jitter(data = sel_mes, aes(x = Month, y = pr_mean, colour = Model)) +
  geom_smooth(data=sel_mes, aes(x = Month, y = pr_mean), method="loess", level=0.5, se=F) +
  geom_linerange(data=percentiles_mesChorotega, aes(x=Month, 
                 ymin=pr_5pct, 
                 ymax=pr_95pctl), linetype="dashed") +
  scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
  labs(x = "Mes", y = "Lluvia (mm)") + 
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
