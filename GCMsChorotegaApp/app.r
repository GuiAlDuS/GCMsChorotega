library(shiny)
library(ggplot2)
library(gridExtra)
library(grid)
library(rgdal)
library(leaflet)
library(data.table)
library(shinyWidgets)
library(scales)

#importar archivos
anual_GCMs <- fread("anual_CIGEFI_ID.csv")
anual_GCMs_Ch <- data.table(readRDS("anual_CIGEFI_TodoChorotega.rds"))
gridcells <- readOGR(dsn = ".", layer = "Celdas_ubicaciones")
percentiles <- data.table(readRDS("percentiles_CIGEFI.rds"))
percentilesChorotega <- readRDS("percentiles_CIGEFI_TodoChorotega.rds")
mensual_GCMs <- fread("mensual_CIGEFI.csv")
mensual_GCMsChorotega <- data.table(readRDS("mensual_CIGEFI_TodoChorotega.rds"))
percentiles_mes <- readRDS("percentiles_CIGEFI_mensual.rds")
percentiles_mesChorotega <- readRDS("percentiles_CIGEFI_mensual_TodoChorotega.rds")
extremos_mes <- data.table(readRDS("extremos_CIGEFI_mensual.rds"))
extremos_mesChorotega <- readRDS("extremos_CIGEFI_mensual_Chorotega.rds")
extremos_anual <- data.table(readRDS("extremos_CIGEFI_anual.rds"))
extremos_anualChorotega <- readRDS("extremos_CIGEFI_anual_Chorotega.rds")

#funciones
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

ui <- fluidPage(
  titlePanel("Exploración gráfica de modelos climáticos globales (GCMs) para la región Chorotega. Costa Rica."),
  
  sidebarLayout(
    sidebarPanel(
      p(tags$strong("Seleccionar celda:")),
      leafletOutput(outputId = "myMap"),
      br(),
      radioButtons("cp", "Seleccionar escenario futuro:",
                   choices = list("RCP 4.5" = "rpc45",
                                  "RCP 8.5" = "rpc85")),
      pickerInput("modelos", "Seleccionar modelos a graficar:",
                  choices = list("ccsm4_r1i1p1", 
                                 "ccsm4_r2i1p1", 
                                 "cesm1_cam5_r1i1p1",
                                 "cesm1_cam5_r2i1p1",
                                 "cmcc_cms_r1i1p1",
                                 "ec_earth_r2i1p1",
                                 "giss_e2_r_r1i1p1",
                                 "miroc5_r1i1p1",
                                 "miroc5_r3i1p1",
                                 "mpi_esm_lr_r1i1p1",
                                 "mpi_esm_lr_r2i1p1",
                                 "mpi_esm_lr_r3i1p1"),
                  selected = list("ccsm4_r1i1p1", 
                                  "ccsm4_r2i1p1", 
                                  "cesm1_cam5_r1i1p1",
                                  "cesm1_cam5_r2i1p1",
                                  "cmcc_cms_r1i1p1",
                                  "ec_earth_r2i1p1",
                                  "giss_e2_r_r1i1p1",
                                  "miroc5_r1i1p1",
                                  "miroc5_r3i1p1",
                                  "mpi_esm_lr_r1i1p1",
                                  "mpi_esm_lr_r2i1p1",
                                  "mpi_esm_lr_r3i1p1"),
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Ninguno",
                    `select-all-text` = "Todos",
                    `none-selected-text` = "No hay selección",
                    `selected-text-format` = "count",
                    `count-selected-text` = "{0} modelos seleccionados"),
                  multiple = TRUE),
      sliderInput("aNo", "Seleccionar periodo de años:",
                  min = 1980, max = 2100, value = c(2030, 2060), step = 10, sep = ""),
      checkboxInput("loess", "Mostrar línea de tendencia.", value = F),
      checkboxInput("extremos", "Mostrar rango promedio de los valores históricos modelados (1979 a 1999).", value = F),
      h5("Nota:"),
      p("- Si no se selecciona una celda, los gráficos muestran los valores para el área que cubre todas las celdas."),
      p("- La línea de tendencia está calculada por medio de una regresión local (LOESS)."),
      p(""),
      ("- Los GCMs utilizados se seleccionaron con base en el estudio"),tags$a(href="http://onlinelibrary.wiley.com/doi/10.1002/joc.4216/abstract", tags$i("Skill of CMIP5 climate models in reproducing 20th century basic climate features in Central America")), 
      ("de Hidalgo y Alfaro (2014). El cambio de escala a 5km x 5km que se les hizo a los GCMs también fue realizado por ellos."),
      p(""),
      ("- "), tags$a(href="http://www.oscc.gob.es/es/general/salud_cambio_climatico/Nuevos_escenarios_emision_RCPs.htm", "En este enlace"),(" se encuentra información en español sobre los escenarios"), tags$i("Representative Concentration Pathways"),("(RCP) del Quinto Informe del IPCC."),
      p(""),
      p(""),
      p("La herramienta se elaboró como parte de la colaboración entre el Centro de Investigaciones Geofísicas de la Universidad de Costa Rica (CIGEFI) y el Centro de Recursos Hídricos para Centroamérica y el Caribe de la Universidad Nacional de Costa Rica (HIDROCEC)."),
      ("Herramienta desarrollada por"), tags$a(href="https://github.com/GuiAlDuS", "Guillermo Durán"),("en R/Shiny."),
      p(""),
      p("Última actualización 16-4-2018.")
      ),
      
    mainPanel(
      plotOutput("grafico1"))
  )
)

server <- function(input,output,session) {

  #mapa para selecciones
  foundational.map <- function(){
    leaflet() %>%  
      addTiles() %>%
      addPolygons(data = gridcells, 
                  weight = 0.5, 
                  fillOpacity = 0.2, 
                  opacity = 0.5, 
                  color = "#444444",
                  layerId = gridcells$id, 
                  group = "click.list") %>% 
      setView(lng=-85.186, lat=10.451, zoom = 8)
  }
  
  myMap_reval <- reactiveVal(foundational.map())
  output$myMap <- renderLeaflet({
    myMap_reval()
  }) 
  
  click.list <- reactiveValues( ids = vector() )
  
  observeEvent( input$myMap_shape_click, ignoreNULL = T, ignoreInit = T, {
    
    # Eliminar selección anterior si existiese
    if(length(click.list)>0)
    {
      remove_id = click.list$ids
      lines.of.interest <- gridcells[ which( gridcells$id %in% remove_id) , ]
      leafletProxy( mapId = "myMap" ) %>%
        addPolygons(data = lines.of.interest, 
                    weight = 0.5, 
                    fillOpacity = 0.2, 
                    opacity = 0.5, 
                    color = "#444444",
                    layerId = lines.of.interest@data$id
        )
    }
    
    # Agregar selección actual
    click <- input$myMap_shape_click
    click.list$ids <- click$id  # solo guardar último click
    lines.of.interest <- gridcells[ which( gridcells$id %in% click.list$ids ) , ]

    if( is.null( click$id ) ){
      req( click$id )
    } else {
      leafletProxy( mapId = "myMap" ) %>%
        addPolylines(data = lines.of.interest,
                     layerId = lines.of.interest@data$id,
                     color = "red",
                     weight = 5,
                     opacity = 1
        ) 
    }
  })

  #graficos
  output$grafico1 <- renderPlot({
    #funciones de selección para todas las celdas
    if(is.null(input$myMap_shape_click)) {
      seleccion <- anual_GCMs_Ch[Scenario == input$cp & 
                                   Model %in% input$modelos &
                                   Year >= input$aNo[1] & 
                                   Year <= input$aNo[2]]
      
      sel_mes <- mensual_GCMsChorotega[Scenario == input$cp &
                                         Model %in% input$modelos &
                                         ini_year >= input$aNo[1] & 
                                         ini_year <= input$aNo[2]-10]
      
      extr_ch <- data.frame(input$aNo[1], input$aNo[2], 
                                extremos_anualChorotega$tasmin_y, 
                                extremos_anualChorotega$tasmax_y, 
                                extremos_anualChorotega$prmin_y,
                                extremos_anualChorotega$prmax_y)
      names(extr_ch) <- c("xmin", "xmax", "tas_min", "tas_max", "pr_min", "pr_max")
      
      #graficos como objetos para todas las celdas
      g1 <- ggplot() + 
        geom_line(data = seleccion, aes(x = Year, y = tas_m, colour = Model)) +
        labs(x = "Años", y = "Temperatura (C)") + 
        scale_colour_discrete(name="Modelos") +
        labs(title = paste("Promedio anual de temperatura mensual"))
      g2 <- ggplot() + 
        geom_line(data = seleccion, aes(x = Year, y = pr_y, colour = Model)) +
        labs(x = "Años", y = "Lluvia (mm)") + 
        labs(title = paste("Total de lluvia anual"))
      g3 <- ggplot() + 
        geom_jitter(data = sel_mes, aes(x = Month, y = pr_mean, colour = Model), width = 0.15) +
        geom_boxplot(data = sel_mes, aes(x = Month, y = pr_mean, group = Month), outlier.colour=NA, fill=NA) +
        scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
        labs(x = "Mes", y = "Lluvia (mm)") + 
        labs(title = paste("Total de lluvia mensual por década"))
      g4 <- ggplot() + 
        geom_jitter(data = sel_mes, aes(x = Month, y = tas_mean, colour = Model), width = 0.15) +
        geom_boxplot(data = sel_mes, aes(x = Month, y = tas_mean, group = Month), outlier.colour=NA, fill=NA) +
        scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
        labs(x = "Mes", y = "Temperatura (C)") + 
        labs(title = paste("Promedio de temperatura mensual por década"))
      
      #grafico en blanco si no hay selección de GCMs
      if(is.null(input$modelos)){
      } else if (input$loess == F && input$extremos == F) {
        #gráfico sin línea de tendencia
        grid_arrange_shared_legend(g1,g4,g2,g3)
      } else if (input$loess == F && input$extremos == T) {
        g1 <- g1 +
          geom_rect(data = extr_ch, 
                    aes(xmin = xmin, xmax = xmax, ymin = tas_min, ymax = tas_max), 
                    fill = "black", alpha = 0.15)
        g2 <- g2 + 
          geom_rect(data = extr_ch, 
                    aes(xmin = xmin, xmax = xmax, ymin = pr_min, ymax = pr_max), 
                    fill = "black", alpha = 0.15)
        g3 <- g3 + geom_linerange(data=extremos_mesChorotega, aes(x=Month, ymin=prmin, ymax=prmax), size = 10, alpha = 0.15)
        g4 <- g4 + geom_linerange(data=extremos_mesChorotega, aes(x=Month, ymin=tasmin, ymax=tasmax), size = 10, alpha = 0.15)
        grid_arrange_shared_legend(g1,g4,g2,g3)
      } else if (input$loess == T && input$extremos == F) {
        g1 <- g1 + stat_smooth(data=seleccion, aes(x = Year, y = tas_m), method="loess", level=0.5, se=F) 
        g2 <- g2 + stat_smooth(data=seleccion, aes(x = Year, y = pr_y), method="loess", level=0.5, se=F)
        g3 <- g3 + geom_smooth(data=sel_mes, aes(x = Month, y = pr_mean), method="loess", level=0.5, se=F)
        g4 <- g4 + geom_smooth(data=sel_mes, aes(x = Month, y = tas_mean), method="loess", level=0.5, se=F)
        grid_arrange_shared_legend(g1,g4,g2,g3)
      } else if (input$loess == T && input$extremos == T) {
        g1 <- g1 + geom_rect(data = extr_ch, 
                             aes(xmin = xmin, xmax = xmax, ymin = tas_min, ymax = tas_max), 
                             fill = "black", alpha = 0.15) +
          stat_smooth(data=seleccion, aes(x = Year, y = tas_m), method="loess", level=0.5, se=F)
        g2 <- g2 + geom_rect(data = extr_ch, 
                             aes(xmin = xmin, xmax = xmax, ymin = pr_min, ymax = pr_max), 
                             fill = "black", alpha = 0.15) +
          stat_smooth(data=seleccion, aes(x = Year, y = pr_y), method="loess", level=0.5, se=F)
        g3 <- g3 + geom_smooth(data=sel_mes, aes(x = Month, y = pr_mean), method="loess", level=0.5, se=F) +
          geom_linerange(data=extremos_mesChorotega, aes(x=Month, ymin=prmin, ymax=prmax), size = 10, alpha = 0.2)
        g4 <- g4 + geom_smooth(data=sel_mes, aes(x = Month, y = tas_mean), method="loess", level=0.5, se=F) +
          geom_linerange(data=extremos_mesChorotega, aes(x=Month, ymin=tasmin, ymax=tasmax), size = 10, alpha = 0.2)
        grid_arrange_shared_legend(g1,g4,g2,g3)
      }
      
      
    } else {
      #funciones de selección para celda seleccionada
      seleccion <- anual_GCMs[(input$myMap_shape_click[1]) == id &
                                Model %in% input$modelos &
                                Scenario == input$cp & 
                                Year >= input$aNo[1] & Year <= input$aNo[2]] 
      sel_extr <- extremos_anual[(input$myMap_shape_click[1]) == id]
      sel_mes <- mensual_GCMs[(input$myMap_shape_click[1]) == id &
                                Model %in% input$modelos &
                                Scenario == input$cp & 
                                ini_year >= input$aNo[1] & ini_year <= input$aNo[2]-10]
      sel_extr_mes <- extremos_mes[(input$myMap_shape_click[1]) == id]
      extr <- data.frame(input$aNo[1], input$aNo[2], 
                         sel_extr$tasmin_y,
                         sel_extr$tasmax_y, 
                         sel_extr$prmin_y,
                         sel_extr$prmax_y)
      names(extr) <- c("xmin", "xmax", "tas_min", "tas_max", "pr_min", "pr_max")
      
      #graficos como objetos para celdas seleccionadas
      g1 <- ggplot() + 
        geom_line(data = seleccion, aes(x = Year, y = tas_mean, colour = Model)) + 
        scale_colour_discrete(name="Modelos") +
        labs(x = "Años", y = "Temperatura (C)") + 
        labs(title = paste("Promedio anual de temperatura mensual"))
      g2 <- ggplot() + 
        geom_line(data = seleccion, aes(x = Year, y = pr_year, colour = Model)) +
        labs(x = "Años", y = "Lluvia (mm)") + 
        labs(title = paste("Total de lluvia anual"))
      g3 <- ggplot() + 
        geom_jitter(data = sel_mes, aes(x = Month, y = pr_mean, colour = Model), width = 0.15) +
        geom_boxplot(data = sel_mes, aes(x = Month, y = pr_mean, group = Month), outlier.colour=NA, fill=NA) +
        scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
        labs(x = "Mes", y = "Lluvia (mm)") + 
        labs(title = paste("Total de lluvia mensual por década"))
      g4 <- ggplot() + 
        geom_jitter(data = sel_mes, aes(x = Month, y = tas_mean, colour = Model), width = 0.15) +
        geom_boxplot(data = sel_mes, aes(x = Month, y = tas_mean, group = Month), outlier.colour=NA, fill=NA) +
        scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
        labs(x = "Mes", y = "Temperatura (C)") + 
        labs(title = paste("Promedio de temperatura mensual por década"))

      #grafico en blanco si no hay selección de GCMs
      if(is.null(input$modelos)){
      } else if (input$loess == F && input$extremos == F) {
        #funciones generales de gráfico 
        grid_arrange_shared_legend(g1,g4,g2,g3)
        }else if (input$loess == F && input$extremos == T) {
          g1 <- g1 +
            geom_rect(data = extr, 
                      aes(xmin = xmin, xmax = xmax, ymin = tas_min, ymax = tas_max), 
                      fill = "black", alpha = 0.15)
          g2 <- g2 +
            geom_rect(data = extr, 
                      aes(xmin = xmin, xmax = xmax, ymin = pr_min, ymax = pr_max), 
                      fill = "black", alpha = 0.15)
          g3 <- g3 +
            geom_linerange(data=sel_extr_mes, aes(x=Month, ymin=prmin, ymax=prmax), size = 10, alpha = 0.2)
          g4 <- g4 +
            geom_linerange(data=sel_extr_mes, aes(x=Month, ymin=tasmin, ymax=tasmax), size = 10, alpha = 0.15)
          grid_arrange_shared_legend(g1,g4,g2,g3)
        } else if (input$loess == T && input$extremos == F) {
          #gráfico con línea de tendencia
          g1 <- g1 + stat_smooth(data=seleccion, aes(x = Year, y = tas_mean), method="loess", level=0.5, se=F)
          g2 <- g2 + stat_smooth(data=seleccion, aes(x = Year, y = pr_year), method="loess", level=0.5, se=F)
          g3 <- g3 + geom_smooth(data=sel_mes, aes(x = Month, y = pr_mean), method="loess", level=0.5, se=F)
          g4 <- g4 + geom_smooth(data=sel_mes, aes(x = Month, y = tas_mean), method="loess", level=0.5, se=F)
          grid_arrange_shared_legend(g1,g4,g2,g3)
        } else if (input$loess == T && input$extremos == T) {
          g1 <- g1 + stat_smooth(data=seleccion, aes(x = Year, y = tas_mean), method="loess", level=0.5, se=F) +
            geom_rect(data = extr, 
                      aes(xmin = xmin, xmax = xmax, ymin = tas_min, ymax = tas_max), 
                      fill = "black", alpha = 0.15)
          g2 <- g2 + stat_smooth(data=seleccion, aes(x = Year, y = pr_year), method="loess", level=0.5, se=F) +
            geom_rect(data = extr, 
                      aes(xmin = xmin, xmax = xmax, ymin = pr_min, ymax = pr_max), 
                      fill = "black", alpha = 0.15)
          g3 <- g3 + geom_smooth(data=sel_mes, aes(x = Month, y = pr_mean), method="loess", level=0.5, se=F) +
            geom_linerange(data=sel_extr_mes, aes(x=Month, ymin=prmin, ymax=prmax), size = 10, alpha = 0.2)
          g4 <- g4 + geom_smooth(data=sel_mes, aes(x = Month, y = tas_mean), method="loess", level=0.5, se=F) +
            geom_linerange(data=sel_extr_mes, aes(x=Month, ymin=tasmin, ymax=tasmax), size = 10, alpha = 0.15)
          grid_arrange_shared_legend(g1,g4,g2,g3)
        }
    }
    },width = "auto", height = 800
  )
}

shinyApp(ui = ui, server = server)