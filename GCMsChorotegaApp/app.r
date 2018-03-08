library(shiny)
library(ggplot2)
library(gridExtra)
library(grid)
library(rgdal)
library(leaflet)
library(data.table)

anual_GCMs <- fread("anual_CIGEFI_ID.csv")
anual_GCMs_Ch <- data.table(readRDS("anual_CIGEFI_TodoChorotega.rds"))
gridcells <- readOGR(dsn = ".", layer = "Celdas_ubicaciones")
percentiles <- data.table(readRDS("percentiles_CIGEFI.rds"))
percentilesChorotega <- readRDS("percentiles_CIGEFI_TodoChorotega.rds")
mensual_GCMs <- fread("mensual_CIGEFI.csv")
mensual_GCMsChorotega <- data.table(readRDS("mensual_CIGEFI_TodoChorotega.rds"))
percentiles_mes <- readRDS("percentiles_CIGEFI_mensual.rds")
percentiles_mesChorotega <- readRDS("percentiles_CIGEFI_mensual_TodoChorotega.rds")

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
      
      sliderInput("aNo", "Seleccionar periodo de años:",
                  min = 2000, max = 2100, value = c(2030, 2060), step = 10, sep = ""),
      checkboxInput("loess", "Mostrar línea de tendencia.", value = F),
      h5("Nota:"),
      p("- Las líneas punteadas representan el rango entre los percentiles 5 y 95 de los datos históricos modelados (1979 a 1999)."),
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
      p("Herramienta desarrollada por Guillermo Durán (HIDROCEC) en R/Shiny."),
      p(""),
      p("Última actualización 7-3-2018.")
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
    #funciones de selección
    if(is.null(input$myMap_shape_click)) {
      seleccion <- anual_GCMs_Ch[Scenario == input$cp & 
                                   Year >= input$aNo[1] & 
                                   Year <= input$aNo[2]]
      
      sel_mes <- mensual_GCMsChorotega[Scenario == input$cp&
                                ini_year >= input$aNo[1] & 
                                ini_year <= input$aNo[2]]
     
      if (input$loess == T) {
        #grafico con línea de tenencia 
        g1 <- ggplot(seleccion, aes(x = Year, y = tas_m)) + geom_jitter(aes(colour = Model), width = 0.25) +
          stat_smooth(data=seleccion, method="loess", level=0.5, se=F) +
          geom_hline(yintercept = percentilesChorotega$tas_95pctl, linetype="dashed") +
          geom_hline(yintercept = percentilesChorotega$tas_5pctl, linetype="dashed") +
          labs(x = "Años", y = "Temperatura (C)") + 
          scale_colour_discrete(name="Modelos") +
          labs(
            title = paste("Promedio anual de temperatura mensual")
          )
        g2 <- ggplot(seleccion, aes(x = Year, y = pr_y)) + geom_jitter(aes(colour = Model), width = 0.25) +
          stat_smooth(data=seleccion, method="loess", level=0.5, se=F) +
          geom_hline(yintercept = percentilesChorotega$pr_95pctl, linetype="dashed") +
          geom_hline(yintercept = percentilesChorotega$pr_5pct, linetype="dashed") +
          labs(x = "Años", y = "Lluvia (mm)") + 
          labs(
            title = paste("Total de lluvia anual")
          )
        g3 <- ggplot() + geom_jitter(data = sel_mes, aes(x = Month, y = pr_mean, colour = Model)) +
          geom_smooth(data=sel_mes, aes(x = Month, y = pr_mean), method="loess", level=0.5, se=F) +
          geom_linerange(data=percentiles_mesChorotega, aes(x=Month, ymin=pr_5pct, ymax=pr_95pctl), linetype="dashed") +
          scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
          labs(x = "Mes", y = "Lluvia (mm)") + 
          labs(title = paste("Total de lluvia mensual")
               )
        g4 <- ggplot() + geom_jitter(data = sel_mes, aes(x = Month, y = tas_mean, colour = Model)) +
          geom_smooth(data=sel_mes, aes(x = Month, y = tas_mean), method="loess", level=0.5, se=F) +
          geom_linerange(data=percentiles_mesChorotega, aes(x=Month, ymin=tas_5pctl, ymax=tas_95pctl), linetype="dashed") +
          scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
          labs(x = "Mes", y = "Temperatura (C)") + 
          labs(title = paste("Promedio de temperatura mensual")
          )
        grid_arrange_shared_legend(g1,g4,g2,g3)
      } else {
        #gráfico sin línea de tendencia
        g1 <- ggplot(seleccion, aes(x = Year, y = tas_m)) + geom_jitter(aes(colour = Model), width = 0.25) +
          labs(x = "Años", y = "Temperatura (C)") + 
          geom_hline(yintercept = percentilesChorotega$tas_95pctl, linetype="dashed") +
          geom_hline(yintercept = percentilesChorotega$tas_5pctl, linetype="dashed") +
          scale_colour_discrete(name="Modelos") +
          labs(
            title = paste("Promedio anual de temperatura mensual")
          )
        g2 <- ggplot(seleccion, aes(x = Year, y = pr_y)) + geom_jitter(aes(colour = Model), width = 0.25) +
          labs(x = "Años", y = "Lluvia (mm)") + 
          geom_hline(yintercept = percentilesChorotega$pr_95pctl, linetype="dashed") +
          geom_hline(yintercept = percentilesChorotega$pr_5pct, linetype="dashed") +
          labs(
            title = paste("Total de lluvia anual")
          )
        g3 <- ggplot() + geom_jitter(data = sel_mes, aes(x = Month, y = pr_mean, colour = Model)) +
          geom_linerange(data=percentiles_mesChorotega, aes(x=Month, ymin=pr_5pct, ymax=pr_95pctl), linetype="dashed") +
          scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
          labs(x = "Mes", y = "Lluvia (mm)") + 
          labs(
            title = paste("Total de lluvia mensual")
          )
        g4 <- ggplot() + geom_jitter(data = sel_mes, aes(x = Month, y = tas_mean, colour = Model)) +
          geom_linerange(data=percentiles_mesChorotega, aes(x=Month, ymin=tas_5pctl, ymax=tas_95pctl), linetype="dashed") +
          scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
          labs(x = "Mes", y = "Temperatura (C)") + 
          labs(
            title = paste("Promedio de temperatura mensual")
          )
        grid_arrange_shared_legend(g1,g4,g2,g3)
      }
      
    } else {
      seleccion <- anual_GCMs[(input$myMap_shape_click[1]) == id & 
                                Scenario == input$cp & 
                                Year >= input$aNo[1] & Year <= input$aNo[2]] 
      sel_percentil <- percentiles[(input$myMap_shape_click[1]) == id]
      sel_mes <- mensual_GCMs[(input$myMap_shape_click[1]) == id & 
                                Scenario == input$cp & 
                                ini_year >= input$aNo[1] & ini_year <= input$aNo[2]]
      sel_percentiles_mes <- percentiles_mes %>% dplyr::filter((input$myMap_shape_click[1]) == id)
      
      if (input$loess == T) {
        #funciones generales de gráfico 
        g1 <- ggplot(seleccion, aes(x = Year, y = tas_mean)) + geom_jitter(aes(colour = Model), width = 0.25) + 
          stat_smooth(data=seleccion, method="loess", level=0.5, se=F) +
          geom_hline(yintercept = sel_percentil$tas_95pctl, linetype="dashed") +
          geom_hline(yintercept = sel_percentil$tas_5pctl, linetype="dashed") +
          scale_colour_discrete(name="Modelos") +
          labs(x = "Años", y = "Temperatura (C)") + 
          labs(
            title = paste("Promedio anual de temperatura mensual")
            )
        g2 <- ggplot(seleccion, aes(x = Year, y = pr_year)) + geom_jitter(aes(colour = Model), width = 0.25) +
          stat_smooth(data=seleccion, method="loess", level=0.5, se=F) +
          geom_hline(yintercept = sel_percentil$pr_95pctl, linetype="dashed") +
          geom_hline(yintercept = sel_percentil$pr_5pct, linetype="dashed") +
          labs(x = "Años", y = "Lluvia (mm)") + 
          labs(
            title = paste("Total de lluvia anual")
            )
        g3 <- ggplot() + geom_jitter(data = sel_mes, aes(x = Month, y = pr_mean, colour = Model)) +
          geom_smooth(data=sel_mes, aes(x = Month, y = pr_mean), method="loess", level=0.5, se=F) +
          geom_linerange(data=sel_percentiles_mes, aes(x=Month, ymin=pr_5pct, ymax=pr_95pctl), linetype="dashed") +
          scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
          labs(x = "Mes", y = "Lluvia (mm)") + 
          labs(
            title = paste("Total de lluvia mensual")
          )
        g4 <- ggplot() + geom_jitter(data = sel_mes, aes(x = Month, y = tas_mean, colour = Model)) +
          geom_smooth(data=sel_mes, aes(x = Month, y = tas_mean), method="loess", level=0.5, se=F) +
          geom_linerange(data=sel_percentiles_mes, aes(x=Month, ymin=tas_5pctl, ymax=tas_95pctl), linetype="dashed") +
          scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
          labs(x = "Mes", y = "Temperatura (C)") + 
          labs(
            title = paste("Promedio de temperatura mensual")
          )
        grid_arrange_shared_legend(g1,g4,g2,g3)
        } else {
          #gráfico sin línea de tendencia
          g1 <- ggplot(seleccion, aes(x = Year, y = tas_mean)) + geom_jitter(aes(colour = Model), width = 0.25) +
            labs(x = "Años", y = "Temperatura (C)") + 
            geom_hline(yintercept = sel_percentil$tas_95pctl, linetype="dashed") +
            geom_hline(yintercept = sel_percentil$tas_5pctl, linetype="dashed") +
            scale_colour_discrete(name="Modelos") +
            labs(
              title = paste("Promedio anual de temperatura mensual")
              )
          g2 <- ggplot(seleccion, aes(x = Year, y = pr_year)) + geom_jitter(aes(colour = Model), width = 0.25) +
            labs(x = "Años", y = "Lluvia (mm)") + 
            geom_hline(yintercept = sel_percentil$pr_95pctl, linetype="dashed") +
            geom_hline(yintercept = sel_percentil$pr_5pct, linetype="dashed") +
            labs(
              title = paste("Total de lluvia anual")
              )
          g3 <- ggplot() + geom_jitter(data = sel_mes, aes(x = Month, y = pr_mean, colour = Model)) +
            geom_linerange(data=sel_percentiles_mes, aes(x=Month, ymin=pr_5pct, ymax=pr_95pctl), linetype="dashed") +
            scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
            labs(x = "Mes", y = "Lluvia (mm)") + 
            labs(
              title = paste("Total de lluvia mensual")
            )
          g4 <- ggplot() + geom_jitter(data = sel_mes, aes(x = Month, y = tas_mean, colour = Model)) +
            geom_linerange(data=sel_percentiles_mes, aes(x=Month, ymin=tas_5pctl, ymax=tas_95pctl), linetype="dashed") +
            scale_x_continuous(breaks=seq(1,12,1), labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Set","Oct","Nov","Dic")) +
            labs(x = "Mes", y = "Temperatura (C)") + 
            labs(
              title = paste("Promedio de temperatura mensual")
            )
          grid_arrange_shared_legend(g1,g4,g2,g3)
        }
    }
    },width = "auto", height = 800
  )
  }

shinyApp(ui = ui, server = server)