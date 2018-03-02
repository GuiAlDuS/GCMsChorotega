library(shiny)
library(ggplot2)
library(gridExtra)
library(grid)
library(rgdal)
library(leaflet)
library(data.table)

anual_GCMs <- data.table(readRDS("anual_CIGEFI_ID.rds"))
anual_GCMs_Ch <- data.table(readRDS("anual_CIGEFI_TodoChorotega.rds"))
gridcells <- readOGR(dsn = ".", layer = "Celdas_ubicaciones")

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
                  min = 2000, max = 2100, value = c(2030, 2060), step = 5, sep = ""),
      checkboxInput("loess", "Mostrar línea de tendencia.", value = F),
      br(),
      p("Herramienta elaborada como parte de la colaboración entre el Centro de Investigaciones Geofísicas de la Universidad de Costa Rica (CIGEFI) y el Centro de Recursos Hídricos para Centroamérica y el Caribe de la Universidad Nacional de Costa Rica (HIDROCEC-UNA)."),
      ("Los GCMs utilizados fueron cambiados de escala"), tags$i("(downscaling)"), ("a 5km x 5km por los investigadores del CIGEFI Hugo Hidalgo y Eric Alfaro. Todos los GCMs utilizados son parte de la fase 5 del"), tags$i("Coupled model intercomparison project"), ("(CMIP5)."),
      p(""),
      p("La línea de tendencia está calculada por medio de una regresión local (LOESS)."),
      br(),
      ("App elaborada en R-Shiny por Guillermo Durán, HIDROCEC-UNA. El código de la herramienta se puede acceder en"), tags$a(href="https://github.com/GuiAlDuS/GCMsChorotega/blob/master/GCMsChorotegaApp/app.r", "GitHub"), ("."),
      p("Última actualización 2-3-2018.")
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
      if (input$loess == T) {
        #funciones generales de gráfico 
        g1 <- ggplot(seleccion, aes(x = Year, y = tas_m)) + geom_point(aes(colour = Model)) +
          stat_smooth(data=seleccion, method="loess", level=0.8, se=F) +
          labs(x = "Años", y = "Temperatura (C)") + 
          labs(
            title = paste("Promedio anual de temperatura mensual")
          )
        g2 <- ggplot(seleccion, aes(x = Year, y = pr_y)) + geom_point(aes(colour = Model)) +
          stat_smooth(data=seleccion, method="loess", level=0.8, se=F) +
          labs(x = "Años", y = "Lluvia (mm)") + 
          labs(
            title = paste("Total de lluvia anual")
          )
        grid_arrange_shared_legend(g1,g2)
      } else {
        #gráfico con línea de tendencia
        g3 <- ggplot(seleccion, aes(x = Year, y = tas_m)) + geom_point(aes(colour = Model)) +
          labs(x = "Años", y = "Temperatura (C)") + 
          labs(
            title = paste("Promedio anual de temperatura mensual")
          )
        g4 <- ggplot(seleccion, aes(x = Year, y = pr_y)) + geom_point(aes(colour = Model)) +
          labs(x = "Años", y = "Lluvia (mm)") + 
          labs(
            title = paste("Total de lluvia anual")
          )
        grid_arrange_shared_legend(g3,g4)
      }
      
    } else {
      seleccion <- anual_GCMs[(input$myMap_shape_click[1]) == id & 
                                Scenario == input$cp & 
                                Year >= input$aNo[1] & Year <= input$aNo[2]] 
      if (input$loess == T) {
        #funciones generales de gráfico 
        g1 <- ggplot(seleccion, aes(x = Year, y = tas_mean)) + geom_point(aes(colour = Model)) +
        stat_smooth(data=seleccion, method="loess", level=0.8, se=F) +
        labs(x = "Años", y = "Temperatura (C)") + 
        labs(
          title = paste("Promedio anual de temperatura mensual")
          )
        g2 <- ggplot(seleccion, aes(x = Year, y = pr_year)) + geom_point(aes(colour = Model)) +
        stat_smooth(data=seleccion, method="loess", level=0.8, se=F) +
        labs(x = "Años", y = "Lluvia (mm)") + 
        labs(
          title = paste("Total de lluvia anual")
        )
        grid_arrange_shared_legend(g1,g2)
        } else {
          #gráfico con línea de tendencia
          g3 <- ggplot(seleccion, aes(x = Year, y = tas_mean)) + geom_point(aes(colour = Model)) +
            labs(x = "Años", y = "Temperatura (C)") + 
            labs(
              title = paste("Promedio anual de temperatura mensual")
              )
          g4 <- ggplot(seleccion, aes(x = Year, y = pr_year)) + geom_point(aes(colour = Model)) +
            labs(x = "Años", y = "Lluvia (mm)") + 
            labs(
              title = paste("Total de lluvia anual")
              )
          grid_arrange_shared_legend(g3,g4)
        }
    }
    },width = "auto", height = 800
  )
  }

shinyApp(ui = ui, server = server)