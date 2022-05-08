packages = c("shiny", "dplyr", "ggplot2", "leaflet", "maps", "miceadds", "DT", "shinydashboard","readr")
# use this function to check if each package is on the local machine
# if a package is installed, it will be loaded
# if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
        install.packages(x, dependencies = TRUE,repos='http://cran.rediris.es')
        library(x, character.only = TRUE)
    }
})

# aumento el tamaño de permitido
options(shiny.maxRequestSize = 40*1024^2) # permitimos archivos de hasta 40 mb

ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "SeMap Proyect"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Cargar datos", tabName = "carga", icon = icon("refresh")),
            menuItem("Graficos", icon = icon("stats", lib = "glyphicon"), tabName = "graficos"),
            menuItem("Mapa Museos ", icon = icon("home", lib = "glyphicon"), tabName = "mapa_museos"),
            menuItem("Mapa obras de arte ", icon = icon("dashboard"), tabName = "mapa_obras")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "carga",
                    h2(strong("Aqui podras cargar tus datos...")),
                    
                    fileInput("DatosFichero", 
                              "Selecciona un fichero de datos", 
                              accept = ".csv"),
                    
                    fluidRow(
                        # A static valueBox
                        valueBoxOutput("numeroregistros"),
                        
                        # Dynamic valueBoxes
                        valueBoxOutput("registrosok"),
                        
                        valueBoxOutput("registrosnook")),
                    
                    DT::dataTableOutput("tabladatos")
            ),
            
            tabItem(tabName = "graficos",
                    h2(strong("Graficos")),
                    
                    plotOutput("plot_histo_tecn"),
                    
                    selectInput("variable_plot",
                                "Selecciona una variable para representar",
                                c("Material"="material", 
                                  "Técnica"="tecnica"),
                                selected="material")
            ),
            
            tabItem(tabName = "mapa_museos",
                    h2(strong("Mapa")),
                    
                    leafletOutput("MapMuseos"),
                    uiOutput("filtronombreobjeto")
            ),
            
            tabItem(tabName = "mapa_obras",
                    h2(strong("Mapa Procendecia de las obras")),
                    
                    leafletOutput("MapObras"),
                    br(),
                    
                    fluidRow(
                        column(4,
                            uiOutput("selectmaterial")),
                        column(4,
                            uiOutput("selecttecnica")),
                        column(4,
                            sliderInput(
                                "sliderperiodo",
                                "Key Value",
                                min = -5290,
                                max =  as.integer(format(Sys.Date(), "%Y")),
                                value = c(-5290,2030),
                                step=1
                            )
                        )
                        
                    )
                    
            )
        )
    )
)

server <- function(input, output, session) {
    
    datos <- reactive({
        inFile <- input$DatosFichero
        if (is.null(inFile))
            return(NULL)
        df <- read.csv2(inFile$datapath, header = TRUE,sep = ";")
        
        return(df)
    })
    
   
    
    output$selectmaterial <- renderUI({
        df <- datos()
        if (!is.null(df)){
            selectInput("selectmaterial",
                        "Selecciona material",
                        choices = c("Todos", datos() %>% select(material) %>% unique()))
        }
    })
    
    select.tecnica.result <- reactive({
        df <- datos()
        
        if (!is.null(df)){
            if (input$selectmaterial == "Todos"){
                tecnicas <- df %>% select(tecnica) %>% distinct()
                return(tecnicas)
            }else{
                tecnicas <- df %>% filter(material==input$selectmaterial) %>%  select(tecnica) %>% distinct()
                return(tecnicas)
            }
        }
    })
    
    output$selecttecnica <- renderUI({
        if (!is.null(df)){
            selectInput("selecttecnica",
                        "Selecciona tecnica",
                        choices =  c("Todos", select.tecnica.result()))
        }
    })
    
    
    output$filtronombreobjeto <- renderUI({
        df <- datos()
        if (!is.null(df)){
            selectInput("filtronombreobjeto",
                        "Selecciona un nombre de objeto",
                        choices =  c("Todos", datos() %>% select(nombre_loc) %>% unique()))
        }
    })
    
    
    output$numeroregistros <- renderValueBox({
        if (is.null(datos())){
            return(valueBox(
                "0", "registros totales.", icon = icon("list"),
                color = "purple"
            ))}
        df <- datos()
        
        valueBox(
            toString(nrow(df)), "registros totales.", icon = icon("list"),
            color = "purple"
        )
    })
    
    output$registrosok <- renderValueBox({
        if (is.null(datos())){
            return(valueBox(
                "0 %", "registros incompletos", icon = icon("thumbs-down", lib = "glyphicon"),
                color = "red"
            ))}
        df <- datos()
        incompletos <- sum(!complete.cases(df))/nrow(df)*100
        
        valueBox(
            paste0(incompletos, "%"), "registros incompletos", icon = icon("thumbs-down", lib = "glyphicon"),
            color = "red"
        )
    })
    
    output$registrosnook <- renderValueBox({
        if (is.null(datos())){
            return(valueBox(
                "0 %", "registros completos", icon = icon("thumbs-up", lib = "glyphicon"),
                color = "green"
            ))}
        
        df <- datos()
        completos <- sum(complete.cases(df))/nrow(df)*100
        
        valueBox(
            paste0(completos, "%"), "registros completos", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "green"
        )
    })
    
    output$tabladatos<- DT::renderDataTable({
        df <- datos()
        DT::datatable(df)
    })
    
    output$plot_histo_tecn <- renderPlot({
        df <- datos()
        
        if (!is.null(df)){
            
            if (input$variable_plot=="tecnica"){
                df_show <- distinct(df, tecnica_id, id_loc, tecnica)
                
                ggplot(df_show, aes(x=tecnica)) + 
                    geom_bar(col='black', fill='green', alpha=0.4) +
                    theme(axis.text = element_text(angle = 45))
                
            }else if (input$variable_plot=="material"){
                df_show <- distinct(df, material_id, id_loc, material)
                
                ggplot(df_show, aes(x=material)) + 
                    geom_bar(col='black', fill='red', alpha=0.4) +
                    theme(axis.text = element_text(angle = 45))
            }
            
            
            
        }
    })
    
    output$MapMuseos <- renderLeaflet({
        df <- datos()
        
        
        if (!is.null(df)){
            museos <- df
            
            # filtro el nombre de objeto
            if (input$filtronombreobjeto != "Todos"){
                museos <- museos %>% filter(nombre_loc==input$filtronombreobjeto)
            }
            
            museos <- museos %>% 
                distinct(museo_id, .keep_all = TRUE)
            
            
            
            obras <- df %>% 
                distinct(id_loc, .keep_all = TRUE)
            
            museos$num_obras <- 0
            for (i in 1:nrow(museos)){
                item <- museos[i, ]
                
                if (input$filtronombreobjeto != "Todos"){
                    item$num_obras <- nrow(obras[obras$nombre_museo==item$nombre_museo & obras$nombre_loc==input$filtronombreobjeto, ] )
                }else{
                    item$num_obras <- nrow(obras[obras$nombre_museo==item$nombre_museo, ] )
                }
                
                
                museos[i, ] <- item
            }
            
            leaflet() %>%
                
                addTiles(options = providerTileOptions(minZoom = 4)) %>%
                addMarkers(data = museos,
                           lng=~longitud_museo, lat=~latitud_museo, label = sprintf(
                                                                           "<strong>%s</strong><br/>Total obras de arte: %s",
                                                                           museos$nombre_museo, museos$num_obras) %>%
                                                                           lapply(htmltools::HTML),
                           clusterOptions = markerClusterOptions())
            
            
            
        }
    })
    
    
    
    output$MapObras <- renderLeaflet({
        df <- datos()
        
        
        if (!is.null(df) && !is.null(input$selectmaterial) && !is.null(input$selecttecnica)){
            obras <- df 
                
                if (input$selectmaterial!="Todos"){
                    obras <- obras %>% filter(material==input$selectmaterial)
                }
            
                if (input$selecttecnica!="Todos"){
                    obras <- obras %>% filter(tecnica==input$selecttecnica)
                }
                
                obras <- obras %>% filter(desde_loc >= input$sliderperiodo[1] & hasta_loc <= input$sliderperiodo[2]) %>% distinct(longitud_loc, latitud_loc, nombre_loc, .keep_all = TRUE)
            
            leaflet() %>%
                
                addTiles(options = providerTileOptions(minZoom = 1)) %>%
                addMarkers(data = obras,
                           lng=~longitud_loc, lat=~latitud_loc, label = sprintf("<strong>%s</strong><br/>Museo actual: %s",
                                                                           obras$nombre_loc, obras$nombre_museo) %>%
                                                                           lapply(htmltools::HTML),
                           clusterOptions = markerClusterOptions())
            
            
            
        }
    })
    
}

shinyApp(ui, server)