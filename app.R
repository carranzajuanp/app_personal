library(shiny)
library(dplyr)      
library(DT)         
library(ggplot2)    
library(shinythemes)
library(lubridate)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  navbarPage(
    "Listado de Personal de la UNC",
    
    tabPanel(
      "Tabla de Datos",
      sidebarLayout(
        sidebarPanel(
          h4("Filtros de Búsqueda"),
          
          uiOutput('elegir_dependencia'),
          uiOutput('elegir_cargo'),
          uiOutput('elegir_categoria'),
          uiOutput('elegir_dedicacion'),
          
          downloadButton("downloadFilteredData", "Descargar Selección (CSV)")
        ),
        
        mainPanel(
          textOutput("update_date_text"),
          h3("Listado de Personal"),
          DT::dataTableOutput('listado')
        )
      )
    ),
    
    tabPanel(
      "Visualización Gráfica",
      fluidRow(
        column(
          4,
          h4("Opciones de Visualización"),
          selectInput(
            "plot_group_by",
            "Agrupar por:",
            choices = c(
              "Dependencia" = "Dependencia",
              "Cargo" = "Cargo",
              "Categoría" = "Categoria",
              "Dedicación" = "Dedicacion",
              "Tipo de Cargo" = "Tipo de Cargo"
            ),
            selected = "Dependencia"
          )
        ),
        column(
          8,
          h3(textOutput("plot_title")),
          plotOutput("dependencia_plot", height = "500px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$update_date_text <- renderText({
    formatted_date <- format(floor_date(Sys.Date(), "week"), "%d/%m/%Y")
    paste("Información actualizada al:", formatted_date)
  })
  
  datos <- read.csv("C:/Users/Usuario/Downloads/shiny_personal_completo.csv", header = TRUE, sep = ",")
  
  names(datos) <- c("Dependencia","Apellido y Nombre", "DNI", "Cargo","Categoria","Dedicacion","Tipo de Cargo")
  
  output$elegir_dependencia <- renderUI({
    listado <- sort(as.vector(unique(datos$Dependencia)))
    selectInput("dependencia", "Seleccionar Dependencia:", choices = listado, multiple = TRUE, selectize = TRUE)    
  })
  
  output$elegir_cargo <- renderUI({
    listado <- sort(as.vector(unique(datos$Cargo)))
    selectInput("cargo", "Seleccionar Cargo:", choices = listado, multiple = TRUE, selectize = TRUE)    
  })
  
  output$elegir_categoria <- renderUI({
    listado <- sort(as.vector(unique(datos$Categoria)))
    selectInput("categoria", "Seleccionar Categoría:", choices = listado, multiple = TRUE, selectize = TRUE)    
  })
  
  output$elegir_dedicacion <- renderUI({
    listado <- sort(as.vector(unique(datos$Dedicacion)))
    selectInput("dedicacion", "Seleccionar Dedicación:", choices = listado, multiple = TRUE, selectize = TRUE)    
  })
  
  filtered_tabla <- reactive({
    data_filtered <- datos
    
    if (!is.null(input$dependencia)) {
      data_filtered <- data_filtered %>% filter(Dependencia %in% input$dependencia)
    }
    if (!is.null(input$cargo)) {
      data_filtered <- data_filtered %>% filter(Cargo %in% input$cargo)
    }
    if (!is.null(input$categoria)) {
      data_filtered <- data_filtered %>% filter(Categoria %in% input$categoria)
    }
    if (!is.null(input$dedicacion)) {
      data_filtered <- data_filtered %>% filter(Dedicacion %in% input$dedicacion)
    }
    
    data_filtered %>% select(-Dependencia)
  })
  
  output$listado <- DT::renderDataTable({
    DT::datatable(
      filtered_tabla(),
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'pdf'),
        pageLength = 15,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json')
      ),
      rownames = FALSE
    )
  })
  
  output$downloadFilteredData <- downloadHandler(
    filename = function() {
      paste("personal_filtrado_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_tabla(), file, row.names = FALSE)
    }
  )
  
  plot_data <- reactive({
    data_for_plot <- datos
    
    if (!is.null(input$cargo)) {
      data_for_plot <- data_for_plot %>% filter(Cargo %in% input$cargo)
    }
    if (!is.null(input$categoria)) {
      data_for_plot <- data_for_plot %>% filter(Categoria %in% input$categoria)
    }
    if (!is.null(input$dedicacion)) {
      data_for_plot <- data_for_plot %>% filter(Dedicacion %in% input$dedicacion)
    }
    
    if (!is.null(input$dependencia)) {
      data_for_plot <- data_for_plot %>% filter(Dependencia %in% input$dependencia)
    }
    
    req(input$plot_group_by)
    
    data_for_plot %>%
      group_by(across(input$plot_group_by)) %>%
      summarise(Count = n()) %>%
      ungroup() %>%
      arrange(desc(Count))
  })
  
  output$plot_title <- renderText({
    paste("Distribución de Personal por", input$plot_group_by)
  })
  
  output$dependencia_plot <- renderPlot({
    req(nrow(plot_data()) > 0) 
    
    ggplot(plot_data(), aes(x = reorder(!!sym(input$plot_group_by), -Count), y = Count, fill = !!sym(input$plot_group_by))) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(aes(label = Count), vjust = -0.5, size = 4, color = "black") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 15)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, margin = margin(r = 10)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      ) +
      labs(
        y = "Número de Personas"
      ) +
      scale_fill_viridis_d()
  })
  
}

shinyApp(ui, server)
