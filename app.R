library(shiny)

fluidPage(ui <- fluidPage( 
  sidebarLayout(
    sidebarPanel(
      uiOutput('elegir_facultad'),
      downloadButton("downloadData", "Descargar selección")
    ),
    mainPanel(
      tableOutput('listado')
    )
  )
)) 

server <- function(input, output, session) {
  datos <- read.csv("~/Descargas/shiny_personal.csv", header = TRUE, sep = ",")
  names(datos) <- c("Unidad", "Apellido y Nombre", "Tipo de Cargo")
  
  output$elegir_facultad <- renderUI({
    listado <- as.vector( unique(datos$Unidad) )
    selectInput("facultad", "Seleccionar:", choices = listado, multiple=TRUE, selectize = TRUE)    
  })
  
  
  tabla <- reactive({
    subset(datos, Unidad %in% input$facultad)
  })
  

  output$listado <-  function(){
    tabla() %>%
      knitr::kable("html") %>%
      kableExtra::kable_styling("striped", full_width = TRUE)
  }
  
  output$downloadData <- downloadHandler(
    filename = "personal.csv",
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(model.data(), file)
    }
  )
  
  
}
runApp(shinyApp(ui,server))