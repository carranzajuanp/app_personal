library(shiny)

ui <- fluidPage( 
  sidebarLayout(
    sidebarPanel(
      uiOutput('choose_course')
    ),
    mainPanel(
      tableOutput('courseTable')
    )
  ) ,
  downloadButton("downloadData", "Descargar selección")
)

server <- function(input, output, session) {
  # nombres = c("Juan", "Pedro", "José", "María", "Susana", "Clara")
  # apellidos = c("Aaaaa", "Bbbbb", "Cccccc", "Dddddd", "Eeeeee", "Ffffff", "Gggggg")
  # paste0(sample(nombres, 1), " ", sample(apellidos, 1))
  # cargo = c("Docente", "Nodocente")
  # opciones =  c("Facultad de Ciencias Médicas",
  #               "Facultad de Ciencias Exactas, Físicas y Naturales",
  #               "Escuela Superior de Comercio Manuel Belgrano",
  #               "Facultad de Derecho",
  #               "Facultad de Arquitectura, Urbanismo y Diseño",
  #               "Facultad de Ciencias Económicas",
  #               "Facultad de Ciencias Químicas",
  #               "Facultad de Filosofía Y Humanidades",
  #               "Facultad de Odontología",
  #               "Área Central",
  #               "Colegio Nacional de Monserrat",
  #               "Facultad de Ciencias Agropecuarias",
  #               "Facultad de Lenguas",
  #               "Facultad de Psicología",
  #               "Facultad de Artes",
  #               "Facultad de Ciencias de la Comunicación",
  #               "Facultad de Matemática, Astronomía, Física y Computación",
  #               "Facultad de Ciencias Sociales",
  #               "Observatorio Astronómico")
  # n = 50
  # datos = data.frame(unidad = replicate(n, sample(opciones, 1)),
  #                               nombre = replicate(n, paste0(sample(nombres, 1), " ", sample(apellidos, 1))),
  #                               cargo = replicate(n, sample(cargo, 1)))

  datos = read.csv("http://pentaho-dev-6.psi.unc.edu.ar/shiny_personal.csv", header = TRUE, sep = ",")
  
  output$choose_course <- renderUI({
    course.names <- as.vector( unique(datos$unidad) )
    selectInput("facultad", "Seleccionar:", choices=course.names, multiple=TRUE)    
  })
  
  
  model.data <- reactive({
    subset(datos, unidad %in% input$facultad)
  })
  
  output$courseTable <- renderTable({ model.data() })
  
  output$downloadData <- downloadHandler(
    filename = "personal.csv",
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(model.data(), file)
    }
  )
  
  
}
runApp(shinyApp(ui,server))