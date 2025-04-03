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
  # datos = data.frame(Unidad = replicate(n, sample(opciones, 1)),
  #                               Nombre = replicate(n, paste0(sample(nombres, 1), " ", sample(apellidos, 1))),
  #                               Cargo = replicate(n, sample(cargo, 1)))
  library(RPostgreSQL)
  dsn_database = "<database name>"       # for example  "compose"
  dsn_hostname = "<your host name>"     # for example  "aws-us-east-1-portal.4.dblayer.com"
  dsn_port = "<port>"                 # for example  11101 
  dsn_uid = "<your user id>"        # for example  "admin"
  dsn_pwd = "<your password>"      # for example  "xxx"
  
  tryCatch({
    drv <- dbDriver("PostgreSQL")
    print("Connecting to database")
    conn <- dbConnect(drv, 
                      dbname = dsn_database,
                      host = dsn_hostname, 
                      port = dsn_port,
                      user = dsn_uid, 
                      password = dsn_pwd)
    print("Connected!")
  },
  error=function(cond) {
    print("Unable to connect to database.")
  })
  
  datos <- dbGetQuery(conn, "SELECT datname from pg_database")
  
  output$choose_course <- renderUI({
    course.names <- as.vector( unique(datos$Unidad) )
    selectInput("facultad", "Seleccionar:", choices=course.names, multiple=TRUE)    
  })
  
  
  model.data <- reactive({
    subset(datos, Unidad %in% input$facultad)
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