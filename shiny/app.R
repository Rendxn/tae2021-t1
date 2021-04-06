#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# libraries
library(shiny)
library(bslib)
library(caret)

# front end - navbar page
ui <-  navbarPage("TAE 2021 - 1",
                  # select flatly bootswatch theme
                  theme = bs_theme(bootswatch = "flatly"),
                  tabPanel("Modelo", sidebarLayout(
                      # sidebar con el formulario
                      sidebarPanel(
                          h4('Información sobre el cabeza de hogar'),
                          # sexo
                          radioButtons("sex", "Sexo",
                                       c("Masculino" = 1,
                                         "Femenino" = 2),
                                       inline = TRUE,
                                       ),
                          # edad
                          numericInput("age", "Edad", value = NULL, min = 0),
                          # estado civil
                          selectInput("civil", "Estado Civil",
                                      c("No está casado(a) y vive en pareja hace menos de dos años" = 1,
                                        "No está casado(a) y vive en pareja hace dos años o más" = 2,
                                        "Está viudo(a)" = 3,
                                        "Está separado(a) o divorciado(a)" = 4,
                                        "Está soltero(a)" = 5,
                                        "Está casado(a)" = 6
                                        )
                                      ),
                          # pueblo o etnia
                          selectInput("etnia", "Pueblo o Etnia",
                                      c("Indígena" = 1,
                                        "Gitano(a) (Rom)" = 2,
                                        "Raizal del archipiélago de San Andrés, Providencia y Santa Catalina" = 3,
                                        "Palenquero(a) de San Basilio" = 4,
                                        "Negro(a), mulato(a) (afrodescendiente), afrocolombiano(a)" = 5,
                                        "Ninguno de los anteriores" = 6
                                      )),
                          # nivel educativo
                          selectInput("edu", "Nivel educativo",
                                      c("Ninguno" = 1,
                                        "Preescolar" = 2,
                                        "Básica Primaria (1° - 5°)" = 3,
                                        "Básica Secundaria (6° - 9°)" = 4,
                                        "Media (10° - 13°)" = 5,
                                        "Técnico sin título" = 6,
                                        "Técnico sin título" = 7,
                                        "Tecnológico sin título" = 8,
                                        "Tecnológico con título" = 9,
                                        "Universitario sin título" = 10,
                                        "Universitario con título" = 11,
                                        "Postgrado sin título" = 12,
                                        "Postgrado con título" = 13
                                      )),
                          # región
                          selectInput("region", "Región",
                                      c("Caribe" = 1,
                                        "Oriental" = 2,
                                        "Central" = 3,
                                        "Pacífica (sin valle)" = 4,
                                        "Bogotá" = 5,
                                        "Antioquia" = 6,
                                        "Valle del Cauca" = 7,
                                        "San Andrés" = 8,
                                        "Orinoquía - Amazonía" = 9
                                      )),
                          # estrato
                          selectInput("estrato", "Estrato",
                                      c("Bajo - Bajo" = 1,
                                        "Bajo" = 2,
                                        "Medio - Bajo" = 3,
                                        "Medio" = 4,
                                        "Medio - Alto" = 5,
                                        "Alto" = 6,
                                        "Planta Eléctrica" = 8,
                                        "No conoce el estrato o no cuenta con recibo de pago" = 9,
                                        "Recibos sin estrato o el servicio es pirata" = 0
                                      )),
                          # ingreso mensual total del hogar
                          numericInput("ingreso", "Ingreso mensual total del hogar", value = NULL, min = 0),
                          # cantidad de personas en el hogar
                          numericInput("cant_personas", "Cantidad de personas en el hogar", value = NULL, min = 0),
                          # botón para enviar formulario
                          submitButton(text = "Enviar")
                          ),
                      mainPanel(
                          h2('Predicción de la cantidad de hijos en los hogares colombianos'),
                          hr(),
                          # predicción
                          uiOutput('prediction')
                      )
                  )
                  ),
                  tabPanel('Informe', div(
                      a('Enlace del informe en RPubs', href = 'https://rpubs.com/sarendongi/tae2021-t1', target='_blank')
                  )),
                  tabPanel('Repositorio', div(
                      a('Enlace del repositorio en GitHub', href = 'https://github.com/Rendxn/tae2021-t1', target='_blank')
                  )),
                  tabPanel('Video', div(
                      a('Enlace del video', target='_blank')
                  ))
)

server <- function(input, output) {
    modelo <- readRDS('data/knnModel.rds')
    output$prediction <- renderUI({
        # se validan las entradas al modelo
        validate(
            need(input$sex, 'Seleccione su sexo.'),
            need(input$age > 0, 'Ingrese una edad válida.'),
            need(input$civil, 'Seleccione su estado civil.'),
            need(input$etnia, 'Seleccione su población o etnia.'),
            need(input$edu, 'Seleccion su nivel de educación.'),
            need(input$region, 'Seleccione su región.'),
            need(input$estrato, 'Seleccione su estrato.'),
            need(input$ingreso >= 0, 'Digite un ingreso mensual total válido.'),
            need(input$cant_personas > 0, 'Ingrese una cantidad de personas en el hogar válido')
        )
        # se genera un dataframe con las entradas
        entrada <- as.data.frame(cbind(input$sex, input$age, input$civil,
                                       input$etnia, input$edu, 
                                       input$region, input$estrato,
                                       input$ingreso, input$cant_personas
        ))
        colnames(entrada) <- c('P6020', 'P6040', 'P5502', 'P6080', 'P8587', 'REGION', 'P8520S1A1', 'I_HOGAR', 'CANT_PERSONAS_HOGAR')
        entrada$P6040 <- as.integer(entrada$P6040)
        entrada$I_HOGAR <- as.numeric(entrada$I_HOGAR)
        entrada$CANT_PERSONAS_HOGAR <- as.integer(entrada$CANT_PERSONAS_HOGAR)
        # predicción
        result <- predict(modelo, entrada)
        
        # renderizar resultado
        div(p('La cantidad de hijos en su hogar es:'),
            h3(round(result)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
