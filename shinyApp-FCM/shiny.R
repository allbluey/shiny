source("data_processing.R")
disciplinas <- sort(unique(fy$Disciplinas))
years <- sort(unique(fy$`Ano Ingresso`))

library(readxl)
library(readr)
library(data.table)
library(dplyr) 
library(tidyr)
library(DT)
library(knitr)  
library(shiny)
library(shinydashboard)
library(shinythemes)

#-------------------------------SHINY--------------------------------------------------------------

ui <- navbarPage(theme = shinytheme("sandstone"),
                 title = "Análise de dados - FCM",
                 tabPanel("Sobre",
                   sidebarLayout(
                     sidebarPanel(
                       h2("Introdução aos dados", align = "center"),
                       br(),
                       p("API criado para a visualização de dados. Tabelas Dinâmicas disponíveis com informações sobre todas as disciplinas oferecidas desde 2005.
                         Visualizações gráficas em construção."),
                       br(),
                       img(src = "myImage.png", height = 200, width = 210, align = "center"),
                       br(),
                       br(),
                       p("Dados referentes a ", 
                         a("Faculdade de ciências médicas",
                       href = "https://www.fcm.unicamp.br/fcm/graduacao/medicina")),
                       width = 3
                       
                     ),
                     mainPanel(
                       h1("Breve introdução ao API"),
                       p("Shiny é um novo pacote proveniente do software RStudio que cria aplicações web intertivas 
                         com muita", 
                         em("facilidade "), 
                         "na visualização dos dados."),
                       br(),
                       p("Para mais informações, visite ",
                         a("Shiny homepage.", 
                           href = "http://shiny.rstudio.com")),
                       br(),
                       h2("Objetivos"),
                       p("- Facilidade na busca de dados de planilhas."),
                       p("- Visualização dos dados de forma automática. O app Shiny tem a opção de mudar
                         instantaneamente à medida que os usuários modificam suas preferências.")
                     )
                   )
                   
                 ),
                 navbarMenu("Tabelas",
                            tabPanel("Geral", 
                                     sidebarLayout(
                                       sidebarPanel(
                                         sliderInput("year", "Selecione o ano de ingresso:", min = 2005, max = 2011,
                                                     value = c(2007, 2010)),
                                         uiOutput("disciplinasControl"),
                                         actionButton(inputId = "clearAllBottom",
                                                      label = "Clear selection",
                                                      icon = icon("square-o")),
                                         actionButton(inputId = "selectAllBottom",
                                                      label = "Select all",
                                                      icon = icon("check-square-o"))
                                       ),
                                       mainPanel(
                                           dataTableOutput(outputId="dTable")
                              )
                            )
                          ),
                            tabPanel("Médias", fluidRow(column(10, offset= 1, align = "center", DT::dataTableOutput('media'))))
                        ),
                 
                 tabPanel("Visualizações", 
                          sidebarLayout(
                            sidebarPanel(widht = 3,
                                         selectInput(inputId = 'anos', 'Escolha o ano de ingresso',
                                                     choices = fy$`Ano Ingresso`, selectize = TRUE),
                                         br()
                            ),
                            mainPanel(plotOutput('plot'))
                          )
                 )
               
)

server <- function(input, output, session){
  
  values <- reactiveValues()
  values$disciplinas <- disciplinas
  
  
  observe({
    if(input$selectAllBottom > 0) {
      updateCheckboxGroupInput(session=session, inputId="disciplinas", 
                               choices=disciplinas, selected=disciplinas)
      values$disciplinas <- disciplinas
    }
  })
  
  
  observe({
    if(input$clearAllBottom > 0) {
      updateCheckboxGroupInput(session=session, inputId="disciplinas", 
                               choices=disciplinas, selected=NULL)
      values$disciplinas <- c()
    }
  })
  
  # Create event type checkbox
  output$disciplinasControl <- renderUI({
    checkboxGroupInput('disciplinas', 'Selecione as disciplinas:', 
                       disciplinas, selected = values$disciplinas)
  })
  
  # Prepare dataset
  dataTable <- reactive({
    groupByDisciplina(fy, input$year[1], 
                      input$year[2], input$disciplinas)
  })
  
  output$dTable <- renderDataTable({
    dataTable()
  })
  
  output$media <- renderDataTable({media1})
  
  selectData <- reactive({
    if(input$anos %in% fy$`Ano Ingresso`)
      return(fy %>% 
               filter(`Ano Ingresso` == input$anos))
  })
  
  output$plot <- renderPlot({
    p <-ggplot(selectData(), aes(x = CR, y = Notas, fill = Período)) + geom_boxplot() + facet_grid(. ~ Período) + 
      theme(legend.position = "none")
    
    print(p)
  })
  
}

shinyApp(ui, server)