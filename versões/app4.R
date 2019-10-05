ui <- navbarPage(
  tabPanel("Dados FCM",
  sidebarLayout(
    sidebarPanel(
      h2("Análise de desempenho", align = "center"),
      br(),
      p("Tabela Dinâmica disponível com todas as disciplinas oferecidas desde 2005."),
      br(),
      br(),
      br(),
      img(src = "myImage.png", height = 150, width = 200),
      br(), 
      width = 3
    ),
    tabPanel("Tabela",
             fluidRow(column(10, offset = 1,
                                      DT::dataTableOutput('notas'))),
             fluidRow(column(2, offset = 10)),
             br(),
             br(),
             verbatimTextOutput("summary"))
  )
),
  navbarMenu("Visualização",
             tabPanel("Anos"),
             tabPanel("Disciplinas")
  )
)



server <- function(input, output, session){
  
  output$notas = DT::renderDataTable({fy})
  
  output$summary <- renderPrint(
    summary({fy})
  )
}


shinyApp(ui = ui, server = server)
