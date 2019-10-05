ui <- ({sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Notas em geral", tabName = "notas", icon = icon("database")),
    menuItem("Média das disciplinas", icon = icon("database"), tabName = "media")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "notas",
            fluidRow(titlePanel("Tabelas com todos as notas"), column(2)),
            fluidRow(column(8, offset = 2,
                            DT::dataTableOutput('notas'))),
            fluidRow(column(2, offset = 10))
    ),
    
    tabItem(tabName = "media",
            fluidRow(titlePanel("Tabelas das médias"),
                     column(12,
                            dataTableOutput('media'))
            )
    )
  ))

# Put them together into a dashboardPage
dashboardPage(skin = "green",
              dashboardHeader(title = "Dados FCM"),
              sidebar,
              body
)
})