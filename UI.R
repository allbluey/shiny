sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Sobre", tabName = "about", icon = icon("mortar-board")),
              menuItem("Tabelas", tabName="tab", icon=icon("database"),
                       menuSubItem("Índices", tabName = "tabind", icon = icon("database")),
                       menuSubItem("HIV no mundo", tabName = "tabhiv", icon = icon("database"))),
              menuItem("Gráficos", tabName = "plot", icon=icon("line-chart"),
                       menuSubItem("Índices por país", tabName = "plotind", icon = icon("line-chart")),
                       menuSubItem("Doenças no Brasil", tabName = "doe_brasil", icon = icon("line-chart")),
                       menuSubItem("Doenças por Continente", tabName = "doe_cont", icon = icon("line-chart")),
                       menuSubItem("Índices por gênero", tabName = "ind_gen", icon = icon("line-chart"))),
              menuItem("Mapas",  icon = icon("map"),
                       menuSubItem("HIV x America do Sul", tabName = "mapaAShiv", icon = icon("map-marker")),
                       menuSubItem("Mapa Mundo", tabName = "mapaMundo", icon = icon("map-marker"))),
              menuItem("Código", tabName = "cod", icon = icon("code"),
                       menuSubItem("UI.R", tabName = "ui", icon = icon("code")),
                       menuSubItem("SERVER.R", tabName = "se", icon = icon("code")))
              
  ),
  hr()
)

body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "about",
            fluidRow(column(12,
                            includeMarkdown("about2.Rmd")) #, style="font-family: 'arial'; font-si20pt"))
            )
    ),
    
    
    tabItem(tabName = "tabind",
            fluidRow(titlePanel("Tabelas com todos os Índices"), column(2)),
            fluidRow(column(8, offset = 2,
                            DT::dataTableOutput('tabind'))),
            fluidRow(column(2, offset = 10)),
            fluidRow(column(12,
                            includeMarkdown("indices.Rmd"))
            )
    ),
    tabItem(tabName = "tabhiv",
            fluidRow(titlePanel("Tabelas dos valores do HIV em 2016"),
                     column(12,
                            dataTableOutput('tabhiv')
                     )
            )
    ),
    
    
    tabItem(tabName = "plotind", titlePanel("Análise dos índices por país ao longo dos anos"),
            sidebarPanel(width = 12,
                         column(4,
                                selectInput(inputId = "continente", label = strong("Escolha o continente"),
                                            choices = c(unique(locais$`Four World Regions`), "Oceania"),
                                            selected = "America"),
                                tableOutput('tind')
                         ),
                         column(2),
                         column(6, 
                                conditionalPanel(
                                  condition = "input.continente == 'Africa'",
                                  selectInput(inputId = "paisAf", label = strong("Escolha o pais"),
                                              choices = local_af$Location,
                                              selected = "Egypt")
                                ),
                                conditionalPanel(
                                  condition = "input.continente == 'America'",
                                  selectInput(inputId = "paisAm", label = strong("Escolha o pais"),
                                              choices = local_am$Location,
                                              selected = "Brazil")
                                ),
                                conditionalPanel(
                                  condition = "input.continente == 'Asia'",
                                  selectInput(inputId = "paisAs", label = strong("Escolha o pais"),
                                              choices = local_as$Location,
                                              selected = "India")
                                ),
                                conditionalPanel(
                                  condition = "input.continente == 'Europe'",
                                  selectInput(inputId = "paisEu", label = strong("Escolha o pais"),
                                              choices = local_eu$Location,
                                              selected = "Italy")
                                ),
                                conditionalPanel(
                                  condition = "input.continente == 'Oceania'",
                                  selectInput(inputId = "paisOc", label = strong("Escolha o pais"),
                                              choices = local_oc,
                                              selected = "Australia")
                                ),
                                dygraphOutput(outputId = "plotind", width= "100%", height = "400px")
                         )
            )
    ),
    
    tabItem(titlePanel("Principais classes de doenças no Brasil"), tabName = "doe_brasil",
            d3heatmapOutput('doe_brasil')
    ),
    
    tabItem(titlePanel("Principais classes de doenças por continente"), tabName = "doe_cont",
            selectInput(inputId = "cont", label = strong("Escolha o continente"),
                        choices = c("Africa", "America", "Asia", "Europa", "Oceania"),
                        selected = "America"),
            d3heatmapOutput('doe_cont')
    ),
    
    tabItem(titlePanel("Comparação dos IDH por gênero"), tabName = "ind_gen",
            plotlyOutput('ind_gen')
    ),
    
    
    #Mapas
    tabItem(titlePanel("IDH x HIV em 2015 na América do Sul"), tabName = "mapaAShiv",
            highchartOutput("mapaAShiv", width = "80%")
    ),
    tabItem(titlePanel("Doença cardiovasculares no mundo em 2015"), tabName = "mapaMundo",
            highchartOutput("mapaM", width = "80%")
    ),
    
    
    tabItem(tabName = "ui", titlePanel("Código ui.R"),
            pre(includeText("idh_ui.R"))
    ),
    tabItem(tabName = "se", titlePanel("Código server.R"),
            pre(includeText("idh_server.R"))
    )
    
  )
)

dashboardPage(
  dashboardHeader(title = "O IDH e as Doenças no mundo"),
  sidebar,
  body
)

})