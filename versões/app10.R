library(readxl)
library(readr)  # ler dados
#install.packages("data.table")
library(data.table)   # ler dados
#install.packages("dplyr")
library(dplyr) #manipulacao tabelas
#instal.packages("tidyr")
library(tidyr) #manipulacao tabelas
library(knitr)  #trabalho dinamico
library(ggplot2)
library(shiny)
library(DT)
library(shinythemes)


fcm <- read_xlsx("plan2.xlsx", col_names = TRUE)
#fcm2010 <- filter(fcm,`Ano Ingresso` == 2010)
fcm2 <- fcm[,-c(1,4)] #excluindo colunas inuteis
glimpse(fcm2)

#passando ano e ra para factor
fcm2$`Ano Ingresso` <- as.integer(fcm2$`Ano Ingresso`)
fcm2$RA <- as.factor(fcm2$RA)

#passando os dados para numérico
fcm2$MD136 <- as.numeric(fcm2$MD136)
fcm2$MD138 <- as.numeric(fcm2$MD138)
fcm2$MD447 <- as.numeric(fcm2$MD447)
fcm2$MD342 <- as.numeric(fcm2$MD342)
fcm2$MD344 <- as.numeric(fcm2$MD344)
fcm2$MD142 <- as.numeric(fcm2$MD142)

#organizando a tabela com a coluna disciplinas e notas
fcm2p2 <- gather(fcm2, "Disciplinas", "Notas", 'BS111':'MD139')

#como indexar o ano correspondente de cada disciplica

primeiroano <- fcm2p2 %>% filter(Disciplinas %in% c("BS111","BS121","BS122","BS123",	"BS124","MD141","MD142","MD148","BS221",
                                                    "BS222","BS223","BS224","MD241", "MD242","MD243","MD244","MD248"))
segundoano <- fcm2p2 %>% filter(Disciplinas %in% c("BS320","BS330","BS340","MD342","MD343","MD344","MD348","MD542","BS420",
                                                   "BS430","MD442","MD443","MD444","MD445","MD447","MD448","MD449"))
terceiroano <-  fcm2p2 %>% filter(Disciplinas %in% c("MD543","MD544","MD546","MD548","MD643","MD644","MD646","MD748"))
quartoano <- fcm2p2 %>% filter(Disciplinas %in% c("MD752","MD753","MD754","MD758","MD759"))
quintoano <- fcm2p2 %>% filter(Disciplinas %in% c("MD941","MD942","MD943","MD944","MD945"))
sextoano <- fcm2p2 %>% filter(Disciplinas %in% c("MD127","MD131","MD132","MD133","MD134","MD135","MD136","MD138","MD139"))


primeiro_ano <- primeiroano %>% mutate(Período = rep("primeiro ano", dim(primeiroano)[1]))
segundo_ano <- segundoano %>% mutate(Período = rep("segundo ano", dim(segundoano)[1]))
terceiro_ano <- terceiroano %>% mutate(Período = rep("terceiro ano", dim(terceiroano)[1]))
quarto_ano <- quartoano %>% mutate(Período = rep("quarto ano", dim(quartoano)[1]))
quinto_ano <- quintoano %>% mutate(Período = rep("quinto ano", dim(quintoano)[1]))
sexto_ano <- sextoano %>% mutate(Período = rep("sexto ano", dim(sextoano)[1]))

fcmof <- rbind(primeiro_ano, segundo_ano, terceiro_ano, quarto_ano, quinto_ano, sexto_ano)
fcmof$Período <- factor(fcmof$Período)
fcmof$Notas <- as.numeric(fcmof$Notas)
fy <- arrange(fcmof, `Ano Ingresso`) 

dis <- c("MD241", "MD242","MD243","MD244","MD248","MD342","MD343","MD344","MD348","MD542",
         "MD442","MD443","MD444","MD445","MD447","MD448","MD449", "MD543","MD544","MD546","MD548","MD643","MD644","MD646","MD748",
         "MD752","MD753","MD754","MD758","MD759","MD941","MD942","MD943","MD944","MD945","MD127","MD131","MD132","MD133","MD134","MD135","MD136","MD138","MD139")
fy <- fy %>% filter(`Ano Ingresso` %in% c(2005,2006,2007,2008,2009,2010,2011), Disciplinas %in% dis)

fy$Período <- factor(fy$Período, 
                     levels = c("primeiro ano", "segundo ano", "terceiro ano", "quarto ano", "quinto ano", "sexto ano"))


media1 <- aggregate(Notas ~`Ano Ingresso` + Disciplinas + Período, data = fy, FUN = mean)
colnames(media1) <- c("Ano Ingresso", "Disciplinas", "Período", "Média")
media1$Média <- round(media1$Média, digits = 2)

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

server <- function(input, output){
  
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