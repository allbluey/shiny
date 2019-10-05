library(readxl)

library(readr)  # ler dados
#install.packages("data.table")
library(data.table)   # ler dados
#install.packages("dplyr")
library(dplyr) #manipulacao tabelas
#instal.packages("tidyr")
library(tidyr) #manipulacao tabelas
#instal.packages("stringr")
library(DT)  #tabela dinamica
#install.packages("knitr")
library(knitr)  #trabalho dinamico
library(plotly) 
library(shinydashboard)
library(ggplot2)
library(rsconnect)

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

#média de cada ano para cada disciplina
media <- aggregate(Notas ~`Ano Ingresso` + Disciplinas + Período, data = fy, FUN = mean)
#-----------------------------------------------------------------------------------------------------



ui <- ({sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Notas em geral", tabName = "notas", icon = icon("database")),
    menuItem("Média das disciplinas", icon = icon("database"), tabName = "media"),
    menuItem("Gráficos", icon = icon("area-chart"), tabName = "graf")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "notas",
            fluidRow(column(2)),
            fluidRow(column(8, offset = 2,
                            DT::dataTableOutput('notas'))),
            fluidRow(column(2, offset = 10))
    ),
    
    tabItem(tabName = "media",
            fluidRow(titlePanel("Tabelas das médias"),
                     column(12,
                            dataTableOutput('media'))
            )
    ),
    tabItem(tabName = "graf", 
            sidebarLayout(
              sidebarPanel(widht = 3,
                           selectInput(inputId = 'anos', 'Escolha o ano de ingresso',
                                       choices = fy$`Ano Ingresso`, selectize = TRUE),
                           br(),
                           selectInput(inputId = 'disciplina', 'Escolha a disciplina',
                                       choices = fy$Disciplinas, selectize = TRUE)
              ),
              mainPanel(plotOutput('plot'))
            ))
  )
)

# Put them together into a dashboardPage
dashboardPage(skin = "green",
              dashboardHeader(title = "Dados FCM"),
              sidebar,
              body
)
})

server <- function(input, output){
  output$notas = DT::renderDataTable({fy})
  
  output$media = renderDataTable({media})
  
  selectData <- reactive({
    if(input$anos %in% fy$`Ano Ingresso` & input$disciplina %in% fy$Disciplinas)
      return(fy %>% 
               filter(`Ano Ingresso` == input$anos & Disciplinas == input$disciplina))
  })
  
  output$plot <- renderPlot({
    ggplot(selectData(), aes(x = Notas)) + geom_density()
  })
}


shinyApp(ui = ui, server = server)