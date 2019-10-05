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
fy <- arrange(fcmof, `Ano Ingresso`) #ordem por ano

#média de cada ano para cada disciplina
media1 <- aggregate(Notas ~`Ano Ingresso` + Disciplinas + Período, data = fy, FUN = mean)
colnames(media1) <- c("Ano Ingresso", "Disciplinas", "Período", "Média")
media1$Média <- round(media1$Média, digits = 2)

#PROBLEMA : criar coluna média junto com as outras colunas tal que cada aluno da disciplina seja 
#comparado com a média da mesma

#neste gráfico é possível selecionar o ano de ingresso e o periodo para verificar a média das  disciplinas


m101 <- filter(media1, `Ano Ingresso` == 2010 & Período == "primeiro ano")
m102 <- filter(media1,`Ano Ingresso` == 2010 & Período == "segundo ano")
m103 <- filter(media1, `Ano Ingresso` == 2010 & Período == "terceiro ano")
m104 <- filter(media1, `Ano Ingresso` == 2010 & Período == "quarto ano")
m105 <- filter(media1, `Ano Ingresso` == 2010 & Período == "quinto ano")
m106 <- filter(media1, `Ano Ingresso` == 2010 & Período == "sexto ano")

#COMO ARRUMAR O LOESS E COMO ARRUMAR O TEMA 
library(gridExtra)
m101 <- ggplot(m101, aes(y = Média,x = Disciplinas, group = 1, color = Disciplinas)) + geom_line() + geom_point(size = 2) +
  geom_smooth(method = loess) + scale_y_continuous(breaks = c(7,7.5,8,8.5,9,9.5,10)) + labs(x = "Disciplinas", y = "Média") + 
  theme(legend.position = "none") 

m102 <- ggplot(m102, aes(y = Média,x = Disciplinas, group = 1, color = Disciplinas)) + geom_line() + geom_point(size = 2) +
  geom_smooth(method = loess) + scale_y_continuous(breaks = c(7,7.5,8,8.5,9,9.5,10)) + labs(x = "Disciplinas", y = "Média") + 
  theme(legend.position = "none") 

m103 <- ggplot(m103, aes(y = Média,x = Disciplinas, group = 1, color = Disciplinas)) + geom_line() + geom_point(size = 2) +
  geom_smooth(method = loess) + scale_y_continuous(breaks = c(7,7.5,8,8.5,9,9.5,10)) + labs(x = "Disciplinas", y = "Média") + 
  theme(legend.position = "none") 

m104 <- ggplot(m104, aes(y = Média,x = Disciplinas, group = 1, color = Disciplinas)) + geom_line() + geom_point(size = 2) +
  geom_smooth(method = loess) + scale_y_continuous(breaks = c(7,7.5,8,8.5,9,9.5,10)) + labs(x = "Disciplinas", y = "Média") + 
  theme(legend.position = "none") 

#m105 <- ggplot(m105, aes(y = Média,x = Disciplinas, group = 1, color = Disciplinas)) + geom_line() + geom_point(size = 2) +
  #geom_smooth(method = loess)+  labs(x = "Disciplinas", y = "Média") + 
  #theme(legend.position = "none") 

m106 <- ggplot(m106, aes(y = Média,x = Disciplinas, group = 1, color = Disciplinas)) + geom_line() + geom_point(size = 2) +
  geom_smooth(method = loess) + scale_y_continuous(breaks = c(7,7.5,8,8.5,9,9.5,10)) + labs(x = "Disciplinas", y = "Média") + 
  theme(legend.position = "none") 

grid.arrange(m101, m102, m103, m106, ncol = 2)


#gráfico boxplot para selecionar o ano de ingresso e verificar os periodos, neste estamos vendo as notas dos
#alunos em comparação com o CR (que é fixo). Com o boxplot se tem uma noção da média, mediana, quartis, mínimo, máximo e outliers 

test <- fy %>% filter(`Ano Ingresso` == 2010)

test$Período <- factor(test$Período, 
                     levels = c("primeiro ano", "segundo ano", "terceiro ano", "quarto ano", "quinto ano", "sexto ano"))

ggplot(test, aes(x = CR, y = Notas, fill = Período)) + geom_boxplot() + facet_grid(. ~ Período) + 
  theme(legend.position = "none") + ggtitle("Boxplot: Notas dos alunos 2010")

#tipo de gráfico interativo

#selecionar disciplina
md141 <- filter(fy, Disciplinas == "MD141" & `Ano Ingresso` == 2008)

#selecionar RA e ver desenvolvimento do aluno
RA <- filter(fy, RA == 104243)
RA <- arrange(RA, Período)


library(gridExtra)
#GRÁFICOS DE DENSIDADE POR ENQUANTO 
#por periodos e sem mostrar as disciplinas (vale a pena?)

p1 <- filter(RA, Período == "primeiro ano")
p2 <- filter(RA, Período == "segundo ano")
p3 <- filter(RA, Período == "terceiro ano")
p4 <- filter(RA, Período == "quarto ano")
p5 <- filter(RA, Período == "quinto ano")
p6 <- filter(RA, Período == "sexto ano")

library(gridExtra)
ex1 <- ggplot(p1, aes(x = Notas)) + stat_density() + theme_bw() + ggtitle("Primeiro ano")
ex2 <- ggplot(p2, aes(x = Notas)) + stat_density() + theme_bw() + ggtitle("Segundo ano")
ex3 <- ggplot(p3, aes(x = Notas)) + stat_density() + theme_bw() + ggtitle("Terceiro ano")
ex4 <- ggplot(p4, aes(x = Notas)) + stat_density() + theme_bw() + ggtitle("Quarto ano")
ex5 <- ggplot(p5, aes(x = Notas)) + stat_density() + theme_bw() + ggtitle("Quinto ano")
ex6 <- ggplot(p6, aes(x= Notas)) + stat_density() + theme_bw() + ggtitle("Sexto ano")

grid.arrange(ex1, ex2, ex3, ex4, ex5, ex6, ncol = 3)

#que tipo de gráfico conseguimos ver o desenvolvimento dos alunos nas disciplinas por período

colourCount = length(unique(RA$Período))
getPalette = colorRampPalette(brewer.pal(6, "Reds"))

#frequência de quantas vezes o aluno tirou aquela nota em determinado período
#ORGANIZAR LEGENDA

RA$Período <- factor(RA$Período, 
                       levels = c("primeiro ano", "segundo ano", "terceiro ano", "quarto ano", "quinto ano", "sexto ano"))

P <- ggplot(RA, aes(x = Notas, fill = Período)) + geom_histogram(binwidth = 0.5) +
scale_fill_manual(values = getPalette(colourCount)) + labs(x = "Notas", y = "Frequência") + theme_bw()

ggplotly(P) #interativo

#P <- ggplot(p6, aes(x = Notas, fill = Disciplinas)) + geom_histogram(binwidth = 1) + scale_fill_manual(values = getPalette(colourCount)) + labs(x = "Notas", y = "Frequência") + theme_bw()

#tentar usar test 

p1e <- filter(test, Período == "primeiro ano")
p2e <- filter(test, Período == "segundo ano")
p3e <- filter(test, Período == "terceiro ano")
p4e <- filter(test, Período == "quarto ano")
p5e <- filter(test, Período == "quinto ano")
p6e <- filter(test, Período == "sexto ano")

library(gridExtra)
ex1 <- ggplot(p1e, aes(x = Notas)) + stat_density() + theme_bw() + ggtitle("Primeiro ano")
ex2 <- ggplot(p2e, aes(x = Notas)) + stat_density() + theme_bw() + ggtitle("Segundo ano")
ex3 <- ggplot(p3e, aes(x = Notas)) + stat_density() + theme_bw() + ggtitle("Terceiro ano")
ex4 <- ggplot(p4e, aes(x = Notas)) + stat_density() + theme_bw() + ggtitle("Quarto ano")
ex5 <- ggplot(p5e, aes(x = Notas)) + stat_density() + theme_bw() + ggtitle("Quinto ano")
ex6 <- ggplot(p6e, aes(x= Notas)) + stat_density() + theme_bw() + ggtitle("Sexto ano")

grid.arrange(ex1, ex2, ex3, ex4, ex5, ex6, ncol = 3)



