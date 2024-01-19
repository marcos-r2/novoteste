#Esta nova versão estou trazendo para BGitHub

#-------------------------------------------------------------------------------

# Fonte de dados é a Companhia Nacional de Abastecimento [CONAB]
# https://www.conab.gov.br/info-agro/safras/serie-historica-das-safras/itemlist/category/891-cana-de-acucar-agricola
# A produtividade é resultado dos volume Produzido (Kg) pelo Area plantada (ha) [Kg/ha]
# Na planilha tem o total por estado, neste estudo estamos usando sumário por região

#-------------------------------------------------------------------------------
# Instalação e Carregamento de Todos os Pacotes 

pacotes <- c("readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth","tidyverse", "tsibble", "fable","tsibbledata", "fpp3",
             "urca")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

#-------------------------------------------------------------------------------
# Importando dados do Excel e limpando dados


#prodcana <- read_excel("/Users/User/Documents/Marcos/CanaSerieHistZ.xlsx")
#prodcana <- read_excel("C:\Users\User\Documents\Marcos\CanaSerieHistZ.xlsx",sheet ="Produtividade" ))


prodcana <- read_excel("CanaSerieHistZ.xlsx",sheet ="Produtividade" )
titulo = paste(prodcana[1,1], colnames(prodcana)[1],  prodcana[2,1]   )
prodcana <- prodcana[-c(1,2,3,40,41,42),]
prodcana <- t(prodcana)
colnames(prodcana) <-  as.character(unlist(prodcana[1,]))
colnames(prodcana)[1] = "Data" 
prodcana <- prodcana[-1,]
colnames(prodcana)[20] = "CENTROOESTE" 

#prodcana[,1] <- paste(substr(prodcana[,1], start = 1, stop = 4), '01-01 00:00:00',sep = "-")
#prodcana[,1] = as.Date(prodcana[,1])

prodcana[,1] <- paste(substr(prodcana[,1], start = 1, stop = 4), '01-01',sep = "-")


View(prodcana)
#-------------------------------------------------------------------------------
# Plotando dados
# Aqui consegui plotar dados da Brasil e territorios,
# Porém ainda ficou algumas coisas: 
# resolvido - Não consegui colocar a data no eixo X
# resolvido - Não consegui plotar os 6 gráficos no mesmo gráfico
# Não consegui usar ggplot
#
# Observa-se que produtividade baixa de outras regiões afetam o produtividade
# Brasileira a ponto de ficar mais baixa que em alguns estados
# Será bom lista 


Brasil       <- ts(prodcana[,"BRASIL"], start=2005, end=2023,  frequency = 1)
plot(Brasil,main=titulo,xlab="Safra (ano)", ylab="Kg/ha")

CentroOeste  <- ts(prodcana[,"CENTROOESTE"], start=2005, end=2023, frequency = 1)
#CentroOeste  <- ts(prodcana[,"CENTRO"], start=2005, end=2023, , frequency = 1)
#Centro  <- ts(prodcana[,"CENTRO"], start=2005, end=2023, , frequency = 1)
Nordeste     <- ts(prodcana[,"NORDESTE"], start=2005, end=2023,  frequency = 1)
Norte        <- ts(prodcana[,"NORTE"], start=2005, end=2023,  frequency = 1)
Sudeste      <- ts(prodcana[,"SUDESTE"], start=2005, end=2023,  frequency = 1)
Sul          <- ts(prodcana[,"SUL"], start=2005, end=2023, frequency = 1)

# colocando os dados em forma de uma matriz com todos os dados no conjunto dados1

dados1     <- ts(matrix(1,19,6))
dados1[,1] <- Brasil
dados1[,2] <- CentroOeste
dados1[,3] <- Nordeste
dados1[,4] <- Norte
dados1[,5] <- Sudeste
dados1[,6] <- Sul

colnames(dados1)[1] <- 'Brasil'
colnames(dados1)[2] <- 'CentroOeste'
colnames(dados1)[3] <- 'Nordeste'
colnames(dados1)[4] <- 'Norte'
colnames(dados1)[5] <- 'Sudeste'
colnames(dados1)[6] <- 'Sul'

#plot(dados1, main="Dados de Produtividade da produção de Cana de Açucar",
#     xlab="Safra (de 2005 à 2023) ",axis.Date(1, at=seq(min(2005), max(2023))))
plot(dados1, main="Dados de Produtividade da produção de Cana de Açucar",
     xlab="Safra (de 2005 à 2023) ", ylab="Kg/ha")  

# Construindo uma janela com 4 gráficos

par(mfrow=c(3,2))

plot(Brasil, main="Brasil",
     xlab="Safra (ano)", ylab="Kg/ha")    
#options(scipen = 999)

plot(CentroOeste, main="CentroOeste",
     xlab="Safra (ano)", ylab="Kg/ha")    

#plot(Centro, main="CentroOeste",
xlab="Safra (ano)", ylab="Kg/ha")    

plot(Nordeste, main="Nordeste",
     xlab="Safra (ano)", ylab="Kg/ha")

plot(Norte, main="Norte",
     xlab="Safra (ano)", ylab="Kg/ha")

plot(Sudeste, main="Sudeste",
     xlab="Safra (ano)", ylab="Kg/ha")
#options(scipen = 999)

plot(Sul, main="Sul",
     xlab="Safra (ano)", ylab="Kg/ha")

#-------------------------------------------------------------------------------
# Testes com Plotagem de gráficos
# Consegui faxzer ggplot, a fonte de dados precisa ser data.frame...
# porem tem muitos parametros para aprender
df = as.data.frame(prodcana)

ggplotly(
  ggplot(df,  aes(x = Data, y = NORTE)) +
    geom_line()
)

df %>% 
  ggplot(aes(x = Data, y = NORTE)) 


#Autoplot é simples, usa timeseries...
autoplot(Sul)

# Estes 2 deveria ter x num,erico
acf(Sul)
pacf(Sul, lag.max = 5)

ggplot(data = df) +
  geom_line( aes(x = Data, y = NORTE))


#Outra forma simples de plotagem de TS
ggtsdisplay(Norte)

# Teste de Estacionariedade
testevarejo=ur.df(Sul)
summary(testevarejo)

#-------------------------------------------------------------------------------
# Agora entendo que preciso Decompor os dados, do Brasil
#

# Decomposição pelo modelo ADITIVO

s=ts(c(Brasil), start=c(2005),
     end=c(2023), frequency = 1)
plot(s)

decompa=decompose(s,type = "additive")
plot(decompa)

decompa$trend
decompa$seasonal
decompa$random

# Decomposição pelo modelo MULTIPLICATIVO

decompm=decompose(s,type = "multiplicative")
plot(decompm)

decompm$trend
decompm$seasonal
decompm$random





