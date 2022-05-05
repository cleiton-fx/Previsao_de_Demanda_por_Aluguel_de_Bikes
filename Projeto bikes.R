#Projeto

# Coleta e Transformação de Dados

# Este código contém comandos para filtrar e transformar os dados de aluguel de bikes, 
# dados que estão no dataset.

# Este código foi criado para executar tanto no Azure, quanto no RStudio.
# Para executar no Azure, altere o valor da variavel Azure para TRUE. 
# Se o valor for FALSE, o código sera executado no RStudio



# Configurando o diretório de trabalho

# Coloque entre aspas o diretório de trabalho que você está usando no seu computador


#setwd("C:/FCD/BigDataRAzure/Cap14/Projeto")
#getwd()


# Variável que controla a execução do script
Azure <- FALSE

# Execução de acordo com o valor da variável Azure
if(Azure){
  source("src/Tools.R")
  bikes <- maml.mapInputPort(1)
  bikes$dteday <- set.asPOSIXct(bikes)
}else{
  source("src/Tools.R")
  bikes <- read.csv("bikes.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE )
  
  # Selecionar as variáveis que serão usadas
  cols <- c("dteday", "mnth", "hr", "holiday",
            "workingday", "weathersit", "temp",
            "hum", "windspeed", "cnt")
  
  # Criando um subset dos dados
  bikes <- bikes[, cols]
  
  # Transformar o objeto de data
  bikes$dteday <- char.toPOSIXct(bikes)
  
  # Esta linha acima gera dois valores NA
  
  # Esta linha abaixo corrige
  bikes <- na.omit(bikes)
  
  # Normalizar as variaveis preditoras numericas 
  cols <- c("temp", "hum", "windspeed") 
  bikes[, cols] <- scale(bikes[, cols])  
}

#?scale
str(bikes)
View(bikes)

# Criar uma nova variável para indicar dia da semana (workday)
bikes$isWorking <- ifelse(bikes$workingday & !bikes$holiday, 1, 0)  

# Adicionar uma coluna com a quantidade de meses, o que vai ajudar a criar o modelo
bikes <- month.count(bikes)

# Criar um fator ordenado para o dia da semana, comecando por segunda-feira
# Neste fator eh convertido para ordenado numérico para ser compativel com os tipos de dados do Azure ML
bikes$dayWeek <- as.factor(weekdays(bikes$dteday))


############ ATENÇÃO ############

# ==> Analise o dataframe bikes. 

# Se os nomes dos dias da semana estiverem em português na coluna bikes$dayWeek, 
# execute o Bloco1 abaixo, caso contrátio, execute o Bloco2 com os nomes em inglês. 
# Execute um bloco ou o outro.
str(bikes$dayWeek)

# Bloco1
# Se o seu sistema operacional estiver em portugês, execute o comando abaixo.
bikes$dayWeek <- as.numeric(ordered(bikes$dayWeek, 
                                    levels = c("segunda-feira", 
                                               "terça-feira", 
                                               "quarta-feira", 
                                               "quinta-feira", 
                                               "sexta-feira", 
                                               "sábado", 
                                               "domingo")))

# Bloco2
# Se o seu sistema operacional estiver em inglês, execute o comando abaixo.
bikes$dayWeek <- as.numeric(ordered(bikes$dayWeek, 
                                    levels = c("Monday", 
                                               "Tuesday", 
                                               "Wednesday", 
                                               "Thursday", 
                                               "Friday", 
                                               "Saturday", 
                                               "Sunday")))

# Agora os dias da semana devem estar como valores numéricos
# Se estiverem como valores NA, volte e verifique se você seguiu as instruções acima.
str(bikes$dayWeek)
str(bikes)

# Adiciona uma variável com valores únicos para o horário do dia em dias de semana e dias de fim de semana
# Com isso diferenciamos as horas dos dias de semana, das horas em dias de fim de semana
bikes$workTime <- ifelse(bikes$isWorking, bikes$hr, bikes$hr + 24) 

# Transforma os valores de hora na madrugada, quando a demanda por bibicletas é praticamente nula 
bikes$xformHr <- ifelse(bikes$hr > 4, bikes$hr - 5, bikes$hr + 19)

# Adiciona uma variável com valores únicos para o horário do dia para dias de semana e dias de fim de semana
# Considerando horas da madrugada
bikes$xformWorkHr <- ifelse(bikes$isWorking, bikes$xformHr, bikes$xformHr + 24) 

# str(bikes)
# View(bikes)

# O trabalho que fizemos até aqui também é chamado de Feature Engineering ou 
# Engenharia de Atributos

# Gera saída no Azure ML
if(Azure) maml.mapOutputPort('bikes')



# Análise de Correlação 

# Este código contém comandos para análise de correlação.

# Este código foi criado para executar tanto no Azure, quanto no RStudio.
# Para executar no Azure, altere o valor da variavel Azure para TRUE. 
# Se o valor for FALSE, o codigo será executado no RStudio


# Variável que controla a execução do script
Azure <- FALSE

if(Azure){
  source("src/Tools.R")
  bikes <- maml.mapInputPort(1)
  bikes$dteday <- set.asPOSIXct(bikes)
}else{
  bikes <- bikes
}

View(bikes)

# Definindo as colunas para a análise de correlação 
cols <- c("mnth", "hr", "holiday", "workingday",
          "weathersit", "temp", "hum", "windspeed",
          "isWorking", "monthCount", "dayWeek", 
          "workTime", "xformHr", "cnt")

# Métodos de Correlação

# Pearson - coeficiente usado para medir o grau de relacionamento entre duas variáveis com relação linear
# Spearman - teste não paramétrico, para medir o grau de relacionamento entre duas variaveis
# Kendall - teste não paramétrico, para medir a força de dependência entre duas variaveis

# Vetor com os métodos de correlação
metodos <- c("pearson", "spearman")

# Aplicando os métodos de correlação com a função cor()
cors <- lapply(metodos, function(method) 
  (cor(bikes[, cols], method = method)))

#Caso queira visualizar essas correlações só chamar o parâmetro head
head(cors)

# Preprando o plot
require(lattice)
plot.cors <- function(x, labs){
  diag(x) <- 0.0 
  plot( levelplot(x, 
                  main = paste("Plot de Correlação usando Método", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

# Mapa de Correlação
Map(plot.cors, cors, metodos)



# Análise de Série Temporal 

# Este código contém comandos para análise de série temporal


# Variável que controla a execução do script
Azure <- FALSE

if(Azure){
  source("src/Tools.R")
  Bikes <- maml.mapInputPort(1)
  Bikes$dteday <- set.asPOSIXct(Bikes)
}else{
  bikes <- bikes
}

# Avaliando a demanda por aluguel de bikes ao longo do tempo
# Construindo um time series plot para alguns determinados horários 
# em dias úteis e dias de fim de semana.
times <- c(7, 9, 12, 15, 18, 20, 22) 

#Definindo a função
# Time Series Plot
tms.plot <- function(times){
  ggplot(bikes[bikes$workTime == times, ], aes(x = dteday, y = cnt)) + 
    geom_line() +
    ylab("Numero de Bikes") +
    labs(title = paste("Demanda de Bikes as ", as.character(times), ":00", sep = "")) +
    theme(text = element_text(size = 20))
}

#Carrega o pacote e em seguida executa aplicando ao objeto times que é um vetor de horas que foi criado acima
require(ggplot2)
lapply(times, tms.plot)




# Analisando BoxPlots



# Variável que controla a execução do script
Azure <- FALSE

if(Azure){
  source("src/Tools.R")
  Bikes <- maml.mapInputPort(1)
  Bikes$dteday <- set.asPOSIXct(Bikes)
}else{
  bikes <- bikes
}

# Convertendo a variável dayWeek para fator ordenado e plotando em ordem de tempo
bikes$dayWeek <- fact.conv(bikes$dayWeek)

# Demanda de bikes x potenciais variáveis preditoras
#Serão criados 4 boxblot, para não ter que  ficar configurandoo título para cada um deles
#Estamos criando um título de labels e depois aplico isto na função que cria os boxplot, ou seja,
#automatizando o trabalho
labels <- list("Boxplots - Demanda de Bikes por Hora",
               "Boxplots - Demanda de Bikes por Estação",
               "Boxplots - Demanda de Bikes por Dia Útil",
               "Boxplots - Demanda de Bikes por Dia da Semana")

#Lista com variáveis que irei usar
xAxis <- list("hr", "weathersit", "isWorking", "dayWeek")

# Função para criar os boxplots
plot.boxes  <- function(X, label){ 
  ggplot(bikes, aes_string(x = X, y = "cnt", group = X)) + 
    geom_boxplot( ) + 
    ggtitle(label) +
    theme(text = element_text(size = 18)) 
}

Map(plot.boxes, xAxis, labels)


# Analisando Density Plots


# Variável que controla a execução do script
Azure <- FALSE

if(Azure){
  source("src/Tools.R")
  Bikes <- maml.mapInputPort(1)
  Bikes$dteday <- set.asPOSIXct(Bikes)
}else{
  bikes <- bikes
}

#Obs: Esta parte de análise é um pouco mais pesada,pde ser que demora um pouco 
#a execução dependendo do seu computador,pode serque leve um pouco mais de tempo
#pois este é um script um pouco mais pesado.


# Visualizando o relacionamento entre as variáveis preditoras e demanda por bike
labels <- c("Demanda de Bikes vs Temperatura",
            "Demanda de Bikes vs Humidade",
            "Demanda de Bikes vs Velocidade do Vento",
            "Demanda de Bikes vs Hora")

#Variáveis que serão utilizadas
xAxis <- c("temp", "hum", "windspeed", "hr")

# Função para os Density Plots
#Criamos uma funçao  com geom point e o ggplot par gerar o density plot
#O geom Smooth coloca uma especie de linha que mostra a evolução da relação
# e ainda uma margem de erro


plot.scatter <- function(X, label){ 
  ggplot(bikes, aes_string(x = X, y = "cnt")) + 
    geom_point(aes_string(colour = "cnt"), alpha = 0.1) + 
    scale_colour_gradient(low = "green", high = "blue") + 
    geom_smooth(method = "loess") + 
    ggtitle(label) +
    theme(text = element_text(size = 20)) 
}

Map(plot.scatter, xAxis, labels)


# Explorando a interação entre tempo e dia, em dias da semana e fins de semana
labels <- list("Box plots - Demanda por Bikes as 09:00 para \n dias da semana e fins de semana",
               "Box plots - Demanda por Bikes as 18:00  para \n dias da semana e fins de semana")

Times <- list(9, 18)

plot.box2 <- function(time, label){ 
  ggplot(bikes[bikes$hr == time, ], aes(x = isWorking, y = cnt, group = isWorking)) + 
    geom_boxplot( ) + ggtitle(label) +
    theme(text = element_text(size = 18)) }

Map(plot.box2, Times, labels)

# Gera saída no Azure ML
if(Azure) maml.mapOutputPort('bikes')


# Feature Selection

# Este código contém comandos para feature selection

#(colar parte explicando sobre feature selection)

# Variável que controla a execução do script
Azure <- FALSE

if(Azure){
  source("src/Tools.R")
  bikes <- maml.mapInputPort(1)
  bikes$dteday <- set.asPOSIXct(bikes)
}else{
  bikes <- bikes
}

#Verificando a dimensão do dataset
#Observando que há 16 atributos e 17377 observações
dim(bikes)

#Verificando se há algum valor NA
any(is.na(bikes))

# Criando um modelo para identificar os atributos com maior importância para o modelo preditivo
require(randomForest)

# Avalidando a importância de todas as variaveis
#Random Forest irá criar o modelo e extrair as variaveis mais importantes
#True porque neste caso quero que ele tambem identifique as variáveis mais importantes.
modelo <- randomForest(cnt ~ . , 
                       data = bikes, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = TRUE)

# Removendo variáveis colineares
modelo <- randomForest(cnt ~ . - mnth
                       - hr
                       - workingday
                       - isWorking
                       - dayWeek
                       - xformHr
                       - workTime
                       - holiday
                       - windspeed
                       - monthCount
                       - weathersit, 
                       data = bikes, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = TRUE)

# Plotando as variáveis por grau de importância
#Plot de variáveis importantes
varImpPlot(modelo)

# Gravando o resultado
df_saida <- bikes[, c("cnt", rownames(modelo$importance))]


if(Azure) maml.mapOutputPort("df_saida ")



# Criando um modelo preditivo usando randomForest



# Função para tratar as datas
set.asPOSIXct <- function(inFrame) { 
  dteday <- as.POSIXct(
    as.integer(inFrame$dteday), 
    origin = "1970-01-01")
  
  as.POSIXct(strptime(
    paste(as.character(dteday), 
          " ", 
          as.character(inFrame$hr),
          ":00:00", 
          sep = ""), 
    "%Y-%m-%d %H:%M:%S"))
}

char.toPOSIXct <-   function(inFrame) {
  as.POSIXct(strptime(
    paste(inFrame$dteday, " ", 
          as.character(inFrame$hr),
          ":00:00", 
          sep = ""), 
    "%Y-%m-%d %H:%M:%S")) }


# Variável que controla a execução do script
Azure <- FALSE

if(Azure){
  dataset$dteday <- set.asPOSIXct(dataset)
}else{
  bikes <- bikes
}


#Criação do modelo com RandomForest
require(randomForest)
model <- randomForest(cnt ~ xformWorkHr + dteday + temp + hum, 
                      data = bikes, # altere o nome do objeto data para "dataset" de estiver trabalhando no Azure ML
                      ntree = 40, 
                      nodesize = 5)
print(model)



# Score do modelo preditivo com randomForest



# Função para tratar as datas
set.asPOSIXct <- function(inFrame) { 
  dteday <- as.POSIXct(
    as.integer(inFrame$dteday), 
    origin = "1970-01-01")
  
  as.POSIXct(strptime(
    paste(as.character(dteday), 
          " ", 
          as.character(inFrame$hr),
          ":00:00", 
          sep = ""), 
    "%Y-%m-%d %H:%M:%S"))
}

char.toPOSIXct <-   function(inFrame) {
  as.POSIXct(strptime(
    paste(inFrame$dteday, " ", 
          as.character(inFrame$hr),
          ":00:00", 
          sep = ""), 
    "%Y-%m-%d %H:%M:%S")) }


# Variável que controla a execução do script
Azure <- FALSE


if(Azure){
  bikes <- dataset
  bikes$dteday <- set.asPOSIXct(bikes)
}else{
  bikes <- bikes
}

require(randomForest)
scores <- data.frame(actual = bikes$cnt,
                     prediction = predict(model, newdata = bikes))

# Avaliação do modelo



# Variável que controla a execução do script
Azure <- FALSE

if(Azure){
  source("src/Tools.R")
  inFrame <- maml.mapInputPort(1)
  refFrame <- maml.mapInputPort(2)
  refFrame$dteday <- set.asPOSIXct2(refFrame)
}else{
  source("src/Tools.R")
  inFrame <- scores[, c("actual", "prediction")]
  refFrame <- bikes
}

# Criando um dataframe
inFrame[, c("dteday", "monthCount", "hr", "xformWorkHr")] <- refFrame[, c("dteday", "monthCount", "hr", "xformWorkHr")]

# Nomeando o dataframe
names(inFrame) <- c("cnt", "predicted", "dteday", "monthCount", "hr", "xformWorkHr")

#  Time series plot mostrando a diferença entre valores reais e valores previstos
library(ggplot2)
inFrame <- inFrame[order(inFrame$dteday),]
s <- c(7, 9, 12, 15, 18, 20, 22)

lapply(s, function(s){
  ggplot() +
    geom_line(data = inFrame[inFrame$hr == s, ], 
              aes(x = dteday, y = cnt)) +
    geom_line(data = inFrame[inFrame$hr == s, ], 
              aes(x = dteday, y = predicted), color = "red") +
    ylab("Numero de Bikes") +
    labs(title = paste("Demanda de Bikes as ",
                       as.character(s), ":00", spe ="")) +
    theme(text = element_text(size = 20))
})

# Computando os resíduos (valor previsto - valor observado)
library(dplyr)
inFrame <-  mutate(inFrame, resids = predicted - cnt)

# Plotando os resíduos
ggplot(inFrame, aes(x = resids)) + 
  geom_histogram(binwidth = 1, fill = "white", color = "black")

#Gráficos que irá nos ajudar a compreender os resíduos
qqnorm(inFrame$resids)
qqline(inFrame$resids)

# Plotando os resíduos com as horas transformadas
inFrame <- mutate(inFrame, fact.hr = as.factor(hr),
                  fact.xformWorkHr = as.factor(xformWorkHr))                                  
facts <- c("fact.hr", "fact.xformWorkHr") 
lapply(facts, function(x){ 
  ggplot(inFrame, aes_string(x = x, y = "resids")) + 
    geom_boxplot( ) + 
    ggtitle("Residuos - Demanda de Bikes por Hora - Atual vs Previsto")})


# Mediana dos resíduos por hora
evalFrame <- inFrame %>%
  group_by(hr) %>%
  summarise(medResidByHr = format(round(
    median(predicted - cnt), 2), 
    nsmall = 2)) 

# Computando a mediana dos resíduos
tempFrame <- inFrame %>%
  group_by(monthCount) %>%
  summarise(medResid = median(predicted - cnt)) 

evalFrame$monthCount <- tempFrame$monthCount
evalFrame$medResidByMcnt <- format(round(
  tempFrame$medResid, 2), 
  nsmall = 2)

print("Resumo dos residuos")
print(evalFrame)

# Output
outFrame <- data.frame(evalFrame)
if(Azure) maml.mapOutputPort('outFrame')

