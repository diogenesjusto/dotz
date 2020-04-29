# Fontes de dados
# PIB: https://www.itau.com.br/itaubba-pt/analises-economicas/nossas-series-economicas/pib-mensal-itau-unibanco
# Tr??fego: www.abcr.org.br
# install.packages("e1071")
# Obs: voc?? pode utilizar rstudio.cloud para executar
library(e1071)
erro_hist <- 0
pib <- read.csv("https://raw.githubusercontent.com/diogenesjusto/dotz-mini-curso-ingestao-ml/master/PIB.csv", sep=";", dec=",")
# Exemplo simples de feature
# pib$MES <- substr(pib$ANO_MES,1,3)

# An??lise explorat??ria - comportamento PIB 12 anos
plot(pib$PIB, type="l")
# An??lise de correla????o entre PIB e Tr??fego Brasil
plot(pib$PIB ~ pib$BRT)

# Separa????o de bases de treino e teste
treino <- pib[1:126,]
teste  <- pib[127:136,]

# Modelo (algoritmo)
mod1 <- lm(PIB~BRT, data=treino)
#mod1 <- lm(PIB~BRL+BRP, data=treino)
#mod1 <- lm(PIB~BRL+BRP+MES, data=treino)
#mod1 <- lm(PIB~poly(BRL,2)+BRP+MES, data=treino)
#mod1 <- lm(PIB~poly(BRL,2)+poly(BRP,2)+MES, data=treino)
#mod1 <- lm(PIB~log(BRL,2)+log(BRP,2)+MES, data=treino)
prev <- predict(mod1, newdata=teste)
erro <- mean(abs(teste$PIB - prev))
# Erro absoluto m??dio
erro
# Erro percentual m??dio
erro/mean(teste$PIB)
erro_hist <- c(erro_hist,erro/mean(teste$PIB))

# Evolu????o erro
plot(erro_hist[2:6], type="l")
