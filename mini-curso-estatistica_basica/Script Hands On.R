#######################################################
# PARTE 1: M??TRICAS DE ESTAT??STICA DESCRITIVA
#######################################################

# M??TRICAS DE CENTRALIDADE DE DADOS
# M??dia de 3 n??meros = (4+6+2)/3 = 4
a <- c(4,6,2)
mean(a)
# Mediana = ?? o elemento central de um conjunto ordenado
# Ou m??dia dos 2 elementos centrais
# => 4
median(a)

# Como m??dia e mediana divergem?
b <- c(a,500,1000)
mean(b)
median(b)
# Conclus??o: Mediana ?? um dos elementos e n??o ?? afetada por valores extremos
# Boa pr??tica: sempre calcular ambas m??tricas e 
#              procurar avaliar o comparativo entre elas

# M??TRICAS DE VARIA????O
# Desvio padr??o: ?? a varia????o m??dia dos pontos em rela????o a m??dia (medida central)
c<-c(2,2,2,2,2,2,2,2)
plot(c)
sd(c)
# N??o ha varia????o! Vamos incluir uma perturba????o
c<-c(2,2,2,3,2,2,2,2)
plot(c)
sd(c)
# Pequena varia????o! Vamos incluir mais perturba????es
c<-c(2,3,2,3,2,3,2,3)
plot(c)
sd(c)
# Muita varia????o!

# Agora vamos tentar um conjunto de dados maior
d <- as.data.frame(1:100)
names(d)<-'x'
d$mean_x <- mean(d$x)
d$median_x<-median(d$x)
# Incluindo a perturba????o
d[10,]$x <- 20

plot(d$x, type="l")
lines(d$mean_x)
lines(d$median_x, col=4)


# Os dados variam muito ou pouco?
sd(d$x)
# Intervalo Superior +sd Inferior -sd
lines(d$mean_x+sd(d$x), col=6)
lines(d$mean_x-sd(d$x), col=6)

# ANALISANDO A VARIA????O SOBRE M??DIA = CUIDADO!!!
# ANALISANDO A VARIA????O EM RELA????O A COMPORTAMENTO ESPERADO
# => DETEC????O DE ANOMALIAS
d$y <- 1:100

plot(d$x~d$y, type="l")
mod<-lm(y~x, data=d)
abline(mod, col=6)
plot(mod$residuals)
sd(mod$residuals)
# Conclus??o: desvio padr??o ?? calculo em rela????o a m??dia
# Portanto: avaliar corretamente se a m??dia ?? o baseline correto

#######################################################
# PARTE 2: AMOSTRAGEM DE DADOS
#######################################################
d<-read.csv("https://raw.githubusercontent.com/diogenesjusto/dotz/master/mini-curso-estatistica_basica/diamonds.csv")

# Popula????o: d
# Amostra: vamos tentar gerar uma amostra de tamanho 3K
mean(d$price)
median(d$price)
sd(d$price)

# Amostra a1
a1 <- d[1:3000,]
mean(a1$price)
median(a1$price)
sd(a1$price)

# Amostra a2
a2 <- d[3001:6000,]
mean(a2$price)
median(a2$price)
sd(a2$price)

# Estat??stica Descritiva - uso de gr??ficos
hist(d$price)
hist(a1$price)
hist(a2$price)

# Gera????o de uma amostra aleat??ria
set.seed(33)
va<-sample(53940)
a3<-d[va[1:3000],]
mean(a3$price)
median(a3$price)
sd(a3$price)
hist(a3$price)