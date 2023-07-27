#install.packages("lme4") # Para ajustar modelos mistos
#install.packages("lmerTest") # Para testes de hipóteses com modelos mistos
#install.packages("ggplot2") # Para gráficos

library(lme4)
library(lmerTest)
library(ggplot2)
library(readxl)
library(dplyr)

dados <- read_excel("C:/Users/ferna/Downloads/Tomate.xlsx")
View(dados)

#Ordenando a base de dados por Tratamento, Bloco, Estação e Colheita
dados <- dados[order(dados$Tratamento, dados$Bloco, dados$`Estação cultivo`, dados$colheita), ]

# Calculando o peso acumulado para cada combinação de Tratamento, Bloco e Estação
dados$PesoAcumulado <- ave(dados$`Peso frutos parcela`, dados$Tratamento, dados$Bloco, dados$`Estação cultivo`, FUN = cumsum)

# Mantendo apenas a última linha de cada combinação
dados <- dados[!duplicated(dados[, c('Tratamento', 'Bloco', 'Estação cultivo')], fromLast = TRUE), ]

#alguns gráficos
ggplot(dados, aes(x = dados$`Estação cultivo`, y = dados$PesoAcumulado , color = `Estação cultivo`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Tratamento)

ggplot(dados, aes(x=as.factor(dados$Tratamento), y=dados$PesoAcumulado))+
  geom_boxplot() 

ggplot(dados, aes(x=as.factor(dados$Bloco), y=dados$PesoAcumulado))+
  geom_boxplot() 


#gráfico por estação/ bloco/tratamento com colheita acumulada
aa = matrix(NA, nrow=3, ncol = 12)

#outono inverno
for(i in 1:3){
  mat = dados %>% filter(`Estação cultivo`=="O-I", Bloco==i)
  aa[i,] = tapply(mat$PesoAcumulado, mat$Tratamento, mean)
}

#tratamentos pares
aa2 = cbind(aa[,2],aa[,4],aa[,6],aa[,8],aa[,10],aa[,12]) 
tratamentos <- c(2, 4, 6, 8, 10, 12)

plot(1, xlim=c(min(tratamentos), max(tratamentos)), ylim = c(2000, 9000),type ="n",
     xlab = "Tratamentos", ylab = "Peso acumulado", main = "")
col = rainbow(3)
axis(side = 1, at = tratamentos1, labels = tratamentos1)
for(i in (1:3)){
  lines(tratamentos,aa2[i,], col = col[i])
}
legend("topleft", legend = 1:3, title = "Blocos", lty=1, col = col)


#tratamentos impares
aa1 = cbind(aa[,1],aa[,3],aa[,5],aa[,7],aa[,9],aa[,11]) 
tratamentos1 <- c(1, 3, 5, 7, 9, 11)

plot(1, xlim=c(min(tratamentos1), max(tratamentos1)), ylim = c(2000, 9000),type ="n",
     xlab = "Tratamentos", ylab = "Peso acumulado", main = "")
col = rainbow(3)
axis(side = 1, at = tratamentos1, labels = tratamentos1)
for(i in (1:3)){
  lines(tratamentos1,aa1[i,], col = col[i])
}
legend("topleft", legend = 1:3, title = "Blocos", lty=1, col = col)


#verão- primavera

aa = matrix(NA, nrow=3, ncol = 12)

for(i in 1:3){
  mat = dados %>% filter(`Estação cultivo`=="P-V", Bloco==i)
  aa[i,] = tapply(mat$PesoAcumulado, mat$Tratamento, mean)
}

#tratamentos pares
aa2 = cbind(aa[,2],aa[,4],aa[,6],aa[,8],aa[,10],aa[,12]) 
tratamentos <- c(2, 4, 6, 8, 10, 12)

plot(1, xlim=c(min(tratamentos), max(tratamentos)), ylim = c(2000, 20000),type ="n",
     xlab = "Tratamentos", ylab = "Peso acumulado", main = "")
col = rainbow(3)
axis(side = 1, at = tratamentos1, labels = tratamentos1)

for(i in (1:3)){
  lines(tratamentos,aa2[i,], col = col[i])
}
legend("topleft", legend = 1:3, title = "Blocos", lty=1, col = col)

#tratamentos impares
aa1 = cbind(aa[,1],aa[,3],aa[,5],aa[,7],aa[,9],aa[,11]) 
tratamentos1 <- c(1, 3, 5, 7, 9, 11)

plot(0, xlim=c(min(tratamentos1), max(tratamentos1)), ylim = c(5000, 15000),type ="n",
     xlab = "Tratamentos", ylab = "Peso acumulado", main = "")
axis(side = 1, at = tratamentos1, labels = tratamentos1)

for(i in (1:3)){
  lines(tratamentos1,aa1[i,], col = col[i])
}
legend("topleft", legend = 1:3, title = "Blocos", lty=1, col = col)

