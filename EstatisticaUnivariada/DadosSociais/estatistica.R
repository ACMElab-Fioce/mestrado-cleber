# -----------------------------------------
### Análises estatísticas dos dados sociais
# -----------------------------------------

library(dplyr)
library(ggplot2)
library(gridExtra)
library(rstatix)
library(PMCMRplus)

dados <- read.csv("dados_sociais.csv", sep=";")

antiN <- subset(dados, tipo == 1)
antiS <- subset(dados, tipo == 2)
antiS <- subset(antiS, anticorpo < 40000)

#------------------------------------------------------
#Mann-whitney (2 grupos)

# Sexo/gênero
masculino_n <- subset(antiN, sexo == "Masculino")
feminino_n <- subset(antiN, sexo == "Feminino")
resultado <- wilcox.test(masculino_n$anticorpo, feminino_n$anticorpo)

masculino_s <- subset(antiS, sexo == "Masculino")
feminino_s <- subset(antiS, sexo == "Feminino")
resultado <- wilcox.test(masculino_s$anticorpo, feminino_s$anticorpo)


#Kruskall-wallis (>2 grupos) 
#Teste de Dunn com correções de Bonferroni (>2 grupos)
#Faixa etária
antiN2 <- subset(antiN, faixa_etaria != "ND")
antiS2 <- subset(antiS, faixa_etaria != "ND")
resultado1 <- kruskal.test(anticorpo ~ faixa_etaria, data = antiN2)
resultado2 <- kruskal.test(anticorpo ~ faixa_etaria, data = antiS2)

teste1 <- dunn_test(anticorpo ~ faixa_etaria, data = antiN2, p.adjust.method = "bonferroni")
teste2 <- dunn_test(anticorpo ~ faixa_etaria, data = antiS2, p.adjust.method = "bonferroni")
write.csv(teste1, file = "AntiN_faixa_etaria.csv", row.names = FALSE)
write.csv(teste2, file = "AntiS_faixa_etaria.csv", row.names = FALSE)

#Raça/etnia
antiN2 <- subset(antiN, raca != "ND")
antiS2 <- subset(antiS, raca != "ND")
antiN2 <- subset(antiN2, raca != "INDIGENA")
antiS2 <- subset(antiS2, raca != "INDIGENA")
resultado1 <- kruskal.test(anticorpo ~ raca, data = antiN2)
resultado2 <- kruskal.test(anticorpo ~ raca, data = antiS2)

teste1 <- dunn_test(anticorpo ~ raca, data = antiN2, p.adjust.method = "bonferroni")
teste2 <- dunn_test(anticorpo ~ raca, data = antiS2, p.adjust.method = "bonferroni")

#Escolaridade
antiN2 <- subset(antiN, escolaridade != "ND")
antiS2 <- subset(antiS, escolaridade != "ND")
resultado1 <- kruskal.test(anticorpo ~ escolaridade2, data = antiN2)
resultado2 <- kruskal.test(anticorpo ~ escolaridade2, data = antiS2)

teste1 <- dunn_test(anticorpo ~ escolaridade2, data = antiN2, p.adjust.method = "bonferroni")
teste2 <- dunn_test(anticorpo ~ escolaridade2, data = antiS2, p.adjust.method = "bonferroni")
write.csv(teste1, file = "AntiN_escolaridade.csv", row.names = FALSE)
write.csv(teste2, file = "AntiS_escolaridade.csv", row.names = FALSE)



