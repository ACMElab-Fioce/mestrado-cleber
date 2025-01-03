install.packages("effsize")
library(dplyr)
library(ggplot2)
library(gridExtra)
library(rstatix)
library(PMCMRplus)
library(effsize)

###-----------------------------------------------------------------------------
###   DADOS SOCIAIS   ###
###-----------------------------------------------------------------------------

dados <- read.csv("dados_sociais.csv", sep=";")
antiN <- subset(dados, tipo == 1)
antiS <- subset(dados, tipo == 2)
antiS <- subset(antiS, anticorpo < 40000)

#------------------------
#Faixa etaria - antiN
#------------------------
#Teste 01 - 16-39 e 40-59
teste <- subset(antiN, faixa_etaria != "60-69" & faixa_etaria != "ND")
table(teste$faixa_etaria)
cliff.delta(anticorpo ~ faixa_etaria, data=teste)

#Teste 02 - 40-59 e 60-69
teste <- subset(antiN, faixa_etaria != "16-39" & faixa_etaria != "ND")
table(teste$faixa_etaria)
cliff.delta(anticorpo ~ faixa_etaria, data=teste)

#------------------------
#Faixa etaria - antiS
#------------------------
#Teste 01 - 16-39 e 40-59
teste <- subset(antiS, faixa_etaria != "60-69" & faixa_etaria != "ND")
table(teste$faixa_etaria)
cliff.delta(anticorpo ~ faixa_etaria, data=teste)


#------------------------
#Raça - antiN
#------------------------
#Teste 01 - Amarela e Parda
teste <- subset(antiN, raca != "Branca" & raca != "Preta" 
                     & raca != "INDIGENA" & raca != "ND")
table(teste$raca)
cliff.delta(anticorpo ~ raca, data=teste)

#Teste 02 - Amarela e Preta
teste <- subset(antiN, raca != "Branca" & raca != "Parda" 
                & raca != "INDIGENA" & raca != "ND")
table(teste$raca)
cliff.delta(anticorpo ~ raca, data=teste)

#Teste 03 - Branca e Parda
teste <- subset(antiN, raca != "Preta" & raca != "Amarela" 
                & raca != "INDIGENA" & raca != "ND")
table(teste$raca)
cliff.delta(anticorpo ~ raca, data=teste)

#Teste 04 - Branca e Preta
teste <- subset(antiN, raca != "Parda" & raca != "Amarela" 
                & raca != "INDIGENA" & raca != "ND")
table(teste$raca)
cliff.delta(anticorpo ~ raca, data=teste)


#------------------------
#Escolaridade - antiN
#------------------------
#Teste 01 - EMI e TC
table(antiN$escolaridade2)
teste <- subset(antiN, escolaridade2 != "Ensino medio completo" & 
                       escolaridade2 != "ND")
table(teste$escolaridade2)
cliff.delta(anticorpo ~ escolaridade2, data=teste)

#Teste 02 - EMC e TC
teste <- subset(antiN, escolaridade2 != "Ensino medio incompleto" & 
                  escolaridade2 != "ND")
table(teste$escolaridade2)
cliff.delta(anticorpo ~ escolaridade2, data=teste)


#------------------------
#Escolaridade - antiS
#------------------------
#Teste 01 - EMI e EMC
teste <- subset(antiS, escolaridade2 != "Terceiro grau completo" & 
                  escolaridade2 != "ND")
table(teste$escolaridade2)
cliff.delta(anticorpo ~ escolaridade2, data=teste)

#Teste 02- EMI e TC
teste <- subset(antiS, escolaridade2 != "Ensino medio completo" & 
                  escolaridade2 != "ND")
table(teste$escolaridade2)
cliff.delta(anticorpo ~ escolaridade2, data=teste)







