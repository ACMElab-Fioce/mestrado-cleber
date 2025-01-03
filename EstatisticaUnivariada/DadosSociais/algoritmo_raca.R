# ---------------------------------------------------------
### Algoritmo criado para analisar dados de Raça/Etnia
# ---------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gridExtra)

dados <- read.csv("dados_sociais.csv", sep=";")
dados <- subset(dados, raca != "ND")
dados <- subset(dados, raca != "INDIGENA")

antiN <- subset(dados, tipo == 1)
antiS <- subset(dados, tipo == 2)
antiS <- subset(antiS, anticorpo < 40000) #Remoção de possíveis outliers 
                                          #(limitação do equipamento)

#---ANTI-S--------------------------------------------
antiS_A <- subset(antiS, raca == "Amarela")
antiS_A$anticorpo <- antiS_A$anticorpo/0.142

antiS_B <- subset(antiS, raca == "Branca")
antiS_B$anticorpo <- antiS_B$anticorpo/0.142

antiS_PA <- subset(antiS, raca == "Parda")
antiS_PA$anticorpo <- antiS_PA$anticorpo/0.142

antiS_PR <- subset(antiS, raca == "Preta")
antiS_PR$anticorpo <- antiS_PR$anticorpo/0.142

median(antiS_A$anticorpo)
median(antiS_B$anticorpo)
median(antiS_PA$anticorpo)
median(antiS_PR$anticorpo)

table(antiS$raca)

#---ANTI-N--------------------------------------------
antiN_A <- subset(antiN, raca == "Amarela")
antiN_A$anticorpo <- antiN_A$anticorpo/0.142

antiN_B <- subset(antiN, raca == "Branca")
antiN_B$anticorpo <- antiN_B$anticorpo/0.142

antiN_PA <- subset(antiN, raca == "Parda")
antiN_PA$anticorpo <- antiN_PA$anticorpo/0.142

antiN_PR <- subset(antiN, raca == "Preta")
antiN_PR$anticorpo <- antiN_PR$anticorpo/0.142

median(antiN_A$anticorpo)
median(antiN_B$anticorpo)
median(antiN_PA$anticorpo)
median(antiN_PR$anticorpo)

table(antiN$raca)

#---GRAFICOS BOXPLOT------------------------------------------------------------
#---AntiN-----------------------------------------------------------------------

grafico7 <- ggplot(data = antiN, aes(x = raca, y = anticorpo/0.142, fill = raca)) +
  geom_violin(alpha = 1, position = position_dodge(width = 0.75), size = 1, color = NA) +
  #ggbeeswarm::geom_quasirandom(shape = 21, dodge.width = 0.75, color = "black", alpha = 0.2, show.legend = F) +
  geom_boxplot(notch = F, outlier.size = -1, color = "black", lwd = 1, alpha = 0.7, show.legend = F) +
  scale_fill_manual(values=c("#efbbff", "#efbbff", "#efbbff", "#efbbff"))+
  theme(
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Raça autodeclarada") +
  ylab("IgG anti-N (BAU/ml)") +
  labs(fill = "Legenda") +
  guides(fill = FALSE) +
  coord_cartesian(ylim = c(0, 20))
#geom_text(data = medianas, aes(x = escolaridade2, y = mediana, label = sprintf("Md: %.2f", mediana)),
#position = position_dodge(width = 0.75), vjust = -0.5, size = 5, color = "black")

#-------------------------------------------------------------------------------
#---AntiS-----------------------------------------------------------------------
grafico8 <- ggplot(data = antiS, aes(x = raca, y = (anticorpo / 0.142) / 10000, fill = raca)) +
  geom_violin(alpha = 1, position = position_dodge(width = 0.75), size = 1, color = NA) +
  #ggbeeswarm::geom_quasirandom(shape = 21, dodge.width = 0.75, color = "black", alpha = 0.2, show.legend = F) +
  geom_boxplot(notch = F, outlier.size = -1, color = "black", lwd = 1, alpha = 0.7, show.legend = F) +
  scale_fill_manual(values=c("#43e8d8", "#43e8d8", "#43e8d8", "#43e8d8")) +
  theme(
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Raça autodeclarada") +
  ylab("IgG anti-S (x10000) (BAU/ml)") +
  labs(fill = "Legenda") +
  guides(fill = FALSE) +
  coord_cartesian(ylim = c(0, 30))
#geom_text(data = medianas, aes(x = escolaridade2, y = mediana, label = sprintf("Md: %.2f", mediana)),
#position = position_dodge(width = 0.75), vjust = -1.5, size = 5, color = "black")


grafico9 <- grid.arrange(grafico7, grafico8, ncol = 2)

ggsave("Grafico raca.jpg", plot = grafico9, width = 16, height = 6, dpi = 600)
