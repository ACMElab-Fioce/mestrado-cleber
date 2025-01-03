# ---------------------------------------------------------
### Algoritmo criado para analisar dados de Sexo/Gênero
# ---------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gridExtra)

dados <- read.csv("dados_sociais.csv", sep=";")

antiN <- subset(dados, tipo == 1)
antiS <- subset(dados, tipo == 2)
antiS <- subset(antiS, anticorpo < 40000)



antiS_M <- subset(antiS, sexo == "Masculino")
antiS_M$anticorpo <- antiS_M$anticorpo/0.142
antiS_F <- subset(antiS, sexo == "Feminino")
antiS_F$anticorpo <- antiS_F$anticorpo/0.142
median(antiS_M$anticorpo)
median(antiS_F$anticorpo)

antiN_M <- subset(antiN, sexo == "Masculino")
antiN_M$anticorpo <- antiN_M$anticorpo/0.142
antiN_F <- subset(antiN, sexo == "Feminino")
antiN_F$anticorpo <- antiN_F$anticorpo/0.142
median(antiN_M$anticorpo)
median(antiN_F$anticorpo)

#---GRAFICOS BOXPLOT------------------------------------------------------------
#---AntiN-----------------------------------------------------------------------

grafico1 <- ggplot(data = antiN, aes(x = sexo, y = anticorpo/0.142, fill = sexo)) +
  geom_violin(alpha = 1, position = position_dodge(width = 0.75), size = 1, color = NA) +
  geom_boxplot(notch = F, outlier.size = -1, color = "black", lwd = 1, alpha = 0.7, show.legend = F) +
  scale_fill_manual(values=c("#efbbff", "#efbbff")) +
  theme(
    axis.text = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 18)
  ) +
  xlab("Sexo") +
  ylab("IgG anti-N (BAU/ml)") +
  labs(fill = "Legenda") +
  guides(fill = FALSE) +
  coord_cartesian(ylim = c(0, 20))
  #geom_text(data = medianas, aes(x = sexo, y = mediana, label = sprintf("Md: %.2f", mediana)),
            #position = position_dodge(width = 0.75), vjust = -0.1, size = 6, color = "black")

#-------------------------------------------------------------------------------
#---AntiS-----------------------------------------------------------------------

grafico2 <- ggplot(data = antiS, aes(x = sexo, y = (anticorpo / 0.142) / 10000, fill = sexo)) +
  geom_violin(alpha = 1, position = position_dodge(width = 0.75), size = 1, color = NA) +
  geom_boxplot(notch = F, outlier.size = -1, color = "black", lwd = 1, alpha = 0.7, show.legend = F) +
  scale_fill_manual(values=c("#43e8d8", "#43e8d8")) +
  theme(
    axis.text = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 18)
  ) +
  xlab("Sexo") +
  ylab("IgG anti-S (x10000) (BAU/ml)") +
  labs(fill = "Legenda") +
  guides(fill = FALSE) +
  coord_cartesian(ylim = c(0, 30))
  #geom_text(data = medianas, aes(x = sexo, y = mediana, label = sprintf("Md: %.2f", mediana)),
            #position = position_dodge(width = 0.75), vjust = -0.5, size = 7, color = "black")


grafico3 <- grid.arrange(grafico1, grafico2, ncol = 2)

ggsave("Grafico_sexo.jpg", plot = grafico3, width = 16, height = 6, dpi = 600)





