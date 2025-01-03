# ---------------------------------------------------------
### Algoritmo criado para analisar dados de Escolaridade
# ---------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gridExtra)

dados <- read.csv("dados_sociais.csv", sep=";")
dados <- subset(dados, escolaridade2 != "ND")

dados$escolaridade2 <- factor(dados$escolaridade2, 
                              levels = c("Ensino medio incompleto",
                                         "Ensino medio completo",
                                         "Terceiro grau completo"))
                                                              
                                                            

antiN <- subset(dados, tipo == 1)
antiS <- subset(dados, tipo == 2)
antiS <- subset(antiS, anticorpo < 40000) #Remoção de possíveis outliers 
                                          #(limitação do equipamento)

#---ANTI-S--------------------------------------------
antiS_EMI <- subset(antiS, escolaridade2 == "Ensino medio incompleto")
antiS_EMI$anticorpo <- antiS_EMI$anticorpo/0.142

antiS_EMC <- subset(antiS, escolaridade2 == "Ensino medio completo")
antiS_EMC$anticorpo <- antiS_EMC$anticorpo/0.142

antiS_TGC <- subset(antiS, escolaridade2 == "Terceiro grau completo")
antiS_TGC$anticorpo <- antiS_TGC$anticorpo/0.142

median(antiS_EMI$anticorpo)
median(antiS_EMC$anticorpo)
median(antiS_TGC$anticorpo)


table(antiS$escolaridade2)

#---ANTI-N--------------------------------------------
antiN_EMI <- subset(antiN, escolaridade2 == "Ensino medio incompleto")
antiN_EMI$anticorpo <- antiN_EMI$anticorpo/0.142

antiN_EMC <- subset(antiN, escolaridade2 == "Ensino medio completo")
antiN_EMC$anticorpo <- antiN_EMC$anticorpo/0.142

antiN_TGC <- subset(antiN, escolaridade2 == "Terceiro grau completo")
antiN_TGC$anticorpo <- antiN_TGC$anticorpo/0.142

median(antiN_EMI$anticorpo)
median(antiN_EMC$anticorpo)
median(antiN_TGC$anticorpo)


table(antiN$escolaridade2)

#---GRAFICOS BOXPLOT------------------------------------------------------------

#---AntiN-----------------------------------------------------------------------
grafico7 <- ggplot(data = antiN, aes(x = escolaridade2, y = anticorpo/0.142, fill = escolaridade2)) +
  geom_violin(alpha = 1, position = position_dodge(width = 0.75), size = 1, color = NA) +
  #ggbeeswarm::geom_quasirandom(shape = 21, dodge.width = 0.75, color = "black", alpha = 0.2, show.legend = F) +
  geom_boxplot(notch = F, outlier.size = -1, color = "black", lwd = 1, alpha = 0.7, show.legend = F) +
  scale_fill_manual(values=c("#efbbff", "#efbbff", "#efbbff"))+
  theme(
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Escolaridade") +
  ylab("IgG anti-N (BAU/ml)") +
  labs(fill = "Legenda") +
  guides(fill = FALSE) +
  coord_cartesian(ylim = c(0, 20))
  #geom_text(data = medianas, aes(x = escolaridade2, y = mediana, label = sprintf("Md: %.2f", mediana)),
            #position = position_dodge(width = 0.75), vjust = -0.5, size = 5, color = "black")

#-------------------------------------------------------------------------------
#---AntiS-----------------------------------------------------------------------
medianas <- antiS %>%
  group_by(escolaridade2) %>%
  summarise(mediana = median((anticorpo / 0.142) / 10000))

grafico8 <- ggplot(data = antiS, aes(x = escolaridade2, y = (anticorpo / 0.142) / 10000, fill = escolaridade2)) +
  geom_violin(alpha = 1, position = position_dodge(width = 0.75), size = 1, color = NA) +
  #ggbeeswarm::geom_quasirandom(shape = 21, dodge.width = 0.75, color = "black", alpha = 0.2, show.legend = F) +
  geom_boxplot(notch = F, outlier.size = -1, color = "black", lwd = 1, alpha = 0.7, show.legend = F) +
  scale_fill_manual(values=c("#43e8d8", "#43e8d8", "#43e8d8"))+
  theme(
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Escolaridade") +
  ylab("IgG anti-S (x10000) (BAU/ml)") +
  labs(fill = "Legenda") +
  guides(fill = FALSE) +
  coord_cartesian(ylim = c(0, 30))
  #geom_text(data = medianas, aes(x = escolaridade2, y = mediana, label = sprintf("Md: %.2f", mediana)),
            #position = position_dodge(width = 0.75), vjust = -1.5, size = 5, color = "black")



grafico9 <- grid.arrange(grafico7, grafico8, ncol = 2)

ggsave("Grafico_escolaridade.jpg", plot = grafico9, width = 16, height = 6, dpi = 600)
