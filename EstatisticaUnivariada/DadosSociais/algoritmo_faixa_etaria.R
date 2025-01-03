# ---------------------------------------------------------
### Algoritmo criado para analisar dados de Faixa Etária
# ---------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gridExtra)

dados <- read.csv("dados_sociais.csv", sep=";")
dados <- subset(dados, faixa_etaria != "ND")

antiN <- subset(dados, tipo == 1)
antiS <- subset(dados, tipo == 2)
antiS <- subset(antiS, anticorpo < 40000) #Remoção de possíveis outliers 
                                          #(limitação do equipamento)

#---ANTI-S--------------------------------------------
antiS_16_39 <- subset(antiS, faixa_etaria == "16-39")
antiS_16_39$anticorpo <- antiS_16_39$anticorpo/0.142

antiS_40_59 <- subset(antiS, faixa_etaria == "40-59")
antiS_40_59$anticorpo <- antiS_40_59$anticorpo/0.142

antiS_60_69 <- subset(antiS, faixa_etaria == "60-69")
antiS_60_69$anticorpo <- antiS_60_69$anticorpo/0.142

median(antiS_16_39$anticorpo)
median(antiS_40_59$anticorpo)
median(antiS_60_69$anticorpo)

#---ANTI-N--------------------------------------------
antiN_16_39 <- subset(antiN, faixa_etaria == "16-39")
antiN_16_39$anticorpo <- antiN_16_39$anticorpo/0.142

antiN_40_59 <- subset(antiN, faixa_etaria == "40-59")
antiN_40_59$anticorpo <- antiN_40_59$anticorpo/0.142

antiN_60_69 <- subset(antiN, faixa_etaria == "60-69")
antiN_60_69$anticorpo <- antiN_60_69$anticorpo/0.142

median(antiN_16_39$anticorpo)
median(antiN_40_59$anticorpo)
median(antiN_60_69$anticorpo)



#---GRAFICOS BOXPLOT------------------------------------------------------------
#---AntiN-----------------------------------------------------------------------

grafico4 <- ggplot(data = antiN, aes(x = faixa_etaria, y = anticorpo/0.142, fill = faixa_etaria)) +
  geom_violin(alpha = 1, position = position_dodge(width = 0.75), size = 1, color = NA) +
  #ggbeeswarm::geom_quasirandom(shape = 21, dodge.width = 0.75, color = "black", alpha = 0.2, show.legend = F) +
  geom_boxplot(notch = F, outlier.size = -1, color = "black", lwd = 1, alpha = 0.7, show.legend = F) +
  scale_fill_manual(values=c("#efbbff", "#efbbff", "#efbbff", "#efbbff", "#efbbff"))+
  theme(
    axis.text = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 18)
  ) +
  xlab("Faixa etária (anos)") +
  ylab("IgG anti-N (BAU/ml)") +
  labs(fill = "Legenda") +
  guides(fill = FALSE) +
  coord_cartesian(ylim = c(0, 20))
  #geom_text(data = medianas, aes(x = faixa_etaria, y = mediana, label = sprintf("Md: %.2f", mediana)),
            #position = position_dodge(width = 0.75), vjust = -0.2, size = 4, color = "black")

#-------------------------------------------------------------------------------
#---AntiS-----------------------------------------------------------------------

grafico5 <- ggplot(data = antiS, aes(x = faixa_etaria, y = (anticorpo / 0.142) / 10000, fill = faixa_etaria)) +
  geom_violin(alpha = 1, position = position_dodge(width = 0.75), size = 1, color = NA) +
  geom_boxplot(notch = F, outlier.size = -1, color = "black", lwd = 1, alpha = 0.7, show.legend = F) +
  scale_fill_manual(values=c("#43e8d8", "#43e8d8", "#43e8d8", "#43e8d8", "#43e8d8")) +
  theme(
    axis.text = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold", size = 18)
  ) +
  xlab("Faixa etária (anos)") +
  ylab("IgG anti-S (x10000) (BAU/ml)") +
  labs(fill = "Legenda") +
  guides(fill = FALSE) +
  coord_cartesian(ylim = c(0, 30))
  #geom_text(data = medianas, aes(x = faixa_etaria, y = mediana, label = sprintf("Md: %.2f", mediana)),
            #position = position_dodge(width = 0.75), vjust = -1.5, size = 5, color = "black")



grafico6 <- grid.arrange(grafico4, grafico5, ncol = 2)

ggsave("Grafico_faixa_etaria.jpg", plot = grafico6, width = 16, height = 6, dpi = 600)


