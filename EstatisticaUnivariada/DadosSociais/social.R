library(dplyr)
library(ggplot2)
library(gridExtra)

dados <- read.csv("dados_sociais.csv", sep=";")

antiN <- subset(dados, tipo == 1)
antiS <- subset(dados, tipo == 2)
antiS <- subset(antiS, anticorpo < 40000)

#---GRAFICOS BOXPLOT------------------------------------------------------------

#---SEXO------------------------------------------------------------------------
#---AntiN-----------------------------------------------------------------------

medianas <- antiN %>%
  group_by(sexo) %>%
  summarise(mediana = median(anticorpo / 0.142))


grafico1 <- ggplot(data = antiN, aes(x = sexo, y = anticorpo/0.142, fill = sexo)) +
  geom_violin(alpha = 0.5, position = position_dodge(width = 0.75), size = 1, color = NA) +
  ggbeeswarm::geom_quasirandom(shape = 21, dodge.width = 0.75, color = "black", alpha = 0.2, show.legend = F) +
  geom_boxplot(notch = F, outlier.size = -1, color = "black", lwd = 1, alpha = 0.7, show.legend = F) +
  theme(
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Sexo") +
  ylab("IgG anti-N (BAU/ml)") +
  labs(fill = "Legenda") +
  guides(fill = FALSE) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 90, by = 10)) +
  geom_text(data = medianas, aes(x = sexo, y = mediana, label = sprintf("Md: %.2f", mediana)),
            position = position_dodge(width = 0.75), vjust = -1.2, size = 6, color = "black")

#-------------------------------------------------------------------------------
#---SEXO------------------------------------------------------------------------
#---AntiS-----------------------------------------------------------------------

medianas <- antiS %>%
  group_by(sexo) %>%
  summarise(mediana = median((anticorpo / 0.142) / 10000))

grafico2 <- ggplot(data = antiS, aes(x = sexo, y = (anticorpo / 0.142) / 10000, fill = sexo)) +
  geom_violin(alpha = 0.5, position = position_dodge(width = 0.75), size = 1, color = NA) +
  ggbeeswarm::geom_quasirandom(shape = 21, dodge.width = 0.75, color = "black", alpha = 0.2, show.legend = F) +
  geom_boxplot(notch = F, outlier.size = -1, color = "black", lwd = 1, alpha = 0.7, show.legend = F) +
  theme(
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Sexo") +
  ylab("IgG anti-S (x10000) (BAU/ml)") +
  labs(fill = "Legenda") +
  guides(fill = FALSE) +
  scale_y_continuous(limits = c(0, 30)) +
  geom_text(data = medianas, aes(x = sexo, y = mediana, label = sprintf("Md: %.2f", mediana)),
           position = position_dodge(width = 0.75), vjust = -1.5, size = 6, color = "black")


#-------------------------------------------------------------------------------
#---FAIXA ETARIA------------------------------------------------------------------------
#---AntiN-----------------------------------------------------------------------
antiN <- subset(antiN, faixa_etaria != "ND")
antiS <- subset(antiS, faixa_etaria != "ND")

medianas <- antiN %>%
  group_by(faixa_etaria) %>%
  summarise(mediana = median(anticorpo / 0.142))


grafico3 <- ggplot(data = antiN, aes(x = faixa_etaria, y = anticorpo/0.142, fill = faixa_etaria)) +
  geom_violin(alpha = 0.5, position = position_dodge(width = 0.75), size = 1, color = NA) +
  ggbeeswarm::geom_quasirandom(shape = 21, dodge.width = 0.75, color = "black", alpha = 0.2, show.legend = F) +
  geom_boxplot(notch = F, outlier.size = -1, color = "black", lwd = 1, alpha = 0.7, show.legend = F) +
  theme(
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Faixa etária") +
  ylab("IgG anti-N (BAU/ml)") +
  labs(fill = "Legenda") +
  guides(fill = FALSE) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 90, by = 10)) +
  geom_text(data = medianas, aes(x = faixa_etaria, y = mediana, label = sprintf("Md: %.2f", mediana)),
            position = position_dodge(width = 0.75), vjust = -0.8, size = 6, color = "black")


#-------------------------------------------------------------------------------
#---SEXO------------------------------------------------------------------------
#---AntiS-----------------------------------------------------------------------

medianas <- antiS %>%
  group_by(faixa_etaria) %>%
  summarise(mediana = median((anticorpo / 0.142) / 10000))

grafico4 <- ggplot(data = antiS, aes(x = faixa_etaria, y = (anticorpo / 0.142) / 10000, fill = faixa_etaria)) +
  geom_violin(alpha = 0.5, position = position_dodge(width = 0.75), size = 1, color = NA) +
  ggbeeswarm::geom_quasirandom(shape = 21, dodge.width = 0.75, color = "black", alpha = 0.2, show.legend = F) +
  geom_boxplot(notch = F, outlier.size = -1, color = "black", lwd = 1, alpha = 0.7, show.legend = F) +
  theme(
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14)
  ) +
  xlab("Faixa etária") +
  ylab("IgG anti-S (x10000) (BAU/ml)") +
  labs(fill = "Legenda") +
  guides(fill = FALSE) +
  scale_y_continuous(limits = c(0, 30)) +
  geom_text(data = medianas, aes(x = faixa_etaria, y = mediana, label = sprintf("Md: %.2f", mediana)),
            position = position_dodge(width = 0.75), vjust = -1.5, size = 6, color = "black")
