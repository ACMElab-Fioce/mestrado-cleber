library(dplyr)
library(ggplot2)
library(gridExtra)

dados <- read.csv("FORTALEZA_resumo.csv", sep=";")
dados$data <- as.Date(substr(dados$data, 1, 10), format = "%d/%m/%Y")

dados <- dados[dados$data >= as.Date("2020-01-01") & dados$data <= as.Date("2024-04-30"), ]


#---CASOS-----------------------------------------------------------------------
casos <- subset(dados, tipo != "Total")
casos <- subset(casos, tipo != "Obitos")

casos$tipo <- factor(casos$tipo, levels = c("Negativo", "Positivo"))

#Grafico - casos
grafico1 <- ggplot(data = casos, aes(x = data, y = quantidade/1000, fill = tipo)) +
  geom_area(position = "stack") +
  scale_fill_manual(values = c("#b3cde0", "#03396c")) +
  #scale_y_continuous(labels = function(x) paste0(x, "k")) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.05, 0.70),
  ) +
  xlab("") +
  ylab("Número de casos (x1000)") +
  labs(fill = "Legenda:") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 5))
  #guides(fill = FALSE) 

grafico1 <- grafico1 + scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")

#ggsave("grafico_joaquim.jpg", plot = grafico1, width = 16, height = 8, dpi = 600)

#---OBITOS----------------------------------------------------------------------
obitos <- subset(dados, tipo =="Obitos")

grafico2 <- ggplot(data = obitos, aes(x = data, y = quantidade, fill = tipo)) +
  geom_area(position = "stack") +
  scale_fill_manual(values = c("#bf0000")) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.05, 0.70),
  ) +
  xlab("") +
  ylab("Número de óbitos") +
  labs(fill = "Legenda:")
  #guides(fill = FALSE)

grafico2 <- grafico2 + scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")

#---VACINA----------------------------------------------------------------------
vacina <- read.csv("vacina_CE.csv", sep=",")

vacina$Vacina <- factor(vacina$Vacina, levels = c("Sem vacina", "Dose 01", 
                                                  "Dose 02", "Dose 03", 
                                                  "Dose 04"))

grafico3 <- ggplot(data = vacina, aes(fill = Vacina, y = porcentagem/100, x = Periodo)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_bw() +
  scale_fill_manual(values = c("#b2d8d8", "#66b2b2","#008080", "#006666", "#004c4c")) +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.06, 0.60)
    #legend.position = c(0.06, 0.50)
  ) +
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = function(x) format(x, scientific = FALSE)) +
  labs(fill = "Legenda") +
  ylab("Vacinação (percentual)")
  #guides(fill = FALSE)

ggsave("grafico_vacina.jpg", plot = grafico3, width = 16, height = 8, dpi = 600)


#---PROSANGUE-------------------------------------------------------------------
prosangue <- read.csv("prosangue_igg.csv", sep=";")

#-------------------------------------------------------------------------------
#AntiN
antiN <- subset(prosangue, tipo == 1)

grafico4 <- ggplot(data = antiN, aes(x = data, y = anticorpo/0.142, fill = "tipo")) +
  geom_violin(alpha = 0.5, position = position_dodge(width = 0.75), size = 1, color = NA) +
  geom_boxplot(notch = F, outlier.size = -1, color = "black", lwd = 1, alpha = 0.7, show.legend = F) +
  ggbeeswarm::geom_quasirandom(shape = 21, dodge.width = 0.75, color = "black", alpha = 0.5, show.legend = F) +
  scale_fill_manual(values = c("#d896ff")) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.9, 0.70),
  ) +
  xlab("") +
  ylab("IgG anti-N (BAU/ml)") +
  labs(fill = "Legenda") +
  guides(fill = FALSE) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, by = 10))

#-------------------------------------------------------------------------------
#AntiS
antiS <- subset(prosangue, tipo == 2)
#antiS$data <- as.Date(antiS$data_coleta, format = "%Y-%m-%d")

#antiS$data <- substr(antiS$data_coleta, 1, 7)
#antiS$data <- as.Date(antiS$data)
#antiS <- subset(antiS, anticorpo != 40000)
#antiS <- subset(antiS, anticorpo != 80000)

grafico5 <- ggplot(data = antiS, aes(x = data, y = (anticorpo/0.142)/10000, fill = "tipo")) +
  geom_violin(alpha = 0.5, position = position_dodge(width = 0.75), size = 1, color = NA) +
  geom_boxplot(notch = F, outlier.size = -1, color = "black", lwd = 1, alpha = 0.7, show.legend = F) +
  ggbeeswarm::geom_quasirandom(shape = 21, dodge.width = 0.75, color = "black", alpha = 0.5, show.legend = F) +
  scale_fill_manual(values = c("#00c2c7")) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.9, 0.70),
  ) +
  xlab("Período") +
  ylab("IgG anti-S (x10000) (BAU/ml)") +
  labs(fill = "Legenda:") +
  guides(fill = FALSE) +
  scale_y_continuous(limits = c(0, 30))



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#---UNIAO-----------------------------------------------------------------------
grafico6 <- grid.arrange(grafico1, grafico2, grafico3, ncol = 1)
grafico7 <- grid.arrange(grafico4, grafico5, ncol = 1)
grafico8 <- grid.arrange(grafico6, grafico7, ncol = 1)

#ggsave("casos_obitos_vacina.jpg", plot = grafico6, width = 16, height = 16, dpi = 600)
#ggsave("antiN_antiS.jpg", plot = grafico7, width = 16, height = 12, dpi = 600)
ggsave("grafico_final.jpg", plot = grafico8, width = 16, height = 20, dpi = 600)


#SLIDES
slide1 <- grid.arrange(grafico1, grafico2, grafico4, ncol = 1)
ggsave("slide1.jpg", plot = slide1, width = 22, height = 12, dpi = 600)

slide2 <- grid.arrange(grafico1, grafico3, grafico5, ncol = 1)
ggsave("slide2.jpg", plot = slide2, width = 22, height = 12, dpi = 600)






