# 1) Para los datos gastricXelox usar R para representar el estimador de 
# Kaplan-Meier de la supervivencia con intervalos de confianza (log-log) y 
# poniendo marcas en las observaciones censuradas. Transformar previamente el 
# tiempo de semanas a meses. ¿En cuánto se estima la supervivencia a 6 meses? 
# ¿Y a un año?

library(ggplot2)  # representación gráfica
library(asaur)  # datos
data("gastricXelox")
source("Tema_2/Funcion_K-M.R")

## Transformamos o tempo (días) a meses
dias_ano <- 365.25  # días por ano aproximadamente
dias_mes <- dias_ano / 12  # días por mes aproximadamente
semanas_mes <- dias_mes / 7  # semanas por mes aproximadamente
gastricXelox$timesMonths <- gastricXelox$timeWeeks / semanas_mes

## Axustamos o modelo segundo o estimador K-M
modelo_km <- survfit(Surv(gastricXelox$timesMonths, gastricXelox$delta)~1)
df_km <- data.frame(
  t = modelo_km$time,
  d = modelo_km$n.censor,
  s = modelo_km$surv,
  lower = modelo_km$lower,
  upper = modelo_km$upper
)

## Supervivencia en 6 meses
supervivencia_6_meses <- df_km$s[sum(df_km$t <= 6)]

## Supervivencia en 12 meses
supervivencia_12_meses <- df_km$s[sum(df_km$t <= 12)]

## Graficamos os resultados
ggplot(df_km, aes(x = t, y = s)) +
  geom_step(color = "cyan4", linewidth = 1.5) +
  geom_step(aes(y = lower), color = "lightblue4", linewidth = 0.5, linetype = "dashed") +
  geom_step(aes(y = upper), color = "lightblue4", linewidth = 0.5, linetype = "dashed") +
  geom_point(data = subset(df_km, d == 1), aes(x = t, y = s, color = "Dato censurado"),
             size = 3, shape = 4) +
  annotate("point", x = 6, y = supervivencia_6_meses, color = "violet", size = 3, shape = 16) +
  annotate("point", x = 12, y = supervivencia_12_meses, color = "violet", size = 3, shape = 16) +
  annotate("segment", x = 0, xend = 6, y = supervivencia_6_meses, yend = supervivencia_6_meses,
           linetype = "dashed", color = "violet", linewidth = 1) +  # liña horizontal 6 meses
  annotate("segment", x = 0, xend = 12, y = supervivencia_12_meses, yend = supervivencia_12_meses,
           linetype = "dashed", color = "violet", linewidth = 1) +  # liña horizontal 12 meses
  annotate("segment", x = 6, xend = 6, y = 0, yend = supervivencia_6_meses,
           linetype = "dashed", color = "violet", linewidth = 1) +  # liña vertical 6 meses
  annotate("segment", x = 12, xend = 12, y = 0, yend = supervivencia_12_meses,
           linetype = "dashed", color = "violet", linewidth = 1) +  # liña vertical 12 meses
  scale_color_manual(values = c("Dato censurado" = "red")) +
  labs(title = "Función de supervivencia; Ŝ(t) con IC log-log",
       x = "Tempo (meses)",
       y = "Probabilidade de supervivencia",
       color = "Lenda") +
  ylim(0, 1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = c(0.85, 0.85),
        legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
        legend.title = element_text(hjust = 0.5, color = "black", face = "bold"))
