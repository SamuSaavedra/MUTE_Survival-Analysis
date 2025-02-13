# 4) Se tienen las siguientes observaciones correspondientes al tiempo de
# recaída de cierta enfermedad (en meses). Las cruces indican tiempos censurados:
# 6, 6, 6+, 5, 5+, 6, 3+, 3, 3, 5. Resuelve a mano de forma detallada los
# siguientes apartados:
# a) Calcula y representa el estimador de Kaplan-Meier de la función de 
# supervivencia.
# b) Estima la supervivencia en t=4 y calcula un intervalo de confianza para 
# esa supervivencia.
# c) ¿En qué tiempo se observa un riesgo estimado más alto?
# d) Estima el tiempo medio y mediano de recaída a partir del estimador de 
# Kaplan-Meier de lasupervivencia.
# Ahora utiliza R y resuelve los apartados anteriores. 
# Revisa que los resultados coinciden.

library(survival)
library(ggplot2)

observacions <- data.frame(
  t = c(6, 6, 6, 5, 5, 6, 3, 3, 3, 5),
  d = c(1, 1, 0, 1, 0, 1, 0, 1, 1, 1)
)

## a) Calcula e representa o estimador de K-M
modelo_km <- survfit(Surv(observacions$t, observacions$d)~1, conf.type = "plain")
df_km <- data.frame(
  t = c(0, modelo_km$time),
  s = c(1, modelo_km$surv)
)

ggplot(df_km, aes(x = t, y = s)) +
  geom_step(color = "cyan4", linewidth = 1.5) +
  labs(title = "Función de supervivencia; Ŝ(t)",
       x = "Tempo (meses)",
       y = "Probabilidade de supervivencia") +
  ylim(0, 1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## b) Estima a supervivencia en t = 4 e calcula un IC para esa supervivencia
tempo_4 <- summary(modelo_km, times = 4)
cat(paste0("Ŝ(4) = ", tempo_4$surv, 
           "\nIC95% = [", round(tempo_4$lower, 2), ", ", round(tempo_4$upper, 2), "]\n"))

## c) En que tempo se observa un risco estimado maior?
ti <- modelo_km$time
ni <- modelo_km$n.risk
di <- modelo_km$n.event
ht <- di/ni

cat(paste0("λ(", ti[which.max(ht)], ") = ", max(ht), "\n"))

## d) Estima o tempo medio e mediano de recaída a partir do estimador K-M
media <- summary(modelo_km)$table["rmean"][[1]]
mediana <- summary(modelo_km)$table["median"][[1]]
cat(paste0("E(t): ", round(media, 2), " meses.\nMediana(t): ", mediana, " meses.\n"))
