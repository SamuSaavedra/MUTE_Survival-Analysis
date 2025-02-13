# 3) Se tienen las siguientes observaciones correspondientes al tiempo (en días)
# que viven unas ratas de laboratorio sometidas a radiación. Las cruces indican 
# tiempos censurados: 1, 2, 3, 3, 3+, 1, 7+, 5, 2+, 4+. 
# a) Calcula y representa a mano el estimador de Kaplan-Meier de la función de 
# supervivencia. Indica el valor de la mediana. 
# b) Repite el apartado a) utilizando R.

library(ggplot2)  # para a representación gráfica
source("Tema_2/Funcion_K-M.R")  # cargar a función de Kaplan-Meier calculada polo método do produto

observacions <- data.frame(
  t = c(1,2,3,3,3,1,7,5,2,4),
  d = c(1,1,1,1,0,1,0,1,0,0)
)

## Cáculo da función de supervivencia e da mediana
funcion_supervivencia <- kaplanmeier_produto(observacions, times_name = "t", deltas_name = "d")

mediana <- funcion_supervivencia$t[which(funcion_supervivencia$s <= 0.5)[1]]

## Representación da función de supervivencia e da mediana
ggplot(data = funcion_supervivencia, aes(x = t, y = s)) +
  geom_step(color = "cyan4", linewidth = 1.5) +
  annotate("segment", x = 0, xend = mediana, y = 0.5, yend = 0.5,
           linetype = "dashed", color = "red", linewidth = 1) +  # liña horizontal mediana
  annotate("segment", x = mediana, xend = mediana, y = 0, yend = 0.5,
           linetype = "dashed", color = "red", linewidth = 1) +  # liña vertical mediana
  annotate("point", x = mediana, y = 0.5, color = "red", size = 3) +  # punto de intersección mediana
  annotate("text", x = mediana + 0.2, y = 0, label = paste0("Mediana (Me = ", mediana,")"), color = "black", 
           size = 4, hjust = 0) +  # etiqueta mediana
  annotate("point", x = mediana, y = 0, color = "black", size = 5, shape = 4, stroke = 2) +  # x na mediana
  labs(title = "Función de supervivencia; Ŝ(t)",
       x = "Tempo (días)",
       y = "Probabilidade de supervivencia") +
  ylim(0, 1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
