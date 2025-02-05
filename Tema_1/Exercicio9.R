############################## Exercicio 9 ##############################
# Representa las funciones de riesgo de Y con distribución loglogística, donde 
# log(Y) es una logística con media=1 y desviación=0.5, 1 y 1.5, respectivamente.

library(ggplot2)  ## Para a representación gráfica

## Función de risco logloxística
h_loglog <- function (x, mu, sigma) {
  (exp(-mu) * sigma ^ -1 * (x * exp(-mu))^(1/sigma-1))/(1+x*exp(-mu)^(1/sigma))
}

## Parámetros
MU <- 1
SIGMA_VEC <- c(0.5, 1, 1.5)
COLOR_VEC <- c("red", "blue", "darkgreen")
LABEL_VEC <- paste("μ = 1, σ =", SIGMA_VEC)
  
## Crear marco do gráfico
grafico <- ggplot() + 
  xlim(0, 10) +
  labs(
    title = "Funcións de risco de Y con distribución logloxística",
    x = "tempo",
    y = "y"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_blank())

## Engadir cada función de risco avaliada nos parámetros dados
for (i in 1:length(SIGMA_VEC)) {
  grafico <- grafico + 
    geom_function(aes(color = !!LABEL_VEC[i]), fun = h_loglog, args = list(mu = MU, sigma = SIGMA_VEC[i]), linetype = "dashed")
}
grafico + 
  scale_color_manual(values = COLOR_VEC)

## Representar o gráfico
print(grafico)
