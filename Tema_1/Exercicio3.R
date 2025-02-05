############################## Exercicio 3 ##############################
# Sea X una variable discreta con probabilidad 1/3 en cada uno de los tiempos 
# (meses): 1, 2 y 4. Calcula y representa su función de probabilidad, de 
# distribución, de supervivencia, la función de riesgo y la de riesgo acumulado.

## X = "Tempo (en meses)" v.a. discreta, valores {1, 2, 4}, P(X = x) = 1/3
## f(x), F(x), S(x), h(x), H(x)?

library(ggplot2)  ## Para a representación gráfica

## 1. Función de probabilidade (de masa) [pmf, P(X = x)]
## P(X = 1) = 1/3, P(X = 2) = 1/3, P(X = 4) = 1/3.
prob_f_prob_masa <- rep(0, 6)
prob_f_prob_masa[c(2, 3, 5)] <- 1/3
meses <- 0:5
data_f_prob_masa <- data.frame(x = meses, y = prob_f_prob_masa)

ggplot(data_f_prob_masa, aes(x = x, y = y)) + 
  geom_point(color = "cyan4", size = 5) +
  geom_segment(aes(xend = x, yend = 0), color = "cyan4", size = 2) +
  labs(title = "Función de probabilidade de masa, f(x)", x = "x (meses)", y = "P(X = x)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## 2. Función de distribución (acumulada) [F(x)]
## F(x) = P(X <= x).
prob_f_distrib <- cumsum(prob_f_prob_masa)
data_f_distrib <- data.frame(x = meses, y = prob_f_distrib)

ggplot(data_f_distrib, aes(x = x, y = y)) + 
  geom_step(color = "cyan4", size = 1.5) +
  labs(title = "Función de distribución, F(x)", x = "x (meses)", y = "P(X = x)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## 3. Función de supervivencia [S(x)]
## S(x) = P(X > x) = 1 - F(x).
prob_f_superv <- 1 - prob_f_distrib
data_f_superv <- data.frame(x = meses, y = prob_f_superv)

ggplot(data_f_superv, aes(x = x, y = y)) + 
  geom_step(color = "cyan4", size = 1.5) +
  labs(title = "Función de supervivencia, S(x)", x = "x (meses)", y = "P(X = x)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


## 4. Función de risco [h(x)]
## h(x) = f(x) / S(x).
prob_f_risco <- prob_f_prob_masa / prob_f_superv
prob_f_risco[is.nan(prob_f_risco) | is.infinite(prob_f_risco)] <- 0
data_f_risco <- data.frame(x = meses, y = prob_f_risco)

ggplot(data_f_risco, aes(x = x, y = y)) + 
  geom_point(color = "cyan4", size = 5) +
  geom_segment(aes(xend = x, yend = 0), color = "cyan4", size = 2) +
  labs(title = "Función de risco, h(x)", x = "x (meses)", y = "P(X = x)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



## 5. Función de risco acumulado [H(x)]
## H(x) = sum(h(t)[1:x]).
prob_f_risco_acumu <- cumsum(prob_f_risco)
data_f_risco_acumu <- data.frame(x = meses, y = prob_f_risco_acumu)

ggplot(data_f_risco_acumu, aes(x = x, y = y)) + 
  geom_step(color = "cyan4", size = 1.5) +
  labs(title = "Función de risco acumulado, H(x)", x = "x (meses)", y = "P(X = x)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
