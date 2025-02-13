### Función Kaplan-Meier método do produto ###
kaplanmeier_produto <- function(conxunto_datos, times_name, deltas_name) {
  # Kaplan-Meier calculado polo método do produto
  ## Ordeamos o conxunto de datos en función do tempo
  conxunto_datos_ordeados <- conxunto_datos[order(conxunto_datos[[times_name]], -conxunto_datos[[deltas_name]]),]
  
  ## Calculamos o número de individuos en risco en cada tempo
  tempos_unicos <- unique(conxunto_datos_ordeados[[times_name]])
  existe_censura <- unique(conxunto_datos_ordeados[[times_name]][conxunto_datos_ordeados[[deltas_name]] == 1])
  funcion_supervivencia <- data.frame(
    t = c(0, existe_censura),
    s = c(1, rep(NA, length(existe_censura)))
  )
  for (i_ti in 1:length(existe_censura)) {
    ti <- existe_censura[i_ti]  # valor sen censura
    posicion_ti <- which(conxunto_datos_ordeados[[times_name]] == ti) # posición do valor sen censura
    ni <- length(conxunto_datos_ordeados[[times_name]]) - posicion_ti[1] + 1  # número de individuos en risco
    di <- sum(conxunto_datos_ordeados[[deltas_name]][posicion_ti])  # número de eventos
    oposto_qi <- 1 - di / ni  # probabilidade de non ter un evento
    funcion_supervivencia$s[i_ti + 1] <- funcion_supervivencia$s[i_ti] * oposto_qi
  }
  return(funcion_supervivencia)
}
