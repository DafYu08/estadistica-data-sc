
library("ggplot2")
library("aplpack")
library(MASS)

#Punto A
n <- 100
lambda <- 1
muestra <- rexp(n, rate = lambda)

empirica <- ecdf(muestra) # Distribución empírica

x <- seq(0, 5, length.out = 1000)# Valores de la acumulada
y_acumulada <- pexp(x, rate = lambda)# Verdadera función de distribución acumulada
y_empirica <- empirica(x)# Empirica

#Punto B

EMM_lambda <- 1/mean(muestra)
EMV_lambda <- 1/mean(muestra)

#Punto C
#Vamos a estimar la distribucion acumulada con la densidad y luego integramos

elegir_ventana_loocv <- function(muestra){
  ventanas <- c(0.25, 0.5, 0.75, 0.8, 1, 1.5, 2, 3, 4)
  error_minimo <- Inf
  ventana_optima <- 0
  
  for (h in ventanas) {
    error_cv <- 0
    for (i in 1:length(muestra)) {
      dato <- muestra[i]
      muestra_sin_dato <- muestra[-i]
      f_i <- 0
      
      for (resto in muestra_sin_dato) {
        if (-1 <= (dato - resto) / h && (dato - resto) / h <= 1) {
          f_i <- f_i + 0.5
        }
      }
      
      f_i <- f_i / ((length(muestra_sin_dato)) * h)
      error_cv <- error_cv - log(f_i)
    }
    
    error_cv <- error_cv / length(muestra)
    
    if (error_cv < error_minimo) {
      error_minimo <- error_cv
      ventana_optima <- h
    }
  }
  
  return(ventana_optima)
}

estimar_densidad_parzen <- function(muestra, h_optima) {
  densidad_estimada <- function(x) {
    suma <- 0
    for (dato in muestra) {
      suma <- suma + dnorm((x - dato) / h_optima, mean = 0, sd = 1)
    }
    return(suma / (length(muestra) * h_optima))
  }
  return(densidad_estimada)
}

ventana_optima <- elegir_ventana_loocv(muestra)
densidad_estimada <- estimar_densidad_parzen(muestra, ventana_optima)

FDA_estimada <- function(x) { # Función de distribucion acumulada
  integral <- integrate(densidad_estimada, lower = -Inf, upper = x)$value
  return(pmax(integral, 0))  
}

densidad_estimada_suavizada <- density(muestra, bw = "SJ")

x_densidad <- densidad_estimada_suavizada$x
y_densidad <- densidad_estimada_suavizada$y

FDA_estimada_suavizada <- function(x) { # Función de distribucion acumulada
  integral <- integrate(densidad_estimada, lower = -Inf, upper = x)$value
  return(pmax(integral, 0))  
}

plot(x, y_acumulada, type = "l", col = "orange", lwd = 2, ylim = c(0, 1),
     xlab = "Valor", ylab = "Probabilidad")
lines(x, y_empirica, type = "s", col = "violet", lwd = 2)
lines(x_vals, y_vals, type = "l", col = "green", lwd = 2)
lines(x_vals, FDA_estimada_suavizada(x), col = "red") #Consultar
legend("bottomright", legend = c("Acumulada", "Empirica", "Estimador de Parzen", "Con nucleo Epanechnikov"), col = c("orange", "violet", "green", "red"), lty = c(1, 1), lwd = c(2, 2))
points(muestra, empirica(muestra), col = "pink", pch = 15)

























