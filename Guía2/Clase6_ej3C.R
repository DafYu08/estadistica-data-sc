library(MASS)

log_verosimilitud_gamma <- function(params, datos) {
  alpha <- params[1]
  lambda <- params[2]
  n <- length(datos)
  
  log_verosimilitud <- n * (alpha * log(lambda) - lgamma(alpha)) + 
    (alpha - 1) * sum(log(datos)) - lambda * sum(datos)
  
  return(-log_verosimilitud)  # Se retorna el negativo para maximizar
}


derivada_parcial_alpha <- function(alpha, lambda, datos) {
  n <- length(datos)
  return(n * (digamma(alpha) - log(lambda)) + sum(log(datos)))
}

derivada_parcial_lambda <- function(alpha, lambda, datos) {
  return(-sum(datos) + n * alpha / lambda)
}

newton_raphson <- function(datos, i) {
  #Arranco con los de momentos como X_0
  alpha_mom <- mean(datos)^2 / var(datos)
  lambda_mom <- mean(datos) / var(datos)
  
  alpha <- alpha_mom
  lambda <- lambda_mom
  
  for (iter in 1:i) {
    # Calcular derivadas
    d_alpha_val <- derivada_parcial_alpha(alpha, lambda, datos)
    d_lambda_val <- derivada_parcial_lambda(alpha, lambda, datos)
    
    # Calcular decremento de parámetros. Lo de optimize me lo sugirió chat-gpt para que queden mejor las divisiones
    decremento_alpha <- d_alpha_val / optimize(function(x) -log_verosimilitud_gamma(c(x, lambda), datos), interval = c(0.001, 100))$objective
    decremento_lambda <- d_lambda_val / optimize(function(x) -log_verosimilitud_gamma(c(alpha, x), datos), interval = c(0.001, 100))$objective
    
    alpha <- alpha - decremento_alpha
    lambda <- lambda - decremento_lambda
  }
  
  return(c(alpha, lambda))
}

# Pruebo con 1000 datos
set.seed(456)
n <- 1000
shape <- 3
rate <- 0.25
datos_gamma <- rgamma(n, shape = shape, scale = 1/rate)

# Estimar parámetros por Newton-Raphson con 10 iteraciones
estimacion <- newton_raphson(datos_gamma, i = 10)
estimacion

error_estimacion <- c(abs(estimacion[1] - shape), abs(estimacion[2] - rate))
error_estimacion























