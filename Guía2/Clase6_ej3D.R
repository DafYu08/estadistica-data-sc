library(MASS)

log_verosimilitud_gamma <- function(params, datos) {
  alpha <- params[1]
  lambda <- params[2]
  n <- length(datos)
  
  log_verosimilitud <- n * (alpha * log(lambda) - lgamma(alpha)) + 
    (alpha - 1) * sum(log(datos)) - lambda * sum(datos)
  
  return(log_verosimilitud)  # Retorno sin negativo para maximize
}

derivada_parcial_alpha <- function(alpha, lambda, datos, n) {
  return(n * (digamma(alpha) - log(lambda)) + sum(log(datos)))
}

derivada_parcial_lambda <- function(alpha, lambda, datos, n) {
  return(-sum(datos) + n * alpha / lambda)
}

newton_raphson <- function(datos, i) {
  n <- length(datos)  # Obtener n
  alpha_mom <- mean(datos)^2 / var(datos)
  lambda_mom <- mean(datos) / var(datos)
  
  alpha <- alpha_mom
  lambda <- lambda_mom
  
  for (iter in 1:i) {
    d_alpha_val <- derivada_parcial_alpha(alpha, lambda, datos, n)
    d_lambda_val <- derivada_parcial_lambda(alpha, lambda, datos, n)
    
    incremento_alpha <- d_alpha_val / optimize(function(x) log_verosimilitud_gamma(c(x, lambda), datos), interval = c(0.001, 100))$objective
    incremento_lambda <- d_lambda_val / optimize(function(x) log_verosimilitud_gamma(c(alpha, x), datos), interval = c(0.001, 100))$objective
    
    alpha <- alpha - incremento_alpha
    lambda <- lambda - incremento_lambda
  }
  
  return(c(alpha, lambda))
}
'
# Prueba con diferentes tamaños de muestra y repetir 10 veces
set.seed(456)
tamaños_muestra <- c(6, 10, 20, 40, 80, 200)
shape <- 3
rate <- 4
reps <- 5

# Vectores para almacenar los ECM
ECM_alpha_momentos <- c()
ECM_alpha_MV <- c()
ECM_lambda_momentos <- c()
ECM_lambda_MV <- c()

for (n in tamaños_muestra) {
  error_cuadratico_medio_alpha_momentos <- 0
  error_cuadratico_medio_alpha_MV <- 0
  error_cuadratico_medio_lambda_momentos <- 0
  error_cuadratico_medio_lambda_MV <- 0
  
  for (k in 1:reps) {
    datos <- rgamma(n, shape = shape, scale = 1/rate)
    
    alpha_momentos <- mean(datos)^2 / var(datos)
    lambda_momentos <- mean(datos) / var(datos)
    
    estimador_MV <- newton_raphson(datos, i = 10)
    
    error_cuadratico_medio_alpha_momentos <- error_cuadratico_medio_alpha_momentos + ((alpha_momentos - shape)^2)/reps
    error_cuadratico_medio_alpha_MV <- error_cuadratico_medio_alpha_MV + ((rate - estimador_MV[1])^2)/reps
    error_cuadratico_medio_lambda_momentos <- error_cuadratico_medio_lambda_momentos + ((shape - lambda_momentos)^2)/reps
    error_cuadratico_medio_lambda_MV <- error_cuadratico_medio_lambda_MV + ((rate - estimador_MV[2])^2)/reps
  }
  
  # Guardar los ECM en los vectores
  ECM_alpha_momentos <- c(ECM_alpha_momentos, error_cuadratico_medio_alpha_momentos)
  ECM_alpha_MV <- c(ECM_alpha_MV, error_cuadratico_medio_alpha_MV)
  ECM_lambda_momentos <- c(ECM_lambda_momentos, error_cuadratico_medio_lambda_momentos)
  ECM_lambda_MV <- c(ECM_lambda_MV, error_cuadratico_medio_lambda_MV)
}

# Graficar los ECM
plot(tamaños_muestra, ECM_alpha_momentos, type = "b", col = "blue", xlab = "Tamaño de muestra", ylab = "ECM", main = "ECM para estimador de momentos (alpha)")
points(tamaños_muestra, ECM_alpha_MV, type = "b", col = "red")
points(tamaños_muestra, ECM_lambda_momentos, type = "b", col = "green")
points(tamaños_muestra, ECM_lambda_MV, type = "b", col = "purple")
legend("topright", legend = c("ECM_alpha_momentos", "ECM_alpha_MV", "ECM_lambda_momentos", "ECM_lambda_MV"), col = c("blue", "red", "green", "purple"), pch = c(1, 1, 1, 1))
'



