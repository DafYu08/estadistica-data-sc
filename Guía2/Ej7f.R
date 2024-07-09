
datos_norm <- c(1.53, 1.65, 1.72, 1.83, 1.62, 1.75, 1.72, 1.68, 1.65, 1.61, 1.70, 1.60, 1.73, 1.61, 1.52, 1.81, 1.72, 1.50, 1.51, 1.65, 1.58, 1.82, 1.65, 1.72, 1.65)

estimador_mu <- mean(datos_norm)
estimador_sigma <- sd(datos_norm)

estimador_sigma_manual <- function(datos_norm){
  suma <- 0
  media <- mean(datos_norm)
  for (d in datos_norm) {
    suma <- suma + (d - media)**2
  }
  suma <- suma/length(datos_norm)
  suma <- sqrt(suma)
  return(suma)
}
estimador_sigma_manual(datos_norm)

#Queremos la proba de tener menos de 1.73 de colesterol P(x <= 1.73)

proba_menor_173_manual <-function(datos_norm){
  proba <- 0
  for (d in datos_norm) {
    if (d <= 1.73){
      proba <- proba + 1/length(datos_norm)
    }
  }
  return(proba)
}
proba_menor_173_manual(datos_norm)

# Con pnorm()
normalizado <- (1.73 - estimador_mu) / estimador_sigma
proba <- pnorm(normalizado)
proba


####

nivel_confianza <- 0.90
grados_libertad <- length(datos_norm) - 1

# Calcular los cuantiles de la distribución chi-cuadrado
# Cuantil superior
chi_superior <- qchisq(1 - (1 - nivel_confianza) / 2, df = grados_libertad)

# Cuantil inferior
chi_inferior <- qchisq((1 - nivel_confianza) / 2, df = grados_libertad)

# Mostrar los resultados
print(paste("Cuantil superior de chi-cuadrado:", chi_superior))
print(paste("Cuantil inferior de chi-cuadrado:", chi_inferior))

