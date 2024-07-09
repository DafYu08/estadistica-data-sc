
'Ejercicio: Usando aproximación normal, graficar en R las funciones de potencia aproximada de
ambos tests en un mismo gráfico para n = 50 y p0 = 0,3. ¿Difieren mucho? ¿Con cuál
se quedaría?
'
# Parámetros del test
n <- 50
p0 <- 0.3
alpha <- 0.05  # Nivel de significancia

# Generamos una muestra de tamaño n de una distribución binomial Bi(1, p)
# Por ejemplo, con una proporción verdadera p = 0.35
set.seed(2024)
p_true <- 0.35
muestra <- rbinom(n, size = 1, prob = p_true)
p_EMV <- mean(muestra)

# Test 9a
estadistico_a <- (p_EMV - p0) / sqrt(p0 * (1 - p0) / n)
z_alpha <- qnorm(1 - alpha / 2)

test_a <- if (abs(estadistico_a) > z_alpha) {
  "Rechazamos H0"
} else {
  "No rechazamos H0"
}

resultado_test_a <- list(p_EMV = p_EMV, estadistico_a = estadistico_a, z_alpha = z_alpha, test_a = test_a)
print(resultado_test_a)

# Test 9b
estadistico_b <- (p_EMV - p0) / sqrt(p_EMV * (1 - p_EMV) / n)

test_b <- if (abs(estadistico_b) > z_alpha) {
  "Rechazamos H0"
} else {
  "No rechazamos H0"
}

resultado_test_b <- list(p_EMV = p_EMV, estadistico_b = estadistico_b, z_alpha = z_alpha, test_b = test_b)
print(resultado_test_b)

# Función de Potencia (a)
p_alternativa <- seq(0, 1, length.out = 100)

# Calculamos la potencia del test para cada valor de p bajo H_a
potencia_a <- sapply(p_alternativa, function(p) {
  z_beta_pos <- (z_alpha - (p0 - p) / sqrt(p * (1 - p) / n))
  z_beta_neg <- (-z_alpha - (p0 - p) / sqrt(p * (1 - p) / n))
  1 - pnorm(z_beta_pos) + pnorm(z_beta_neg)
})

# Calculamos la potencia del test para cada valor de p bajo H_a
potencia_b <- sapply(p_alternativa, function(p) {
  z_beta_pos <- (z_alpha * sqrt(p0 * (1 - p0) / (p * (1 - p))) - (p0 - p) / sqrt(p * (1 - p) / n))
  z_beta_neg <- (-z_alpha * sqrt(p0 * (1 - p0) / (p * (1 - p))) - (p0 - p) / sqrt(p * (1 - p) / n))
  1 - pnorm(z_beta_pos) + pnorm(z_beta_neg)
})

# Graficamos la función de potencia
plot(p_alternativa, potencia_a, type = "l", col = "blue", lwd = 2,
     xlab = expression(p), ylab = "Potencia",
     main = "Función de Potencia del Test",
     ylim = c(0, 1))
lines(p_alternativa, potencia_b, type = "l", col = "red")

# Añadimos una línea horizontal en y = alpha para referencia
abline(h = alpha, col = "black", lty = 2)

# Añadimos líneas verticales en los mínimos
min_a <- p_alternativa[which.min(potencia_a)]
min_b <- p_alternativa[which.min(potencia_b)]

abline(v = min_a, col = "blue", lty = 2)
abline(v = min_b, col = "red", lty = 2)
# Añadimos una leyenda
legend("bottomright", legend = c("Potencia Test A", "Potencia Test B", expression(alpha), min_a, min_b),
       col = c("blue", "red", "black", "blue", "red"), lty = c(1, 1, 2, 2, 2), lwd = c(2, 2, 1, 1, 1))

"En este caso, las dos son muy parecidas pero la A se acerca un poco más a p0. Quizás porque en b tomamos doble límite y eso
hace que sea como una doble aproximación.

Para el ejercicio f, alcanzaría con hacer todo con un for en los valores p = c(0,03, 0,1, 0,2, 0,3, 0,4, 0,5, 0,6, 0,7, 0,8, 0,9, 0,97)"





# Parámetros
n <- 10
p0 <- 0.4
alpha <- 0.05

# Datos de la muestra
x <- 9
p_hat <- x / n

# Estadístico de prueba
z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)

# Valor crítico para un test unilateral
z_alpha <- qnorm(1 - alpha)

# Decisión del test
decision <- if (z > z_alpha) {
  "Rechazamos H0"
} else {
  "No rechazamos H0"
}

# Cálculo del p-valor
p_value <- 1 - pnorm(z)

# Resultados
list(
  p_hat = p_hat,
  z = z,
  z_alpha = z_alpha,
  decision = decision,
  p_value = p_value
)
