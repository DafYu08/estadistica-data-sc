set.seed(2024)
dias <- c(747, 766, 749, 741, 714, 659, 624)
n <- 5000
theta_0 <- 1/7

res_test <- c()
z_values <- c()
p_values <- c()

for(i in 1:length(dias)) {
  proporción_observada <- dias[i] / n
  z_value <- (proporción_observada - theta_0) / sqrt((theta_0 * (1 - theta_0)) / n)
  z_values <- c(z_values, z_value)
  
  p_value <- 2 * (1 - pnorm(abs(z_value)))
  p_values <- c(p_values, p_value)
  
  if (p_value < 0.05) {
    res_test <- c(res_test, 1)  # Rechazar H0
  } else {
    res_test <- c(res_test, 0)  # No rechazar H0
  }
}

print(res_test)
print(p_values)

#Para el inciso c, nos preguntan cuál es el nivel de significación que tiene que tener el test phi para que rechace
#si alguno de los días da que rechazamos
nivel_significacion_phi <- min(p_values)
nivel_significacion_phi
