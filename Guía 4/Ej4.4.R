set.seed(2024)
dado <- scan("dado.txt")

# Ejercicio a
n <- length(dado)
probabilidad_par_original <- 0
for (i in 1:n) {
  if (dado[i] %% 2 == 0) {
    probabilidad_par_original <- probabilidad_par_original + 1/n
  }
}

B <- 5000
titas_bootstrap <- c()
for (i in 1:B) {
  muestra_boot <- sample(dado, n, replace = TRUE)
  tita_boot <- 0
  for (j in 1:n) {
    if (muestra_boot[j] %% 2 == 0) {
      tita_boot <- tita_boot + 1/n
    }
  }
  titas_bootstrap <- c(titas_bootstrap, tita_boot)
}

error_titas <- sd(titas_bootstrap)
hist(titas_bootstrap, col = "darkorange", probability = TRUE)
lines(density(titas_bootstrap, kernel = "gauss", window = bw.nrd0), lwd = 3, col = "purple")
abline(v = probabilidad_par_original, lwd = 5, col = "darkgreen")

legend("topright", legend = c("Distribución Bootstrap", "Densidad Estimada", "Probabilidad Original"), lwd = c(0, 3, 5), col = c("darkorange", "purple", "darkgreen"), fill = c("darkorange", NA, NA), title = "Leyenda")
'Sí, tiene forma acampanada con centro en la proba original, que es 0.48'

#Método 1

int_de_confianza_metodo1 <- function(sd, estimador, alpha){
  inferior <- mean(estimador) - qnorm(1 - alpha/2)*sd
  superior <- mean(estimador) + qnorm(1 - alpha/2)*sd
  return(c(inferior, superior))
}

int_de_confianza_metodo1(error_titas, titas_bootstrap, 1 - 0.95)
probabilidad_par_original

#Método 2: 

int_de_confianza_metodo2 <- function(estimador, alpha){
  ordenar_medias <- sort(estimador)
  inferior <- quantile(estimador, alpha/2)
  superior <- quantile(estimador, 1 - alpha/2)
  return(c(inferior, superior))
}

int_de_confianza_metodo2(titas_bootstrap, 1 - 0.95)
probabilidad_par_original

'No contradice el hecho de que el dado sea equilibrado porque se encuentra el 50% en el intervalo'

#Ejercicio b

probabilidad_cinco_original <- 0
for (i in 1:n) {
  if (dado[i] == 5) {
    probabilidad_cinco_original <- probabilidad_cinco_original + 1/n
  }
}

titas_bootstrap_b <- c()
for (i in 1:B) {
  muestra_boot_b <- sample(dado, n, replace = TRUE)
  tita_boot <- 0
  for (j in 1:n) {
    if (muestra_boot_b[j] == 5) {
      tita_boot <- tita_boot + 1/n
    }
  }
  titas_bootstrap_b <- c(titas_bootstrap_b, tita_boot)
}

error_titas_b <- sd(titas_bootstrap_b)
hist(titas_bootstrap_b, col = "pink", probability = TRUE)
lines(density(titas_bootstrap_b, kernel = "gauss", window = bw.ucv), lwd = 3, col = "purple")
abline(v = probabilidad_cinco_original, lwd = 5, col = "darkgreen")

legend("topright", legend = c("Distribución Bootstrap", "Densidad Estimada", "Probabilidad Original"), lwd = c(0, 3, 5), col = c("pink", "purple", "darkgreen"), fill = c("pink", NA, NA), title = "Leyenda")
'Sí, tiene forma acampanada con centro en la proba original, que es '

#Método 1
int_de_confianza_metodo1(error_titas_b, titas_bootstrap_b, 1 - 0.95)
probabilidad_cinco_original

#Método 2: 
int_de_confianza_metodo2(titas_bootstrap_b, 1 - 0.95)
probabilidad_cinco_original

'En este caso, como 1/6 no se encuentra en el intervalo de confianza (Ni siquiera se acerca en ninguno de los dos casos), podemos ver que con una confianza del
95%, el dado no está equilibrado.'