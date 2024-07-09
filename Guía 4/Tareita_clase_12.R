library(MASS)
set.seed(2024)
B <-1000
n <-500
muestra_original <- rexp(n, rate = 1)
quantile(muestra_original, 0.5)
T_media_original <- mean(muestra_original)
T_mediana_original <- median(muestra_original)

estimador_mediana <- c()
estimador_media <- c()
for (i in 1:B) {
  Xboot <- sample(muestra_original, n, replace = TRUE)
  mean_boot <- mean(Xboot)
  median_boot <- median(Xboot)
  estimador_media <- c(estimador_media, mean_boot)
  estimador_mediana <- c(estimador_mediana, median_boot)
}

hist(estimador_media, freq = FALSE, col="darkorange" )
lines(density(estimador_media, kernel = "gauss", window = bw.ucv), col="purple", lwd = 3)

hist(estimador_mediana, freq = FALSE, col = "lightgreen")
lines(density(estimador_mediana, kernel = "epanechnikov", window = bw.ucv), col="darkblue", lwd = 3)

#Ahora, quiero calcular los I de C
#Mètodo 1: Intervalo Bootstrap noemL

int_de_confianza_metodo1 <- function(sd, estimador, alpha){
  inferior <- mean(estimador) - qnorm(1 - alpha/2)*sd
  superior <- mean(estimador) + qnorm(1 - alpha/2)*sd
  return(c(inferior, superior))
}

error_est_media <- sd(estimador_media)
error_est_mediana <- sd(estimador_mediana)
int_de_confianza_metodo1(error_est_media, estimador_media, 1 - 0.95)
T_media_original
int_de_confianza_metodo1(error_est_mediana, estimador_mediana, 1 - 0.95)
T_mediana_original

#Método 2: 

int_de_confianza_metodo2 <- function(estimador, alpha){
  ordenar_medias <- sort(estimador)
  inferior <- quantile(estimador, alpha/2)
  superior <- quantile(estimador, 1 - alpha/2)
  return(c(inferior, superior))
}

int_de_confianza_metodo2(estimador_media, 1 - 0.95)
T_media_original
int_de_confianza_metodo2(estimador_mediana, 1 - 0.95)
T_mediana_original







