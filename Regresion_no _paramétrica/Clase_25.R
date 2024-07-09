set.seed(2024)
setwd("~/Estadística/Regresion_no _paramétrica")
datos <- read.table("lidar.txt", header = TRUE)

range <- datos$range
logratio <- datos$logratio

#Para núcleo normal
ventanas <- c(5, 10, 30, 50)
colores <- c("red", "darkorange", "darkgreen", "darkblue")
plot(range, logratio, col = "black", xlab = "Range", ylab = "Logratio", main = "Estimador de Nadaraya-Watson con núcleo normal para 
     distintas ventanas")
for (i in 1:length(ventanas)){
  estimador_nadayara_watson <- ksmooth(range, logratio, kernel = "normal", bandwidth = ventanas[i])
  lines(estimador_nadayara_watson$x, estimador_nadayara_watson$y, col = colores[i], lwd = 2)
}

#núcelo rectangular
ventanas <- c(5, 10, 30, 50)
colores <- c("red", "darkorange", "darkgreen", "darkblue")
plot(range, logratio, col = "black", xlab = "Range", ylab = "Logratio", main = "Estimador de Nadaraya-Watson con núcleo rectangular para 
     distintas ventanas")
for (i in 1:length(ventanas)){
  estimador_nadayara_watson <- ksmooth(range, logratio, kernel = "box", bandwidth = ventanas[i])
  lines(estimador_nadayara_watson$x, estimador_nadayara_watson$y, col = colores[i], lwd = 2)
}

#parte de cross validation
cv_error <- function(h, x, y) {
  cv <- rep(0, length(y))
  for (i in 1: length(y)) {
    xi <- x[-i]
    yi <- y[-i]
    m_estimado <- ksmooth(xi, yi, kernel = "normal", bandwidth = h, x.points = x[i])
    cv[i] <- (y[i] - m_estimado$y)^2
  }
  return(mean(cv))
}

h_optimos <- seq(3, 165, by = 1)
valores_del_error_h_optimo <- sapply(h_optimos, cv_error, x = range, y = logratio)
h_optimo <- h_optimos[which.min(valores_del_error_h_optimo)]

plot(h_optimos, valores_del_error_h_optimo, type = "l", col = "blue", lwd = 2,
     xlab = "h (Ventana)", ylab = "CV(h)", main = "Validación cruzada para seleccionar h óptimo")
abline(v = h_optimo, col = "red", lty = 2)
print(paste("La ventana óptima es:", h_optimo))



