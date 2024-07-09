
'
#Genero datos
set.seed(2023)
rep <- 1000

error_cuadratico_medio <- function(estimador){
  return((estimador - 4)**2)
}

muestras <- c()
est1 <- c()
est2 <- c()
error1 <- 0
error2 <- 0
for (i in 1:rep) {
  muestra_exp <- rexp(20, 0.5)
  muestras <- c(muestras, muestra_exp)
  t1 <- mean(muestra_exp)^2  
  est1 <- c(est1, t1)
  t2 <- sd(muestra_exp)^2 
  est2 <- c(est2, t2)
  error1 <- error1 + error_cuadratico_medio(t1)
  error2 <- error2 + error_cuadratico_medio(t2)
}

#Ejercicio c
#Armo boxplots paralelos
par(mfrow=c(1,2))
boxplot(est1, col="blue", main = "Primer estimador")
boxplot(est2, col = "green", main = "Segundo estimador")

#Ejercicio d

#Funciones para elegir ventana
nucleo_gaussiano <- function(u){
  kernel <- exp(-(u^2)/2)/sqrt(2*pi)
  return(kernel)
}

f_sombrero <- function(muestra, kernel, datos, h){ 
  suma <- 0
  for(i in 1:length(datos)){
    actual <- kernel((muestra-datos[i])/h)
    suma <- suma + actual
  }
  f <- suma / (length(datos) * h)
  return(f)
}

# Ventana de Silverman (al final no lo usé porque son exponenciales)
h_Silverman <- 1.06 * min(sd(muestras), IQR(muestras) / 1.349) * length(muestras) ^ (-1/5)

# Venta usando Convalidación Cruzada
elegir_ventana_CV <- function(muestras){
  h <- c(8, 10, 0.001)
  vector_1 <- c()
  for(j in 1:length(h)){
    vector_2 <- c()
    for(i in 1:length(muestras)){
      f_actual <- f_sombrero(muestras[i], nucleo_gaussiano, muestras[-i], h[j])
      vector_2[i] <- log(f_actual)
    }
    vector_1[j] <- mean(na.omit(vector_2))
  }
  h_CV <- h[which.max(vector_1)]
  return(h_CV)
}

par(mfrow=c(1,1))
# Utilizando la ventana estimada_2 para las densidades 1 y 2
densidad_t1 <- density(est1, kernel = "gauss", window = bw.ucv(est1))
densidad_t2 <- density(est2, kernel = "gauss", window = bw.ucv(est2))
plot(densidad_t2, col = "violet")
lines(densidad_t1, col = "orange")
legend("topright", legend = c("densidad primer estimador varianza", "densidad segundo estimador varianza"), col = c("orange", "violet"), lty = c(1, 1), lwd = c(2, 2))

#Ejercicio e

error1 <- (1/rep) * error1
error1
error2 <- (1/rep) * error2
error2
'



error_cuadratico_medio <- function(estimador){
  return((estimador - 4)**2)
}

set.seed(2023)
n = 20
Nrep = 1000

estimador1 <-  c()
estimador2 <- c()
error_est_1 <- 0
error_est_2 <- 0

for (i in 1:Nrep) {
  muestra <-  rexp(n, 0.5)
  est1 <- mean(muestra)^2
  est2 <- sd(muestra)^2
  
  estimador1 <- c(estimador1, est1)
  estimador2 <- c(estimador2, est2)
  
  error_est_1 <-  error_est_1 + error_cuadratico_medio(est1)
  error_est_2 <- error_est_2 + error_cuadratico_medio(est2)
}

par(mfrow = c(1, 2))
boxplot(estimador1, col = "darkorange", main = "Estimador de la varianza T1")
boxplot(estimador2, col = "purple", main = "Estimador de la varianza T2")

par(mfrow = c(1,1))
densidad_est1 <- density(estimador1, kernel = "gauss", window = bw.ucv)
densidad_est2 <- density(estimador2, kernel = "gauss", window = bw.ucv)

plot(densidad_est1,col = "darkorange", main = "Estimador no paramétrico de la densidad", xlab = "x", lwd = 3,)
lines(densidad_est2, col = "purple", lwd = 3)

error_est_1 <-error_est_1 * (1/Nrep)
error_est_1

error_est_2 <- error_est_2 * (1/Nrep)
error_est_2













































