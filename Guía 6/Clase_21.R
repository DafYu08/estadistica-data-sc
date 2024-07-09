setwd("~/Estadística/Guía 6")
datos <- read.table("bajoPeso.txt",header=TRUE)
head(datos)

plot(datos$apgar5, datos$presSist,
     xlab = "Apgar",
     ylab = "Presión Sistólica",
     main = "Diagrama de Dispersión: Apgar vs Presión Sistólica")

plot(datos$edadG, datos$presSist,
     xlab = "Edad",
     ylab = "Presión Sistólica",
     main = "Diagrama de Dispersión: Edad vs Presión Sistólica")

modelo_1 <- lm(presSist ~ edadG + apgar5, data = datos)
summary(modelo_1)

x_0 = c(1,31,7)
prediccion_estimada <- sum(modelo_1$coefficients*x_0)
prediccion_estimada

x_1 = c(1,32,8)
prediccion_estimada <- sum(modelo_1$coefficients*x_1)
prediccion_estimada

X <- cbind(rep(1,100),datos$edadG, datos$apgar5)
n <- nrow(X)
p <- ncol(X)

S <- summary(modelo_1)$sigma
aux <- qt(0.975, n-p)*S*sqrt(x_0%*%solve(t(X)%*%X)%*%x_0)

lim_inferior <- prediccion_estimada-aux[1]
lim_superior <- prediccion_estimada+aux[1]

c(lim_inferior ,lim_superior)

nuevo_dato <- data.frame(edadG = 31, apgar5 = 7)
prediccion <- predict(modelo_1, newdata = nuevo_dato, interval = "confidence", level = 0.95)
prediccion

Xt.X<- t(model.matrix(modelo_1))%*%model.matrix(modelo_1)
solve(Xt.X)

A<-matrix(rep(0,6), nrow = 2, ncol = 3)
A[1,2]=A[2,3]=1
A

betas_estimados<- modelo_1$coefficients
beta_estrella <- A%*%betas_estimados
CovMat<- (summary(modelo_1)$sigma^2)*A%*%solve(Xt.X)%*%t(A)
CovMat

t(beta_estrella)%*%(solve(CovMat))%*%(beta_estrella)/2

summary(modelo_1)

X <- cbind(rep(1,100), datos$edadG, datos$ apgar5) # X es la matriz de diseño
n <- nrow(X)
p <- ncol(X)

matriz_Covarianza <- summary(modelo_1)$sigma^2*solve(t(X)%*%X)

lim_inferior <- -qt(0.975,n-p)*sqrt(matriz_Covarianza[2,2]) + modelo_1$coefficients[2]
lim_superior <-  qt(0.975,n-p)*sqrt(matriz_Covarianza[2,2]) + modelo_1$coefficients[2]
c(lim_inferior ,lim_superior)

confint(modelo_1, "edadG", level = 0.95)


