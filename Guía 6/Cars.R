
autos = cars

plot(autos$speed, autos$dist,
     xlab = "Velocidad",
     ylab = "Distancia de frenado",
     main = "Diagrama de Dispersión")

modelo <- lm(dist ~ speed, data = autos)
summary(model)

coeficientes <- coef(modelo)
b0 <- coeficientes[1]
b1 <- coeficientes[2]
abline(modelo, col = "blue")

predicciones <- predict(modelo)
points(autos$speed, predicciones, col = "orange", pch = 19)

# Calcular los residuos del modelo
residuos <- residuals(modelo)

# Calcular el error cuadrático medio de los residuos
MSE <- sum(residuos^2) / modelo$df.residual
MSE

# Ajustar el modelo polinomial de segundo grado
modelo_polinomial <- lm(dist ~ speed + I(speed^2), data = autos)

# Obtener los valores predichos del modelo polinomial
predicciones_polinomial <- predict(modelo_polinomial)

# Graficar la curva polinomial sobre el gráfico realizado en el ítem b)
plot(autos$speed, autos$dist, xlab = "Velocidad (mph)", ylab = "Distancia de frenado (pies)", main = "Diagrama de dispersión de Velocidad vs Distancia de frenado")
abline(modelo, col = "blue")
points(autos$speed, predicciones, col = "orange", pch = 19)

# Graficar la curva polinomial en color rojo
lines(autos$speed, predicciones_polinomial, col = "red", lwd = 2)