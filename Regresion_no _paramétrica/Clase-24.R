library(glmnet)

# Cargar el dataframe desde un archivo CSV
df <- read.csv("rdpercentGDP.csv")

# Crear el gráfico
plot(df$year, df$argen, type = "l", ylim = c(0, 5), col = "green", 
     xlab = "Año", ylab = "Valores", main = "Comparación de Países a lo Largo de los Años")

# Lista de países y colores
countries <- c("argen", "germany", "france", "china", "japan", "usa", "uk", "nld", "finland")
colors <- c("green", "red", "purple", "blue", "orange", "darkgreen", "brown", "pink", "yellow")
names <- c("Argentina", "Alemania", "Francia", "China", "Japón", "USA", "Reino Unido", "Países Bajos", "Finland")

# Añadir las líneas de cada país y el texto correspondiente
for (i in seq_along(countries)) {
  lines(df$year, df[[countries[i]]], col = colors[i])
  text(df$year[length(df$year)], df[[countries[i]]][length(df[[countries[i]]])], 
       names[i], pos = 3, col = colors[i])
}

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(2024)
train_indices <- sample(seq_len(nrow(df)), size = 0.8 * nrow(df))

train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Verificar la división
print("Conjunto de entrenamiento:")
print(train_data)

print("Conjunto de prueba:")
print(test_data)

# Crear el modelo de regresión lineal
modelo <- lm(usa ~ germany + france + china + japan + uk + nld + finland, data = train_data)
summary(modelo)

# Calcular el error cuadrático medio en los datos de prueba
predicciones <- predict(modelo, newdata = test_data)
ECM <- mean((test_data$usa - predicciones)^2)
print(ECM)

# Analizar la correlación entre las variables
print(cor(train_data[ , -c(1, 2, 8, 10)]))

'RIDGE'
matriz_diseño <- as.matrix(train_data[ , -c(1, 2, 8, 10)])

ajuste.ridge <- glmnet(matriz_diseño, train_data$usa, alpha = 0)
plot(ajuste.ridge)

# RIDGE con CV
lambda_grid <- 10^seq(-4, 4, length = 100)
cv.out <- cv.glmnet(matriz_diseño, train_data$usa, alpha = 0, lambda = lambda_grid, nfolds = 3)
plot(cv.out)

bestlam <- cv.out$lambda.min
print(log(bestlam))
print(bestlam)

# Coeficientes del modelo con el mejor lambda
print(coef(cv.out, s = bestlam))

# Predicciones en el conjunto de prueba y cálculo del ECM
matriz_diseño_test <- as.matrix(test_data[ , -c(1, 2, 8, 10)])
ridge.pred <- predict(cv.out, s = bestlam, newx = matriz_diseño_test)
ECM_ridge <- mean((ridge.pred - test_data$usa)^2)
print(ECM_ridge)

# Criterio de 1SE
lambda_1se <- cv.out$lambda.1se
print(log(lambda_1se))

# Coeficientes del modelo con el lambda de 1SE
print(coef(cv.out, s = lambda_1se))

# Predicciones en el conjunto de prueba y cálculo del ECM con lambda de 1SE
ridge.pred_1se <- predict(cv.out, s = lambda_1se, newx = matriz_diseño_test)
ECM_ridge_1se <- mean((ridge.pred_1se - test_data$usa)^2)

'LASSO'
matriz_diseño <- as.matrix(train_data[ , -c(1, 2, 8, 10)])

ajuste.lasso <- glmnet(matriz_diseño, train_data$usa, alpha = 1)
plot(ajuste.lasso)

# RIDGE con CV
lambda_grid <- 10^seq(-4, 4, length = 100)
cv.out <- cv.glmnet(matriz_diseño, train_data$usa, alpha = 1, lambda = lambda_grid, nfolds = 3)
plot(cv.out)

bestlam <- cv.out$lambda.min
print(log(bestlam))
print(bestlam)

# Coeficientes del modelo con el mejor lambda
print(coef(cv.out, s = bestlam))

# Predicciones en el conjunto de prueba y cálculo del ECM
matriz_diseño_test <- as.matrix(test_data[ , -c(1, 2, 8, 10)])
lasso.pred <- predict(cv.out, s = bestlam, newx = matriz_diseño_test)
ECM_lasso <- mean((lasso.pred - test_data$usa)^2)
print(ECM_lasso)

# Criterio de 1SE
lambda_1se <- cv.out$lambda.1se
print(log(lambda_1se))

# Coeficientes del modelo con el lambda de 1SE
print(coef(cv.out, s = lambda_1se))

# Predicciones en el conjunto de prueba y cálculo del ECM con lambda de 1SE
lasso.pred_1se <- predict(cv.out, s = lambda_1se, newx = matriz_diseño_test)
ECM_lasso_1se <- mean((lasso.pred_1se - test_data$usa)^2)



print(ECM_ridge_1se)
print(ECM_lasso_1se)

'Podemos observar que el modelo con LASSO tiene muchos más coeficientes que son cero'

'Conclusiones:
En principio, cuando hacemos el análisis Naive, podemos observar que los valores que arroja el modelo lineal sin penalización
son que Alemania, China, Japón, uk, y netherlands son los países que a nivel 0.05 tienen importancia. Ya con este ajuste,
tenemos un valor de R2 de 0.98, lo que nos dice que este modelo explica muy bien, aunque podríamos tener redundancias.
El ECM del modelo dio 0.004.

Cuando vemos la matriz de correlación, podemos ver que la correlación entre Alemania y China es cercana a 0.98. Esto podría
significar que la independencia entre ambas variables es débil (tienen una relación casi lineal), por lo que quizás cuando 
tengamos un modelo que penalice tenermuchos parámetros, tal vez elimine a una de las dos (o podríamos elegir al azar a mano).

Con regresión de Ridge, observamos que la mayoría de los coeficientes no son cero (usa todos los parámetros en el mdoelo).
El lambda que elige es 0.00021 y el ECM es 0.00338.

Con penalización LASSO, a partir de log(lambda)>2, empezamos a tener 0 coeficientes. Por lo que elige bestlam = 0.00053, 
donde se queda con 7 coeficientes de 9 y el ECM es 0.02, que es menor que el anterior, por lo que podríamos quedarnos con
este en lugar del anterior en este punto.

Ahora, podríamos usar el criterio que marcó Ana de usar el error stándar para tomar una decisión: el error 1se en el caso de 
Ridge es de 0.0015 y el de LASSO es 0.0033, el doble de desvío.

Depende qué prioricemos para elegir el modelo: si queremos menor varianza, Ridge, y si queremos menos coeficientes, LASSO.'


