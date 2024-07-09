library(ISLR)
library(plotmo)
library(glmnet)

#Cargo datos
names(Hitters) 
dim(Hitters) #tenemos 322 obs y 20 parámetros

#Chequeo missings
sum(is.na(Hitters$Salary)) #hay 59 missings

Hitters <- na.omit(Hitters)
sum(is.na(Hitters$Salary))
dim(Hitters)

#Ajustamos (Naive) con mínimos cuadrados utilizando todas las covariables
summary(lm(Salary ~ ., Hitters)) #El ajuste es bastante malo porque tiene poco R2

'Podríamos usar el método stepwise (forward o backward) pero en este caso uso LASSO'

'LASSO'
#Creamos la muestra de entrenamiento y validación
set.seed(1)
train <- sample(c(TRUE, FALSE) , nrow(Hitters), rep = TRUE)
test <- (!train)

#Creamos la matriz de diseño y el vector de respuestas
x <- model.matrix(Salary ~ .,Hitters[-1])
y <-  Hitters$Salary
y.test = y[test]

#Calculo LASSO
grid <- 10^seq(10, -2, length = 100)
lasso.mod = glmnet(x[train , ], y[train], alpha = 1, lambda = grid) #por default estandariza variables

#Veo los coeficiones
dim(coef(lasso.mod))
lasso.mod$lambda

#Inspeccionamos para distintos valores de lambda

lasso.mod$lambda[90]
coef(lasso.mod)[,90]

lasso.mod$lambda[70]
coef(lasso.mod)[,70]

lasso.mod$lambda[30]
coef(lasso.mod)[,30]

plot(lasso.mod)

plot(lasso.mod, label=T, xvar = "lambda")
'para valores de lambda cuyo logaritmo es mayor a 5, todos sus coeficientes son practicamente cero'

#Vamos a intentar encarar el problema usando convalidación cruzada

x2 <- model.matrix(Salary ~ .,Hitters[-1])
y2 <-  Hitters$Salary
y2.test = y2[test]
set.seed(1)
cv.out <- cv.glmnet(x2[train, ], y2[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
log(bestlam)
bestlam

#Miro cuánto vale el MSE en la muestra de validación
coef(lasso.mod, s= bestlam)
lasso.pred = predict(lasso.mod, s=bestlam, newx = x2[test,])
mean((lasso.pred - y.test)^2)

#También tenemos el log de la regla 1 desvío stándar
log(cv.out$lambda.1se)
coef(lasso.mod, s = cv.out$lambda.1se)
lasso.pred = predict(lasso.mod, s= cv.out$lambda.1se, newx = x2[test,])
mean((lasso.pred - y.test)^2)

'Si usamos la regla del desvío, tenemos un estimador mucho más parsimonioso'

'RIDGE (basandonos en la norma 2)'

grid <- 10^seq(10, -2, length = 100)
lasso.mod = glmnet(x[train , ], y[train], alpha = 0, lambda = grid) #por default estandariza variables0
plot(lasso.mod)
plot(lasso.mod, label=T, xvar = "lambda")
'Hay un shrinkage pero los coeficientes son distintos de cero'

set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
log(bestlam)
bestlam
coef(lasso.mod, s= bestlam)
lasso.pred = predict(lasso.mod, s=bestlam, newx = x2[test,])
mean((lasso.pred - y.test)^2)








