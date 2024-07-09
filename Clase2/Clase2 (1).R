library("ggplot2")
library("aplpack")

datos_bebes <- read.table("ENNyS_menorA2.txt", header = TRUE)
names(datos_bebes)
attach(datos_bebes)

#Ejercicio 1a: Lo hacemos con Sexo
class(Sexo) #Me tira character
Sexo <- as.factor(Sexo)
class(Sexo) #Chequeo que sea factor

tablaSexo <- table(Sexo)/sum(table(Sexo)) ## acá tenemos la tabla de frecuencias relativas
# Crear un vector de colores para cada categoría
colores <- c("blue", "pink")  # Puedes elegir los colores que desees

# Graficar el gráfico de barras con diferentes colores
barplot(tablaSexo, width = 0.5, col = colores, ylim = c(0, 0.6), ylab = "Frecuencia Relativa", xlab = "Sexo")
# Agregar leyenda
legend("topright", legend = levels(Sexo), fill = colores)

#Ejercicio 1b: Lo hacemos con Tipo de embarazo

class(Tipo_embarazo) #Me tira character
Tipo_embarazo <- as.factor(Tipo_embarazo)
class(Tipo_embarazo) #Chequeo que sea factor

tablaEmbarazo <- table(Tipo_embarazo)
barplot(tablaEmbarazo,width=0.02,xlim=c(0,0.15), ylim = c(0, 6000) ,col= colores, ylab = "Frecuencia", xlab = "Tipo_embarazo")
# Agregar leyenda
legend("topright", legend = levels(Tipo_embarazo), fill = colores)

#Ejercicio 2: Tabla de contingencias
contingencia <- table(Sexo,Tipo_embarazo)
contingencia

#Ejercicio 3A: Peso y Talla podrían ser normal...

hist(Perim_encef,probability = TRUE,main="Histograma de perìmetro encefálico")
hist(Edad,probability = TRUE,main="Histograma de Edad")
hist(Peso,probability = TRUE,main="Histograma de Peso")
hist(Talla,probability = TRUE,main="Histograma de Talla")

#Ejercicio 3b: boxplot de cada categoría

#Ejercicio 3E
?boxplot
boxplot(Edad) #El ~ significa explicado por
boxplot(Talla)

qqnorm(Talla)
qqline(Talla)

qqnorm(Edad)
qqline(Edad)

x <- rnorm(1000, mean = 10, sd=14)
qqnorm(x)
qqline(x)

#Ejercicio 4

bagplot(Peso, Perim_encef)

#Ejercicio 5
boxplot(Peso~Sexo)
boxplot(Perim_encef~Sexo)
boxplot(Talla~Sexo)
