#¿Cómo se distribuye el p-valor?

set.seed(2024)
n <- 25
p_valores <- c()

for(i in 1:1000){
  datita <- rnorm(n,10,3)
  testeo <- t.test(datita, y = NULL,
                   alternative = c("two.sided"),
                   mu = 10,
                   conf.level = 0.95)
  p_valores <- c(p_valores, testeo$p.value)
}

hist(p_valores, col = 'darkgreen', freq = TRUE)
#Pareciera que el p-valor se distribuye como una uniforme

# Estimar la probabilidad de rechazar H0 para alpha = 0.05 y alpha = 0.10
prob_rechazo_05 <- mean(p_valores < 0.05)
prob_rechazo_10 <- mean(p_valores < 0.10)

prob_rechazo_05
prob_rechazo_10

'''
Aunque todas las muestras se generan bajo H0, la probabilidad de rechazar la hipótesis nula 
es aproximadamente igual al nivel de significancia 𝛼. Esto es porque los p-valores bajo H0 
se distribuyen uniformemente entre 0 y 1. Por lo tanto, la proporción de p-valores menores a 
𝛼debería ser aproximadamente 𝛼.
'''