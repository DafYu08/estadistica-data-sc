quad <- function(a, b, c) {
  if (a == 0) {
    if (b == 0) {
      if (c == 0) {
        return("Infinitas soluciones")
      } else {
        return("No tiene solución")
      }
    } else {
      return(-c / b)
    }
  }
  
  discriminante <- b^2 - 4 * a * c
  if (is.complex(discriminante) || is.na(discriminante)) {
    return("Raíces complejas")
  }
  
  raiz1 <- (-b + sqrt(discriminante)) / (2 * a)
  raiz2 <- (-b - sqrt(discriminante)) / (2 * a)
  
  if (identical(raiz1, raiz2)) {
    return(raiz1)
  } else {
    return(c(raiz1, raiz2))
  }
}

metodo1 <- function(muestra){
  aux <- sqrt((mean(muestra) * (1 - mean(muestra))) / length(muestra))
  inferior <- mean(muestra + qnorm(0.025) * aux)
  superior <- mean(muestra + qnorm(0.975) * aux)
  return(c(inferior, superior))
}

metodo2 <- function(muestra, n){
  a = (qnorm(0.025)^2 + n)
  b = 2*n*mean(muestra) + qnorm(0.025)^2
  c = n * mean(muestra)^2
  raices <- quad(a, b, c)
  return(raices)
}


k <- 100
n <- 100
p <- 0.4
binomiales <- c()
int_de_confianza_metodo1 <- c()
int_de_confianza_metodo2 <- c()

for (i in 1:k){
  binomial <- rbinom(n, 1, p)
  binomiales[[i]] <- binomial
  
  longitud1 <- metodo1res(binomial)[2] - metodo1res(binomial)[1]
  
  int_de_confianza_metodo1 <- c(int_de_confianza_metodo1, metodo1(binomial))
  int_de_confianza_metodo2 <- c(int_de_confianza_metodo2, metodo2(binomial, n))
}  












