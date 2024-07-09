#---------------------PARTE A--------------------------
set.seed(2024)
ECM <- function(muestra, theta_0){
  return(var(muestra) + (mean(muestra) - theta_0)**2)
}

estimador_primer_momento <- function(muestra, theta_0){
  sum <- 0
  for(i in 1:length(muestra)){
    sum <- sum + (muestra[i] - theta_0)**2
  }
  return(sum/length(muestra))
}

demostracion_15V <- function(){
  rep = 1000
  for (n in c(6, 10, 20, 40, 80, 200)){
    replicaciones_EMV <- c()
    replicaciones_M <- c()
    replicaciones_mod <- c()
    for (x in 1:rep){
      muestra <- runif(n, min = 0, max = 3)
      theta_EMV <- max(muestra)
      replicaciones_EMV <- c(replicaciones_EMV, theta_EMV)
      
      theta_M <- 2*mean(muestra)
      replicaciones_M <- c(replicaciones_M, theta_M)
      
      theta_mod <- ((n+1)/n)* max(muestra)
      replicaciones_mod <- c(replicaciones_mod, theta_mod)
    }
    ECM_EMV <- ECM(replicaciones_EMV, 3)
    estimador_prim_mom_theta_EMV <- estimador_primer_momento(replicaciones_EMV, 3)
    ECM_M <- ECM(replicaciones_M, 3)
    estimador_prim_mom_theta_M <- estimador_primer_momento(replicaciones_M, 3)
    ECM_mod <- ECM(replicaciones_mod, 3)
    estimador_prim_mom_theta_mod <- estimador_primer_momento(replicaciones_mod, 3)
    return(c(abs(ECM_EMV - estimador_prim_mom_theta_EMV), abs(ECM_M - estimador_prim_mom_theta_M), abs(ECM_mod - estimador_prim_mom_theta_mod)))
  }
}

demostracion_15V() #Concluimos que son lo mismo con theta_0 conocido (dio: 0.0001384097 0.0005025220 0.0001883910)


#--------------PARTE B------------------------------

ejercicio_15ii <- function(){
  rep = 1000
  resultados_EMV <- c()
  resultados_M <- c()
  resultados_mod <- c()
  
  for (n in c(6, 10, 20, 40, 80, 200)){
    replicaciones_EMV <- c()
    replicaciones_M <- c()
    replicaciones_mod <- c()
    for (x in 1:rep){
      muestra <- runif(n, min = 0, max = 3)
      theta_EMV <- max(muestra)
      replicaciones_EMV <- c(replicaciones_EMV, theta_EMV)
      
      theta_M <- 2*mean(muestra)
      replicaciones_M <- c(replicaciones_M, theta_M)
      
      theta_mod <- ((n+1)/n)* max(muestra)
      replicaciones_mod <- c(replicaciones_mod, theta_mod)
    }
    ECM_EMV <- ECM(replicaciones_EMV, 3)
    ECM_M <- ECM(replicaciones_M, 3)
    ECM_mod <- ECM(replicaciones_mod, 3)
    resultados_EMV <- c(resultados_EMV, ECM_EMV)  # Cambio aquí
    resultados_M <- c(resultados_M, ECM_M)  # Cambio aquí
    resultados_mod <- c(resultados_mod, ECM_mod)  # Cambio aquí
  }
  resultados <- list(EMV = resultados_EMV, M = resultados_M, Modificado = resultados_mod)  # Cambio aquí
  return(resultados)
}

# Ejecutar la función
resultados <- ejercicio_15ii()

ECM_EMV <- resultados$EMV
ECM_M <- resultados$M
ECM_Mod <- resultados$Modificado

n = c(6, 10, 20, 40, 80, 200)
# Crear boxplots
par(mfrow = c(1,3))
boxplot(ECM_EMV, col = "blue", main = "ECM para Estimador EMV",
        ylab = "ECM", xlab = "Tamaño de muestra (n)", border = "black")
boxplot(ECM_M, col = "green", main = "ECM para Estimador M",
        ylab = "ECM", xlab = "Tamaño de muestra (n)", border = "black")
boxplot(ECM_Mod, col = "red", main = "ECM para Estimador Modificado",
        ylab = "ECM", xlab = "Tamaño de muestra (n)", border = "black")
