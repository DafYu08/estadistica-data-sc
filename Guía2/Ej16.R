library("ggplot2")
library("aplpack")
library("MASS")

#Ejercicio A
df <- read.table("tormenta.txt")
datos <- df$V1
primer_mom <- mean(datos)
segundo_mom <- mean(datos^2)
est_mom_alpha <- (primer_mom^2)/(segundo_mom - primer_mom)
est_mom_lambda <- primer_mom/(segundo_mom - primer_mom)

est_mv <- fitdistr(datos, densfun = "gamma")

est_mv_alpha <- as.numeric(est_mv$estimate["shape"])
est_mv_lambda <- as.numeric(est_mv$estimate["rate"])

#Ejercicio B

#Con empírica
valor_observado <- length(datos[datos < 20]) / nrow(df)
plot(ecdf(datos))

