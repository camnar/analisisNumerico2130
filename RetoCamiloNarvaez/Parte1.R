library(pracma)
library(readxl)

temp = read_excel("D:\\Users\\Diana\\Documents\\Universidad 2021-03\\Análisis Numérico\\Analisis-Numerico\\Reto2\\datos.xls",
                  sheet = "Itatira")
temp$`Dia Juliano`<-as.numeric(temp$`Dia Juliano`)
temp$Hora<-as.numeric(temp$Hora)
temp$Hora<-temp$Hora/10000

cantidadDatos<-length(temp$`Temp. Interna (ºC)`)

temp$DH <- temp$`Dia Juliano`+temp$Hora

x = temp$`DH`
y = temp$`Temp. Interna (ºC)`
#pdf("D:\\Users\\Diana\\Documents\\Universidad 2021-03\\Análisis Numérico\\Analisis-Numerico\\Reto2\\datos_iniciales.pdf")
plot(x, y, type = "l", main = "Datos iniciales", ylab = "Temperaturas", xlab = "Día Juliano")
#dev.off()

#Eliminar 20 % y rellenar datos

set.seed(6)

ones = rep(1, cantidadDatos)
eliminate = sample.int(cantidadDatos, cantidadDatos * 0.2)
for (e in eliminate) {
  ones[e] = 0
}

newX = c()
newY = c()

i = 1
j = 1

for (o in ones) {
  if (o == 1) {
    newX[i] = x[j]
    newY[i] = y[j]
    i = i + 1
  }
  j = j + 1
}

#PCHIP
xi <- seq(1, cantidadDatos, len = cantidadDatos)

datosPCHIP = pchip(newX, newY, xi)
datosPCHIP = round(datosPCHIP,3)
funcionDatosPCHIP = pchipfun(newX, newY)
#pdf("D:\\Users\\Diana\\Documents\\Universidad 2021-03\\Análisis Numérico\\Analisis-Numerico\\Reto2\\inicialesVPCHIP.pdf")
plot(x, y, type = "l", main = "Comparación PCHIP", ylab = "Temperaturas", xlab = "Día Juliano")
lines(datosPCHIP, col = "darkorchid1")
#dev.off()

#Spline
datosSplineCh = spline(newX, newY)
datosSpline <-as.numeric(unlist(datosSplineCh))
funcionDatosSpline = splinefun(newX, newY)
datosSpline = round(datosSpline,3)
#pdf("D:\\Users\\Diana\\Documents\\Universidad 2021-03\\Análisis Numérico\\Analisis-Numerico\\Reto2\\inicialesVspline.pdf")
plot(x, y, type = "l", main = "Comparación Spline", ylab = "Temperaturas", xlab = "Día Juliano")
lines(datosSplineCh, col = "chartreuse1")
#dev.off()

#Interpolación Lineal
datosLinealCh = approx(newX, newY, method = "linear", n = length(newX))
datosLineal <- as.numeric(unlist(datosLinealCh))
datosLineal = round(datosLineal,3)
funcionDatosLineal = approxfun(newX, newY)
#pdf("D:\\Users\\Diana\\Documents\\Universidad 2021-03\\Análisis Numérico\\Analisis-Numerico\\Reto2\\inicialesVaprox.pdf")
plot(x, y, type = "l", main = "Comparación Lineal", ylab = "Temperaturas", xlab = "Día Juliano ")
lines(datosLinealCh, col = "firebrick1")
#dev.off()

#Comparación tres métodos
#pdf("D:\\Users\\Diana\\Documents\\Universidad 2021-03\\Análisis Numérico\\Analisis-Numerico\\Reto2\\tresMétodos.pdf")
plot(x, y, type = "l", main = "Comparación con tres métodos", ylab = "Temperaturas", xlab = "Día Juliano")
lines(datosSplineCh, col = "chartreuse1")
lines(datosLinealCh, col = "firebrick1")
lines(datosPCHIP, col = "darkorchid1")
#dev.off()

#Adicionales
#datosBaryLag = barylag(newX, newY, xi)
#pdf("D:\\Users\\Diana\\Documents\\Universidad 2021-03\\Análisis Numérico\\Analisis-Numerico\\Reto2\\inicialesVbarylag.pdf")
#plot(x, y, type = "l", main = "Comparación Barycentric Lagrange", ylab = "Temperaturas", xlab = "Día Juliano")
#lines(datosBaryLag, col = "yellow")
#dev.off()

errorSpline = c()
errorInter = c()
errorPCHIP = c()

k = 1

for (var in x) {
  
  errorSpline[k] = round(abs((y[k] - funcionDatosSpline(var)) / y[k]), 3)
  errorPCHIP[k] = round(abs((y[k] - funcionDatosPCHIP(var)) / y[k]), 3)
  errorInter[k] = round(abs((y[k] - funcionDatosLineal(var)) / y[k]), 3)
  
  k = k + 1
}

cat("Con el uso de la funcioncion spline")
cat("Cantidad de errores: ", sum(errorSpline != 0))
cat("Error maximo: ", max(errorSpline))
cat("Error minimo: (para valores distintos de cero)", min(errorSpline[errorSpline > 0]))
cat("Error medio: (para valores distintos de cero)", median(errorSpline[errorSpline > 0]))
cat("Indice de Jaccard: ", round(sum(errorSpline == 0) / length(x), 3))

cat("Con el uso de la funcioncion approx")
cat("Cantidad de errores: ", sum(errorInter != 0))
cat("Error maximo: ", max(errorInter))
cat("Error minimo: (para valores distintos de cero)", min(errorInter[errorInter > 0]))
cat("Error medio: (para valores distintos de cero)", median(errorInter[errorInter > 0]))
cat("Indice de Jaccard: ", round(sum(errorInter == 0) / length(x), 3))

cat("Con el uso de la funcioncion PCHIP")
cat("Cantidad de errores: ", sum(errorPCHIP != 0))
cat("Error maximo: ", max(errorPCHIP))
cat("Error minimo: (para valores distintos de cero)", min(errorPCHIP[errorPCHIP > 0]))
cat("Error medio: (para valores distintos de cero)", median(errorPCHIP[errorPCHIP > 0]))
cat("Indice de Jaccard: ", round(sum(errorPCHIP == 0) / length(x), 3))
