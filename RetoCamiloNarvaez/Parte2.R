library(pracma)
library(readxl)

temp = read_excel("D:\\Users\\Diana\\Documents\\Universidad 2021-03\\Análisis Numérico\\Analisis-Numerico\\Reto2\\datos.xls",
                  sheet = "Itatira")

cantidadDatos<-length(temp$`Temp. Interna (ºC)`)

temp$`Dia Juliano`<-as.numeric(temp$`Dia Juliano`)
temp$Hora<-as.numeric(temp$Hora)
temp$Hora<-temp$Hora/10000
temp$DH <- temp$`Dia Juliano`+temp$Hora

#Cambiar por Santa Quitéria o Pentecoste
temp2 = read_excel("D:\\Users\\Diana\\Documents\\Universidad 2021-03\\Análisis Numérico\\Analisis-Numerico\\Reto2\\datos.xls",
                   sheet = "Santa Quitéria")

temp2$`Dia Juliano`<-as.numeric(temp2$`Dia Juliano`)
temp2$Hora<-as.numeric(temp2$Hora)
temp2$Hora<-temp2$Hora/10000
temp2$DH2 <- temp2$`Dia Juliano`+temp2$Hora

x = temp$`DH`
y = temp$`Temp. Interna (ºC)`

x2 = temp2$`DH2`
y2 = temp2$`Temp. Interna (ºC)`

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
    newX[i] = x2[j]
    newY[i] = y2[j]
    i = i + 1
  }
  j = j + 1
}

#SplineA
datosSplineCh = spline(newX, newY)
datosSpline <-as.numeric(unlist(datosSplineCh))
funcionDatosSpline = splinefun(newX, newY)
datosSpline = round(datosSpline,3)
plot(x, y, type = "l", main = "Aproximación Santa Quitéria", ylab = "Temperaturas", xlab = "Día Juliano")
lines(datosSplineCh, col = "aquamarine1")
errorSpline = c()
k = 1

for (var in x) {
  errorSpline[k] = round(abs((y[k] - funcionDatosSpline(var)) / y[k]), 3)
  k = k + 1
}

cat("Con el uso de la funcioncion spline")
cat("Cantidad de errores: ", sum(errorSpline != 0))
cat("Error maximo: ", max(errorSpline))
cat("Error minimo: (para valores distintos de cero)", min(errorSpline[errorSpline > 0]))
cat("Error medio: (para valores distintos de cero)", median(errorSpline[errorSpline > 0]))
cat("Indice de Jaccard: ", round(sum(errorSpline == 0) / length(x), 3))