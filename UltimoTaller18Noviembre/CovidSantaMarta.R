# Remove-------------------------------------------------------------------
rm(list=ls())
gc()  # garbage collector
#--------------------------------------------------------------------------
library(rgl) 
library (ggplot2)
library(PolynomF)

# Parametros
Beta<-0.06
c <-1.5
gama<- 0.021
N1<-479835   #779853

# Condiciones iniciales 
I0<-2/N1
S0<-1-I0
R0<-0

paso<-1

# Numero de pasos (dias)
dias<-652

S<-function(x,y,z,t) -Beta*x*y
I<-function(x,y,z,t) Beta*x*y-gama*y
R<-function(x,y,z,t) gama*y

vS<-replicate(dias+1,0)
vI<-replicate(dias+1,0)
vR<-replicate(dias+1,0)

vS[1]<-S0
vI[1]<-I0
vR[1]<-R0

#------Punto 1
p<-2
while(p<=dias+1){
  xi<-vS[p-1]
  yi<-vI[p-1]
  zi<-vR[p-1]
  
  k1<-paso*S(xi,yi,zi,ti)
  l1<-paso*I(xi,yi,zi,ti)
  m1<-paso*R(xi,yi,zi,ti)
  
  k2<-paso*S(xi+(1/2)*k1,yi+(1/2)*l1,zi+(1/2)*m1,ti+paso/2)
  l2<-paso*I(xi+(1/2)*k1,yi+(1/2)*l1,zi+(1/2)*m1,ti+paso/2)
  m2<-paso*R(xi+(1/2)*k1,yi+(1/2)*l1,zi+(1/2)*m1,ti+paso/2)
  
  k3<-paso*S(xi+(1/2)*k2,yi+(1/2)*l2,zi+(1/2)*m2,ti+paso/2)
  l3<-paso*I(xi+(1/2)*k2,yi+(1/2)*l2,zi+(1/2)*m2,ti+paso/2)
  m3<-paso*R(xi+(1/2)*k2,yi+(1/2)*l2,zi+(1/2)*m2,ti+paso/2)
  
  k4<-paso*S(xi+k3,yi+l3,zi+m3,ti+paso)
  l4<-paso*I(xi+k3,yi+l3,zi+m3,ti+paso)
  m4<-paso*R(xi+k3,yi+l3,zi+m3,ti+paso)
  
  vS[p]<-xi+(1/6)*(k1+2*k2+2*k3+k4)
  vI[p]<-yi+(1/6)*(l1+2*l2+2*l3+l4)
  vR[p]<-zi+(1/6)*(m1+2*m2+2*m3+m4)
  
  p<-p+1
}

#------ Punto 2

plot(vS, type="l", col="blue",axes=F,xlab = "Dias",ylab="Porcentaje")
par(new=TRUE)
plot(vI, type="l",col="red",axes=F,xlab = "Dias",ylab="Porcentaje")
par(new=TRUE)
plot(vR, type="l",col="black",xlab = "Dias",ylab="Porcentaje")
title(main="Modelo aproximado contagio Covid-19 SantaMarta")

legend(x = "right", legend = c("Susceptibles", "Infectados", "Recuperados"), fill = c("blue", "red","black"),box.lty=0)


#------- Punto 3

MaxInfectados<- max(vI)
MaxInfectados*N1
DiaMaxInfectados<-which.max(vI)
DiaMaxInfectados

#------- Punto 4

MaxRecuperados<- max(vR)
MaxRec*N1
DiaMaxRec <- which.max(vR)
DiaMaxRec

PorcentajeInfectados<- MaxInfectados
PorcentajeInfectados
PorcentajeRecuperados<- MaxRecuperados
PorcentajeRecuperados


#----- Punto 5

#  Como I(t)=C/N
#  gama/(Beta*C)>S/N
#  gama/(Beta*I(t))>S(t)

control<-integer(dias)

for (i in 1:dias){
 if (gama/(Beta*vI[i])>vS[i]){
   control[i]<-1
 } 
}

plot(control)

#-----------Punto 6


#----------Punto 7

vectorRe<- integer(652)
for(i in 1:652){
  vectorRe[i]<-Beta*c*vS[i]/(gama*N1)   #(Beta*vI[i]*vS[i])/gama
}

max(vectorRe)

plot(vectorRe, type="l")

#----------Punto 8


#----------Punto 9
# Solucion Exacta

vS2<-replicate(dias+1,0)
vI2<-replicate(dias+1,0)
vR2<-replicate(dias+1,0)

vS2[1]<-1
vI2[1]<-14/N1
vR2[1]<-7/N1

p<-2
while(p<=dias+1){
  xi<-vS[p-1]
  yi<-vI[p-1]
  zi<-vR[p-1]
  
  k1<-paso*S(xi,yi,zi,ti)
  l1<-paso*I(xi,yi,zi,ti)
  m1<-paso*R(xi,yi,zi,ti)
  
  k2<-paso*S(xi+(1/2)*k1,yi+(1/2)*l1,zi+(1/2)*m1,ti+paso/2)
  l2<-paso*I(xi+(1/2)*k1,yi+(1/2)*l1,zi+(1/2)*m1,ti+paso/2)
  m2<-paso*R(xi+(1/2)*k1,yi+(1/2)*l1,zi+(1/2)*m1,ti+paso/2)
  
  k3<-paso*S(xi+(1/2)*k2,yi+(1/2)*l2,zi+(1/2)*m2,ti+paso/2)
  l3<-paso*I(xi+(1/2)*k2,yi+(1/2)*l2,zi+(1/2)*m2,ti+paso/2)
  m3<-paso*R(xi+(1/2)*k2,yi+(1/2)*l2,zi+(1/2)*m2,ti+paso/2)
  
  k4<-paso*S(xi+k3,yi+l3,zi+m3,ti+paso)
  l4<-paso*I(xi+k3,yi+l3,zi+m3,ti+paso)
  m4<-paso*R(xi+k3,yi+l3,zi+m3,ti+paso)
  
  vS2[p]<-xi+(1/6)*(k1+2*k2+2*k3+k4)
  vI2[p]<-yi+(1/6)*(l1+2*l2+2*l3+l4)
  vR2[p]<-zi+(1/6)*(m1+2*m2+2*m3+m4)
  
  p<-p+1
}

plot(vS2, type="l", col="blue",axes=F,xlab = "Dias",ylab="Porcentaje")
par(new=TRUE)
plot(vI2, type="l",col="red",axes=F,xlab = "Dias",ylab="Porcentaje")
par(new=TRUE)
plot(vR2, type="l",col="black",xlab = "Dias",ylab="Porcentaje")
title(main="Valores reales contagio Covid-19 SantaMarta")

legend(x = "right", legend = c("Susceptibles", "Infectados", "Recuperados"), fill = c("blue", "red","black"),box.lty=0)

#-------------Punto 10
# Tabla de errores absolutos medios y errores relativos

MatrizError<-matrix(0:0, nrow=10, ncol=12)
MatrizError<-data.frame(MatrizError)
colnames(MatrizError)<-c("SusceptibleReal","SusceptibleAprox", "ErrorrelativoS","ErrorAbsolutoS",
      "InfectadosReal", "InfectadosAprox", "ErrorrelativoI","ErrorAbsolutoI",
      "RecuperadosReal","RecuperadosAprox", "ErrorrelativoR", "ErrorAbsolutoR")

MatrizError$SusceptibleReal<-vS2[1:10]
MatrizError$SusceptibleAprox<-vS[1:10]
MatrizError$InfectadosReal<-vI2[1:10]
MatrizError$InfectadosAprox<-vI[1:10]
MatrizError$RecuperadosReal<-vR2[1:10]
MatrizError$RecuperadosAprox<-vR[1:10]

for (i in 1:10){ 
  MatrizError$ErrorAbsolutoS[i]<-abs(vS[i]-vS2[i])
  MatrizError$ErrorRelativoS[i]<-MatrizError$ErrorrelativoS[i]/vS[i]
  MatrizError$ErrorAbsolutoI[i]<-abs(vI[i]-vI2[i])
  MatrizError$ErrorRelativoI[i]<-MatrizError$ErrorrelativoI[i]/vI[i]
  MatrizError$ErrorAbsolutoR[i]<-abs(vR[i]-vR2[i])
  MatrizError$ErrorRelativoR[i]<-MatrizError$ErrorrelativoR[i]/vR[i]
}

View(MatrizError)














