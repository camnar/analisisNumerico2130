#######################################################
# Polinomios de interpolacion de Newton
#######################################################
# Remove-----------------------------------------------
rm(list=ls())
x<-c(1, 3, 4, 8)
y<-c(2, 3, 2, 10)

# Definimos la matriz de diferencias
n<-length(x)
A<-matrix(0,n,n+1)
for( i in 1:n)
{
  A[i,1]<-x[i]
  A[i,2]<-y[i]
}

j<-3 # Ubicacion columna
while(j <= n+1){
  k<- n-j+2 # Determina el numero de veces que se hace el proceso por columna
  i<-1
  p<-0 # Variable auxiliar
  while (i<=k){
    p<-(A[i+1,j-1]-A[i,j-1])/(A[i+j-2,1]-A[i,1])
    A[i,j]<-p
    i<-i+1
  }
  j<-j+1
}

View(A)

# Graficamos los polinomios interpoladores 
x<-c(1:0.1:8)
plot(A[,1],A[,2])
curve(A[1,2]+A[1,3]*(x-A[1,1]),1,8,add=T,col = "gray")
curve(A[1,2]+A[1,3]*(x-A[1,1])+A[1,4]*(x-A[1,1])*(x-A[2,1]),add = TRUE,col = "violet")
curve(A[1,2]+A[1,3]*(x-A[1,1])+A[1,4]*(x-A[1,1])*(x-A[2,1])+A[1,5]*(x-A[1,1])*(x-A[2,1])*(x-A[3,1]),add = TRUE,col = "red")
