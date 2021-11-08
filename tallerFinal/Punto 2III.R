# Sistema de Ecuaciones Diferenciales - Sistema de Lorentz
# x'(t)=ax+yz
# y'(t)=b(y-z)
# z'(t)=-xy+cy-z
# Remove-------------------------------------------------------------------
rm(list=ls())
#--------------------------------------------------------------------------
library(rgl) 
library("scatterplot3d") 

# Parametros
a<-8/3
b<-10
c<-28

# Condiciones iniciales 
x0<-0.1
y0<-1
z0<-0.1

# Numero de pasos
N<-100

paso<-0.05


f<-function(x,y,z,t) a*(y-x)
g<-function(x,y,z,t) x*(b-z)-y
h<-function(x,y,z,t) x*y-c*z

RK4<-function(f,g,h,xi,yi,zi,ti,paso){
  k1<-paso*f(xi,yi,zi,ti)
  l1<-paso*g(xi,yi,zi,ti)
  m1<-paso*h(xi,yi,zi,ti)
  
  k2<-paso*f(xi+(1/2)*k1,yi+(1/2)*l1,zi+(1/2)*m1,ti+paso/2)
  l2<-paso*g(xi+(1/2)*k1,yi+(1/2)*l1,zi+(1/2)*m1,ti+paso/2)
  m2<-paso*h(xi+(1/2)*k1,yi+(1/2)*l1,zi+(1/2)*m1,ti+paso/2)
  
  k3<-paso*f(xi+(1/2)*k2,yi+(1/2)*l2,zi+(1/2)*m2,ti+paso/2)
  l3<-paso*g(xi+(1/2)*k2,yi+(1/2)*l2,zi+(1/2)*m2,ti+paso/2)
  m3<-paso*h(xi+(1/2)*k2,yi+(1/2)*l2,zi+(1/2)*m2,ti+paso/2)
  
  k4<-paso*f(xi+k3,yi+l3,zi+m3,ti+paso)
  l4<-paso*g(xi+k3,yi+l3,zi+m3,ti+paso)
  m4<-paso*h(xi+k3,yi+l3,zi+m3,ti+paso)
  
  xs<-xi+(1/6)*(k1+2*k2+2*k3+k4)
  ys<-yi+(1/6)*(l1+2*l2+2*l3+l4)
  zs<-zi+(1/6)*(m1+2*m2+2*m3+m4)
  return(c(xs,ys,zs))
}

vx<-replicate(N+1,0)
vy<-replicate(N+1,0)
vz<-replicate(N+1,0)

vx[1]<-x0
vy[1]<-y0
vz[1]<-z0

i<-2
while(i<=N+1){
  sol<- RK4(f,g,h,vx[i-1],vy[i-1],vz[i-1],t[i],paso)
  vx[i]<-sol[1]
  vy[i]<-sol[2]
  vz[i]<-sol[3]
  i<-i+1
}

datos<-data.frame(vx,vy,vz)
colnames(datos)<-c("ValorX", "ValorY", "ValorZ")
scatterplot3d(x=vx,y=vy,z=vz)