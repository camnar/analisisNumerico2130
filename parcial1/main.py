library(pracma)

options(digits=8)

funcion = function(x){exp(1)^x*cos(x)}

tay = taylor(funcion, x0 = 0, n = 3)
tay
# Se prefiere usar el polinomio de taylor calculado a mano puesto que
# el calculado numericamente tiene errores de precision en x^2

# Puesto que el polinomio de taylor se vuelve más impreciso alejado del centro,
# se decidió trabajar con un intervalo +-1 del valor a evaluar
lim = c(-0.5, 1.5)

taylorfun = function(x){1+x-x^3/3}

curve(taylorfun, xlim=lim, ylab="f(x)", col="red")
curve(funcion, xlim=lim,  add=T)
legend("topleft", inset=0.1, legend=c("f(x)", "P3(x)"),
       col=c("black", "red"), lty=1:1, cex=0.8)

firstD = function(x){exp(1)^x*(cos(x)-sin(x))}
secondD = function(x){-2*exp(1)^x*cos(x)} 
fourthD = function(x){exp(1)^x*cos(x)}

metodoNewton = function(f, f_prima, x_0, err){
  res = x_0
  f_x = f(x_0)
  while (abs(f_x) > err)
  {
    res = res - f(res) / f_prima(res)
    f_x = f(res)
  }
  return(res)
}

# para hallar el mayor valor c del Error de Lagrange para polinomios de Taylor
largest = metodoNewton(firstD, secondD, 1, 1e-8)

RN = abs(funcion(largest)*lim[2]^4/6)

errorx = function(x){ abs(funcion(x) - taylorfun(x)) }
errorx(1.5)
cat("RN(Maximo error teorico)=",RN,"errormaximo real=",errorx(1.5))

curve(errorx, xlim=c(-0.5,1.8),ylim=c(0,largest+0.5))
abline(h = RN,col="red",lty=2)
legend("topleft", inset=0.1, legend=c("error(x)", "RN"),
       col=c("black", "red"), lty=1:2, cex=0.8)

xasterisco = seq(from=lim[1], to=lim[2],by=0.1)
for( i in xasterisco)
{
  cat("x*=",i," e(x*)=",errorx(i),"\n")
}

cat("P3(0.5) = ",taylorfun(0.5),"\n")
cat("f(0.5) = ",funcion(0.5))