# -*- coding: utf-8 -*-
"""
Created on Thu Nov  4 07:48:44 2021

@author: Diana Sofía Carrillo
"""
import math as math

def simpson(f, a, b, m): #integración numérica, f es la función, a y b el invervalo y m es el doble de parábolas que se usan
    h = (b-a)/m #se divide el espacio entre los dos puntos entre el número 
    s = 0
    x = a
    for i in range (1, m):
        s = s + 2 * (i%2+1) * f(x+i*h)
    s = h/3 * (f(a) + s + f(b))
    return s

def f(x): return math.sqrt(1+math.cos(x)**2) #función entregada


#menor a 0.00001 = 0.000001
print(simpson(f, 0, 2, 4)) #n/a
print(simpson(f, 0, 2, 8)) #0.001
print(simpson(f, 0, 2, 12)) #0.00004
print(simpson(f, 0, 2, 16)) #0.000005
print(simpson(f, 0, 2, 20)) #0.000001