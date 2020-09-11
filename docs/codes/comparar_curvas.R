# Definición matemática de los modelos logistico, gompertz e monomolecular
lo <- function(x, K, y0, r) {K/(1+((K-y0)/y0)*exp(-r*x))}
go <- function(x, K, y0, r) {K*(exp(-exp(y0-r*x)))}
mo <- function(x, K, y0, r) {K-(K-y0)*exp(-r*x)}

library(ggplot2)

# Graficar los 3 modelos con los mismos coeficientes

param = list(K=0.8, y0=0.01, r=0.3)

ggplot(data = data.frame(x = 0), aes(x = x))+
  stat_function(fun=lo, args=param, aes(colour="Logístico")) +
  stat_function(fun=go, args=param, aes(colour="Gompertz")) +
  stat_function(fun=mo, args=param, aes(colour="Monomolecular")) +
  xlim(0,25) + ylim(0,1) +
  scale_colour_manual("Modelo", values = c("red", "green", "blue"))

# La predicción para $t=15$ para los 3 modelos seria:

lo(15, 0.8, 0.01, 0.3)
go(15, 0.8, 0.01, 0.3)
mo(15, 0.8, 0.01, 0.3)


# Graficar dos curvas logisticas con igual $r$ (0.5) pero diferente $k$ (0.6 y 0.8):
  
param1 = list(K=0.6, y0=0.01, r=0.5)
param2 = list(K=0.8, y0=0.01, r=0.5)

ggplot(data = data.frame(t = 0), aes(x = t))+
  stat_function(fun = lo, args=param1, aes(colour = "r=0.5;k=0.6")) +
  stat_function(fun = lo, args=param2, aes(colour = "r=0.5;k=0.8")) +
  xlim(0,25) + ylim(0,1) +
  scale_colour_manual("Modelo \nLogístico", values = c("red", "blue"))

# Ahora, lo contrario, dos curvas logisticas con diferente $r$ (0.3 y 0.6) e igual $k$ (0.8):
  
param3 = list(K=0.8, y0=0.01, r=0.4)
param4 = list(K=0.8, y0=0.01, r=0.6)

ggplot(data = data.frame(t = 0), aes(x = t))+
  stat_function(fun = lo, args=param3, aes(colour = "r=0.4;k=0.8")) +
  stat_function(fun = lo, args=param4, aes(colour = "r=0.6;k=0.8")) +
  xlim(0,25) + ylim(0,1) +
  scale_colour_manual("Modelo \nLogístico", values = c("red","blue"))
