"Postwork. Predicciones de la temperatura global
OBJETIVO

Estimar modelos ARIMA y realizar predicciones

DESARROLLO

Utilizando el siguiente vector numérico, realiza lo que se indica:"
  
  url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/global.txt"
Global <- scan(url, sep="")

#Crea una objeto de serie de tiempo con los datos de Global. La serie debe ser mensual comenzado en Enero de 1856
Global.ts <- ts(Global, start = c(1856, 1), freq = 12)
#Realiza una gráfica de la serie de tiempo anterior de 2005
G2005 <- ts(Global.ts, start = c(1856,1), end = c(2004, 12), frequency = 12)
plot(G2005, main = "Serie de tiempo PW 7", xlab = "Tiempo", sub= "Enero de 1856 - Diciembre de 2004")

#Ahora realiza una gráfica de la serie de tiempo anterior, transformando a la primera diferencia:
plot(diff(G2005), type = "l", main = "Primera diferencia de Global",
     xlab = "t", ylab = expression(G2005[t]),
     sub = expression(G2005[t]==G2005[t-1]+w[t]))
acf(diff(G2005))
pacf(diff(G2005))
#¿Consideras que la serie es estacionaria en niveles o en primera diferencia?
#Según los datos observados la serie es estacionaria en niveles
#Con base en tu respuesta anterior, obten las funciones de autocorrelación y autocorrelación parcial?
acf(Global.ts)
pacf(Global.ts)
