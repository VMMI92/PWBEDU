"Supongamos que nuestro trabajo consiste en aconsejar a un cliente sobre como mejorar las ventas de un producto particular, 
y el conjunto de datos con el que disponemos son datos de publicidad que consisten en las ventas de aquel producto en 
200 diferentes mercados, junto con presupuestos de publicidad para el producto en cada uno de aquellos mercados 
para tres medios de comunicación diferentes: TV, radio, y periódico. No es posible para nuestro cliente incrementar 
directamente las ventas del producto. Por otro lado, ellos pueden controlar el gasto en publicidad para cada uno de los 
tres medios de comunicación. Por lo tanto, si determinamos que hay una asociación entre publicidad y ventas, 
entonces podemos instruir a nuestro cliente para que ajuste los presupuestos de publicidad, y así indirectamente 
incrementar las ventas.

En otras palabras, nuestro objetivo es desarrollar un modelo preciso que pueda ser usado para predecir las ventas sobre la 
base de los tres presupuestos de medios de comunicación. Ajuste modelos de regresión lineal múltiple a los datos advertisement.csv 
y elija el modelo más adecuado siguiendo los procedimientos vistos

Considera:
  
Y: Sales (Ventas de un producto)
X1: TV (Presupuesto de publicidad en TV para el producto)
X2: Radio (Presupuesto de publicidad en Radio para el producto)
X3: Newspaper (Presupuesto de publicidad en Periódico para el producto)"

adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")
str(adv)

#Generamos nuestra matriz de correlación, a la vez que la graficamos, para evaluar la correlación que existe entre nuestra variables de interes
round(cor(adv),4)
attach(adv)

pairs(~Sales + TV + Radio + Newspaper,
      data = adv, gap = 0.4, cex.labels = 1.5)

#Creamos nuestro modelo de regresión lineal

m1.pw6 <- lm(Sales ~ TV + Radio + Newspaper)
summary(m1.pw6)

m1.pw6u <- update(m1.pw6, ~.-Newspaper)
summary(m1.pw6u)

#Verificamos que nuestro termino de error siga una distribución normal
StanRes <- rstandard(m1.pw6u)

par(mfrow = c(2, 2))
plot(TV, StanRes, ylab = "Residuales Estandarizados")
plot(Radio, StanRes, ylab = "Residuales Estandarizados")
plot(Newspaper, StanRes, ylab = "Residuales Estandarizados")

qqnorm(StanRes)
qqline(StanRes)

dev.off()

shapiro.test(StanRes)
#Ho: La variable distribuye como  una normal
#Ha: La variable no distribuye como una normal

"Con NC del 99% EEE para no rechazar Ho, lo cual indica que nuestros residuos distribuyen como una normal"