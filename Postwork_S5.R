library(ggplot2)
"Postwork Sesión 5
OBJETIVO

Realizar inferencia estadística para extraer información de la muestra que sea contrastable con la población

REQUISITOS

Haber desarrollado los postworks anteriores
Cubrir los temas del prework
Replicar los ejemplos de la sesión

DESARROLLO

El data frame iris contiene información recolectada por Anderson sobre 50 flores de 3 especies distintas (setosa, versicolor y virginca), 
incluyendo medidas en centímetros del largo y ancho del sépalo así como de los pétalos.

Estudios recientes sobre las mismas especies muestran que:"
data("iris")
str(iris)
#Abstraemos los datos requeridos para las inferencias requeridas
P1.Setosa <- iris[iris$Species=="setosa","Sepal.Length"]
P2.Virginica <- iris[iris$Species=="virginica","Petal.Width"]
P3.Versicolor <- iris[iris$Species=="versicolor","Petal.Length"]
P3.Virginica <- iris[iris$Species=="virginica","Petal.Length"]
P4.Setosa <- iris[iris$Species=="setosa","Sepal.Width"]
P4.Virginica <- iris[iris$Species=="virginica","Sepal.Width"]
P4.Versicolor <- iris[iris$Species=="versicolor","Sepal.Width"]

#P1.En promedio, el largo del sépalo de la especie setosa (Sepal.Length) es igual a 5.7 cm
#Ho:Mean.Setosa.Sepal.Length = 5.7
#Ha:Mean.Setosa.Sepal.Length != 5.7
t.test(x=P1.Setosa, alternative = 'two.sided', mu=5.7)
"A un NC del 99% EEE para rechazar Ho, el promedio del largo del sépalo de la especie setosa es distinto de 5.7"

#P2.En promedio, el ancho del pétalo de la especie virginica (Petal.Width) es menor a 2.1 cm
#Ho:Mean.Virginica.Petal.Width => 2.1
#Ha:Mean.Virginica.Petal.Width < 2.1
t.test(x=P2.Virginica, alternative = 'less', mu=2.1)
"A un NC del 99% EEE para no rechazar Ho, el promedio del ancho de pétalo de la especie virginica es menor a 2.1"

#P3.En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más grande que el promedio del largo del pétalo de la especie versicolor.
#Ho: Mean.Virginica.Petal.Length - Mean.Versicolor.Petal.Length => 1.1
#Ha: Mean.Virginica.Petal.Length - Mean.Versicolor.Petal.Length < 1.1
var.test(P3.Virginica, 
         P3.Versicolor,
         ratio = 1, alternative = 'less')

t.test(x=P3.Virginica, 
       y=P3.Versicolor, 
       alternative = 'less', 
       mu= 1.1,
       var.equal = TRUE)
"A un NC del 99% EEE para rechazar Ho, el promedio del largo del pétalo de la especie virginica no es 1.1 más grande que el promedio del largo del pétalo de la especie versicolor"
#P4.En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies.
#Ho:Mean.Setosa.Sepal.Width = Mean.Virginica.Sepal.Width = Mean.Versicolor.Sepal.Width
#Ha:Cualquiera es diferente

P4.anova <- aov(log(P4.Setosa) ~ P4.Versicolor + P4.Virginica,
               data = iris)
summary(P4.anova)
"Con 99% de NC EEE para rechazar la hipótesis nula, los promedios del ancho del sépalo de las 3 especies presentan diferencias"

#Utilizando pruebas de inferencia estadística, concluye si existe evidencia suficiente para concluir que los datos recolectados por Anderson están en línea con los nuevos estudios.
#Utiliza 99% de confianza para toda las pruebas, en cada caso realiza el planteamiento de hipótesis adecuado y concluye.