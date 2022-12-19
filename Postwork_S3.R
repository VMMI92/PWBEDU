library(dplyr)
library(DescTools)
library(ggplot2)
library(ggpubr)
library(moments)
library(KernSmooth)
# Postwork Sesión 3

#### Objetivo

#- Realizar un análisis descriptivo de las variables de un dataframe

#### Requisitos

#1. R, RStudio
#2. Haber realizado el prework y seguir el curso de los ejemplos de la sesión
#3. Curiosidad por investigar nuevos tópicos y funciones de R

#### Desarrollo

"Utilizando el dataframe `boxp.csv` realiza el siguiente análisis descriptivo. No olvides excluir los missing values y transformar las variables a su
tipo y escala correspondiente."
df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/boxp.csv")
str(df)
df.clean <- df[complete.cases(df),]
df.clean
df.clean$Categoria <- as.factor(df.clean$Categoria)
df.clean$Grupo <- as.factor(df.clean$Grupo)
summary(df)
summary(df.clean)
#1) Calcula e interpreta las medidas de tendencia central de la variable `Mediciones`
#Media
(Media <- mean(df.clean$Mediciones))

#Mediana
(Mediana <- median(df.clean$Mediciones))

#Moda
(Moda <- Mode(df.clean$Mediciones)[1])
#Se define el ancho de barra a la medida de cada observación
ancho_barras <- dpih(df.clean$Mediciones)
nbarras <- seq(min(df.clean$Mediciones) - ancho_barras,
               max(df.clean$Mediciones) + ancho_barras, by = ancho_barras)

histograma <- hist(df.clean$Mediciones, breaks = nbarras, main = "Histograma Boxp", xlab = "Mediciones", ylab = "Frecuencias")
#2) Con base en tu resultado anterior, ¿qué se puede concluir respecto al sesgo de `Mediciones`?
#La distribución de los datos tiene un sesgo a la derecha

#3) Calcula e interpreta la desviación estándar y los cuartiles de la distribución de `Mediciones`
(desv_stndr <- sd(df.clean$Mediciones))
(cuartiles <- quantile(df.clean$Mediciones, probs = c(0.25, 0.5, 0.75)))

"4) Con ggplot, realiza un histograma separando la distribución de `Mediciones` por `Categoría`
¿Consideras que sólo una categoría está generando el sesgo?"
#No, las categorías contienen casi la misma distribución de Mediciones
(HP4 <- ggplot(df.clean, aes(x=Categoria, fill = Categoria)) +
  geom_bar() + 
  labs(title = "Histograma Mediciones por Categoria", 
       x = "Categoría",
       y = "Mediciones") + 
  theme_linedraw())

"5) Con ggplot, realiza un boxplot separando la distribución de `Mediciones` por `Categoría` 
y por `Grupo` dentro de cada categoría. ¿Consideras que hay diferencias entre categorías? ¿Los grupos al interior de cada categoría 
podrían estar generando el sesgo?"
#Sí, las categorías del grupo 1 tienen sus datos con una distribución simétrica, mientras que las categorías del grupo 0 presentan un sesgo a la izquierda
(HP5 <- ggboxplot(df.clean, x = "Grupo", y = "Mediciones", color = "Categoria", main = "Boxplot Mediciones por Categoria y Grupo"))
