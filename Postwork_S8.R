library(ggplot2)
library(dplyr)
"# Postwork sesión 8. Análisis de la Inseguridad Alimentaria en México

#### OBJETIVO

- Realizar un análisis estadístico completo de un caso 
- Publicar en un repositorio de Github el análisis y el código empleado 

#### REQUISITOS

- Haber realizado los works y postworks previos 
- Tener una cuenta en Github o en RStudioCloud

#### DESARROLLO

Un centro de salud nutricional está interesado en analizar estadísticamente y probabilísticamente los patrones de gasto en alimentos saludables y no saludables 
en los hogares mexicanos con base en su nivel socioeconómico, en si el hogar 
tiene recursos financieros extrar al ingreso y en si presenta o no inseguridad alimentaria.
Además, está interesado en un modelo que le permita identificar los determinantes socioeconómicos de la inseguridad alimentaria.

La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) levantada por el Instituto Nacional de Salud Pública en México.
La mayoría de las personas afirman que los hogares con menor nivel socioeconómico tienden a gastar más en productos no saludables que las personas 
con mayores niveles socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar presente cierta inseguridad alimentaria.

La base de datos contiene las siguientes variables:
- nse5f (Nivel socieconómico del hogar): 1 'Bajo', 2 'Medio bajo', 3 'Medio', 4 'Medio alto', 5 'Alto'
- area (Zona geográfica): 0 'Zona urbana', 1 'Zona rural'
- numpeho (Número de persona en el hogar)
- refin (Recursos financieros distintos al ingreso laboral): 0 'no', 1 'sí'
- edadjef (Edad del jefe/a de familia)
- sexoje (Sexo del jefe/a de familia): 0 'Hombre', 1 'Mujer'
- añosedu (Años de educación del jefe de familia)
- ln_als (Logarítmo natural del gasto en alimentos saludables)
- ln_alns (Logarítmo natural del gasto en alimentos no saludables)
- IA (Inseguridad alimentaria en el hogar): 0 'No presenta IA', 1 'Presenta IA'"


df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")
df <- na.omit(df)

#1) Plantea el problema del caso
"Se busca analizar los patrones de gasto alimenticio en los hogares mexicanos en alimentos saludables y no saludables, en función de su nivel socioeconómico,
además saber si el hogar cuenta con ingreso extra de recursos financieros y si el mismo presenta o no inseguridad alimentaria e 
identificar los factores de la inseguridad alimentaria."
#2) Realiza un análisis descriptivo de la información
str(df)
df$nse5f <- factor(df$nse5f, labels= c("Bajo", "Medio bajo", "Medio", "Medio alto", "Alto"), ordered = TRUE)
df$area <- factor(df$area, labels = c("Zona urbana", "Zona rural"))
df$refin <- factor(df$refin, labels = c("No", "Yes"))
df$sexojef <- factor(df$sexojef, labels = c("Hombre", "Mujer"))
df$IA <- factor(df$IA, labels = c("No presenta IA", "Presenta IA"))
str(df)
summary(df)

#Las siguientes tablas nos muestran las frecuencias relativas y absolutas de las variables cualitativas del data frame
#Nivel Socioeconomico
(freq_nse5f <- table(df$nse5f))
transform(freq_nse5f,
          Frecuencia_Relativa = prop.table(Freq),
          Frecuencia_Acumulada = cumsum(prop.table(Freq)))
#Zona geografica
(freq_area <- table(df$area))
transform(freq_area,
          Frecuencia_Relativa = prop.table(Freq))
#Sexo jef@
(freq_sexo <- table(df$sexojef))
transform(freq_sexo,
          Frecuencia_Relativa = prop.table(Freq))
#Recursos financieros distintos al ingreso laboral
(freq_refin <- table(df$refin))
transform(freq_refin,
          Frecuencia_Relativa = prop.table(Freq))
#Inseguridad alimentaria en el hogar
(freq_IA <- table(df$IA))
transform(freq_IA,
          Frecuencia_Relativa = prop.table(Freq))

#Las siguientes son tablas de distribución de frecuencias para las variables cuantitativas
#ln_als
k_als = ceiling(sqrt(length(df$ln_als)))

ggplot(df, aes(ln_alns))+
  geom_histogram(bins = 16)+
  labs(title = "Gasto en Alimentos No Saludables",
       x = "Gasto",
       y = "Frecuencia")+
  theme_classic()

#ln_alns
k_alns = ceiling(sqrt(length(df$ln_alns)))

ggplot(df, aes(ln_als))+
  geom_histogram(bins = k_als)+
  labs(title = "Gasto en Alimentos Saludables",
       x = "Gasto",
       y = "Frecuencia")+
  theme_classic()

df.select <- select(df, ln_alns, edadjef, añosedu, numpeho)
round(cor(df.select),4)

pairs(~ ln_alns + edadjef + añosedu + numpeho + area + nse5f + IA + sexojef + refin,
      data = df, gap = 0.4, cex.labels=1.2)

m1 <- lm(df$ln_alns ~ df$edadjef + df$añosedu + df$numpeho + df$IA)
summary(m1)

m_IA <- lm(df$ln_als ~ df$edadjef + df$añosedu + df$numpeho + df$IA)
summary(m_IA)

#Este pipeline nos describe el promedio de edad y años de educación del jefe de hogar agrupados por el sexo
datos_jefam <- df %>% 
  group_by(sexojef) %>%
  summarize(total_H_M = length(sexojef),
            edad_promedio= round(mean(edadjef)),
            años_educacion_promedio=round(mean(añosedu)),
            prom_gasto_alim_nosalu= mean(ln_alns),
            prom_gasto_alim_salu= mean(ln_als))

#El siguiente nos agrupa por nivel socioeconomico y nos da el total de hogares por nivel, el promedio de personas por hogar,
#y los promedios de gastos en alimentos saludables y no saludables
nivel_socio_FNIA <- df %>%
  group_by(nse5f) %>%
  summarize(total_hogares_por_categoria=length(nse5f),
            promedio_num_per=mean(numpeho),
            alimentos_saludables=mean(ln_als),
            alimentos_no_saludables=mean(ln_alns))


nivel_socio_FIA <- df %>%
  group_by(IA) %>%
  summarize(total_hogares_por_categoria=length(IA),
            promedio_num_per=mean(numpeho),
            alimentos_saludables=mean(ln_als),
            alimentos_no_saludables=mean(ln_alns))
por_zona <- df %>%
  group_by(area) %>%
  summarize(total_hogares_por_categoria=length(area))
por_refin <- df %>%
  group_by(refin) %>%
  summarize(total_hogares_por_categoria=length(refin))
#3) Calcula probabilidades que nos permitan entender el problema en México
#4) Plantea hipótesis estadísticas y concluye sobre ellas para entender el problema en México

library(ggplot2)
str(df)

boxplot(log(df$ln_alns))

boxplot(log(df$ln_alns) ~ df$nse5f,
        data = df)

anova <- aov(log(df$ln_alns) ~ df$IA,
             data = df)

summary(anova)
#5) Estima un modelo de regresión, lineal o logístico, para identificiar los determinantes de la inseguridad alimentaria en México
logistic.1 <- glm(df$nse5f ~ df$ln_als, family=binomial)
summary(logistic.1)
exp(coe)
#6) Escribe tu análisis en un archivo README.MD y tu código en un script de R y publica ambos en un repositorio de Github.

#NOTA: Todo tu planteamiento deberá estár correctamente desarrollado y deberás analizar e interpretar todos tus 
#resultados para poder dar una conclusión final al problema planteado.