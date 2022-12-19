"Postwork Sesión 4
Objetivo

Realizar un análisis probabilístico del total de cargos internacionales de una compañía de telecomunicaciones

Requisitos

R, RStudio
Haber trabajado con el prework y el work

Desarrollo

Utilizando la variable total_intl_charge de la base de datos telecom_service.csv de la sesión 3, realiza un análisis probabilístico. 
Para ello, debes determinar la función de distribución de probabilidad que más se acerque el comportamiento de los datos. 
Hint: Puedes apoyarte de medidas descriptivas o técnicas de visualización."

df.pw4 <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")
str(df.pw4)
summary(df.pw4)

#Una vez que hayas seleccionado el modelo, realiza lo siguiente:

#Grafica la distribución teórica de la variable aleatoria total_intl_charge
mu <- mean(df.pw4$total_intl_charge)
sd <- sd(df.pw4$total_intl_charge)
curve(dnorm(x, mean=mu, sd=sd),from = min(df.pw4$total_intl_charge), to = max(df.pw4$total_intl_charge),
      col="blue", main = "Distribución Cargos totales internacionales",
      ylab='f(x)', xlab= "X")
                                                                             
#¿Cuál es la probabilidad de que el total de cargos internacionales sea menor a 1.85 usd?
(pnorm(q=1.84, mean=mu, sd=sd))
#[1] 0.1099855
#¿Cuál es la probabilidad de que el total de cargos internacionales sea mayor a 3 usd?
(pnorm(q=3, mean = mu, sd=sd, lower.tail = FALSE))
#[1] 0.3773985
#¿Cuál es la probabilidad de que el total de cargos internacionales esté entre 2.35usd y 4.85 usd?
(pnorm(q=4.85, mean=mu, sd=sd)-pnorm(q=2.3, mean=mu, sd=sd))
#[1] 0.7283336
#Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales más alto que podría esperar?
(qnorm(p=0.48, mean=mu, sd=sd, lower.tail = FALSE))
#[1] 2.802386
#¿Cuáles son los valores del total de cargos internacionales que dejan exactamente al centro el 80% de probabilidad?
qnorm(p=0.1, mean=mu, sd=sd); qnorm(p=0.1, mean=mu, sd=sd,lower.tail = FALSE)
#[1] 1.798583
#[1] 3.73058