#Se asigna el archivo csv a la variable SP1
library(dplyr)
library(readr)
SP1 <- read_csv("https://www.football-data.co.uk/mmz4281/1920/SP1.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                                     Time = col_time(format = "%H:%M")))

#Extraemos los vectores que nos interesan
FTHG <- SP1$FTHG
FTAG <- SP1$FTAG

#Filtramos la tabla para mostrar solo los juegos empatados
TG <- data.frame(ifelse(FTHG == FTAG, FTHG,""),ifelse(FTHG == FTAG, FTAG,""))
colnames(TG) <- c('FTHG', 'FTAG')
TGD <- TG[!(TG$FTHG==""),]

#Generamos una tabla de frecuencia absoluta
RGD <- as.data.frame(table(FTHG = TG$FTHG))
RGD <- RGD[-1,]
colnames(RGD) <- c('Empates','Num_Partidos')

# ¿Cuál es el marcador de empate más frecuente?
# 49 partidos con marcados (1-1)
(P1 <- RGD%>%
  slice(which.max(RGD$Num_Partidos)))

# ¿En cuántos partidos ambos equipos empataron 0 a 0?
# 33 partidos
(P2 <- RGD%>%
  filter(RGD$Empates==0))

# ¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar que el equipo visitante (AG) metiera un solo gol?
# Un partido (6-1)
DFP3 <- data.frame(FTHG, FTAG)
(P3 <- DFP3%>%filter(FTHG == max(FTHG) & FTAG == 0))
