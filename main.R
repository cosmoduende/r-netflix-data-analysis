
# PAQUETES
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(plotly)

# LECTURA DE DATOS DESDE CSV DESCARGADO DE NETFLIX
minetflix <- read.csv("NetflixViewingHistory.csv") 
str(minetflix)
minetflix$Date <- dmy(minetflix$Date)

# SEPARAR COLUMNA DE TÍTULO EN TÍTULO DE SERIE, TEMPORADA Y TÍTULO DE EPISODIO
minetflix_serie <- minetflix %>%
  separate(col = Title, into = c("title", "temporada", "titulo_episodio"), sep = ': ')
head(minetflix_serie)

# QUITAR OCURRENCIAS DONDE TEMPORADA Y EPISODIO SON VACÍOS (PORQUE NO SON SERIES)
minetflix_serie <- minetflix_serie[!is.na(minetflix_serie$temporada),]
minetflix_serie <- minetflix_serie[!is.na(minetflix_serie$titulo_episodio),]
head(minetflix_serie)

# REGISTRO DE NÚMERO DE EPISODIOS VISTOS POR DÍA, POR SERIE
maratones_minetflix <- minetflix_serie %>%
  count(title, Date)
maratones_minetflix

# CONSIDEREMOS COMO MARATÓN 6 O MÁS EPISODIOS POR DÍA y ORDENAR POR FECHA
maratones_minetflix <- maratones_minetflix[maratones_minetflix$n >= 6,]
maratones_minetflix
maratones_minetflix <- maratones_minetflix[order(maratones_minetflix$Date),]
maratones_minetflix


# AGRUPANDO DATOS POR TÍTULO DE SERIE Y ORDENANDO POR NÚMERO DE EPISODIOS VISTOS
maratones_minetflix_todas <- maratones_minetflix %>% 
  group_by(title) %>% 
  summarise(n = sum(n)) %>%
  arrange(desc(n))
maratones_minetflix_todas

# VISUALIZANDO DATOS TOP 10 DE SERIES MÁS VISTAS EN MARATÓN
maratones_minetflix_top <- maratones_minetflix_todas %>% 
  top_n(10) %>%
  ggplot(aes(x = reorder(title, n), y = n)) +
  geom_col(fill = "#0097d6") +
  coord_flip() +
  ggtitle("Top 10 de series más vistas en maratón en mi Netflix", "4 o más episodios por día") +
  labs(x = "Serie en Netflix", y = "Episodios vistos en total") +
  theme_minimal()
maratones_minetflix_top
ggplotly()


# EPISODIOS POR DÍA
netflix_episodios_dia <- minetflix %>%
  count(Date) %>%
  arrange(desc(n))
head(netflix_episodios_dia)

# VISUALIZACIÓN DE EPISODIOS POR DÍA
netflix_episodios_dia_plot <- ggplot(aes(x = Date, y = n, color = n), 
                                 data = netflix_episodios_dia) +
  geom_col(color = c("#f16727")) +
  theme_minimal() +
  ggtitle("Episodios vistos en mi Netflix por día", "Historial de 2016 a 2020") +
  labs(x = "Fecha", y = "Episodios vistos") 
netflix_episodios_dia_plot
ggplotly()



# CALENDARIO CON NÚMERO DE EPIODIOS VISTOS POR DÍA EN HEATMAP
netflix_episodios_dia <- netflix_episodios_dia[order(netflix_episodios_dia$Date),]
netflix_episodios_dia$diasemana <- wday(netflix_episodios_dia$Date)
netflix_episodios_dia$diasemanaF <- weekdays(netflix_episodios_dia$Date, abbreviate = T)
netflix_episodios_dia$mesF <- months(netflix_episodios_dia$Date, abbreviate = T)

netflix_episodios_dia$diasemanaF <-factor(netflix_episodios_dia$diasemana,
                                    levels = rev(1:7),
                                    labels = rev(c("Lun","Mar","Mier","Jue","Vier","Sáb","Dom")),
                                    ordered = TRUE) 
netflix_episodios_dia$mesF <- factor(month(netflix_episodios_dia$Date),
                                   levels = as.character(1:12), 
                                   labels = c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"),
                                   ordered = TRUE)

netflix_episodios_dia$añomes <- factor(as.yearmon(netflix_episodios_dia$Date)) 
netflix_episodios_dia$semana <- as.numeric(format(netflix_episodios_dia$Date,"%W"))
netflix_episodios_dia$semanames <- ceiling(day(netflix_episodios_dia$Date) / 7)

netflix_episodios_dia_calendario <- ggplot(netflix_episodios_dia, aes(semanames, diasemanaF, fill = netflix_episodios_dia$n)) + 
  geom_tile(colour = "white") + 
  facet_grid(year(netflix_episodios_dia$Date) ~ mesF) + 
  scale_fill_gradient(low = "#FFD000", high = "#FF1919") + 
  ggtitle("Episodios vistos por día en mi Netflix", "Heatmap por día de la semana, mes y año") +
  labs(x = "Número de semana", y = "Día de la semana") +
  labs(fill = "No.Episodios")
netflix_episodios_dia_calendario
ggplotly()


# FRECUENCIA DE ACTIVIDAD EN MI NETFLIX POR DÍA
vista_dia <- netflix_episodios_dia %>%
  count(diasemanaF)
vista_dia

vista_dia_plot <- vista_dia %>% 
  ggplot(aes(diasemanaF, n)) +
  geom_col(fill = "#5b59d6") +
  coord_polar()  +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 16, face = "bold")) +
  ggtitle("Frecuencia de episodios vistos", "Actividad por día de la semana en mi Netflix")
vista_dia_plot

# FRECUENCIA DE ACTIVIDAD EN MI NETFLIX POR MES
vista_mes <- netflix_episodios_dia %>%
  count(mesF)
vista_mes

vista_mes_plot <- vista_mes %>% 
  ggplot(aes(mesF, n)) +
  geom_col(fill = "#808000") +
  coord_polar()  +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  ggtitle("Frecuencia de episodios vistos", "Actividad por mes en mi Netflix") 
vista_mes_plot


# FRECUENCIA DE ACTIVIDAD EN MI NETFLIX POR AÑO
vista_años <- netflix_episodios_dia %>%
  count(añomes)
vista_años

vista_años_plot <- vista_años %>% 
  ggplot(aes(añomes, n)) +
  geom_col(fill = "#1a954d") +
  coord_polar()  +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 18, face = "bold")) +
  ggtitle("Frecuencia de episodios vistos", "Actividad por mes del año en mi Netflix")
vista_años_plot
