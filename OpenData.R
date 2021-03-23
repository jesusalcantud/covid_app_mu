## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------
library(rvest)
library(stringr)
library(XML)
library(xml2)
library(readr)
library(lubridate)
library(tidyverse)
library(zoo)
library(TTR)
library(rgdal)


## ----pressure, echo=FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------
URL_gh <- 'https://raw.githubusercontent.com/montera34/escovid19data/master/data/output/covid19-ccaa-spain_consolidated.csv'

dat_es <- read_csv(url(URL_gh))

glimpse(dat_es)


## -------------------------------------------------------------------------------------------------------------------------------------------
dat_es$date <- ymd(dat_es$date)
dat_es$date_vac <- ymd(dat_es$date_vac)

cols_casos <- c("ccaa", 'poblacion',"date", "num_casos2","ia14", "hospitalized_per_100000", "intensive_care_per_1000000","deceassed_per_100000")
cols_vac <- c("ccaa_vac",'poblacion',"date_vac","vac_dosis_entregadas", "vac_dosis_administradas","vac_perc_entregadas","vac_dosis_pauta_completada")

dat_es_casos <- dat_es[, cols_casos] %>% filter(date >= '2020-08-01')

dat_es_vac <- dat_es[, cols_vac] %>% filter(date_vac >= '2021-01-01')


## -------------------------------------------------------------------------------------------------------------------------------------------
dat_es_casos$w <- dat_es_casos$poblacion/47329981

casos_esp <- dat_es_casos %>% group_by(date) %>% 
  summarise_at(c("num_casos2","ia14", "hospitalized_per_100000", "intensive_care_per_1000000","deceassed_per_100000"), 
               ~ sum(.x*w))


## -------------------------------------------------------------------------------------------------------------------------------------------
casos_esp$intensive_care_per_1000000 <- na.locf(casos_esp$intensive_care_per_1000000, fromLast = TRUE, na.rm=FALSE)

casos_esp[is.na(casos_esp)] = 0


## -------------------------------------------------------------------------------------------------------------------------------------------
casos_mu <- dat_es_casos %>% filter(ccaa == 'Murcia, Región de')
casos_mu$intensive_care_per_1000000 <- na.locf(casos_mu$intensive_care_per_1000000, fromLast = TRUE, na.rm=FALSE)


## -------------------------------------------------------------------------------------------------------------------------------------------
casos_comp <- merge(casos_esp,casos_mu, by = 'date', all = T, suffixes = c(".es",".mu"))

casos_comp[, c('poblacion', 'w', 'ccaa')] <- NULL

casos_comp[is.na(casos_comp)] = 0

smooth <- function(x) SMA(x, n = 5)

casos_comp <- casos_comp %>% mutate_if(is.numeric,smooth)


## -------------------------------------------------------------------------------------------------------------------------------------------
casos_comp <- casos_comp %>% drop_na()


## -------------------------------------------------------------------------------------------------------------------------------------------
casos_comp$riesgo.mu <- ifelse(casos_comp$ia14.mu <= 25, 'nueva normalidad', 
                               ifelse(casos_comp$ia14.mu <= 50, 'riesgo bajo',
                                      ifelse(casos_comp$ia14.mu <= 150, 'riesgo medio',
                                             ifelse(casos_comp$ia14.mu <= 250, 'riesgo alto', 'riesgo extremo'))))

casos_comp$riesgo.mu <- factor(casos_comp$riesgo.mu, 
                               levels = c('nueva normalidad','riesgo bajo','riesgo medio', 'riesgo alto', 'riesgo extremo'))

casos_comp$riesgo.es <- ifelse(casos_comp$ia14.es <= 25, 'nueva normalidad', 
                               ifelse(casos_comp$ia14.es <= 50, 'riesgo bajo',
                                      ifelse(casos_comp$ia14.es <= 150, 'riesgo medio',
                                             ifelse(casos_comp$ia14.es <= 250, 'riesgo alto', 'riesgo extremo'))))

casos_comp$riesgo.es <- factor(casos_comp$riesgo.es, 
                               levels = c('nueva normalidad','riesgo bajo','riesgo medio', 'riesgo alto', 'riesgo extremo'))


## -------------------------------------------------------------------------------------------------------------------------------------------
for (i in c(2:11)){
  casos_comp[,i] <- round(casos_comp[,i], 2)
}


## -------------------------------------------------------------------------------------------------------------------------------------------
dat_es_vac <- dat_es_vac %>% 
  group_by(ccaa_vac) %>% 
  complete(date_vac) %>% 
  ungroup()


## -------------------------------------------------------------------------------------------------------------------------------------------
dat_es_vac$w <- dat_es_vac$poblacion/47329981

dat_es_vac$vac_dosis_entregadas_1000 <- (dat_es_vac$vac_dosis_entregadas/dat_es_vac$poblacion)*1000

dat_es_vac$vac_dosis_administradas_1000 <- (dat_es_vac$vac_dosis_administradas/dat_es_vac$poblacion)*1000

dat_es_vac$vac_dosis_pauta_completada_1000 <- (dat_es_vac$vac_dosis_pauta_completada/dat_es_vac$poblacion)*1000


## -------------------------------------------------------------------------------------------------------------------------------------------
vac_es_avg <- dat_es_vac %>% group_by(date_vac) %>% 
  summarise_at(c("vac_dosis_entregadas_1000", "vac_dosis_administradas_1000","vac_perc_entregadas","vac_dosis_pauta_completada_1000"), 
               ~ sum(.x*w))

vac_mu <- dat_es_vac %>% filter(ccaa_vac == 'Murcia, Región de')

vac_comp <- merge(vac_es_avg,vac_mu, by = 'date_vac', all = T, suffixes = c(".es",".mu"))

vac_comp[, c('poblacion', 'w', 'ccaa_vac')] <- NULL

vac_comp <- vac_comp %>% drop_na()

for (i in c(2:9)){
  vac_comp[,i] <- round(vac_comp[,i], 2)
}



## -------------------------------------------------------------------------------------------------------------------------------------------
dir.create('data')

URL_mu <- 'https://www.murciasalud.es/archivo.php?id=469031'

download.file(URL_mu, 'data/datos_municipios_hoy.csv')

municipios_hoy <- read.csv('data/datos_municipios_hoy.csv', sep = ';', skip = 3)

municipios_hoy <- municipios_hoy[1:45,]


## -------------------------------------------------------------------------------------------------------------------------------------------
names(municipios_hoy)[names(municipios_hoy) == "Casos.PCR.Ag...acum."] <- "PCR_acum"
names(municipios_hoy)[names(municipios_hoy) == "X..Total.casos"] <- "Total_casos"
names(municipios_hoy)[names(municipios_hoy) == "IA."] <- "IA"
names(municipios_hoy)[names(municipios_hoy) == "Casos.PCR.Ag...últ.14.días"] <- "PCR_14"
names(municipios_hoy)[names(municipios_hoy) == "IA..últ.14.días"] <- "IA14"
names(municipios_hoy)[names(municipios_hoy) == "Casos.PCR.Ag...últ..7.días"] <- "PCR_7"
names(municipios_hoy)[names(municipios_hoy) == "IA..últ.7.días"] <- "IA7"

for (i in c(2:9)){
  municipios_hoy[,i] <- as.numeric(gsub(',','.',municipios_hoy[,i]))
}


## -------------------------------------------------------------------------------------------------------------------------------------------
for (i in c(2:9)){
  municipios_hoy[,i] <- round(municipios_hoy[,i], 2)
}


## -------------------------------------------------------------------------------------------------------------------------------------------
URL_alertas <- 'http://www.murciasalud.es/pagina.php?id=472956&idsec=6574'

musalud.html <- read_html(URL_alertas, encoding = 'ISO-8859-1')

musalud.dat <- html_nodes(musalud.html, "div#coronavirus")


## -------------------------------------------------------------------------------------------------------------------------------------------
xml_child(xml_child(musalud.dat[[1]], 3), 1)


## -------------------------------------------------------------------------------------------------------------------------------------------
alertas <- data.frame(Municipio=character(),
                      Fase=character(),
                      Nivel_alerta=character(),
                      stringsAsFactors=FALSE)

for (i in 1:nrow(municipios_hoy)){

  x <- xml_child(xml_child(musalud.dat[[1]], 3), i)
  
  hh <- htmlParse(x, asText = T, encoding = 'UTF-8')
  
  str <- xmlToDataFrame(hh)[1,]
  
  str <- gsub(' Fase', ';Fase', str)
  
  str <- gsub('Nivel de alerta: ', ';', str)
  
  mcpo.str <- str_split(str, fixed(";"))
  
  mcpo.df <- data.frame(do.call(rbind, mcpo.str))
  
  names(mcpo.df)[names(mcpo.df) == "X1"] <- "Municipio"
  names(mcpo.df)[names(mcpo.df) == "X2"] <- "Fase"
  names(mcpo.df)[names(mcpo.df) == "X3"] <- "Nivel_alerta"
  
  alertas <- rbind(alertas, mcpo.df)
    
}


## -------------------------------------------------------------------------------------------------------------------------------------------
municipios_full <- merge(municipios_hoy, alertas, by = 'Municipio')

municipios_full$Nivel_alerta <- factor(municipios_full$Nivel_alerta, levels = c('extremo', 'muy alto', 'medio/alto', 'bajo'))


## -------------------------------------------------------------------------------------------------------------------------------------------
last_update <- xml_attrs(xml_child(xml_child(musalud.dat[[1]], 2), 1))[["datetime"]]


## -------------------------------------------------------------------------------------------------------------------------------------------
URL_shp <- 'https://github.com/jesusalcantud/covid_app_mu/raw/master/municipios_shapefiles.zip'

temp <- tempfile(fileext = ".zip")

download.file(URL_shp,temp)

unzip(temp, exdir = tempdir())

mapa <- rgdal::readOGR(dsn = tempdir(), layer = 'recintos_municipales_inspire_peninbal_etrs89')



## -------------------------------------------------------------------------------------------------------------------------------------------
cod_mu <- c(34143030001:34143030043,34143030901,34143030902)


mapa_mu <- subset(
  x = mapa, 
  subset = mapa@data$NATCODE %in% cod_mu)

Encoding(mapa_mu@data$NAMEUNIT) <- 'UTF-8'


## -------------------------------------------------------------------------------------------------------------------------------------------
municipios_full$Municipio <- ifelse(municipios_full$Municipio == 'Alcázares, Los', 'Los Alcázares', municipios_full$Municipio)
municipios_full$Municipio <- ifelse(municipios_full$Municipio == 'Unión, La', 'La Unión', municipios_full$Municipio)
municipios_full$Municipio <- ifelse(municipios_full$Municipio == 'Torres de Cotillas, Las', 'Las Torres de Cotillas', municipios_full$Municipio)


## -------------------------------------------------------------------------------------------------------------------------------------------
mapa_mu@data <- mapa_mu@data %>%
  left_join(
    y  = municipios_full,
    by = c('NAMEUNIT' = 'Municipio')
  )


## -------------------------------------------------------------------------------------------------------------------------------------------
# Transformación del mapa para tratarlo como dataframe.
mapa_df <- fortify(model = mapa, region = "NATCODE")

info_municipios <- mapa@data


info_municipios <- info_municipios %>%
  mutate(
    pais       = str_sub(string = NATCODE, start = 1, end = 2),
    c_autonoma = str_sub(string = NATCODE, start = 3, end = 4),
    provincia  = str_sub(string = NATCODE, start = 5, end = 6),
    municipio  = str_sub(string = NATCODE, start = 7, end = -1)
  ) %>%
  rename(Municipio = NAMEUNIT)

info_municipios <- info_municipios %>%
  select(
    NATCODE, Municipio, c_autonoma, provincia, municipio
  )

mapa_df <- mapa_df %>%
  left_join(info_municipios, by = c("id" = "NATCODE"))


# Extracción de datos para Murcia
mapa_df_r_murcia <- mapa_df %>% filter(c_autonoma == "14")

# Unión con nuestro dataframe de datos actuales de Murcia
Encoding(mapa_df_r_murcia$Municipio) <- 'UTF-8'

mapa_df_r_murcia <- mapa_df_r_murcia %>%
  left_join(
    y  = municipios_full,
    by = "Municipio"
  )


## -------------------------------------------------------------------------------------------------------------------------------------------
mapa <- NULL


## -------------------------------------------------------------------------------------------------------------------------------------------
save.image(file = "dataviz.RData")

