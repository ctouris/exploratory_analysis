library(tidyverse)
library(dplyr)
library(VIM)
library(lattice)
library(mice)


# CARGANDO CSV DESDE INTERNET

data <- read.csv(file="http://data.un.org/_Docs/SYB/CSV/SYB63_1_202105_Population,%20Surface%20Area%20and%20Density.csv",sep=",", header = T, skip = 1)
head(data)
View(data)

# MAGENES Y RESETEAR ENTORNO GRAFICO

par(mar=c(0,0,0,0))
dev.off()


# CAMBIO DE NOMBRES DE COLUMNAS

nombres_de_col <- names(data)
nombres_de_col = c('Region/Country/Area', 'Country', 'Year', 'Series', 'Value', 'Footnotes', 'Source')
names(data) = nombres_de_col
View(data)


# ELIMINO DATOS DE AREAS GLOBALES

data1 <- data[-1:-812,]
View(data1)
str(data1) 


# CONVIERTO A NUMERIC LAS COLUMNAS QUE CORRESPONDEN

data1$'Region/Country/Area' = as.numeric(data1$'Region/Country/Area')
data1$'Year' = as.integer(data1$'Year')
data1$'Value' = as.numeric(data1$'Value')
str(data1)


# ELIMINAR NA

!is.na(data1)

data1 = na.omit(data1)
View(data1)


# GRAFICANDO BOXPLOT variable de Population mid-year estimates (millions), para el año 2005, para todos los países del mundo

library(ggplot2)


# CAMBIANDO DATA TYPE DE COLUMNA & FILTRADO DE DATOS PARA ARMAR EL BOXPLOT

data1$Series = as.factor(data1$Series)
data_filtro_05=data1[data1$Year == 2005 & data1$Series == "Population mid-year estimates (millions)",c('Region/Country/Area','Country', 'Year', 'Series', 'Value', 'Footnotes', 'Source')]
View(data_filtro_05)


# BOX PLOT 

boxplot(data_filtro_05$Value)


#OUTLIERS

outliers = boxplot(data_filtro_05$Value)$out
outliers
which(data_filtro_05$Value %in% outliers)
data_filtro_05[which(data_filtro_05$Value %in% outliers),]
data_filtro_05 <- data_filtro_05[-which(data_filtro_05$Value %in% outliers),]
boxplot(data_filtro_05$Value)
View(data_filtro_05) 


# FILTRADO NUEVO DATA FRAME CON 6 PAISES

Uruguay = data1[data1$'Country' == 'Uruguay',c('Region/Country/Area','Country', 'Year', 'Series', 'Value', 'Footnotes', 'Source')]
Argentina = data1[data1$'Country' == 'Argentina',c('Region/Country/Area','Country', 'Year', 'Series', 'Value', 'Footnotes', 'Source')]
España = data1[data1$'Country' == 'Spain',c('Region/Country/Area','Country', 'Year', 'Series', 'Value', 'Footnotes', 'Source')]
South_Africa = data1[data1$'Country' == 'South Africa',c('Region/Country/Area','Country', 'Year', 'Series', 'Value', 'Footnotes', 'Source')]
Australia = data1[data1$'Country' == 'Australia',c('Region/Country/Area','Country', 'Year', 'Series', 'Value', 'Footnotes', 'Source')]
Portugal = data1[data1$'Country' == 'Portugal',c('Region/Country/Area','Country', 'Year', 'Series', 'Value', 'Footnotes', 'Source')]
View(Uruguay)
boxplot(Uruguay$Value)
paises = rbind.data.frame(Uruguay, Argentina, España, South_Africa, Australia, Portugal)
View(paises)
paises$Country = as.factor(paises$Country)
boxplot_paises = boxplot(paises$Value ~ paises$Country)


#OUTLIERS 6 PAISES (URU, ARG, ESP, SOUTH_AFR, AUST, PORT)

outliers_paises = boxplot(paises$Value)$out
outliers_paises
which(paises$Value %in% outliers_paises)
paises[which(paises$Value %in% outliers_paises),]
paises = paises[-which(paises$Value %in% outliers_paises),]
boxplot(paises$Value)
View(paises) 
boxplot_paises = boxplot(paises$Value ~ paises$Country)


#ESTUDIO DE 2 VARIABLES PARA 6 PAISES

#PAISES_MALES

paises_males = paises[paises$'Series' == 'Population mid-year estimates for males (millions)' ,c('Region/Country/Area','Country', 'Year', 'Series', 'Value', 'Footnotes', 'Source')]
View(paises_males)

fill <- "#4271AE"
line <- "#1F3552"

boxplot_paises_males <- ggplot(paises_males, aes(x = Country, y = Value)) +
  geom_boxplot(fill = fill, colour = line) +
  scale_y_continuous(name = "Population mid-year estimates for males (millions)",
                     breaks = seq(0, 30, 10),
                     limits=c(0, 30)) +
  scale_x_discrete(name = "Country") +
  ggtitle("Boxplot: Population mid-year estimates for males")
boxplot_paises_males


#PAISES_FEMALES

paises_females = paises[paises$'Series' == 'Population mid-year estimates for females (millions)' ,c('Region/Country/Area','Country', 'Year', 'Series', 'Value', 'Footnotes', 'Source')]
View(paises_females)

fill <- "gold1"
line <- "goldenrod2"

boxplot_paises_females <- ggplot(paises_females, aes(x = Country, y = Value)) +
 geom_boxplot(fill = fill, colour = line) +
 scale_y_continuous(name = "Population mid-year estimates for females (millions)",
                     breaks = seq(0, 30, 10),
                     limits=c(0, 30)) +
  scale_x_discrete(name = "Country") +
  ggtitle("Boxplot: Population mid-year estimates for females")
boxplot_paises_females


# VALORES FALTANTES

# SACO DE LOS 2 DATASET COLUMNAS QUE NO NECESITO

paises_f = paises_females[,c(2,3,5)]
View(paises_f)

paises_m = paises_males[,c(2,3,5)]
View(paises_m)

library(zoo)

missingDataYears = c(2006,2007,2008,2009,2011,2012,2013,2014,2015,2016)
paises_lista = c('Uruguay', 'Argentina', 'Spain', 'South Africa', 'Australia', 'Portugal')

data_faltante = merge(paises_lista, missingDataYears)
data_faltante$Value = NA

columnas_datafaltante = names(data_faltante)
columnas_datafaltante = c( 'Country', 'Year', 'Value')
names(data_faltante) = columnas_datafaltante
View(data_faltante)


# DATA MALES FALTANTE + COMPLETA

dataM_completa = rbind(data_faltante,paises_m)

ordenadoM = arrange(dataM_completa, Country, Year)

View(ordenadoM)


# CALCULANDO VALORES NA MALES

ordenadoM$Value=na.approx(ordenadoM$Value)

View(ordenadoM)

# DATA FEMALES FALTANTE + COMPLETA

dataF_completa = rbind(data_faltante,paises_f)

ordenadoF = arrange(dataF_completa, Country, Year)

View(ordenadoF)

# CALCULANDO VALORES NA FEMALES

ordenadoF$Value=na.approx(ordenadoF$Value)

View(ordenadoF)


# GRAFICA MALES

ggplot(ordenadoM, aes(y=Value,color=Country)) + geom_boxplot() + ggtitle("Population mid-year estimates for males(millions)")

ggplot(ordenadoM, aes(y=Value,fill=Country)) + geom_boxplot()+ ggtitle("Population mid-year estimates for males(millions)")

ggplot(ordenadoM, aes(x=Value,fill=Country)) + geom_density(alpha=0.5)+ ggtitle("Population mid-year estimates for males(millions)")

ggplot(ordenadoM,aes(x = Value, fill = Country)) + geom_histogram()+ ggtitle("Population mid-year estimates for males(millions)")


# GRAFICA FEMALES

ggplot(ordenadoF, aes(y=Value,color=Country)) + geom_boxplot() + ggtitle("Population mid-year estimates for males(millions)")

ggplot(ordenadoF, aes(y=Value,fill=Country)) + geom_boxplot()+ ggtitle("Population mid-year estimates for males(millions)")

ggplot(ordenadoF, aes(x=Value,fill=Country)) + geom_density(alpha=0.5)+ ggtitle("Population mid-year estimates for males(millions)")

ggplot(ordenadoF,aes(x = Value, fill = Country)) + geom_histogram()+ ggtitle("Population mid-year estimates for males(millions)")


# REPORTE 5 PAISES CON MAYOR Y 5 CON MENOR “Sex ratio (males per 100 females)”

paises_sratio = data1[data1$Year <= 2010 & data1$'Series' == 'Sex ratio (males per 100 females)' ,c('Country', 'Year', 'Series', 'Value')]
paises_sratio = paises_sratio[paises_sratio$Year >= 2005 & paises_sratio$'Series' == 'Sex ratio (males per 100 females)',c('Country', 'Year', 'Series', 'Value')]
paises_sratio_ordenado = arrange(paises_sratio, Value)
View(paises_sratio)
View(paises_sratio_ordenado)
paises_menor_sratio = head(paises_sratio_ordenado)
View(paises_menor_sratio)
paises_mayor_sratio = tail(paises_sratio_ordenado)
View(paises_mayor_sratio)
