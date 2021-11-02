
# Librerías ---------------------------------------------------------------

require(tidyverse)

# Cargar Base de Datos ----------------------------------------------------

datos <- read_delim("Datos/dataset.csv")
generos <- read_delim("Datos/categories.csv")
autores <- read_delim("Datos/authors.csv")

# Conservar primeros 10.000 en el rank de bestseller -------------------------------


datos2 <- datos[(datos$`bestsellers-rank` < 10000),]
datos3 <- datos2[!is.na(datos2$`bestsellers-rank`),]


save(datos3, file="Datos/Datos_Filtados.Rdata")
