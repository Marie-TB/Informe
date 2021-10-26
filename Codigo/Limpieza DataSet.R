# Cargar Base de Datos ----------------------------------------------------

require(tidyverse)
datos <- read_delim("Datos/dataset.csv")
generos <- read_delim("Datos/categories.csv")

# Conservar primeros 10.000 en el rank de bestseller -------------------------------


datos2 <- datos[(datos$`bestsellers-rank` < 10000),]
datos3 <- datos2[!is.na(datos2$`bestsellers-rank`),]


# Reemplazar ID de Género -------------------------------------------------

###SUJETO A CORRECCIÓN
colnames(datos3)[3] <- "category_id"
Datos <- merge(datos3, generos)
columna = c()
for (dat in datos3[3]) {
  data_genero = c()
  for(cat in dat) {
    a = which(generos[1] == cat)
    data_genero = c(data_genero, generos[2][a,])
  }
  columna=c(columna, data_genero)
}
  



