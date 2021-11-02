# Cargar Datos ------------------------------------------------------------

datos <- load("Datos/Datos_Filtados.Rdata")
Autores <- rio::import("Datos/authors.csv")

# Cargar Librerías --------------------------------------------------------
#nota ver librería regex
require(tidyverse)


# Reemplazar ID de Autores ------------------------------------------------
autor <- c()
for (i in 1:length(datos3$authors)) {
  p = datos3[i,1] %>% stringr::str_sub( start = 2L, end = -2L) %>%
    stringr::str_remove_all(" ") %>%
    stringr::str_split(",")
  autor = c(autor, p)
}
length(autor[3])

#### Cambio de ID por Nombres
autores <- c()
autores_crudo <-c() 
for (i in autor){
  aux = unlist(i) 
  if (length(aux) > 1){
    nombres = ""
    contador = 0
    for (j in aux){
      posicion = which(j == Autores$author_id)
      if (contador != 0){
        nombres = paste(nombres, Autores$author_name[posicion], sep = "; ")
      }
      else {
        nombres = Autores$author_name[posicion]
        contador = 1
      }
      
      autores_crudo = c(autores_crudo, Autores$author_name[posicion])
      
    }
  }
  else {
    posicion = which(i == Autores$author_id)
    nombres = Autores$author_name[posicion]
    autores_crudo = c(autores_crudo, Autores$author_name[posicion])
    
  }
  autores = c(autores, nombres)
}

save(autores, file="Datos/autores.Rdata")
save(autores_crudo, file="Datos/autores_crudo.Rdata")

contador_Autores <- plyr::count(autores_crudo)
data_frame <- as.data.frame(autores_crudo) %>% 
  mutate(freq = NA)
for (i in 1:length(data_frame$autores_crudo)){
  posicion = which(data_frame$autores_crudo[i] == contador_Autores$x)
  data_frame$freq[i] = contador_Autores$freq[posicion]
}

datos3[1]
data_frame %>%
  filter(freq > 30) %>%
  ggplot(aes(autores_crudo))  +
  geom_bar(fill="thistle", col = "gray40") +
  labs(x = "Autores", y = "Frecuencia", title = "Top 10 Autores con Mayor Cantidad de Best-seller")

###Se borra la columna Authors y se añade autores
base_ajustada= datos3 %>% 
  select(-authors) %>%
  mutate(autores) 







# Reemplazar ID de Género -------------------------------------------------

###SUJETO A CORRECCIÓN (Incompleto)
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




