# Cargar Datos ------------------------------------------------------------

datos <- load("Datos/Datos_Filtados.Rdata")
Autores <- rio::import("Datos/authors.csv")
Categorias <- rio::import("Datos/categories.csv")

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
  else if (aux == "") {
    nombres = "Anonimo"
    autores_crudo = c(autores_crudo, nombres)
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


###Dataframe

contador_Autores <- plyr::count(autores_crudo)
data_frame <- as.data.frame(autores_crudo) %>% 
  mutate(freq = NA)
for (i in 1:length(data_frame$autores_crudo)){
  posicion = which(data_frame$autores_crudo[i] == contador_Autores$x)
  data_frame$freq[i] = contador_Autores$freq[posicion]
}

###Histograma
data_frame %>%
  filter(freq > 30) %>%
  ggplot(aes(autores_crudo))  +
  geom_bar(fill="thistle", col = "gray40") +
  labs(x = "Autores", y = "Frecuencia", title = "Los 11 Autores más nombrados en los 10.000 Top-Seller") +
  ggthemes::theme_base()

###Se borra la columna Authors y se añade autores
base_ajustada= datos3 %>% 
  select(-authors) %>%
  mutate(autores) 




# Reemplazar ID de Categoría -------------------------------------------------

categ <- c()
for (i in 1:length(datos3$category_id)) {
  p = datos3[i,3] %>% stringr::str_sub( start = 2L, end = -2L) %>%
    stringr::str_remove_all(" ") %>%
    stringr::str_split(",")
  categ = c(categ, p)
}

#### Cambio de ID por nombres de Categoria
categorias <- c()
categorias_crudo <-c() 
for (i in categ){
  aux = unlist(i) 
  if (length(aux) > 1){
    nombre_ctg = ""
    contador = 0
    for (j in aux){
      posicion = which(j == Categorias$category_id)
      if (contador != 0){
        nombre_ctg = paste(nombre_ctg, Categorias$category_name[posicion], sep = "; ")
      }
      else {
        nombre_ctg = Categorias$category_name[posicion]
        contador = 1
      }
      
      categorias_crudo = c(categorias_crudo, Categorias$category_name[posicion])
      
    }
  }
  else if (aux == "") {
    nombre_ctg = "Sin Especificar"
    categorias_crudo = c(categorias_crudo, nombre_ctg)
  }
  else {
    posicion = which(i == Categorias$category_id)
    nombre_ctg = Categorias$category_name[posicion]
    categorias_crudo = c(categorias_crudo, Categorias$category_name[posicion])
    
  }
  categorias = c(categorias, nombre_ctg)
}

#Guardar listas#
save(categorias, file="Datos/categorias.Rdata")
save(categorias_crudo, file="Datos/categorias_crudo.Rdata")

contador_Categorias <- plyr::count(categorias_crudo)
data_frame <- as.data.frame(categorias_crudo) %>% 
  mutate(freq = NA)
for (i in 1:length(data_frame$categorias_crudo)){
  posicion = which(data_frame$categorias_crudo[i] == contador_Categorias$x)
  data_frame$freq[i] = contador_Categorias$freq[posicion]
}

###Histograma
data_frame %>%
  filter(freq > 360) %>%
  ggplot(aes(categorias_crudo))  +
  geom_bar(fill="thistle", col = "gray40") +
  labs(x = "Categorías", y = "Frecuencia", title = "Top 10 Categorías con Mayor Cantidad de Best-seller")
