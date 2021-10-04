# Datos
Esta carpeta contiene 5 bases de datos en total, donde 4 son únicamente llaves con su respectiva información.

## Dataset.csv

Esta es la base de datos con la que se trabajará en la mayoría del tiempo. La cual contiene:
1) authors: ID-Autor(es) (lista de str)
2) bestsellers-rank: ID-Ranking de los más vendidos (int)
3) categories: ID-Categorías. (lista de str)
4) description: Descripción (str)
5) dimension-x: Dimensión X (float en cm)
6) dimension-y: Dimensión Y (float en cm)
7) dimension-z: Dimensión Z (float en mm)
8) edition: Edición (str)
9) edition-statement: Declaración de edición (str)
10) for-ages: Rango de edades (str)
11) format: ID-Formato. (int)
12) id: Identificador único (int)
13) illustrations-note:
14) image-checksum: Suma de comprobación de la imagen de portada
15) image-path: Ruta del archivo de la imagen de portada
16) image-url: URL de la imagen de portada
17) imprint: No contiene nada
18) index-date: Fecha de rastreo (date)
19) isbn10: ISBN-10 (str)
20) isbn13: ISBN-13 (str)
21) lang: Lista de los idiomas del libro
22) publication-date: Fecha de publicación (date)
23) publication-place: ID-Lugar de publicación (int)
24) rating-avg: Calificación promedio 0-5
25) rating-count: Número de calificaciones
26) title: Título del libro (str)
27) url: URL relativa ( https://bookdepository.com + url)
28) weight: Peso (en kg)

## authors.csv

Contiene 2 columnas: ID y el respectivo Nombre del Autor

## categories.csv

Contiene 2 columnas: ID y el respectivo Nombre de la Categoría (Adult, Fantasy, etc.)

## formats.csv

Contiene 2 columnas: ID y el respectivo formato (Book, CD, Audio, etc.)

## places.csv

Contiene 2 columnas: ID y el respectivo lugar de publicación
Lamentablemente no presenta datos.



### Referencias

Panagiotis, S. (2021, agosto). Conjunto de datos de depósito de libros. Kaggle. https://www.kaggle.com/sp1thas/book-depository-dataset?select=dataset.csv 
