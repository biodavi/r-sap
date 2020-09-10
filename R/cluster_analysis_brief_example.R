---
  title: "Trabajo Clase 3. Analisis de Clusters"
output: html_notebook

---
  Autor: Juan David Carvajal Agudelo, Universidad de Caldas

Cargar todas las librerias necesarias.
```{r}
library(stringr)
library(stringdist)
library(bibliometrix)
library(igraph)
library(tidyverse)
library(roadoi) # titles
library(fulltext) # Abstract
library(tm)
library(SnowballC)
library(wordcloud)
library(cluster)
library(tidyr)
library(ggplot2)
```

Archivo necesario sobre la busqueda utilizad (base de datos de scienciometrics usada en clase), y convertirla en formato de tos.
```{r}
source("ToS_old_algorithm_DS.R")
arbol <- tos_wos(c("savedrecs - 2020-08-29T093831.545.txt"))
```
Sobreescribir archivos csv, para generar outputs y archivos necesarios para el analisis.
```{r}
View(arbol$tos)
write.csv(arbol$tos, file = "arbol_scienci.csv", row.names = F)
```

```{r}
write.graph(arbol$graph, file = "red_scienc.graphml", format = "graphml")
```

```{r}
graph <- read_graph("red_scienc.graphml", format = "graphml")
```

Implementacion del algoritmo de optimización de modularidad multinivel para encontrar la estructura de la comunidad, basada en el articulo de VD Blondel, JL Guillaume, R Lambiotte y E Lefebvre.
```{r}
sub_areas <- cluster_louvain(as.undirected(graph), weights = NULL)
graph_1 <-
  graph %>%
  set_vertex_attr(name = "sub_area",
                  value = membership(sub_areas))
sub_areas
```

Recoleccion y clasificacion de datos para el uso de los clusters, fecha y longitud de comunidades.
```{r}
df_graph <-
  data.frame(vertices = V(graph_1)$name,
             sub_area = V(graph_1)$sub_area,
             stringsAsFactors = TRUE) %>%
  arrange(desc(sub_area))
df_graph$year <- str_extract(df_graph$vertices, "[0-9]+")
df_graph <- na.omit(df_graph)

sizes_sub_areas <-
  data.frame(sizes(sub_areas)) %>%
  arrange(desc(Freq))
sizes_sub_areas
```

Uso del algoritmo y los datos clasificados por modularidad para definir la produccion de articulos por año.
```{r}
all_data <-
  data.frame(Year = c(min(df_graph$year):max(df_graph$year)),
             stringsAsFactors = FALSE)
names_all_data <- list("Year")

for (i in 1:length(unique(df_graph$sub_area))){
  frecuency_sub_area <-
    df_graph %>%
    filter(sub_area == i) %>%
    count(year)
  names(frecuency_sub_area) <- c("Año", "Frecuencia")
  frecuency_sub_area$comunidades <- paste("sub_area", as.character(i), sep = "")

  frec_sub_area <- ifelse(all_data$Year %in% frecuency_sub_area$Año,
                          frecuency_sub_area$Frecuencia, 0)
  all_data <- cbind(all_data, frec_sub_area)

  names_all_data <- append(names_all_data,
                           paste("production_sub_area", as.character(i), sep = ""))
}
names(all_data) <- names_all_data
all_data
```

Graficar los principales clusters por medio de ggplot2, para verificar patrones y comunidades mas importantes.
```{r}
para_graficar <- all_data %>%
  select(Year, production_sub_area12, production_sub_area10,
         production_sub_area9, production_sub_area4) %>%
  gather(key="sub_areas", value="frecuencia", 2:5)

ggplot(para_graficar, aes(x=Year, y=frecuencia, color=sub_areas)) +
  xlim(c(1940, 2020)) +
  geom_point(size=0.6) +
  geom_line() +
  labs(title = "Producción sub_areas",
       x = "Years",
       y = "Frecuencia")
```

La optimizacion el algoritmo de modularidad puede implementarse para caracterizar comunidades usando caracteristicas de la red de citaciones. En este caso, los clusters fueron usados para clasificar de acuerdo a la produccion de los articulos estudiados en la base de datos.

Las caracteristicas usadas fueron el año y la produccion, dea cuerdo con estas clasificacicnes mediadas por el algoritmo basado en modularidad y jerarquia podemos encontrar basicamente cuatro sub areas de produccion, entre las que se encuentran las sub_areas 10, 12, 4 y 9. Esas son las sub areas que mas aportan a la red o a los clusters encontrados por la modularidad, de mayor frecuencia.

- La sub area 9, involucra las mayores producciones por año, principalmente despues de 1980 y 2000, con algunas variaciones en años posteriores.
- La sub area 12, involucra mayores producciones en los años 1995, y 2010, con variaciones en los años de >1970, >1985, y >2010. Aunque menos frecuentes.
- La sub area 4, involucra mayores producciones en los años 2000 y 2005, con pequeñas variaciones en los años 1945, 1970, 1995 y posteriores al 2010. Aunque mucho menos frecuentes.
- La sub area 10, involucra variaciones de la misma frecuencia para los años 1945, 1960, 1990, 1995, 2005, 2010 y 2020.

En conclusion podemos verificar que segun la base de datos las mayores producciones se dieron entre los años de 1980 y 2005.
