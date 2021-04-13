# MAMALS

# Cargamos librerias
library(tidyverse)
library(ggplot2)
library(stats)
library(cluster)
library(factoextra)
library(NbClust)

# Cargo el dataset
mamals <- read.csv("C:/Users/nacho/Documents/CAECE/MineriaDeDatosEmpresarial/Unidad 2/mamals.csv",
                   header = T, stringsAsFactors = F)
str(mamals)

# Observamos que los 25 registros corresponden para cada mamifero univoco
# Entonces podemos eliminar la columna X y que los nombres sean el ID de las filas
unique(mamals$name)
rownames(mamals) <- mamals$name
mamals <- mamals[,-c(1,2)]
View(mamals)

# Nos fijaos si hay algun NA
is.na(mamals)

# Normalizamos los valores
df.s <- scale(mamals)
df.s <- as.data.frame(df.s)

# Nos fijamos si hay outliers mediante el grafico de boxplot
boxplot(df.s)

df.s %>% 
  filter(fat > 2 | ash > 2)

# Calculamos una matriz de distancias para ver si se puede hacer un clustering en el df
# Se puede observar en el grafico que hay ciertas tendencias en los mamiferos
m_distancia <- dist(df.s, method = "euclidean")
fviz_dist(m_distancia)

# Para estimar el numero de cluster veo algunas opciones
# Me arroja que utilice 4 clusters
NbClust(df.s, distance ="euclidean", min.nc = 2, max.nc = 8,
        method = "kmeans", index = "all")
# Con este grafico podemos observar que entre 2 y 4 clusters seria lo ideal
fviz_nbclust(df.s,kmeans, method = "wss")

# Con este grafico nos arroja un k = 2
fviz_nbclust(df.s,kmeans, method = "silhouette")


# Generamos el modelo con 4 clusters
# El modelo explica una variabilidad del 83.3%
mod_kmeans <- kmeans(x = df.s, centers = 4, nstart = 20)
mod_kmeans
fviz_cluster(mod_kmeans,data = df.s)
fviz_cluster(mod_kmeans, data = df.s, ellipse.type = "euclid", repel = TRUE, star.plot = TRUE)

# Agrego al data frame la columna cluster
df.s$Cluster <- as.factor(mod_kmeans$cluster)

# Calculamos la media de cada variable agrupando por cluster
df.s %>%
  group_by(Cluster) %>% 
    summarise_all("mean")


mod_hc <- hclust(m_distancia, method = "ward.D2")
fviz_dend(mod_hc, rect = TRUE, cex = 0.5,
          k_colors = c("blue","red"))
plot(mod_hc, hang = -0.01, cex = 0.8)
mod_hc$merge


# Transformo el data frame en un data long
# Visualizo las medias por cada una de las variables
class(df.s$Cluster)
dl <- gather(df.s, key = Concepto, value = Valor, water:ash ,factor_key = TRUE)
dl
ggplot(dl, aes(as.factor(x = Concepto), y = Valor, group = Cluster, col = Cluster)) +
  stat_summary(fun = mean, geom = "pointrange", size = 1) +
  stat_summary(geom = "line") +
  theme_classic() +
  xlab("Concepto") +
  labs(title = "Las medias por variable y cluster") +
  theme(plot.title = element_text( hjust = 0.5, face = "bold", size = 15),
        legend.title = element_text(face = "bold"))

