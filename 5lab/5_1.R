library(factoextra)
library(NbClust)
library(cluster)
library(scatterplot3d)
library(parameters)
library(mclust)
library(easystats)

data <- read.table("C:/Users/Vova/Desktop/Универ/BigData/bigdata/5lab/03_French Food/French Food Data_R.dat", header = TRUE)

# Проверяем структуру
str(data)

# Выводим первые несколько строк
head(data)

french_food_data <- data[, -c(1,2)]

#Дескриптивный анализ
summary(french_food_data)

# Стандартизация данных
scaled_ffd <- scale(french_food_data)

#Метод локтя
fviz_nbclust(scaled_ffd, kmeans, method = "wss") + ggtitle("Метод локтя")

# Метод силуэта
fviz_nbclust(scaled_ffd, kmeans, method = "silhouette") + ggtitle("Метод силуэта")

#Статистика разрыва
gap_stat <- clusGap(scaled_ffd, FUN = kmeans, nstart = 25, K.max = 5, B = 50)
fviz_gap_stat(gap_stat) + ggtitle("Статистика разрыва")

#Метод консенсуса
n_clust <- n_clusters(french_food_data,
                      package = c("NbClust", "mclust", "factoextra"),
                      standardize = FALSE)
n_clust
plot(n_clust)

optimal_k = 2

#Дендрограмма
d <- dist(scaled_ffd, method = "euclidean")
hc <- hclust(d, method = "ward.D2")
hc$labels <- data$type
plot(hc, main = "Дендрограмма", xlab = "Еда")
rect.hclust(hc, k = optimal_k, border = 2:optimal_k+1)

#Сравнительный анализ групп
clusters <- cutree(hc, k = optimal_k)

#Столбчатая диаграмма
normalized_data <- as.data.frame(scaled_ffd)
agg_norm <- aggregate(normalized_data, by = list(Cluster = clusters), FUN = mean)
barplot(
  as.matrix(agg_norm[, -1]), 
  beside = TRUE, 
  col = 2:(optimal_k + 1),
  main = "Средние значения по кластерам",
  xlab = "Переменные",
  ylab = "Нормализованные значения",
  legend.text = paste("Кластер", agg_norm$Cluster),
  args.legend = list(x = "topright", inset = c(0, 0))
)


#Боксплоты
par(mfrow = c(2, 3))
for (i in 1:ncol(french_food_data)) {
  boxplot(french_food_data[, i] ~ clusters, 
          main = colnames(french_food_data)[i],
          col = 2:(optimal_k + 1),
          xlab = "Кластер",
          ylab = "Значение")
}
par(mfrow = c(1, 1))

#K-means
set.seed(123)
km <- kmeans(scaled_ffd, centers = 3, nstart = 25)
fviz_cluster(km, data = scaled_ffd) + 
  ggtitle("K-means кластеризация")

#Scatterplot
pairs(scaled_ffd, col = km$cluster, pch = 19)

#3D визуализация
scatterplot3d(scaled_ffd[,1:3], color = km$cluster, pch = 19,
              main = "3D кластеризация",
              xlab = "Хлеб", ylab = "Овощи", zlab = "Фрукты")

