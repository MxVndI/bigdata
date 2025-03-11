# install.packages("readxl") 
data <- readxl::read_excel("C:/Users/Vova/Desktop/R/testirovanie_vksn_tck.xlsx") 
print(data)

max_values <- lapply(data[, -c(1,2)], max, na.rm = TRUE) 
print(max_values)
min_values <- lapply(data[, -c(1,2)], min, na.rm = TRUE) 
print(min_values)
mean_values <- lapply(data[, -c(1,2)], mean, na.rm = TRUE) 
print(mean_values)

preference_high <- colSums(data[, -c(1,2)] > 7, na.rm = TRUE)
print(preference_high)
preference_low <- colSums(data[, -c(1,2)] < 3, na.rm = TRUE)
print(preference_low)


mean_ratings <- data.frame(Фамилия = data$`Фамилия`, Оценка = data$`Общее впечатление`) 
sorted_ratings <- mean_ratings[order(-mean_ratings$Оценка), ] 
print(sorted_ratings) 


data_no_na <- na.omit(data) 
print(data_no_na) 

data_filled_na <- data[rowSums(is.na(data)) == 0, ] 
print(data_filled_na) 


new_data <- data
data_with_na_filled <- new_data
for (col in colnames(data_with_na_filled)) {
  data_with_na_filled[[col]][is.na(data_with_na_filled[[col]])] <- mean(data_with_na_filled[[col]], na.rm = TRUE)
}
print(data_with_na_filled)


filtered_data <- data[data$`Разнообразие блюд` > 8, ] 
print(filtered_data) 


mean_ratings <- colMeans(data[,-c(1,2)], na.rm = TRUE)
ratings_df <- data.frame(
  questions = names(mean_ratings),
  meanRating = mean_ratings
)
barplot(ratings_df$meanRating,  
        names.arg = ratings_df$questions,  
        main = "Распределение средней оценки",  
        ylab = "Средний рейтинг",
        las = 3, 
        col = "lightblue", 
        border = "blue")

#2.2
library(readxl) 
data_xlsx <- readxl::read_excel("C:/Users/Vova/Desktop/R/testirovanie_vksn_tck.xlsx") 
print("Данные из XLSX:") 
print(data_xlsx) 
 
desc_stats <- lapply(data_xlsx[c(-2, -1)], function(col) { 
  c( 
    min = min(col, na.rm = TRUE), 
    max = max(col, na.rm = TRUE), 
    mean = mean(col, na.rm = TRUE), 
    median = median(col, na.rm = TRUE), 
    sd = sd(col, na.rm = TRUE) 
  ) 
}) 

print("Описательные статистики:") 
print(desc_stats) 


hist(data_xlsx$`Общее впечатление`,  
     main = "Рис. 1. Распределение общего впечатления",  
     xlab = "Оценка",  
     ylab = "Частота") 

 
boxplot(data_xlsx$`Общее впечатление`,  
        main = "Рис. 2. Боксплот общего впечатления",  
        ylab = "Оценка") 


sorted_data <- data_xlsx[order(-data_xlsx$`Общее впечатление`), ] 
print("Отсортированные данные:") 
print(sorted_data) 


sub_data <- data_xlsx[data_xlsx$`Общее впечатление` > 8, ] 
print("Поднабор данных (оценка > 8):") 
print(sub_data) 


dim_sub_data <- dim(sub_data) 
print("Размерность поднабора:") 
print(dim_sub_data) 


desc_stats_sub <- lapply(sub_data[c(-2,-1)], function(col) { 
  c( 
    min = min(col, na.rm = TRUE), 
    max = max(col, na.rm = TRUE), 
    mean = mean(col, na.rm = TRUE), 
    median = median(col, na.rm = TRUE), 
    sd = sd(col, na.rm = TRUE) 
  ) 
}) 

print("Описательные статистики для поднабора:") 
print(desc_stats_sub) 


hist(sub_data$`Общее впечатление`,  
     main = "Рис. 3. Распределение общего впечатления (оценка > 8)",  
     xlab = "Оценка",  
     ylab = "Частота") 


print(names(data_xlsx))
new_data <- readxl::read_excel("C:/Users/Vova/Desktop/R/meow.xlsx")
merged_data <- rbind(data_xlsx, new_data) 


print("Объединенные данные:")
print(merged_data)


data_xlsx <- data_xlsx[, !names(data_xlsx) %in% c("Разнообразие блюд")] 
print("Данные без столбца 'Разнообразие блюд':") 
print(data_xlsx)


subset_data <- data_xlsx[data_xlsx$`Общее впечатление` > 7, ] 
print("Подмножество данных (оценка > 7):") 
print(subset_data) 



#overall_mean <- mean(ratings_df$meanRating)
#abline(h = overall_mean, col = "red", lwd = 2, lty = 2)
#legend("topright", legend = paste("Общее среднее:", round(overall_mean, 2)), 
       col = "red", lty = 2, lwd = 2)
