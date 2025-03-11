ibrary(readxl)
data_xlsx <- read_excel("C:/Users/Vova/Desktop/R/testirovanie_vksn_tck.xlsx")
print("Данные из XLSX:")
print(data_xlsx)

#________________________________________
modas <- c()
for (col in names(data_xlsx[c(-2,-1)])) {
  column_data <- data_xlsx[[col]]
  data_for_moda <- table(column_data)
  moda <- as.numeric(names(data_for_moda)[which.max(data_for_moda)])
  modas <- c(modas, moda)
}
desc_stats <- summary(data_xlsx[c(-2,-1)])
desc_stats_with_moda <- rbind(desc_stats, Moda = modas)
print("Дескрептивный анализ:")
print(desc_stats_with_moda)

hist(data_xlsx$`Общее впечатление`, 
     main = "Рис. 1. Распределение общего впечатления", 
     xlab = "Оценка", 
     ylab = "Частота")

data_xlsx.boxplot()
boxplot(data_xlsx[c(-2,-1)], 
        main = "Рис. 2. Боксплот общего впечатления", 
        ylab = "Оценка")

#________________________________________
sorted_data <- data_xlsx[order(-data_xlsx$`Разнообразие блюд`), ]
print("Отсортированные данные:")
print(sorted_data)
#________________________________________

sub_data <- data_xlsx[data_xlsx$`Общее впечатление` > 8, ]
print("Поднабор данных (оценка > 8):")
print(sub_data)

dim_sub_data <- dim(sub_data)
print("Размерность поднабора:")
print(dim_sub_data)
print(sub_data)

modas <- c()
for (col in names(sub_data[c(-2,-1)])) {
  column_data <- sub_data[c(-2,-1)][[col]]
  data_for_moda <- table(column_data)
  moda <- as.numeric(names(data_for_moda)[which.max(data_for_moda)])
  modas <- c(modas, moda)
}
desc_stats <- summary(sub_data[c(-2,-1)])
desc_stats_with_moda <- rbind(desc_stats, Moda = modas)
print("Дескриптивный анализ:")
print(desc_stats_with_moda)

hist(sub_data$`Общее впечатление`, 
     main = "Рис. 3. Распределение общего впечатления (оценка > 8)", 
     xlab = "Оценка", 
     ylab = "Частота")

#________________________________________

new_data1 <- data.frame(
  Участники = c("Петров Иван", "Сидорова Анна"),
  `Разнообразие блюд` = c(9, 7),
  `Общее впечатление` = c(9, 8)
)
new_data2 <- data.frame(
  Участники = c("Кто Джебен", "Фанат Папича"),
  `Разнообразие блюд` = c(9, 7),
  `Общее впечатление` = c(1, 1)
)

merged_data <- rbind(new_data1, new_data2)
print("Объединенные данные:")
print(merged_data)

new_row <- data.frame(
  Участники = "Вудушающий Гений",
  `Разнообразие блюд` = 8,
  `Общее впечатление` = 9
)

merged_data <- rbind(merged_data, new_row)
print("Данные с добавленной строкой:")
print(merged_data)

merged_data_without <- merged_data[, !names(merged_data) %in% c("Разнообразие.блюд")]
print("Данные без столбца 'Разнообразие блюд':")
print(merged_data_without)

subset_data <- merged_data[merged_data$`Общее.впечатление` > 7, ]
print("Подмножество данных (оценка > 7):")
print(subset_data)