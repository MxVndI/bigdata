library(rvest)

target_countries <- c("Russia", "Germany", "Sweden", "France", "Finland")
years <- 2014:2021
required_columns <- c("Country", "Purchasing_Power_Index", "Safety_Index", 
                      "Health_Care_Index", "Cost_of_Living_Index", 
                      "Property_Price_to_Income_Ratio", "Traffic_Commute_Time_Index", 
                      "Pollution_Index", "Climate_Index")

for (year in years) {
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)
  filename <- paste0("quality_of_life_", year, ".html")
  download.file(url, destfile = filename, quiet = TRUE)
  
  content <- read_html(filename)
  tables <- html_nodes(content, "table")
  df <- html_table(tables[[2]], fill = TRUE)[-1]
  
  colnames(df) <- c("Country", "Quality_of_Life_Index", "Purchasing_Power_Index",
                    "Safety_Index", "Health_Care_Index", "Cost_of_Living_Index",
                    "Property_Price_to_Income_Ratio", "Traffic_Commute_Time_Index",
                    "Pollution_Index", "Climate_Index")[1:length(df)]
  
  for (col in required_columns) {
    if (!col %in% colnames(df)) df[[col]] <- NA
  }
  df <- df[required_columns]
  
  for (col in setdiff(required_columns, "Country")) {
    df[[col]] <- as.numeric(gsub(",", "", as.character(df[[col]])))
  }
  
  df_filtered <- df[df$Country %in% target_countries, ]
  assign(paste0("df_", year), df)
  assign(paste0("df_filtered_", year), df_filtered)
}

combined_df <- do.call(rbind, lapply(years, function(year) {
  df <- get(paste0("df_filtered_", year))
  df$Year <- year
  return(df)
}))

index_list <- list(
  list(var = "Purchasing_Power_Index", title = "Индекс покупательной способности"),
  list(var = "Pollution_Index", title = "Индекс загрязнения"),
  list(var = "Property_Price_to_Income_Ratio", title = "Отношение цены жилья к доходу"),
  list(var = "Cost_of_Living_Index", title = "Индекс стоимости жизни"),
  list(var = "Safety_Index", title = "Индекс безопасности"),
  list(var = "Health_Care_Index", title = "Индекс здравоохранения"),
  list(var = "Traffic_Commute_Time_Index", title = "Индекс времени в пути"),
  list(var = "Climate_Index", title = "Климатический индекс", filter = function(df) subset(df, Year >= 2016))
)

colors <- rainbow(length(target_countries))
par(mfrow = c(1, 1))

for (index in index_list) {
  current_df <- if (!is.null(index$filter)) index$filter(combined_df) else combined_df
  
  plot(NA, 
       xlim = range(current_df$Year), 
       ylim = range(current_df[[index$var]], na.rm = TRUE),
       main = paste(index$title, min(current_df$Year), "-", max(current_df$Year)),
       xlab = "Год", ylab = "Значение индекса")
  
  for (i in seq_along(target_countries)) {
    country_data <- current_df[current_df$Country == target_countries[i], ]
    lines(country_data$Year, country_data[[index$var]], 
          type = "o", col = colors[i], lwd = 2)
  }
  legend("topright", legend = target_countries, col = colors, lwd = 2, bty = "n")
  
  cat("Нажмите Enter, чтобы перейти к следующему графику...\n")
  readline()
}

