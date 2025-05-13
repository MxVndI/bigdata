library(rvest)
library(stringr)

parse_museum_data <- function(file_path) {
  html <- read_html(file_path, encoding = "UTF-8")
  items <- html %>% html_nodes("li.poster__item")
  
  extract_data <- function(item) {
    tryCatch({
      
      name <- item %>%
        html_node(".poster__title a") %>%
        html_text(trim = TRUE) %>%
        str_squish()

      address <- item %>%
        html_node(".poster__addr") %>%
        html_text(trim = TRUE) %>%
        str_squish()
      
      description <- item %>%
        html_node(".poster__text") %>%
        html_text(trim = TRUE) %>%
        str_squish()
      
      relative_link <- item %>%
        html_node(".poster__title a, .poster__img-link") %>%
        html_attr("href")
      
      link <- ifelse(
        grepl("^https?://", relative_link),
        relative_link,
        paste0("https://tonkosti.ru", relative_link)
      )
      
      data.frame(
        Название = name,
        Адрес = address,
        Описание = description,
        Ссылка = link,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      message("Ошибка: ", e$message)
      return(NULL)
    })
  }
  
  result <- lapply(items, extract_data) %>% 
    Filter(Negate(is.null), .) %>%
    do.call(rbind, .)
  
  result <- result[!is.na(result$Название) & !duplicated(result$Название), ]
  return(result)
}

museum_df <- parse_museum_data("C:/Users/Vova/Desktop/Универ/BigData/bigdata/4lab/museums.html")
print(museum_df)
