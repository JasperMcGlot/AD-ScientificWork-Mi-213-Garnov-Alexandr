library(jsonlite)
library(stringr)

path <- "D:\\tmdb_5000_movies.csv"
movies <- read.csv(path)


# очистка genres
extract_first_genre <- function(json_string) {
  matches <- str_extract(json_string, '"name":\\s*"([^"]+)"')
  if (!is.na(matches[1])) {
    return(matches[1])
  } else {
    return(NA)
  }
}

extract_first_lang <- function(json_string) {
  matches <- str_extract(json_string, '"iso_639_1":\\s*"([^"]+)"')
  if (!is.na(matches[1])) {
    return(matches[1])
  } else {
    return(NA)
  }
}

movies$first_genre <- sapply(movies$genres, extract_first_genre)

head(movies[, c("genres", "first_genre")])

clean_first_genre <- function(genre) {
  return(sub('"name":\\s*"', '', genre))
}

movies$first_genre <- sapply(movies$first_genre, clean_first_genre)

movies$genres <- movies$first_genre

movies <- subset(movies, select = -c(first_genre))

head(movies[, "genres"])

movies$genres <- sub('"$', '', movies$genres)

head(movies[, "genres"])

# очистка production_companies

movies$pr <- sapply(movies$production_companies, extract_first_genre)

head(movies[, c("pr", "production_companies")])

movies$pr <- sapply(movies$pr, clean_first_genre)

movies$pr <- sub('"$', '', movies$pr)

movies$production_companies <- movies$pr

movies <- subset(movies, select = -c(pr))

# очистка keywords 

movies$pr <- sapply(movies$keywords, extract_first_genre)

head(movies[, c("pr", "keywords")])

movies$pr <- sapply(movies$pr, clean_first_genre)

movies$pr <- sub('"$', '', movies$pr)

movies$keywords <- movies$pr

movies <- subset(movies, select = -c(pr))

# очистка production_countries 

movies$pr <- sapply(movies$production_countries, extract_first_genre)

head(movies[, c("pr", "production_countries")])

movies$pr <- sapply(movies$pr, clean_first_genre)

movies$pr <- sub('"$', '', movies$pr)

movies$production_countries <- movies$pr

movies <- subset(movies, select = -c(pr))

# очистка spoken_languages 

movies$pr <- sapply(movies$spoken_languages, extract_first_lang)

head(movies[, c("pr", "spoken_languages")])

movies$pr <- sub('"iso_639_1":\\s*"', '', movies$pr)

head(movies[, "pr"])

movies$pr <- sub('"$', '', movies$pr)

movies$spoken_languages <- movies$pr

movies <- subset(movies, select = -c(pr))

# Задайте путь для сохранения файла
save_path <- "D:\\tmdb_5000_movies.csv"

# Сохраните переменную movies в файл
write.csv(movies, file = save_path, row.names = FALSE)






