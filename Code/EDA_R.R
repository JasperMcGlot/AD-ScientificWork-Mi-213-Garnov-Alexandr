path <- "D:\\tmdb_5000_movies_clean.csv"
movies <- read.csv(path)

head(movies)

movies$release_date <- substr(movies$release_date, 6, nchar(movies$release_date) - 3)

head(movies$release_date)

unique_genres <- unique(movies$genres)
print(unique_genres)

numeric_variables <- sapply(movies, is.numeric)
numeric_data <- movies[, numeric_variables]

categorical_variables <- sapply(movies, function(x) is.factor(x) | is.character(x))
categorical_data <- movies[, categorical_variables]

numeric_data
categorical_data

library(ggplot2)

ggplot(movies, aes(x = budget)) +
  geom_histogram(binwidth = 2e7, fill = 'skyblue', color = 'black') +
  labs(title = 'Histogram of Budget', x = 'Budget', y = '') +
  scale_x_continuous(limits = c(0, 280e6), breaks = seq(0, 280e6, by = 40e6), labels = scales::comma) +
  coord_cartesian(ylim = c(0, 1250)) +
  theme_minimal()

ggplot(movies, aes(x = revenue)) +
  geom_histogram(binwidth = 2e8, fill = 'skyblue', color = 'black') +
  labs(title = 'Histogram of Revenue', x = 'Revenue', y = '') +
  scale_x_continuous(limits = c(0, 1.5e9), breaks = seq(0, 1.5e9, by = 200e6), labels = scales::comma) +
  coord_cartesian(ylim = c(0, 1000)) +
  theme_minimal()

ggplot(movies, aes(x = runtime)) +
  geom_histogram(binwidth = 10, fill = 'skyblue', color = 'black', na.rm = TRUE) +
  labs(title = 'Histogram of Runtime', x = 'Runtime', y = '') +
  scale_x_continuous(limits = c(0, 260), breaks = seq(0, 260, by = 20)) +
  theme_minimal()

genres_counts <- table(movies$genres)

genres_data <- data.frame(genre = names(genres_counts), count = as.numeric(genres_counts))

ggplot(genres_data, aes(x = genre, y = count, fill = genre)) +
  geom_bar(stat = "identity") +
  labs(title = 'Number of Movies by Genre', x = 'Genre', y = 'Count') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

movies$language_group <- ifelse(!is.na(movies$spoken_language), 
                                ifelse(movies$spoken_language == "en", "en", "other"), 
                                NA)
movies <- na.omit(movies)


ggplot(movies, aes(x = language_group, fill = language_group)) +
  geom_bar() +
  labs(title = 'Number of Movies by Language', x = 'Language', y = 'Count') +
  theme_minimal() +
  scale_fill_manual(values = c("en" = "blue", "other" = "red"))  # Установка цветов

library(dplyr)

film_counts <- movies %>%
  filter(!is.na(production_companies)) %>%  # Исключение записей с NA
  group_by(production_companies) %>%
  summarise(total_films = n()) %>%
  filter(total_films > 50) %>%
  arrange(desc(total_films))

ggplot(film_counts, aes(x = reorder(production_companies, -total_films), y = total_films)) +
  geom_bar(stat = "identity", fill = 'skyblue', color = 'black') +
  labs(title = 'Number of Films by Production Companies', x = 'Production Companies', y = 'Total Films') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = total_films), vjust = -0.5, size = 3, color = 'black') 



film_counts <- movies %>%
  group_by(production_companies) %>%
  summarise(total_films = n()) %>%
  mutate(company_group = ifelse(total_films > 30, as.character(production_companies), 'Other')) %>%
  group_by(company_group) %>%
  summarise(total_films = sum(total_films)) %>%
  filter(!is.na(company_group)) %>%
  arrange(desc(total_films))

ggplot(film_counts, aes(x = reorder(company_group, -total_films), y = total_films)) +
  geom_bar(stat = "identity", fill = ifelse(film_counts$company_group == 'Other', 'orange', 'skyblue'), color = 'black') +
  labs(title = 'Number of Films by Production Companies', x = 'Production Companies', y = 'Total Films') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = total_films), vjust = -0.5, size = 3, color = 'black') +
  scale_fill_manual(values = c('skyblue', 'orange'))


movies$isPaidOff <- ifelse(movies$revenue > movies$budget * 3, "Yes",
                           ifelse(movies$revenue > movies$budget, "Refunded the Costs", "No"))

head(movies[, c("revenue", "budget", "isPaidOff")])

ggplot(movies, aes(x = isPaidOff, fill = isPaidOff)) +
  geom_bar() +
  labs(title = 'Distribution of isPaidOff', x = 'isPaidOff', y = 'Count') +
  theme_minimal()

paid_off_counts <- movies %>%
  filter(isPaidOff == "Yes") %>%
  group_by(production_companies) %>%
  summarise(count = n()) %>%
  filter(count > 50) %>%
  arrange(desc(count))

# Создание гистограммы с ggplot2
ggplot(paid_off_counts, aes(x = reorder(production_companies, -count), y = count)) +
  geom_bar(stat = "identity", fill = 'skyblue', color = 'black') +
  labs(title = 'Number of Films with "Yes" in isPaidOff by Production Companies', 
       x = 'Production Companies', y = 'Count') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = count), vjust = -0.5, size = 3, color = 'black') 


country_counts <- table(unlist(strsplit(as.character(movies$production_countries), ',')))

country_data <- data.frame(country = names(country_counts), count = as.numeric(country_counts))

filtered_country_data <- subset(country_data, count > 30)

ggplot(filtered_country_data, aes(x = reorder(country, -count), y = count, fill = country)) +
  geom_bar(stat = "identity") +
  labs(title = 'Number of Movies by Production Country', x = 'Production Country', y = 'Number of Movies') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

movies$scale <- ifelse(movies$budget > 80e6, "Large Budget",
                       ifelse(movies$budget > 20e6, "Average Budget", "Low Budget"))

head(movies[, c("budget", "scale")])

ggplot(movies, aes(x = scale, fill = scale)) +
  geom_bar() +
  labs(title = 'Distribution of Budget Scales', x = 'Budget Scale', y = 'Count') +
  theme_minimal()

percentage_paid_off_by_scale <- movies %>%
  group_by(scale) %>%
  summarise(total_films = n(),
            paid_off_films = sum(isPaidOff == "Yes")) %>%
  filter(total_films > 0) %>%
  arrange(desc(total_films)) %>%
  mutate(percentage_paid_off = (paid_off_films / total_films) * 100)

ggplot(percentage_paid_off_by_scale, aes(x = scale, y = percentage_paid_off, fill = scale)) +
  geom_bar(stat = "identity") +
  labs(title = 'Percentage of Films with "Yes" in isPaidOff by Budget Scale', 
       x = 'Budget Scale', y = 'Percentage Paid Off') +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()

genres_paid_off_counts <- movies %>%
  filter(isPaidOff == "Yes") %>%
  separate_rows(genres, sep = ", ") %>%
  group_by(genres) %>%
  summarise(count = n()) %>%
  filter(count > 0) %>%
  arrange(desc(count))

ggplot(genres_paid_off_counts, aes(x = reorder(genres, -count), y = count)) +
  geom_bar(stat = "identity", fill = 'skyblue', color = 'black') +
  labs(title = 'Number of Films with "Yes" in isPaidOff by Genres', 
       x = 'Genres', y = 'Count') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = count), vjust = -0.5, size = 3, color = 'black') 

count_by_release_date <- movies %>%
  filter(isPaidOff == "Yes") %>%
  group_by(release_date) %>%
  summarise(count = n()) %>%
  arrange(release_date)

ggplot(count_by_release_date, aes(x = release_date, y = count)) +
  geom_bar(stat = "identity", fill = 'skyblue', color = 'black') +
  labs(title = 'Number of Films with "Yes" in isPaidOff by Release Date', 
       x = 'Release Date', y = 'Count') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


unique_languages <- unique(movies$original_language)

print(unique_languages)


movies$spoken_languages <- ifelse(movies$spoken_languages == "en", movies$spoken_languages, "other")

print(movies)


company_counts <- table(movies$production_companies)

selected_companies <- names(company_counts[company_counts > 90])

movies$production_companies <- ifelse(movies$production_companies %in% selected_companies,
                                          movies$production_companies, "other")

head(movies)

library(dplyr)

movies <- movies %>%
  group_by(production_countries) %>%
  mutate(country_count = n()) %>%
  mutate(production_countries = ifelse(country_count > 50, as.character(production_countries), "other")) %>%
  select(-country_count)


movies <- movies[complete.cases(movies$production_countries), ]

movies <- movies[complete.cases(movies$genres), ]

movies <- movies[complete.cases(movies$spoken_languages), ]

movies <- movies[complete.cases(movies$release_date), ]




