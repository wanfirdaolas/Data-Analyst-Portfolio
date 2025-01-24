
## Investingating Fandango Movie Rating
## This data analysis is based on the article 
## "Be Suspicious Of Online Movie Ratings, Especially Fandango's" by Walt Hickey
  

install.packages("dplyr")
install.packages("tidyverse")
install.packages("readr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("tidyr")

library(dplyr)
library(tidyverse)
library(readr)
library(stringr)
library(ggplot2)
library(tidyr)

# read csv file
before <- read_csv("fandango_score_comparison.csv")
after <- read_csv("movie_ratings_16_17.csv")

head(before)
head(after)

# select columns
rating_before <- before %>%
  select(FILM, Fandango_Stars, Fandango_Ratingvalue, Fandango_votes, Fandango_Difference)

rating_after <- after %>%
  select(movie, year, fandango)

# Goal: find out if there is any difference between Fandango's ratings for 
# popular movie in 2015 and Fandango's ratings for popular movies in 2016.

set.seed(1)
sample_n(rating_before, size = 10)

# Using Hickey's benchmark of 30 fans raing and count a movie as popular if it has
# 3 fan ratings or more on Fandango's website.
sum(rating_before$Fandango_votes < 30)

head(rating_before$FILM, n = 10)
unique(rating_after$year, n = 10)

# seperate film title and year
rating_before <- rating_before %>%
  mutate(year = str_sub(rating_before$FILM, -5, -2))

table(rating_before$year)

# choose only year 2015
rating_2015 <- rating_before %>% 
  filter(year == 2015)

table(rating_2015$year)

# choose only year 2016
rating_2016 <- rating_after %>%
  filter(year == 2016)

table(rating_2016$year)

head(rating_2015, n = 10)
head(rating_2016, n = 10)

# compare distribution shape between 2015 and 2016
ggplot(data = rating_2015, 
       aes(x = Fandango_Stars)) +
  geom_density() +
  geom_density(data = rating_2016, 
               aes(x = fandango),
               color = "blue") +
  labs(title = "Comparing Distribution shape of Fandango's Ratings 2015 vs 2016") +
  labs(x = "Stars") +
  labs(y = "Density") +
  scale_x_continuous(breaks = seq(0, 5, 0.5),
                     limits = c(0, 5))

# frequency table
rating_2015 %>%
  group_by(Fandango_Stars) %>%
  summarize(percentage = n() / nrow(rating_2015) * 100)

rating_2016 %>%
  group_by(fandango) %>%
  summarize(percentage = n() / nrow(rating_2016) * 100)

# compute mean, median, mode
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

summary_2015 <- rating_2015 %>%
  summarize(year = "2015",
            mean = mean(rating_2015$Fandango_Stars),
            median = median(rating_2015$Fandango_Stars),
            mode = mode(rating_2015$Fandango_Stars))

summary_2016 <- rating_2016 %>%
  summarize(year = "2016",
            mean = mean(rating_2016$fandango),
            median = median(rating_2016$fandango),
            mode = mode(rating_2016$fandango))

# prepare data for bar chart
summary <- rbind(summary_2015, summary_2016)
print(summary)

summary <- summary%>%
  pivot_longer(names_to = "statistic", values_to = "value", - year)

ggplot(summary, aes(x = statistic, y = value, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparing summary statistics 2015 vs 2016",
       x = " ",
       y = "Stars")
