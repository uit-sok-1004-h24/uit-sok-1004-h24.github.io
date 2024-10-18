

# SOK-1004 Forelesning 6
# Måling av ulikhet

# Lag figur med P90/P10 og S80/S20 over tid

rm(list=ls()) 

library(rjstat)
library(httr)
library(tidyverse)

# hent data fra SSB

url <- "https://data.ssb.no/api/v0/no/table/07756/"

query <- '{
  "query": [
    {
      "code": "Forbruksenhet2",
      "selection": {
        "filter": "item",
        "values": [
          "01",
          "99"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Ginikoeffisient",
          "P90P10",
          "S80S20"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'

hent_indeks.tmp <- url %>%
  POST(body = query, encode = "json")

df <-  hent_indeks.tmp %>%
  content("text") %>%
  fromJSONstat() %>%
  as_tibble()














# vi velger ut de dataene som er interessant for oss og lage et nytt datasett

utvalgt_data <- df %>%
  filter(statistikkvariabel %in% c("P90/P10", "S80/S20")) %>% 
  filter(person == "Hele befolkningen")

# plott verdiene

utvalgt_data %>% 
  ggplot(aes(x = år, y = value, color = statistikkvariabel, group = statistikkvariabel)) +
  geom_line() +
  geom_point() +
  labs(
    title = "P90/P10 og S80/S20 i Norge, 1986-2022 (Hele befolkningen)",
    x = "År",
    y = "Verdi",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
  )

# siste linje roterer x-aksen 45 grader og setter teksten til høyre

# dersom vi vil utelate enkelte år på x-aksen, kan vi gjøre det slik:
# først må vi endre år fra <chr> til <int> (evt <numeric>)

utvalgt_data <- utvalgt_data %>%
  mutate(år = as.integer(år))

# Nå kan vi plotte og sette hvilke år vi vil ha med
# tittelen er også dynamisk nå - vi bruker min og maks år fra datasettet

utvalgt_data %>% 
  ggplot(aes(x = år, y = value, color = statistikkvariabel, group = statistikkvariabel)) +
  geom_line() +
  geom_point() +labs(
    title = paste(
      "P90/P10 og S80/S20 i Norge,",
      min(utvalgt_data$år),
      "-",
      max(utvalgt_data$år),
      "(Hele befolkningen)"
    ),
    x = "År",
    y = "Verdi",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(utvalgt_data$år), max(utvalgt_data$år), by = 4)) # ta med hvert 4. år
