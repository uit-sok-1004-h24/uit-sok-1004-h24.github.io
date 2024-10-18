# JSON API spørring til SSB

# SOK-1004 Forelesning 9
# Utvikling i reallønn og produktivitet


rm(list=ls()) 

library(rjstat)
library(httr)
library(tidyverse)

# last inn "Bruttoprodukt per utførte timeverk. Endring fra året før (prosent). Faste priser"
# fra tabell 09174 SSB

url <- "https://data.ssb.no/api/v0/no/table/09174/"

query <- '{
  "query": [
    {
      "code": "NACE",
      "selection": {
        "filter": "vs:NRNaeringPubAgg",
        "values": []
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "BruttoprodTimev"
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

df_prod <-  hent_indeks.tmp %>%
  content("text") %>%
  fromJSONstat() %>%
  as_tibble()


# last inn "Reallønn. Årslønn, påløpt i 2010-priser. Indeks (2010=100)"
# fra tabell 09786 SSB

url2 <- "https://data.ssb.no/api/v0/no/table/09786/"

query <- '{
  "query": [
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "RealArslonnIndeks"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}'

hent_indeks.tmp <- url2 %>%
  POST(body = query, encode = "json")

df_wage <-  hent_indeks.tmp %>%
  content("text") %>%
  fromJSONstat() %>%
  as_tibble()



# følgende kode fra ChatGPT lager df_prod om til en indeks med 2010=100

# 1970 er NA så vi fjerner den

df_prod <- df_prod %>% 
  filter(år != "1970")


# Initialize the index starting from 1971
df_prod <- df_prod %>%
  arrange(år) %>%
  mutate(index = ifelse(år == 1971, 100, NA))  # Set the initial index for 1971 to 100

# Function to calculate the index
calculate_index <- function(value, initial_index) {
  cumprod(c(initial_index, 1 + value[-1] / 100))
}

# Apply the function to calculate the index
df_prod <- df_prod %>%
  mutate(index = calculate_index(value, 100))


# Rebase the index so that 2010 = 100


rebase_index <- function(data, base_year) {
  base_value <- data %>% filter(år == base_year) %>% pull(index)
  data %>%
    mutate(index = (index / base_value) * 100)
}

# Rebase the index to 2010
df_prod_rebased <- rebase_index(df_prod, 2010)


# nå kan vi kombinerer begge dataframes

# ta bort 1970 og kolonne "value" fra df_wage


df_wage <- df_wage %>% 
  filter(år != "1970") %>% 
  rename(index = value)

# ta bort kolonne "value" fra df_prod_rebased

df_prod_rebased <- df_prod_rebased %>% 
  select(-value)

df <- bind_rows(df_wage, df_prod_rebased) %>%
  arrange(år, statistikkvariabel)

# gi nye navn til statistikkvariablene

unique_names <- unique(df$statistikkvariabel)

df <- df %>%
  mutate(statistikkvariabel = ifelse(statistikkvariabel == unique_names[1], "Bruttoprodukt per timeverk","Reallønn"))



# OPPGAVE: Bruk df og lag en figur som viser utviklingen av reallønn bruttoprodukt over tid

df <- df %>%
  mutate(år = as.integer(år))

# nå kan vi plotte og sette hvilke år vi vil ha med


df %>% 
  ggplot(aes(x = år, y = index, color = statistikkvariabel, group = statistikkvariabel)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Utvikling i bruttoprodukt og reallønn i Norge, 1971-2023",
    x = "År",
    y = "2010 = 100",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(df$år), max(df$år), by = 4)) # ta med hvert 4. år

# Plott av kun reallønn (brukes i forelesning 10)
df %>% 
  filter(statistikkvariabel == "Reallønn") %>%
  ggplot(aes(x = år, y = index, color = statistikkvariabel)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Utvikling i reallønn i Norge, 1971-2023",
    x = "År",
    y = "2010 = 100",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(df$år), max(df$år), by = 4)) # ta med hvert 4. år

