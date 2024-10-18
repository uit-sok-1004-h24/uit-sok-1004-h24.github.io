# JSON API spørring til SSB

# SOK-1004 Forelesning 7
# Produktivitet


rm(list=ls()) 

library(rjstat)
library(httr)
library(tidyverse)


url <- "https://data.ssb.no/api/v0/no/table/09174/"

query <- '{
  "query": [
    {
      "code": "NACE",
      "selection": {
        "filter": "vs:NRNaeringPubAgg",
        "values": [
          "pub2X03",
          "pub2X35",
          "pub2X41_43",
          "pub2X45_47",
          "pub2X85"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "ProduksjonTimev"
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

# endre år til numerisk verdi

df <- df %>% 
  mutate(år = as.integer(år))

df %>% 
  ggplot(aes(x = år, y = value, color = næring, group = næring)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Produktivitet i utvalgte næringer, 1970-2023",
    subtitle = "Produksjon per utførte timeverk. Endring fra året før (prosent). Faste priser",
    x = "År",
    y = "Prosent (%)",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(df$år), max(df$år), by = 4)) # ta med hvert 4. år

# prøv å lage bedre oversikt med facet_wrap()

df %>% 
  ggplot(aes(x = år, y = value, color = næring, group = næring)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Produktivitet i utvalgte næringer, 1970-2023",
    subtitle = "Produksjon per utførte timeverk. Endring fra året før (prosent). Faste priser",
    x = "År",
    y = "Prosent (%)",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(df$år), max(df$år), by = 4)) + # ta med hvert 4. år
  facet_wrap(~ næring)

# litt vanskelig å lese fordi én næring har store endringer
# bruk scales = "free" eller scales = "free_y" i facet_wrap()
# ser du forskjellen?

df %>% 
  ggplot(aes(x = år, y = value, color = næring, group = næring)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Produktivitet i utvalgte næringer, 1970-2023",
    subtitle = "Produksjon per utførte timeverk. Endring fra året før (prosent). Faste priser",
    x = "År",
    y = "Prosent (%)",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(df$år), max(df$år), by = 4)) + # ta med hvert 4. år
  facet_wrap(~ næring, scales = "free_y")

# ta med en linje på y=0?
# bruk scales = "free" her

df %>% 
  ggplot(aes(x = år, y = value, color = næring, group = næring)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Produktivitet i utvalgte næringer, 1970-2023",
    subtitle = "Produksjon per utførte timeverk. Endring fra året før (prosent). Faste priser",
    x = "År",
    y = "Prosent (%)",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = seq(min(df$år), max(df$år), by = 4)) + # ta med hvert 4. år
  facet_wrap(~ næring, scales = "free")


