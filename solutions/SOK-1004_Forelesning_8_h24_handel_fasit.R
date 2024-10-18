# JSON API spørring til SSB

# SOK-1004 Forelesning 8
# Internasjonal handel

# Løsningsforslag

# Hent data fra SSBs tabell 10482

rm(list=ls()) 

library(rjstat)
library(httr)
library(tidyverse)

url <- "https://data.ssb.no/api/v0/no/table/10482/"

query <- '{
  "query": [
    {
      "code": "Region",
      "selection": {
        "filter": "item",
        "values": [
          "0",
          "31",
          "32",
          "30",
          "01",
          "02",
          "03",
          "34",
          "04",
          "05",
          "33",
          "06",
          "39",
          "40",
          "38",
          "07",
          "08",
          "42",
          "09",
          "10",
          "11",
          "46",
          "12",
          "14",
          "15",
          "50",
          "16",
          "17",
          "18",
          "55",
          "56",
          "54",
          "19",
          "20",
          "21",
          "22",
          "NVFF",
          "PUDT",
          "99"
        ]
      }
    },
    {
      "code": "SITC",
      "selection": {
        "filter": "item",
        "values": [
          "SITCT",
          "SITC0-1",
          "SITC03u",
          "SITC2_4",
          "SITC3b",
          "SITC5",
          "SITC6",
          "SITC7+9",
          "SITC8"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2023"
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

# her er det mange 0 verdier pga at fylket ikke eksisterer i 2023
# vi fjerner disse

df <- df %>% 
  filter(value != 0)

# så forenkler vi navnene på fylkene

forenkling <- c(
  " - Romsa ja Finnmárku \\(2020-2023\\)" = "",
  " - Nordlánnda" = "",
  " - Trööndelage" = "",
  " \\(2020-2023\\)" = ""
)


df <- df %>%
  mutate(region = str_replace_all(region, forenkling))

# vi velger ut kun fylker

utvalgte_regioner <- c(
  "Oslo",
  "Viken",
  "Innlandet",
  "Vestfold og Telemark",
  "Agder",
  "Rogaland",
  "Vestland",
  "Møre og Romsdal",
  "Trøndelag",
  "Nordland",
  "Troms og Finnmark"
)

df_utvalgt <- df %>% 
  filter(region %in% utvalgte_regioner)


# OPPGAVE 1. Bruk df_utvalgt og lag en stolpediagram som viser "Varer i alt" for hvert fylke


options(scipen = 999)
df_utvalgt %>% 
  filter(varegruppe == "Varer i alt") %>%
  ggplot(aes(x = region, y = value, fill = region)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Fastlandseksport etter produksjonsfylke (varer i alt), 2023",
       x = "Fylke",
       y = "Millioner NOK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# OPPGAVE 2. Nå skal du plotte alle varegruppene for hvert fylke. 
# Vi tar først bort "Forskjellige ferdigvarer" og "Varer i alt"

df_utvalgt_ny <- df_utvalgt %>% 
  filter(varegruppe != "Forskjellige ferdigvarer", varegruppe != "Varer i alt")

# vi bruker facet_wrap(~ varegruppe) for å få en graf per varegruppe

df_utvalgt_ny %>% 
  ggplot(aes(x = region, y = value, fill = region)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Fastlandseksport etter produksjonsfylke og varegruppe, 2023",
       x = "Fylke",
       y = "Millioner NOK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  facet_wrap(~ varegruppe)


# dersom vi får vitenskapelig notasjon på aksen kan vi fjerne med options(scipen = 999)

# vi ser at det er en varegruppe som heter "¬ Fisk"
# som er en undergruppe av "Matvarer, drikkevarer, tobakk"

df_endelig <- df_utvalgt_ny %>% 
  mutate(varegruppe = ifelse(varegruppe == "¬ Fisk", "Matvarer, drikkevarer, tobakk (Fisk)", varegruppe))


# OPPGAVE 3. Tegn samme figur som i oppgave 2 ved å bruke df_endelig.
# Denne gangen skal du bruke facet_wrap(~ varegruppe, scales = "free_y")
# Hva skjer?



df_endelig %>% 
  ggplot(aes(x = region, y = value, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Fastlandseksport etter produksjonsfylke og varegruppe, 2023",
       x = "Fylke",
       y = "Millioner NOK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position ="none") +
  facet_wrap(~ varegruppe)

# legg merke til at vi har samme skala på y-aksen for alle varegruppene
# dette kan være misvisende, vi kan endre dette ved å legge til scales = "free_y"

df_endelig %>% 
  ggplot(aes(x = region, y = value, fill = region)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Fastlandseksport etter produksjonsfylke og varegruppe, 2023",
       x = "Fylke",
       y = "Millioner NOK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position ="none") +
  facet_wrap(~ varegruppe, scales = "free_y")


