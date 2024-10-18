# SOK-1004 Forelesning 10

# Arbeidstid


rm(list=ls()) 


library(tidyverse)
library(eurostat) 



df <- get_eurostat("tps00071")  %>% 
  label_eurostat() 



# OPPGAVE 1: Lag en stolpediagram av gjennomsnittlig arbeidstid per uke i 2023 for hvert land



df %>% 
  filter(TIME_PERIOD == "2023-01-01") %>%
  ggplot(aes(x = geo, y = values)) +
  geom_col(fill = "lightblue") +
  labs(
    title = "Gjennomsnittlig arbeidstid per uke 2023",
    x = "Land",
    y = "Antall timer"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Roter x-akse labels for lesbarhet
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10)
  ) +
  geom_text(aes(label = values), 
            position = position_stack(vjust = 1.1), 
            size = 2, color = "black") # Legg til verdi på stolpene

# La oss farlgelegge Norge i en annen farge
# Vi lager en ny variabel is_norway som er TRUE hvis geo er Norway, 
# ellers tar den verdi "Other"


df <- df %>% 
  mutate(is_norway = ifelse(geo == "Norway", "Norway", "Other"))

# Bruker scale_fill_manual for å spesifisere fargene på stolpene

df %>% 
  filter(TIME_PERIOD == "2023-01-01") %>%
  ggplot(aes(x = geo, y = values, fill = is_norway)) +
  geom_col() +
  scale_fill_manual(values = c(Norway = "red", Other = "lightblue")) +
  labs(
    title = "Gjennomsnittlig arbeidstid per uke 2023",
    x = "Land",
    y = "Antall timer"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Roter x-akse labels for lesbarhet
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = "none"
  ) +
  geom_text(aes(label = values, color = is_norway), 
            position = position_stack(vjust = 1.1), 
            size = 2)+ # Legg til verdi på stolpene
  scale_color_manual(values = c(Norway = "red", Other = "black")) # fargelegg verdiene


# OPPGAVE 2: Vis utviklingen over tid i gjennomsnittlig arbeidstid i Norge

df %>% 
  filter(geo == "Norway") %>%
  ggplot(aes(x = TIME_PERIOD, y = values)) +
  geom_line(color = "lightblue") +
  geom_point(color = "lightblue", size = 2) +  
  geom_text(aes(label = round(values, 1)), vjust = -0.5, size = 3.5) +  # sett in verdiene
  labs(
    title = "Gjennomsnittlig arbeidstid per uke i Norge, 2012-2023",
    x = "År",
    y = "Antall timer"
  ) +
  ylim(37, 40) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  ) 


