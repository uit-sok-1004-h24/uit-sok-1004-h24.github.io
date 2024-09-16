# SOK-1004 Forelesning 5

# Sammenheng mellom lønnsvekst og arbeidsledighet i Norge, 1971-2023

# Løsningsforslag

# Data fra SSB, tabell 09786 og 08517

# rydd

rm(list=ls())

# last inn pakken
library(tidyverse)



# URL (csv fil)
url <- "https://raw.githubusercontent.com/uit-sok-1004-h24/uit-sok-1004-h24.github.io/main/data/lonn_arbled_f5.csv"

# last inn fila

df_ny <- read.csv(url)


# Plott med ggplot2

df_ny %>% 
  ggplot(aes(x = År, y = Verdi, color = Variabel)) +
  geom_line() +
  geom_point() +
  labs(title = "Reell lønnsvekst og arbeidsledighet i Norge, 1971-2023",
       x = "År",
       y = "Prosent (%)",
       color = "") +
  theme_minimal()


# dersom du ikke liker fargene som R velger, kan du endre dem manuelt med

# + scale_color_manual(values = c("Lønnsvekst" = "darkblue", "Arbeidsledighet" = "purple"))

df_ny %>% 
  ggplot(aes(x = År, y = Verdi, color = Variabel)) +
  geom_line() +
  geom_point() +
  labs(title = "Reell lønnsvekst og arbeidsledighet i Norge, 1971-2023",
       x = "År",
       y = "Prosent (%)",
       color = "") +
  theme_minimal() +
  scale_color_manual(values = c("Lønnsvekst" = "darkblue", "Arbeidsledighet" = "lightgreen"))
