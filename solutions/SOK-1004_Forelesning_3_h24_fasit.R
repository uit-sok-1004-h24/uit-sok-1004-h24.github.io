# SOK-1004 H24 Kode til Forelesning 3

# Løsningsforslag

############################
### R-kode forelesning 3 ###
### Skrevet av:          ###
### Even S. Hvinden      ###
### Tidligere UiT        ###
### Nå: Forsvarets       ### 
###forskningsinstitutt   ###
############################

# rydd opp
rm(list=ls())

# last inn tidyverse
library(tidyverse)

############################
### data i tibble-format ### 
############################


# les CO2 data i .csv fra OWID
co2data <- read_csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

# se: https://github.com/owid/co2-data
# se en beskrivelse av data her: https://github.com/owid/co2-data/blob/master/owid-co2-codebook.csv

# se på dataene i konsollen
co2data

###########################
### kommandoen select() ### 
###########################

select(co2data,year)

# beskriv hva kommandoen gjør

###########################################################
### Oppgave 1: Kan du skrive om kommandoen med en pipe? ### 
###########################################################

co2data %>% 
  select(year)


###########################
### kommandoen filter() ### 
###########################

filter(co2data,iso_code == "SWE")

# beskriv hva kommandoen gjør

############################################################################
### Oppgave 2: Kan du lage en tabell som viser verdiene for Norge mellom ###
### årene 2018 og 2020? Bruk en pipe! Hint: kjør "1:3" i konsollen ;-)   ### 
############################################################################

co2data %>% 
  filter(iso_code == "NOR") %>% 
  filter(year == 2018:2020)

# NB vi får en advarsel fra year == 2018:2020
# dette fikser vi nedenfor 

# lager en figur med  Norge
co2data %>%
  filter(country =="Norway") %>% 
  ggplot(aes(x=year, y=co2)) %>%
  + geom_line() %>%
  + theme_minimal()

######################
### Kommandoen c() ###
######################

# c() er kort for "concatenate" -- et fancy begrep for å koble sammen
# nyttig for å lage lister
liste <- c("Ola", "Geir")

# eksempel fra i ste


co2data %>% 
  filter(iso_code=="NOR", year %in% c(2018,2019, 2020)) 
# eller
co2data %>% 
  filter(iso_code=="NOR") %>% 
  filter(year %in% c(2018,2019, 2020)) 

# eller
co2data %>% 
  filter(iso_code=="NOR") %>% 
  filter(year %in% 2018:2020) 

# hvorfor funker ikke:

co2data %>% 
  filter(iso_code=="NOR" & year==2018:2020) 

# & betyr at begge betingelser må være oppfylt
# R leter etter rader hvor iso_code er NOR OG
# år er 2018, 2019, og 2020 (eksisterer ikke)



##########################################################################
### Oppgave 3: Kan du lage en figur med CO2 utslipp for Norge og Kina? ###
##########################################################################

### Tips: Bruk filter(var %in% c("verdi1", "verdi2))
### Tips: Bruk color = country i aes()

co2data %>% 
  filter(country %in% c("Norway", "China")) %>% 
  ggplot(aes(x=year, y=co2, color = country)) %>%
  + geom_line() %>%
  + theme_minimal()
  
  
  
  
# kommenter figuren. gir den en god sammenligning?

#############################################################################
### Oppgave 4: Kan du lage en figur fra 1990 med CO2 utslipp per person for Norge, ###
### Kina, Frankrike, USA og Saudi Arabia?                                 ###
#############################################################################

countries <- c("Norway", "China", "France", "United States", "Saudi Arabia")

co2data %>% 
  filter(country %in% countries, year >=1990) %>% 
  ggplot(aes(x=year, y=co2_per_capita, color = country)) %>%
  + geom_line() %>%
  + theme_minimal()


# beskriv figuren. hva er det vi ser? hva legger du merke til? 
# hva tror du ligger bak forskjellene? 



options(scipen = 999) # fjerner vitenskapelig tall på aksen
co2data %>% 
  filter(country == "Saudi Arabia") %>% 
  filter(year >= 1990) %>% 
  ggplot(aes(x=year, y=population, color = country)) %>%
  + geom_line() %>%
  + theme_minimal()


############################
### kommandoen arrange() ### 
############################

# vi bruker arrange() til å lage en ordnet tabell

arrange(co2data, co2)

df <- co2data %>%
  arrange(gdp,methane,co2)

# skriver den ut i konsollen. 
df

# eksempel utenom datasettet

df1 <- tibble(x = c(3, 3, 4, 2), y = c("B", "A", "D", "C"))

df1


arranged_df1_x <- df1 %>% arrange(x)


arranged_df1_x

arranged_df2_x <- df1 %>% arrange(desc(x))

arranged_df2_x                                  

arranged_df1_y <- df1 %>% arrange(y)

arranged_df1_y

arranged_df1_x_y <- df1 %>% arrange(x,y)

arranged_df1_x_y


##########################################################################
### Oppgave 5: Beskriv hvordan co2data var arrangert i utgangspunktet. ###
### Kan du bruke arrange() til å transformere df tilbake og lagre?     ###
##########################################################################


df <- df %>% 
  arrange(country, year)

df
###########################
### kommandoen mutate() ### 
###########################

# Variabelen co2 viser utslipp i millioner tonn. For å få den i tonn:

co2data %>%
  mutate(co2_tonn=1e6*co2) %>% 
  filter(country =="Norway") %>% 
  ggplot(aes(x=year, y=co2_tonn)) %>%
  + geom_line() %>%
  + theme_minimal()

# variabelen consumption_co2_per_capita måler utslipp knyttet til konsum.
# variabelen co2_per_capita måler utslipp knyttet til produksjon

##############################################################################
### Oppgave 6: Variabelen consumption_co2_per_capita måler utslipp knyttet ###
### til konsum. Bruk mutate() til å lage en variabel som viser utslipp per ###
### capita utenom konsum. Gjenskap figuren med Norge, USA, Kina, Frankrike ###
### og Saudi Arabia. Kommenter. Ble du overrasket?                         ###
##############################################################################

countries <- c("Norway", "China", "France", "United States", "Saudi Arabia")


co2data %>%
  mutate(non_consumption_co2_per_capita=co2_per_capita-consumption_co2_per_capita) %>% 
  filter(country %in% countries) %>% 
  filter(year>=1990) %>% 
  ggplot(aes(x=year, y=non_consumption_co2_per_capita, color=country)) %>%
  + geom_line() %>%
  + theme_minimal()

# legg merke til at vi trenger ikke %>%  for å spesifisere lag i ggplot



co2data %>%
  mutate(non_consumption_co2_per_capita=co2_per_capita-consumption_co2_per_capita) %>% 
  filter(country %in% countries) %>% 
  filter(year>=1990) %>% 
  ggplot(aes(x=year, y=non_consumption_co2_per_capita, color=country)) +
  geom_line() +
  theme_minimal()

# vi får en advarsel i konsollen fordi variabelen som vi plotter mangler observasjoner
# disse kan vi filtrere ut


co2data %>%
  mutate(non_consumption_co2_per_capita = co2_per_capita - consumption_co2_per_capita) %>% 
  filter(country %in% countries) %>% 
  filter(year >= 1990) %>% 
  filter(!is.na(non_consumption_co2_per_capita)) %>%   # fjern NA for plottvariabelen
  ggplot(aes(x = year, y = non_consumption_co2_per_capita, color = country)) +
  geom_line() +
  theme_minimal()

# samme plott, nå uten advarsel

##############################
### kommandoen summarise() ### 
##############################

# n() gir antall observasjoner


co2data %>%
  group_by(year) %>% 
  summarise(sum = sum(co2), n=n()) 






###################################################################
### Oppgave 7: Lag en figur med totale globale utslipp over tid ### 
###################################################################

# tips, bruk na.rm = TRUE i summarise() for å fjerne manglende observasjoner


co2data %>%
  group_by(year) %>% 
  summarise(sum = sum(co2, na.rm = TRUE)) 


# prøv å summere opp

co2data %>%
  group_by(year) %>% 
  summarise(co2_sum=sum(co2, na.rm = TRUE)) %>% 
  ggplot(aes(x=year, y=co2_sum)) %>% 
  + geom_line() %>%
  + theme_minimal()

# hvor mange land i datasettet?

co2data %>%
  group_by(year) %>% 
  summarise(sum = sum(co2, na.rm = TRUE), n=n()) %>% 
  print(n=Inf) # skriv ut alle år

# 256 observasjoner, men ifølge FN finnes det ca 195 land i verden
#Hva skjer?

co2data %>% 
  distinct(country) %>% 
  print(n=Inf) # skriv ut alle land

# lag en figur med World for å se på global utslipp
# bruk variablen "World"

co2data %>%
  filter(country=="World") %>% 
  ggplot(aes(x=year, y=co2)) %>% 
  + geom_line(lwd=.5, color = "blue") %>%
  + theme_minimal()

##########################################################################
### Oppgave 8: Lag en vakker graf. Legg til ny farge, tittel, lag egne ###
### benevninger på aksene, skalering, tykkere linjer. prøv deg frem!   ### 
##########################################################################

# tips: bruk xlab(), ylab(), ggtitle()...


co2data %>%
  filter(country=="World") %>% 
  ggplot(aes(x=year, y=co2)) %>% 
  + geom_line(lwd=0.5, color = "blue") %>%
  + xlab("År") %>% 
  + ylab("CO2 utslipp mill ton") %>% 
  + ggtitle("Globale utslipp av CO2 1750-2022") %>%  
  + theme_minimal()



############################################################
# endre størrelse/farge på tittel og aksene

co2data %>%
  filter(country=="World") %>% 
  ggplot(aes(x=year, y=co2)) %>% 
  + geom_line(lwd=1.25, color = "red") %>%
  + theme_minimal() %>% 
  + xlab("År") %>% 
  + ylab("CO2 utslipp mill ton") %>% 
  + ggtitle("Globale utslipp av CO2 1750-2022") %>% 
  + theme(
    plot.title = element_text(size = 24, color = "blue"), # størrelse og farge på tittel
    axis.text.x = element_text(size = 14), #størrelse av tall på x
    axis.text.y = element_text(size = 14), # størrelse tall på y
    axis.title.x = element_text(size = 18), # størrelse tekst på x
    axis.title.y = element_text(size = 18) # størrelse tekst y
  )

# theme_minimal har som utgangspunkt 13,2 for tittel, 
# 11 for aksetittel
# og 8,8 for aksetekst

# prøv med andre tema: https://ggplot2.tidyverse.org/reference/ggtheme.html


# Legg merke til hva som skjer dersom theme_minimal kommer etter dine endringer

co2data %>%
  filter(country=="World") %>% 
  ggplot(aes(x=year, y=co2)) %>% 
  + geom_line(lwd=.5, color = "red") %>%
  + xlab("År") %>% 
  + ylab("CO2 utslipp mill ton") %>% 
  + ggtitle("Globale utslipp av CO2 1750-2022") %>% 
  + theme(
    plot.title = element_text(size = 24, color = "blue"), # størrelse og farge på tittel
    axis.text.x = element_text(size = 14), #størrelse av tall på x
    axis.text.y = element_text(size = 14), # størrelse tall på y
    axis.title.x = element_text(size = 18), # størrelse tekst på x
    axis.title.y = element_text(size = 18) # størrelse tekst y
    ) %>% 
  + theme_minimal()


#####################################################################
# bruk av labs() for å samle alle labels

co2data %>%
  filter(country=="World") %>% 
  ggplot(aes(x=year, y=co2)) %>% 
  + geom_line(lwd=.5, color = "red") %>%
  + labs(
    title = "CO2, globale utslipp",
    subtitle = "1750-2022",
    caption = "Kilde: Our World in Data",
    x = "År",
    y = "CO2 utslipp mill ton"
  ) %>% 
  + theme_minimal() %>% 
  + theme(
    plot.title = element_text(size = 24, color = "blue"), # størrelse og farge på tittel
    plot.subtitle = element_text(size = 20, color = "green"), # subtitle
    plot.caption = element_text(size = 14), # caption
    axis.text.x = element_text(size = 14), #størrelse av tall på x
    axis.text.y = element_text(size = 14), # størrelse tall på y
    axis.title.x = element_text(size = 18), # størrelse tekst på x
    axis.title.y = element_text(size = 18) # størrelse tekst y
  )
 


###############################################
# Mer avansert - du kan godt hoppe over dette!#
###############################################



# prøv å lage en dynamisk tittel 
# som henter info fra datasettet

# la oss definere lavest og høyest årstall

min_year <- co2data %>% 
  summarise(min(year, na.rm = TRUE))
min_year

max_year <- co2data %>% 
  summarise(max(year, na.rm = TRUE))
max_year

# se i "data" i "environment"

# følgende kode virker ikke ettersom ggtitle tar kun ett argument

co2data %>%
  filter(country=="World") %>% 
  ggplot(aes(x=year, y=co2)) %>% 
  + geom_line(lwd=.5, color = "blue") %>%
  + theme_minimal() %>% 
  + xlab("År") %>% 
  + ylab("CO2 utslipp mill ton") %>% 
  + ggtitle("Globale utslipp av CO2", min_year, "-", max_year) 


# vi kan sette sammen tittelen til én streng ved å bruke paste

tittel <- paste("Globale utslipp av CO2", min_year, "-", max_year)

# se i "Values" i "environment"!

# lag figur

co2data %>%
  filter(country=="World") %>% 
  ggplot(aes(x=year, y=co2)) %>% 
  + geom_line(lwd=.5, color = "red") %>%
  + theme_minimal() %>% 
  + xlab("År") %>% 
  + ylab("CO2 utslipp mill ton") %>% 
  + ggtitle(tittel)


# vi kan endre skala på aksen til (naturlig) logaritme

tittel_log <- paste(tittel,", logaritmisk skala")

co2data %>%
  filter(country=="World") %>% 
  ggplot(aes(x=year, y=log(co2))) %>% 
  + geom_line(lwd=1, color = "green") %>%
  + theme_minimal() %>% 
  + xlab("År") %>% 
  + ylab("Log CO2 utslipp mill ton") %>% 
  + ggtitle(tittel_log)

# subscript på co2


ny_tittel <- expression(paste("Globale utslipp av ", CO[2], "1750-2022"))

co2data %>%
  filter(country=="World") %>% 
  ggplot(aes(x=year, y=co2)) %>% 
  + geom_line(lwd=.5, color = "red") %>%
  + theme_minimal() %>% 
  + xlab("År") %>% 
  + ylab("CO2 utslipp mill ton") %>% 
  + ggtitle(ny_tittel)


#############################################################

# følgende gir senket skrift på CO2
# og bruker dynamiske årstall




# paste0 utelater mellomrom mellom streng

tittel_1 <- paste0("Globale utslipp av ")


range <- paste0(min_year, "-", max_year)

# bquote kombinerer ulike variabler og uttrykk
# .(tittel_1) gir oss første del av tittelen
# .(range) gir oss verdien av range i tall
# * brukes for å koble sammen, ~ er mellomrom
# [] gir senket skrift (^2 gir hevet skrift/superscript)

tittel_tekst <- bquote(.(tittel_1) * CO[2] ~ .(range))



co2data %>%
  filter(country=="World") %>% 
  ggplot(aes(x=year, y=co2)) %>% 
  + geom_line(lwd=.5, color = "green") %>%
  + theme_minimal() %>% 
  + xlab("År") %>% 
  + ylab(expression(CO[2] ~ "utslipp mill ton")) %>% 
  + ggtitle(tittel_tekst)
   
