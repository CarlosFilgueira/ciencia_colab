###Disciplina de Ciencia Colaborativa###
##Atividades 01 e 02##

## ATIVIDADE 01 - Planilhamento de Dados ##

#Carregando Pacotes

library(tidyverse)
library(taxize)
library(vegan)
library(rgbif)
library(dplyr)

#Montar planilha de dados
data.frame(
  participante = seq(1, 10, 1),
  fichas = c(read.csv("~/github/cursodados/data/iris_mod.csv", header = T) %>% 
               distinct("A116, A25, A69, A114, A21, A113, A95, A94, A26, A10, A136, A129, A147, A79, A115") %>% 
               pull()%>% 
               sample()),
  n = 'amostras'
) %>% 
  pivot_wider(
    names_from = "n", 
    values_from = "fichas"
  ) %>% 
  knitr::kable()

?bind_rows()
plan1 <- read.csv("atividade1_carlos_filgueira.csv", header = TRUE)
plan2 <- read.csv("atividade1_CAROLINA-OLIVEIRA-VALE.csv", header = TRUE)
plan3 <- read.csv("atividade1_GABRIEL-DEPIANTTI.csv", header = TRUE)
plan4 <- read.csv("atividade1_GUSTAVO_VIANA.csv", header = TRUE)
plan5 <- read.csv("atividade1_ISABELLA-FERREIRA.csv", header = TRUE)
plan6 <- read.csv("atividade1_marcosdelucena.csv", header = TRUE)
plan7 <- read.csv("atividade1_marinasissini.csv", header = TRUE)
plan8 <- read.csv("atividade1_NILSON-BERRIEL.csv", header = TRUE)
plan9 <- read.csv("atividade1_pedrozau.csv", header = TRUE)
plan10 <- read.csv("atividade1_Vanessa Xavier.csv", header = TRUE)


bdados <- bind_rows(plan1, plan2, plan5, plan6, plan7, plan8, plan9, plan10)

## Utilizei a função 'bind_rows' para unir todas as planilhas. Ocorreu algum erro na plan4, ainda preciso verificar e corrigir para poder acrescentar ela na análise;
           ## As demais planilhas foram adicionadas no arquivo 'bdados', totalizando 137 obs. e 8 variaveis.

## ATIVIDADE 02 - Criando arquivos no sistema eMOF ##

iris1 <- read.csv("iris_mod.csv", header = T)

lapply(iris, unique)

str(bdados)
str(iris1)

## Irei utilizar os dados cedidos pelo professor para a realização da Atividade 2, porque os dados unificados não estão organizados da forma adequada.

##Checando os taxons do banco de dados 'iris1'

# check taxa
species <- iris1 %>% 
  distinct(Species) %>% 
  pull() %>% 
  get_tsn() %>% 
  data.frame() %>% 
  bind_cols(iris %>% 
              distinct(Species))
==  3 queries  ===============
  
  Retrieving data for taxon 'Iris setosa'

v  Found:  Iris setosa

Retrieving data for taxon 'Iris versicolor'

v  Found:  Iris versicolor

Retrieving data for taxon 'Iris virginica'

v  Found:  Iris virginica
==  Results  =================
  
  * Total: 3 
* Found: 3 
* Not Found: 0

##Manipulação dos dados
iris_1 <- iris1 %>% 
  dplyr::mutate(eventID = paste(site, date, sep = "_"), # Criar campos de indexação 
                occurrenceID = paste(site, date, amostra, sep = "_")) %>% 
  left_join(species %>% 
              select(Species, uri)) %>% # Add identificador unico da especie
  dplyr::rename(decimalLongitude = lon, # Renomear campos de acordo com DwC
                decimalLatitude = lat,
                eventDate = date,
                scientificName = Species,
                scientificNameID = uri) %>% 
  mutate(geodeticDatum = "WGS84", # adicionar campos complementares
         verbatimCoordinateSystem = "decimal degrees",
         georeferenceProtocol = "Random coordinates obtained from Google Earth",
         locality = "Gaspe Peninsula",
         recordedBy = "Edgar Anderson",
         taxonRank = "Species",
         organismQuantityType = "individuals",
         basisOfRecord = "Human observation")

## Planilhas do eMOF
## create eventCore
eventCore <- iris_1 %>% 
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality, site,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>% 
  distinct() 
## create occurrence
occurrences <- iris_1 %>% 
  select(eventID, occurrenceID, scientificName, scientificNameID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct() 
## create measurementsOrFacts
eMOF <- iris_1 %>% 
  select(eventID, occurrenceID, recordedBy, Sepal.Length:Petal.Width) %>%  
  pivot_longer(cols = Sepal.Length:Petal.Width,
               names_to = "measurementType",
               values_to = "measurementValue") %>% 
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("Sepal.Length", "Sepal.Width", "Petal.Width", "Petal.Length"), 
                                           to = c("sepal length", "sepal width", "petal width", "petal length")))

##Controle de qualidade
# check if all eventID matches
setdiff(eventCore$eventID, occurrences$eventID)
#character(0)

setdiff(eventCore$eventID, eMOF$eventID)
#character(0)

setdiff(occurrences$eventID, eMOF$eventID)
#character(0)

# check NA values
eMOF %>%
  filter(is.na(eventID))
# A tibble: 0 x 6
# ... with 6 variables: eventID <chr>, occurrenceID <chr>, recordedBy <chr>,
#   measurementType <chr>, measurementValue <dbl>, measurementUnit <chr>

occurrences %>%
  filter(is.na(eventID))
[1] eventID              occurrenceID         scientificName      
[4] scientificNameID     recordedBy           taxonRank           
[7] organismQuantityType basisOfRecord       
<0 linhas> (ou row.names de comprimento 0)

##Escrever as matrizes como arquivos de texto

rm(list = setdiff(ls(), c("eventCore", "occurrences", "eMOF")))

files <- list(eventCore, occurrences, eMOF) 
data_names <- c("DF_eventCore","DF_occ","DF_eMOF")
dir.create("Dwc_Files")


for(i in 1:length(files)) {
  path <- paste0(getwd(), "/", "DwC_Files")
  write.csv(files[[i]], paste0(path, "/", data_names[i], ".csv"))
}



















