# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Autor : Gustavo Vital
# Data : 7 de agosto de 2019
# Descrição : Criação de um Corpus em formato
# tibble, salvo em RDS
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

############# Pacotes Necessários  ###############

require(pdftools)
require(rprojroot)
require(tidyverse)
require(readr)
require(tm)
require(tidytext)
library(wordcloud)

############### Definindo Diretório ##############

root = rprojroot::is_rstudio_project
mydir = root$find_file()

# Script para criação de tibble em períodos

data_tibble.economics <- readRDS('Datas/data_tibble.rds')

data_tibble.meirelles <- data_tibble.economics[1:76,]
data_tibble.tombini <- data_tibble.economics[77:121,]
data_tibble.goldfjan <-
  data_tibble.economics[122:nrow(data_tibble.economics),]

saveRDS(data_tibble.meirelles, 'Datas/data_tibble.meirelles.rds')
saveRDS(data_tibble.tombini, 'Datas/data_tibble.tombini.rds')
saveRDS(data_tibble.goldfjan, 'Datas/data_tibble.goldfjan.rds')
