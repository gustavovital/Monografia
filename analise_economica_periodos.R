# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Autor : Gustavo Vital
# Data : 15 de agosto de 2019
# Descrição : Corte em frequencias_economicas e ajustes em
# datas para cada reuniao. Linha do tempo
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

############# Pacotes Necessários  ###############

require(pdftools)
require(rprojroot)
require(tidyverse)
require(readr)
require(tm)
require(tidytext)
require(magrittr)

############### Definindo Diretório ##############

root = rprojroot::is_rstudio_project
mydir = root$find_file()

# Aqui é feita a divisão das atas entre períodos. Cortamos o dataframe
# num formato de serie temporal, e realizamos o mesmo procedimento de analise.
# O dataframe é cortado da seguinte forma: meirelles; tombini; e goldfajn

analise_economica_geral <-
  readRDS('Datas/analise_economica_geral.rds')
# head(analise_economica_geral)

analise_economica_meirelles <- analise_economica_geral %>%
  filter(datas <= '2010-12-08')

analise_economica_tombini <- analise_economica_geral %>%
  filter(datas > '2010-12-08' & datas <= '2016-07-20')

analise_economica_goldfajn <- analise_economica_geral %>%
  filter(datas > '2016-07-20')

saveRDS(analise_economica_meirelles,
        'Datas/analise_economica_meirelles.rds')
saveRDS(analise_economica_tombini,
        'Datas/analise_economica_tombini.rds')
saveRDS(analise_economica_goldfajn,
        'Datas/analise_economica_goldfajn.rds')
