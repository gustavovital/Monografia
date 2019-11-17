# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Autor : Gustavo Vital
# Data : 13 de agosto de 2019
# Descrição : script para analise geral de termos
# economicos
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

############# Pacotes Necessários  ############### 

require(pdftools)
require(rprojroot)
require(tidyverse)
require(readr)
require(tm)
require(tidytext)

############### Definindo Diretório ##############

root = rprojroot::is_rstudio_project 
mydir = root$find_file()

# O que é feito é uma seleção das palavras (com margem de erro de contagem ate 400) de cunho econômico 
# que mais aparecem nas atas. Exclui-se, entao, palavras como 'increase' ou palavras de medida de tempo,
# como 'month'

words <- c('month', 'months', 'twelve', 'p.p', 'nthe', 'july', 'october', 'quarter', 'seasonally', 
           'january', 'september', 'april', 'june', 'march', 'august', 'november', 'february', 'december',
           'due', 'nin', 'monthly', 'de', 'di', '2008', 'thousand', '2006', '2005', '2007', '2009', '2010', 
           '2003', '2', '1', 'p.a', 'daily', '2004', 'days', '2012', '4', '2002', '2011', '6', 'nand', '3',
           '0.1', '12', 'hand', '5', '021', '2013', 'nof', '0.2', '0.3', '7', '0.5', '0.4', '2014', '8', 
           'day', '4.5', '1.2', '2001', '0.6', '10', '0.7', '1.1', '1.3', 'nuci', '0.8', '0.9', 'nmonth', 
           '017', 'paulo', '1.5', '9', '2015', '20', 'são', '15', 'isa', '1.6', 'bcb.gov.br', 'fourth', 
           '1.9', '1.4', 'da', 'nalexandre', '024', 'ntn', '1.7', '1.8', 'nto', 'pm', '2.1', '18', '11',
           '2.3', '023', '2.2', '19', '2017', '2000', 'nassessment', '2.7', '3.5', 'b.p', 'fx', 'ri', 'ten',
           '2.5', '2016', 'nwith', '17', 'silva', 'nrecent', '27', '2.9', '3.8', 'nother', 'njoão', 'meirelles',
           '2.4', '2.4', 'nyear', 'repo', '2018', '3.1', '3.4', 'antonio', '2.6', '025', '3.2', '4.1', '5.5',
           'nby', '16', '28', '30', '5.1', '3.3', '4.2', 'luiz', 'bias', 'nfor', 'gci.bacen	', 'nlabor', '2.8',
           'nmonetary', 'ninflation', 'ninformation', 'neconomic', 'nprices', 'nincreased', 'nexternal', 
           'ncredit', 'nexchange', 'nmoney', 'nincrease', 'rate', 'increased', 'increase', 'billion',
           'nbillion', 'compared', 'ncompared', 'period', 'nperiod', 'economic', 'neconomic', 'meeting',
           'nmeeting', 'data', 'ndata', 'index', 'nindex', 'reached', 'nreached', 'adjusted', 'nadjusted',
           'copom', 'ncopom', 'rates', 'nrates', 'scenario', 'nscenario', 'price', 'operations', 'noperations')

data_tibble <- readRDS('Datas/data_tibble.rds')
data(stop_words)

dicionario <- tibble(
  word = c(stop_words$word, words),
  lexicon = c(stop_words$lexicon, rep('PROPRIO', length(words)))
)

analise_economica <- c()
for (i in 1:nrow(data_tibble)) {
  analise_economica <- str_c(analise_economica,
                             data_tibble[i, 3], collapse = ' ')
}

analise_economica <- tibble(analise = 1,
                            corpus = analise_economica) %>%
  unnest_tokens(word, corpus) %>%
  anti_join(dicionario) %>%
  count(word) %>%
  arrange(desc(n))

saveRDS(analise_economica, 'Datas/analise_economica.rds')
rm(list = ls())
