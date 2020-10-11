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

#### Analise das 200 palavras que mais aparecem nas atas ####

economica <- readRDS('Datas/analise_economica.rds')

#### Definindo Diretório ##############

root = rprojroot::is_rstudio_project
mydir = root$find_file()

# Pego as frequencias_economicas as 10 palavras que mais aparecem na análise geral -
# somente de cunho economico

data_tibble <- readRDS('Datas/data_tibble.rds')

#### criando matriz de palavras ####

matriz.de.frequencias <- matrix(ncol = 200,
                                nrow = nrow(data_tibble),
                                data = 0)

names <- economica$word[1:200]

colnames(matriz.de.frequencias) <- names

nomes <- vector('character', length = nrow(matriz.de.frequencias))

for (i in 1:nrow(matriz.de.frequencias)) {
  nomes[i] <- paste('ATA', i + 79, sep = ' ')
}

rownames(matriz.de.frequencias) <- nomes

palavras <- list()

for (i in 1:length(names)) {
  palavras[[i]] <- c(names[i], paste('n', names[i], sep = ''))
}

#### preencher a matriz com os valores em cada ata ####

for (k in 1:200) {
  for (i in 1:nrow(data_tibble)) {
    geral <- tibble(analise = i, corpus = data_tibble$corpus[[i]]) %>%
      unnest_tokens(word, corpus) %>%
      anti_join(stop_words) %>%
      count(word) %>%
      arrange(desc(n))
    
    for (j in 1:nrow(geral)) {
      for (palavra in palavras[[k]]) {
        count <- 0
        
        if (geral$word[[j]] == palavra) {
          matriz.de.frequencias[i, k] <-
            matriz.de.frequencias[i, k] + geral$n[[j]]
          
        }
      }
    }
  }
}

saveRDS(matriz.de.frequencias, 'Datas/matrizdefrequencias.rds')
rm(list = ls())
