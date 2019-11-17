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

################## lendo os PDFs #################

texts <- vector()

for (i in list.files('Atas')) {
  texts[i] <-
    readPDF(control = list(text = "-layout -enc UTF-8"))(
      elem = list(uri = paste0('Atas/', i)),
      id = paste0('Atas/', i),
      language = 'en'
    )
  
}

atas_corpus <- Corpus(VectorSource(texts))

data <- matrix(ncol = 2, nrow = length(atas_corpus))
for (i in 1:length(atas_corpus)) {
  data[i, 1] <- list.files('Atas')[i]
  data[i, 2] <- atas_corpus[[i]][["content"]]
}

############# organizando as atas ################

ordem <- c()

for (i in 1:nrow(data)) {
  ordem[i] <-  as.numeric(gsub("([0-9]+).*$", "\\1", data[i, 1]))
}

data <- cbind(ordem, data)[order(ordem),]

#### somando os vetores em dois grupos: ####

data_tibble <- tibble(numero = data[, 1],
                      ata = data[, 2],
                      corpus = data[, 3])

# Salvando em RDS:

saveRDS(data_tibble, 'Datas/data_tibble.rds')
rm(list = ls())
