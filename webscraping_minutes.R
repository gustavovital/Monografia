# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Autor : Gustavo Vital
# Data : 8 de junho de 2019
# Descrição : Código modificado de Hudson Costa,
# em que realiza os downloads dos minutes do COPOM
# do período de 2000 à 2018, com exceção da
# primeira ata que foi corrigida
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### Web scrapping - Atas do COPOM - Minutes   ####

############# Pacotes Necessários  ###############

require(rprojroot)
require(rvest)
require(tidyverse)

############### Definindo Diretório ##############

root = rprojroot::is_rstudio_project
mydir = root$find_file()

####  Descobrindo as URL's de todas as atas disponíveis no site do BCB ####

main.page = read_html(x = "http://www.bcb.gov.br/?MINUTES") # pelo pacote rvest

#### Obtendo as URL's ####

urls = main.page %>%
  html_nodes("#cronoAno a") %>% # Pelo http://selectorgadget.com/ para pegar a "#cronoAno a"
  html_attr("href")

#### Obtendo os links dos textos ####

links = main.page %>%
  html_nodes("#cronoAno a") %>%
  html_text()

for (i in 1:length(urls)) {
  urls[i] = paste("http://www.bcb.gov.br", urls[i], sep = '')
}

yearly_page = data.frame(links = links,
                         urls = urls,
                         stringsAsFactors = FALSE)
yearly_page = subset(x = yearly_page, links %in% 2002:2018) # Período de análise

#### Extração das URL's de cada ata de cada ano ####

main.page.yearly = list()
urls.yearly = list()
links.yearly = list()

for (i in 1:length(yearly_page$urls)) {
  main.page.yearly = read_html(x = yearly_page$urls[i])
  urls.yearly[[i]] = main.page.yearly %>%
    html_nodes("#cronoGrupoMes a") %>%
    html_attr("href")
  
  links.yearly[[i]] = main.page.yearly %>%
    html_nodes("#cronoGrupoMes a") %>% # obter os nó CSS
    html_text() # extrair o texto do link (o ano)
}

for (i in 1:length(urls.yearly)) {
  for (j in 1:length(urls.yearly[[i]])) {
    urls.yearly[[i]][j] = paste("http://www.bcb.gov.br",
                                urls.yearly[[i]][j], sep = '')
  }
}

#### Combino "link" e "urls" em um data.frame ####

atas <- data.frame(
  reuniao = unlist(links.yearly),
  urls = unlist(urls.yearly),
  stringsAsFactors = FALSE
)

#### Renomeio as colunas e preparo para o download ####

for (i in 1:nrow(atas)) {
  atas$reuniao[i] <- sub('Copom minutes', '', atas$reuniao[i])
  atas$reuniao[i] <- sub('Copom Minutes', '', atas$reuniao[i])
  atas$reuniao[i] = gsub(" ", "", atas$reuniao[i], fixed = TRUE)
}

for (i in 1:120) {
  atas$reuniao[i] = sub('.*(?=.{5}$)', '', atas$reuniao[i], perl = T)
}

for (i in 121:nrow(atas)) {
  atas$reuniao[i] = sub('.*(?=.{4}$)', '', atas$reuniao[i], perl = T)
}

atas$reuniao[10] <- '210th'

#### Agregar o local onde cada ata está salva ####

for (i in 1:nrow(atas)) {
  atas$path[i] = paste0(mydir, "/Atas/", atas$reuniao[i], ".pdf", sep = '')
}

#### Download das atas ####

for (i in 1:length(atas$urls)) {
  pdf.url = atas$urls[i]
  pdf.name = paste0(mydir, "/Atas/", atas$reuniao[i], ".pdf", sep = '')
  download.file(
    pdf.url,
    pdf.name,
    method = 'libcurl',
    quiet = FALSE,
    mode = "wb",
    cacheOK = TRUE,
    extra = getOption("download.file.extra")
  )
}
