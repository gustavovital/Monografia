# Script com o objetivo de calcular a informação de kullback leibler
# para as frequnencias obtidas das palavras que mais aparecem
# 
# Autor: gustavovital@id.uff.br

library(tidyverse)

#### BASES DE DADOS NECESSÁRIAS ####

relativa <- readRDS('D:/Monografia/Datas/matrizdefrequenciasrelativas.rds')

analise_relativa_meirelles <- relativa %>% 
  filter(datas <= '2010-12-08')

analise_relativa_tombini <- relativa %>% 
  filter(datas > '2010-12-08' & datas <= '2016-07-20')

analise_relativa_goldfajn <- relativa %>% 
  filter(datas > '2016-07-20')


#### relativizando para a frequencia de cada palavra ####

meirelles <- analise_relativa_meirelles[, 2:ncol(analise_relativa_meirelles)]
tombini <- analise_relativa_tombini[, 2:ncol(analise_relativa_tombini)]
goldfajn <- analise_relativa_goldfajn[, 2:ncol(analise_relativa_goldfajn)]

rm(analise_relativa_meirelles, analise_relativa_tombini, analise_relativa_goldfajn, relativa)

#### removendo as colunas de frequencia == 0 ####

# sales, basis, capital, industry, comparison, ibge, exports, durable, 
# manufacturing, expanded, grew, series, construction, totaled, rose, 
# oil, registered, agricultural, imports, products, vehicles, igp, surplus,
# jobs, recorded, record, output, ipa, fgv, maturing, intermediate, household, 
# smoothed, totaling, calculated, bears, million, trimmed, tradable, liquidity, 
# driven, trailing, br, categories, evaluates, earmarked, equipment, individuals, 
# net, delinquency, commodities, fixed


goldfajn <- subset(goldfajn, select = -c(sales, basis, capital, industry, comparison, ibge, exports, durable, 
                                         manufacturing, expanded, grew, series, construction, totaled, rose, 
                                         oil, registered, agricultural, imports, products, vehicles, igp, surplus,
                                         jobs, recorded, record, output, ipa, fgv, maturing, intermediate, household, 
                                         smoothed, totaling, calculated, bears, million, trimmed, tradable, liquidity, 
                                         driven, trailing, br, categories, evaluates, earmarked, equipment, individuals, 
                                         net, delinquency, commodities, fixed))

meirelles <- subset(meirelles, select = -c(sales, basis, capital, industry, comparison, ibge, exports, durable, 
                                           manufacturing, expanded, grew, series, construction, totaled, rose, 
                                           oil, registered, agricultural, imports, products, vehicles, igp, surplus,
                                           jobs, recorded, record, output, ipa, fgv, maturing, intermediate, household, 
                                           smoothed, totaling, calculated, bears, million, trimmed, tradable, liquidity, 
                                           driven, trailing, br, categories, evaluates, earmarked, equipment, individuals, 
                                           net, delinquency, commodities, fixed))

tombini <- subset(tombini, select = -c(sales, basis, capital, industry, comparison, ibge, exports, durable, 
                                       manufacturing, expanded, grew, series, construction, totaled, rose, 
                                       oil, registered, agricultural, imports, products, vehicles, igp, surplus,
                                       jobs, recorded, record, output, ipa, fgv, maturing, intermediate, household, 
                                       smoothed, totaling, calculated, bears, million, trimmed, tradable, liquidity, 
                                       driven, trailing, br, categories, evaluates, earmarked, equipment, individuals, 
                                       net, delinquency, commodities, fixed))


#### calculando Kullback-liebler ####

kl.distance <- function(data1, data2) {
  p <- colSums(data1) / sum(colSums(data1))
  q <- colSums(data2) / sum(colSums(data2))
  return(sum(p * (log(p / q))))
}

#### realizando o procedimento da divergencia de KL ####

# Período meirelles ####

m.t <- c()
for (i in 2:ncol(meirelles)) {
  m.t[i] <-
    kl.distance(as.data.frame(meirelles[, 1:i]), as.data.frame(tombini[, 1:i]))
}

m.g <- c()
for (i in 2:ncol(meirelles)) {
  m.g[i] <-
    kl.distance(as.data.frame(meirelles[, 1:i]), as.data.frame(goldfajn[, 1:i]))
}

data.m <- na.omit(tibble(
  indice = 1:148,
  tombini = m.t,
  goldfajn = m.g
))

ggplot(data = data.m, aes(x = indice)) +
  geom_line(aes(y = tombini, colour = 'tombini'),
            size = 1,
            alpha = 1) +
  geom_line(aes(y = goldfajn, colour = 'goldfajn'),
            size = 1,
            alpha = 1) +
  scale_colour_manual(
    name = 'Período em comparação',
    labels = c('tombini' = 'Tombini',
               'goldfajn' = 'Goldfajn'),
    values = c('tombini' = 'tomato4',
               'goldfajn' = 'dodgerblue3')
  ) +
  labs(x = 'Quantidades de palavras analisadas que mais aparecem',
       y = 'I(f,g)') +
  theme_test()

# Periodo Tombini ####

t.m <- c()
for (i in 2:ncol(tombini)) {
  t.m[i] <-
    kl.distance(as.data.frame(tombini[, 1:i]), as.data.frame(meirelles[, 1:i]))
}

t.g <- c()
for (i in 2:ncol(tombini)) {
  t.g[i] <-
    kl.distance(as.data.frame(tombini[, 1:i]), as.data.frame(goldfajn[, 1:i]))
}

data.t <- na.omit(tibble(
  indice = 1:148,
  meirelles = t.m,
  goldfajn = t.g
))

ggplot(data = data.t, aes(x = indice)) +
  geom_line(aes(y = meirelles, colour = 'meirelles'),
            size = 1,
            alpha = 1) +
  geom_line(aes(y = goldfajn, colour = 'goldfajn'),
            size = 1,
            alpha = 1) +
  scale_colour_manual(
    name = 'Período em comparação',
    labels = c('meirelles' = 'Meirelles',
               'goldfajn' = 'Goldfajn'),
    values = c('meirelles' = 'seagreen4',
               'goldfajn' = 'dodgerblue3')
  ) +
  labs(x = 'Quantidades de palavras analisadas que mais aparecem',
       y = 'I(f,g)') +
  theme_test()

# Período Goldfajn ####

g.m <- c()
for (i in 2:ncol(goldfajn)) {
  g.m[i] <-
    kl.distance(as.data.frame(goldfajn[, 1:i]), as.data.frame(meirelles[, 1:i]))
}

g.t <- c()
for (i in 2:ncol(goldfajn)) {
  g.t[i] <-
    kl.distance(as.data.frame(goldfajn[, 1:i]), as.data.frame(tombini[, 1:i]))
}

data.g <- na.omit(tibble(
  indice = 1:148,
  meirelles = g.m,
  tombini = g.t
))

ggplot(data = data.g, aes(x = indice)) +
  geom_line(aes(y = meirelles, colour = 'meirelles'),
            size = 1,
            alpha = 1) +
  geom_line(aes(y = tombini, colour = 'tombini'),
            size = 1,
            alpha = 1) +
  scale_colour_manual(
    name = 'Período em comparação',
    labels = c('meirelles' = 'Meirelles',
               'tombini' = 'Tombini'),
    values = c('meirelles' = 'seagreen4',
               'tombini' = 'tomato4')
  ) +
  labs(x = 'Quantidades de palavras analisadas que mais aparecem',
       y = 'I(f,g)') +
  theme_test()

palavra <- c(10, 20, 50, 100, 147)

for (i in palavra) {
  print(g.t[i], digits = 2)
}


saveRDS(tombini, 'www/datas/kltombini.rds')
saveRDS(meirelles, 'www/datas/klmeirelles.rds')
saveRDS(goldfajn, 'www/datas/klgoldfajn.rds')
