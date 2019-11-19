#### GRAFICOS ####
# Script para criação dos gráficos utilizados na
# monografia
#
# Autor: gustavovital@id.uff.br

#### PACOTES NECESSARIOS ####

require(ggplot2)
require(plot3D)
require(plotly)
require(ggthemes)
require(gridExtra)
require(Rlab)
require(latex2exp)
require(statmod)
require(LaplacesDemon)
require(wordcloud)
require(tibble)

#### GRAFICO GAMMA ####

tamanho <- seq(0, 50, 0.1)

gamma <- dgamma(tamanho, shape = 4, scale = 4)
weibull <- dweibull(tamanho, shape = 2, scale = 20)
log.normal <- dlnorm(tamanho, meanlog = 2, sdlog = sqrt(2))
gauss <- dinvgauss(tamanho, 16, shape = 64)
normal <- df(tamanho, 4, 10)

df.gamma <-
  data.frame(gamma,
             weibull,
             log.normal,
             gauss,
             normal,
             tamanho)

graf.weibull <-
  ggplot(df.gamma, aes(x = tamanho)) +
  geom_area(aes(y = gamma, fill = 'Gamma'), size = 1, alpha = .6) +
  geom_area(aes(y = weibull, fill = 'Weibull'),
            size = 1,
            alpha = .6) +
  
  scale_fill_manual(name = '',
                    values = c('Gamma' = '#e41a1c',
                               'Weibull' = '#377eb8'),
  ) +
  labs(x = '', y = '') +
  theme_minimal() +
  theme(legend.position = 'bottom')

graf.log.normal <-
  ggplot(df.gamma, aes(x = tamanho)) +
  geom_area(aes(y = gamma, fill = 'Gamma'), size = 1, alpha = .6) +
  geom_area(aes(y = log.normal, fill = 'Log.normal'),
            size = 1,
            alpha = .6) +
  
  scale_fill_manual(name = '',
                    values = c('Gamma' = '#e41a1c',
                               'Log.normal' = '#4daf4a'),
  ) +
  labs(x = '', y = '') +
  theme_minimal() +
  theme(legend.position = 'bottom')


graf.gauss <-
  ggplot(df.gamma, aes(x = tamanho)) +
  geom_area(aes(y = gamma, fill = 'Gamma'), size = 1, alpha = .6) +
  geom_area(aes(y = gauss, fill = 'Gauss'), size = 1, alpha = .6) +
  
  scale_fill_manual(name = '',
                    values = c('Gamma' = '#e41a1c',
                               'Gauss' = '#984ea3'),
  ) +
  labs(x = '', y = '') +
  theme_minimal() +
  theme(legend.position = 'bottom')

graf.normal <-
  ggplot(df.gamma, aes(x = tamanho)) +
  geom_area(aes(y = gamma, fill = 'Gamma'), size = 1, alpha = .6) +
  geom_area(aes(y = normal, fill = 'Normal'),
            size = 1,
            alpha = .6) +
  
  scale_fill_manual(name = '',
                    values = c('Gamma' = '#e41a1c',
                               'Normal' = '#ff7f00'),
  ) +
  
  labs(x = '', y = '') +
  theme_minimal() +
  theme(legend.position = 'bottom')


grid.arrange(graf.normal,
             graf.gauss,
             graf.log.normal,
             graf.weibull, ncol = 2)

rm(list = ls())

#### GRAFICO MONETARY POLICY ####

relativa <- readRDS('Datas/relativa.rds')

monetary <-
  ggplot(relativa, aes(x = datas)) +
  geom_area(aes(y = monetary, fill = 'Monetary'), alpha = .7)  +
  
  scale_fill_manual(name = '',
                    values = c('Monetary' = 'tomato2')) +
  
  labs(x = '', y = '') +
  theme_minimal() +
  theme(legend.position = 'bottom')

policy <-
  ggplot(relativa, aes(x = datas)) +
  geom_area(aes(y = policy, fill = 'Policy'), alpha = .7)  +
  
  scale_fill_manual(name = '',
                    values = c('Policy' = 'dodgerblue3')) +
  
  labs(x = '', y = '') +
  theme_minimal() +
  theme(legend.position = 'bottom')

grid.arrange(monetary,
             policy, ncol = 2)

rm(list = ls())

#### grafico ipca mon pol ####

data <- readRDS('Datas/ipca_pol_mon.rds')

ggplot(data, aes(x = data)) +
  
  geom_rect(
    data = data,
    mapping = aes(
      xmin = as.Date('2010-12-08'),
      xmax = as.Date('2016-07-20'),
      ymin = -Inf,
      ymax = Inf
    ),
    fill = '#ece2f0',
    alpha = 0.2
  ) +
  
  geom_line(aes(y = monetary, colour = 'Monetary'), size = 1.5) +
  geom_line(aes(y = policy, colour = 'Policy'), size = 1.5) +
  geom_line(aes(y = ipca / 600, colour = 'IPCA'), size = 1.5) +
  
  scale_color_manual(
    name = '',
    values = c(
      'Monetary' = '#bcbddc',
      'Policy' = '#756bb1',
      'IPCA' = '#54278f'
    )
  ) +
  
  scale_y_continuous(sec.axis = sec_axis(~ . * 600)) +
  annotate("text",
           x = as.Date('2007-12-08'),
           y = 0.027,
           label = "Meirelles") +
  annotate("text",
           x = as.Date('2013-12-08'),
           y = 0.025,
           label = "Tombini") +
  annotate("text",
           x = as.Date('2018-03-08'),
           y = 0.029,
           label = "Goldfajn") +
  labs(colour = '',
       x = '',
       y = '') +
  theme_minimal() +
  theme(legend.position = 'bottom')

rm(list = ls())

#### wordcloud ####

df <- readRDS('Datas/analise_economica.rds')

goldfajn <-
  readRDS('D:/Monografia/Datas/tibble.goldfajn.rds')[1:200,]

word.geral <- df[1:200,]
saveRDS(goldfajn,
        'D:/Monografia/dashBoard/www/datas/wordsgoldfajn.rds')

wordcloud(
  words = df$word,
  freq = df$n,
  max.words = 150,
  random.order = FALSE,
  use.r.layout = FALSE,
  colors = brewer.pal(12, "Paired")
)

rm(list = ls())

#### grafico indice ####

indice <- as.numeric(readRDS('Datas/indice01.rds'))
datas <- seq(as.Date('2003-01-01'), as.Date('2018-12-01'), by = 'm')
otimismo <- ifelse(indice > 0.5, 'Otimismo', 'Pessimismo')

df <- tibble(indice, datas, otimismo)

ggplot(df, aes(x = datas)) +
  geom_col(aes(y = indice, fill = otimismo), alpha = .8) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(name = '',
                    values = c('Otimismo' = '#feb24c',
                               'Pessimismo' = '#f03b20')) +
  labs(x = '', y = '') +
  theme_minimal() +
  theme(legend.position = 'bottom')

rm(list = ls())


#### IMPULSO RESPOSTA ####

irf <- data.frame(x = 1:13, readRDS('Datas/impulsoresposta.rds'))

indice <- ggplot(irf, aes(x = x)) +
  geom_hline(yintercept = 0,
             colour = 'tomato2',
             size = .6) +
  geom_line(aes(y = irf.indice), colour = 'dodgerblue4', size = 1) +
  geom_line(
    aes(y = up.indice),
    linetype = 'dotted',
    colour = 'dodgerblue2',
    size = 1
  ) +
  geom_line(
    aes(y = low.indice),
    linetype = 'dotted',
    colour = 'dodgerblue2',
    size = 1
  ) +
  labs(x = '', y = '', title = 'Índice -> Índice') +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

ipca <- ggplot(irf, aes(x = x)) +
  geom_hline(yintercept = 0,
             colour = 'tomato2',
             size = 1) +
  geom_line(aes(y = irf.ipca), colour = 'dodgerblue4', size = 1) +
  geom_line(aes(y = up.ipca),
            linetype = 'dotted',
            colour = 'dodgerblue2',
            size = 1) +
  geom_line(
    aes(y = low.ipca),
    linetype = 'dotted',
    colour = 'dodgerblue2',
    size = 1
  ) +
  labs(x = '', y = '', title = 'Índice -> IPCA') +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

ibcbr <- ggplot(irf, aes(x = x)) +
  geom_hline(yintercept = 0,
             colour = 'tomato2',
             size = .6) +
  geom_line(aes(y = irf.ibcbr), colour = 'dodgerblue4', size = 1) +
  geom_line(
    aes(y = up.ibcbr),
    linetype = 'dotted',
    colour = 'dodgerblue2',
    size = 1
  ) +
  geom_line(
    aes(y = low.ibcbr),
    linetype = 'dotted',
    colour = 'dodgerblue2',
    size = 1
  ) +
  labs(x = '', y = '', title = 'Índice -> IbcBr') +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(indice,
             ipca,
             ibcbr, ncol = 1, nrow = 3)


#### google ####

indice <- read.csv('Datas/google.csv')[, 2]
datas <- seq(as.Date('2004-02-01'), as.Date('2019-10-01'), by = 'm')

data <- tibble::tibble(data = datas, valor = indice)

ggplot(data, aes(x = data)) +
  geom_area(aes(y = valor),
            fill = 'tomato2',
            colour = 'tomato4',
            alpha = .5) +
  labs(x = '', y = '') +
  theme_light()

#### grafico kullback-liebler ####

kl.normal <- function(tau, iota) {
  # para a função normal
  return((1 / 2) * (log(1 / (tau ^ 2)) + (tau ^ 2 + iota ^ 2) - 1))
}

kl.laplace <- function(sigma) {
  (1 / 2) * log(2 * pi * sigma ^ 2) + 2 / sigma ^ 2 - log(2) - 1
}

#plot(kl.laplace(Sigma), type = 'l')

Tau <- Sigma <- iota <- seq(.5, 40, by = .2)

I <- outer(Tau, Sigma, kl.normal)

plot_ly(z = ~ I) %>%
  add_surface() %>%
  layout(title = '',
         scene = list(
           xaxis = list(title = "x"),
           yaxis = list(title = "y"),
           zaxis = list(title = 'I')
         )) %>%
  config(mathjax = 'cdn')

Sigma <- seq(.5, 15, by = .1)

laplace <- kl.laplace(Sigma)

df <- data.frame(Sigma, laplace)

ggplot(df, aes(x = Sigma)) +
  geom_line(aes(y = laplace),
            colour = 'tomato4',
            size = 2.5,
            alpha = .5) +
  geom_line(aes(y = laplace),
            colour = 'tomato2',
            size = 1.5,
            alpha = .5) +
  labs(y = latex2exp::TeX('$\\textit{I(f,g)}$'),
       x = latex2exp::TeX('$\\sigma^2$')) +
  theme_minimal() +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 13))

