# Script criado com o intuito de estimar um VAR para a estimação de uma
# função de resposta ao impuls, para a recriação de um exercício do Shapiro.
# Foram utilizados os seguintes procedimentos: teste de raiz unitária,
# diferenciação das séries, estimação do VAR e raiz unitária
#
# Autor: gustavovital@id.uff.br


require(BETS)
require(vars)
require(urca)
require(xtable)

#### PEGANDO AS SERIES ####

serie <- readRDS('Datas/indice01.rds')
ibcbr <- BETSget(24364)
inf <- BETSget(13522)

ibcbr <- window(ibcbr, end = c(2018, 12))
inf <- window(inf, start = c(2003, 01), end = c(2018, 12))
#### teste de raiz unitária ####

## DF ####

df.ibcbr.none <-
  summary(ur.df(ibcbr, selectlags = 'AIC', type = 'none'))
df.ibcbr.drift <-
  summary(ur.df(ibcbr, selectlags = 'AIC', type = 'drift'))
df.ibcbr.trend <-
  summary(ur.df(ibcbr, selectlags = 'AIC', type = 'trend'))

df.inf.none <-
  summary(ur.df(inf, selectlags = 'AIC', type = 'none'))
df.inf.drift <-
  summary(ur.df(inf, selectlags = 'AIC', type = 'drift'))
df.inf.trend <-
  summary(ur.df(inf, selectlags = 'AIC', type = 'trend'))

df.serie.none <-
  summary(ur.df(serie, selectlags = 'AIC', type = 'none'))
df.serie.drift <-
  summary(ur.df(serie, selectlags = 'AIC', type = 'drift'))
df.serie.trend <-
  summary(ur.df(serie, selectlags = 'AIC', type = 'trend'))

#### EM DIFERENÇA ####

## DF ####

diff.df.ibcbr.none <-
  summary(ur.df(diff(ibcbr), selectlags = 'AIC', type = 'none'))
diff.df.ibcbr.drift <-
  summary(ur.df(diff(ibcbr), selectlags = 'AIC', type = 'drift'))
diff.df.ibcbr.trend <-
  summary(ur.df(diff(ibcbr), selectlags = 'AIC', type = 'trend'))

diff.df.inf.none <-
  summary(ur.df(diff(inf), selectlags = 'AIC', type = 'none'))
diff.df.inf.drift <-
  summary(ur.df(diff(inf), selectlags = 'AIC', type = 'drift'))
diff.df.inf.trend <-
  summary(ur.df(diff(inf), selectlags = 'AIC', type = 'trend'))

diff.df.serie.none <-
  summary(ur.df(diff(serie), selectlags = 'AIC', type = 'none'))
diff.df.serie.drift <-
  summary(ur.df(diff(serie), selectlags = 'AIC', type = 'drift'))
diff.df.serie.trend <-
  summary(ur.df(diff(serie), selectlags = 'AIC', type = 'trend'))

#### CRIANDO A TABELA DE TESTES ####

tabela <-
  matrix(
    c(
      df.ibcbr.none@teststat[1],
      df.ibcbr.drift@teststat[1],
      df.ibcbr.trend@teststat[1],
      df.inf.none@teststat[1],
      df.inf.drift@teststat[1],
      df.inf.trend@teststat[1],
      df.serie.none@teststat[1],
      df.serie.drift@teststat[1],
      df.serie.trend@teststat[1],
      
      diff.df.ibcbr.none@teststat[1],
      diff.df.ibcbr.drift@teststat[1],
      diff.df.ibcbr.trend@teststat[1],
      diff.df.inf.none@teststat[1],
      diff.df.inf.drift@teststat[1],
      diff.df.inf.trend@teststat[1],
      diff.df.serie.none@teststat[1],
      diff.df.serie.drift@teststat[1],
      diff.df.serie.trend@teststat[1],
      
      df.ibcbr.none@cval[3],
      df.ibcbr.drift@cval[1, 3],
      df.ibcbr.trend@cval[1, 3],
      df.inf.none@cval[2],
      df.inf.drift@cval[1, 2],
      df.inf.trend@cval[1, 2],
      df.serie.none@cval[1],
      df.serie.drift@cval[1, 1],
      df.serie.trend@cval[1, 1]
    ),
    nrow = 9,
    ncol = 3,
    byrow = T
  )



#print(xtable(tabela,digits=c(0,4,4,4)))

rm(list = ls())

#### PREPARANDO AS SERIES PRO VAR ####

serie <- readRDS('Datas/indice01.rds')
ibcbr <- BETSget(24364)
inf <- BETSget(13522)

ibcbr <- window(ibcbr, end = c(2018, 12))
ipca <- window(inf, start = c(2003, 02), end = c(2018, 12))
indice <- window(serie, start = c(2003, 02))
ibcbr.diff <- diff(ibcbr)


data <- data.frame(indice, ipca, ibcbr.diff)

#### ESTIMANDO O VAR ####

VARselect(data)

var <- VAR(data, p = 2, type = 'both')
summary(var)

# estabilidade ####

#var.stabil <- stability(var, type = 'OLS-CUSUM')
#var.stabil

#plot(var.stabil)

# FIR ####

FIR <- irf(var,
           response = 'indice',
           n.ahead = 12,
           boot = T)
plot(FIR)

data <-
  data.frame(FIR[["irf"]][["indice"]], FIR[["irf"]][["ipca"]], FIR[["irf"]][["ibcbr.diff"]],
             FIR[["Upper"]][["indice"]], FIR[["Upper"]][["ipca"]], FIR[["Upper"]][["ibcbr.diff"]],
             FIR[["Lower"]][["indice"]], FIR[["Lower"]][["ipca"]], FIR[["Lower"]][["ibcbr.diff"]])

colnames(data) <- c(
  'irf.indice',
  'irf.ipca',
  'irf.ibcbr',
  'up.indice',
  'up.ipca',
  'up.ibcbr',
  'low.indice',
  'low.ipca',
  'low.ibcbr'
)

saveRDS(data, 'Datas/impulsoresposta.rds')
