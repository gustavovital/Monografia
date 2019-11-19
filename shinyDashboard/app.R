# Script com o objetivo de criar um shinyApp para apresentações
# dinamicas do Tcc. É previsto uma elaboração inicial para:
#
# 1 - Nuvens de palavras entre períodos;
# 2 - frequências de palavras entre periodos;
# 3 - frequências de palavras por período especificado;
# 4 - calculo da divergência de K-L
#
# Autor: gustavovital@id.uff.br

# pacotes necessários ====

require(shiny)
require(shinydashboard)
require(wordcloud)
require(ggplot2)

# Bases de dados ====

# WINDOWS ####

# nuvemgeral <-
#   readRDS('D:/Monografia/dashBoard/www/datas/wordsgeral.rds')
# nuvemgoldfajn <-
#   readRDS('D:/Monografia/dashBoard/www/datas/wordsgoldfajn.rds')
# nuvemtombini <-
#   readRDS('D:/Monografia/dashBoard/www/datas/wordstombini.rds')
# nuvemmeirelles <-
#   readRDS('D:/Monografia/dashBoard/www/datas/wordsmeirelles.rds')
# 
# klmeirelles <-
#   readRDS('D:/Monografia/dashBoard/www/datas/klmeirelles.rds')
# kltombini <-
#   readRDS('D:/Monografia/dashBoard/www/datas/kltombini.rds')
# klgoldfajn <-
#   readRDS('D:/Monografia/dashBoard/www/datas/klgoldfajn.rds')
# 
# freqmeirelles <-
#   readRDS('D:/Monografia/dashBoard/www/datas/analise_relativa_meirelles.rds')
# freqtombini <-
#   readRDS('D:/Monografia/dashBoard/www/datas/analise_relativa_tombini.rds')
# freqgoldfajn <-
#   readRDS('D:/Monografia/dashBoard/www/datas/analise_relativa_goldfajn.rds')

# LINUX ####

nuvemgeral <-
  readRDS(
    '/media/ragnar/Seagate Expansion Drive/Monografia/dashBoard/www/datas/wordsgeral.rds'
  )
nuvemgoldfajn <-
  readRDS(
    '/media/ragnar/Seagate Expansion Drive/Monografia/dashBoard/www/datas/wordsgoldfajn.rds'
  )
nuvemtombini <-
  readRDS(
    '/media/ragnar/Seagate Expansion Drive/Monografia/dashBoard/www/datas/wordstombini.rds'
  )
nuvemmeirelles <-
  readRDS(
    '/media/ragnar/Seagate Expansion Drive/Monografia/dashBoard/www/datas/wordsmeirelles.rds'
  )

klmeirelles <-
  readRDS(
    '/media/ragnar/Seagate Expansion Drive/Monografia/dashBoard/www/datas/klmeirelles.rds'
  )
kltombini <-
  readRDS(
    '/media/ragnar/Seagate Expansion Drive/Monografia/dashBoard/www/datas/kltombini.rds'
  )
klgoldfajn <-
  readRDS(
    '/media/ragnar/Seagate Expansion Drive/Monografia/dashBoard/www/datas/klgoldfajn.rds'
  )

freqmeirelles <-
  readRDS(
    '/media/ragnar/Seagate Expansion Drive/Monografia/dashBoard/www/datas/analise_relativa_meirelles.rds'
  )
freqtombini <-
  readRDS(
    '/media/ragnar/Seagate Expansion Drive/Monografia/dashBoard/www/datas/analise_relativa_tombini.rds'
  )
freqgoldfajn <-
  readRDS(
    '/media/ragnar/Seagate Expansion Drive/Monografia/dashBoard/www/datas/analise_relativa_goldfajn.rds'
  )

# açoes  necessárias ====

kl.distance <- function(data1, data2) {
  p <- colSums(data1) / sum(colSums(data1))
  q <- colSums(data2) / sum(colSums(data2))
  return(sum(p * (log(p / q))))
}

cores <- c(
  '#9e0142',
  '#d53e4f',
  '#f46d43',
  '#fdae61',
  '#fee08b',
  '#e6f598',
  '#abdda4',
  '#66c2a5',
  '#3288bd',
  '#5e4fa2'
)

# Interface de usuario ====

ui <-
  dashboardPage(
    skin = 'green',
    dashboardHeader(
      title = "Análise de Sentimento das ATAS do COPOM",
      titleWidth = 450,
      tags$li(
        class = 'dropdown',
        tags$a(
          href = 'https://github.com/gustavovital/Monografia',
          icon('github'),
          'Código Fonte',
          target = '_blank'
        )
      )
    ),
    dashboardSidebar(
      width = 350,
      
      sidebarMenu(
        br(),
        h5(span('Apresentação do Trabalho', style = 'color:white'), align = 'center'),
        menuItem("Introdução", tabName = 'intro', icon = icon('ello')),
        menuItem(
          "1.1 - Motivação do Trabalho",
          tabName = 'present',
          icon = icon("file-pdf-o")
        ),
        menuItem(
          "1.2 - Analise de Sentimentos e Bancos Centrais",
          tabName = 'analisebc',
          icon = icon("file-pdf-o")
        ),
        menuItem(
          HTML("1.3 - Índices de Sentimentos como Variáveis <br/>
                 Econômicas"),
          tabName = 'econind',
          icon = icon("file-pdf-o")
        ),
        menuItem(
          "1.4 - O Índice como uma Variável Econométrica",
          tabName = 'enders',
          icon = icon("file-pdf-o")
        ),
        h5(span('Metodologia', style = 'color:white'), align = 'center'),
        menuItem(
          "2.1 - Web scraping",
          tabName = 'webscraping',
          icon = icon("calculator")
        ),
        menuItem(
          "2.2 - Text mining",
          tabName = 'textmining',
          icon = icon("calculator")
        ),
        menuItem(
          "2.3 - Índice de Sentimentos",
          tabName = 'indice',
          icon = icon("calculator")
        ),
        menuItem(
          "2.4 - Kullback-Liebler",
          tabName = 'liebler',
          icon = icon("calculator")
        ),
        menuItem(
          "2.5 - Vetor Auto-regressivo",
          tabName = 'var',
          icon = icon("calculator")
        ),
        h5(span('Resultados do Trabalho', style = 'color:white'), align = 'center'),
        menuItem("Word Clouds", tabName = 'wordclouds', icon = icon('cloud')),
        menuItem(
          'Frequências',
          tabName = 'freq',
          icon = icon('chart-bar')
        ),
        menuItem(
          'Kullback-Liebler',
          tabName = 'kullback',
          icon = icon("calculator")
        ),
        menuItem('VAR-FRI', tabName = 'fri', icon = icon("book")),
        menuItem('Bibliografia', tabName = 'bib', icon = icon("book"))
      )
    ),
    dashboardBody(
      tabItems(
        # INTRODUÇÃO ====
        
        tabItem(
          tabName = 'intro',
          h1(strong('Análise de Sentimentos e as ATAS do COPOM'), align = 'center'),
          h2(
            span('Uma análise dos períodos de 2003 à 2018', style = 'color:green'),
            align = 'center'
          ),
          br(),
          h3(
            'O vigente trabalho de monografia visa analisar de forma quantitativa expressões e informações das atas publicadas pelo Comitê de Política Monetária do Banco Central do Brasil.'
          ),
          h3(
            'É sabido que dependendo da gestão de um Banco Central, seus interesses e focos podem variar. Seja pela gestão em si - se considerarmos uma abordagem mais ortodoxa ou heterodoxa
                   frente a política monetária - ou mesmo de forma intrínceca ao governo vigente no pais. Ainda, seria interessante uma análise quantitativa perante as gestões vigentes: será que
                   com uma mudança na presidência do Banco Central a abordagem em relação a política monetária realmente mudaria?'
          ),
          h3(
            'Colocando em outras palavras, podemos afirmar que durante a gestão Meirelles (governo Lula), a abordagem perante a política monetária realmente divere da abordagem quando
                   considerando a gestão',
            span(strong('Goldfajn'), style = 'color:green'),
            '(Governo Temer)? E o período ',
            span(strong('Tombini'), style = 'color:green'),
            ' (Governo Dilma), realmente se assemelha
                   mais ao período ',
            span(strong('Meirelles'), style = 'color:green'),
            '?'
          ),
          br(),
          h3(
            'Levando em consideração critérios estatísticos, e nos fazendo valer de elementos computacionais, é possível obtermos indícios de o que realmente ocorreu durante um período
                   análisado quando comparamos com um outro período.'
          ),
          h3(
            'Esse trabalho pretende, então, demonstrar como que, por meio de documentos textuais, podemos encontrar semelhanças e diferênças em relação a diferentes períodos monetários,
                   bem como correlacionar expressões textuai com variáveis macroeconômicas reais.'
          ),
          br(),
          br(),
          img(
            src = 'tidyverse.png',
            width = 90,
            height = 100
          ),
          img(
            src = 'tidytext.png',
            width = 90,
            height = 100
          ),
          img(
            src = 'ggplot.png',
            width = 90,
            height = 100
          ),
          img(
            src = 'knitr.png',
            width = 90,
            height = 100
          ),
          img(
            src = 'dplyr.png',
            width = 90,
            height = 100
          ),
          img(
            src = 'Rlogo.png',
            width = 90,
            height = 90
          )
          
        ),
        
        # APRESENTAÇÃO ====
        
        tabItem(
          tabName = 'present',
          h1(span('Motivação do Trabalho', style = 'color:green'), align = 'center'),
          br(),
          column(
            width = 12,
            box(
              width = 12,
              status = "warning",
              h2(
                'Análise de sentimentos pode ser definida como um conjunto de algoritimos capaz de categorizar e interpretar expressões e essências em um texto ou em um conjunto de textos'
              ),
              h2(
                'A partir de um corpus é possível identificar padrões, e de forma geral, interpretar o que esse conjunto de documentos quer dizer. Indo mais além, é possível identificar
                          positividade ou negatividade - isto é, a polaridade do corpus.'
              ),
              h2(
                span(
                  'Dessa forma, utilizando algoritimos e técnicas estatísticas podemos verificar se com uma mudança de política monetária as formas de expressão de um banco central muda de acordo
                               com a sua política monetária. Em outras palavras, podemos verificar se o tratamento dado às questões macroeconômicas mudam - efetivamente - conforme mudam suas políticas monetárias,
                               conforme muda a gestão do banco central.',
                  style = 'color:green'
                )
              ),
              h2(
                'Essencialmente, análises de sentimento ainda é muito pouco utilizada no campo da ciência econômica, sendo, entretanto, muito mais utilizada para análises nos campos das ciências políticas -
                          por exemplo, por meio de análises de discursos - ou mesmo em áreas de marketing, para verificar análises de demandas ou de ofertas.'
              ),
              br(),
              h2('Entretanto, já é possível afirmar que isso vêm mudando...')
              
            )
          )
        ),
        
        # ANALISE DE CENTIMENTOS E BANCOS CENTRAIS ====
        
        tabItem(
          tabName = 'analisebc',
          h1(
            span('Análises de Sentimentos e Bancos Centrais', style = 'color:green'),
            align = 'center'
          ),
          br(),
          column(width = 12,
                 box(
                   width = 12,
                   status = "warning",
                   h2(
                     'Pesquisas recentes sugerem que essa técnica pode ser amplamente utilizada de forma a se obter dados quantificados a partir de corpus. Partindo da premissa que TUDO pode
                              ser um dado, isso faz muito sentido. Obter dados textuais de forma quantitativa pode, além de servir como uma ótima proxy para modelos indicar interesses reais e movimentos reais do
                              que realmente acontece na economia'
                   ),
                   h2(
                     span(
                       'O Bank of England há cerca de uma década considera variáveis obtidas por meio de text mining como possíveis proxys para obtenção de resultados em modelos macroeconômicos. Por exemplo,
                                   a partir de dados oriundos de volumes de buscas online é possível entendermos o que acontece em determinado momento da economia. Para simplificar, faz sentido que em período de desemprego
                                   as pessoas procurem por emprego na internet, ou mesmo mudem seus status do linkdin. Um outro exemplo é quanto ao mercado mobiliário: buscas por compras e vendas de imóveis podem indicar
                                   um indício de alta ou baixo no mercado mobiliário.',
                       style = 'color:green'
                     )
                   ),
                   h2(
                     'Ainda, quando se leva em consideração o que é expresso em artigos e jornais de economia, podemos entender melhor o que espera um agente econômico. A questão, então, passa a ser quantificar os
                              dados obtidos afim de manipulá-los.'
                   ),
                   br(),
                   h2(
                     span(
                       'Assim, é possível a elaboração de índices a partir de corpus já quantificados para termos indícios de o que realmente acontece no cenário macroeconômico..'
                     )
                   )
                   
                 ))
        ),
        
        # Índices de Sentimentos como Variáveis Econômicas ====
        
        tabItem(
          tabName = 'econind',
          h1(
            span('Índices de Sentimentos como Variáveis Econômicas', style = 'color:green'),
            align = 'center'
          ),
          br(),
          column(
            width = 12,
            box(
              width = 12,
              status = "warning",
              h2(
                'Em uma recente pesquisa publicada (2016), Hudson Costa propõe um índice de sentimentos para analisar como o banco central brasileiro de expressa, por meio das atas disponibilizadas pelo
                              Comitê de Política Monetária (COPOM). Basicamente, o índice consiste em dividir o número de palavras positivas em um corpus pelo número total de palavras (positivas e negativas), tal que:'
              ),
              withMathJax(),
              h2('$$I_i = \\frac{NP_i}{NP_i + NN_i}$$'),
              h2(
                span(
                  'com I variando de 0 (somente palavras negativas) a 1, (somente palavras positivas). Por meio de um índice como esse é possível identificarmos, por exemplo, o "estado de otimismo" do
                                   banco central em determinado período no qual a reunião foi realizada.',
                  style = 'color:green'
                )
              ),
              h2(
                'Hudson vai além: estaria este índice correlacionado com variáveis macroeconômicas? E então, é demonstrada uma correlação do índice com variáveis macroeconômicas reais (IPCA, SELIC, e
                              Atividade Econômica)'
              ),
              br()
            )
          )
        ),
        
        # Econometria ====
        
        tabItem(
          tabName = 'enders',
          h1(
            span('Índice de Sentimentos x Variáveis Reais', style = 'color:green'),
            align = 'center'
          ),
          br(),
          column(width = 12,
                 box(
                   width = 12,
                   status = "warning",
                   h2(
                     'Em um trabalho publicado por Shapiro (2019) é feito algo a mais. É apresentado um índice de otimismo/pessimismo e relacionado com as variáveis econômicas estadunidenses. Feito isso, é estimado um
                   vetor auto-regressivo (VAR) e suas funções de resposta impulso, a fim de correlacionar o índice com variáveis macroeconômicas. A função resposta impulso neste caso teria papel crucial na compreensão de como
                              variáveis econômicas responderiam a uma melhora do índice.'
                   ),
                   h2(
                     'Isso é, como a variação da taxa de crescimento da atividade econômica americana responderia a uma melhora no índice? É mostrado que, por exemplo, quanto mais otimista fosse o índice, maior
                              seria a resposta negativa da taxa de juros real. Ainda, quanto mais otimista fosse o índice melhor seria a resposta da atividade econômica, ou pior seria a resposta da inflação acumulada'
                   ),
                   h2(
                     span(
                       'Hudson ainda propõe outras aplicações para índices de otimismo. Visto que os índices têm correlações com variáveis reais da economia, seria possível, então, outras aplicações - indo desde
                                   testes de causalidades a utilização desses para prever tendências da política monetária.',
                       style = 'color:green'
                     )
                   ),
                   h2(''),
                   br()
                 ))
        ),
        
        # Web Scraping ====
        
        tabItem(
          tabName = 'webscraping',
          h1(
            span('Coleta de Dados (Web Scraping)', style = 'color:green'),
            align = 'center'
          ),
          br(),
          column(
            width = 12,
            box(
              width = 12,
              status = "warning",
              h2(
                'Coleta de Dados (ou web scraping ) pode ser definida como a técnica de coleta de informação (dados) a partir de um ambiente web. Esta técnica tem como sustento as características homogêneas
                           que existem no armazenamento das informações dos computadores. Desta forma, é possível utilizar microprocessadores para obter e processar informações contidas em computadores. '
              ),
              br(),
              br(),
              HTML('<center><img src="webscraping.png"></center>'),
              br(),
              h2(
                span(
                  'Dizemos, então, que o web scraping compreende um conjunto de algoritimos afim de processar informações na rede.',
                  style = 'color:green'
                )
              )
            )
          )
        ),
        
        # text mining ====
        
        tabItem(
          tabName = 'textmining',
          h1(
            span('Mineração de textos (ou Text Mining)', style = 'color:green'),
            align = 'center'
          ),
          br(),
          column(width = 12,
                 box(
                   width = 12,
                   status = "warning",
                   h2(
                     'A mineração de textos (ou text mining) pode ser definida como a técnica de exploração de dados de forma não estruturados. Em outras palavras, é uma técnica de quantificação de textos. Um conjunto de
                              texto (corpus) é definida como o objeto de trabalho, afim de se extrair informações não tão claras.'
                   ),
                   h2(
                     span(
                       'Esta técnica tem algumas peculiaridades particulares: para um melhor entendimento de expressões e termos, utiliza-se, por exemplo, dicionários léxicos. Esses dicionários são na maioria das vezes
                                   oriundos de pesquisas institucionais que tem o objetivo de atribuir valores a certas palavras. Como exemplo, faz sentido pensarmos que "melhora" possui um peso positivo, e "piora" um peso negativo.',
                       style = 'color:green'
                     )
                   ),
                   br(),
                   h2(
                     'Ainda, utilizamos como ferramenta a chamada técnica de "stop words". Esta técnica consiste em remover de um texto palavras não essenciais para a sua compreensão - como por exemplo "a", "às",
                              "até"...'
                   ),
                   h2(
                     span(
                       'O objetivo aqui foi extrair as informações necessárias para melhor entendimento dos principais termos tratados em cada período econômico analisado, bem como os três períodos conjuntamente.
                                   Assim, a partir de um corpus (conjunto total de atas), podemos inferir certos resultados...',
                       style = 'color:green'
                     )
                   )
                 ))
        ),
        
        # indice de sentimentos ====
        
        tabItem(
          tabName = 'indice',
          h1(span('Índice de Sentimentos', style = 'color:green'), align = 'center'),
          br(),
          column(
            width = 12,
            box(
              width = 12,
              status = "warning",
              h2(
                'O objetivo do índice de sentimentos deste trabalho é quantificar as expressões e termos mais utilizados durante as reuniões do COPOM de 2003 a 2018, afim de extraírmos possíveis indícios de
                              otimismo e pessimismo. Foi seguida a metodologia proposta por Hudson Costa (2016), tal qual, dá ao índice o seguinte formato:'
              ),
              withMathJax(),
              h2(
                span('$$indice_i = \\frac{NP_i}{NP_i + NN_i}$$', style = 'color:green')
              ),
              br(),
              h2(
                'Isso é, levamos em consideração uma reunião "i" e a partir desta extraimos as palavras positivas e negativas. Desta forma, o índice se torna relativo e varia de 0 à 1.'
              ),
              HTML(
                '<center><img src="indice.png" height="420" width="1200"></center>'
              )
            )
            
          )
        ),
        
        # kl ====
        
        tabItem(
          tabName = 'liebler',
          h1(
            span('Divergência de Kullback-Liebler', style = 'color:green'),
            align = 'center'
          ),
          br(),
          column(
            width = 12,
            box(
              width = 12,
              status = "warning",
              h2(
                'A divergência de Kullback-Liebler é uma ferramenta estatística utilizada para analisar semelhanças e diferenças entre variáveis aleatórias.'
              ),
              h2('Para o caso contínuo ela é dada por:'),
              withMathJax(),
              h2(
                span(
                  '$$I(f,g) = \\int f(x) log \\left(\\frac{f(x)}{g(x)}\\right) \\quad dx$$',
                  style = 'color:green'
                )
              ),
              h2('e para o caso discreto nós temos:'),
              withMathJax(),
              h2(
                span(
                  '$$I(f,g) = \\sum_{i=1}^{k} p(x_i) log \\left(\\frac{p(x_i)}{q(x_i)}\\right)$$',
                  style = 'color:green'
                )
              ),
              h2(
                'A notação I(f,g) representa "a informação perdida quando g é utilizado para aproximar f". Assim, se ambas as distribuições f e g forem iguais, I(f,g) = 0 '
              )
            )
          )
        ),
        
        # var ====
        
        tabItem(
          tabName = 'var',
          h1(span('Vetor Auto-Regressivo', style = 'color:green'), align = 'center'),
          br(),
          column(
            width = 12,
            box(
              width = 12,
              status = "warning",
              h2(
                'O surgimento dos modelos auto-regressivos são oriundos da década de 80, como alternativa às críticas dos grandes números de restrições impostas às estimações pelos modelos estruturais.'
              ),
              h2(
                'Matematicamente, é possível representar um modelo VAR de primeira ordem (VAR(1)) da seguinte forma:'
              ),
              withMathJax(),
              h2(
                span(
                  '$$y_{1t} = \\delta_1 + \\phi_{11}y_{1,t-1} + \\phi_{12}y_{2,t-1} + u_{1t} \\\\
                                    y_{2t} = \\delta_2 + \\phi_{21}y_{1,t-1} + \\phi_{22}y_{2,t-1} + u_{2t}$$',
                  style = 'color:green'
                )
              ),
              h2('Ainda, considerando a notação matricial:'),
              withMathJax(),
              h2(
                span(
                  '$$\\begin{bmatrix} y_{1t} \\\\  y_{2t} \\end{bmatrix}= \\begin{bmatrix}\\delta_1 \\\\ \\delta_2 \\\\ \\end{bmatrix} + \\begin{bmatrix} \\delta_1 \\\\ \\delta_2 \\\\ \\end{bmatrix}
                           + \\begin{bmatrix} \\phi_{11} & \\phi_{21} \\\\ \\phi_{12} & \\phi_{22} \\\\ \\end{bmatrix} \\begin{bmatrix}  y_{1,t-1} \\\\ y_{2,t-1} \\\\ \\end{bmatrix} + \\begin{bmatrix} u_{1t} \\\\ u_{2t} \\\\
                           \\end{bmatrix}$$',
                  style = 'color:green'
                )
              ),
              h2(
                'Onde os "y" representam variáveis endógenas e os "u" os erros para cada equação. Já os coeficientes relacionados representam a dependência linear das variáveis.'
              )
            )
          )
        ),
        
        
        
        # NUVEM DE PALAVRAS ====
        
        tabItem(tabName = "wordclouds",
                fluidRow(
                  box(
                    h3(
                      span('Núvem de Palavras de um Período ou Gestão', style = 'color:green'),
                      align = 'center'
                    ),
                    plotOutput("nuvem", height = 500, width = '90%'),
                    
                    selectInput(
                      "escolhanuvem",
                      label = h3("Selecione o período de gestão do BCB desejado"),
                      choices = list(
                        "Todos os Períodos" = 'geral',
                        "Meirelles" = 'meirelles',
                        "Tombini" = 'tombini',
                        "Goldfajn" = 'goldfajn'
                      ),
                      selected = 1
                    ),
                    
                    hr(),
                    fluidRow(column(3, verbatimTextOutput("value")))
                  ),
                  
                  box(
                    h2(
                      span("O que é uma Núvem de Palavras?", style = 'color:green'),
                      align = 'center'
                    ),
                    br(),
                    h3(
                      'Núvem de palavras (wordclouds) é uma ferramenta amplamente utilizada no campo da análise textual por permitir uma visualização simples
                       e compacta do que acontece em um documento ou num conjunto de documentos. Aqui apresentamos uma opção um pouco mais customizavel - isso é,
                       é possível escolhermos para qual período queremos a núvel de palavras, bem como qual a quantidade de palavras desejada para este período.'
                    ),
                    br(),
                    br(),
                    sliderInput(
                      "slidenuvem",
                      "Número de palavras desejado para a comparação:",
                      1,
                      200,
                      60
                    )
                  )
                  
                )),
        
        # KULLBACK-LIEBLER ====
        
        tabItem(tabName = 'kullback',
                fluidRow(
                  box(
                    title = h1('A divergência de Kullback-Liebler'),
                    br(),
                    withMathJax(),
                    h3(
                      'A divergência de Kullback-Liebler é um critério de informação utilizado para mensurar proximidades de distribuições simples ou contínuas. Para o caso das distribuições discretas,
                       temos: sejam f(x) e g(x) duas distribuições discretas. A divergência de Kullback-Liebler será dada por:',
                      span(
                        '$$I(f,g) = \\sum p(x_i) \\ast \\log \\frac{p(X_i)}{q(x_i)}$$',
                        style = 'color:green'
                      ),
                      '
                       A notação I(f,g) representa " a informação perdida quando g é usado para aproximar f" '
                    ),
                    h3(
                      'Dessa forma, se as distribuições forem idênticas, o valor de I(f,g) será 0, quanto mais estas se afastarem, maior será o valor de I(f,g)'
                    )
                  ),
                  box(
                    title = h3(span('Mensurando a divergência de K-L', style = 'color:green')),
                    h3(
                      'Quando lidamos com palavras e textos, lidamos fundamentalmente com dados discretos. Dessa forma, é possível compararmos distribuições relativas a palavras e a expressões. É possível por meio desta
                       seção calcularmos o valor da divergência de K-L para as 100 palavras que mais aparecem nas atas do COPOM em cada período, escolhendo o período referência e o período relativo:'
                    ),
                    sliderInput(
                      "slidekl",
                      h3("Número de palavras desejado para o cálculo da divergência:"),
                      1,
                      100,
                      10
                    ),
                    
                    selectInput(
                      "kl1",
                      label = h3("Selecione o período de gestão referência"),
                      choices = list(
                        "Meirelles" = 'meirelles',
                        "Tombini" = 'tombini',
                        "Goldfajn" = 'goldfajn'
                      ),
                      selected = 'meirelles'
                    ),
                    selectInput(
                      "kl2",
                      label = h3("Selecione o período de gestão relativo"),
                      choices = list(
                        "Meirelles" = 'meirelles',
                        "Tombini" = 'tombini',
                        "Goldfajn" = 'goldfajn'
                      ),
                      selected = 'tombini'
                    ),
                    hr(),
                    fluidRow(column(
                      8, h3('Valor da divergência I(f,g):'), verbatimTextOutput("kl")
                    ))
                    
                  )
                  
                )),
        
        # impulso resposta ====
        
        tabItem(tabName = 'fri',
                fluidRow(
                  box(
                    h2('Função de Resposta Impulso'),
                    h3(
                      span(
                        'A partir de um VAR(2), foi estimado a função de resposta impulso por meio da identificação de Cholesky para as variáveis: Índice de Otimismo; Índice de Atividade Econômica do Banco Central (Ibc-Br);
                             IPCA acumulado em 12 meses.',
                        style = 'color:green'
                      )
                    ),
                    h3(
                      '1 - A partir de um impulso no Índice de Otimismo, o Ibr-Br sofre uma resposta positiva, tendendo a retornar ao seu estado estacionário em cerca de 15 períodos'
                    ),
                    br(),
                    br(),
                    h3(
                      '2 - A partir de um impulso no Índice de Otimismo, o IPCA acumulado em 12 meses sofre uma resposta negativa tendendo, também, a retornar ao seu estado estacionário em cerca de 15 períodos.'
                    ),
                    br(),
                    br(),
                    h3(
                      span(
                        'Por fim, o Índice de Otimismo retorna ao seu estado estacionário em cerca de 15 períodos, após um impulso nele mesmo.',
                        style = 'color:green'
                      )
                    )
                  ),
                  box(img(src = 'impulsoresposta.png', width = 730))
                  
                )),
        
        # FREQUENCIA ====
        
        tabItem(tabName = 'freq',
                fluidRow(column(
                  width = 12,
                  box(
                    width = 12,
                    h2(
                      span(
                        'Análise de Frequência das palavras que mais parecem nas ATAS do COPOM',
                        style = 'color:green'
                      ),
                      align = 'center'
                    ),
                    br(),
                    h3(
                      'Nesta seção é possível compararmos as frequências relativas que mais aparecem nas ATAS do COmitê de política monetária por períodos, ou mesmo durante todo o período analisado
                       (2003-2018). A análise de frequências nos permite entender quais foram os temas mais abordados pelas gestões e - por vezes - relacionarmos esses temas com um possível cenário macroeconômico
                       vigente. Por conveniência, é disponibilizado as opções de visualização das 10 palavras que mais aparecem nas atas.'
                    ),
                    box(
                      width = 6,
                      selectInput(
                        "periodofreq",
                        label = h3("Selecione o período de gestão do BCB desejado"),
                        choices = list(
                          "Meirelles" = 1,
                          "Tombini" = 2,
                          "Goldfajn" = 3
                        ),
                        selected = 1
                      ),
                      selectInput(
                        "palavrafreq",
                        label = h3("Selecione a palavra desejada para o gráfico de densidade"),
                        choices = list(
                          "Inflação" = 1,
                          "Preços" = 2,
                          "Monetária" = 3,
                          "Política" = 4,
                          "Mercado" = 5,
                          "Produção" = 6,
                          "Crescimento" = 7,
                          "Industrial" = 8,
                          "Consumidor" = 9,
                          "Crédito" = 10
                        ),
                        selected = 1
                      )
                      
                    ),
                    box(width = 6,
                        plotOutput(
                          'freqgeral', height = 500, width = '90%'
                        ))
                  )
                ))),
        
        # BIBILOGRAFIA ====
        
        tabItem(
          tabName = 'bib',
          h1(
            span('Referências Bibliográficas Principais', style = 'color:green'),
            align = 'center'
          ),
          br(),
          br(),
          h2(
            strong(
              '- BURNHAM, K. P.; ANDERSON, D. R. A practical information-theoretic approach.Model selection and multimodel inference, 2nd ed. Springer, New York, 2002.'
            )
          ),
          h2(
            strong(
              '- COSTA, H. C. Big data, machine learning e text mining em economia: Estudos recentes e análise de sentimento do bacen. 2016.'
            )
          ),
          h2(
            strong(
              '- ENDERS, W. Applied econometric time series. [S.l.]: John Wiley & Sons, 2008.'
            )
          ),
          h2(
            strong(
              '- FEINERER, I.; HORNIK, K.; MEYER, D. Text mining infrastructure in R. Journal of  Statistical  Software,  v.  25,  n.  5,  p.  1–54,  March  2008.'
            )
          ),
          h2(
            strong(
              '- HU, M.; LIU, B. Mining opinion features in customer reviews. In: AAAI. [S.l.: s.n.], 2004. v. 4, n. 4, p. 755–760.'
            )
          ),
          h2(
            strong(
              '- WICKHAM, H. rvest: Easily Harvest (Scrape) Web Pages . [S.l.], 2019. R package version 0.3.4'
            )
          ),
          h2(
            strong(
              '- SIMS, C. A. Macroeconomics and reality. Econometrica: journal of the Econometric Society, JSTOR, p. 1–48, 1980'
            )
          ),
          h2(
            strong(
              '- SHAPIRO, A. H.; WILSON, D. Taking the fed at its word: Direct estimation of central bank objectives using text analytics. In: FEDERAL RESERVE BANK OF SANFRANCISCO. [S.l.], 2019. '
            )
          ),
          br(),
          img(
            src = 'opensource.png',
            width = 110,
            height = 120
          )
        )
      )
    )
  )

# Server ====

server <-
  function(input, output, session) {
    # Nuvem de palavras ====
    output$nuvem <- renderPlot({
      datanuvem <- switch(
        input$escolhanuvem,
        "geral" = nuvemgeral,
        "meirelles" = nuvemmeirelles,
        "tombini" = nuvemtombini,
        "goldfajn" = nuvemgoldfajn
      )
      
      wordcloud(
        words = datanuvem$word,
        freq = datanuvem$n,
        max.words = seq_len(input$slidenuvem),
        random.order = FALSE,
        use.r.layout = FALSE,
        colors = brewer.pal(12, "Paired")
      )
    })
    
    # K-L ====
    output$kl <- renderText({
      p <- switch(
        input$kl1,
        "meirelles" = klmeirelles,
        "tombini" = kltombini,
        "goldfajn" = klgoldfajn
      )
      q <- switch(
        input$kl2,
        "meirelles" = klmeirelles,
        "tombini" = kltombini,
        "goldfajn" = klgoldfajn
      )
      
      kl.distance(as.data.frame(p[, 1:input$slidekl]), as.data.frame(q[, 1:input$slidekl]))
      
    })
    
    # frequencias ====
    
    output$freqgeral <- renderPlot({
      data <- switch (input$periodofreq,
                      '1' = freqmeirelles,
                      '2' = freqtombini,
                      '3' = freqgoldfajn)
      
      ggplot(data) +
        geom_density(aes(x = data[, as.integer(input$palavrafreq) + 1]), fill = cores[as.integer(input$palavrafreq)], alpha = .6) +
        labs(
          title = paste('Gráfico de Densidade para "', colnames(data)[as.integer(input$palavrafreq) + 1], '"' , sep = ''),
          x = '',
          y = 'Densidade'
        ) +
        theme_minimal()
      
      
    })
    
    
  }

shinyApp(ui, server)

