## Repositório para organização do Trabalho de Monografia

Repositório criado para uma melhor organização dos códigos utilizados para uma análise de sentimento das atas (minutes) do banco central (BC) no período de 2003-2018, período Lula-Temer.

Fundamentalmente os scripts listados aqui estão em .R. De forma geral, o sistema, e o R (versão) utilizado se apresenta da seguinte forma:

```
platform       x86_64-pc-linux-gnu         
arch           x86_64                      
os             linux-gnu                   
system         x86_64, linux-gnu           
status                                     
major          3                           
minor          6.0                         
year           2019                        
month          04                          
day            26                          
svn rev        76424                       
language       R                           
version.string R version 3.6.0 (2019-04-26)
nickname       Planting of a Tree
```
### Pastas

As pastas utilizadas, em geral servem para organização e armazenamento de dados já coletados. A pasta referencia faz referência ao material bibliográfico utilizado; dados, por sua vez, corresponde aos _minutes_ (na primeira vez que o código foi rodado).

As atualizações dos códigos são feitas de forma esporádicas - alguns, por sua vez, podem não estar atualizados.

A pasta monografia contém o documento em LaTeX - main.tex, _childs_ e classe, bem como os _logs_   

--- 

### Ordem dos Códigos

Para uma boa compreensão dos códigos, e afim de organizar o funcionamento dessa estrutura, lista-se em uma tabela a data de criação, modificação - em ordem necessária para o bom funcionamento dos _scripts_.

Os códigos de análises gráficas, no entanto, não seguem essa logíca, visto que esses serão os últimos a serem rodados, bem como testes de distribuição (por Kullback-Liebler), ou análise **efetiva** de sentimentos.

|**script**| **Data de criação** | **Última Data de Modificação** |
|----------:|---------------------:|--------------------------------:|
|webscrapping_minutes.R|8 de junho de 2019|18 de agosto de 2019|
|data_tibble.R|7 de agosto de 2019|18 de agosto de 2019|
|analise_economica.R|18 de agosto de 2019|18 de agosto de 2019|
|frequencias_economicas.R|7 de agosto de 2019|18 de agosto de 2019|
|analise_economica_geral.R|18 de agosto de 2019|18 de agosto de 2019|
|analise_economica_periodos.R|18 de agosto de 2019|18 de agosto de 2019|
|tibble_periodos.R|18 de agosto de 2019|18 de agosto de 2019|
|frequencia_relativa.R|21 de agosto de 2019|21 de agosto de 2019|
|analise_relativa_periodos.R|21 de agosto de 2019|21 de agosto de 2019|
|graficos.R|21 de outubro de 2019|21 de outubro de 2019|
|kullback_liebler.R|21 de outubro de 2019|21 de outubro de 2019|
|var.R|21 de outubro de 2019|21 de outubro de 2019|
|shinyDashboard.R|15 de novembro de 2019|15 de novembro de 2019|






