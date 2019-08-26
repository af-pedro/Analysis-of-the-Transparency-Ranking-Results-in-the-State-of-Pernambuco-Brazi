################################################################
########################## SCRIPT ##############################
################################################################

 # Carregando pacotes necessários
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(DataExplorer) == F) install.packages("DataExplorer"); require(DataExplorer)
if(require(dplyr) == F) install.packages("dplyr"); require(dplyr)
if(require(rgdal) == F) install.packages("rgdal"); require(rgdal)
if(require(maptools) == F) install.packages("maptools"); require(maptools)
if(require(ggplot2) == F) install.packages("ggplot2"); require(ggplot2)
if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(modelsummary) == F) install.packages("modelsummary"); require(modelsummary)
if(require(ggstance) == F) install.packages("ggstance"); require(ggstance)
if(require(jtools) == F) install.packages("jtools"); require(jtools)
if(require(scales) == F) install.packages("scales"); require(scales)
if(require(grid) == F) install.packages("grid"); require(grid)
if(require(waffle) == F) install.packages("waffle"); require(waffle)
if(require(texreg) == F) install.packages('texreg'); require(texreg)
if(require(olsrr) == F) install.packages('olsrr'); require(olsrr)
options(scipen = 999)
------------------------------------------------------------------------------------------------------
 # Carregando banco de dados 

  dados <- read_excel("C:/Users/Pedro/Desktop/Mestrado/1º Semestre/Analise de Dados/Trabalho de Conclusao de Cadeira/pedro-fonseca-ad-ufpe-2019/pedro-fonseca-ad-ufpe-2019.xlsx", 
                                           col_types = c("numeric", "text", "numeric", 
                                                         "numeric", "numeric", "text", "numeric", 
                                                         "numeric", "text"))

 # Observando se há NA's

is.na(dados) 

 # Removendo linhas com NA's

dados <- na.omit(dados)

 # Transformando em variavel dummy, em que prefeitos de esquerda são 1 e os de direita são 0

dados <- mutate(dados, prefeitos2016 = paste(as.numeric(dados$ideologia == "Esquerda"))) 


################################### REVISÃO DE LITERATURA ################################################

 # Gráfico da difusão de leis de transparências pelo mundo, baseado no exemplo de Batista (2017)
  
  # Carregando banco de dados

dados_lai <- read_excel("LAI_Paises.xlsx")

  # Selecionando dados

lai_paises <- dados_lai %>% filter(Ranking > 1)
rm(dados_lai)

  # Plotando gráfico

ggplot(data = lai_paises, aes(x = Ano, y = Ranking, group = 1)) +
  labs(x = "Ano",
       y = "Leis de transparência", 
       title = "Gráfico 1 - Leis de transparência no mundo (1960-2014)",
       caption = "Elaboração própria com base em Batista (2018)") +
  geom_step() +
  theme_bw()

############################################### RESULTADOS ###############################################

# Gráfico das % das notas do ranking

parts <- c(`Desejado\nITMPe ≥ 0,75` = 103, `Moderado\n0,75 > ITMPe ≥ 0,50` = 65, 
           `Insuficiente\n0,50 > ITMPe ≥ 0,25` = 10, `Crítico\n0,25 > ITMPe > 0,00` = 6 )

waffle(
  parts, rows = 8, size = 1, colors = c("blue", "yellow", "orange", "Red"), 
  title = "Gráfico 2 - Distribuição dos Municípios por Nível de Transparência em 2018",
  xlab = "Elaboração própria com base nos dados do TCE-PE" )


### ggplot(df, aes(x = group)) +  
### geom_bar(aes(y = (..count..)/sum(..count..))) +  
### scale_y_continuous(labels = percent) ### Realizar gráfico em porcentagem

# Analise exploratoria dos dados

summary(dados)

# Desvio Padrão

sd(dados$ITMPe)
sd(dados$pib2016)
sd(dados$idhm)
sd(dados$regiao)
sd(dados$ifgf)

# Histograma

plot_histogram(dados)

# Densidade

plot_density(dados)

# Gráfico de correlaão

plot_correlation(dados, 
                 type = 'continuous',
                 'Review.Date')



###################### Analisando a relação entre idhm e ITMPe ########################################

# Gráfico de dispersão entre as variável independente idhm e a variável dependente ITMPe: 

ggplot(dados, aes(idhm, ITMPe)) + 
  geom_point()

# Regressao 1

reg1 <- lm(ITMPe ~ idhm, data = dados)

summary(reg1)

confint(reg1)

# Gráfico da regressão 1

ggplot(dados, aes(idhm, ITMPe, color = idhm)) +
  labs(x = "IDHM", 
       y = "ITMPe",
       title = "Gráfico de dispersão",
       caption = "Elaboração própria") +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() 

# Adicionando texto no gráfico

reg1_text <- "R² = 0.058"
reg1_grob = grid.text(reg1_text,
                      x = 0.8, # Posicição no eixo x
                      y = 0.3, # Posição do eixo y
                      gp = gpar(col = "black", # Cor
                                fontsize = 10, # tamanho
                                fontface = "bold"))

# Observando o distribuição de normalidade, erros e resíduos: 

ols_plot_resid_qq(reg1)
ols_plot_resid_fit(reg1)

###################### Analisando a relação entre pib2016 e ITMPe #####################################

# Gráfico de dispersão entre as variável independente pib2016 e a variável dependente ITMPe: 

ggplot(dados, aes(pib2016, ITMPe)) + 
  geom_point()

# Gerando o modelo de regressão
reg2 <- lm(ITMPe ~ pib2016, data = dados)

summary(reg2)

screenreg(reg2)

confint(reg2)

# Gerando o modelo com a transformação log

logpib <- log(dados$pib2016)

# Gráfico de dispersão entre as variável independente logpib e a variável dependente ITMPe: 

ggplot(dados, aes(log(pib2016), ITMPe)) + 
  geom_point()

reg2log <- lm(ITMPe ~ log(pib2016), data = dados)

summary(reg2log)

screenreg(reg2log)

confint(reg2log)

# Gráfico da regressão 2

ggplot(dados, aes(log(pib2016), ITMPe, color = log(pib2016))) +
  labs(x = "PIB 2016", 
       y = "ITMPe", 
       title = "Gráfico de dispersão",
       caption = "Elaboração própria") +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() 

# Adicionando texto no gráfico

reg2_text <- "R² = 0.59"
reg2_grob = grid.text(reg2_text,
                      x = 0.8, # Posicição no eixo x
                      y = 0.3, # Posição do eixo y
                      gp = gpar(col = "black", # Cor
                                fontsize = 10, # tamanho
                                fontface = "bold"))

# Observando o distribuição de normalidade, erros e resíduos: 

ols_plot_resid_qq(reg2)
ols_plot_resid_fit(reg2)

# O modelo alterado:

ols_plot_resid_qq(reg2log)
ols_plot_resid_fit(reg2log)

###################### Analisando a relação entre ifgf e ITMPe ########################################

# Gráfico de dispersão entre as variável independente ifgf e a variável dependente ITMPe: 

ggplot(dados, aes(ifgf, ITMPe)) + 
  geom_point()

# Regressao 3

reg3 <- lm(ITMPe ~ ifgf, data = dados)

summary(reg3)

confint(reg3)

# Gráfico da regressão 3

ggplot(dados, aes(ifgf, ITMPe, color = ifgf)) +
  labs(x = "IFGF",
       y = "ITMPe",
       title = "Gráfico de dispersão",
       caption = "Elaboração própria") +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() 

# Adicionando texto no gráfico

reg3_text <- "R² = 0.31 "
reg3_grob = grid.text(reg3_text,
                      x = 0.8, # Posição no eixo x
                      y = 0.3, # Posição do eixo y
                      gp = gpar(col = "black", # Cor
                                fontsize = 10, # tamanho
                                fontface = "bold"))

# Observando o distribuição de normalidade, erros e resíduos: 

ols_plot_resid_qq(reg3)
ols_plot_resid_fit(reg3)

###################### Analisando a relação entre regiao e ITMPe ########################################

# Gráfico de dispersão entre as variável independente regiao e a variável dependente ITMPe: 

ggplot(dados, aes(regiao, ITMPe)) + 
  geom_point()

# Boxplor por região

ggplot(dados, aes(factor(regiao), ITMPe)) +
  labs(x = "", 
       y = "ITMPe", 
       title = "Gráfico de dispersão",
       caption = "Elaboração própria") +
  geom_boxplot() +
  geom_smooth(method = "lm") +
  theme_bw() 

# Regressao 4

reg4 <- lm(ITMPe ~ regiao, data = dados)

summary(reg4)

confint(reg4)

# Gráfico da regressão 4

ggplot(dados, aes(regiao, ITMPe)) +
  labs(x = "Região de desenvolvimento", 
       y = "ITMPe", 
       title = "Gráfico de dispersão",
       caption = "Elaboração própria") +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw() 

# Adicionando texto no gráfico

reg4_text <- "R² = 0.001"
reg4_grob = grid.text(reg4_text,
                      x = 0.8, # Posicição no eixo x
                      y = 0.3, # Posição do eixo y
                      gp = gpar(col = "black", # Cor
                                fontsize = 10, # tamanho
                                fontface = "bold"))

# Observando o distribuição de normalidade, erros e resíduos: 

ols_plot_resid_qq(reg4)
ols_plot_resid_fit(reg4)

###################### Analisando a relação entre prefeitos e ITMPe ########################################

# Boxplot prefeitos
ggplot(dados, aes(factor(ideologia), ITMPe)) +
  labs(x = "Ideologia dos prefeitos", 
       y = "ITMPe", 
       title = "Gráfico de dispersão",
       caption = "Elaboração própria") +
  geom_boxplot() +
  geom_smooth(method = "lm") +
  theme_bw() 


# Regressão 5

reg5 <- lm(ITMPe ~ factor(ideologia), data = dados)

summary(reg5)

confint(reg5)

# Observando o distribuição de normalidade, erros e resíduos: 

ols_plot_resid_qq(reg5)

ols_plot_resid_fit(reg5)

###################### Modelo multivariado ##############################################

# Regressao 6
reg6 <- lm(ITMPe ~ log(pib2016) + idhm + factor(ideologia) + regiao + ifgf, data = dados)

summary(reg6)

confint(reg6)

# Observando o distribuição de normalidade, erros e resíduos: 

ols_plot_resid_qq(reg6)
ols_plot_resid_fit(reg6)

screenreg(list(reg1, reg2log, reg3, reg4, reg5, reg6))

# Tabela 

models <- list()
models[['reg1']] <- lm(ITMPe ~ idhm, data = dados)
models[['reg2log']] <- lm(ITMPe ~ log(pib2016), data = dados)
models[['reg3']] <- lm(ITMPe ~ ifgf, data = dados)
models[['reg4']] <- lm(ITMPe ~ regiao, data = dados)
models[['reg5']] <- lm(ITMPe ~ factor(ideologia), data = dados)
models[['reg6']] <- lm(ITMPe ~ log(pib2016) + idhm + factor(ideologia) + regiao + ifgf, data = dados)

msummary(models, 
         title = 'Tabela 3 - Resultados',
         notes = '*** p < 0.001; ** p < 0.01; * p < 0.05.',
         stars = TRUE,
         stars_note = FALSE) 

# Gráfico dos modelos

plot_summs(reg1, reg2log, reg3, reg4, reg5, reg6, scale = TRUE, inner_ci_level = .9,
           model.names = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5", "Modelo 6"))

#### MAPA ####

# Pacotes ----------------------------------------------------------------------
if(require(rgdal) == F) install.packages("rgdal"); require(rgdal)
if(require(maptools) == F) install.packages("maptools"); require(maptools)
if(require(ggmap) == F) install.packages("ggmap"); require(ggmap)
if(require(mapproj) == F) install.packages("mapproj"); require(mapproj)
if(require(ggplot2) == F) install.packages("ggplot2"); require(ggplot2)
if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)

# Carregando bases -----------------------------------------------------------------------------------------

 # Carregando shapefile

shapefile_pe <- readOGR("C:/Users/Pedro/Desktop/Mestrado/Analise de Dados/Trabalho de Conclusao de Cadeira/pedro-fonseca-ad-ufpe-2019/Shape"
                        , "26MUE250GC_SIR")
plot(shapefile_pe)
shapefile_pe@data

# Convertendo o shapefile para dataframe -------------------------------------------------------------------
shapefile_df <- fortify(shapefile_pe)
dim(shapefile_df)

names(shapefile_df)
head(shapefile_df)

shapefile_data <- fortify(shapefile_pe@data)
shapefile_data$id <- row.names(shapefile_data)

shapefile_df <- full_join(shapefile_df, shapefile_data, by="id")

names(shapefile_df)
head(shapefile_df)

# Removendo Fernando de Noronha (2605459) da base e produzir o mapa novamente ------------------------------
shapefile_df <- shapefile_df %>% filter(CD_GEOCMU != "2605459")

 # Mapa ggplot
map <- ggplot() +
  geom_polygon(data = shapefile_df,
               aes(x = long, y = lat, group = group, fill = IDHM),
               colour = "black", fill = 'white', size = .2) +
  coord_map()

map

# Fazendo união com a base dados --------------------------------------------------------------------------
dados$codigo <- as.factor(dados$codigo)
shapefile_df <- shapefile_df %>% left_join(dados,
                                           by = c("CD_GEOCMU" = as.character("codigo")))
head(shapefile_df)

map <- ggplot() + 
  geom_polygon(data = shapefile_df,
               aes(x = long, y = lat, group = group, fill = ITMPe),
               colour = "gray", size = .2) +
  theme_void() + # essa é a função que dexa o fundo vazio
  coord_map() +
  labs(title = "Índice de Transparência Municipal de Pernambuco 2018",
       caption = "Elaborção própria com base nos dados do TCE-PE") +
  theme(legend.position = "bottom") # Coloca a legenda abaixo do grafico

map

