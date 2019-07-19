################################################################
########################## SCRIPT ##############################
################################################################

 # Carregando pacotes necessários
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(DataExplorer) == F) install.packages("DataExplorer"); require(DataExplorer)
if(require(dplyr) == F) install.packages("dplyr"); require(dplyr)
if(require(rgdal) == F) install.packages("rgdal"); require(rgdal)
if(require(maptools) == F) install.packages("maptools"); require(maptools)
if(require(ggmap) == F) install.packages("ggmap"); require(ggmap)
if(require(mapproj) == F) install.packages("mapproj"); require(mapproj)
if(require(ggplot2) == F) install.packages("ggplot2"); require(ggplot2)
if(require(tidyverse) == F) install.packages("tidyverse"); require(tidyverse)
if(require(modelsummary) == F) install.packages("modelsummary"); require(modelsummary)
options(scipen = 999)
------------------------------------------------------------------------------------------------------
 # Carregando banco de dados 

  dados <- read_excel("pedro-fonseca-ad-ufpe-2019.xlsx", 
                      col_types = c("numeric", "numeric", "text", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "text", "numeric"))

 # Transformando em variavel dummy, em que prefeitos do PSB são 1 e os demais são 0

dados <- mutate(dados, prefeitos2016 = paste(as.numeric(dados$partido2016 == "PSB"))) 

dados$prefeitos2016

################################### REVISÃO DE LITERATURA ################################################

 # Gráfico da difusão de leis de transparências pelo mundo, baseado no exemplo de Batista (2017)
  
  # Carregando banco de dados
dados_lai <- read_excel("LAI_Paises.xlsx")

  # Selecionando dados
lai_paises <- dados_lai %>% filter(Ranking > 1)
rm(dados_lai)

  #Plotando gráfico
ggplot(data = lai_paises, aes(x = Ano, y = Ranking, group = 1)) +
  geom_step() +
  theme_bw()

############################################### RESULTADOS ###############################################

# Gráfico das % das notas do ranking
porcentagem <- c(8.65, 9.19, 7.03, 10.81, 8.65, 10.81, 16.76,
                 14.59, 5.41, 5.95, 2.16)
notas <- c("0", "0.1-0.9", "1.0-1.9", "2.0-2.9", 
           "3.0-3.9", "4.0-4.9", "5.0-5.9", "6.0-6.9",
           "7.0-7.9", "8.0-8.9","9.0-10.0")

z <- data.frame(porcentagem, notas)

ggplot(data = z) + 
  geom_bar(mapping = aes(x = notas, y = porcentagem), stat ="identity") +
  labs(x = "Nota", y = "Porcentagem", title = "Gráfico 2 - Distribuição das notas") +
  theme_bw()  



  # Analise exploratoria dos dados

summary(dados) 
sd(dados$Nota)
sd(dados$PIB2016)
sd(dados$IDHM)
sd(dados$regiao)
sd(dados$IFGF)


plot_histogram(dados)

plot_density(dados)

plot_correlation(dados, 
                 type = 'continuous',
                 'Review.Date')


 # Observando os dados graficamente
ggplot(dados, aes(x = regiao, y = IDHM)) +
  geom_point()


 # Plotando um gráfico para cada região de PE
ggplot(data = dados, mapping = aes(x = IDHM, y = PIB2016)) +
  geom_line() +
  facet_wrap(~ regiao) +
  labs(title = "PIB x IDHM regiao de PE",
       x = "IDHM",
       y = "PIB") +
  theme_bw() +
  theme(panel.grid = element_blank())

 # Grafico de dispersao por regiao
ggplot(data = dados, mapping = aes(x = IDHM, y = PIB2016, color = regiao)) +
  geom_point()

ggplot(data = dados, mapping = aes(x = IDHM, y = PIB2016)) +
  geom_point(aes(color = Nota)) +
  theme_bw() 
  
 # Boxplot: prefeitos
ggplot(dados, aes(factor(prefeitos2016), Nota)) +
  labs(x = "Prefeitos", y = "Nota", title = "Gráfico de dispersão") +
  geom_boxplot() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() 

 # Regressao 1
reg1 <- lm(Nota ~ IDHM, data = dados)

summary(reg1)

plot(reg1)
confint(reg1)

ggplot(dados, aes(Nota, IDHM)) +
  labs(x = "Nota", y = "IDHM", title = "Gráfico de dispersão") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

 # Regressao 2
reg2 <- lm(Nota ~ PIB2016, data = dados)

summary(reg2)
plot(reg2)
confint(reg2)

ggplot(dados, aes(Nota, PIB2016)) +
  labs(x = "Nota", y = "PIB 2016", title = "Gráfico de dispersão") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

 # Regressao 3
reg3 <- lm(Nota ~ IFGF, data = dados)

summary(reg3)
plot(reg3)
confint(reg3)


 # Regressao 4
reg4 <- lm(Nota ~ regiao, data = dados)
summary(reg4)

ggplot(dados, aes(Nota, regiao)) +
  labs(x = "Nota", y = "regiao", title = "Gráfico de dispersão") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

  # Regressao 5
reg5 <- lm(Nota ~ PIB2016 + IDHM + factor(prefeitos2016) + regiao, data = dados)

  # Tabela 
install.packages("modelsummary")
require(modelsummary)

models <- list()
models[['reg1']] <- lm(Nota ~ IDHM, data = dados)
models[['reg2']] <- lm(Nota ~ PIB2016, data = dados)
models[['reg3']] <- lm(Nota ~ IFGF, data = dados)
models[['reg4']] <- lm(Nota ~ regiao, data = dados)
models[['reg5']] <- lm(Nota ~ PIB2016 + IDHM + factor(prefeitos2016) + regiao + IFGF, data = dados)

msummary(models, 
         title = 'Tabela 3 - Resultados.',
         notes = '*** p < 0.001; ** p < 0.01; * p < 0.05.',
         stars = TRUE,
         stars_note = FALSE) 

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
               aes(x = long, y = lat, group = group, fill = Nota),
               colour = "gray", size = .2) +
  theme_void() + # essa é a função que dexa o fundo vazio
  coord_map() +
  labs(title = "Transparência dos municipios de Pernambuco",
       subtitle = "") +
  theme(legend.position = "bottom") # Coloca a legenda abaixo do grafico

map
