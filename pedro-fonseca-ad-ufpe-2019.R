################################################################
########################## SCRIPT ##############################
################################################################

# Carrgando pacotes necessários
if(require(ggplot2) == F) install.packages("ggplot2"); require(ggplot2)
if(require(readxl) == F) install.packages("readxl"); require(readxl)
if(require(DataExplorer) == F) install.packages("DataExplorer"); require(DataExplorer)
if(require(dplyr) == F) install.packages("dplyr"); require(dplyr)
options(scipen = 999)
------------------------------------------------------------------------------------------------------
  # Carregando banco de dados 
  setwd("C:/Users/Pedro/Desktop/Mestrado/Analise de Dados/Trabalho de Conclusao de Cadeira")

dados <- read_excel("pedro-fonseca-ad-ufpe-2019.xlsx")

# Transformando em variavel dummy, em que prefeitos do PSB são 1 e os demais são 0

dados <- mutate(dados, prefeitos2016 = paste(as.numeric(dados$partido2016 == "PSB"))) 

dados$prefeitos2016

# Analise exploratoria dos dados
view(dados)
summary(dados) 
class(dados)
mean(dados)
head(dados)
str(dados)

plot_str(dados)

plot_missing(dados)

plot_histogram(dados)

plot_density(dados)

plot_correlation(dados, 
                 type = 'continuous',
                 'Review.Date')

# Observando os dados graficamente
ggplot(dados, aes(x = regiao, y = IDHM)) +
  geom_point()

# Plotando um gráfico para cada região de PE
ggplot(data = dados, mapping = aes(x = IDHM, y = PIB2015)) +
  geom_line() +
  facet_wrap(~ regiao) +
  labs(title = "PIB x IDHM regiao de PE",
       x = "IDHM",
       y = "PIB") +
  theme_bw() +
  theme(panel.grid = element_blank())

# Grafico de dispersao por regiao
ggplot(data = dados, mapping = aes(x = IDHM, y = PIB2015, color = regiao)) +
  geom_point()

ggplot(data = dados, mapping = aes(x = IDHM, y = PIB2015)) +
  geom_point(aes(color = Avaliacao2))

# Histograma 
ggplot(dados, aes(x = IDHM)) +
  geom_histogram()

ggplot(dados, aes(x = Avaliacao2)) +
  geom_histogram()

# Regressao 1
reg1 <- lm(Avaliacao2 ~ IDHM, data = dados)
summary(reg1)
plot(reg1)
confint(reg1)

ggplot(dados, aes(Avaliacao2, IDHM)) +
  labs(x = "Avaliação", y = "IDHM", title = "Gráfico de dispersão") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Regressao 2
reg2 <- lm(Avaliacao2 ~ PIB2015, data = dados)
summary(reg2)
plot(reg2)
confint(reg2)

ggplot(dados, aes(Avaliacao2, PIB2015)) +
  labs(x = "Avaliação", y = "PIB 2015", title = "Gráfico de dispersão") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Regressao 3
reg3 <- lm(Avaliacao2 ~ IDHM + PIB2015, data = dados)
summary(reg3)
plot(reg3)
confint(reg3)

# Regressao 4
reg4 <- lm(Avaliacao2 ~ regiao, data = dados)
summary(reg4)

ggplot(dados, aes(Avaliacao2, regiao)) +
  labs(x = "Avaliação", y = "regiao", title = "Gráfico de dispersão") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Correlacao
cor(dados$Avaliacao2,dados$IDHM)
cor.test(dados$Avaliacao2, dados$IDHM)

# Grafico de correlacao
ggplot(dados, aes(x = IDHM, y = Avaliacao2)) + 
  geom_point() + 
  ggtitle("IDHM x transparencia") + 
  geom_smooth(method = lm, se = FALSE) + 
  scale_x_continuous(name = "IDHM") + 
  scale_y_continuous(name = "Transparencia") +
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_blank(), 
        axis.line = element_line(color="black"),                                                                                                                                                                                                                                                                                                       
        axis.line.x = element_line(color="black"))

# Grafico de densidade
ggplot(dados, aes(IDHM)) +
  geom_density(alpha = 0.55)

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
shapefile_pe <- readOGR("C:/Users/Pedro/Desktop/Mestrado/Analise de Dados/Trabalho de Conclusão de Cadeira/Shape"
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
Dados$codigo <- as.factor(Dados$codigo)
shapefile_df <- shapefile_df %>% left_join(Dados,
                                           by = c("CD_GEOCMU" = as.character("codigo")))
head(shapefile_df)

map <- ggplot() + 
  geom_polygon(data = shapefile_df,
               aes(x = long, y = lat, group = group, fill = Avaliacao2),
               colour = "gray", size = .2) +
  theme_void() + # essa é a função que dexa o fundo vazio
  coord_map() +
  labs(title = "Transparencia dos municipios de PE",
       subtitle = "") +
  theme(legend.position = "bottom") # Coloca a legenda abaixo do grafico

map