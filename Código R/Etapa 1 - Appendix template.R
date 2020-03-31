#Limpar
rm(list=ls())


#Pacotes
library(data.table)
library(dplyr)
library(tidyr)
library(stargazer)


#Diretório
setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/Dissertacao/data")


#Carregamento e configuração dos dados----

base <- read.table("base.csv", header = TRUE,sep = ",",dec=".",
                   fileEncoding = "latin1")


base<-base %>% 
  select(-m,-di,-tratado,-periodo)

base<-base[complete.cases(base),]

base<-base %>% 
  mutate(condicao=ifelse(condicao=="mandante",1,0)) %>% 
  mutate(derrota=1-vit) %>% 
  mutate(int = derrota*condicao) %>% 
  mutate(zona = ifelse(pos_rodada>11,1,0))



#Modelo----

logit <- glm(trocou ~ cum_surpresa + pts_4 + int + diff_gols + pos_adv_rodada , 
              family = binomial(link = "logit"), 
              data = base)


probit <- glm(trocou ~ cum_surpresa + pts_4 + int + diff_gols + pos_adv_rodada , 
             family = binomial(link = "probit"), 
             data = base)

stargazer(logit,probit, decimal.mark = ",")

