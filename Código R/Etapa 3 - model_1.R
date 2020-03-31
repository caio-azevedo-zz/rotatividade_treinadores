

#Limpar
rm(list=ls())


#Diretório
setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/Dissertacao/data")

#Pacotes
library(data.table)
library(dplyr)
library(tidyr)
library(xtable)
library(stargazer)


#Carregamento e configuração dos dados----

dados <- read.table("base_window_001.csv", header = TRUE,sep = ",",dec=".",
                   fileEncoding = "latin1")


dados<-dados %>% 
  select(-di,-pts_4) %>% 
  mutate("home_match"=ifelse(condicao=="mandante",1,0))



#Naive Model----

dados.naive<-dados %>% 
  filter(tratado==1)

dados.naive<-dados.naive[complete.cases(dados.naive),]

model_naive1<-formula(pts ~ home_match + pos_adv_rodada + periodo)
naive1<-lm(model_naive1, data = dados.naive)

model_naive2<-formula(diff_gols ~ home_match + pos_adv_rodada + periodo)
naive2<-lm(model_naive2, data = dados.naive)

model_naive3<-formula(vit ~ home_match + pos_adv_rodada + periodo)
naive3<-lm(model_naive3, data = dados.naive)


#Linear Model----
dados.linear<-dados[complete.cases(dados),]

dados.linear<-dados.linear %>% 
  mutate("interaction"=tratado*periodo)

model_linear1<-formula(pts ~ home_match + pos_adv_rodada + tratado + periodo + 
                         interaction)
linear1<-lm(model_linear1, data = dados.linear)

model_linear2<-formula(diff_gols ~ home_match + pos_adv_rodada + tratado + 
                         periodo + interaction)
linear2<-lm(model_linear2, data = dados.linear)

model_linear3<-formula(vit ~ home_match + pos_adv_rodada + tratado + periodo + interaction)
linear3<-lm(model_linear3, data = dados.linear)

#Saídas----
summary(naive1)
summary(naive2)
summary(naive3)
summary(linear1)
summary(linear2)
summary(linear3)

#stargazer(naive1,naive2,naive3, decimal.mark = ",")
#stargazer(linear1,linear2,linear3, decimal.mark = ",")



         

         
        


