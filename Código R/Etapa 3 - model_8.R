#Limpar
rm(list=ls())


#Pacotes
library(data.table)
library(dplyr)
library(tidyr)
library(xtable)
library(stargazer)


#Diretório
setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/Dissertacao/data")


#Carregamento e configuração dos dados----

base <- read.table("base_window_008.csv", header = TRUE,sep = ",",dec=".",
                   fileEncoding = "latin1")

base<-base %>% 
  mutate("home_match"=ifelse(condicao=="mandante",1,0)) %>% 
  select(-di, -pts_4) %>% 
  mutate("m"=as.numeric(m)) 

base<-base[complete.cases(base),]



#Selecionando as observações da janela----

k<-8                      #definir o tamanho da janela

x<-c(seq(1,nrow(base),k))
temporada<-matrix(nrow=length(x))
time<-matrix(nrow=length(x))
pos_adv<-matrix(nrow=length(x))
pts<-matrix(nrow=length(x))
vit<-matrix(nrow=length(x))
diff_gols<-matrix(nrow=length(x))
home_match<-matrix(nrow = length(x))
tratado<-matrix(nrow = length(x))
periodo<-matrix(nrow = length(x))
m<-matrix(nrow = length(x))



for(i in x){
  temporada[[i]]<-c(mean(base$temporada[i:(i+k-1)]))
}

for(i in x){
  time[[i]]<-c(base$time[i])
}

for(i in x){
  pos_adv[[i]]<-c(mean(base$pos_adv_rodada[i:(i+k-1)]))
}

for(i in x){
  pts[[i]]<-c(sum(base$pts[i:(i+k-1)])/(k*3))
}

for(i in x){
  vit[[i]]<-c(sum(base$vit[i:(i+k-1)])/k)
}  

for(i in x){
  diff_gols[[i]]<-c(sum(base$diff_gols[i:(i+k-1)]))
}  

for(i in x){
  home_match[[i]]<-c(sum(base$home_match[i:(i+k-1)]))
}

for(i in x){
  tratado[[i]]<-c(mean(base$tratado[i:(i+k-1)]))
}

for(i in x){
  periodo[[i]]<-c(mean(base$periodo[i:(i+k-1)]))
}

for(i in x){
  m[[i]]<-c(mean(base$m[i:(i+k-1)]))
}


temporada<-temporada[complete.cases(temporada)]
time<-time[complete.cases(time)]
pos_adv<-pos_adv[complete.cases(pos_adv)]
pts<-pts[complete.cases(pts)]
vit<-vit[complete.cases(vit)]
diff_gols<-diff_gols[complete.cases(diff_gols)]
home_match<-home_match[complete.cases(home_match)]
tratado<-tratado[complete.cases(tratado)]
periodo<-periodo[complete.cases(periodo)]
m<-m[complete.cases(m)]

dados<-cbind(temporada,time,pos_adv,pts,vit,diff_gols,home_match,tratado,
             periodo,m)

dados<-as.data.frame(dados)

#Naive Model----

dados.naive<-dados %>% 
  filter(tratado==1)

model_naive1<-formula(pts ~ home_match + pos_adv + periodo)
naive1<-lm(model_naive1, data = dados.naive)

model_naive2<-formula(diff_gols ~ home_match + pos_adv + periodo)
naive2<-lm(model_naive2, data = dados.naive)

model_naive3<-formula(vit ~ home_match + pos_adv + periodo)
naive3<-lm(model_naive3, data = dados.naive)


#Linear Model----


dados.linear<-dados %>% 
  mutate("interaction"=tratado*periodo)

model_linear1<-formula(pts ~ home_match + pos_adv + tratado + periodo + 
                         interaction)
linear1<-lm(model_linear1, data = dados.linear)

model_linear2<-formula(diff_gols ~ home_match + pos_adv + tratado + 
                         periodo + interaction)
linear2<-lm(model_linear2, data = dados.linear)

model_linear3<-formula(vit ~ home_match + pos_adv + tratado + periodo + 
                         interaction)
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