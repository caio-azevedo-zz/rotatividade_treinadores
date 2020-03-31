
# Limpar
rm(list=ls())


#Diretório
setwd("C:/Users/Caio Azevedo/Documents/Documentos Caio/Github/Dissertacao")


# Pacotes
library(dplyr)
library(tidyr)
library(readxl)
library(optmatch)
library(RItools)
library(ggplot2)





#Carregamento e configuração dos dados----
tb_brasileirao <-read_xlsx("tb_brasileirao.xlsx")
tb_mudanca <- read_xlsx("tb_mudanca_tecnico.xlsx")

dados<-left_join(tb_brasileirao,tb_mudanca,c("cod"))

rm(tb_brasileirao,tb_mudanca)

dados<-dados %>% 
  mutate("trocou"=ifelse(is.na(time.y),0,1)) 


base<-dados %>% 
  select(-"time.y",-"mt_antecessor",-"mt_motivo",-"mt_ultimo_jogo",-"condição"
         ,-"mt_sucessor",-"temporada.y",-"mt_data",-"mt_rodada",-"mt_posicao")

base<-base %>% 
  rename("temporada"="temporada.x", "time"="time.x")


#Inicio PSM----

x<-read_xlsx("times.xlsx")

#Criação de lista para recebeminto dos dados
dados.time<-vector('list',32)
b<-vector('list',32)

for(i in c(1:33)[-20]){ 
  dados.time[[i]]<-filter(base, time==x$Time[i])
  
  
  dados.time[[i]]$m <- pairmatch(glm(trocou ~ cum_surpresa, family = binomial(
                        link = "logit") ,data=dados.time[[i]])
                                 ,grouped=TRUE,method = "nearest",controls = 5) 
  
  dados.time[[i]] <- filter(dados.time[[i]],!is.na(m)) 
  
  b[[i]] <- dados.time[[i]] %>%
    group_by(m) %>%
    mutate(
      freq = n(),
      di = abs(cum_surpresa[trocou==1] - cum_surpresa)
    ) %>% 
    filter(di < 0.5, freq>1, rodada>4,rodada<35) %>%
    mutate(
      freq = n()
    ) %>% 
    arrange(m,di) %>% 
    distinct(temporada,.keep_all = TRUE) %>%
    slice(1:2) %>% 
    group_by(m) %>%
    mutate(
      media = mean(trocou)
    ) %>%
    filter(media==0.5) %>% 
    select(-"media",-"freq")
  i<-i+1
}


z<-b[[1]]

for(i in c(2:33)[-21]){
  z<-rbind(z, b[[i]])
  i<-i+1
}


rm(b,dados.time,i)

# Gerar gráfico de sobreposição de pareamento
z<-z %>% 
  mutate(Tratamento = ifelse(trocou==1, "Sim", "Não"))


z2<-z %>% 
  select("cod","m","di")

base<-left_join(base,z2,c("cod"))


base<-base %>% 
  mutate(tratado=NA, periodo=NA)

#Exportando----
write.table(base,file='data/base.csv',sep=',',na="",quote=TRUE, row.names=FALSE)


#Salvando----

save(base, dados, z ,file='data/selecionados.rData')


  