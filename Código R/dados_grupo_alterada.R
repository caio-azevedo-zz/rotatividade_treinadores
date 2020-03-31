
# Limpar
rm(list=ls())


#Diretório
setwd("C:/Users/Caio Azevedo/Dropbox/cb_demissao_tecnico/Caio/data")


# Pacotes
library(dplyr)
library(tidyr)
library(readxl)
library(optmatch)
library(RItools)
library(ggplot2)
#library(xlsx)




#Carregamento e configuração dos dados----
tb_brasileirao <-read_xlsx("tb_brasileirao_alterada.xlsx")
tb_mudanca <- read_xlsx("tb_mudanca_tecnico_alterada.xlsx")

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
z2<-z %>% 
  mutate(Tratamento = ifelse(trocou==1, "Sim", "Não"))

# Desbloquear para gerar Tabela do apêndice
# appendix<-z %>%
# select("rodada","temporada","time","trocou","m")

z<-z %>% 
  select("cod","m","di")

base<-left_join(base,z,c("cod"))


base<-base %>% 
  mutate(tratado=NA, periodo=NA)

#Exportando----
#write.xlsx(base,file = "base.xlsx")
#write.table(base,file='base.csv',sep=',',na="",quote=TRUE, row.names=FALSE)


#Gráficos----


#Configuração
cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_line(color = "white"),
                panel.background = element_blank(),
                legend.position = "bottom",
                axis.line = element_line(color = "black"),
                axis.title = element_text(size = 18),
                legend.text = element_text(size = 14),
                axis.text = element_text(size = 14))

cleanup1 = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_line(color = "white"),
                panel.background = element_blank(),
                legend.position = "none",
                axis.line = element_line(color = "black"),
                axis.title = element_text(size = 24),
                axis.text = element_text(size = 24))



# Gráficos Heterogeneidade

setwd("C:/Users/Caio Azevedo/Dropbox/cb_demissao_tecnico/Caio/Figuras")

#Caso Vasco

#Configurando os dados

Vasco2017 <- base %>% filter(time=="Vasco",temporada==2017) %>%
  mutate(mt_rodada = if_else(m==1.7,rodada,NULL))

Vasco2010 <- base %>% filter(time=="Vasco",temporada==2010) %>%
  mutate(mt_rodada = if_else(m==1.7,rodada,NULL))

Vasco<-rbind(Vasco2010,Vasco2017)

Vasco<-Vasco %>% 
  mutate(Grupo=ifelse(temporada=="2010","Controle","Tratamento"))


#Gráfico

ggplot(data=Vasco, aes(x=rodada, y=cum_surpresa, colour=Grupo)) +
  geom_line(lwd=2) + 
  geom_vline(aes(xintercept=mt_rodada, colour=Grupo),lwd=1.4) +
  xlab("Rodada") + 
  ylab("Surpresa acumulada")+
  cleanup

dev.copy(pdf,"vasco.pdf")
dev.off()

#Caso Internacional

#Configurando os dados

Internacional2009 <- base %>% filter(time=="Internacional",temporada==2009) %>%
  mutate(mt_rodada = if_else(m==1.5,rodada,NULL))

Internacional2014 <- base %>% filter(time=="Internacional",temporada==2014) %>%
  mutate(mt_rodada = if_else(m==1.5,rodada,NULL))

Internacional<-rbind(Internacional2009,Internacional2014)

Internacional<-Internacional %>% 
  mutate(Grupo=ifelse(temporada=="2014","Controle","Tratamento"))


#Gráfico

ggplot(data=Internacional, aes(x=rodada, y=cum_surpresa, colour=Grupo)) +
  geom_line(lwd=2) + 
  geom_vline(aes(xintercept=mt_rodada, colour=Grupo),lwd=1.4) +
  xlab("Rodada") + 
  ylab("Surpresa acumulada")+
  cleanup

dev.copy(pdf,"internacional.pdf")
dev.off()





# Densidade das mudanças 

# Mudando diretório
setwd("C:/Users/Caio Azevedo/Dropbox/cb_demissao_tecnico/Caio/Figuras")

# Configurando os dados

q <- dados %>%
  mutate(
    Mudança = if_else(
      !is.na(mt_antecessor) & mt_motivo=="Demitido","Demissões e renúncias",
      if_else(!is.na(mt_antecessor) & mt_motivo=="Resignado","Demissões e renúncias",
      if_else(!is.na(mt_antecessor) & mt_motivo=="Renunciado","Demissões e renúncias",      
      if_else(is.na(mt_antecessor), "Sem trocas","Outras motivações")
    ))),
    tratado = if_else(!is.na(mt_antecessor),1,0)
  ) %>%
  drop_na(Mudança)

#Gráfico

ggplot(data=q, aes(x=cum_surpresa,color=Mudança)) +  
         # definimos o tipo de geometria (gráfico)  
         geom_density(lwd=2) +
         xlab("Surpresa Acumulada") + 
         ylab("Densidade")+cleanup1

dev.copy(pdf,"cum_surpresa.pdf")
dev.off()


#Sobreposição de pareamento----

ggplot(data=z2, aes(x=cum_surpresa,color=Tratamento)) +  
  # definimos o tipo de geometria (gráfico)  
  geom_density(lwd=2) +
  xlab("Surpresa Acumulada") + 
  ylab("Densidade") + cleanup1

dev.copy(pdf,"sob_surp.pdf")
dev.off()





#Saída---- 
#print(xtable(appendix, caption = "Pareamento", 
 #            digits = 0,
  #           label = "parmatch"),
   #   caption.placement = "top",
    #  include.rownames = FALSE)


tab<-dados %>% 
  mutate(Mudança = if_else(mt_motivo=="Demitido","Coach dismissals",
      if_else(mt_motivo=="Resignado","Quits",
              if_else(mt_motivo=="Renunciado","Quits",      
                      if_else(is.na(mt_antecessor), "No coach dismissals","Other reasons")
              )))) %>% 
  filter(trocou==1) %>% 
  group_by(temporada.x,Mudança) %>% 
  summarise("Frequência"=n())
  