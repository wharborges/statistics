################################################################################
###############PROJETO NOCOES DE PROBABILIDADE E ESTATISTICA EM R###############
################################################################################
# PACOTES ESSENCIAIS
install.packages("Epi")
library(tidyverse)
library(Epi)
library(ggplot2)
library(dplyr)
library(tidyselect)
#--------------------------------------//--------------------------------------
# Exercicios
#--------------------------------------//--------------------------------------
#23) Os dados a seguir representam individuos que foram contaminados pelo veneno
#de um certo tipo de inseto e submetidos a tratamento (três diferentes tipos). 
#--------------------------------------//--------------------------------------
#As variaveis sao:
#Paciente: numero do prontuario do paciente.
#Idade: idade do paciente no momento de admissao, em anos.
#Diag: tempo, em horas, gasto entre o contato com o inseto e administracao do 
#tratamento e recuperacao.
#Recup: tempo, em horas, entre a administracao do tratamento e recuperacao.
#Tratam: tipo do tratamento administrado.
#Coag: presenca de coagulos no momento de admissao.
#--------------------------------------//--------------------------------------
# Resolucao 23)
#--------------------------------------
# Entrada/Digitacao dos dados:
#--------------------------------------
Paciente<-c(19,4,27,7,14,5,11,10,25,6,16,20,13,15,8,18,12,24,21,22,3,2,23,26,17,9)
Paciente<-as.factor(Paciente)
Idade<-c(28,15,76,15,21,11,16,16,47,18,40,24,32,31,10,31,31,46,21,39,15,9,75,54,35,18)
Diag<-c(7,52,30,53,3,46,55,54,13,59,20,3,9,9,44,9,10,13,1,17,53,42,30,18,12,58)
Recup<-c(3,45,23,46,2,42,47,47,12,51,11,1,3,3,40,3,4,11,2,8,46,39,22,16,5,50)
Tratam<-c('II','I','III','I','II','I','I','I','III','II','III','II','II','II','I','II','II','III','II','III','I','I','III','III','II','II')
Tratamen<-as.factor(Tratam)
Coag<-c('nao','nao','sim','sim','nao','nao','nao','sim','sim','nao','sim','nao','nao','nao','sim','sim','sim','sim','sim','sim','sim','nao','sim','nao','sim','sim')
Coagul<-as.factor(Coag)
#--------------------------------------
# Montagem/Leitura do Data Frame
#--------------------------------------
#--------------------------------------
# a. Crie a planilha com os dados
#--------------------------------------
pacicontamin <- data.frame(Paciente=Paciente, Idade=Idade,Diagnostico=Diag,
                           Recuperacao=Recup,Tratamento=Tratam,Coagulacao=Coag)

pacicontamin
#--------------------------------------
# i. Classifique as variáveis
attach(pacicontamin) #desintegrar os dados e usar as novas variáveis do frame.
names(pacicontamin)
class(Paciente); class(Idade); class(Diagnostico); class(Recuperacao);class(Tratamento); class(Coagulacao)
#Paciente: fator
#Idade: numerico
#Diagnostico: numerico
#Recuperacao: numerico
#Tratamento: fator
#Coagulacao: fator
#--------------------------------------
# ii. Construir a tabela de freq. para Diagnostico.
# Primeiro é preciso definir a amplitude das 05 classes:
k=sqrt(length(Diagnostico)) # Numero de Classes
k # Valor referente ao numero de classes que e 5.09902.
LI = min(Diagnostico) # Limite Inferior da distribuicao
LS = max(Diagnostico) # Limite Superior da distribuicao
a = (LS-LI)/k # Amplitude das classes aproximada 11.37474
round(a) # A Amplitude das classes arredondada é 11.
# Tabela de Frequencias forma comum
li.value<-min(Diagnostico)
ls.value<-max(Diagnostico)
step.value<-k
breaks_classes<- seq(li.value,ls.value,step.value)
classes<-round(breaks_classes)
midponts<-seq(li.value+step.value/2,ls.value-step.value/2,step.value)
midpont<-round(midponts)
midpont

pontcorte<-cut(Diagnostico,breaks=breaks_classes)
TAB<-table(pontcorte)
pacicontaminVA<-data.frame(TAB,midpont)
pacicontaminVA #Note que dessa forma nao foi composto de 05 classes e desconsi-
#derou os valores superiores na ultima classe. Isto e, tres valores sumiram da 
#base devido as adequacoes das classes conforme o ultimo algoritmo.
 
# Tabela de Frequencia com o pacote "Epi" e mais facil e simples de resolver.
pacicontamin <- transform(pacicontamin,DiagnosticoAgrp=cut(Diagnostico, breaks=c(1,12,23,34,45,60),right=FALSE))
stat.table(DiagnosticoAgrp,data=pacicontamin)
#--------------------------------------
# iii. Representacao grafica adequada
IdadeInf <- min(Idade) 
IdadeSup <- max(Idade)
kIdade <- sqrt(length(Idade))
aIdade <- (IdadeSup - IdadeInf)/kIdade
aIdadeX<- round(aIdade)
pacicontamin <- transform(pacicontamin, IdadeAgrp = cut(Idade, breaks=c(8,22,35,48,61,76)))
attach(pacicontamin)
names(pacicontamin)

# Utilizando um princípio de SQL para filtragens em R
pacicontamin %>%
select(Tratamento, Idade)
#Tratamento
    filter(pacicontamin, Idade,Tratamento == 'I')
      hist(Idade,col='green')
    filter(pacicontamin,Idade,Tratamento == 'II')
      hist(Idade)
    filter(pacicontamin, Idade,Tratamento == 'III')
      hist(Idade,col='red')
    par(mfrow=c(3,1))
ggplot(data=pacicontamin, aes(Idade, label= Idade)) + 
  geom_histogram(breaks=seq(9,76, by =  13), 
                 col="red", 
                 fill="gray", 
                 alpha = .2) + 
  #geom_label(aes(Idade,vjust=1.5))  + # Verificar o label!!!
  labs(title="Histograma para Idade") +
  labs(x="Idade", y="Frequência") + 
  xlim(c(9,76)) + 
  ylim(c(0,15))

# Teste
pacicontamin %>%
select(IdadeAgrp,Tratamento, Diagnostico)
ggplot(data=pacicontamin, aes(Diagnostico, label= Diagnostico)) + 
  geom_histogram(breaks=seq(9,76, by =  13), 
                 col="red", 
                 fill="gray", 
                 alpha = .2) + 
  #geom_text(aes(label="",vjust=1.5))  + # Verificar o label!!!
  labs(title="Histograma para Diagnóstico") +
  labs(x="Idade", y="Frequência") + 
  xlim(c(1,59)) + 
  ylim(c(0,30))

#Histograma "Dados empilhados!"
ggplot(aes(displ),data =mpg) + geom_histogram(aes(fill=class),binwidth = 1,col="black")


#--------------------------------------//--------------------------------------
