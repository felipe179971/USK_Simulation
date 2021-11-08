##Carregando os pacotes##########################################################
library(USK)
#library(dplyr)
#library(plotly)
library(mice)
library(stringr)
##Testando com um exemplo##########################################################

taus=c(4,4,-4,-4,9,-9)
Tratamento<-as.factor(rep(c(paste("trat",seq(1:length(taus)))),3))
erro<-rnorm(3*length(taus),0,1)
y<-2+taus+erro
y[round(runif(1,min=1,max=length(y)),0)]<-NA
dados<-data.frame(y,Tratamento)

a<-usktest(y~Tratamento,dados,ANOVA=F)
plot_usk(a)
plotly_usk(a)

help("usktest")
###################################################################################
###################################################################################
###################################################################################
##FUNÇÕES##########################################################################

dataset<-function(taus,mu,sigma,observations,missings,groups,seed,iterations){
  #Número de tratamentos
  Ntreatment<-length(taus)
  #Vetor Erro
  set.seed(seed)
  erro<-rnorm(observations*Ntreatment,0,sigma)
  y<-mu+taus+erro
  #Vetor que será a coluna "Treatment"
  Treatment<-as.factor(rep(c(seq(1:Ntreatment)),observations))
  #Definindo quem receberá NA
  recebe_NA<-ceiling(observations*Ntreatment*(missings/100))
  set.seed(seed)
  y[sample(1:length(y),recebe_NA,replace=F)]<-NA
  #Banco de dados
  dados<-data.frame(ID=seq(1:length(y)),y,Treatment)
  #Imputação
  imputed<-mice(dados, m=20, method = 'norm',print=F)
  dados<-merge(dados,complete(imputed,)[,-3],by="ID")[,-1]
  dados$original<-mu+taus+erro
  #Identificando
  colnames(dados)<-c("Missing","Treatment","Imputed","Original")
  return(dados)
}
Verificacao<-function(x){
  #Verificando se algum tratamento ficou totalmente vazio
  return(sum(is.na(tapply(x$Missing, x$Treatment,mean,na.rm=T))))
}

simulacao<-function(iterations,alpha,taus,mu,sigma,observations,missings,groups){
  #Vetor vazio que armazenará o resultado
  resultado_O<-c()
  resultado_M<-c()
  resultado_I<-c()
  for(i in 1:iterations){
    dados<-dataset(taus,mu,sigma,observations,missings,groups,seed=i,iterations)
    #Verificando se algum tratamento ficou totalmente vazio
    Verificando<-Verificacao(dados)
    #Refazer o banco até que nenhum tratamento fique só com missing
    j<-1
    while (Verificando>0) {
      print(paste0("Trying again...[missing ",j,"]"))
      #Tentando novamente
      dados<-dataset(taus,mu,sigma,observations,missings,groups,seed=iterations+j,iterations)
      #Verificando se algum tratamento ficou totalmente vazio
      Verificando<-Verificacao(dados)
      print(Verificando)
      j<-j+1
    }
    scott_O<-as.data.frame(usktest(Original~Treatment,dados,alpha,ANOVA=F))[,c(3:8)]
    scott_M<-as.data.frame(usktest(Missing~Treatment,dados,alpha,ANOVA=F))[,c(3:8)]
    scott_I<-as.data.frame(usktest(Imputed~Treatment,dados,alpha,ANOVA=F))[,c(3:8)]

    #Exemplo, se taus=([2,2],[-2,-2]) serão 4 tratamentos em 2 groups
    #Definindo se dividiu na quantidade esperada
    divisao_O<-ifelse(length(levels(as.factor(scott_O$Group)))==groups ,paste0("groups=",groups ),"Diferente")
    divisao_M<-ifelse(length(levels(as.factor(scott_M$Group)))==groups ,paste0("groups=",groups ),"Diferente")
    divisao_I<-ifelse(length(levels(as.factor(scott_I$Group)))==groups ,paste0("groups=",groups ),"Diferente")

    resultado_O[i]<-(divisao_O)
    resultado_M[i]<-(divisao_M)
    resultado_I[i]<-(divisao_I)
    #print(dados)
    #limpando 'Console' do Rstudio
    cat("\014")
  }

  return(list(Original=c(prop.table(table(resultado_O))),Missing=c(prop.table(table(resultado_M))),Imputed=c(prop.table(table(resultado_I)))))

}
###################################################################################
###################################################################################
###################################################################################
####################################################################################

##Executando as funções#############################################################
##Simulação I#####
#Poucas Observações e Poucos Tratamentos para Verificaro Erro Tipo I
#[Erro Tipo I (% e alpha) 20 observações (4 trat e 5 obs)]

#Criando lista para armazenar os resultados
percentage<-c(0,5,10,15,20)
alphas<-c(0.01,0.05,0.10,0.15,0.20)
result<-as.list(1:length(alphas))
result2<-as.list(1:length(percentage))
#Executando
for (j in 1:length(alphas)){

  for (i in 1:length(percentage)){
    result2[[i]]<-simulacao(
      #Quantidade de casos a serem avaliados (iterações)
      iterations=1000,
      #Erro Tipo I
      alpha=alphas[j],
      #Definindo os valores dos taus e quantos tratamentos terei
      taus=c(0,0,0,0),
      #Média
      mu=20,
      #Sigma do modelo erro~N(0,sigma)
      sigma=1,
      #Número de observações (ou blocos) em cada tratamentos
      observations=5,
      #Em porcentagem ("missings=1" = 1% de y vai receber NA de forma aleatória)
      missings=percentage[i],
      #Quantidades de grupos que o teste deveria retornar
      groups=1
    )
    names(result2)[i]<-paste0("Missings= ",percentage[i],"%","; Alpha= ",alphas[j])
    #Avaliando o progresso
    print(paste0(i," (",round(i/length(percentage)*100),"%)"))
  }
  result[[j]]<-result2
  names(result)[j]<-paste0("Alpha= ",alphas[j])
}
result

#Olhando o resultado
quero<-result
##Pegando apenas o que quero (acertos) e imprimindo tabularmente
#Pegando só o que acertou
a<-matrix(0,nrow=2+length(quero),ncol=length(quero[[1]])*3+1)
a[1,1]<-c("Alpha/Missing")
a[2,1]<-c("-")
#Nomeando os alphas
for(i in 1:length(quero)){
  a[i+2,1]<-str_split(names(quero)[i],fixed(' '))[[1]][2]
}
#Nomeando as porcentagens
pega<-c()
for(i in 1:length(quero[[1]])){
  pega[i]<-str_split(str_split(names(quero[[1]])[i], fixed(';'))[[1]][1],fixed(' '))[[1]][2]
}
pega<-rep(pega,1,each = 3)
for(i in 1:length(pega)){
  a[1,i+1]<-pega[i]
}
#Nomeando o métodos
pega<-rep(c("Original","Missing","Imputed"),length(quero[[1]]))
for(i in 1:length(pega)){
  a[2,i+1]<-pega[i]
}


#Original
for (k in 1:3) {
  #porcen
  for (i in 1:length(quero[[1]])) {
    #alpha
    for (j in 1:length(quero)) {
      #coletando
      a[j+2,2+(3*(i-1))+(k-1)]<-quero[[j]][[i]][[k]][["groups=1"]]
    }
  }
}
#Imprimindo
(b<-data.frame(a))

##Simulação II#####
#Muitas Observações e Poucos Tratamentos para Verificar o Erro Tipo I
#[Erro Tipo I (% e alpha) 100 observações (4 trat e 25 obs)]
#Criando lista para armazenar os resultados
percentage<-c(0,5,10,15,20)
alphas<-c(0.01,0.05,0.10,0.15,0.20)
result<-as.list(1:length(alphas))
result2<-as.list(1:length(percentage))
#Executando
for (j in 1:length(alphas)){

  for (i in 1:length(percentage)){
    result2[[i]]<-simulacao(
      #Quantidade de casos a serem avaliados (iterações)
      iterations=1000,
      #Erro Tipo I
      alpha=alphas[j],
      #Definindo os valores dos taus e quantos tratamentos terei
      taus=c(0,0,0,0),
      #Média
      mu=20,
      #Sigma do modelo erro~N(0,sigma)
      sigma=1,
      #Número de observações (ou blocos) em cada tratamentos
      observations=25,
      #Em porcentagem ("missings=1" = 1% de y vai receber NA de forma aleatória)
      missings=percentage[i],
      #Quantidades de grupos que o teste deveria retornar
      groups=1
    )
    names(result2)[i]<-paste0("Missings= ",percentage[i],"%","; Alpha= ",alphas[j])
    #Avaliando o progresso
    print(paste0(i," (",round(i/length(percentage)*100),"%)"))
  }
  result[[j]]<-result2
  names(result)[j]<-paste0("Alpha= ",alphas[j])
}
result

#Olhando o resultado
quero<-result
##Pegando apenas o que quero (acertos) e imprimindo tabularmente
#Pegando só o que acertou
a<-matrix(0,nrow=2+length(quero),ncol=length(quero[[1]])*3+1)
a[1,1]<-c("Alpha/Missing")
a[2,1]<-c("-")
#Nomeando os alphas
for(i in 1:length(quero)){
  a[i+2,1]<-str_split(names(quero)[i],fixed(' '))[[1]][2]
}
#Nomeando as porcentagens
pega<-c()
for(i in 1:length(quero[[1]])){
  pega[i]<-str_split(str_split(names(quero[[1]])[i], fixed(';'))[[1]][1],fixed(' '))[[1]][2]
}
pega<-rep(pega,1,each = 3)
for(i in 1:length(pega)){
  a[1,i+1]<-pega[i]
}
#Nomeando o métodos
pega<-rep(c("Original","Missing","Imputed"),length(quero[[1]]))
for(i in 1:length(pega)){
  a[2,i+1]<-pega[i]
}


#Original
for (k in 1:3) {
  #porcen
  for (i in 1:length(quero[[1]])) {
    #alpha
    for (j in 1:length(quero)) {
      #coletando
      a[j+2,2+(3*(i-1))+(k-1)]<-quero[[j]][[i]][[k]][["groups=1"]]
    }
  }
}
#Imprimindo
(b<-data.frame(a))

##Simulação III#####
#Poucas Observações e Muitos Tratamentos para Veri-ficar o Erro Tipo I
#[Erro Tipo I (% e alpha) 100 obserações (10 trat e 10 obs)]
#Criando lista para armazenar os resultados
percentage<-c(0,5,10,15,20)
alphas<-c(0.01,0.05,0.10,0.15,0.20)
result<-as.list(1:length(alphas))
result2<-as.list(1:length(percentage))
#Executando
for (j in 1:length(alphas)){

  for (i in 1:length(percentage)){
    result2[[i]]<-simulacao(
      #Quantidade de casos a serem avaliados (iterações)
      iterations=1000,
      #Erro Tipo I
      alpha=alphas[j],
      #Definindo os valores dos taus e quantos tratamentos terei
      taus=c(rep(0,10)),
      #Média
      mu=20,
      #Sigma do modelo erro~N(0,sigma)
      sigma=1,
      #Número de observações (ou blocos) em cada tratamentos
      observations=10,
      #Em porcentagem ("missings=1" = 1% de y vai receber NA de forma aleatória)
      missings=percentage[i],
      #Quantidades de grupos que o teste deveria retornar
      groups=1
    )
    names(result2)[i]<-paste0("Missings= ",percentage[i],"%","; Alpha= ",alphas[j])
    #Avaliando o progresso
    print(paste0(i," (",round(i/length(percentage)*100),"%)"))
  }
  result[[j]]<-result2
  names(result)[j]<-paste0("Alpha= ",alphas[j])
}
result

#Olhando o resultado
quero<-result
##Pegando apenas o que quero (acertos) e imprimindo tabularmente
#Pegando só o que acertou
a<-matrix(0,nrow=2+length(quero),ncol=length(quero[[1]])*3+1)
a[1,1]<-c("Alpha/Missing")
a[2,1]<-c("-")
#Nomeando os alphas
for(i in 1:length(quero)){
  a[i+2,1]<-str_split(names(quero)[i],fixed(' '))[[1]][2]
}
#Nomeando as porcentagens
pega<-c()
for(i in 1:length(quero[[1]])){
  pega[i]<-str_split(str_split(names(quero[[1]])[i], fixed(';'))[[1]][1],fixed(' '))[[1]][2]
}
pega<-rep(pega,1,each = 3)
for(i in 1:length(pega)){
  a[1,i+1]<-pega[i]
}
#Nomeando o métodos
pega<-rep(c("Original","Missing","Imputed"),length(quero[[1]]))
for(i in 1:length(pega)){
  a[2,i+1]<-pega[i]
}


#Original
for (k in 1:3) {
  #porcen
  for (i in 1:length(quero[[1]])) {
    #alpha
    for (j in 1:length(quero)) {
      #coletando
      a[j+2,2+(3*(i-1))+(k-1)]<-quero[[j]][[i]][[k]][["groups=1"]]
    }
  }
}
#Imprimindo
(b<-data.frame(a))



##Simulação IV#####
#Muitas Observações e Muitos Tratamentos para Veri-ficar o Erro Tipo I
#[Erro Tipo I (% e alpha) 500 obserações (10 trat e 50 obs)]
#Criando lista para armazenar os resultados
percentage<-c(0,5,10,15,20)
alphas<-c(0.01,0.05,0.10,0.15,0.20)
result<-as.list(1:length(alphas))
result2<-as.list(1:length(percentage))
#Executando
for (j in 1:length(alphas)){

  for (i in 1:length(percentage)){
    result2[[i]]<-simulacao(
      #Quantidade de casos a serem avaliados (iterações)
      iterations=1000,
      #Erro Tipo I
      alpha=alphas[j],
      #Definindo os valores dos taus e quantos tratamentos terei
      taus=c(rep(0,10)),
      #Média
      mu=20,
      #Sigma do modelo erro~N(0,sigma)
      sigma=1,
      #Número de observações (ou blocos) em cada tratamentos
      observations=50,
      #Em porcentagem ("missings=1" = 1% de y vai receber NA de forma aleatória)
      missings=percentage[i],
      #Quantidades de grupos que o teste deveria retornar
      groups=1
    )
    names(result2)[i]<-paste0("Missings= ",percentage[i],"%","; Alpha= ",alphas[j])
    #Avaliando o progresso
    print(paste0(i," (",round(i/length(percentage)*100),"%)"))
  }
  result[[j]]<-result2
  names(result)[j]<-paste0("Alpha= ",alphas[j])
}
result

#Olhando o resultado
quero<-result
##Pegando apenas o que quero (acertos) e imprimindo tabularmente
#Pegando só o que acertou
a<-matrix(0,nrow=2+length(quero),ncol=length(quero[[1]])*3+1)
a[1,1]<-c("Alpha/Missing")
a[2,1]<-c("-")
#Nomeando os alphas
for(i in 1:length(quero)){
  a[i+2,1]<-str_split(names(quero)[i],fixed(' '))[[1]][2]
}
#Nomeando as porcentagens
pega<-c()
for(i in 1:length(quero[[1]])){
  pega[i]<-str_split(str_split(names(quero[[1]])[i], fixed(';'))[[1]][1],fixed(' '))[[1]][2]
}
pega<-rep(pega,1,each = 3)
for(i in 1:length(pega)){
  a[1,i+1]<-pega[i]
}
#Nomeando o métodos
pega<-rep(c("Original","Missing","Imputed"),length(quero[[1]]))
for(i in 1:length(pega)){
  a[2,i+1]<-pega[i]
}


#Original
for (k in 1:3) {
  #porcen
  for (i in 1:length(quero[[1]])) {
    #alpha
    for (j in 1:length(quero)) {
      #coletando
      a[j+2,2+(3*(i-1))+(k-1)]<-quero[[j]][[i]][[k]][["groups=1"]]
    }
  }
}
#Imprimindo
(b<-data.frame(a))






##Simulação V#####
#Poucas Observações e Poucos Tratamentos para Verificar o Poder do Teste
#[Para o Poder do Teste (% e sigma) 20 observações (4 trat e 5 obs)]
#Criando lista para armazenar os resultados
percentage<-c(0,5,10,15,20)
sigmas<-c(1,2,3,4,5)
result<-as.list(1:length(sigmas))
result2<-as.list(1:length(percentage))
#Executando
for (j in 1:length(sigmas)){

  for (i in 1:length(percentage)){
    result2[[i]]<-simulacao(
      #Quantidade de casos a serem avaliados (iterações)
      iterations=1000,
      #Erro Tipo I
      alpha=0.05,
      #Definindo os valores dos taus e quantos tratamentos terei
      taus=c(10,5,-5,-10),
      #Média
      mu=20,
      #Sigma do modelo erro~N(0,sigma)
      sigma=sigmas[j],
      #Número de observações (ou blocos) em cada tratamentos
      observations=5,
      #Em porcentagem ("missings=1" = 1% de y vai receber NA de forma aleatória)
      missings=percentage[i],
      #Quantidades de grupos que o teste deveria retornar
      groups=4
    )
    names(result2)[i]<-paste0("Missings= ",percentage[i],"%","; Sigma= ",sigmas[j])
    #Avaliando o progresso
    print(paste0(i," (",round(i/length(percentage)*100),"%)"))
  }
  result[[j]]<-result2
  names(result)[j]<-paste0("Sigma= ",sigmas[j])
}
result

#Olhando o resultado
quero<-result

##Pegando apenas o que quero (acertos) e imprimindo tabularmente
a<-matrix(0,nrow=2+length(quero),ncol=length(quero[[1]])*3+1)
a[1,1]<-c("Sigma/Missing")
a[2,1]<-c("-")
#Nomeando os Sigmas
for(i in 1:length(quero)){
  a[i+2,1]<-str_split(names(quero)[i],fixed(' '))[[1]][2]
}
#Nomeando as porcentagens
pega<-c()
for(i in 1:length(quero[[1]])){
  pega[i]<-str_split(str_split(names(quero[[1]])[i], fixed(';'))[[1]][1],fixed(' '))[[1]][2]
}
pega<-rep(pega,1,each = 3)
for(i in 1:length(pega)){
  a[1,i+1]<-pega[i]
}
#Nomeando o métodos
pega<-rep(c("Original","Missing","Imputed"),length(quero[[1]]))
for(i in 1:length(pega)){
  a[2,i+1]<-pega[i]
}


#Original
for (k in 1:3) {
  #porcen
  for (i in 1:length(quero[[1]])) {
    #sigma
    for (j in 1:length(quero)) {
      #coletando
      a[j+2,2+(3*(i-1))+(k-1)]<-quero[[j]][[i]][[k]][["groups=4"]]
    }
  }
}
#Imprimindo
(b<-data.frame(a))






##Simulação VI#####
#Muitas Observações e Poucos Tratamentos para Verificar o Poder do Teste
#[Para o Poder do Teste (% e sigma) 100 observações (4 trat e 25 obs)]
#Criando lista para armazenar os resultados
percentage<-c(0,5,10,15,20)
sigmas<-c(1,2,3,4,5)
result<-as.list(1:length(sigmas))
result2<-as.list(1:length(percentage))
#Executando
for (j in 1:length(sigmas)){

  for (i in 1:length(percentage)){
    result2[[i]]<-simulacao(
      #Quantidade de casos a serem avaliados (iterações)
      iterations=1000,
      #Erro Tipo I
      alpha=0.05,
      #Definindo os valores dos taus e quantos tratamentos terei
      taus=c(10,5,-5,-10),
      #Média
      mu=20,
      #Sigma do modelo erro~N(0,sigma)
      sigma=sigmas[j],
      #Número de observações (ou blocos) em cada tratamentos
      observations=25,
      #Em porcentagem ("missings=1" = 1% de y vai receber NA de forma aleatória)
      missings=percentage[i],
      #Quantidades de grupos que o teste deveria retornar
      groups=4
    )
    names(result2)[i]<-paste0("Missings= ",percentage[i],"%","; Sigma= ",sigmas[j])
    #Avaliando o progresso
    print(paste0(i," (",round(i/length(percentage)*100),"%)"))
  }
  result[[j]]<-result2
  names(result)[j]<-paste0("Sigma= ",sigmas[j])
}
result

#Olhando o resultado
quero<-result

##Pegando apenas o que quero (acertos) e imprimindo tabularmente
a<-matrix(0,nrow=2+length(quero),ncol=length(quero[[1]])*3+1)
a[1,1]<-c("Sigma/Missing")
a[2,1]<-c("-")
#Nomeando os Sigmas
for(i in 1:length(quero)){
  a[i+2,1]<-str_split(names(quero)[i],fixed(' '))[[1]][2]
}
#Nomeando as porcentagens
pega<-c()
for(i in 1:length(quero[[1]])){
  pega[i]<-str_split(str_split(names(quero[[1]])[i], fixed(';'))[[1]][1],fixed(' '))[[1]][2]
}
pega<-rep(pega,1,each = 3)
for(i in 1:length(pega)){
  a[1,i+1]<-pega[i]
}
#Nomeando o métodos
pega<-rep(c("Original","Missing","Imputed"),length(quero[[1]]))
for(i in 1:length(pega)){
  a[2,i+1]<-pega[i]
}


#Original
for (k in 1:3) {
  #porcen
  for (i in 1:length(quero[[1]])) {
    #sigma
    for (j in 1:length(quero)) {
      #coletando
      a[j+2,2+(3*(i-1))+(k-1)]<-quero[[j]][[i]][[k]][["groups=4"]]
    }
  }
}
#Imprimindo
(b<-data.frame(a))







##Simulação VII#####
#Poucas Observações e Muitos Tratamentos para Verificar o Poder do Teste
#[Para o Poder do Teste (% e sigma) 100 observações (10 trat e 10 obs)]
#Criando lista para armazenar os resultados
percentage<-c(0,5,10,15,20)
sigmas<-c(1,2,3,4,5)
result<-as.list(1:length(sigmas))
result2<-as.list(1:length(percentage))
#Executando
for (j in 1:length(sigmas)){

  for (i in 1:length(percentage)){
    result2[[i]]<-simulacao(
      #Quantidade de casos a serem avaliados (iterações)
      iterations=1000,
      #Erro Tipo I
      alpha=0.05,
      #Definindo os valores dos taus e quantos tratamentos terei
      taus=c(25,20,15,10,5,-5,-10,-15,-20,-25),
      #Média
      mu=20,
      #Sigma do modelo erro~N(0,sigma)
      sigma=sigmas[j],
      #Número de observações (ou blocos) em cada tratamentos
      observations=10,
      #Em porcentagem ("missings=1" = 1% de y vai receber NA de forma aleatória)
      missings=percentage[i],
      #Quantidades de grupos que o teste deveria retornar
      groups=10
    )
    names(result2)[i]<-paste0("Missings= ",percentage[i],"%","; Sigma= ",sigmas[j])
    #Avaliando o progresso
    print(paste0(i," (",round(i/length(percentage)*100),"%)"))
  }
  result[[j]]<-result2
  names(result)[j]<-paste0("Sigma= ",sigmas[j])
}
result

#Olhando o resultado
quero<-result

##Pegando apenas o que quero (acertos) e imprimindo tabularmente
a<-matrix(0,nrow=2+length(quero),ncol=length(quero[[1]])*3+1)
a[1,1]<-c("Sigma/Missing")
a[2,1]<-c("-")
#Nomeando os Sigmas
for(i in 1:length(quero)){
  a[i+2,1]<-str_split(names(quero)[i],fixed(' '))[[1]][2]
}
#Nomeando as porcentagens
pega<-c()
for(i in 1:length(quero[[1]])){
  pega[i]<-str_split(str_split(names(quero[[1]])[i], fixed(';'))[[1]][1],fixed(' '))[[1]][2]
}
pega<-rep(pega,1,each = 3)
for(i in 1:length(pega)){
  a[1,i+1]<-pega[i]
}
#Nomeando o métodos
pega<-rep(c("Original","Missing","Imputed"),length(quero[[1]]))
for(i in 1:length(pega)){
  a[2,i+1]<-pega[i]
}


#Original
for (k in 1:3) {
  #porcen
  for (i in 1:length(quero[[1]])) {
    #sigma
    for (j in 1:length(quero)) {
      #coletando
      a[j+2,2+(3*(i-1))+(k-1)]<-quero[[j]][[i]][[k]][["groups=10"]]
    }
  }
}
#Imprimindo
(b<-data.frame(a))


##Simulação VIII#####
#Muitas Observações e Muitos Tratamentos para Verificar o Poder do Teste
#[Para o Poder do Teste (% e sigma) 500 observações (10 trat e 50 obs)]
#Criando lista para armazenar os resultados
percentage<-c(0,5,10,15,20)
sigmas<-c(1,2,3,4,5)
result<-as.list(1:length(sigmas))
result2<-as.list(1:length(percentage))
#Executando
for (j in 1:length(sigmas)){

  for (i in 1:length(percentage)){
    result2[[i]]<-simulacao(
      #Quantidade de casos a serem avaliados (iterações)
      iterations=1000,
      #Erro Tipo I
      alpha=0.05,
      #Definindo os valores dos taus e quantos tratamentos terei
      taus=c(25,20,15,10,5,-5,-10,-15,-20,-25),
      #Média
      mu=20,
      #Sigma do modelo erro~N(0,sigma)
      sigma=sigmas[j],
      #Número de observações (ou blocos) em cada tratamentos
      observations=50,
      #Em porcentagem ("missings=1" = 1% de y vai receber NA de forma aleatória)
      missings=percentage[i],
      #Quantidades de grupos que o teste deveria retornar
      groups=10
    )
    names(result2)[i]<-paste0("Missings= ",percentage[i],"%","; Sigma= ",sigmas[j])
    #Avaliando o progresso
    print(paste0(i," (",round(i/length(percentage)*100),"%)"))
  }
  result[[j]]<-result2
  names(result)[j]<-paste0("Sigma= ",sigmas[j])
}
result

#Olhando o resultado
quero<-result

##Pegando apenas o que quero (acertos) e imprimindo tabularmente
a<-matrix(0,nrow=2+length(quero),ncol=length(quero[[1]])*3+1)
a[1,1]<-c("Sigma/Missing")
a[2,1]<-c("-")
#Nomeando os Sigmas
for(i in 1:length(quero)){
  a[i+2,1]<-str_split(names(quero)[i],fixed(' '))[[1]][2]
}
#Nomeando as porcentagens
pega<-c()
for(i in 1:length(quero[[1]])){
  pega[i]<-str_split(str_split(names(quero[[1]])[i], fixed(';'))[[1]][1],fixed(' '))[[1]][2]
}
pega<-rep(pega,1,each = 3)
for(i in 1:length(pega)){
  a[1,i+1]<-pega[i]
}
#Nomeando o métodos
pega<-rep(c("Original","Missing","Imputed"),length(quero[[1]]))
for(i in 1:length(pega)){
  a[2,i+1]<-pega[i]
}


#Original
for (k in 1:3) {
  #porcen
  for (i in 1:length(quero[[1]])) {
    #sigma
    for (j in 1:length(quero)) {
      #coletando
      a[j+2,2+(3*(i-1))+(k-1)]<-quero[[j]][[i]][[k]][["groups=10"]]
    }
  }
}
#Imprimindo
(b<-data.frame(a))

######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
######################################################
###Banco de Dados e Porcentagem de Dados Faltantes:#####
porcentagem<-c(5,30)
for(i in 1:length(porcentagem)){
  x<-dataset(taus=c(-10,10),
             mu=20,
             sigma=5,
             observations=4,
             missings=porcentagem[i],
             groups=2,
             seed=20)
  print(data.frame(Treatment=x$Treatment,Original=x$Original,Missing=x$Missing,Imputed=x$Imputed))
}
###Gráficos e Alpha:#######
x<-dataset(taus=c(1,1,-1,-1),
           mu=1,
           sigma=.5,
           observations=4,
           missings=5,
           groups=2,
           seed=20)
data.frame(`Observação`=c(seq(1:4)),
           Tratamento_1=data.frame(x[,c(1:2)]%>%filter(Treatment==1))[,1],
           Tratamento_2=data.frame(x[,c(1:2)]%>%filter(Treatment==2))[,1],
           Tratamento_3=data.frame(x[,c(1:2)]%>%filter(Treatment==3))[,1],
           Tratamento_4=data.frame(x[,c(1:2)]%>%filter(Treatment==4))[,1])

plotly_usk(usktest(Missing~Treatment,x,0,ANOVA=F))
plot_usk(usktest(Missing~Treatment,x,0,ANOVA=F))

plotly_usk(usktest(Missing~Treatment,x,1,ANOVA=F))
plot_usk(usktest(Missing~Treatment,x,1,ANOVA=F))

plotly_usk(usktest(Missing~Treatment,x,.05,ANOVA=F))
plot_usk(usktest(Missing~Treatment,x,.05,ANOVA=F))
###ANOVA e saída no Console:#######
Com_Anova<-usktest(formula=Missing~Treatment,dataset=x,alpha=.05,ANOVA=T)
Sem_Anova<-usktest(formula=Missing~Treatment,dataset=x,alpha=.05,ANOVA=F)
###Mensagens de Erro###########
x$Treatment_Character<-as.character(x$Treatment)
x$Missing_factor<-as.factor(x$Missing)
x$y_1Missing_NA<-x$Missing;x$y_1Missing_NA[x$Treatment==1]<-NA
x$Treatment_unico<-as.factor(c("1"))

usktest(Missing~Treatment+Imputed,x,.05,ANOVA=F)
usktest(Missing_factor~Treatment_Character,x,.05,ANOVA=F)
usktest(y_1Missing_NA~Treatment,x,.05,ANOVA=F)
usktest(Missing~Treatment_unico,x,.05,ANOVA=F)
###Muitos tratamentos###########
set.seed(17)
taus=c(seq(from=2, to=400, by=4))
Tratamento<-as.factor(rep(c(paste("trat",seq(1:length(taus)))),3))
erro<-rnorm(3*length(taus),0,.5)
y<-2+taus+erro
y[round(runif(1,min=1,max=length(y)),0)]<-NA
dados<-data.frame(y,Tratamento)

plot_usk(usktest(y~Tratamento,dados,ANOVA=T))
plotly_usk(usktest(y~Tratamento,dados,ANOVA=F))

#####Verificando Iterações######################################
verificando_iteracoes<-function(proporcao,n,d){
  acertos=proporcao*n
  amostra<-c(rep(1,acertos),rep(0,(n-acertos)))
  media<-mean(amostra)
  s<-sd(amostra)
  estatistica<-s/sqrt(n)
  resultado<-c(estatistica<d)
  return(list(Acertos=c(acertos),Valores=c(media,estatistica),resultado=c(resultado)))

}
verificando_iteracoes(0.9,1000,0.0255102)

###12 Bancos de dados para o Erro Tipo I######################################
for(i in 1:12){
  x<-dataset(taus=c(0,0,0,0),
             mu=20,
             sigma=1,
             observations=25,
             missings=20,
             groups=1,
             seed=i )#round(runif(1, min = 0, max = 100),0))
  print(plotly_usk(usktest(Missing~Treatment,x,0.20,ANOVA=F)))
}
###12 Bancos de dados para o Poder do Teste (I)######################################
for(i in 1:12){
  x<-dataset(taus=c(10,5,-5,-10),
             mu=20,
             sigma=5,
             observations=25,
             missings=20,
             groups=4,
             seed=i )#round(runif(1, min = 0, max = 100),0))
  print(plotly_usk(usktest(Missing~Treatment,x,0.05,ANOVA=F)))
}

###12 Bancos de dados para o Poder do Teste (II)######################################
for(i in 1:12){
  x<-dataset(taus=c(25,20,15,10,5,-5,-10,-15,-20,-25),
             mu=20,
             sigma=5,
             observations=10,
             missings=20,
             groups=10,
             seed=i )#round(runif(1, min = 0, max = 100),0))
  print(plot_usk(usktest(Missing~Treatment,x,0.05,ANOVA=F)))
}
###Verificando o impacto do sigma nos taus###############################################
x<-dataset(taus=c(1,1,-1,-1),
           mu=1,
           sigma=.5,
           observations=4,
           missings=5,
           groups=2,
           seed=20)
plotly_usk(usktest(Missing~Treatment,x,0.05,ANOVA=T))
x<-dataset(taus=c(1,1,-1,-1),
           mu=1,
           sigma=3,
           observations=1000,
           missings=5,
           groups=2,
           seed=20)
plotly_usk(usktest(Missing~Treatment,x,0.05,ANOVA=T))
