##Instituição: Universidade Federal do Rio de Janeiro
##Curso: Mestrado em Estadística
##Autor: Juan Pablo Argote Osorio
##Orientador: Carlos Tadeu Pagani Zanini
##Titulo da dissertação: Alocação Latente de Dirichlet para Modelagem de Tópicos em Dissertações de Mestrado em Estatística e Áreas Correlatas no Brasil
##Local e data: 15 de abril de 2025

#Conversao de pdf a txt, e criacao do Corpus:

#limpando a memoria
#rm(list=ls()) 

#carregando o pacote tm
require(tm)
require(slam)
require(wordcloud2)
require(textstem)
require(pdftools)
require(tidyverse)

#Trocando directorio

setwd("C:/...") #Diretório onde vai ficar este script

#Removendo caracteres especiais
removeMarcador <- function(texto,marcador){
  texto <- gsub(pattern=marcador,replacement=" ",x=texto)
  return(PlainTextDocument(texto))
}

#####BASE DE DADOS DE RESUMOS EM INGLÊS:#####

##Criando funcao:

#Criando objeto dos diretorios:

pastas=list.dirs("Base de Dados/")
pastas

pastas=grep(pattern = "BD1",pastas,value = TRUE)
pastas

BD_Final=0
for(i in 1:length(pastas)){

#Lendo os arquivos do diretorio para criar o Corpus
arquivos <- DirSource(directory = pastas[i],encoding ="UTF-8")
arquivos

#Carregando os arquivos e criando um Corpus

#ler os arquivos em pdf
corpus <- VCorpus(arquivos, 
                  readerControl = list(reader = readPDF))

#Propriedades do corpus
length(corpus)

#show(corpus)

#inspect(corpus)

#Alterar tudo para minusculo
corpus <- tm_map(corpus, content_transformer(tolower))
#corpus[[1]]$content[1]

#Removendo stopwords

#usando o padrao
corpus <- tm_map(corpus,removeWords,stopwords("english"))
#corpus[[1]]$content[1]

#lematizando

corpus=lemmatize_words(corpus)

#Removendo um caracter em especial
corpus <- tm_map(corpus,content_transformer(removeMarcador),"-")
corpus <- tm_map(corpus,content_transformer(removeMarcador),"[0-9]+")
corpus <- tm_map(corpus,content_transformer(removeMarcador),"abstract")
corpus <- tm_map(corpus,content_transformer(removeMarcador),"[‘’“”]")
#corpus <- tm_map(corpus,content_transformer(removeMarcador),"<.>")
corpus <- tm_map(corpus,content_transformer(removeMarcador),"<[^>]*>")
#corpus <- tm_map(corpus,content_transformer(removeMarcador),"rio")
#corpus <- tm_map(corpus,content_transformer(removeMarcador),"de")
#corpus <- tm_map(corpus,content_transformer(removeMarcador),"janeiro")

#Removendo pontuacao
corpus<- tm_map(corpus,removePunctuation)
#corpus[[1]]$content[1]

for(j in 1:length(corpus)){

#Stem

corpus[[j]]$content=stem_strings(corpus[[j]]$content)

#Removendo espacos em brancos que podem aparecer
corpus <- tm_map(corpus,stripWhitespace)
#corpus[[1]]$content[1]

#Corpus preparado

#Matriz de frequencia
#matriz_termos <- TermDocumentMatrix(corpus)
matriz_termos <- TermDocumentMatrix(corpus[[j]])
#matriz_termos$dimnames

matriz_termos=as.matrix(matriz_termos)

matriz_termos_df=data.frame(rownames(matriz_termos),as.numeric(matriz_termos))
colnames(matriz_termos_df)=c("Termos","Freq")
matriz_termos_df=aggregate(Freq~Termos,matriz_termos_df, sum)
matriz_termos_df=matriz_termos_df[order(matriz_termos_df$Termos),]
matriz_termos_df=cbind(matriz_termos_df,j,i)
colnames(matriz_termos_df)=c("Termos","Freq","Documento","IES")

#matriz_termos_df=aggregate(Freq ~ Termos,matriz_termos_df, sum)

if(j>1 | i>1){
BD_Final=rbind(BD_Final,matriz_termos_df)
}
else{
BD_Final=matriz_termos_df
}
#print(c(i,j))
}

}

#Sys.sleep(20)

BD_Final

####Unificacao de termos com as suas frequencias:

BD_Final_uni=aggregate(Freq ~ Termos,BD_Final, sum)

##Fazendo a ordenacao:

BD_Final_uni=BD_Final_uni[order(BD_Final_uni$Termos),]

##Eliminacao manual de palavras com caracteres especiais (<.>):
#Revisao do arquivo no Excel
#Linhas as quais vao ser eliminadas (desde o Bloc de Notas): 2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,85,86,87,602,792,1027,1028,1547,1548,1549,1654,1655,1727,1728,1729,1765,2033,2694,3599,3601,3771,4391,4407,4959,5115,5156,5995

BD_Final_uni=BD_Final_uni[-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,20,21,22,23,24,
25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,
51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,
77,78,79,80,81,82,85,86,87,602,792,1027,1028,1547,1548,1549,1654,1655,1727,
1728,1729,1765,2033,2694,3599,3601,3771,4391,4407,4959,5115,5156,5995),]

##Codificando as palavras:

BD_Final_uni=cbind(BD_Final_uni,seq(1,dim(BD_Final_uni)[1],1))
colnames(BD_Final_uni)=c("Termos","Freq","Código")

####Base de dados com a coluna dos códigos:

#BD_cod=cbind(BD_Final,BD_Final[,1])
#colnames(BD_cod)=c("Código","Freq","Documento","IES","Termos")

#df1=data.frame(BD_cod[,5])
#colnames(df1)="Termos"
#df2=BD_Final_uni[,-2]

#z=left_join(BD_cod, distinct(df2), by = "Termos")

BD_cod=left_join(BD_Final,distinct(BD_Final_uni[,-2]),by="Termos")

#BD1=cbind(BD_cod[,5],paste0(BD_cod[,3],"-",BD_cod[,4]),BD_cod[,2])
#colnames(BD1)=c("word","doc","freq")

##Base de dados No. 1, com o documento codificado dentro das IES:

doc_ies=cbind(BD_cod$Documento,BD_cod$IES)
doc_ies=unique(doc_ies)
doc_ies=cbind(doc_ies,seq(1,nrow(doc_ies),1))
colnames(doc_ies)=c("Documento","IES","Código")

BD1_1=left_join(BD_cod,distinct(data.frame(doc_ies)),by=c("Documento"="Documento","IES"="IES"))
BD1_1=cbind(BD1_1[,5],BD1_1[,6],BD1_1[,2])
colnames(BD1_1)=c("word","doc","freq")

##Base de dados No. 2, com a coluna dos documentos e das IES:

BD1_2=cbind(BD_cod[,5],BD_cod[,3],BD_cod[,2],BD_cod[,4])
colnames(BD1_2)=c("word","doc","freq","IES")



####################################################################
####Exportar ao Excel:

#xlsx_path = 'BD_Final.xlsx'
csv_path  = 'BD_Final.csv'
csv_path  = 'BD_Final_uni.csv'

# import into Excel
#library(openxlsx)
#write.xlsx(BD_Final, xlsx_path)

# import into the csv
write.csv(BD_Final, csv_path, row.names = FALSE)
write.csv(BD_Final_uni, csv_path, row.names = FALSE)

##Para olhar o nome do arquivo:
#rownames(summary(corpus))[]

####Gerando os arquivos à ser compartilhados no Google Drive:

csv_path  = 'BD1_1.csv'
write.csv(BD1_1, csv_path, row.names = FALSE)

csv_path  = 'BD1_2.csv'
write.csv(BD1_2, csv_path, row.names = FALSE)

csv_path  = 'BD_Final_uni.csv'
write.csv(BD_Final_uni, csv_path, row.names = FALSE)

csv_path  = 'doc_ies.csv'
write.csv(doc_ies, csv_path, row.names = FALSE)

##############################
#### MCMC ####################
##############################

#rm(list=ls(all=TRUE))
require(gtools)
require(Rcpp)
require(RcppArmadillo)

data_mat=data.frame(BD1_1)
str(data_mat)   #Amostra a estrutura do objeto no R

data = data_mat[order(data_mat[,2], decreasing=FALSE), ]

source("./Codes/utils.R")

sourceCpp("./Codes/mcmc_function.cpp")

source("./Codes/mcmc_function.R")

w = transform(data)

# for k = 10 is done
# for k = 15 is in done
# for k = 20 is in fault

# comparar tempo computacional com o pacote lda

start = Sys.time()
mcmc_cpp( data = as.matrix(data), w , K = 20, n_iter = 10, save_it = 5 )
end = Sys.time()
end - start

#save(chain, file = "./CÓDIGOS EN R/neurips/mcmc_chain_10.Rdata")

###########################################################################
###########################################################################

########Escolha de 10 tópicos:
####Cadeias para 10000 iteracoes:

start = Sys.time()
chain_10000_10=mcmc_cpp( data = as.matrix(data), w , K = 10,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_10,file="chain_10000_10.RData")

########Escolha de 20 tópicos:
####Cadeias para 10000 iteracoes:

start = Sys.time()
chain_10000_20=mcmc_cpp( data = as.matrix(data), w , K = 20,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_20,file="chain_10000_20.RData")

########Escolha de 30 tópicos:
####Cadeias para 10000 iteracoes:

start = Sys.time()
chain_10000_30=mcmc_cpp( data = as.matrix(data), w , K = 30,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_30,file="chain_10000_30.RData")

########Escolha de 40 tópicos:
####Cadeias para 10000 iteracoes:

start = Sys.time()
chain_10000_40=mcmc_cpp( data = as.matrix(data), w , K = 40,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_40,file="chain_10000_40.RData")

########Escolha de 50 tópicos:
####Cadeias para 10000 iteracoes:

start = Sys.time()
chain_10000_50=mcmc_cpp( data = as.matrix(data), w , K = 50,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_50,file="chain_10000_50.RData")

#####Cálculo do AIC (Akaike Information Criterion):

load("chain_10000_10.RData")
aic_10t=AIC_LDA(data_mat=data,chain=chain_10000_10,mcmc_ind=1:dim(chain_10000_10[[1]])[3])
save(aic_10t,file="aic_10t.RData")

load("chain_10000_20.RData")
aic_20t=AIC_LDA(data_mat=data,chain=chain_10000_20,mcmc_ind=1:dim(chain_10000_20[[1]])[3])
save(aic_20t,file="aic_20t.RData")

load("chain_10000_30.RData")
aic_30t=AIC_LDA(data_mat=data,chain=chain_10000_30,mcmc_ind=1:dim(chain_10000_30[[1]])[3])
save(aic_30t,file="aic_30t.RData")

load("chain_10000_40.RData")
aic_40t=AIC_LDA(data_mat=data,chain=chain_10000_40,mcmc_ind=1:dim(chain_10000_40[[1]])[3])
save(aic_40t,file="aic_40t.RData")

load("chain_10000_50.RData")
aic_50t=AIC_LDA(data_mat=data,chain=chain_10000_50,mcmc_ind=1:dim(chain_10000_50[[1]])[3])
save(aic_50t,file="aic_50t.RData")

###Carregando os valores dos AIC:

load("aic_10t.RData")
load("aic_20t.RData")
load("aic_30t.RData")
load("aic_40t.RData")
load("aic_50t.RData")

###Tabela dos AIC:

tabela_aic=rbind(aic_10t[[1]],aic_20t[[1]],aic_30t[[1]],aic_40t[[1]],aic_50t[[1]])
colnames(tabela_aic)="Valor do AIC"
rownames(tabela_aic)=c("Modelo com 10 tópicos","Modelo com 20 tópicos",
"Modelo com 30 tópicos","Modelo com 40 tópicos","Modelo com 50 tópicos")

tabela_aic #O melhor modelo é o que tem 10 tópicos

#####Cálculo do BIC (Bayesian Information Criterion):

load("chain_10000_10.RData")
bic_10t=BIC_LDA(data_mat=data,chain=chain_10000_10,mcmc_ind=1:dim(chain_10000_10[[1]])[3])
save(bic_10t,file="bic_10t.RData")

load("chain_10000_20.RData")
bic_20t=BIC_LDA(data_mat=data,chain=chain_10000_20,mcmc_ind=1:dim(chain_10000_20[[1]])[3])
save(bic_20t,file="bic_20t.RData")

load("chain_10000_30.RData")
bic_30t=BIC_LDA(data_mat=data,chain=chain_10000_30,mcmc_ind=1:dim(chain_10000_30[[1]])[3])
save(bic_30t,file="bic_30t.RData")

load("chain_10000_40.RData")
bic_40t=BIC_LDA(data_mat=data,chain=chain_10000_40,mcmc_ind=1:dim(chain_10000_40[[1]])[3])
save(bic_40t,file="bic_40t.RData")

load("chain_10000_50.RData")
bic_50t=BIC_LDA(data_mat=data,chain=chain_10000_50,mcmc_ind=1:dim(chain_10000_50[[1]])[3])
save(bic_50t,file="bic_50t.RData")

###Carregando os valores dos BIC:

load("bic_10t.RData")
load("bic_20t.RData")
load("bic_30t.RData")
load("bic_40t.RData")
load("bic_50t.RData")

###Tabela dos BIC:

tabela_bic=rbind(bic_10t[[1]],bic_20t[[1]],bic_30t[[1]],bic_40t[[1]],bic_50t[[1]])
colnames(tabela_bic)="Valor do BIC"
rownames(tabela_bic)=c("Modelo com 10 tópicos","Modelo com 20 tópicos",
"Modelo com 30 tópicos","Modelo com 40 tópicos","Modelo com 50 tópicos")

tabela_bic #O melhor modelo é o que tem 10 tópicos

#####Cálculo do DIC (Deviance Information Criterion):

load("chain_10000_10.RData")
dic_10t=DIC_LDA(data_mat=data,chain=chain_10000_10,mcmc_ind=1:dim(chain_10000_10[[1]])[3])
save(dic_10t,file="dic_10t.RData")

load("chain_10000_20.RData")
dic_20t=DIC_LDA(data_mat=data,chain=chain_10000_20,mcmc_ind=1:dim(chain_10000_20[[1]])[3])
save(dic_20t,file="dic_20t.RData")

load("chain_10000_30.RData")
dic_30t=DIC_LDA(data_mat=data,chain=chain_10000_30,mcmc_ind=1:dim(chain_10000_30[[1]])[3])
save(dic_30t,file="dic_30t.RData")

load("chain_10000_40.RData")
dic_40t=DIC_LDA(data_mat=data,chain=chain_10000_40,mcmc_ind=1:dim(chain_10000_40[[1]])[3])
save(dic_40t,file="dic_40t.RData")

load("chain_10000_50.RData")
dic_50t=DIC_LDA(data_mat=data,chain=chain_10000_50,mcmc_ind=1:dim(chain_10000_50[[1]])[3])
save(dic_50t,file="dic_50t.RData")

###Carregando os valores dos DIC:

load("dic_10t.RData")
load("dic_20t.RData")
load("dic_30t.RData")
load("dic_40t.RData")
load("dic_50t.RData")

###Tabela dos DIC:

tabela_dic=rbind(dic_10t,dic_20t,dic_30t,dic_40t,dic_50t)
colnames(tabela_dic)="Valor do DIC"
rownames(tabela_dic)=c("Modelo com 10 tópicos","Modelo com 20 tópicos",
"Modelo com 30 tópicos","Modelo com 40 tópicos","Modelo com 50 tópicos")

tabela_dic #O melhor modelo é o que tem 50 tópicos

##Valor do DIC, para o modelo com 100 tópicos (cálculo feito pelo professor),
##valor igual à 765013.6:

tabela_dic1=rbind(tabela_dic,765013.6)

rownames(tabela_dic1)[6]="Modelo com 100 tópicos"
tabela_dic1

###Watanabe–Akaike Information Criterion (WAIC):

require(LaplacesDemon)

load("chain_10000_10.RData")
waic_10t=WAIC_LDA(data_mat=data,chain=chain_10000_10,mcmc_ind=1:dim(chain_10000_10[[1]])[3])
save(waic_10t,file="waic_10t.RData")

load("chain_10000_20.RData")
waic_20t=WAIC_LDA(data_mat=data,chain=chain_10000_20,mcmc_ind=1:dim(chain_10000_20[[1]])[3])
save(waic_20t,file="waic_20t.RData")

load("chain_10000_30.RData")
waic_30t=WAIC_LDA(data_mat=data,chain=chain_10000_30,mcmc_ind=1:dim(chain_10000_30[[1]])[3])
save(waic_30t,file="waic_30t.RData")

load("chain_10000_40.RData")
waic_40t=WAIC_LDA(data_mat=data,chain=chain_10000_40,mcmc_ind=1:dim(chain_10000_40[[1]])[3])
save(waic_40t,file="waic_40t.RData")

load("chain_10000_50.RData")
waic_50t=WAIC_LDA(data_mat=data,chain=chain_10000_50,mcmc_ind=1:dim(chain_10000_50[[1]])[3])
save(waic_50t,file="waic_50t.RData")

###Carregando os valores dos WAIC:

load("waic_10t.RData")
load("waic_20t.RData")
load("waic_30t.RData")
load("waic_40t.RData")
load("waic_50t.RData")

###Tabela dos WAIC:

tabela_waic=rbind(waic_10t$WAIC,waic_20t$WAIC,waic_30t$WAIC,waic_40t$WAIC,
waic_50t$WAIC)
colnames(tabela_waic)="Valor do WAIC"
rownames(tabela_waic)=c("Modelo com 10 tópicos","Modelo com 20 tópicos",
"Modelo com 30 tópicos","Modelo com 40 tópicos","Modelo com 50 tópicos")

tabela_waic #O melhor modelo é o que tem 50 tópicos

###Tabela com todos os criterios de informacao:

tabela_crit=cbind(tabela_aic,tabela_bic,tabela_dic,tabela_waic)
tabela_crit

#csv_path  = 'Tabela_criterios.csv'
#write.csv(tabela_crit, csv_path, row.names = TRUE)

##########################################################################
####Cálculo de MCMC para modelos com 2-9, 11-15 tópicos, e o cálculo dos
####seus criterios de informacao; depois de olhar que o melhor modelo está
####perto do modelo com 10 tópicos:

start = Sys.time()
chain_10000_2=mcmc_cpp( data = as.matrix(data), w , K = 2,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_2,file="chain_10000_2.RData")

start = Sys.time()
chain_10000_3=mcmc_cpp( data = as.matrix(data), w , K = 3,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_3,file="chain_10000_3.RData")

start = Sys.time()
chain_10000_4=mcmc_cpp( data = as.matrix(data), w , K = 4,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_4,file="chain_10000_4.RData")

start = Sys.time()
chain_10000_5=mcmc_cpp( data = as.matrix(data), w , K = 5,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_5,file="chain_10000_5.RData")

start = Sys.time()
chain_10000_6=mcmc_cpp( data = as.matrix(data), w , K = 6,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_6,file="chain_10000_6.RData")

start = Sys.time()
chain_10000_7=mcmc_cpp( data = as.matrix(data), w , K = 7,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_7,file="chain_10000_7.RData")

start = Sys.time()
chain_10000_8=mcmc_cpp( data = as.matrix(data), w , K = 8,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_8,file="chain_10000_8.RData")

start = Sys.time()
chain_10000_9=mcmc_cpp( data = as.matrix(data), w , K = 9,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_9,file="chain_10000_9.RData")

start = Sys.time()
chain_10000_11=mcmc_cpp( data = as.matrix(data), w , K = 11,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_11,file="chain_10000_11.RData")

start = Sys.time()
chain_10000_12=mcmc_cpp( data = as.matrix(data), w , K = 12,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_12,file="chain_10000_12.RData")

start = Sys.time()
chain_10000_13=mcmc_cpp( data = as.matrix(data), w , K = 13,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_13,file="chain_10000_13.RData")

start = Sys.time()
chain_10000_14=mcmc_cpp( data = as.matrix(data), w , K = 14,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_14,file="chain_10000_14.RData")

start = Sys.time()
chain_10000_15=mcmc_cpp( data = as.matrix(data), w , K = 15,burning = 500, n_iter = 10000, save_it = 5 )
end = Sys.time()
end - start

save(chain_10000_15,file="chain_10000_15.RData")

#####Modelos com 2-9, 11-15 tópicos:
#####Cálculo do AIC (Akaike Information Criterion):

#load("chain_10000_2.RData")
aic_2t=AIC_LDA(data_mat=data,chain=chain_10000_2,mcmc_ind=1:dim(chain_10000_2[[1]])[3])
save(aic_2t,file="aic_2t.RData")

#load("chain_10000_3.RData")
aic_3t=AIC_LDA(data_mat=data,chain=chain_10000_3,mcmc_ind=1:dim(chain_10000_3[[1]])[3])
save(aic_3t,file="aic_3t.RData")

#load("chain_10000_4.RData")
aic_4t=AIC_LDA(data_mat=data,chain=chain_10000_4,mcmc_ind=1:dim(chain_10000_4[[1]])[3])
save(aic_4t,file="aic_4t.RData")

#load("chain_10000_5.RData")
aic_5t=AIC_LDA(data_mat=data,chain=chain_10000_5,mcmc_ind=1:dim(chain_10000_5[[1]])[3])
save(aic_5t,file="aic_5t.RData")

#load("chain_10000_6.RData")
aic_6t=AIC_LDA(data_mat=data,chain=chain_10000_6,mcmc_ind=1:dim(chain_10000_6[[1]])[3])
save(aic_6t,file="aic_6t.RData")

#load("chain_10000_7.RData")
aic_7t=AIC_LDA(data_mat=data,chain=chain_10000_7,mcmc_ind=1:dim(chain_10000_7[[1]])[3])
save(aic_7t,file="aic_7t.RData")

load("chain_10000_8.RData")
aic_8t=AIC_LDA(data_mat=data,chain=chain_10000_8,mcmc_ind=1:dim(chain_10000_8[[1]])[3])
save(aic_8t,file="aic_8t.RData")

load("chain_10000_9.RData")
aic_9t=AIC_LDA(data_mat=data,chain=chain_10000_9,mcmc_ind=1:dim(chain_10000_9[[1]])[3])
save(aic_9t,file="aic_9t.RData")

load("chain_10000_11.RData")
aic_11t=AIC_LDA(data_mat=data,chain=chain_10000_11,mcmc_ind=1:dim(chain_10000_11[[1]])[3])
save(aic_11t,file="aic_11t.RData")

load("chain_10000_12.RData")
aic_12t=AIC_LDA(data_mat=data,chain=chain_10000_12,mcmc_ind=1:dim(chain_10000_12[[1]])[3])
save(aic_12t,file="aic_12t.RData")

load("chain_10000_13.RData")
aic_13t=AIC_LDA(data_mat=data,chain=chain_10000_13,mcmc_ind=1:dim(chain_10000_13[[1]])[3])
save(aic_13t,file="aic_13t.RData")

load("chain_10000_14.RData")
aic_14t=AIC_LDA(data_mat=data,chain=chain_10000_14,mcmc_ind=1:dim(chain_10000_14[[1]])[3])
save(aic_14t,file="aic_14t.RData")

load("chain_10000_15.RData")
aic_15t=AIC_LDA(data_mat=data,chain=chain_10000_15,mcmc_ind=1:dim(chain_10000_15[[1]])[3])
save(aic_15t,file="aic_15t.RData")

###Carregando os valores dos AIC:

load("aic_2t.RData")
load("aic_3t.RData")
load("aic_4t.RData")
load("aic_5t.RData")
load("aic_6t.RData")
load("aic_7t.RData")
load("aic_8t.RData")
load("aic_9t.RData")
load("aic_11t.RData")
load("aic_12t.RData")
load("aic_13t.RData")
load("aic_14t.RData")
load("aic_15t.RData")

###Tabela dos AIC:

tabela_aic1=rbind(aic_2t[[1]],aic_3t[[1]],aic_4t[[1]],aic_5t[[1]],aic_6t[[1]],
aic_7t[[1]],aic_8t[[1]],aic_9t[[1]],aic_11t[[1]],aic_12t[[1]],aic_13t[[1]],
aic_14t[[1]],aic_15t[[1]])
colnames(tabela_aic1)="Valor do AIC"
rownames(tabela_aic1)=c("Modelo com 2 tópicos","Modelo com 3 tópicos",
"Modelo com 4 tópicos","Modelo com 5 tópicos","Modelo com 6 tópicos",
"Modelo com 7 tópicos","Modelo com 8 tópicos","Modelo com 9 tópicos",
"Modelo com 11 tópicos","Modelo com 12 tópicos","Modelo com 13 tópicos",
"Modelo com 14 tópicos","Modelo com 15 tópicos")

tabela_aic1 #O melhor modelo é o que tem _ tópicos

#####Cálculo do BIC (Bayesian Information Criterion):

#load("chain_10000_2.RData")
bic_2t=BIC_LDA(data_mat=data,chain=chain_10000_2,mcmc_ind=1:dim(chain_10000_2[[1]])[3])
save(bic_2t,file="bic_2t.RData")

#load("chain_10000_3.RData")
bic_3t=BIC_LDA(data_mat=data,chain=chain_10000_3,mcmc_ind=1:dim(chain_10000_3[[1]])[3])
save(bic_3t,file="bic_3t.RData")

#load("chain_10000_4.RData")
bic_4t=BIC_LDA(data_mat=data,chain=chain_10000_4,mcmc_ind=1:dim(chain_10000_4[[1]])[3])
save(bic_4t,file="bic_4t.RData")

#load("chain_10000_5.RData")
bic_5t=BIC_LDA(data_mat=data,chain=chain_10000_5,mcmc_ind=1:dim(chain_10000_5[[1]])[3])
save(bic_5t,file="bic_5t.RData")

#load("chain_10000_6.RData")
bic_6t=BIC_LDA(data_mat=data,chain=chain_10000_6,mcmc_ind=1:dim(chain_10000_6[[1]])[3])
save(bic_6t,file="bic_6t.RData")

#load("chain_10000_7.RData")
bic_7t=BIC_LDA(data_mat=data,chain=chain_10000_7,mcmc_ind=1:dim(chain_10000_7[[1]])[3])
save(bic_7t,file="bic_7t.RData")

load("chain_10000_8.RData")
bic_8t=BIC_LDA(data_mat=data,chain=chain_10000_8,mcmc_ind=1:dim(chain_10000_8[[1]])[3])
save(bic_8t,file="bic_8t.RData")

load("chain_10000_9.RData")
bic_9t=BIC_LDA(data_mat=data,chain=chain_10000_9,mcmc_ind=1:dim(chain_10000_9[[1]])[3])
save(bic_9t,file="bic_9t.RData")

load("chain_10000_11.RData")
bic_11t=BIC_LDA(data_mat=data,chain=chain_10000_11,mcmc_ind=1:dim(chain_10000_11[[1]])[3])
save(bic_11t,file="bic_11t.RData")

load("chain_10000_12.RData")
bic_12t=BIC_LDA(data_mat=data,chain=chain_10000_12,mcmc_ind=1:dim(chain_10000_12[[1]])[3])
save(bic_12t,file="bic_12t.RData")

load("chain_10000_13.RData")
bic_13t=BIC_LDA(data_mat=data,chain=chain_10000_13,mcmc_ind=1:dim(chain_10000_13[[1]])[3])
save(bic_13t,file="bic_13t.RData")

load("chain_10000_14.RData")
bic_14t=BIC_LDA(data_mat=data,chain=chain_10000_14,mcmc_ind=1:dim(chain_10000_14[[1]])[3])
save(bic_14t,file="bic_14t.RData")

load("chain_10000_15.RData")
bic_15t=BIC_LDA(data_mat=data,chain=chain_10000_15,mcmc_ind=1:dim(chain_10000_15[[1]])[3])
save(bic_15t,file="bic_15t.RData")

###Carregando os valores dos BIC:

load("bic_2t.RData")
load("bic_3t.RData")
load("bic_4t.RData")
load("bic_5t.RData")
load("bic_6t.RData")
load("bic_7t.RData")
load("bic_8t.RData")
load("bic_9t.RData")
load("bic_11t.RData")
load("bic_12t.RData")
load("bic_13t.RData")
load("bic_14t.RData")
load("bic_15t.RData")

###Tabela dos BIC:

tabela_bic1=rbind(bic_2t[[1]],bic_3t[[1]],bic_4t[[1]],bic_5t[[1]],bic_6t[[1]],
bic_7t[[1]],bic_8t[[1]],bic_9t[[1]],bic_11t[[1]],bic_12t[[1]],bic_13t[[1]],
bic_14t[[1]],bic_15t[[1]])
colnames(tabela_bic1)="Valor do BIC"
rownames(tabela_bic1)=c("Modelo com 2 tópicos","Modelo com 3 tópicos",
"Modelo com 4 tópicos","Modelo com 5 tópicos","Modelo com 6 tópicos",
"Modelo com 7 tópicos","Modelo com 8 tópicos","Modelo com 9 tópicos",
"Modelo com 11 tópicos","Modelo com 12 tópicos","Modelo com 13 tópicos",
"Modelo com 14 tópicos","Modelo com 15 tópicos")

tabela_bic1 #O melhor modelo é o que tem _ tópicos

#####Cálculo do DIC (Deviance Information Criterion):

#load("chain_10000_2.RData")
dic_2t=DIC_LDA(data_mat=data,chain=chain_10000_2,mcmc_ind=1:dim(chain_10000_2[[1]])[3])
save(dic_2t,file="dic_2t.RData")

#load("chain_10000_3.RData")
dic_3t=DIC_LDA(data_mat=data,chain=chain_10000_3,mcmc_ind=1:dim(chain_10000_3[[1]])[3])
save(dic_3t,file="dic_3t.RData")

#load("chain_10000_4.RData")
dic_4t=DIC_LDA(data_mat=data,chain=chain_10000_4,mcmc_ind=1:dim(chain_10000_4[[1]])[3])
save(dic_4t,file="dic_4t.RData")

#load("chain_10000_5.RData")
dic_5t=DIC_LDA(data_mat=data,chain=chain_10000_5,mcmc_ind=1:dim(chain_10000_5[[1]])[3])
save(dic_5t,file="dic_5t.RData")

#load("chain_10000_6.RData")
dic_6t=DIC_LDA(data_mat=data,chain=chain_10000_6,mcmc_ind=1:dim(chain_10000_6[[1]])[3])
save(dic_6t,file="dic_6t.RData")

#load("chain_10000_7.RData")
dic_7t=DIC_LDA(data_mat=data,chain=chain_10000_7,mcmc_ind=1:dim(chain_10000_7[[1]])[3])
save(dic_7t,file="dic_7t.RData")

load("chain_10000_8.RData")
dic_8t=DIC_LDA(data_mat=data,chain=chain_10000_8,mcmc_ind=1:dim(chain_10000_8[[1]])[3])
save(dic_8t,file="dic_8t.RData")

load("chain_10000_9.RData")
dic_9t=DIC_LDA(data_mat=data,chain=chain_10000_9,mcmc_ind=1:dim(chain_10000_9[[1]])[3])
save(dic_9t,file="dic_9t.RData")

load("chain_10000_11.RData")
dic_11t=DIC_LDA(data_mat=data,chain=chain_10000_11,mcmc_ind=1:dim(chain_10000_11[[1]])[3])
save(dic_11t,file="dic_11t.RData")

load("chain_10000_12.RData")
dic_12t=DIC_LDA(data_mat=data,chain=chain_10000_12,mcmc_ind=1:dim(chain_10000_12[[1]])[3])
save(dic_12t,file="dic_12t.RData")

load("chain_10000_13.RData")
dic_13t=DIC_LDA(data_mat=data,chain=chain_10000_13,mcmc_ind=1:dim(chain_10000_13[[1]])[3])
save(dic_13t,file="dic_13t.RData")

load("chain_10000_14.RData")
dic_14t=DIC_LDA(data_mat=data,chain=chain_10000_14,mcmc_ind=1:dim(chain_10000_14[[1]])[3])
save(dic_14t,file="dic_14t.RData")

load("chain_10000_15.RData")
dic_15t=DIC_LDA(data_mat=data,chain=chain_10000_15,mcmc_ind=1:dim(chain_10000_15[[1]])[3])
save(dic_15t,file="dic_15t.RData")

###Carregando os valores dos DIC:

load("dic_2t.RData")
load("dic_3t.RData")
load("dic_4t.RData")
load("dic_5t.RData")
load("dic_6t.RData")
load("dic_7t.RData")
load("dic_8t.RData")
load("dic_9t.RData")
load("dic_11t.RData")
load("dic_12t.RData")
load("dic_13t.RData")
load("dic_14t.RData")
load("dic_15t.RData")

###Tabela dos DIC:

tabela_dic1=rbind(dic_2t,dic_3t,dic_4t,dic_5t,dic_6t,
dic_7t,dic_8t,dic_9t,dic_11t,dic_12t,dic_13t,
dic_14t,dic_15t)
colnames(tabela_dic1)="Valor do DIC"
rownames(tabela_dic1)=c("Modelo com 2 tópicos","Modelo com 3 tópicos",
"Modelo com 4 tópicos","Modelo com 5 tópicos","Modelo com 6 tópicos",
"Modelo com 7 tópicos","Modelo com 8 tópicos","Modelo com 9 tópicos",
"Modelo com 11 tópicos","Modelo com 12 tópicos","Modelo com 13 tópicos",
"Modelo com 14 tópicos","Modelo com 15 tópicos")

tabela_dic1 #O melhor modelo é o que tem _ tópicos

###Watanabe–Akaike Information Criterion (WAIC):

require(LaplacesDemon)

#load("chain_10000_2.RData")
waic_2t=WAIC_LDA(data_mat=data,chain=chain_10000_2,mcmc_ind=1:dim(chain_10000_2[[1]])[3])
save(waic_2t,file="waic_2t.RData")

#load("chain_10000_3.RData")
waic_3t=WAIC_LDA(data_mat=data,chain=chain_10000_3,mcmc_ind=1:dim(chain_10000_3[[1]])[3])
save(waic_3t,file="waic_3t.RData")

#load("chain_10000_4.RData")
waic_4t=WAIC_LDA(data_mat=data,chain=chain_10000_4,mcmc_ind=1:dim(chain_10000_4[[1]])[3])
save(waic_4t,file="waic_4t.RData")

#load("chain_10000_5.RData")
waic_5t=WAIC_LDA(data_mat=data,chain=chain_10000_5,mcmc_ind=1:dim(chain_10000_5[[1]])[3])
save(waic_5t,file="waic_5t.RData")

#load("chain_10000_6.RData")
waic_6t=WAIC_LDA(data_mat=data,chain=chain_10000_6,mcmc_ind=1:dim(chain_10000_6[[1]])[3])
save(waic_6t,file="waic_6t.RData")

#load("chain_10000_7.RData")
waic_7t=WAIC_LDA(data_mat=data,chain=chain_10000_7,mcmc_ind=1:dim(chain_10000_7[[1]])[3])
save(waic_7t,file="waic_7t.RData")

load("chain_10000_8.RData")
waic_8t=WAIC_LDA(data_mat=data,chain=chain_10000_8,mcmc_ind=1:dim(chain_10000_8[[1]])[3])
save(waic_8t,file="waic_8t.RData")

load("chain_10000_9.RData")
waic_9t=WAIC_LDA(data_mat=data,chain=chain_10000_9,mcmc_ind=1:dim(chain_10000_9[[1]])[3])
save(waic_9t,file="waic_9t.RData")

load("chain_10000_11.RData")
waic_11t=WAIC_LDA(data_mat=data,chain=chain_10000_11,mcmc_ind=1:dim(chain_10000_11[[1]])[3])
save(waic_11t,file="waic_11t.RData")

load("chain_10000_12.RData")
waic_12t=WAIC_LDA(data_mat=data,chain=chain_10000_12,mcmc_ind=1:dim(chain_10000_12[[1]])[3])
save(waic_12t,file="waic_12t.RData")

load("chain_10000_13.RData")
waic_13t=WAIC_LDA(data_mat=data,chain=chain_10000_13,mcmc_ind=1:dim(chain_10000_13[[1]])[3])
save(waic_13t,file="waic_13t.RData")

load("chain_10000_14.RData")
waic_14t=WAIC_LDA(data_mat=data,chain=chain_10000_14,mcmc_ind=1:dim(chain_10000_14[[1]])[3])
save(waic_14t,file="waic_14t.RData")

load("chain_10000_15.RData")
waic_15t=WAIC_LDA(data_mat=data,chain=chain_10000_15,mcmc_ind=1:dim(chain_10000_15[[1]])[3])
save(waic_15t,file="waic_15t.RData")

###Carregando os valores dos WAIC:

load("waic_2t.RData")
load("waic_3t.RData")
load("waic_4t.RData")
load("waic_5t.RData")
load("waic_6t.RData")
load("waic_7t.RData")
load("waic_8t.RData")
load("waic_9t.RData")
load("waic_11t.RData")
load("waic_12t.RData")
load("waic_13t.RData")
load("waic_14t.RData")
load("waic_15t.RData")

###Tabela dos WAIC:

tabela_waic1=rbind(waic_2t[[1]],waic_3t[[1]],waic_4t[[1]],waic_5t[[1]],
waic_6t[[1]],waic_7t[[1]],waic_8t[[1]],waic_9t[[1]],waic_11t[[1]],waic_12t[[1]],
waic_13t[[1]],waic_14t[[1]],waic_15t[[1]])
colnames(tabela_waic1)="Valor do WAIC"
rownames(tabela_waic1)=c("Modelo com 2 tópicos","Modelo com 3 tópicos",
"Modelo com 4 tópicos","Modelo com 5 tópicos","Modelo com 6 tópicos",
"Modelo com 7 tópicos","Modelo com 8 tópicos","Modelo com 9 tópicos",
"Modelo com 11 tópicos","Modelo com 12 tópicos","Modelo com 13 tópicos",
"Modelo com 14 tópicos","Modelo com 15 tópicos")

tabela_waic1 #O melhor modelo é o que tem _ tópicos

###Tabela com todos os criterios de informacao:

tabela_crit_total=cbind(rbind(tabela_aic,tabela_aic1),
rbind(tabela_bic,tabela_bic1),
rbind(tabela_dic,tabela_dic1),
rbind(tabela_waic,tabela_waic1))
tabela_crit_total=tabela_crit_total[order(tabela_crit_total[,1],decreasing=F),]

tabela_crit_total

#save(tabela_crit_total,file="tabela_crit_total.RData")
#load("tabela_crit_total.RData")

#csv_path  = 'Tabela_crit_total.csv'
#write.csv(tabela_crit_total, csv_path, row.names = TRUE)

###Gráficos dos criterios nos modelos LDA, com diferente número de tópicos:

par(mfrow=c(1,2))

##DIC:

plot(x=c(2:15,20,30,40,50),y=tabela_crit_total[,3],
xlab="Número de tópicos (k)",ylab="Valor calculado do DIC",type="l",
main="Valor calculado do DIC para modelos LDA
 com diferente número de tópicos (k)")

##WAIC

plot(x=c(2:15,20,30,40,50),y=tabela_crit_total[,4],
xlab="Número de tópicos (k)",ylab="Valor calculado do WAIC",type="l",
main="Valor calculado do WAIC para modelos LDA
 com diferente número de tópicos (k)")

###############################################################################
####Calculando novamente o valor do AIC e BIC,
####disminuindo o número de palavras (V)
################################################################################

lista_palavras=0
j=0

for(d in 1:length(w)){

Nd=length(w[[d]])

for(i in 1:Nd){
j=j+1
lista_palavras[j]=w[[d]][i]

}
}

lista_palavras=lista_palavras+1

tf_lp=table(lista_palavras) #Tabela de frequencia da lista de palavras

tf_lp=as.matrix(tf_lp)

#length(lista_palavras)-sum(tf_lp)  #Número de NA, 517
#summary(lista_palavras)[7]

barplot(table(tf_lp),xlab="Frequência da palavra no vocabulário",
ylab="Número de palavras con essa frequência")

tf_fpv=as.matrix(table(tf_lp)) #Tabela de frequencia, da frequencia das palavras no vocabulário

head(tf_fpv)

l_tf_fpv=0    #Lista de frequências

for(i in 1:length(tf_fpv)){

l_tf_fpv=c(l_tf_fpv,rep(rownames(tf_fpv)[i],tf_fpv[i]))

}
l_tf_fpv=l_tf_fpv[-1]

summary(as.numeric(l_tf_fpv))
boxplot(as.numeric(l_tf_fpv))

###
##Agrupando as palavras com frequência maior ou igual do que 30, para barplot:

tf_lp2=tf_lp
ind_mai30=which(tf_lp2>=30)
tf_lp2_sem_mai30=as.vector(tf_lp2[-ind_mai30,])

tf_lp2_mais30=c(tf_lp2_sem_mai30,rep(30,length(ind_mai30)))

tabela=table(tf_lp2_mais30)
names(tabela)[30]="+30"

barplot(tabela,xlab="Frequência da palavra no corpus",
ylab="Número de palavras com essa frequência",cex.lab=1.7)

##Olha-se que o primeiro quartil é dos documentos com frequência "1", entao
##tira-se o número de palavras com essa frequência no vocavulario

#V_new=length(tf_lp)-tf_fpv[1] #O novo tamanho do vocabulário (V) é de 3457 palavras
V_new=length(tf_lp)-sum(tf_fpv[1:3]) #O novo tamanho do vocabulário (V) é de 2113 palavras, tirando as palavras com frequência entre 1 e 3

V_new


###AIC, com o novo V:

load("aic_10t.RData")
load("aic_20t.RData")
load("aic_30t.RData")
load("aic_40t.RData")
load("aic_50t.RData")
load("aic_2t.RData")
load("aic_3t.RData")
load("aic_4t.RData")
load("aic_5t.RData")
load("aic_6t.RData")
load("aic_7t.RData")
load("aic_8t.RData")
load("aic_9t.RData")
load("aic_11t.RData")
load("aic_12t.RData")
load("aic_13t.RData")
load("aic_14t.RData")
load("aic_15t.RData")

p = dim(aic_10t$beta_hat)[1]*V_new + dim(aic_10t$theta_hat)[1]*dim(aic_10t$theta_hat)[2]
aic_10t_new = 2*p-2*aic_10t$Log_Verossimilhança

p = dim(aic_20t$beta_hat)[1]*V_new + dim(aic_20t$theta_hat)[1]*dim(aic_20t$theta_hat)[2]
aic_20t_new = 2*p-2*aic_20t$Log_Verossimilhança

p = dim(aic_30t$beta_hat)[1]*V_new + dim(aic_30t$theta_hat)[1]*dim(aic_30t$theta_hat)[2]
aic_30t_new = 2*p-2*aic_30t$Log_Verossimilhança

p = dim(aic_40t$beta_hat)[1]*V_new + dim(aic_40t$theta_hat)[1]*dim(aic_40t$theta_hat)[2]
aic_40t_new = 2*p-2*aic_40t$Log_Verossimilhança

p = dim(aic_50t$beta_hat)[1]*V_new + dim(aic_50t$theta_hat)[1]*dim(aic_50t$theta_hat)[2]
aic_50t_new = 2*p-2*aic_50t$Log_Verossimilhança

p = dim(aic_2t$beta_hat)[1]*V_new + dim(aic_2t$theta_hat)[1]*dim(aic_2t$theta_hat)[2]
aic_2t_new = 2*p-2*aic_2t$Log_Verossimilhança

p = dim(aic_3t$beta_hat)[1]*V_new + dim(aic_3t$theta_hat)[1]*dim(aic_3t$theta_hat)[2]
aic_3t_new = 2*p-2*aic_3t$Log_Verossimilhança

p = dim(aic_4t$beta_hat)[1]*V_new + dim(aic_4t$theta_hat)[1]*dim(aic_4t$theta_hat)[2]
aic_4t_new = 2*p-2*aic_4t$Log_Verossimilhança

p = dim(aic_5t$beta_hat)[1]*V_new + dim(aic_5t$theta_hat)[1]*dim(aic_5t$theta_hat)[2]
aic_5t_new = 2*p-2*aic_5t$Log_Verossimilhança

p = dim(aic_6t$beta_hat)[1]*V_new + dim(aic_6t$theta_hat)[1]*dim(aic_6t$theta_hat)[2]
aic_6t_new = 2*p-2*aic_6t$Log_Verossimilhança

p = dim(aic_7t$beta_hat)[1]*V_new + dim(aic_7t$theta_hat)[1]*dim(aic_7t$theta_hat)[2]
aic_7t_new = 2*p-2*aic_7t$Log_Verossimilhança

p = dim(aic_8t$beta_hat)[1]*V_new + dim(aic_8t$theta_hat)[1]*dim(aic_8t$theta_hat)[2]
aic_8t_new = 2*p-2*aic_8t$Log_Verossimilhança

p = dim(aic_9t$beta_hat)[1]*V_new + dim(aic_9t$theta_hat)[1]*dim(aic_9t$theta_hat)[2]
aic_9t_new = 2*p-2*aic_9t$Log_Verossimilhança

p = dim(aic_11t$beta_hat)[1]*V_new + dim(aic_11t$theta_hat)[1]*dim(aic_11t$theta_hat)[2]
aic_11t_new = 2*p-2*aic_11t$Log_Verossimilhança

p = dim(aic_12t$beta_hat)[1]*V_new + dim(aic_12t$theta_hat)[1]*dim(aic_12t$theta_hat)[2]
aic_12t_new = 2*p-2*aic_12t$Log_Verossimilhança

p = dim(aic_13t$beta_hat)[1]*V_new + dim(aic_13t$theta_hat)[1]*dim(aic_13t$theta_hat)[2]
aic_13t_new = 2*p-2*aic_13t$Log_Verossimilhança

p = dim(aic_14t$beta_hat)[1]*V_new + dim(aic_14t$theta_hat)[1]*dim(aic_14t$theta_hat)[2]
aic_14t_new = 2*p-2*aic_14t$Log_Verossimilhança

p = dim(aic_15t$beta_hat)[1]*V_new + dim(aic_15t$theta_hat)[1]*dim(aic_15t$theta_hat)[2]
aic_15t_new = 2*p-2*aic_15t$Log_Verossimilhança

##Tabela do AIC com novo "V":

tabela_aic_new=rbind(aic_2t_new,aic_3t_new,aic_4t_new,aic_5t_new,aic_6t_new,
aic_7t_new,aic_8t_new,aic_9t_new,aic_11t_new,aic_12t_new,aic_13t_new,
aic_14t_new,aic_15t_new,aic_10t_new,aic_20t_new,aic_30t_new,aic_40t_new,aic_50t_new)
colnames(tabela_aic_new)="Valor do AIC"
rownames(tabela_aic_new)=c("Modelo com 2 tópicos","Modelo com 3 tópicos",
"Modelo com 4 tópicos","Modelo com 5 tópicos","Modelo com 6 tópicos",
"Modelo com 7 tópicos","Modelo com 8 tópicos","Modelo com 9 tópicos",
"Modelo com 11 tópicos","Modelo com 12 tópicos","Modelo com 13 tópicos",
"Modelo com 14 tópicos","Modelo com 15 tópicos","Modelo com 10 tópicos",
"Modelo com 20 tópicos","Modelo com 30 tópicos","Modelo com 40 tópicos",
"Modelo com 50 tópicos")

tabela_aic_new

plot(x=c(2:15,20,30,40,50),y=tabela_aic_new[c(1:8,14,9:13,15:18)],
xlab="Número de tópicos (K)",ylab="Valor do AIC",type="l")#,main="Cálculo do AIC, tirando as palavras do vocabulario com frequência igual desde 1 até 3")
points(x=c(2:15,20,30,40,50),y=tabela_aic_new[c(1:8,14,9:13,15:18)],
pch=19,cex=0.8)


###BIC, com o novo V:

load("bic_10t.RData")
load("bic_20t.RData")
load("bic_30t.RData")
load("bic_40t.RData")
load("bic_50t.RData")
load("bic_2t.RData")
load("bic_3t.RData")
load("bic_4t.RData")
load("bic_5t.RData")
load("bic_6t.RData")
load("bic_7t.RData")
load("bic_8t.RData")
load("bic_9t.RData")
load("bic_11t.RData")
load("bic_12t.RData")
load("bic_13t.RData")
load("bic_14t.RData")
load("bic_15t.RData")

#nw_new=bic_10t$Número_de_Observações-tf_fpv[1]  #Tirando as observacoes correspondentes as palavras com frequência igual à 1
nw_new=bic_10t$Número_de_Observações-(tf_fpv[1]+2*tf_fpv[2]+3*tf_fpv[3]) #Tirando as observacoes correspondentes as palavras com frequência igual à 1 até 3

p = dim(bic_10t$beta_hat)[1]*V_new + dim(bic_10t$theta_hat)[1]*dim(bic_10t$theta_hat)[2]
bic_10t_new = p*log(nw_new)-2*bic_10t$Log_Verossimilhança

p = dim(bic_20t$beta_hat)[1]*V_new + dim(bic_20t$theta_hat)[1]*dim(bic_20t$theta_hat)[2]
bic_20t_new = p*log(nw_new)-2*bic_20t$Log_Verossimilhança

p = dim(bic_30t$beta_hat)[1]*V_new + dim(bic_30t$theta_hat)[1]*dim(bic_30t$theta_hat)[2]
bic_30t_new = p*log(nw_new)-2*bic_30t$Log_Verossimilhança

p = dim(bic_40t$beta_hat)[1]*V_new + dim(bic_40t$theta_hat)[1]*dim(bic_40t$theta_hat)[2]
bic_40t_new = p*log(nw_new)-2*bic_40t$Log_Verossimilhança

p = dim(bic_50t$beta_hat)[1]*V_new + dim(bic_50t$theta_hat)[1]*dim(bic_50t$theta_hat)[2]
bic_50t_new = p*log(nw_new)-2*bic_50t$Log_Verossimilhança

p = dim(bic_2t$beta_hat)[1]*V_new + dim(bic_2t$theta_hat)[1]*dim(bic_2t$theta_hat)[2]
bic_2t_new = p*log(nw_new)-2*bic_2t$Log_Verossimilhança

p = dim(bic_3t$beta_hat)[1]*V_new + dim(bic_3t$theta_hat)[1]*dim(bic_3t$theta_hat)[2]
bic_3t_new = p*log(nw_new)-2*bic_3t$Log_Verossimilhança

p = dim(bic_4t$beta_hat)[1]*V_new + dim(bic_4t$theta_hat)[1]*dim(bic_4t$theta_hat)[2]
bic_4t_new = p*log(nw_new)-2*bic_4t$Log_Verossimilhança

p = dim(bic_5t$beta_hat)[1]*V_new + dim(bic_5t$theta_hat)[1]*dim(bic_5t$theta_hat)[2]
bic_5t_new = p*log(nw_new)-2*bic_5t$Log_Verossimilhança

p = dim(bic_6t$beta_hat)[1]*V_new + dim(bic_6t$theta_hat)[1]*dim(bic_6t$theta_hat)[2]
bic_6t_new = p*log(nw_new)-2*bic_6t$Log_Verossimilhança

p = dim(bic_7t$beta_hat)[1]*V_new + dim(bic_7t$theta_hat)[1]*dim(bic_7t$theta_hat)[2]
bic_7t_new = p*log(nw_new)-2*bic_7t$Log_Verossimilhança

p = dim(bic_8t$beta_hat)[1]*V_new + dim(bic_8t$theta_hat)[1]*dim(bic_8t$theta_hat)[2]
bic_8t_new = p*log(nw_new)-2*bic_8t$Log_Verossimilhança

p = dim(bic_9t$beta_hat)[1]*V_new + dim(bic_9t$theta_hat)[1]*dim(bic_9t$theta_hat)[2]
bic_9t_new = p*log(nw_new)-2*bic_9t$Log_Verossimilhança

p = dim(bic_11t$beta_hat)[1]*V_new + dim(bic_11t$theta_hat)[1]*dim(bic_11t$theta_hat)[2]
bic_11t_new = p*log(nw_new)-2*bic_11t$Log_Verossimilhança

p = dim(bic_12t$beta_hat)[1]*V_new + dim(bic_12t$theta_hat)[1]*dim(bic_12t$theta_hat)[2]
bic_12t_new = p*log(nw_new)-2*bic_12t$Log_Verossimilhança

p = dim(bic_13t$beta_hat)[1]*V_new + dim(bic_13t$theta_hat)[1]*dim(bic_13t$theta_hat)[2]
bic_13t_new = p*log(nw_new)-2*bic_13t$Log_Verossimilhança

p = dim(bic_14t$beta_hat)[1]*V_new + dim(bic_14t$theta_hat)[1]*dim(bic_14t$theta_hat)[2]
bic_14t_new = p*log(nw_new)-2*bic_14t$Log_Verossimilhança

p = dim(bic_15t$beta_hat)[1]*V_new + dim(bic_15t$theta_hat)[1]*dim(bic_15t$theta_hat)[2]
bic_15t_new = p*log(nw_new)-2*bic_15t$Log_Verossimilhança

##Tabela do BIC com novo "V":

tabela_bic_new=rbind(bic_2t_new,bic_3t_new,bic_4t_new,bic_5t_new,bic_6t_new,
bic_7t_new,bic_8t_new,bic_9t_new,bic_11t_new,bic_12t_new,bic_13t_new,
bic_14t_new,bic_15t_new,bic_10t_new,bic_20t_new,bic_30t_new,bic_40t_new,bic_50t_new)
colnames(tabela_bic_new)="Valor do BIC"
rownames(tabela_bic_new)=c("Modelo com 2 tópicos","Modelo com 3 tópicos",
"Modelo com 4 tópicos","Modelo com 5 tópicos","Modelo com 6 tópicos",
"Modelo com 7 tópicos","Modelo com 8 tópicos","Modelo com 9 tópicos",
"Modelo com 11 tópicos","Modelo com 12 tópicos","Modelo com 13 tópicos",
"Modelo com 14 tópicos","Modelo com 15 tópicos","Modelo com 10 tópicos",
"Modelo com 20 tópicos","Modelo com 30 tópicos","Modelo com 40 tópicos",
"Modelo com 50 tópicos")

tabela_bic_new

###Tabela com AIC e BIC, como o novo número de palavras (V):

tabela_aic_bic_new=cbind(tabela_aic_new,tabela_bic_new)
plot(x=c(2:15,20,30,40,50),y=tabela_aic_bic_new[c(1:8,14,9:13,15:18),1],
xlab="Número de tópicos (k)",ylab="Valor do AIC",type="l",
main="Cálculo do AIC, tirando as palavras do vocabulario 
com frequência igual desde 1 até 3")

tabela_aic_bic_new=tabela_aic_bic_new[order(tabela_aic_bic_new[,1],decreasing=F),]
tabela_aic_bic_new

##############################################
### Cálculo da perplexidade
##############################################

w = transform(data)

for (d in 1:length(w)){
w[[d]] = w[[d]] + 1
}

for (d in 1:length(w)){
w[[d]]=na.omit(w[[d]])
}

obs=0

for (d in 1:length(w)){
obs=obs+length(w[[d]])
}

load("aic_2t.RData")
load("aic_3t.RData")
load("aic_4t.RData")
load("aic_5t.RData")
load("aic_6t.RData")
load("aic_7t.RData")
load("aic_8t.RData")
load("aic_9t.RData")
load("aic_10t.RData")
load("aic_11t.RData")
load("aic_12t.RData")
load("aic_13t.RData")
load("aic_14t.RData")
load("aic_15t.RData")
load("aic_20t.RData")
load("aic_30t.RData")
load("aic_40t.RData")
load("aic_50t.RData")

###Funcao do cálculo da perplexidade:

calc_perplexidade=function(obj_cadeia,e){

#beta_hat=aic_10t$beta_hat
#theta_hat=aic_10t$theta_hat
#e=1e-323

beta_hat=obj_cadeia$beta_hat
theta_hat=obj_cadeia$theta_hat

vet_aux_pwd=rep(0,nrow(theta_hat))

for(d in 1:nrow(theta_hat)){

Nd=length(w[[d]])
prob_pal_docxtop=matrix(0,nrow(beta_hat),Nd) #Probabilidades das palavras do documento d x tópico
vet_aux_top=rep(0,nrow(beta_hat))

for(k in 1:nrow(beta_hat)){
for(i in 1:Nd){

prob_pal_docxtop[k,i]=beta_hat[k,w[[d]][i]]
}
vet_aux_top[k]=prod(prob_pal_docxtop[k,])*theta_hat[d,k]+e #com epsilon para evitar obter zeros
}
vet_aux_pwd[d]=sum(vet_aux_top)
}

perplexidade=exp(-sum(log(vet_aux_pwd))/obs)
return(perplexidade)

}


##Cálculo da perplexidade:

med_perplexidade_2t=calc_perplexidade(aic_2t,1e-323)
med_perplexidade_3t=calc_perplexidade(aic_3t,1e-323)
med_perplexidade_4t=calc_perplexidade(aic_4t,1e-323)
med_perplexidade_5t=calc_perplexidade(aic_5t,1e-323)
med_perplexidade_6t=calc_perplexidade(aic_6t,1e-323)
med_perplexidade_7t=calc_perplexidade(aic_7t,1e-323)
med_perplexidade_8t=calc_perplexidade(aic_8t,1e-323)
med_perplexidade_9t=calc_perplexidade(aic_9t,1e-323)
med_perplexidade_10t=calc_perplexidade(aic_10t,1e-323)
med_perplexidade_11t=calc_perplexidade(aic_11t,1e-323)
med_perplexidade_12t=calc_perplexidade(aic_12t,1e-323)
med_perplexidade_13t=calc_perplexidade(aic_13t,1e-323)
med_perplexidade_14t=calc_perplexidade(aic_14t,1e-323)
med_perplexidade_15t=calc_perplexidade(aic_15t,1e-323)
med_perplexidade_20t=calc_perplexidade(aic_20t,1e-323)
med_perplexidade_30t=calc_perplexidade(aic_30t,1e-323)
med_perplexidade_40t=calc_perplexidade(aic_40t,1e-323)
med_perplexidade_50t=calc_perplexidade(aic_50t,1e-323)

#save(med_perplexidade_2t,file="med_perplexidade_2t.RData")
#save(med_perplexidade_3t,file="med_perplexidade_3t.RData")
#save(med_perplexidade_4t,file="med_perplexidade_4t.RData")
#save(med_perplexidade_5t,file="med_perplexidade_5t.RData")
#save(med_perplexidade_6t,file="med_perplexidade_6t.RData")
#save(med_perplexidade_7t,file="med_perplexidade_7t.RData")
#save(med_perplexidade_8t,file="med_perplexidade_8t.RData")
#save(med_perplexidade_9t,file="med_perplexidade_9t.RData")
#save(med_perplexidade_10t,file="med_perplexidade_10t.RData")
#save(med_perplexidade_11t,file="med_perplexidade_11t.RData")
#save(med_perplexidade_12t,file="med_perplexidade_12t.RData")
#save(med_perplexidade_13t,file="med_perplexidade_13t.RData")
#save(med_perplexidade_14t,file="med_perplexidade_14t.RData")
#save(med_perplexidade_15t,file="med_perplexidade_15t.RData")
#save(med_perplexidade_20t,file="med_perplexidade_20t.RData")
#save(med_perplexidade_30t,file="med_perplexidade_30t.RData")
#save(med_perplexidade_40t,file="med_perplexidade_40t.RData")
#save(med_perplexidade_50t,file="med_perplexidade_50t.RData")

load("med_perplexidade_2t.RData")
load("med_perplexidade_3t.RData")
load("med_perplexidade_4t.RData")
load("med_perplexidade_5t.RData")
load("med_perplexidade_6t.RData")
load("med_perplexidade_7t.RData")
load("med_perplexidade_8t.RData")
load("med_perplexidade_9t.RData")
load("med_perplexidade_10t.RData")
load("med_perplexidade_11t.RData")
load("med_perplexidade_12t.RData")
load("med_perplexidade_13t.RData")
load("med_perplexidade_14t.RData")
load("med_perplexidade_15t.RData")
load("med_perplexidade_20t.RData")
load("med_perplexidade_30t.RData")
load("med_perplexidade_40t.RData")
load("med_perplexidade_50t.RData")

##Amostrando os valores calculados da perplexidade:

vet_perplexidade=c(med_perplexidade_2t,
med_perplexidade_3t,
med_perplexidade_4t,
med_perplexidade_5t,
med_perplexidade_6t,
med_perplexidade_7t,
med_perplexidade_8t,
med_perplexidade_9t,
med_perplexidade_10t,
med_perplexidade_11t,
med_perplexidade_12t,
med_perplexidade_13t,
med_perplexidade_14t,
med_perplexidade_15t,
med_perplexidade_20t,
med_perplexidade_30t,
med_perplexidade_40t,
med_perplexidade_50t)

tabela_perplexidade=cbind(c(2:15,20,30,40,50),vet_perplexidade)
colnames(tabela_perplexidade)=c("Número de tópicos",
"Valor da perplexidade")
tabela_perplexidade

plot(c(2:15,20,30,40,50),vet_perplexidade,type="l",
xlab="Número de tópicos no modelo LDA",
ylab="Valor da perplexidade")

##Depois de olhar os valores calculados da perplexidade, faria-se escolha do
##modelo LDA com 50 tópicos.

############
### Cálculo para o modelo treinado com 50000 iteracoes

data1=data[-(which(is.na(data[,1]))),]

w = transform(data1)

for (d in 1:length(w)){
w[[d]] = w[[d]] + 1
}

for (d in 1:length(w)){
w[[d]]=na.omit(w[[d]])
}

obs=0

for (d in 1:length(w)){
obs=obs+length(w[[d]])
}

load("aic_50000_2t.RData")
load("aic_50000_3t.RData")
load("aic_50000_4t.RData")
load("aic_50000_5t.RData")
load("aic_50000_6t.RData")
load("aic_50000_7t.RData")
load("aic_50000_8t.RData")
load("aic_50000_9t.RData")
load("aic_50000_10t.RData")
#load("aic_50000_11t.RData")
#load("aic_50000_12t.RData")
#load("aic_50000_13t.RData")
#load("aic_50000_14t.RData")
#load("aic_50000_15t.RData")
load("aic_50000_20t.RData")
load("aic_50000_30t.RData")
load("aic_50000_40t.RData")
load("aic_50000_50t.RData")

###Funcao do cálculo da perplexidade:

calc_perplexidade=function(obj_cadeia,e){

#beta_hat=aic_50000_10t$beta_hat
#theta_hat=aic_50000_10t$theta_hat
#e=1e-323

beta_hat=obj_cadeia$beta_hat
theta_hat=obj_cadeia$theta_hat

vet_aux_pwd=rep(0,nrow(theta_hat))

for(d in 1:nrow(theta_hat)){

Nd=length(w[[d]])
prob_pal_docxtop=matrix(0,nrow(beta_hat),Nd) #Probabilidades das palavras do documento d x tópico
vet_aux_top=rep(0,nrow(beta_hat))

for(k in 1:nrow(beta_hat)){
for(i in 1:Nd){

prob_pal_docxtop[k,i]=beta_hat[k,w[[d]][i]]
}
vet_aux_top[k]=prod(prob_pal_docxtop[k,])*theta_hat[d,k]+e #com epsilon para evitar obter zeros
}
vet_aux_pwd[d]=sum(vet_aux_top)
}

perplexidade=exp(-sum(log(vet_aux_pwd))/obs)
return(perplexidade)

}

##Cálculo da perplexidade:

med_50000_perplexidade_2t=calc_perplexidade(aic_50000_2t,1e-323)
med_50000_perplexidade_3t=calc_perplexidade(aic_50000_3t,1e-323)
med_50000_perplexidade_4t=calc_perplexidade(aic_50000_4t,1e-323)
med_50000_perplexidade_5t=calc_perplexidade(aic_50000_5t,1e-323)
med_50000_perplexidade_6t=calc_perplexidade(aic_50000_6t,1e-323)
med_50000_perplexidade_7t=calc_perplexidade(aic_50000_7t,1e-323)
med_50000_perplexidade_8t=calc_perplexidade(aic_50000_8t,1e-323)
med_50000_perplexidade_9t=calc_perplexidade(aic_50000_9t,1e-323)
med_50000_perplexidade_10t=calc_perplexidade(aic_50000_10t,1e-323)
#med_50000_perplexidade_11t=calc_perplexidade(aic_50000_11t,1e-323)
#med_50000_perplexidade_12t=calc_perplexidade(aic_50000_12t,1e-323)
#med_50000_perplexidade_13t=calc_perplexidade(aic_50000_13t,1e-323)
#med_50000_perplexidade_14t=calc_perplexidade(aic_50000_14t,1e-323)
#med_50000_perplexidade_15t=calc_perplexidade(aic_50000_15t,1e-323)
med_50000_perplexidade_20t=calc_perplexidade(aic_50000_20t,1e-323)
med_50000_perplexidade_30t=calc_perplexidade(aic_50000_30t,1e-323)
med_50000_perplexidade_40t=calc_perplexidade(aic_50000_40t,1e-323)
med_50000_perplexidade_50t=calc_perplexidade(aic_50000_50t,1e-323)

#save(med_50000_perplexidade_2t,file="med_50000_perplexidade_2t.RData")
#save(med_50000_perplexidade_3t,file="med_50000_perplexidade_3t.RData")
#save(med_50000_perplexidade_4t,file="med_50000_perplexidade_4t.RData")
#save(med_50000_perplexidade_5t,file="med_50000_perplexidade_5t.RData")
#save(med_50000_perplexidade_6t,file="med_50000_perplexidade_6t.RData")
#save(med_50000_perplexidade_7t,file="med_50000_perplexidade_7t.RData")
#save(med_50000_perplexidade_8t,file="med_50000_perplexidade_8t.RData")
#save(med_50000_perplexidade_9t,file="med_50000_perplexidade_9t.RData")
#save(med_50000_perplexidade_10t,file="med_50000_perplexidade_10t.RData")
#save(med_50000_perplexidade_11t,file="med_50000_perplexidade_11t.RData")
#save(med_50000_perplexidade_12t,file="med_50000_perplexidade_12t.RData")
#save(med_50000_perplexidade_13t,file="med_50000_perplexidade_13t.RData")
#save(med_50000_perplexidade_14t,file="med_50000_perplexidade_14t.RData")
#save(med_50000_perplexidade_15t,file="med_50000_perplexidade_15t.RData")
#save(med_50000_perplexidade_20t,file="med_50000_perplexidade_20t.RData")
#save(med_50000_perplexidade_30t,file="med_50000_perplexidade_30t.RData")
#save(med_50000_perplexidade_40t,file="med_50000_perplexidade_40t.RData")
#save(med_50000_perplexidade_50t,file="med_50000_perplexidade_50t.RData")

load("med_50000_perplexidade_2t.RData")
load("med_50000_perplexidade_3t.RData")
load("med_50000_perplexidade_4t.RData")
load("med_50000_perplexidade_5t.RData")
load("med_50000_perplexidade_6t.RData")
load("med_50000_perplexidade_7t.RData")
load("med_50000_perplexidade_8t.RData")
load("med_50000_perplexidade_9t.RData")
load("med_50000_perplexidade_10t.RData")
#load("med_50000_perplexidade_11t.RData")
#load("med_50000_perplexidade_12t.RData")
#load("med_50000_perplexidade_13t.RData")
#load("med_50000_perplexidade_14t.RData")
#load("med_50000_perplexidade_15t.RData")
load("med_50000_perplexidade_20t.RData")
load("med_50000_perplexidade_30t.RData")
load("med_50000_perplexidade_40t.RData")
load("med_50000_perplexidade_50t.RData")

##Amostrando os valores calculados da perplexidade:

vet_perplexidade=c(med_50000_perplexidade_2t,
med_50000_perplexidade_3t,
med_50000_perplexidade_4t,
med_50000_perplexidade_5t,
med_50000_perplexidade_6t,
med_50000_perplexidade_7t,
med_50000_perplexidade_8t,
med_50000_perplexidade_9t,
med_50000_perplexidade_10t,
#med_50000_perplexidade_11t,
#med_50000_perplexidade_12t,
#med_50000_perplexidade_13t,
#med_50000_perplexidade_14t,
#med_50000_perplexidade_15t,
med_50000_perplexidade_20t,
med_50000_perplexidade_30t,
med_50000_perplexidade_40t,
med_50000_perplexidade_50t)

#tabela_perplexidade=cbind(c(2:15,20,30,40,50),vet_perplexidade)
tabela_perplexidade=cbind(c(2:10,20,30,40,50),vet_perplexidade)
colnames(tabela_perplexidade)=c("Número de tópicos",
"Valor da perplexidade")
tabela_perplexidade

#plot(c(2:15,20,30,40,50),vet_perplexidade,type="l",
plot(c(2:10,20,30,40,50),vet_perplexidade,type="l",
xlab="Número de tópicos no modelo LDA",
ylab="Valor da perplexidade")

##Depois de olhar os valores calculados da perplexidade, faria-se escolha do
##modelo LDA com 50 tópicos.

##############################################
###Cálculo da medida da coerência dos tópicos
##############################################

w = transform(data)

for (d in 1:length(w)){
w[[d]] = w[[d]] + 1
}

for (d in 1:length(w)){
w[[d]]=na.omit(w[[d]])
}

w_unique=list()

for (d in 1:length(w)){
w_unique[[d]] = unique(w[[d]])	#palavras únicas em cada documento
}

load("aic_2t.RData")
load("aic_3t.RData")
load("aic_4t.RData")
load("aic_5t.RData")
load("aic_6t.RData")
load("aic_7t.RData")
load("aic_8t.RData")
load("aic_9t.RData")
load("aic_10t.RData")
load("aic_11t.RData")
load("aic_12t.RData")
load("aic_13t.RData")
load("aic_14t.RData")
load("aic_15t.RData")
load("aic_20t.RData")
load("aic_30t.RData")
load("aic_40t.RData")
load("aic_50t.RData")

###Funcao do cálculo da medida da coerência nos tópicos:

calc_med_coe=function(obj_cadeia,esc_top_palavras,e){

#num_doc=dim(aic_2t$theta_hat)[1]	#número de documentos
#beta_hat=aic_2t$beta_hat
#esc_top_palavras=15
#e=0.0001

num_doc=dim(obj_cadeia$theta_hat)[1]	#número de documentos
beta_hat=obj_cadeia$beta_hat

k_maiusc=dim(beta_hat)[1]
beta_hat_term_score=matrix(0,k_maiusc,dim(beta_hat)[2])

for(i in 1:k_maiusc){
for(j in 1:dim(beta_hat)[2]){

beta_hat_term_score[i,j]=beta_hat[i,j]*log(beta_hat[i,j]/(prod(beta_hat[,j])^(1/k_maiusc)))
}}

head(beta_hat_term_score)

#beta_hat_round_ts=round(beta_hat_term_score,digits=3)

#Ordenamento das palavras com 
#maior pontuacao nos tópicos

beta_hat_t_ts=t(beta_hat_term_score) #A matriz transposta

vet_palavras=1:nrow(beta_hat_t_ts)
#vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t_ts)){

x=cbind(vet_palavras,as.vector(beta_hat_t_ts[,i]))
#x=data.frame(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Pont. no tóp.",i))

if(i==1){
tabela_beta_ts=x
}

if(i>1){
tabela_beta_ts=cbind(tabela_beta_ts,x)
}

}

head(tabela_beta_ts)

tabela_top_palavras=tabela_beta_ts[1:esc_top_palavras,(1:k_maiusc)*2-1]

##P(w_i)

p_wi=matrix(0,k_maiusc,esc_top_palavras)

for(k in 1:k_maiusc){

for(i in 1:esc_top_palavras){

for(d in 1:num_doc){

p_wi[k,i]=p_wi[k,i]+sum(w_unique[[d]]==tabela_top_palavras[i,k])

}
}
}

p_wi=p_wi/num_doc

##Medida da coerência nos tópicos

c_uci=rep(0,k_maiusc)	#Medida da coerência

for(k in 1:k_maiusc){

#prob_conj=matrix(0,esc_top_palavras,esc_top_palavras)
PMI=rep(0,choose(esc_top_palavras,2))
x=0

for(i in 1:esc_top_palavras){
for(j in 2:esc_top_palavras){

if(i<j){

x=x+1
p_wi_wj=0

for(d in 1:num_doc){

y=0
y=y+sum(w_unique[[d]]==tabela_top_palavras[i,k])	#melhorar
y=y+sum(w_unique[[d]]==tabela_top_palavras[j,k])	#fazer identation

if(y>1){

p_wi_wj=p_wi_wj+1

}
}

PMI[x]=log(((p_wi_wj/num_doc)+e)/(p_wi[k,i]*p_wi[k,j]))

}
}
}

c_uci[k]=2/(esc_top_palavras*(esc_top_palavras-1))*sum(PMI)

}

c_uci_barra=sum(c_uci)/k_maiusc

return(list(c_uci_barra=c_uci_barra,c_uci=c_uci,PMI=PMI,p_wi=p_wi,
tabela_beta_ts=tabela_beta_ts[1:15,]))

}

###Funcao do cálculo da medida da coerência nos tópicos- versão 2:

calc_med_coe_v2=function(obj_cadeia,esc_top_palavras,e){

#num_doc=dim(aic_2t$theta_hat)[1]	#número de documentos
#beta_hat=aic_2t$beta_hat
#esc_top_palavras=15
#e=0.0001

num_doc=dim(obj_cadeia$theta_hat)[1]	#número de documentos
beta_hat=obj_cadeia$beta_hat

k_maiusc=dim(beta_hat)[1]
beta_hat_term_score=matrix(0,k_maiusc,dim(beta_hat)[2])

for(i in 1:k_maiusc){
for(j in 1:dim(beta_hat)[2]){

#beta_hat_term_score[i,j]=beta_hat[i,j]*log(beta_hat[i,j]/(prod(beta_hat[,j])^(1/k_maiusc)))
beta_hat_term_score[i,j]=beta_hat[i,j]*(log(beta_hat[i,j])-(1/k_maiusc)*sum(log(beta_hat[,j])))

}}

head(beta_hat_term_score)

#beta_hat_round_ts=round(beta_hat_term_score,digits=3)

#Ordenamento das palavras com 
#maior pontuacao nos tópicos

beta_hat_t_ts=t(beta_hat_term_score) #A matriz transposta

vet_palavras=1:nrow(beta_hat_t_ts)
#vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t_ts)){

x=cbind(vet_palavras,as.vector(beta_hat_t_ts[,i]))
#x=data.frame(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Pont. no tóp.",i))

if(i==1){
tabela_beta_ts=x
}

if(i>1){
tabela_beta_ts=cbind(tabela_beta_ts,x)
}

}

head(tabela_beta_ts)

tabela_top_palavras=tabela_beta_ts[1:esc_top_palavras,(1:k_maiusc)*2-1]

##P(w_i)

p_wi=matrix(0,k_maiusc,esc_top_palavras)

for(k in 1:k_maiusc){

for(i in 1:esc_top_palavras){

for(d in 1:num_doc){

p_wi[k,i]=p_wi[k,i]+sum(w_unique[[d]]==tabela_top_palavras[i,k])

}
}
}

p_wi=p_wi/num_doc

##Medida da coerência nos tópicos

c_uci=rep(0,k_maiusc)	#Medida da coerência

for(k in 1:k_maiusc){

#prob_conj=matrix(0,esc_top_palavras,esc_top_palavras)
PMI=rep(0,choose(esc_top_palavras,2))
x=0

for(i in 1:esc_top_palavras){
for(j in 2:esc_top_palavras){

if(i<j){

x=x+1
p_wi_wj=0

for(d in 1:num_doc){

y=0
y=y+sum(w_unique[[d]]==tabela_top_palavras[i,k])	#melhorar
y=y+sum(w_unique[[d]]==tabela_top_palavras[j,k])	#fazer identation

if(y>1){

p_wi_wj=p_wi_wj+1

}
}

#PMI[x]=log(((p_wi_wj/num_doc)+e)/(p_wi[k,i]*p_wi[k,j]))
PMI[x]=log((p_wi_wj/num_doc)+e)-log(p_wi[k,i])-log(p_wi[k,j])

}
}
}

c_uci[k]=2/(esc_top_palavras*(esc_top_palavras-1))*sum(PMI)

}

c_uci_barra=sum(c_uci)/k_maiusc

return(list(c_uci_barra=c_uci_barra,c_uci=c_uci,PMI=PMI,p_wi=p_wi,
tabela_beta_ts=tabela_beta_ts[1:15,]))

}

##Cálculo da medida da coerência nos tópicos, em cada um dos modelos LDA:

med_coe_calc_2t=calc_med_coe(aic_2t,15,0.0001)
med_coe_calc_3t=calc_med_coe(aic_3t,15,0.0001)
med_coe_calc_4t=calc_med_coe(aic_4t,15,0.0001)
med_coe_calc_5t=calc_med_coe(aic_5t,15,0.0001)
med_coe_calc_6t=calc_med_coe(aic_6t,15,0.0001)
med_coe_calc_7t=calc_med_coe(aic_7t,15,0.0001)
med_coe_calc_8t=calc_med_coe(aic_8t,15,0.0001)
med_coe_calc_9t=calc_med_coe(aic_9t,15,0.0001)
med_coe_calc_10t=calc_med_coe(aic_10t,15,0.0001)
med_coe_calc_11t=calc_med_coe(aic_11t,15,0.0001)
med_coe_calc_12t=calc_med_coe(aic_12t,15,0.0001)
med_coe_calc_13t=calc_med_coe(aic_13t,15,0.0001)
med_coe_calc_14t=calc_med_coe(aic_14t,15,0.0001)
med_coe_calc_15t=calc_med_coe(aic_15t,15,0.0001)
med_coe_calc_20t=calc_med_coe(aic_20t,15,0.0001)
med_coe_calc_30t=calc_med_coe(aic_30t,15,0.0001)
med_coe_calc_40t=calc_med_coe(aic_40t,15,0.0001)
med_coe_calc_50t=calc_med_coe(aic_50t,15,0.0001)


#save(med_coe_calc_2t,file="med_coe_calc_2t.RData")
#save(med_coe_calc_3t,file="med_coe_calc_3t.RData")
#save(med_coe_calc_4t,file="med_coe_calc_4t.RData")
#save(med_coe_calc_5t,file="med_coe_calc_5t.RData")
#save(med_coe_calc_6t,file="med_coe_calc_6t.RData")
#save(med_coe_calc_7t,file="med_coe_calc_7t.RData")
#save(med_coe_calc_8t,file="med_coe_calc_8t.RData")
#save(med_coe_calc_9t,file="med_coe_calc_9t.RData")
#save(med_coe_calc_10t,file="med_coe_calc_10t.RData")
#save(med_coe_calc_11t,file="med_coe_calc_11t.RData")
#save(med_coe_calc_12t,file="med_coe_calc_12t.RData")
#save(med_coe_calc_13t,file="med_coe_calc_13t.RData")
#save(med_coe_calc_14t,file="med_coe_calc_14t.RData")
#save(med_coe_calc_15t,file="med_coe_calc_15t.RData")
#save(med_coe_calc_20t,file="med_coe_calc_20t.RData")
#save(med_coe_calc_30t,file="med_coe_calc_30t.RData")
#save(med_coe_calc_40t,file="med_coe_calc_40t.RData")
#save(med_coe_calc_50t,file="med_coe_calc_50t.RData")


load("med_coe_calc_2t.RData")
load("med_coe_calc_3t.RData")
load("med_coe_calc_4t.RData")
load("med_coe_calc_5t.RData")
load("med_coe_calc_6t.RData")
load("med_coe_calc_7t.RData")
load("med_coe_calc_8t.RData")
load("med_coe_calc_9t.RData")
load("med_coe_calc_10t.RData")
load("med_coe_calc_11t.RData")
load("med_coe_calc_12t.RData")
load("med_coe_calc_13t.RData")
load("med_coe_calc_14t.RData")
load("med_coe_calc_15t.RData")
load("med_coe_calc_20t.RData")
load("med_coe_calc_30t.RData")
load("med_coe_calc_40t.RData")
load("med_coe_calc_50t.RData")

##Olhando a tabela da pontuacao dos termos:

med_coe_calc_2t[5]
med_coe_calc_3t[5]
med_coe_calc_4t[5]
med_coe_calc_5t[5]
med_coe_calc_6t[5]
med_coe_calc_7t[5]
med_coe_calc_8t[5]
med_coe_calc_9t[5]
med_coe_calc_10t[5]
med_coe_calc_11t[5]
med_coe_calc_12t[5]
med_coe_calc_13t[5]
med_coe_calc_14t[5]
med_coe_calc_15t[5]
med_coe_calc_20t[5]
med_coe_calc_30t[5]
med_coe_calc_40t[5]
med_coe_calc_50t[5]   #tira-se o cálculo da medida da coerência nesse modelo LDA

##Amostrando os valores calculados da medida da coerência nos tópicos:

vet_med_coe=c(med_coe_calc_2t[[1]],
med_coe_calc_3t[[1]],
med_coe_calc_4t[[1]],
med_coe_calc_5t[[1]],
med_coe_calc_6t[[1]],
med_coe_calc_7t[[1]],
med_coe_calc_8t[[1]],
med_coe_calc_9t[[1]],
med_coe_calc_10t[[1]],
med_coe_calc_11t[[1]],
med_coe_calc_12t[[1]],
med_coe_calc_13t[[1]],
med_coe_calc_14t[[1]],
med_coe_calc_15t[[1]],
med_coe_calc_20t[[1]],
med_coe_calc_30t[[1]],
med_coe_calc_40t[[1]])

tabela_med_coe=cbind(c(2:15,20,30,40),vet_med_coe)
colnames(tabela_med_coe)=c("Número de tópicos","Valor da medida de coerência")
tabela_med_coe

plot(c(2:15,20,30,40),vet_med_coe,type="l",xlab="Número de tópicos no modelo LDA",
ylab="Valor da medida de coerência")

##Cálculo da medida da coerência nos tópicos, em cada um dos modelos LDA
##com a versao 2 da funcao:

med_coe_calc_v2_2t=calc_med_coe_v2(aic_2t,15,0.0001)
med_coe_calc_v2_3t=calc_med_coe_v2(aic_3t,15,0.0001)
med_coe_calc_v2_4t=calc_med_coe_v2(aic_4t,15,0.0001)
med_coe_calc_v2_5t=calc_med_coe_v2(aic_5t,15,0.0001)
med_coe_calc_v2_6t=calc_med_coe_v2(aic_6t,15,0.0001)
med_coe_calc_v2_7t=calc_med_coe_v2(aic_7t,15,0.0001)
med_coe_calc_v2_8t=calc_med_coe_v2(aic_8t,15,0.0001)
med_coe_calc_v2_9t=calc_med_coe_v2(aic_9t,15,0.0001)
med_coe_calc_v2_10t=calc_med_coe_v2(aic_10t,15,0.0001)
med_coe_calc_v2_11t=calc_med_coe_v2(aic_11t,15,0.0001)
med_coe_calc_v2_12t=calc_med_coe_v2(aic_12t,15,0.0001)
med_coe_calc_v2_13t=calc_med_coe_v2(aic_13t,15,0.0001)
med_coe_calc_v2_14t=calc_med_coe_v2(aic_14t,15,0.0001)
med_coe_calc_v2_15t=calc_med_coe_v2(aic_15t,15,0.0001)
med_coe_calc_v2_20t=calc_med_coe_v2(aic_20t,15,0.0001)
med_coe_calc_v2_30t=calc_med_coe_v2(aic_30t,15,0.0001)
med_coe_calc_v2_40t=calc_med_coe_v2(aic_40t,15,0.0001)
med_coe_calc_v2_50t=calc_med_coe_v2(aic_50t,15,0.0001)

#save(med_coe_calc_v2_2t,file="med_coe_calc_v2_2t.RData")
#save(med_coe_calc_v2_3t,file="med_coe_calc_v2_3t.RData")
#save(med_coe_calc_v2_4t,file="med_coe_calc_v2_4t.RData")
#save(med_coe_calc_v2_5t,file="med_coe_calc_v2_5t.RData")
#save(med_coe_calc_v2_6t,file="med_coe_calc_v2_6t.RData")
#save(med_coe_calc_v2_7t,file="med_coe_calc_v2_7t.RData")
#save(med_coe_calc_v2_8t,file="med_coe_calc_v2_8t.RData")
#save(med_coe_calc_v2_9t,file="med_coe_calc_v2_9t.RData")
#save(med_coe_calc_v2_10t,file="med_coe_calc_v2_10t.RData")
#save(med_coe_calc_v2_11t,file="med_coe_calc_v2_11t.RData")
#save(med_coe_calc_v2_12t,file="med_coe_calc_v2_12t.RData")
#save(med_coe_calc_v2_13t,file="med_coe_calc_v2_13t.RData")
#save(med_coe_calc_v2_14t,file="med_coe_calc_v2_14t.RData")
#save(med_coe_calc_v2_15t,file="med_coe_calc_v2_15t.RData")
#save(med_coe_calc_v2_20t,file="med_coe_calc_v2_20t.RData")
#save(med_coe_calc_v2_30t,file="med_coe_calc_v2_30t.RData")
#save(med_coe_calc_v2_40t,file="med_coe_calc_v2_40t.RData")
#save(med_coe_calc_v2_50t,file="med_coe_calc_v2_50t.RData")

load("med_coe_calc_v2_2t.RData")
load("med_coe_calc_v2_3t.RData")
load("med_coe_calc_v2_4t.RData")
load("med_coe_calc_v2_5t.RData")
load("med_coe_calc_v2_6t.RData")
load("med_coe_calc_v2_7t.RData")
load("med_coe_calc_v2_8t.RData")
load("med_coe_calc_v2_9t.RData")
load("med_coe_calc_v2_10t.RData")
load("med_coe_calc_v2_11t.RData")
load("med_coe_calc_v2_12t.RData")
load("med_coe_calc_v2_13t.RData")
load("med_coe_calc_v2_14t.RData")
load("med_coe_calc_v2_15t.RData")
load("med_coe_calc_v2_20t.RData")
load("med_coe_calc_v2_30t.RData")
load("med_coe_calc_v2_40t.RData")
load("med_coe_calc_v2_50t.RData")

##Olhando a tabela da pontuacao dos termos:

med_coe_calc_v2_2t[5]
med_coe_calc_v2_3t[5]
med_coe_calc_v2_4t[5]
med_coe_calc_v2_5t[5]
med_coe_calc_v2_6t[5]
med_coe_calc_v2_7t[5]
med_coe_calc_v2_8t[5]
med_coe_calc_v2_9t[5]
med_coe_calc_v2_10t[5]
med_coe_calc_v2_11t[5]
med_coe_calc_v2_12t[5]
med_coe_calc_v2_13t[5]
med_coe_calc_v2_14t[5]
med_coe_calc_v2_15t[5]
med_coe_calc_v2_20t[5]
med_coe_calc_v2_30t[5]
med_coe_calc_v2_40t[5]
med_coe_calc_v2_50t[5]

##Amostrando os valores calculados da medida da coerência nos tópicos:

vet_med_coe_v2=c(med_coe_calc_v2_2t[[1]],
med_coe_calc_v2_3t[[1]],
med_coe_calc_v2_4t[[1]],
med_coe_calc_v2_5t[[1]],
med_coe_calc_v2_6t[[1]],
med_coe_calc_v2_7t[[1]],
med_coe_calc_v2_8t[[1]],
med_coe_calc_v2_9t[[1]],
med_coe_calc_v2_10t[[1]],
med_coe_calc_v2_11t[[1]],
med_coe_calc_v2_12t[[1]],
med_coe_calc_v2_13t[[1]],
med_coe_calc_v2_14t[[1]],
med_coe_calc_v2_15t[[1]],
med_coe_calc_v2_20t[[1]],
med_coe_calc_v2_30t[[1]],
med_coe_calc_v2_40t[[1]],
med_coe_calc_v2_50t[[1]])

tabela_med_coe_v2=cbind(c(2:15,20,30,40,50),vet_med_coe_v2)
colnames(tabela_med_coe_v2)=c("Número de tópicos",
"Valor da medida de coerência com a função 2")
tabela_med_coe_v2

plot(c(2:15,20,30,40,50),vet_med_coe_v2,type="l",
xlab="Número de tópicos no modelo LDA",
ylab="Valor da medida de coerência com a função 2")

##Depois de olhar os valores, faria-se escolha do modelo LDA com 40 tópicos.

##Comparacao dos gráficos dos valores cálculados das funcoes da medida de coerência:

par(mfrow = c(1, 2))

plot(c(2:15,20,30,40),vet_med_coe,type="l",xlab="Número de tópicos no modelo LDA",
ylab="Valor da medida de coerência")

plot(c(2:15,20,30,40,50),vet_med_coe_v2,type="l",
xlab="Número de tópicos no modelo LDA",
ylab="Valor da medida de coerência com a função 2")

##Olhando os gráficos de caixa, dos valores calculados da medida de coerência,
##com a funcao 2:

boxplot(cbind(med_coe_calc_v2_2t[[2]],
med_coe_calc_v2_3t[[2]],
med_coe_calc_v2_4t[[2]],
med_coe_calc_v2_5t[[2]],
med_coe_calc_v2_6t[[2]],
med_coe_calc_v2_7t[[2]],
med_coe_calc_v2_8t[[2]],
med_coe_calc_v2_9t[[2]],
med_coe_calc_v2_10t[[2]],
med_coe_calc_v2_11t[[2]],
med_coe_calc_v2_12t[[2]],
med_coe_calc_v2_13t[[2]],
med_coe_calc_v2_14t[[2]],
med_coe_calc_v2_15t[[2]],
med_coe_calc_v2_20t[[2]],
med_coe_calc_v2_30t[[2]],
med_coe_calc_v2_40t[[2]],
med_coe_calc_v2_50t[[2]]),
use.cols=T,names=c(2:15,20,30,40,50),xlab="Número de tópicos no modelo LDA",
ylab="Valor da medida de coerência com a função 2",
main="Gráficos de caixa dos valores calculados
da medida de coerência, com a função 2")

#########################################
####Escolha subjetiva do melhor modelo
#########################################

####Olhando a matriz beta, dos modelos com k= 15, 20, 30 e 50, para interpretacao
####dos tópicos:

##k=15:

load("chain_10000_15.RData")

beta_hat=apply(X=chain_10000_15$beta,FUN=mean,MARGIN=c(1,2))
#beta_hat=aic_15t$beta_hat #Método alternativo

head(beta_hat)

#Visualizando um tópico, pontuacao do termo no beta_hat:

k_maiusc=dim(beta_hat)[1]
beta_hat_term_score=matrix(0,k_maiusc,dim(beta_hat)[2])

for(i in 1:k_maiusc){
for(j in 1:dim(beta_hat)[2]){

beta_hat_term_score[i,j]=beta_hat[i,j]*log(beta_hat[i,j]/(prod(beta_hat[,j])^(1/k_maiusc)))
}}

head(beta_hat_term_score)

beta_hat_round_ts=round(beta_hat_term_score,digits=3)

#Ordenamento das palavras com 
#maior pontuacao nos tópicos

beta_hat_t_ts=t(beta_hat_round_ts) #A matriz transposta

#vet_palavras=1:nrow(beta_hat_t)
vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t_ts)){

#x=cbind(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=data.frame(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Pont. no tóp.",i))

if(i==1){
tabela_beta_ts=x
}

if(i>1){
tabela_beta_ts=cbind(tabela_beta_ts,x)
}

}

head(tabela_beta_ts)

#Gerar tabela no Excel:

csv_path  = 'tabela_beta_ts_15.csv'
write.csv(tabela_beta_ts, csv_path, row.names = FALSE)

##############################################################

##k=20:

load("chain_10000_20.RData")

beta_hat=apply(X=chain_10000_20$beta,FUN=mean,MARGIN=c(1,2))
#beta_hat=aic_20t$beta_hat #Método alternativo

head(beta_hat)

#Visualizando um tópico, pontuacao do termo no beta_hat:

k_maiusc=dim(beta_hat)[1]
beta_hat_term_score=matrix(0,k_maiusc,dim(beta_hat)[2])

for(i in 1:k_maiusc){
for(j in 1:dim(beta_hat)[2]){

beta_hat_term_score[i,j]=beta_hat[i,j]*log(beta_hat[i,j]/(prod(beta_hat[,j])^(1/k_maiusc)))
}}

head(beta_hat_term_score)

beta_hat_round_ts=round(beta_hat_term_score,digits=3)

#Ordenamento das palavras com 
#maior pontuacao nos tópicos

beta_hat_t_ts=t(beta_hat_round_ts) #A matriz transposta

#vet_palavras=1:nrow(beta_hat_t)
vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t_ts)){

#x=cbind(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=data.frame(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Pont. no tóp.",i))

if(i==1){
tabela_beta_ts=x
}

if(i>1){
tabela_beta_ts=cbind(tabela_beta_ts,x)
}

}

head(tabela_beta_ts)

#Gerar tabela no Excel:

csv_path  = 'tabela_beta_ts_20.csv'
write.csv(tabela_beta_ts, csv_path, row.names = FALSE)

##############################################################

##k=30:

load("chain_10000_30.RData")

beta_hat=apply(X=chain_10000_30$beta,FUN=mean,MARGIN=c(1,2))
#beta_hat=aic_30t$beta_hat #Método alternativo

head(beta_hat)

#Visualizando um tópico, pontuacao do termo no beta_hat:

k_maiusc=dim(beta_hat)[1]
beta_hat_term_score=matrix(0,k_maiusc,dim(beta_hat)[2])

for(i in 1:k_maiusc){
for(j in 1:dim(beta_hat)[2]){

beta_hat_term_score[i,j]=beta_hat[i,j]*log(beta_hat[i,j]/(prod(beta_hat[,j])^(1/k_maiusc)))
}}

head(beta_hat_term_score)

beta_hat_round_ts=round(beta_hat_term_score,digits=3)

#Ordenamento das palavras com 
#maior pontuacao nos tópicos

beta_hat_t_ts=t(beta_hat_round_ts) #A matriz transposta

#vet_palavras=1:nrow(beta_hat_t)
vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t_ts)){

#x=cbind(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=data.frame(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Pont. no tóp.",i))

if(i==1){
tabela_beta_ts=x
}

if(i>1){
tabela_beta_ts=cbind(tabela_beta_ts,x)
}

}

head(tabela_beta_ts)

#Gerar tabela no Excel:

csv_path  = 'tabela_beta_ts_30.csv'
write.csv(tabela_beta_ts, csv_path, row.names = FALSE)

##############################################

##k=50:

load("chain_10000_50.RData")

beta_hat=apply(X=chain_10000_50$beta,FUN=mean,MARGIN=c(1,2))
#beta_hat=aic_50t$beta_hat #Método alternativo

head(beta_hat)

#Visualizando um tópico, pontuacao do termo no beta_hat:

k_maiusc=dim(beta_hat)[1]
beta_hat_term_score=matrix(0,k_maiusc,dim(beta_hat)[2])

for(i in 1:k_maiusc){
for(j in 1:dim(beta_hat)[2]){

beta_hat_term_score[i,j]=beta_hat[i,j]*log(beta_hat[i,j]/(prod(beta_hat[,j])^(1/k_maiusc)))
}}

head(beta_hat_term_score)

beta_hat_round_ts=round(beta_hat_term_score,digits=3)

#Ordenamento das palavras com 
#maior pontuacao nos tópicos

beta_hat_t_ts=t(beta_hat_round_ts) #A matriz transposta

#vet_palavras=1:nrow(beta_hat_t)
vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t_ts)){

#x=cbind(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=data.frame(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Pont. no tóp.",i))

if(i==1){
tabela_beta_ts=x
}

if(i>1){
tabela_beta_ts=cbind(tabela_beta_ts,x)
}

}

head(tabela_beta_ts)

#Gerar tabela no Excel:

csv_path  = 'tabela_beta_ts_50.csv'
write.csv(tabela_beta_ts, csv_path, row.names = FALSE)

##tem valores "Inf"

########################################

##k=40:

load("chain_10000_40.RData")

beta_hat=apply(X=chain_10000_40$beta,FUN=mean,MARGIN=c(1,2))
#beta_hat=aic_40t$beta_hat #Método alternativo

head(beta_hat)

#Visualizando um tópico, pontuacao do termo no beta_hat:

k_maiusc=dim(beta_hat)[1]
beta_hat_term_score=matrix(0,k_maiusc,dim(beta_hat)[2])

for(i in 1:k_maiusc){
for(j in 1:dim(beta_hat)[2]){

beta_hat_term_score[i,j]=beta_hat[i,j]*log(beta_hat[i,j]/(prod(beta_hat[,j])^(1/k_maiusc)))
}}

head(beta_hat_term_score)

beta_hat_round_ts=round(beta_hat_term_score,digits=3)

#Ordenamento das palavras com 
#maior pontuacao nos tópicos

beta_hat_t_ts=t(beta_hat_round_ts) #A matriz transposta

#vet_palavras=1:nrow(beta_hat_t)
vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t_ts)){

#x=cbind(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=data.frame(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Pont. no tóp.",i))

if(i==1){
tabela_beta_ts=x
}

if(i>1){
tabela_beta_ts=cbind(tabela_beta_ts,x)
}

}

head(tabela_beta_ts)

#Gerar tabela no Excel:

csv_path  = 'tabela_beta_ts_40.csv'
write.csv(tabela_beta_ts, csv_path, row.names = FALSE)

##################################################################
#################################################################
####Análisis exploratório de theta e beta- estimativas pontuais:
#################################################################
##################################################################

##Para o modelo com 40 tópicos:

load("chain_10000_40.RData") #Carregando a cadeia com 40 tópicos

names(chain_10000_40)
str(chain_10000_40)

##theta

dim(chain_10000_40$theta)

#plot(chain_10000_40$theta[1,1,],type="l")
#plot(chain_10000_40$theta[2,5,],type="l")
#plot(chain_10000_40$theta[300,9,],type="l")

#mean(chain_10000_40$theta[300,10,])
#mean(chain_10000_40$theta[300,10,-(1:500)]) #já resolvido no código .cpp

theta_hat=apply(X=chain_10000_40$theta,FUN=mean,MARGIN=c(1,2))
#load("aic_40t.RData")
#theta_hat=aic_40t$theta_hat    #método alterno para obter theta hat
head(theta_hat)

theta_hat_round=round(theta_hat,digits=3)

#Ordenamento dos tópicos com 
#maior probabilidade nos documentos

theta_hat_t=t(theta_hat_round) #A matriz transposta
#save(theta_hat_t,file="theta_hat_t.RData")
#load("theta_hat_t.RData")

vet_topicos=1:dim(chain_10000_40$beta)[1]

x=0 #Variavel auxiliar

for(i in 1:ncol(theta_hat_t)){

x=cbind(vet_topicos,as.vector(theta_hat_t[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Tópico",paste("Prob. no doc.",i))

if(i==1){
tabela_theta=x
}

if(i>1){
tabela_theta=cbind(tabela_theta,x)
}

}

head(tabela_theta)
#save(tabela_theta,file="tabela_theta_40t.RData")
#load("tabela_theta_40t.RData")

##Tabela do theta, para apresentacao no documento, com os melhores 3 tópicos:

tabela_theta_3t=tabela_theta[1:3,]
tabela_theta_3t=t(tabela_theta_3t)

x=tabela_theta_3t[seq(1,dim(tabela_theta_3t)[1],by=2),] #primeiros tres tópicos
y=tabela_theta_3t[seq(2,dim(tabela_theta_3t)[1],by=2),] #probabilidades dos primeiros tres tópicos

tabela_theta_final=matrix(0,dim(tabela_theta_3t)[1]/2,3)

for(i in 1:nrow(tabela_theta_final)){
for(j in 1:ncol(tabela_theta_final)){

tabela_theta_final[i,j]=paste0(x[i,j]," (",y[i,j],")")

}}

tabela_theta_apres=data.frame(1:nrow(tabela_theta_final),tabela_theta_final) #Tabela de apresentacao das probabilidades dos tópicos nos documentos

colnames(tabela_theta_apres)=c("No. do documento","1° tópico (probabilidade)",
"2° tópico (probabilidade)","3° tópico (probabilidade)")

tabela_theta_apres

#save(tabela_theta_apres,file="tabela_theta_apres_40t.RData")
#load("tabela_theta_apres_40t.RData")

##Gerar tabela no latex:
require(stargazer)

stargazer(tabela_theta_apres)

##Gerar tabela no Excel:

#csv_path  = 'tabela_theta_apres_40t.csv'
#write.csv(tabela_theta_apres, csv_path, row.names = FALSE)

#csv_path  = 'tabela_theta_40t.csv'
#write.csv(tabela_theta, csv_path, row.names = FALSE)

csv_path  = 'tabela_theta_apres_40t.csv'
write.csv(tabela_theta_apres, csv_path, row.names = FALSE)

##beta

dim(chain_10000_40$beta)

#plot(chain_10000_40$beta[1,1,],type="l")
#plot(chain_10000_40$beta[2,15,],type="l")
#plot(chain_10000_40$beta[9,100,],type="l")

#mean(chain_10000_40$beta[3,10,])
#mean(chain_10000_40$beta[3,10,-(1:500)]) #já foi resolvido no código .cpp

beta_hat=apply(X=chain_10000_40$beta,FUN=mean,MARGIN=c(1,2))
#load("aic_40t.RData")
#beta_hat=aic_40t$beta_hat    #método alterno para obter beta hat
head(beta_hat)
#save(beta_hat,file="beta_hat.RData")
#load("beta_hat.RData")

beta_hat_round=round(beta_hat,digits=3)

#Ordenamento das palavras com 
#maior probabilidade nos tópicos

beta_hat_t=t(beta_hat_round) #A matriz transposta

#vet_palavras=1:nrow(beta_hat_t)
vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t)){

#x=cbind(vet_palavras,as.vector(beta_hat_t[,i]))
x=data.frame(vet_palavras,as.vector(beta_hat_t[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Prob. no tóp.",i))

if(i==1){
tabela_beta=x
}

if(i>1){
tabela_beta=cbind(tabela_beta,x)
}

}

head(tabela_beta)

#save(tabela_beta,file="tabela_beta.RData")
#load("tabela_beta.RData")

#Gerar tabela no latex:

stargazer(tabela_beta[1:10,],summary=F,rownames=F)

#Gerar tabela no Excel:

csv_path  = 'tabela_beta.csv'
write.csv(tabela_beta, csv_path, row.names = FALSE)

########
##Visualizando um tópico, pontuacao do termo no beta_hat:

k_maiusc=dim(beta_hat)[1]
beta_hat_term_score=matrix(0,k_maiusc,dim(beta_hat)[2])

for(i in 1:k_maiusc){
for(j in 1:dim(beta_hat)[2]){

beta_hat_term_score[i,j]=beta_hat[i,j]*(log(beta_hat[i,j])-(1/k_maiusc)*sum(log(beta_hat[,j])))

}}

head(beta_hat_term_score)

beta_hat_round_ts=round(beta_hat_term_score,digits=3)

#Ordenamento das palavras com 
#maior pontuacao nos tópicos

beta_hat_t_ts=t(beta_hat_round_ts) #A matriz transposta

#vet_palavras=1:nrow(beta_hat_t)
vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t_ts)){

#x=cbind(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=data.frame(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Pont. no tóp.",i))

if(i==1){
tabela_beta_ts=x
}

if(i>1){
tabela_beta_ts=cbind(tabela_beta_ts,x)
}

}

head(tabela_beta_ts)

#save(tabela_beta_ts,file="tabela_beta_ts_40t.RData")
#load("tabela_beta_ts_40t.RData")

#Gerar tabela no latex:

stargazer(tabela_beta_ts[1:10,],summary=F,rownames=F)

#Gerar tabela no Excel:

csv_path  = 'tabela_beta_ts_40.csv'
write.csv(tabela_beta_ts, csv_path, row.names = FALSE)

#######
##Encontrando documentos semelhantes:

d_maiusc=dim(theta_hat)[1]

doc_sim=matrix(0,d_maiusc,d_maiusc)

for(i in 1:d_maiusc){
for(j in 1:d_maiusc){

doc_sim[i,j]=sum((sqrt(theta_hat[i,])-sqrt(theta_hat[j,]))^2)

}}

doc_sim

#for(i in 1:dim(doc_sim)[1]){

#x=doc_sim[i,]/max(doc_sim[i,])
#doc_sim[i,]=x

#}

doc_sim=doc_sim/max(doc_sim)

head(doc_sim)

top10_doc_sem=function(d){

#d=1

#doc_sim_cons=apply(X=doc_sim,FUN=sum,MARGIN=1)
#doc_sim_cons=cbind(1:d_maiusc,doc_sim_cons)
doc_sim_cons=cbind(1:d_maiusc,doc_sim[,d]) #escolha do documento
doc_sim_cons=doc_sim_cons[order(doc_sim_cons[,2],decreasing=F),]

#doc_sim_cons=doc_sim_cons[-1,] #tirando a linha do mesmmo documento

##Top 10 de documentos semelhantes:

doc_sim_top10=doc_sim_cons[1:11,] #incluido o documento examinado na linha 1

doc_sim_aux=cbind(doc_sim_top10,doc_ies[doc_sim_top10[,1],])
doc_sim_aux=data.frame(doc_sim_aux,pastas[doc_sim_aux[,4]])

x=0

for(i in 1:11){
x[i]=list.files(doc_sim_aux[i,6],pattern="\\.")[doc_sim_aux[i,3]]
}

doc_sim_aux=data.frame(doc_sim_aux,x)

doc_sim_aux=data.frame(doc_sim_aux,substr(doc_sim_aux[,7],
1,nchar(doc_sim_aux[,7])-6),as.numeric(substr(doc_sim_aux[,7],
nchar(doc_sim_aux[,7])-5,nchar(doc_sim_aux[,7])-6+2)))

ementa=list.dirs("Base de Dados/",recursive=F)
x=0

for(i in 1:11){

x[i]=list.files(grep(pattern = paste0(doc_sim_aux[i,8],"-"),ementa,value = TRUE),pattern="\\.")[doc_sim_aux[i,9]]

}

doc_sim_aux=data.frame(doc_sim_aux,paste0(substr(x,1,nchar(x)-4),"- IES: ",doc_sim_aux[,8]))

doc_sim_aux[,2]=round(doc_sim_aux[,2],digits=3)

#doc_sim_top10_final=as.data.frame(doc_sim_aux[2:11,c(10,2)])
doc_sim_top10_final=data.frame(paste0(doc_sim_aux[2:11,1],"- ",
doc_sim_aux[2:11,10]),doc_sim_aux[2:11,2])

#colnames(doc_sim_top10_final)=c("10 DOCUMENTOS MAIS SIMILARES","VALOR DA SIMILARIDADE")
colnames(doc_sim_top10_final)=c("10 DOCUMENTOS MAIS SIMILARES","DISTÂNCIA DE HELLINGER")
rownames(doc_sim_top10_final)=1:10

print(paste0("Documento ",d,": ",doc_sim_aux[1,10])) #Nome do documento examinado

return(doc_sim_top10_final)

#save(doc_sim_top10_final,file="doc_sim_top10_final.RData")
#load("doc_sim_top10_final.RData")

}

##Gerar tabela no Excel:

#csv_path  = 'doc_sim_top10_final.csv'
#write.csv(doc_sim_top10_final, csv_path, row.names = TRUE)

###Principais palavras dos principais tópicos no documento "d" (por pontuação do termo):

top_pal_top_t_doc=function(d){

#d=1

topics=tabela_theta[1:3,d*2-1]
prob_t=tabela_theta[1:3,d*2]

top_pal_top_t_doc=tabela_beta_ts[1:15,topics*2-1]
colnames(top_pal_top_t_doc)=paste0("Tópico ",topics," (prob. ",prob_t,")")
rownames(top_pal_top_t_doc)=1:15

#top_pal_top_t_doc
return(top_pal_top_t_doc)

}

###Proporções de tópico esperadas no documento "d":

prop_t_esp_d=function(d){

#d=1

x=tabela_theta[,c(d*2-1,d*2)]
x=x[order(x[,1],decreasing=F),]

return(barplot(x[,2],ylim=c(0,1),names.arg=x[,1],xlab="Tópico",ylab="Probabilidade",
main=paste0("Proporções de tópico esperadas no documento ",d),cex.main=0.8))

}

###Relatório de apresentacao por documento "d":

relatorio=function(d){

return(list(top10_doc_sem(d),
top_pal_top_t_doc(d),
prop_t_esp_d(d))[-3])

}

###Mapa de Calor:

#heatmap(doc_sim,col=hcl.colors(50))
heatmap(doc_sim,Rowv=NA,Colv=NA)

##########################################################################
##########################################################################
### INFERÊNCIA VARIACIONAL
##########################################################################
##########################################################################

#rm(list=ls(all=TRUE))

require(stargazer)
require(Rcpp)
require(RcppArmadillo)
require(RcppParallel)

# reading c++ functions
sourceCpp("./Codes/vi_lda.cpp")


set.seed(200)

# number of topics
#n_topics = 20

# number of iterations in the inner loop ( loop for phi and gamma)
n_local_iter = 5		

# size of the minibatch
minibatch_size = 3

# Robins-Monroe stepsize parameters, tau is delay, kappa is forgetting rate
step_t=0; tau=1; kappa=0.8 		

# reading the data
data_iv=data.frame(data[,2],data[,1],data[,3])
names(data_iv)=c("doc","word","freq")
original_data = as.matrix(data_iv)
original_data = original_data[-( which(is.na(original_data[,2]))),]

# dimension of the data matrix
D = max( original_data[,1])
N = max( original_data[,2]) 	#deveria ser "V"

####################################################

# number of topics
n_topics = c(3:15,seq(20,50,10))

#n_topics = c(n_topics,60,70) ##Aumentando os modelos com 60 e 70 tópicos

#Vetor de tempos na Inferência Variacional

tempo_iv=list()

fit=list()

for(i in 1:length(n_topics)){
#for(i in 18:19){

# initializing gamma
gamma_init = abs( matrix(1 + rnorm( n = n_topics[i] * D, mean = 0, sd = sqrt(.1) ) , nrow = D, ncol = n_topics[i] ) )

# defining alpha (hyper parameter)
alpha = matrix( 1/n_topics[i] , nrow = D, ncol = n_topics[i] )

#D=100			###???

# defining eta (hyper parameter)
eta = matrix(0.01, nrow = n_topics[i], ncol = N )

# initializing lambda
lambda_init1 = eta + matrix( rexp( n = n_topics[i] * N, rate = 100 * 100 /( n_topics[i] * N ) ), nrow = n_topics[i], ncol = N )

#D = max( original_data[,1])

# Start the clock!
ptm <- proc.time()

fit1 = lda_vi_cpp ( original_data = original_data, 
                   n_topics = n_topics[i], 
                   n_local_iter = n_local_iter, 
                   minibatch_size = minibatch_size,
                   step_t = step_t,
                   tau = tau,
                   kappa = kappa,
                   gamma_init = gamma_init,
                   alpha = alpha,
                   eta = eta,
                   lambda_init = lambda_init1,
                   n_loops_through_data = round(20000/D))

# Stop the clock

tempo_iv[[i]]=proc.time() - ptm

fit[[i]]=fit1

}

#save(tempo_iv,file="tempo_iv.RData")
#save(fit,file="fit_iv.RData")

#save(tempo_iv,file="tempo_iv_com_60_70.RData")
#save(fit,file="fit_iv_com_60_70.RData")


#load("tempo_iv.RData")
#load("fit_iv.RData")

####################################################
#               showing results
####################################################

##################
# log likelihood 
##################

###Base###

par(mfrow = c(4, 5))
#par(mfrow = c(2, 3))

#for(i in 1:length(n_topics)){
for(i in 1:5){

#plot ( y = fit[[i]]$log_likelihood, 
#	x = seq(1, (D * round(10000/D)),by = (minibatch_size * n_local_iter))[-length(seq(1, (D * round(10000/D)),by = (minibatch_size * n_local_iter)))],
#	type = "l", cex.axis=1.5, cex.lab = 1.5, ylab = "Full likelihood", xlab = "Iteration", lwd = 2,
#	main = paste(n_topics[i]," tópicos") )

plot ( y = fit[[i]]$log_likelihood, 
	x = seq(1, (D * round(10000/D)),by = D),
	type = "l", cex.axis=1.5, cex.lab = 1.5, ylab = "Full likelihood", xlab = "Iteration", lwd = 2,
	main = paste(n_topics[i]," tópicos"))#, ylim = c(min(fit[[i]]$log_likelihood),0) )

}

###Grade###

par(mfrow = c(4, 5))

for(i in 1:length(n_topics)){

plot ( y = fit[[i]]$log_likelihood, 
	x = seq(1, (D * round(20000/D)),by = D),
	type = "l", cex.axis=1.5, cex.lab = 1.5, ylab = "Full likelihood", xlab = "Iteration", lwd = 2,
	main = paste(n_topics[i]," tópicos"))

}

###Grade com limites###

min_lv=c()	# Mínimo da logverossimilhanca
max_lv=c()	# Máximo da logverossimilhanca

for(i in 1:length(n_topics)){

min_lv[i]=min(fit[[i]]$log_likelihood)
max_lv[i]=max(fit[[i]]$log_likelihood)

}

par(mfrow = c(4, 5))

for(i in 1:length(n_topics)){

plot ( y = fit[[i]]$log_likelihood, 
	x = seq(1, (D * round(20000/D)),by = D),
	type = "l", cex.axis=1.5, cex.lab = 1.5, ylab = "Full likelihood", xlab = "Iteration", lwd = 2,
	main = paste(n_topics[i]," tópicos"), ylim = c(min(min_lv),max(max_lv)) )

}

###Gráfico com linhas###

min_lv=c()	# Mínimo da logverossimilhanca
max_lv=c()	# Máximo da logverossimilhanca
vet_leg=c()	# Vetor de nomes para a lenda

for(i in 1:length(n_topics)){

min_lv[i]=min(fit[[i]]$log_likelihood)
max_lv[i]=max(fit[[i]]$log_likelihood)
vet_leg[i]=paste0(n_topics[i]," tópicos")

}

#pdf("gráfico.pdf")

par(mar = c(par("mar")[-4],6), xpd=T)

plot ( y = fit[[1]]$log_likelihood, 
	x = seq(1, (D * round(20000/D)),by = D),
	type = "l", cex.axis=1.5, cex.lab = 1.5, ylab = "Full likelihood", xlab = "Iteration", lwd = 2,
	ylim = c(min(min_lv),max(max_lv)) )

for(i in 2:length(n_topics)){

lines( y = fit[[i]]$log_likelihood, x = seq(1, (D * round(20000/D)),by = D), lty = i, lwd = 2 , col = i )

}

legend("topright", inset = c(-0.25,0), title="No. tópicos", legend = as.character(n_topics), col = 1:17, lty = 1:17)

#dev.off()

###Gráfico com linhas, mudando os índices das iteracoes à 6672###

min_lv=c()	# Mínimo da logverossimilhanca
max_lv=c()	# Máximo da logverossimilhanca
vet_leg=c()	# Vetor de nomes para a lenda
t_linhas=c(rep(1,8),rep(3,8),2)
cor_linhas=c(seq(1,8,1),seq(1,8,1),1)

for(i in 1:length(n_topics)){

min_lv[i]=min(fit[[i]]$log_likelihood)
max_lv[i]=max(fit[[i]]$log_likelihood)
vet_leg[i]=paste0(n_topics[i]," tópicos")

}

#pdf("gráfico.pdf",width = 6 )

par(mar = c(par("mar")[-4],6), xpd=T)

plot ( y = fit[[1]]$log_likelihood, 
	x = seq(D/minibatch_size,6672,by = D/minibatch_size),
	type = "l", ylab = "Log-verossimilhança", xlab = "Iteração", lwd = 3,
	ylim = c(min(min_lv),max(max_lv)))# , cex.axis=1.5, cex.lab = 1.5)

for(i in 2:length(n_topics)){

lines( y = fit[[i]]$log_likelihood,
x = seq(D/minibatch_size,6672,by = D/minibatch_size), lty = t_linhas[i], 
lwd = 3 , col = cor_linhas[i] )

}

legend("topright", inset = c(-0.25,0), title="No. tópicos", 
legend = as.character(n_topics), col = cor_linhas, lty = t_linhas, lwd = 3)

#dev.off()

###Tabela da log-verossimilhanca na última iteracao

vet_ult_lv=c()

for(i in 1:length(n_topics)){

vet_ult_lv[i]=fit[[i]]$log_likelihood[1,48]

}

tab_log_ver_iv=data.frame(paste0("Modelo LDA com ",n_topics," tópicos"),vet_ult_lv)
colnames(tab_log_ver_iv)=c("","VALOR DA LOG-VEROSSIMILHANCA")
tab_log_ver_iv

############################################################################
############################################################################

# reading the vocabulary
actual_words = BD_Final_uni[,1]

ementa_theta_hat=list()
ementa_beta_hat=list()
ementa_beta_hat_ts=list()
ementa_frequent_words_beta_hat=list()
ementa_frequent_words_score=list()

for(i in 1:length(n_topics)){
#for(i in 15:15){  #a coordenada 15 é o K=30
#i=15

gamma = fit[[i]]$gamma
lambda = fit[[i]]$lambda

# theta hat
#############

# topic proportions per document

### Nao correr- obtem-se valores negativos###
#theta_hat = matrix( 0, nrow = D, ncol = n_topics[i] )

## estimating theta by its mode
#for ( d in 1:D){
#  theta_hat[d, ] = ( gamma[d, ] - 1 ) / ( sum( gamma[d, ] ) - n_topics[i] ) #??? estimativa pontoal (a moda)
#}
### Nao correr até aquí ###

theta_hat = matrix( 0, nrow = D, ncol = n_topics[i] )

# estimating theta by its média
for ( d in 1:D){
  theta_hat[d, ] = gamma[d, ] / sum( gamma[d, ] )
}

# beta hat
############

# words distribution per topic
beta_hat = matrix(0,nrow = n_topics[i], ncol = N )

# estimating beta by its mean
for ( k in 1:n_topics[i]){
  beta_hat[k, ] = lambda[k, ] / sum( lambda[k, ] )	#???
}

# score   #??? Olhar se é a Pontoacao do Termo
score_matrix = matrix(0, nrow = n_topics[i], ncol = N)
for (k in 1:n_topics[i]){
  for (n in 1:N){
    score_matrix[k, n] = beta_hat[k, n] * log( beta_hat[k,n] ) - beta_hat[k,n] * mean( log( beta_hat[ ,n] ) )
  }
}

# most frequent words in each topic
#n_freq = 10
n_freq = 15

# using beta_hat as point estimate
frequent_words_beta_hat = matrix(0, ncol = n_topics[i] * 2, nrow = n_freq)
frequent_words_beta_hat = as.data.frame(frequent_words_beta_hat)
for (j in 1:n_topics[i] ){
  frequent_words_beta_hat[, j*2-1] = actual_words[ order( beta_hat[j, ], decreasing = TRUE )[1:n_freq] ]
  frequent_words_beta_hat[, j*2] = as.numeric(round(beta_hat[j,][order(beta_hat[j,],decreasing=T)][1:n_freq],3))
}
#fix( frequent_words_beta_hat )

# using scores as point estimate
frequent_words_score = matrix(0, ncol = n_topics[i] * 2, nrow = n_freq)
frequent_words_score = as.data.frame(frequent_words_score)
for (j in 1:n_topics[i] ){
  frequent_words_score[, j*2-1] = actual_words[ order( score_matrix[j, ], decreasing = TRUE )[1:n_freq] ]
  frequent_words_score[, j*2] = as.numeric(round(score_matrix[j,][order(score_matrix[j,],decreasing=T)][1:n_freq],3))
}
#fix( frequent_words_score )

vet_nom_iv=c()
for (j in 1:n_topics[i] ){
vet_nom_iv[j*2-1]="Palavra"
vet_nom_iv[j*2]=paste0("Pont. no tóp. ",j)
}

colnames(frequent_words_score)=vet_nom_iv

#save(frequent_words_score,file="tabela_beta_iv_20.RData")
#load("tabela_beta_iv_20.RData")

#csv_path  = 'tabela_beta_iv_20.csv'
#csv_path  = paste0("tabela_beta_iv_",n_topics[i],".csv")

#write.csv(frequent_words_score, csv_path, row.names = FALSE)

ementa_theta_hat[[i]]=theta_hat
ementa_beta_hat[[i]]=beta_hat
ementa_beta_hat_ts[[i]]=score_matrix
ementa_frequent_words_beta_hat[[i]]=frequent_words_beta_hat
ementa_frequent_words_score[[i]]=frequent_words_score

}

#############################################################################
#############################################################################


##### Cálculo dos tempos proporcionais dos modelos MCMC ######

#install.packages("RcppArmadillo", configure.args = "--with-arma-64bit")
#detach("package:RcppArmadillo", unload = TRUE)

tempo_mcmc=list()

for(i in 1:length(n_topics)){

ptm <- proc.time()

y=100
x=mcmc_cpp( data = as.matrix(data), w = w , K = n_topics[i], burning = y*0.05, n_iter = y, save_it = 5 )

# Stop the clock

tempo_mcmc[[i]]=proc.time() - ptm

}

#save(tempo_mcmc,file="tempo_mcmc.RData")
#load("tempo_mcmc.RData")

##############

tempo_mcmc_prop=list()

for(i in 1:length(n_topics)){

tempo_mcmc_prop[[i]]=tempo_mcmc[[i]]*100

}

# Comparacao de tempos computacionais MCMC e Inferência Variacional

tab_tempos=matrix(0,length(n_topics),2)

for(i in 1:length(n_topics)){

tab_tempos[i,]=c(tempo_mcmc_prop[[i]][[3]],tempo_iv[[i]][[3]]/2)	#Os tempos da IV sao divididos em 2, porque tem 20000 iteracoes

}

colnames(tab_tempos)=c("MCMC","Inferência Variacional")
rownames(tab_tempos)=n_topics

tab_tempos

##Em inferência variacional se tem 6672 iteracoees e vai-se pasar à 10000

tab_tempos2=cbind(tab_tempos,as.vector(tab_tempos[,2]*10000/6672))


####################################################
# Escolha do melhor modelo na Inferência Variacional
####################################################

###Medida da coerência

w = transform(data)

for (d in 1:length(w)){
w[[d]] = w[[d]] + 1
}

for (d in 1:length(w)){
w[[d]]=na.omit(w[[d]])
}

w_unique=list()

for (d in 1:length(w)){
w_unique[[d]] = unique(w[[d]])	#palavras únicas em cada documento
}

###Funcao do cálculo da medida da coerência nos tópicos- versão 2:

calc_med_coe_iv_v2=function(obj_theta,obj_beta,esc_top_palavras,e){

#num_doc=dim(aic_2t$theta_hat)[1]	#número de documentos
#beta_hat=aic_2t$beta_hat
#esc_top_palavras=15
#e=0.0001

num_doc=dim(obj_theta)[1]	#número de documentos
beta_hat=obj_beta

k_maiusc=dim(beta_hat)[1]
beta_hat_term_score=matrix(0,k_maiusc,dim(beta_hat)[2])

for(i in 1:k_maiusc){
for(j in 1:dim(beta_hat)[2]){

#beta_hat_term_score[i,j]=beta_hat[i,j]*log(beta_hat[i,j]/(prod(beta_hat[,j])^(1/k_maiusc)))
beta_hat_term_score[i,j]=beta_hat[i,j]*(log(beta_hat[i,j])-(1/k_maiusc)*sum(log(beta_hat[,j])))

}}

head(beta_hat_term_score)

#beta_hat_round_ts=round(beta_hat_term_score,digits=3)

#Ordenamento das palavras com 
#maior pontuacao nos tópicos

beta_hat_t_ts=t(beta_hat_term_score) #A matriz transposta

vet_palavras=1:nrow(beta_hat_t_ts)
#vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t_ts)){

x=cbind(vet_palavras,as.vector(beta_hat_t_ts[,i]))
#x=data.frame(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Pont. no tóp.",i))

if(i==1){
tabela_beta_ts=x
}

if(i>1){
tabela_beta_ts=cbind(tabela_beta_ts,x)
}

}

head(tabela_beta_ts)

tabela_top_palavras=tabela_beta_ts[1:esc_top_palavras,(1:k_maiusc)*2-1]

##P(w_i)

p_wi=matrix(0,k_maiusc,esc_top_palavras)

for(k in 1:k_maiusc){

for(i in 1:esc_top_palavras){

for(d in 1:num_doc){

p_wi[k,i]=p_wi[k,i]+sum(w_unique[[d]]==tabela_top_palavras[i,k])

}
}
}

p_wi=p_wi/num_doc

##Medida da coerência nos tópicos

c_uci=rep(0,k_maiusc)	#Medida da coerência

for(k in 1:k_maiusc){

#prob_conj=matrix(0,esc_top_palavras,esc_top_palavras)
PMI=rep(0,choose(esc_top_palavras,2))
x=0

for(i in 1:esc_top_palavras){
for(j in 2:esc_top_palavras){

if(i<j){

x=x+1
p_wi_wj=0

for(d in 1:num_doc){

y=0
y=y+sum(w_unique[[d]]==tabela_top_palavras[i,k])	#melhorar
y=y+sum(w_unique[[d]]==tabela_top_palavras[j,k])	#fazer identation

if(y>1){

p_wi_wj=p_wi_wj+1

}
}

#PMI[x]=log(((p_wi_wj/num_doc)+e)/(p_wi[k,i]*p_wi[k,j]))
PMI[x]=log((p_wi_wj/num_doc)+e)-log(p_wi[k,i])-log(p_wi[k,j])

}
}
}

c_uci[k]=2/(esc_top_palavras*(esc_top_palavras-1))*sum(PMI)

}

c_uci_barra=sum(c_uci)/k_maiusc

return(list(c_uci_barra=c_uci_barra,c_uci=c_uci,PMI=PMI,p_wi=p_wi,
tabela_beta_ts=tabela_beta_ts[1:15,]))

}

##Cálculo da medida da coerência nos tópicos, em cada um dos modelos LDA na Inferência Variacional:

med_coe_iv=list()

for(i in 1:length(n_topics)){
#for(i in 18:19){

med_coe_iv[[i]]=calc_med_coe_iv_v2(ementa_theta_hat[[i]],ementa_beta_hat[[i]],15,0.0001)

}

save(med_coe_iv,file="med_coe_iv.RData")
#save(med_coe_iv,file="med_coe_iv_com_60_70.RData")
#load("med_coe_iv.RData")
#load("med_coe_iv_com_60_70.RData")

##Amostrando os valores calculados da medida da coerência nos tópicos:

vet_med_coe_iv=c()

for(i in 1:length(n_topics)){

vet_med_coe_iv[i]=med_coe_iv[[i]][[1]]

}

tabela_med_coe_iv=cbind(n_topics,vet_med_coe_iv)
colnames(tabela_med_coe_iv)=c("Número de tópicos",
"Valor da medida de coerência")
tabela_med_coe_iv

#plot(c(3:15,20,30,40,50,60,70),vet_med_coe_iv,type="l",
plot(c(3:15,20,30,40,50),vet_med_coe_iv,type="l",
xlab="Número de tópicos no modelo LDA na Inferência Variacional",
ylab="Valor da medida de coerência")

##Olhando os gráficos de caixa, dos valores calculados da medida de coerência

boxplot(cbind(med_coe_iv[[1]][[2]],
med_coe_iv[[2]][[2]],
med_coe_iv[[3]][[2]],
med_coe_iv[[4]][[2]],
med_coe_iv[[5]][[2]],
med_coe_iv[[6]][[2]],
med_coe_iv[[7]][[2]],
med_coe_iv[[8]][[2]],
med_coe_iv[[9]][[2]],
med_coe_iv[[10]][[2]],
med_coe_iv[[11]][[2]],
med_coe_iv[[12]][[2]],
med_coe_iv[[13]][[2]],
med_coe_iv[[14]][[2]],
med_coe_iv[[15]][[2]],
med_coe_iv[[16]][[2]],
med_coe_iv[[17]][[2]]),
#med_coe_iv[[18]][[2]],
#med_coe_iv[[19]][[2]]),
#use.cols=T,names=c(3:15,20,30,40,50,60,70),xlab="Número de tópicos no modelo LDA",
use.cols=T,names=c(3:15,20,30,40,50),xlab="Número de tópicos no modelo LDA",
ylab="Valor da medida de coerência")
#,main="Gráficos de caixa dos valores calculados da medida de coerência na Inferência Variacional")

###Gráfico da média e da mediana da medida da coerência,
###no MCMC e a Inferência Variacional:

n_topics_mcmc=c(2:15,20,30,40,50)

vet_med_coe_mcmc_media=c()

for(i in 1:length(n_topics_mcmc)){
vet_med_coe_mcmc_media[i]=get(paste0("med_coe_calc_v2_",n_topics_mcmc[i],
"t"))$c_uci_barra
}

vet_med_coe_mcmc_media=c(vet_med_coe_mcmc_media,NA,NA)

#######

vet_med_coe_mcmc_mediana=c()

for(i in 1:length(n_topics_mcmc)){
vet_med_coe_mcmc_mediana[i]=median(get(paste0("med_coe_calc_v2_",n_topics_mcmc[i],
"t"))$c_uci)
}

vet_med_coe_mcmc_mediana=c(vet_med_coe_mcmc_mediana,NA,NA)

#######

vet_med_coe_iv_media=c()

for(i in 1:length(n_topics)){
vet_med_coe_iv_media[i]=med_coe_iv[[i]][[1]]
}

vet_med_coe_iv_media=c(NA,vet_med_coe_iv_media)

#######

vet_med_coe_iv_mediana=c()

for(i in 1:length(n_topics)){
vet_med_coe_iv_mediana[i]=median(med_coe_iv[[i]][[2]])
}

vet_med_coe_iv_mediana=c(NA,vet_med_coe_iv_mediana)

#####

pdf("Media e mediana da C_UCI- MCMC e IV.pdf")

plot(c(2:15,20,30,40,50,60,70),vet_med_coe_iv_media,type="l",
xlab="Número de tópicos no modelo LDA",
ylab="Valor da medida de coerência",
ylim = range(c(vet_med_coe_mcmc_media,vet_med_coe_mcmc_mediana,
vet_med_coe_iv_media,vet_med_coe_iv_mediana), na.rm = TRUE),lwd=2)

lines(c(2:15,20,30,40,50,60,70),vet_med_coe_iv_mediana,type="l",lty=2,col=2,lwd=2)
lines(c(2:15,20,30,40,50,60,70),vet_med_coe_mcmc_media,type="l",lty=1,col=3,lwd=2)
lines(c(2:15,20,30,40,50,60,70),vet_med_coe_mcmc_mediana,type="l",lty=2,col=4,lwd=2)

legend("topleft",                     
       legend = c("Média", "Mediana"),
       col = c(3,4),        
       lty = c(1,2),                 
       lwd = 2,                       
       title = "MCMC",cex = 0.7)
legend("bottomright",                     
       legend = c("Média", "Mediana"),
       col = c(1,2),        
       lty = c(1,2),                 
       lwd = 2,                       
       title = "Inferência Variacional",
	 cex = 0.7)

dev.off()

##############################################################################
###Inferencia, medida da similaridade e relatório, para o modelo LDA escolhido
###na inferência variacional (K=30)
##############################################################################

##Para o modelo com 30 tópicos:

#ementa_theta_hat[[i]]
#ementa_beta_hat[[i]]
#ementa_beta_hat_ts[[i]]
#ementa_frequent_words_beta_hat[[i]]
#ementa_frequent_words_score[[i]]
#load("fit_iv.RData")

##theta

theta_hat=ementa_theta_hat[[15]]

head(theta_hat)

theta_hat_round=round(theta_hat,digits=3)

#Ordenamento dos tópicos com 
#maior probabilidade nos documentos

theta_hat_t=t(theta_hat_round) #A matriz transposta
#save(theta_hat_t,file="theta_hat_t.RData")
#load("theta_hat_t.RData")

vet_topicos=1:ncol(theta_hat)

x=0 #Variavel auxiliar

for(i in 1:ncol(theta_hat_t)){

x=cbind(vet_topicos,as.vector(theta_hat_t[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Tópico",paste("Prob. no doc.",i))

if(i==1){
tabela_theta=x
}

if(i>1){
tabela_theta=cbind(tabela_theta,x)
}

}

head(tabela_theta)
#save(tabela_theta,file="tabela_theta_40t.RData")
#load("tabela_theta_40t.RData")

##Tabela do theta, para apresentacao no documento, com os melhores 3 tópicos:

tabela_theta_3t=tabela_theta[1:3,]
tabela_theta_3t=t(tabela_theta_3t)

x=tabela_theta_3t[seq(1,dim(tabela_theta_3t)[1],by=2),] #primeiros tres tópicos
y=tabela_theta_3t[seq(2,dim(tabela_theta_3t)[1],by=2),] #probabilidades dos primeiros tres tópicos

tabela_theta_final=matrix(0,dim(tabela_theta_3t)[1]/2,3)

for(i in 1:nrow(tabela_theta_final)){
for(j in 1:ncol(tabela_theta_final)){

tabela_theta_final[i,j]=paste0(x[i,j]," (",y[i,j],")")

}}

tabela_theta_apres=data.frame(1:nrow(tabela_theta_final),tabela_theta_final) #Tabela de apresentacao das probabilidades dos tópicos nos documentos

colnames(tabela_theta_apres)=c("No. do documento","1° tópico (probabilidade)",
"2° tópico (probabilidade)","3° tópico (probabilidade)")

tabela_theta_apres

#save(tabela_theta_apres,file="tabela_theta_apres_40t.RData")
#load("tabela_theta_apres_40t.RData")

##Gerar tabela no latex:
#require(stargazer)

#stargazer(tabela_theta_apres)

##Gerar tabela no Excel:

#csv_path  = 'tabela_theta_apres_40t.csv'
#write.csv(tabela_theta_apres, csv_path, row.names = FALSE)

#csv_path  = 'tabela_theta_40t.csv'
#write.csv(tabela_theta, csv_path, row.names = FALSE)

#csv_path  = 'tabela_theta_apres_40t.csv'
#write.csv(tabela_theta_apres, csv_path, row.names = FALSE)

##beta

beta_hat=ementa_beta_hat[[15]]
head(beta_hat)

beta_hat_round=round(beta_hat,digits=3)

#Ordenamento das palavras com 
#maior probabilidade nos tópicos

beta_hat_t=t(beta_hat_round) #A matriz transposta

#vet_palavras=1:nrow(beta_hat_t)
vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t)){

#x=cbind(vet_palavras,as.vector(beta_hat_t[,i]))
x=data.frame(vet_palavras,as.vector(beta_hat_t[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Prob. no tóp.",i))

if(i==1){
tabela_beta=x
}

if(i>1){
tabela_beta=cbind(tabela_beta,x)
}

}

head(tabela_beta)

#save(tabela_beta,file="tabela_beta.RData")
#load("tabela_beta.RData")

#Gerar tabela no latex:

#stargazer(tabela_beta[1:10,],summary=F,rownames=F)

#Gerar tabela no Excel:

#csv_path  = 'tabela_beta.csv'
#write.csv(tabela_beta, csv_path, row.names = FALSE)

########
##Visualizando um tópico, pontuacao do termo no beta_hat:

k_maiusc=dim(beta_hat)[1]
beta_hat_term_score=matrix(0,k_maiusc,dim(beta_hat)[2])

for(i in 1:k_maiusc){
for(j in 1:dim(beta_hat)[2]){

beta_hat_term_score[i,j]=beta_hat[i,j]*(log(beta_hat[i,j])-(1/k_maiusc)*sum(log(beta_hat[,j])))

}}

head(beta_hat_term_score)

beta_hat_round_ts=round(beta_hat_term_score,digits=3)

#Ordenamento das palavras com 
#maior pontuacao nos tópicos

beta_hat_t_ts=t(beta_hat_round_ts) #A matriz transposta

#vet_palavras=1:nrow(beta_hat_t)
vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t_ts)){

#x=cbind(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=data.frame(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Pont. no tóp.",i))

if(i==1){
tabela_beta_ts=x
}

if(i>1){
tabela_beta_ts=cbind(tabela_beta_ts,x)
}

}

head(tabela_beta_ts)

#save(tabela_beta_ts,file="tabela_beta_ts_40t.RData")
#load("tabela_beta_ts_40t.RData")

#Gerar tabela no latex:

#stargazer(tabela_beta_ts[1:10,],summary=F,rownames=F)

#Gerar tabela no Excel:

#csv_path  = 'tabela_beta_ts_40.csv'
#write.csv(tabela_beta_ts, csv_path, row.names = FALSE)

#######
##Encontrando documentos semelhantes:

d_maiusc=dim(theta_hat)[1]

doc_sim=matrix(0,d_maiusc,d_maiusc)

for(i in 1:d_maiusc){
for(j in 1:d_maiusc){

doc_sim[i,j]=sum((sqrt(theta_hat[i,])-sqrt(theta_hat[j,]))^2)

}}

doc_sim

#for(i in 1:dim(doc_sim)[1]){

#x=doc_sim[i,]/max(doc_sim[i,])
#doc_sim[i,]=x

#}

doc_sim=doc_sim/max(doc_sim)

head(doc_sim)

top10_doc_sem=function(d){

#d=1

#doc_sim_cons=apply(X=doc_sim,FUN=sum,MARGIN=1)
#doc_sim_cons=cbind(1:d_maiusc,doc_sim_cons)
doc_sim_cons=cbind(1:d_maiusc,doc_sim[,d]) #escolha do documento
doc_sim_cons=doc_sim_cons[order(doc_sim_cons[,2],decreasing=F),]

#doc_sim_cons=doc_sim_cons[-1,] #tirando a linha do mesmmo documento

##Top 10 de documentos semelhantes:

doc_sim_top10=doc_sim_cons[1:11,] #incluido o documento examinado na linha 1

doc_sim_aux=cbind(doc_sim_top10,doc_ies[doc_sim_top10[,1],])
doc_sim_aux=data.frame(doc_sim_aux,pastas[doc_sim_aux[,4]])

x=0

for(i in 1:11){
x[i]=list.files(doc_sim_aux[i,6],pattern="\\.")[doc_sim_aux[i,3]]
}

doc_sim_aux=data.frame(doc_sim_aux,x)

doc_sim_aux=data.frame(doc_sim_aux,substr(doc_sim_aux[,7],
1,nchar(doc_sim_aux[,7])-6),as.numeric(substr(doc_sim_aux[,7],
nchar(doc_sim_aux[,7])-5,nchar(doc_sim_aux[,7])-6+2)))

ementa=list.dirs("Base de Dados/",recursive=F)
x=0

for(i in 1:11){

x[i]=list.files(grep(pattern = paste0(doc_sim_aux[i,8],"-"),ementa,value = TRUE),pattern="\\.")[doc_sim_aux[i,9]]

}

doc_sim_aux=data.frame(doc_sim_aux,paste0(substr(x,1,nchar(x)-4),"- IES: ",doc_sim_aux[,8]))

doc_sim_aux[,2]=round(doc_sim_aux[,2],digits=3)

#doc_sim_top10_final=as.data.frame(doc_sim_aux[2:11,c(10,2)])
doc_sim_top10_final=data.frame(paste0(doc_sim_aux[2:11,1],"- ",
doc_sim_aux[2:11,10]),doc_sim_aux[2:11,2])

#colnames(doc_sim_top10_final)=c("10 DOCUMENTOS MAIS SIMILARES","VALOR DA SIMILARIDADE")
colnames(doc_sim_top10_final)=c("10 DOCUMENTOS MAIS SIMILARES","DISTÂNCIA DE HELLINGER")
rownames(doc_sim_top10_final)=1:10

print(paste0("Documento ",d,": ",doc_sim_aux[1,10])) #Nome do documento examinado

return(doc_sim_top10_final)

#save(doc_sim_top10_final,file="doc_sim_top10_final.RData")
#load("doc_sim_top10_final.RData")

}

##Gerar tabela no Excel:

#csv_path  = 'doc_sim_top10_final.csv'
#write.csv(doc_sim_top10_final, csv_path, row.names = TRUE)

###Principais palavras dos principais tópicos no documento "d" (por pontuação do termo):

top_pal_top_t_doc=function(d){

#d=1

topics=tabela_theta[1:3,d*2-1]
prob_t=tabela_theta[1:3,d*2]

top_pal_top_t_doc=tabela_beta_ts[1:15,topics*2-1]
colnames(top_pal_top_t_doc)=paste0("Tópico ",topics," (prob. ",prob_t,")")
rownames(top_pal_top_t_doc)=1:15

#top_pal_top_t_doc
return(top_pal_top_t_doc)

}

###Proporções de tópico esperadas no documento "d":

prop_t_esp_d=function(d){

#d=1

x=tabela_theta[,c(d*2-1,d*2)]
x=x[order(x[,1],decreasing=F),]

return(barplot(x[,2],ylim=c(0,1),names.arg=x[,1],xlab="Tópico",ylab="Probabilidade",
main=paste0("Proporções de tópico esperadas no documento ",d),cex.main=0.8))

}

###Relatório de apresentacao por documento "d":

relatorio=function(d){

return(list(top10_doc_sem(d),
top_pal_top_t_doc(d),
prop_t_esp_d(d))[-3])

}

###Mapa de Calor:

#heatmap(doc_sim,col=hcl.colors(50))
heatmap(doc_sim,Rowv=NA,Colv=NA)

##################################################################
##################################################################
######### OUTRAS SIMULACOES E IMPLEMENTACOES
##################################################################
##################################################################

####################################################
## MCMC 50000 e 500000 iteracoes
####################################################

####MCMC:

#rm(list=ls(all=TRUE))
require(gtools)
require(Rcpp)
require(RcppArmadillo)

data_mat=data.frame(BD1_1)
str(data_mat)   #Amostra a estrutura do objeto no R

data = data_mat[order(data_mat[,2], decreasing=FALSE), ]

source("./Codes/utils.R")

sourceCpp("./Codes/mcmc_function.cpp")

source("./Codes/mcmc_function.R") 

w = transform(data) 

##########################################################################
####Cálculo de MCMC para modelos com 2-9, 11-15 tópicos, e o cálculo dos
####seus criterios de informacao; depois de olhar que o melhor modelo está
####perto do modelo com 10 tópicos:

start = Sys.time()
chain_50000_2=mcmc_cpp( data = as.matrix(data), w , K = 2,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_2,file="chain_50000_2.RData")

start = Sys.time()
chain_50000_3=mcmc_cpp( data = as.matrix(data), w , K = 3,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_3,file="chain_50000_3.RData")

start = Sys.time()
chain_50000_4=mcmc_cpp( data = as.matrix(data), w , K = 4,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_4,file="chain_50000_4.RData")

start = Sys.time()
chain_50000_5=mcmc_cpp( data = as.matrix(data), w , K = 5,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_5,file="chain_50000_5.RData")

start = Sys.time()
chain_50000_6=mcmc_cpp( data = as.matrix(data), w , K = 6,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_6,file="chain_50000_6.RData")

start = Sys.time()
chain_50000_7=mcmc_cpp( data = as.matrix(data), w , K = 7,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_7,file="chain_50000_7.RData")

start = Sys.time()
chain_50000_8=mcmc_cpp( data = as.matrix(data), w , K = 8,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_8,file="chain_50000_8.RData")

start = Sys.time()
chain_50000_9=mcmc_cpp( data = as.matrix(data), w , K = 9,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_9,file="chain_50000_9.RData")

start = Sys.time()
chain_50000_11=mcmc_cpp( data = as.matrix(data), w , K = 11,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_11,file="chain_50000_11.RData")

start = Sys.time()
chain_50000_12=mcmc_cpp( data = as.matrix(data), w , K = 12,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_12,file="chain_50000_12.RData")

start = Sys.time()
chain_50000_13=mcmc_cpp( data = as.matrix(data), w , K = 13,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_13,file="chain_50000_13.RData")

start = Sys.time()
chain_50000_14=mcmc_cpp( data = as.matrix(data), w , K = 14,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_14,file="chain_50000_14.RData")

start = Sys.time()
chain_50000_15=mcmc_cpp( data = as.matrix(data), w , K = 15,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_15,file="chain_50000_15.RData")

########Escolha de 10 tópicos:
####Cadeias para 50000 iteracoes:

start = Sys.time()
chain_50000_10=mcmc_cpp( data = as.matrix(data), w , K = 10,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_10,file="chain_50000_10.RData")

########Escolha de 20 tópicos:
####Cadeias para 50000 iteracoes:

start = Sys.time()
chain_50000_20=mcmc_cpp( data = as.matrix(data), w , K = 20,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_20,file="chain_50000_20.RData")

########Escolha de 30 tópicos:
####Cadeias para 50000 iteracoes:

start = Sys.time()
chain_50000_30=mcmc_cpp( data = as.matrix(data), w , K = 30,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_30,file="chain_50000_30.RData")

########Escolha de 40 tópicos:
####Cadeias para 50000 iteracoes:

start = Sys.time()
chain_50000_40=mcmc_cpp( data = as.matrix(data), w , K = 40,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_40,file="chain_50000_40.RData")

########Escolha de 50 tópicos:
####Cadeias para 50000 iteracoes:

start = Sys.time()
chain_50000_50=mcmc_cpp( data = as.matrix(data), w , K = 50,burning = 500, n_iter = 50000, save_it = 5 )
end = Sys.time()
end - start

save(chain_50000_50,file="chain_50000_50.RData")

#################################################################
data1=data[-(which(is.na(data[,1]))),]

w = transform(data1)

#for (d in 1:length(w)){
#w[[d]]=na.omit(w[[d]])
#}

vet_k=c(2:15,20,30,40,50)
#chain_50000 = list()
#tempo_mcmc_50000 = list()

for(i in 1:length(vet_k)){
#for(i in 2:4){ #salvo-se até 5 na lista

ptm = proc.time()
chain_50000[[i]]=mcmc_cpp( data = as.matrix(data1), w , K = vet_k[i] ,burning = 3000, n_iter = 50000, save_it = 10 )
tempo_mcmc_50000[[i]] = proc.time() - ptm

}

save(chain_50000,file="chain_50000.RData")
save(tempo_mcmc_50000,file="tempo_mcmc_50000.RData")

#load("chain_50000.RData")
#load("tempo_mcmc_50000.RData")

###################################

ptm = proc.time()
chain_50000_6=mcmc_cpp( data = as.matrix(data1), w , K = 6 ,burning = 3000, n_iter = 50000, save_it = 10 )
tempo_mcmc_50000_6 = proc.time() - ptm

save(chain_50000_6,file="chain_50000_6.RData")
save(tempo_mcmc_50000_6,file="tempo_mcmc_50000_6.RData")

#load("chain_50000_6.RData")
#load("tempo_mcmc_50000_6.RData")

###

ptm = proc.time()
chain_50000_7=mcmc_cpp( data = as.matrix(data1), w , K = 7 ,burning = 3000, n_iter = 50000, save_it = 10 )
tempo_mcmc_50000_7 = proc.time() - ptm

save(chain_50000_7,file="chain_50000_7.RData")
save(tempo_mcmc_50000_7,file="tempo_mcmc_50000_7.RData")

#load("chain_50000_7.RData")
#load("tempo_mcmc_50000_7.RData")

###

ptm = proc.time()
chain_50000_8=mcmc_cpp( data = as.matrix(data1), w , K = 8 ,burning = 3000, n_iter = 50000, save_it = 10 )
tempo_mcmc_50000_8 = proc.time() - ptm

save(chain_50000_8,file="chain_50000_8.RData")
save(tempo_mcmc_50000_8,file="tempo_mcmc_50000_8.RData")

#load("chain_50000_8.RData")
#load("tempo_mcmc_50000_8.RData")

###

ptm = proc.time()
chain_50000_9=mcmc_cpp( data = as.matrix(data1), w , K = 9 ,burning = 3000, n_iter = 50000, save_it = 10 )
tempo_mcmc_50000_9 = proc.time() - ptm

save(chain_50000_9,file="chain_50000_9.RData")
save(tempo_mcmc_50000_9,file="tempo_mcmc_50000_9.RData")

#load("chain_50000_9.RData")
#load("tempo_mcmc_50000_9.RData")

###

ptm = proc.time()
chain_50000_10=mcmc_cpp( data = as.matrix(data1), w , K = 10 ,burning = 3000, n_iter = 50000, save_it = 10 )
tempo_mcmc_50000_10 = proc.time() - ptm

save(chain_50000_10,file="chain_50000_10.RData")
save(tempo_mcmc_50000_10,file="tempo_mcmc_50000_10.RData")

#load("chain_50000_10.RData")
#load("tempo_mcmc_50000_10.RData")

###pulo###

ptm = proc.time()
chain_50000_20=mcmc_cpp( data = as.matrix(data1), w , K = 20 ,burning = 3000, n_iter = 50000, save_it = 10 )
tempo_mcmc_50000_20 = proc.time() - ptm

save(chain_50000_20,file="chain_50000_20.RData")
save(tempo_mcmc_50000_20,file="tempo_mcmc_50000_20.RData")

#load("chain_50000_20.RData")
#load("tempo_mcmc_50000_20.RData")

###

ptm = proc.time()
chain_50000_30=mcmc_cpp( data = as.matrix(data1), w , K = 30 ,burning = 3000, n_iter = 50000, save_it = 10 )
tempo_mcmc_50000_30 = proc.time() - ptm

save(chain_50000_30,file="chain_50000_30.RData")
save(tempo_mcmc_50000_30,file="tempo_mcmc_50000_30.RData")

#load("chain_50000_30.RData")
#load("tempo_mcmc_50000_30.RData")

###

ptm = proc.time()
chain_50000_40=mcmc_cpp( data = as.matrix(data1), w , K = 40 ,burning = 3000, n_iter = 50000, save_it = 10 )
tempo_mcmc_50000_40 = proc.time() - ptm

save(chain_50000_40,file="chain_50000_40.RData")
save(tempo_mcmc_50000_40,file="tempo_mcmc_50000_40.RData")

#load("chain_50000_40.RData")
#load("tempo_mcmc_50000_40.RData")

###

ptm = proc.time()
chain_50000_50=mcmc_cpp( data = as.matrix(data1), w , K = 50 ,burning = 3000, n_iter = 50000, save_it = 10 )
tempo_mcmc_50000_50 = proc.time() - ptm

save(chain_50000_50,file="chain_50000_50.RData")
save(tempo_mcmc_50000_50,file="tempo_mcmc_50000_50.RData")

#load("chain_50000_50.RData")
#load("tempo_mcmc_50000_50.RData")


##########################
###Fazendo o cálculo da matriz beta_hat e theta_hat

load("chain_50000_2.RData")
aic_50000_2t=AIC_LDA(data_mat=data1,chain=chain_50000_2,mcmc_ind=1:dim(chain_50000_2[[1]])[3])
save(aic_50000_2t,file="aic_50000_2t.RData")

load("chain_50000_3.RData")
aic_50000_3t=AIC_LDA(data_mat=data1,chain=chain_50000_3,mcmc_ind=1:dim(chain_50000_3[[1]])[3])
save(aic_50000_3t,file="aic_50000_3t.RData")

load("chain_50000_4.RData")
aic_50000_4t=AIC_LDA(data_mat=data1,chain=chain_50000_4,mcmc_ind=1:dim(chain_50000_4[[1]])[3])
save(aic_50000_4t,file="aic_50000_4t.RData")

load("chain_50000_5.RData")
aic_50000_5t=AIC_LDA(data_mat=data1,chain=chain_50000_5,mcmc_ind=1:dim(chain_50000_5[[1]])[3])
save(aic_50000_5t,file="aic_50000_5t.RData")

load("chain_50000_6.RData")
aic_50000_6t=AIC_LDA(data_mat=data1,chain=chain_50000_6,mcmc_ind=1:dim(chain_50000_6[[1]])[3])
save(aic_50000_6t,file="aic_50000_6t.RData")

load("chain_50000_7.RData")
aic_50000_7t=AIC_LDA(data_mat=data1,chain=chain_50000_7,mcmc_ind=1:dim(chain_50000_7[[1]])[3])
save(aic_50000_7t,file="aic_50000_7t.RData")

load("chain_50000_8.RData")
aic_50000_8t=AIC_LDA(data_mat=data1,chain=chain_50000_8,mcmc_ind=1:dim(chain_50000_8[[1]])[3])
save(aic_50000_8t,file="aic_50000_8t.RData")

load("chain_50000_9.RData")
aic_50000_9t=AIC_LDA(data_mat=data1,chain=chain_50000_9,mcmc_ind=1:dim(chain_50000_9[[1]])[3])
save(aic_50000_9t,file="aic_50000_9t.RData")

load("chain_50000_10.RData")
aic_50000_10t=AIC_LDA(data_mat=data1,chain=chain_50000_10,mcmc_ind=1:dim(chain_50000_10[[1]])[3])
save(aic_50000_10t,file="aic_50000_10t.RData")

load("chain_50000_20.RData")
aic_50000_20t=AIC_LDA(data_mat=data1,chain=chain_50000_20,mcmc_ind=1:dim(chain_50000_20[[1]])[3])
save(aic_50000_20t,file="aic_50000_20t.RData")

load("chain_50000_30.RData")
aic_50000_30t=AIC_LDA(data_mat=data1,chain=chain_50000_30,mcmc_ind=1:dim(chain_50000_30[[1]])[3])
save(aic_50000_30t,file="aic_50000_30t.RData")

load("chain_50000_40.RData")
aic_50000_40t=AIC_LDA(data_mat=data1,chain=chain_50000_40,mcmc_ind=1:dim(chain_50000_40[[1]])[3])
save(aic_50000_40t,file="aic_50000_40t.RData")

load("chain_50000_50.RData")
aic_50000_50t=AIC_LDA(data_mat=data1,chain=chain_50000_50,mcmc_ind=1:dim(chain_50000_50[[1]])[3])
save(aic_50000_50t,file="aic_50000_50t.RData")

###########################################################
###Rodando 500.000 iteracoes no modelo LDA de 40 tópicos
###########################################################

###

ptm = proc.time()
chain_500000_40=mcmc_cpp( data = as.matrix(data1), w , K = 40 ,burning = 3000, n_iter = 500000, save_it = 100 )
tempo_mcmc_500000_40 = proc.time() - ptm

save(chain_500000_40,file="chain_500000_40.RData")
save(tempo_mcmc_500000_40,file="tempo_mcmc_500000_40.RData")

#load("chain_500000_40.RData")
#load("tempo_mcmc_500000_40.RData")

##########################

load("chain_500000_40.RData")
aic_500000_40t=AIC_LDA(data_mat=data1,chain=chain_500000_40,mcmc_ind=1:dim(chain_500000_40[[1]])[3])
save(aic_500000_40t,file="aic_500000_40t.RData")

##############################################
###Modelo MCMC treinado com 50000 iteracoes
#############################################

##Para o modelo com 20 tópicos:

load("chain_50000_20.RData") #Carregando a cadeia com 40 tópicos

names(chain_50000_20)
str(chain_50000_20)

##theta

dim(chain_50000_20$theta)

#plot(chain_50000_20$theta[1,1,],type="l")
#plot(chain_50000_20$theta[2,5,],type="l")
#plot(chain_50000_20$theta[300,9,],type="l")

#mean(chain_50000_20$theta[300,10,])
#mean(chain_50000_20$theta[300,10,-(1:500)]) #já resolvido no código .cpp

theta_hat=apply(X=chain_50000_20$theta,FUN=mean,MARGIN=c(1,2))
#load("aic_50000_20t.RData")
#theta_hat=aic_50000_20t$theta_hat    #método alterno para obter theta hat
head(theta_hat)

theta_hat_round=round(theta_hat,digits=3)

#Ordenamento dos tópicos com 
#maior probabilidade nos documentos

theta_hat_t=t(theta_hat_round) #A matriz transposta
#save(theta_hat_t,file="theta_hat_t.RData")
#load("theta_hat_t.RData")

vet_topicos=1:dim(chain_50000_20$beta)[1]

x=0 #Variavel auxiliar

for(i in 1:ncol(theta_hat_t)){

x=cbind(vet_topicos,as.vector(theta_hat_t[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Tópico",paste("Prob. no doc.",i))

if(i==1){
tabela_theta=x
}

if(i>1){
tabela_theta=cbind(tabela_theta,x)
}

}

head(tabela_theta)
#save(tabela_theta,file="tabela_theta_40t.RData")
#load("tabela_theta_40t.RData")

##Tabela do theta, para apresentacao no documento, com os melhores 3 tópicos:

tabela_theta_3t=tabela_theta[1:3,]
tabela_theta_3t=t(tabela_theta_3t)

x=tabela_theta_3t[seq(1,dim(tabela_theta_3t)[1],by=2),] #primeiros tres tópicos
y=tabela_theta_3t[seq(2,dim(tabela_theta_3t)[1],by=2),] #probabilidades dos primeiros tres tópicos

tabela_theta_final=matrix(0,dim(tabela_theta_3t)[1]/2,3)

for(i in 1:nrow(tabela_theta_final)){
for(j in 1:ncol(tabela_theta_final)){

tabela_theta_final[i,j]=paste0(x[i,j]," (",y[i,j],")")

}}

tabela_theta_apres=data.frame(1:nrow(tabela_theta_final),tabela_theta_final) #Tabela de apresentacao das probabilidades dos tópicos nos documentos

colnames(tabela_theta_apres)=c("No. do documento","1° tópico (probabilidade)",
"2° tópico (probabilidade)","3° tópico (probabilidade)")

tabela_theta_apres

#save(tabela_theta_apres,file="tabela_theta_apres_40t.RData")
#load("tabela_theta_apres_40t.RData")

##Gerar tabela no latex:
require(stargazer)

stargazer(tabela_theta_apres)

##Gerar tabela no Excel:

#csv_path  = 'tabela_theta_apres_40t.csv'
#write.csv(tabela_theta_apres, csv_path, row.names = FALSE)

#csv_path  = 'tabela_theta_40t.csv'
#write.csv(tabela_theta, csv_path, row.names = FALSE)

csv_path  = 'tabela_theta_apres_40t.csv'
write.csv(tabela_theta_apres, csv_path, row.names = FALSE)

##beta

dim(chain_50000_20$beta)

#plot(chain_50000_20$beta[1,1,],type="l")
#plot(chain_50000_20$beta[2,15,],type="l")
#plot(chain_50000_20$beta[9,100,],type="l")

#mean(chain_50000_20$beta[3,10,])
#mean(chain_50000_20$beta[3,10,-(1:500)]) #já foi resolvido no código .cpp

beta_hat=apply(X=chain_50000_20$beta,FUN=mean,MARGIN=c(1,2))
#load("aic_50000_20t.RData")
#beta_hat=aic_50000_20t$beta_hat    #método alterno para obter beta hat
head(beta_hat)
#save(beta_hat,file="beta_hat.RData")
#load("beta_hat.RData")

beta_hat_round=round(beta_hat,digits=3)

#Ordenamento das palavras com 
#maior probabilidade nos tópicos

beta_hat_t=t(beta_hat_round) #A matriz transposta

#vet_palavras=1:nrow(beta_hat_t)
vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t)){

#x=cbind(vet_palavras,as.vector(beta_hat_t[,i]))
x=data.frame(vet_palavras,as.vector(beta_hat_t[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Prob. no tóp.",i))

if(i==1){
tabela_beta=x
}

if(i>1){
tabela_beta=cbind(tabela_beta,x)
}

}

head(tabela_beta)

#save(tabela_beta,file="tabela_beta.RData")
#load("tabela_beta.RData")

#Gerar tabela no latex:

stargazer(tabela_beta[1:10,],summary=F,rownames=F)

#Gerar tabela no Excel:

csv_path  = 'tabela_beta.csv'
write.csv(tabela_beta, csv_path, row.names = FALSE)

########
##Visualizando um tópico, pontuacao do termo no beta_hat:

k_maiusc=dim(beta_hat)[1]
beta_hat_term_score=matrix(0,k_maiusc,dim(beta_hat)[2])

for(i in 1:k_maiusc){
for(j in 1:dim(beta_hat)[2]){

beta_hat_term_score[i,j]=beta_hat[i,j]*(log(beta_hat[i,j])-(1/k_maiusc)*sum(log(beta_hat[,j])))

}}

head(beta_hat_term_score)

beta_hat_round_ts=round(beta_hat_term_score,digits=3)

#Ordenamento das palavras com 
#maior pontuacao nos tópicos

beta_hat_t_ts=t(beta_hat_round_ts) #A matriz transposta

#vet_palavras=1:nrow(beta_hat_t)
vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t_ts)){

#x=cbind(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=data.frame(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Pont. no tóp.",i))

if(i==1){
tabela_beta_ts=x
}

if(i>1){
tabela_beta_ts=cbind(tabela_beta_ts,x)
}

}

head(tabela_beta_ts)

#save(tabela_beta_ts,file="tabela_beta_ts_40t.RData")
#load("tabela_beta_ts_40t.RData")

#Gerar tabela no latex:

stargazer(tabela_beta_ts[1:10,],summary=F,rownames=F)

#Gerar tabela no Excel:

csv_path  = 'tabela_beta_ts_40.csv'
write.csv(tabela_beta_ts, csv_path, row.names = FALSE)

#######
##Encontrando documentos semelhantes:

d_maiusc=dim(theta_hat)[1]

doc_sim=matrix(0,d_maiusc,d_maiusc)

for(i in 1:d_maiusc){
for(j in 1:d_maiusc){

doc_sim[i,j]=sum((sqrt(theta_hat[i,])-sqrt(theta_hat[j,]))^2)

}}

doc_sim

#for(i in 1:dim(doc_sim)[1]){

#x=doc_sim[i,]/max(doc_sim[i,])
#doc_sim[i,]=x

#}

doc_sim=doc_sim/max(doc_sim)

head(doc_sim)

top10_doc_sem=function(d){

#d=1

#doc_sim_cons=apply(X=doc_sim,FUN=sum,MARGIN=1)
#doc_sim_cons=cbind(1:d_maiusc,doc_sim_cons)
doc_sim_cons=cbind(1:d_maiusc,doc_sim[,d]) #escolha do documento
doc_sim_cons=doc_sim_cons[order(doc_sim_cons[,2],decreasing=F),]

#doc_sim_cons=doc_sim_cons[-1,] #tirando a linha do mesmmo documento

##Top 10 de documentos semelhantes:

doc_sim_top10=doc_sim_cons[1:11,] #incluido o documento examinado na linha 1

doc_sim_aux=cbind(doc_sim_top10,doc_ies[doc_sim_top10[,1],])
doc_sim_aux=data.frame(doc_sim_aux,pastas[doc_sim_aux[,4]])

x=0

for(i in 1:11){
x[i]=list.files(doc_sim_aux[i,6],pattern="\\.")[doc_sim_aux[i,3]]
}

doc_sim_aux=data.frame(doc_sim_aux,x)

doc_sim_aux=data.frame(doc_sim_aux,substr(doc_sim_aux[,7],
1,nchar(doc_sim_aux[,7])-6),as.numeric(substr(doc_sim_aux[,7],
nchar(doc_sim_aux[,7])-5,nchar(doc_sim_aux[,7])-6+2)))

ementa=list.dirs("Base de Dados/",recursive=F)
x=0

for(i in 1:11){

x[i]=list.files(grep(pattern = doc_sim_aux[i,8],ementa,value = TRUE),pattern="\\.")[doc_sim_aux[i,9]]

}

doc_sim_aux=data.frame(doc_sim_aux,paste0(substr(x,1,nchar(x)-4),"- IES: ",doc_sim_aux[,8]))

doc_sim_aux[,2]=round(doc_sim_aux[,2],digits=3)

#doc_sim_top10_final=as.data.frame(doc_sim_aux[2:11,c(10,2)])
doc_sim_top10_final=data.frame(paste0(doc_sim_aux[2:11,1],"- ",
doc_sim_aux[2:11,10]),doc_sim_aux[2:11,2])

colnames(doc_sim_top10_final)=c("10 DOCUMENTOS MAIS SIMILARES","VALOR DA SIMILARIDADE")
rownames(doc_sim_top10_final)=1:10

print(paste0("Documento ",d,": ",doc_sim_aux[1,10])) #Nome do documento examinado

return(doc_sim_top10_final)

#save(doc_sim_top10_final,file="doc_sim_top10_final.RData")
#load("doc_sim_top10_final.RData")

}

##Gerar tabela no Excel:

#csv_path  = 'doc_sim_top10_final.csv'
#write.csv(doc_sim_top10_final, csv_path, row.names = TRUE)

###Principais palavras dos principais tópicos no documento "d" (por pontuação do termo):

top_pal_top_t_doc=function(d){

#d=1

topics=tabela_theta[1:3,d*2-1]
prob_t=tabela_theta[1:3,d*2]

top_pal_top_t_doc=tabela_beta_ts[1:15,topics*2-1]
colnames(top_pal_top_t_doc)=paste0("Tópico ",topics," (prob. ",prob_t,")")
rownames(top_pal_top_t_doc)=1:15

#top_pal_top_t_doc
return(top_pal_top_t_doc)

}

###Proporções de tópico esperadas no documento "d":

prop_t_esp_d=function(d){

#d=1

x=tabela_theta[,c(d*2-1,d*2)]
x=x[order(x[,1],decreasing=F),]

return(barplot(x[,2],ylim=c(0,1),names.arg=x[,1],xlab="Tópico",ylab="Probabilidade",
main=paste0("Proporções de tópico esperadas no documento ",d),cex.main=0.8))

}

###Relatório de apresentacao por documento "d":

relatorio=function(d){

return(list(top10_doc_sem(d),
top_pal_top_t_doc(d),
prop_t_esp_d(d))[-3])

}

###Mapa de Calor:

#heatmap(doc_sim,col=hcl.colors(50))
heatmap(doc_sim,Rowv=NA,Colv=NA)

##############################################
###Modelo MCMC treinado com 500000 iteracoes
#############################################

##Para o modelo com 40 tópicos:

load("chain_500000_40.RData") #Carregando a cadeia com 40 tópicos

names(chain_500000_40)
str(chain_500000_40)

##theta

dim(chain_500000_40$theta)

#plot(chain_500000_40$theta[1,1,],type="l")
#plot(chain_500000_40$theta[2,5,],type="l")
#plot(chain_500000_40$theta[300,9,],type="l")

#mean(chain_500000_40$theta[300,10,])
#mean(chain_500000_40$theta[300,10,-(1:500)]) #já resolvido no código .cpp

theta_hat=apply(X=chain_500000_40$theta,FUN=mean,MARGIN=c(1,2))
#load("aic_500000_40t.RData")
#theta_hat=aic_500000_40t$theta_hat    #método alterno para obter theta hat
head(theta_hat)

theta_hat_round=round(theta_hat,digits=3)

#Ordenamento dos tópicos com 
#maior probabilidade nos documentos

theta_hat_t=t(theta_hat_round) #A matriz transposta
#save(theta_hat_t,file="theta_hat_t.RData")
#load("theta_hat_t.RData")

vet_topicos=1:dim(chain_500000_40$beta)[1]

x=0 #Variavel auxiliar

for(i in 1:ncol(theta_hat_t)){

x=cbind(vet_topicos,as.vector(theta_hat_t[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Tópico",paste("Prob. no doc.",i))

if(i==1){
tabela_theta=x
}

if(i>1){
tabela_theta=cbind(tabela_theta,x)
}

}

head(tabela_theta)
#save(tabela_theta,file="tabela_theta_40t.RData")
#load("tabela_theta_40t.RData")

##Tabela do theta, para apresentacao no documento, com os melhores 3 tópicos:

tabela_theta_3t=tabela_theta[1:3,]
tabela_theta_3t=t(tabela_theta_3t)

x=tabela_theta_3t[seq(1,dim(tabela_theta_3t)[1],by=2),] #primeiros tres tópicos
y=tabela_theta_3t[seq(2,dim(tabela_theta_3t)[1],by=2),] #probabilidades dos primeiros tres tópicos

tabela_theta_final=matrix(0,dim(tabela_theta_3t)[1]/2,3)

for(i in 1:nrow(tabela_theta_final)){
for(j in 1:ncol(tabela_theta_final)){

tabela_theta_final[i,j]=paste0(x[i,j]," (",y[i,j],")")

}}

tabela_theta_apres=data.frame(1:nrow(tabela_theta_final),tabela_theta_final) #Tabela de apresentacao das probabilidades dos tópicos nos documentos

colnames(tabela_theta_apres)=c("No. do documento","1° tópico (probabilidade)",
"2° tópico (probabilidade)","3° tópico (probabilidade)")

tabela_theta_apres

#save(tabela_theta_apres,file="tabela_theta_apres_40t.RData")
#load("tabela_theta_apres_40t.RData")

##Gerar tabela no latex:
require(stargazer)

stargazer(tabela_theta_apres)

##Gerar tabela no Excel:

#csv_path  = 'tabela_theta_apres_40t.csv'
#write.csv(tabela_theta_apres, csv_path, row.names = FALSE)

#csv_path  = 'tabela_theta_40t.csv'
#write.csv(tabela_theta, csv_path, row.names = FALSE)

csv_path  = 'tabela_theta_apres_40t.csv'
write.csv(tabela_theta_apres, csv_path, row.names = FALSE)

##beta

dim(chain_500000_40$beta)

#plot(chain_500000_40$beta[1,1,],type="l")
#plot(chain_500000_40$beta[2,15,],type="l")
#plot(chain_500000_40$beta[9,100,],type="l")

#mean(chain_500000_40$beta[3,10,])
#mean(chain_500000_40$beta[3,10,-(1:500)]) #já foi resolvido no código .cpp

beta_hat=apply(X=chain_500000_40$beta,FUN=mean,MARGIN=c(1,2))
#load("aic_500000_40t.RData")
#beta_hat=aic_500000_40t$beta_hat    #método alterno para obter beta hat
head(beta_hat)
#save(beta_hat,file="beta_hat.RData")
#load("beta_hat.RData")

beta_hat_round=round(beta_hat,digits=3)

#Ordenamento das palavras com 
#maior probabilidade nos tópicos

beta_hat_t=t(beta_hat_round) #A matriz transposta

#vet_palavras=1:nrow(beta_hat_t)
vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t)){

#x=cbind(vet_palavras,as.vector(beta_hat_t[,i]))
x=data.frame(vet_palavras,as.vector(beta_hat_t[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Prob. no tóp.",i))

if(i==1){
tabela_beta=x
}

if(i>1){
tabela_beta=cbind(tabela_beta,x)
}

}

head(tabela_beta)

#save(tabela_beta,file="tabela_beta.RData")
#load("tabela_beta.RData")

#Gerar tabela no latex:

stargazer(tabela_beta[1:10,],summary=F,rownames=F)

#Gerar tabela no Excel:

csv_path  = 'tabela_beta.csv'
write.csv(tabela_beta, csv_path, row.names = FALSE)

########
##Visualizando um tópico, pontuacao do termo no beta_hat:

k_maiusc=dim(beta_hat)[1]
beta_hat_term_score=matrix(0,k_maiusc,dim(beta_hat)[2])

for(i in 1:k_maiusc){
for(j in 1:dim(beta_hat)[2]){

beta_hat_term_score[i,j]=beta_hat[i,j]*(log(beta_hat[i,j])-(1/k_maiusc)*sum(log(beta_hat[,j])))

}}

head(beta_hat_term_score)

beta_hat_round_ts=round(beta_hat_term_score,digits=3)

#Ordenamento das palavras com 
#maior pontuacao nos tópicos

beta_hat_t_ts=t(beta_hat_round_ts) #A matriz transposta

#vet_palavras=1:nrow(beta_hat_t)
vet_palavras=BD_Final_uni[,1]

x=0 #Variavel auxiliar

for(i in 1:ncol(beta_hat_t_ts)){

#x=cbind(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=data.frame(vet_palavras,as.vector(beta_hat_t_ts[,i]))
x=x[order(x[,2],decreasing=T),]
colnames(x)=c("Palavra",paste("Pont. no tóp.",i))

if(i==1){
tabela_beta_ts=x
}

if(i>1){
tabela_beta_ts=cbind(tabela_beta_ts,x)
}

}

head(tabela_beta_ts)

#save(tabela_beta_ts,file="tabela_beta_ts_40t.RData")
#load("tabela_beta_ts_40t.RData")

#Gerar tabela no latex:

stargazer(tabela_beta_ts[1:10,],summary=F,rownames=F)

#Gerar tabela no Excel:

csv_path  = 'tabela_beta_ts_40.csv'
write.csv(tabela_beta_ts, csv_path, row.names = FALSE)

#######
##Encontrando documentos semelhantes:

d_maiusc=dim(theta_hat)[1]

doc_sim=matrix(0,d_maiusc,d_maiusc)

for(i in 1:d_maiusc){
for(j in 1:d_maiusc){

doc_sim[i,j]=sum((sqrt(theta_hat[i,])-sqrt(theta_hat[j,]))^2)

}}

doc_sim

#for(i in 1:dim(doc_sim)[1]){

#x=doc_sim[i,]/max(doc_sim[i,])
#doc_sim[i,]=x

#}

doc_sim=doc_sim/max(doc_sim)

head(doc_sim)

top10_doc_sem=function(d){

#d=1

#doc_sim_cons=apply(X=doc_sim,FUN=sum,MARGIN=1)
#doc_sim_cons=cbind(1:d_maiusc,doc_sim_cons)
doc_sim_cons=cbind(1:d_maiusc,doc_sim[,d]) #escolha do documento
doc_sim_cons=doc_sim_cons[order(doc_sim_cons[,2],decreasing=F),]

#doc_sim_cons=doc_sim_cons[-1,] #tirando a linha do mesmmo documento

##Top 10 de documentos semelhantes:

doc_sim_top10=doc_sim_cons[1:11,] #incluido o documento examinado na linha 1

doc_sim_aux=cbind(doc_sim_top10,doc_ies[doc_sim_top10[,1],])
doc_sim_aux=data.frame(doc_sim_aux,pastas[doc_sim_aux[,4]])

x=0

for(i in 1:11){
x[i]=list.files(doc_sim_aux[i,6],pattern="\\.")[doc_sim_aux[i,3]]
}

doc_sim_aux=data.frame(doc_sim_aux,x)

doc_sim_aux=data.frame(doc_sim_aux,substr(doc_sim_aux[,7],
1,nchar(doc_sim_aux[,7])-6),as.numeric(substr(doc_sim_aux[,7],
nchar(doc_sim_aux[,7])-5,nchar(doc_sim_aux[,7])-6+2)))

ementa=list.dirs("Base de Dados/",recursive=F)
x=0

for(i in 1:11){

x[i]=list.files(grep(pattern = doc_sim_aux[i,8],ementa,value = TRUE),pattern="\\.")[doc_sim_aux[i,9]]

}

doc_sim_aux=data.frame(doc_sim_aux,paste0(substr(x,1,nchar(x)-4),"- IES: ",doc_sim_aux[,8]))

doc_sim_aux[,2]=round(doc_sim_aux[,2],digits=3)

#doc_sim_top10_final=as.data.frame(doc_sim_aux[2:11,c(10,2)])
doc_sim_top10_final=data.frame(paste0(doc_sim_aux[2:11,1],"- ",
doc_sim_aux[2:11,10]),doc_sim_aux[2:11,2])

colnames(doc_sim_top10_final)=c("10 DOCUMENTOS MAIS SIMILARES","VALOR DA SIMILARIDADE")
rownames(doc_sim_top10_final)=1:10

print(paste0("Documento ",d,": ",doc_sim_aux[1,10])) #Nome do documento examinado

return(doc_sim_top10_final)

#save(doc_sim_top10_final,file="doc_sim_top10_final.RData")
#load("doc_sim_top10_final.RData")

}

##Gerar tabela no Excel:

#csv_path  = 'doc_sim_top10_final.csv'
#write.csv(doc_sim_top10_final, csv_path, row.names = TRUE)

###Principais palavras dos principais tópicos no documento "d" (por pontuação do termo):

top_pal_top_t_doc=function(d){

#d=1

topics=tabela_theta[1:3,d*2-1]
prob_t=tabela_theta[1:3,d*2]

top_pal_top_t_doc=tabela_beta_ts[1:15,topics*2-1]
colnames(top_pal_top_t_doc)=paste0("Tópico ",topics," (prob. ",prob_t,")")
rownames(top_pal_top_t_doc)=1:15

#top_pal_top_t_doc
return(top_pal_top_t_doc)

}

###Proporções de tópico esperadas no documento "d":

prop_t_esp_d=function(d){

#d=1

x=tabela_theta[,c(d*2-1,d*2)]
x=x[order(x[,1],decreasing=F),]

return(barplot(x[,2],ylim=c(0,1),names.arg=x[,1],xlab="Tópico",ylab="Probabilidade",
main=paste0("Proporções de tópico esperadas no documento ",d),cex.main=0.8))

}

###Relatório de apresentacao por documento "d":

relatorio=function(d){

return(list(top10_doc_sem(d),
top_pal_top_t_doc(d),
prop_t_esp_d(d))[-3])

}

###Mapa de Calor:

#heatmap(doc_sim,col=hcl.colors(50))
heatmap(doc_sim,Rowv=NA,Colv=NA)

#################################################################
###Rodando a cadeia para olhar as primeiras iteracoes sem burning
#################################################################

sourceCpp("./Codes/mcmc_function-beta_inicial.cpp")

K=40		#O melhor modelo escolhido
V=max(na.omit(data[,1]))

##Olhando o início da cadeia dos beta_{13 , 1} e beta_{27 , 970}, para o modelo LDA
##com K=40 e com 10000 iteracoes

chain_init_10000_40=list()
beta_inicial=list()

mat_beta=matrix(1/V,K,V)

##com semente de 0

mat_beta1=mat_beta
b1=rep(1/(V-1),V)
b1[1]=0
b2=rep(1/(V-1),V)
b2[970]=0
mat_beta1[13,]=b1
mat_beta1[27,]=b2
beta_inicial[[1]]=mat_beta1

##com semente de 0.5

mat_beta1=mat_beta
b1=rep(0.5/(V-1),V)
b1[1]=0.5
b2=rep(0.5/(V-1),V)
b2[970]=0.5
mat_beta1[13,]=b1
mat_beta1[27,]=b2
beta_inicial[[2]]=mat_beta1

##com semente de 1

mat_beta1=mat_beta
b1=rep(0,V)
b1[1]=1
b2=rep(0,V)
b2[970]=1
mat_beta1[13,]=b1
mat_beta1[27,]=b2
beta_inicial[[3]]=mat_beta1


###

for(i in 1:3){

chain_init_10000_40[[i]]=mcmc_cpp( data = as.matrix(data), w , K = 40,burning = 0, beta_init = beta_inicial[[i]], n_iter = 500, save_it = 5 )

}

save(chain_init_10000_40,file="chain_init_10000_40.RData")

#load("chain_init_10000_40.RData")

semente=c(0,"0,5",1)

par(mfrow=c(3,2))

for(i in 1:3){
plot(chain_init_10000_40[[i]]$beta[13,1,],type="l",
ylab=bquote(beta["13,1"]),
xlab=bquote("Número de iteração, com semente de "*.(semente[i])))
plot(chain_init_10000_40[[i]]$beta[27,970,],type="l",
ylab=bquote(beta["27,970"]),
xlab=bquote("Número de iteração, com semente de "*.(semente[i])))
}

#############################################################################
###Olhando as cadeias dos beta_{kv} e theta_{dk} com maior pontoacao do termo
############################################################################

##Obtendo as coordenadas na matrix do beta

### Nao correr ###
head(beta_hat_term_score)

which.max(beta_hat_term_score)
arrayInd(which.max(beta_hat_term_score), dim(beta_hat_term_score))
beta_hat_term_score[13,1]
### Nao correr até aquí ###

ind_beta=matrix(0,prod(dim(beta_hat_term_score)),3)
x=0

for(k in 1:nrow(beta_hat_term_score)){
for(v in 1:ncol(beta_hat_term_score)){

x=x+1

ind_beta[x,]=c(beta_hat_term_score[k,v],k,v)

}}

ind_beta_ord=ind_beta[order(ind_beta[, 1], decreasing = TRUE), ]
head(ind_beta_ord)

##Olhando os gráficos das cadeias dos beta_{kv}

dim(chain_500000_40$beta)

par(mfrow=c(3,2))

for(i in 1:6){
#i=1
plot(chain_500000_40$beta[ind_beta_ord[i,2],ind_beta_ord[i,3],],type="l",
ylab=bquote(beta[.(ind_beta_ord[i,2])* "," *.(ind_beta_ord[i,3])]),
xlab="Número de iteração, depois do\n aquecimento e espaçamento")
}

#######
##Olhando com a matriz das probabilidades

#beta_hat=aic_40t$beta_hat

ind_beta=matrix(0,prod(dim(beta_hat)),3)
x=0

for(k in 1:nrow(beta_hat)){
for(v in 1:ncol(beta_hat)){

x=x+1

ind_beta[x,]=c(beta_hat[k,v],k,v)

}}

ind_beta_ord=ind_beta[order(ind_beta[, 1], decreasing = TRUE), ]
head(ind_beta_ord)

##Olhando os gráficos das cadeias dos beta_{kv}

dim(chain_500000_40$beta)

par(mfrow=c(3,2))

for(i in 1:6){
#i=2
plot(chain_500000_40$beta[ind_beta_ord[i,2],ind_beta_ord[i,3],],type="l",
ylab=bquote(beta[.(ind_beta_ord[i,2])* "," *.(ind_beta_ord[i,3])]),
xlab="Número de iteração, depois do\n aquecimento e espaçamento")
}

###############################

##Obtendo as coordenadas na matrix do theta

#load("aic_40t.RData")
#theta_hat=aic_40t$theta_hat

ind_theta=matrix(0,prod(dim(theta_hat)),3)
x=0

for(d in 1:nrow(theta_hat)){
for(k in 1:ncol(theta_hat)){

x=x+1

ind_theta[x,]=c(theta_hat[d,k],d,k)

}}

ind_theta_ord=ind_theta[order(ind_theta[, 1], decreasing = TRUE), ]
head(ind_theta_ord)

##Olhando os gráficos das cadeias dos theta_{dk}

dim(chain_500000_40$theta)

par(mfrow=c(3,2))

for(i in 1:6){
#i=2
plot(chain_500000_40$theta[ind_theta_ord[i,2],ind_theta_ord[i,3],],type="l",
ylab=bquote(theta[.(ind_theta_ord[i,2])* "," *.(ind_theta_ord[i,3])]),
xlab="Número de iteração, depois do\n aquecimento e espaçamento")
}
