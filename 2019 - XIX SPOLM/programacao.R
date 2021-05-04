#ATIVACAO
library(RCurl)
library(XML)
library(NLP)
library(tm)
library(stringi)
library(stopwords)
library(ptstem)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(e1071)

library(cluster)
library(factoextra) 
library(ppclust)
library(fclust)
library(vegclust)


#===============================================================================#
#                         PRE-PROCESSAMENTO TEXTO 1                             #
#===============================================================================#
#1) Primeiramente se tentou ler o texto diretamente da internet. No entanto,...
#...por algum motivo desconhecido, o RStudio nao reconheceu. Dessa forma,...
#... optou-se por outra alternativa, que consiste em copiar o texto do site e...
#... colar em um arquivo .txt;
#Retirado de: https://www.logisticadescomplicada.com/cadeia-de-suprimentos-na-logistica/

#LENDO TEXTO DO SITE
texto = getURL("https://www.logisticadescomplicada.com/cadeia-de-suprimentos-na-logistica/",ssl.verifypeer = F)
txt = stri_trans_general(texto,"Latin-ASCII")
txt = htmlTreeParse(txt, useInternal = TRUE);txt
txt = unlist(xpathApply(txt, path="//p", fun = xmlValue)) #remove endereco de URL
class(txt)
print(txt)

#SALVANDO O TEXTO COMO COPIA
write.table(txt,"texto_spolm.txt")

#TRABALHANDO COM AS PARTES QUE INTERESSAM
txtnovo = txt[1:11];txtnovo

#RETIRANDO ESPACOS E O TRANSFORMANDO EM UM UNICO TEXTO
txtnovo = paste(txtnovo,collapse = " ");txtnovo

#RETIRA PONTUACAO
txtnovo = gsub(pattern = "\\W",replace = " ", txtnovo)

#REMOVE NUMEROS
txtnovo = gsub(pattern = "\\d",replace = " ", txtnovo);txtnovo

#LETRAS MAIUSCULAS EM MINUSCULAS
txtnovo = tolower(txtnovo);txtnovo

#TRNASFORMANDO OS DADOS EM UM CORPUS
t1 = VCorpus(VectorSource(txtnovo))
m1   = tm_map(t1,PlainTextDocument)
mtd1 = TermDocumentMatrix(m1) #termos nas linhas e documento na coluna
dim(mtd1) #matriz com 274 termos e 1 documento
Terms(mtd1) #exibe os termos

#REMOVE ACENTOS
txtnovo = iconv(txtnovo,to="ASCII//TRANSLIT");txtnovo

#REMOVE CARACTERES ESPECIAIS
txtnovo = gsub(pattern="\\b[A-z]\\b{1}",replace= " ",txtnovo);txtnovo

#10) REMOVE ESPACO EM BRANCO
txtnovo = stripWhitespace(txtnovo);txtnovo

#11) REMOVE STOPWORDS
txtnovo = removeWords(txtnovo,stopwords("pt")) #lista de stopwords do RStudio
txtnovo = removeWords(txtnovo,as.matrix(palavras)) #lista com outras stopwords

#12) TRNASFORMANDO OS DADOS EM UM CORPUS
t1 = VCorpus(VectorSource(txtnovo))

#13) MATRIZ TERMO-DOCUMENTO
m1   = tm_map(t1,PlainTextDocument)
mtd1 = TermDocumentMatrix(m1) #termos nas linhas e documento na coluna
dim(mtd1) #matriz com 201 termos e 1 documento
Terms(mtd1) #exibe os termos

#SALVANDO EM .TXT PARA O ALGORITMO DE STEMMING
write.table(dimnames(mtd1)$Terms,"rslp1.txt")

#15) APLICANDO O PROCESSO DE STEMMING RSLP
dados1 = read.table("rslp1.txt", header=T)
dados1 = as.matrix(dados1)
st1    = ptstem_words(dados1,algorithm = "rslp");st1

#16) JUNTANDO OS TERMOS SEMELHANTES
for (j in seq(t1)){
  t1[[j]] = gsub("chain","",  t1[[j]])
  t1[[j]] = gsub("supply","supplychain",  t1[[j]])
  t1[[j]] = gsub("armazenamento","armazenagem",  t1[[j]])
  t1[[j]] = gsub("atendimento","atender",  t1[[j]])
  t1[[j]] = gsub("suprimentos","",  t1[[j]])
  t1[[j]] = gsub("cadeias","cadeiadesuprimentos",  t1[[j]])
  t1[[j]] = gsub("cadeia","cadeiadesuprimentos",  t1[[j]])
  t1[[j]] = gsub("clientes","cliente", t1[[j]])
  t1[[j]] = gsub("conceitos","conceito",  t1[[j]])
  t1[[j]] = gsub("conhecimento","conhecer",  t1[[j]])
  t1[[j]] = gsub("consumidores","consumidor",  t1[[j]])
  t1[[j]] = gsub("empresas","empresa",  t1[[j]])
  t1[[j]] = gsub("entendem","entender",  t1[[j]])
  t1[[j]] = gsub("fluxos","fluxo",  t1[[j]])
  t1[[j]] = gsub("fornecedores","fornecedor",  t1[[j]])
  t1[[j]] = gsub("gestoes","gestao",  t1[[j]])
  t1[[j]] = gsub("importantissimo","importancia",  t1[[j]])
  t1[[j]] = gsub("introduzidos","introduzida",  t1[[j]])
  t1[[j]] = gsub("logistico","logistica",  t1[[j]])
  t1[[j]] = gsub("logisticos","logistica",  t1[[j]])
  t1[[j]] = gsub("logisticas","logistica",  t1[[j]])
  t1[[j]] = gsub("ofertar","oferta",  t1[[j]])
  t1[[j]] = gsub("supply","supplychain",  t1[[j]])
  t1[[j]] = gsub("percebidos","perceber",  t1[[j]])
  t1[[j]] = gsub("percepcao","perceber",  t1[[j]])
  t1[[j]] = gsub("planejadas","planejamento",  t1[[j]])
  t1[[j]] = gsub("planejamentos","planejamento",  t1[[j]])
  t1[[j]] = gsub("realizados","realizado",  t1[[j]])
  t1[[j]] = gsub("servicos","servico",  t1[[j]])
  t1[[j]] = gsub("decisao","",  t1[[j]])
  t1[[j]] = gsub("tomada","tomadadedecisao",  t1[[j]])
  t1[[j]] = gsub("voltados","voltadas",  t1[[j]])
  t1[[j]] = gsub("variancias","variar",  t1[[j]])
  t1[[j]] = gsub("cadeiadesuprimentosdesuprimentos","cadeiadesuprimentos",  t1[[j]])
  t1[[j]] = gsub("supplychainchain","supplychain",  t1[[j]])
  
}  
t1   = tm_map(t1,stripWhitespace) 
m1   = tm_map(t1,PlainTextDocument)
mtd1 = TermDocumentMatrix(m1)  #termos nas linhas e documento na coluna
dim(mtd1)   #matriz com 178 termos e 1 documento
Terms(mtd1) #exibe os termos

#18) SALVANDO O ARQUIVO EM .TXT
write.csv2(as.matrix(mtd1),"termos_texto1.csv")
#===============================================================================#



#LENDO TEXTO DO SITE
texto2 = getURL("https://portogente.com.br/portopedia/91207-o-que-e-a-gestao-da-cadeia-de-suprimentos-e-como-funciona/",ssl.verifypeer = F)
txt2 = stri_trans_general(texto2,"Latin-ASCII")
txt2 = htmlTreeParse(txt2, useInternal = TRUE);txt2
txt2 = unlist(xpathApply(txt2, path="//p", fun = xmlValue)) #remove endereco de URL
class(txt2)
print(txt2)

#SALVANDO O TEXTO COMO COPIA
write.table(txt2,"texto2_spolm.txt")

#TRABALHANDO COM AS PARTES QUE INTERESSAM
txtnovo2 = c(txt2[1],txt2[3:20]);txtnovo2

#RETIRANDO ESPACOS E O TRANSFORMANDO EM UM UNICO TEXTO
txtnovo2 = paste(txtnovo2,collapse = " ");txtnovo2

#RETIRA PONTUACAO
txtnovo2 = gsub(pattern = "\\W",replace = " ", txtnovo2)

#REMOVE NUMEROS
txtnovo2 = gsub(pattern = "\\d",replace = " ", txtnovo2);txtnovo2

#LETRAS MAIUSCULAS EM MINUSCULAS
txtnovo2 = tolower(txtnovo2);txtnovo2

#TRNASFORMANDO OS DADOS EM UM CORPUS
t2 = VCorpus(VectorSource(txtnovo2))
m2   = tm_map(t2,PlainTextDocument)
mtd2 = TermDocumentMatrix(m2) #termos nas linhas e documento na coluna
dim(mtd2) #matriz com 274 termos e 1 documento
Terms(mtd2) #exibe os termos

#REMOVE ACENTOS
txtnovo2 = iconv(txtnovo2,to="ASCII//TRANSLIT");txtnovo2

#REMOVE CARACTERES ESPECIAIS
txtnovo2 = gsub(pattern="\\b[A-z]\\b{1}",replace= " ",txtnovo2);txtnovo2

#10) REMOVE ESPACO EM BRANCO
txtnovo2 = stripWhitespace(txtnovo2);txtnovo2

#11) REMOVE STOPWORDS
palavras = read.table("C:/Users/carla/OneDrive/Área de Trabalho/Dissertacao/Programacao/Stopwords.txt",header=F) 
txtnovo2 = removeWords(txtnovo2,stopwords("pt")) #lista de stopwords do RStudio
txtnovo2 = removeWords(txtnovo2,as.matrix(palavras)) #lista com outras stopwords

#12) TRNASFORMANDO OS DADOS EM UM CORPUS
t2 = VCorpus(VectorSource(txtnovo2))

#13) MATRIZ TERMO-DOCUMENTO
m2   = tm_map(t2,PlainTextDocument)
mtd2 = TermDocumentMatrix(m2) #termos nas linhas e documento na coluna
dim(mtd2) #matriz com 201 termos e 1 documento
Terms(mtd2) #exibe os termos

#SALVANDO EM .TXT PARA O ALGORITMO DE STEMMING
write.table(dimnames(mtd2)$Terms,"rslp2.txt")

#15) APLICANDO O PROCESSO DE STEMMING RSLP
dados2 = read.table("rslp2.txt", header=T)
dados2 = as.matrix(dados2)
st2    = ptstem_words(dados2,algorithm = "rslp");st2

#16) JUNTANDO OS TERMOS SEMELHANTES
for (j in seq(t2)){
  t2[[j]] = gsub("chain","",  t2[[j]])
  t2[[j]] = gsub("supply","supplychain",  t2[[j]])
  t2[[j]] = gsub("atender","atendimento",  t2[[j]])
  t2[[j]] = gsub("auxiliem","auxiliar",  t2[[j]])
  t2[[j]] = gsub("cliente","cliente",  t2[[j]])
  t2[[j]] = gsub("conhecido","conhecer",  t2[[j]])
  t2[[j]] = gsub("cadeia","",  t2[[j]])
  t2[[j]] = gsub("suprimentos","cadeiadesuprimentos",  t2[[j]])
  t2[[j]] = gsub("conquistar","conseguir",  t2[[j]])
  t2[[j]] = gsub("custos","custo",  t2[[j]])
  t2[[j]] = gsub("empresas","empresa",  t2[[j]])
  t2[[j]] = gsub("exista","existir",  t2[[j]])
  t2[[j]] = gsub("existem","exitir",  t2[[j]])
  t2[[j]] = gsub("fornecedores","fornecedor",  t2[[j]])
  t2[[j]] = gsub("funciona","funcionamento",  t2[[j]])
  t2[[j]] = gsub("fundamentais","fundamental",  t2[[j]])
  t2[[j]] = gsub("garantindo","garantir",  t2[[j]])
  t2[[j]] = gsub("gerenciar","gerenciamento",  t2[[j]])
  t2[[j]] = gsub("incluindo","incluir",  t2[[j]])
  t2[[j]] = gsub("informacoes","informacao",  t2[[j]])
  t2[[j]] = gsub("informando","informacao",  t2[[j]])
  t2[[j]] = gsub("logistico","logistica",  t2[[j]])
  t2[[j]] = gsub("necessarias","necessario",  t2[[j]])
  t2[[j]] = gsub("ofertado","oferta",  t2[[j]])
  t2[[j]] = gsub("otimizar","otimizacao",  t2[[j]])
  t2[[j]] = gsub("passar","passa",  t2[[j]])
  t2[[j]] = gsub("passando","passa",  t2[[j]])
  t2[[j]] = gsub("produtos","produto",  t2[[j]])
  t2[[j]] = gsub("servicos","servico",  t2[[j]])
  t2[[j]] = gsub("transportes","transporte",  t2[[j]])
}  
outras = c("just","ipog","gcs","mba","scm")
t2 = tm_map(t2,removeWords,outras)
t2   = tm_map(t2,stripWhitespace) 
m2   = tm_map(t2,PlainTextDocument)
mtd2 = TermDocumentMatrix(m2)  #termos nas linhas e documento na coluna
dim(mtd2)   #matriz com 178 termos e 1 documento
Terms(mtd2) #exibe os termos

#18) SALVANDO O ARQUIVO EM .TXT
write.csv2(as.matrix(mtd2),"termos_texto2.csv")
#===============================================================================#


#===============================================================================#
#                         PRE-PROCESSAMENTO TEXTO 1                             #
#===============================================================================#
#1) Primeiramente se tentou ler o texto diretamente da internet. No entanto,...
#...por algum motivo desconhecido, o RStudio nao reconheceu. Dessa forma,...
#... optou-se por outra alternativa, que consiste em copiar o texto do site e...
#... colar em um arquivo .txt;
#Retirado de: https://www.logisticadescomplicada.com/cadeia-de-suprimentos-na-logistica/

#LENDO TEXTO DO SITE
texto = getURL("https://www.logisticadescomplicada.com/cadeia-de-suprimentos-na-logistica/",ssl.verifypeer = F)
txt = stri_trans_general(texto,"Latin-ASCII")
txt = htmlTreeParse(txt, useInternal = TRUE);txt
txt = unlist(xpathApply(txt, path="//p", fun = xmlValue)) #remove endereco de URL
class(txt)
print(txt)

#SALVANDO O TEXTO COMO COPIA
write.table(txt,"texto_spolm.txt")

#TRABALHANDO COM AS PARTES QUE INTERESSAM
txtnovo = txt[1:11];txtnovo

#RETIRANDO ESPACOS E O TRANSFORMANDO EM UM UNICO TEXTO
txtnovo = paste(txtnovo,collapse = " ");txtnovo

#RETIRA PONTUACAO
txtnovo = gsub(pattern = "\\W",replace = " ", txtnovo)

#REMOVE NUMEROS
txtnovo = gsub(pattern = "\\d",replace = " ", txtnovo);txtnovo

#LETRAS MAIUSCULAS EM MINUSCULAS
txtnovo = tolower(txtnovo);txtnovo

#TRNASFORMANDO OS DADOS EM UM CORPUS
t1 = VCorpus(VectorSource(txtnovo))
m1   = tm_map(t1,PlainTextDocument)
mtd1 = TermDocumentMatrix(m1) #termos nas linhas e documento na coluna
dim(mtd1) #matriz com 274 termos e 1 documento
Terms(mtd1) #exibe os termos

#REMOVE ACENTOS
txtnovo = iconv(txtnovo,to="ASCII//TRANSLIT");txtnovo

#REMOVE CARACTERES ESPECIAIS
txtnovo = gsub(pattern="\\b[A-z]\\b{1}",replace= " ",txtnovo);txtnovo

#10) REMOVE ESPACO EM BRANCO
txtnovo = stripWhitespace(txtnovo);txtnovo

#11) REMOVE STOPWORDS
txtnovo = removeWords(txtnovo,stopwords("pt")) #lista de stopwords do RStudio
txtnovo = removeWords(txtnovo,as.matrix(palavras)) #lista com outras stopwords

#12) TRNASFORMANDO OS DADOS EM UM CORPUS
t1 = VCorpus(VectorSource(txtnovo))

#13) MATRIZ TERMO-DOCUMENTO
m1   = tm_map(t1,PlainTextDocument)
mtd1 = TermDocumentMatrix(m1) #termos nas linhas e documento na coluna
dim(mtd1) #matriz com 201 termos e 1 documento
Terms(mtd1) #exibe os termos

#SALVANDO EM .TXT PARA O ALGORITMO DE STEMMING
write.table(dimnames(mtd1)$Terms,"rslp1.txt")

#15) APLICANDO O PROCESSO DE STEMMING RSLP
dados1 = read.table("rslp1.txt", header=T)
dados1 = as.matrix(dados1)
st1    = ptstem_words(dados1,algorithm = "rslp");st1

#16) JUNTANDO OS TERMOS SEMELHANTES
for (j in seq(t1)){
  t1[[j]] = gsub("chain","",  t1[[j]])
  t1[[j]] = gsub("supply","supplychain",  t1[[j]])
  t1[[j]] = gsub("armazenamento","armazenagem",  t1[[j]])
  t1[[j]] = gsub("atendimento","atender",  t1[[j]])
  t1[[j]] = gsub("suprimentos","",  t1[[j]])
  t1[[j]] = gsub("cadeias","cadeiadesuprimentos",  t1[[j]])
  t1[[j]] = gsub("cadeia","cadeiadesuprimentos",  t1[[j]])
  t1[[j]] = gsub("clientes","cliente", t1[[j]])
  t1[[j]] = gsub("conceitos","conceito",  t1[[j]])
  t1[[j]] = gsub("conhecimento","conhecer",  t1[[j]])
  t1[[j]] = gsub("consumidores","consumidor",  t1[[j]])
  t1[[j]] = gsub("empresas","empresa",  t1[[j]])
  t1[[j]] = gsub("entendem","entender",  t1[[j]])
  t1[[j]] = gsub("fluxos","fluxo",  t1[[j]])
  t1[[j]] = gsub("fornecedores","fornecedor",  t1[[j]])
  t1[[j]] = gsub("gestoes","gestao",  t1[[j]])
  t1[[j]] = gsub("importantissimo","importancia",  t1[[j]])
  t1[[j]] = gsub("introduzidos","introduzida",  t1[[j]])
  t1[[j]] = gsub("logistico","logistica",  t1[[j]])
  t1[[j]] = gsub("logisticos","logistica",  t1[[j]])
  t1[[j]] = gsub("logisticas","logistica",  t1[[j]])
  t1[[j]] = gsub("ofertar","oferta",  t1[[j]])
  t1[[j]] = gsub("supply","supplychain",  t1[[j]])
  t1[[j]] = gsub("percebidos","perceber",  t1[[j]])
  t1[[j]] = gsub("percepcao","perceber",  t1[[j]])
  t1[[j]] = gsub("planejadas","planejamento",  t1[[j]])
  t1[[j]] = gsub("planejamentos","planejamento",  t1[[j]])
  t1[[j]] = gsub("realizados","realizado",  t1[[j]])
  t1[[j]] = gsub("servicos","servico",  t1[[j]])
  t1[[j]] = gsub("decisao","",  t1[[j]])
  t1[[j]] = gsub("tomada","tomadadedecisao",  t1[[j]])
  t1[[j]] = gsub("voltados","voltadas",  t1[[j]])
  t1[[j]] = gsub("variancias","variar",  t1[[j]])
  t1[[j]] = gsub("cadeiadesuprimentosdesuprimentos","cadeiadesuprimentos",  t1[[j]])
  t1[[j]] = gsub("supplychainchain","supplychain",  t1[[j]])
  
}  
t1   = tm_map(t1,stripWhitespace) 
m1   = tm_map(t1,PlainTextDocument)
mtd1 = TermDocumentMatrix(m1)  #termos nas linhas e documento na coluna
dim(mtd1)   #matriz com 178 termos e 1 documento
Terms(mtd1) #exibe os termos

#18) SALVANDO O ARQUIVO EM .TXT
write.csv2(as.matrix(mtd1),"termos_texto1.csv")
#===============================================================================#


#-------------------------------------------------------------------------------
t   = tm_map(c(t1,t2),PlainTextDocument)
mtd = TermDocumentMatrix(t);mtd 
dim(mtd)
Terms(mtd)

for (j in seq(t)){
  t[[j]] = gsub("buscados","busca", t[[j]])
  t[[j]] = gsub("chegando","chegar", t[[j]])
  t[[j]] = gsub("chega","chegar", t[[j]])
  t[[j]] = gsub("controladas","controle", t[[j]])
  t[[j]] = gsub("conhecers","conhecer", t[[j]])
  t[[j]] = gsub("consumidores","consumidor", t[[j]])
  t[[j]] = gsub("estoques","estoque", t[[j]])
  t[[j]] = gsub("etapas","etapa", t[[j]])
  t[[j]] = gsub("excelentes","excelencia", t[[j]])
  t[[j]] = gsub("exigidos","exigencias", t[[j]])
  t[[j]] = gsub("exitir","existir", t[[j]])
  t[[j]] = gsub("fluxos","fluxo", t[[j]])
  t[[j]] = gsub("garantam","garantir", t[[j]])
  t[[j]] = gsub("garantindo","garantir", t[[j]])
  t[[j]] = gsub("informacoes","informacao", t[[j]])
  t[[j]] = gsub("integrada","integracao", t[[j]])
  t[[j]] = gsub("integrando","integracao", t[[j]])
  t[[j]] = gsub("integram","integracao", t[[j]])
  t[[j]] = gsub("lidam","lidando", t[[j]])
  t[[j]] = gsub("ligadas","ligada", t[[j]])
  t[[j]] = gsub("longas","longo", t[[j]])
  t[[j]] = gsub("necessarios","necessario", t[[j]])
  t[[j]] = gsub("objetivos","objetivo", t[[j]])
  t[[j]] = gsub("primas","", t[[j]])
  t[[j]] = gsub("prima","", t[[j]])
  t[[j]] = gsub("materias","materiaprima", t[[j]])
  t[[j]] = gsub("materia","materiaprima", t[[j]])
  t[[j]] = gsub("utilizando","utilizado", t[[j]])
  t[[j]] = gsub("utilizadas","utilizado", t[[j]])
  t[[j]] = gsub("voltadas","voltado", t[[j]])
}
outras = c("datem")
t   = tm_map(t,removeWords,outras)
t   = tm_map(t,stripWhitespace) 
m   = tm_map(t,PlainTextDocument)
mtd = TermDocumentMatrix(m)  #termos nas linhas e documento na coluna
dim(mtd)   #matriz com 285 termos e 2 documentos
Terms(mtd) #exibe os termos


#--------------------------------------------------------------------------------
#DOCUMENTO 1
#--------------------------------------------------------------------------------
#LENDO OS ARQUIVOS COM AS PALAVRAS OBTIDAS
d1 = read.csv2("termos_texto1.csv",sep=";",header=T) 
names(d1) = c("Palavras","Freqabs");d1

#ORDENANDO AS FREQUENCIAS
freq1 = rowSums(as.matrix(d1[,2]))  #transformando o data.frame em matriz
R1    = order(freq1,decreasing = F) #ordenando
names(freq1) = d1[,1]
freq1[R1]
table(freq1[R1])
#Resultados:
#- 85 palavras aparecem 1 vez
#-  9 palavras aparecem 2 vezes  
#-  4 palavras aparecem 3 vezes
#-  4 palavras aparecem 4 vezes
#-  1 palavras aparece 11 vezes

#GERANDO TABELA COM AS FREQUENCIAS ABSOLUTAS E RELATIVAS
mfreq1     = matrix(NA, nrow = nrow(d1), ncol=3);mfreq1  
rownames(mfreq1) = names(freq1[R1])
mfreq1[,1] = as.numeric(freq1[R1])
mfreq1[,2] = as.numeric(freq1[R1])/sum(as.numeric(freq1[R1]))
mfreq1[1,3] = mfreq1[1,2]
for (i in 2:nrow(mfreq1)){
  mfreq1[i,3] = mfreq1[i-1,3] + mfreq1[i,2]
}
colnames(mfreq1) = c("absoluta","relativa","acumulada")
mfreq1
write.csv2(mfreq1,"frequencias_doc1.csv")
#--------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#NUVEM DE PALAVRAS
#-------------------------------------------------------------------------------
#NUVEM DOCUMENTO 1
freqt1 = as.matrix(mfreq1) 
freqg1 = as.data.frame(freqt1)
nomes1 = rownames(freqg1)
valor1 = as.integer(freqg1[,1])
wordcloud(nomes1, valor1, min.freq=1, max.words = 100, scale = c(3,.6),  
          random.order = F,colors=brewer.pal(8,"Dark2"))
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#SELECAO DE TERMOS
#-------------------------------------------------------------------------------
mtermos = as.matrix(mtd);mtermos  #lendo a matriz
contermos = NULL
j = 1
for (i in 1:nrow(mtermos)){
  if (sum(mtermos[i,])<3){
    contermos[j] = i
    j = j + 1
  }
}
contermos #termos nao aparecem nos 3 documentos
length(contermos) #quantidade de termos que nao aparecem nos 3 documentos

mtermos_escolhidos = mtermos #copiando a matriz para manipulacao
mtermos_escolhidos = mtermos[-contermos,]
mtermos_escolhidos #matriz com os termo escolhidos
nrow(mtermos_escolhidos) #total de 77 termos
write.csv2(mtermos_escolhidos,"termos_escolhidos.csv")
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#QUANTIDADE DE GRUPOS
#-------------------------------------------------------------------------------
#Tabela com as frequencias relativas
matriz_freq_rel = matrix(NA, nrow=48,ncol=2)
matriz_freq_rel[,1] = round(mtermos_escolhidos[,1]/sum(mtermos_escolhidos[,1]),4)
matriz_freq_rel[,2] = round(mtermos_escolhidos[,2]/sum(mtermos_escolhidos[,2]),4)
matriz_freq_rel
colnames(matriz_freq_rel) = c("doc1","doc2")
rownames(matriz_freq_rel) = rownames(mtermos_escolhidos)
matriz_freq_rel
write.csv2(matriz_freq_rel,"frequencias_relativas_termos_escolhidos.csv")

#Spearman
ma = matriz_freq_rel;ma
rank1 = rank(ma[,1]);rank1
rank2 = rank(ma[,2]);rank2
dif   = rank2 - rank1;dif 
num   = 6*sum(dif^2);num 
den   = 48*((48^2) - 1);den 
rho   = 1 - (num/den);rho 
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#CONGLOMERADOS
#-------------------------------------------------------------------------------
#Calculando os centros de cada cluster
k = 2
matpad = (mtermos_escolhidos - mean(mtermos_escolhidos))/sd(mtermos_escolhidos)
v = inaparc::kmpp(matpad, k = k)$v
v
#write.csv2(v,"centros_iniciais_cluster1.csv")

#Inicializando a matriz com os graus de pertinencia
u = inaparc::imembrand(nrow(matpad), k=k)$u
u
#ui = u
#rownames(ui) = rownames(matpad)
#write.csv2(ui,"pertinencias_iniciais1.csv")

#Aplicando o fuzzy c-means
res.fcm = fcm(matpad, centers = v,  memberships = u, dmetric = "euclidean")
res.fcm

#iteracoes e funcao objetivo
#100 e 13.64126
#200 e 13.62247
#300 e 13.62235
#400 e 13.62235
#500 e 13.62235
#600 e 13.62235
#700 e 13.62235
#800 e 13.62235
#900 e 13.62235
#1000 e 13.62235


#Resultados
res.fcm$u        #pertinencias
write.csv2(res.fcm$u,"pertinencias_finais_fcm.csv")
res.fcm$v0       #centros iniciais
res.fcm$v        #centros finais
write.csv2(res.fcm$v,"centros_finais_fcm.csv")
res.fcm$func.val #funcao objetivo
res.fcm$cluster  #qual termo pertence a qual cluster
write.csv2(res.fcm$cluster,"conglomerados_fcm.csv")
res.fcm$csize    #tamanho dos clusters
res.fcm$d        #matriz com as distancias euclidianas  
write.csv2(res.fcm$d,"distancias_fcm.csv")

#Separando por termos e grupos
lgrupos = list() #Salvando os respectivos documentos em uma lista
names(res.fcm$cluster) = rownames(res.fcm$u)
lgrupos[[1]] = names(res.fcm$cluster)[which(res.fcm$cluster == 1)]
lgrupos[[2]] = names(res.fcm$cluster)[which(res.fcm$cluster == 2)]
lgrupos[[3]] = names(res.fcm$cluster)[which(res.fcm$cluster == 3)]
lgrupos[[4]] = names(res.fcm$cluster)[which(res.fcm$cluster == 4)]
lgrupos[[5]] = names(res.fcm$cluster)[which(res.fcm$cluster == 5)]
lgrupos[[6]] = names(res.fcm$cluster)[which(res.fcm$cluster == 6)]
lgrupos

#Grafico
plotcluster(res.fcm, cp=1, trans=TRUE)
res.fcm2 = ppclust2(res.fcm, "kmeans")
fviz_cluster(res.fcm2, data = matpad, 
             ellipse.type = "norm",
             palette = brewer.pal(8,"Dark2"),
             repel = TRUE, xlim=c(-2,6), ylim=c(-2,5),xlab="",ylab="", main="")
?fviz_cluster



#Valores
cmedoids = vegclust(matpad, mobileCenters = v, method = "FCMdd", m = 2)
cmedoids
cmedoids$functional #funcao objetivo
cmedoids$mobileCenters  #medoids
write.csv2(cmedoids$mobileCenters,"medoids.csv")
cmedoids$memb #pertinencias
write.csv2(cmedoids$memb,"pertinencias_medoids.csv")
cmedoids$dist2clusters #dissimilaridade
cmedoids$size #cardinalidade dos conglomerados fuzzy
defuzzify(cmedoids$memb)$cluster #qual palavra pertence a qual cluster

#Separando por termos e grupos
lgrupos_medoids = list() #Salvando os respectivos documentos em uma lista
lgrupos_medoids[[1]] = names(defuzzify(cmedoids$memb)$cluster)[which(defuzzify(cmedoids$memb)$cluster == "M1")]
lgrupos_medoids[[2]] = names(defuzzify(cmedoids$memb)$cluster)[which(defuzzify(cmedoids$memb)$cluster == "M2")]
lgrupos_medoids[[3]] = names(defuzzify(cmedoids$memb)$cluster)[which(defuzzify(cmedoids$memb)$cluster == "M3")]
lgrupos_medoids[[4]] = names(defuzzify(cmedoids$memb)$cluster)[which(defuzzify(cmedoids$memb)$cluster == "M4")]
lgrupos_medoids[[5]] = names(defuzzify(cmedoids$memb)$cluster)[which(defuzzify(cmedoids$memb)$cluster == "M5")]
lgrupos_medoids[[6]] = names(defuzzify(cmedoids$memb)$cluster)[which(defuzzify(cmedoids$memb)$cluster == "M6")]
lgrupos_medoids[[7]] = names(defuzzify(cmedoids$memb)$cluster)[which(defuzzify(cmedoids$memb)$cluster == "M1+M3")]
lgrupos_medoids[[8]] = names(defuzzify(cmedoids$memb)$cluster)[which(defuzzify(cmedoids$memb)$cluster == "M4+M5")]
length(lgrupos_medoids[[1]]) #20
length(lgrupos_medoids[[2]]) #1
length(lgrupos_medoids[[3]]) #28
length(lgrupos_medoids[[4]]) #6
length(lgrupos_medoids[[5]]) #1
length(lgrupos_medoids[[6]]) #19
length(lgrupos_medoids[[7]]) #14
length(lgrupos_medoids[[8]]) #1

#Grafico
matpad2 = matpad
fviz_cluster(object = list(data = matpad2, cluster = defuzzify(cmedoids$memb)$cluster), 
             ellipse.type = "convex", repel = T,
             palette = brewer.pal(8,"Dark2"), 
             #ggtheme = theme_minimal(),
             main = "", xlab="", ylab="", xlim=c(-2,7), ylim=c(-2,5))
#======================================================================



