cor_de_olhos <- c(2,2,4,1,5,5,5,6,1,3,6,3,1,4)
cor_de_olhos_categoricos <- factor(cor_de_olhos) 
levels(cor_de_olhos_categoricos)
levels(cor_de_olhos_categoricos) <- c("Ambar","Azul", "Marrom","Cinza","Verde","Avelã") 
levels(cor_de_olhos_categoricos)
cor_de_olhos_categoricos
escore_de_empatia <- c(15,21,45,32,61,74,53,92,83,22,67,55,42,44) 
olhos_e_empatia <- list(codigo_de_cor=cor_de_olhos, olhos=cor_de_olhos_categoricos, empatia=escore_de_empatia) 
olhos_e_empatia
olhos_e_empatia$empatia
olhos_e_empatia$empatia[4]

plot(olhos_e_empatia$olhos, olhos_e_empatia$empatia,  xlab= "Cor dos Olhos", ylab = "Nota do Grau de Empatia", main = "Relação da Cor dos Olhos e Grau de empatia")
##ESTATISTICAS
#Verifico quanto a media de enpatia difere de 30 por exemplo
t.resultado <- t.test(olhos_e_empatia$empatia, mu = 30) 
#Obsenho o resumo do teste e o próprio programa me diz se aceito ou rejeiro a hipótese nula
t.resultado
#valor T
t.resultado$statistic
#graus de liberdade (n-1)
t.resultado$parameter
#Valor P
t.resultado$p.value
#Intervalo de confiança
t.resultado$conf.int
#media real
t.resultado$estimate
#Media estimada para hipotese nula
t.resultado$null.value
#Descrição da hipótese alternativa
t.resultado$alternative
#tipo de teste T
t.resultado$method
#nome dos dados
t.resultado$data.name

df <- data.frame(cor_de_olhos, cor_de_olhos_categoricos, escore_de_empatia)
df
#Empatia do registro 7
df[7,3]
#Registro 7
df[7,]
#Coluna categórica de cor dos olhos
df[,2]
#Atalho para edição do DataFrame
edit(df)
#Recuperamos os escores de empatia por cor dos olhos
df.azul <- df$escore_de_empatia[df$cor_de_olhos_categoricos=="Azul"]
df.verde <- df$escore_de_empatia[df$cor_de_olhos_categoricos=="Verde"]
df.avela <- df$escore_de_empatia[df$cor_de_olhos_categoricos=="Avelã"]
#Obtemos as 
df.medias <- c(mean(df.azul),mean(df.verde),mean(df.avela)) 
df.medias
df.tamanho <- c(length(df.azul),length(df.verde),length(df.avela)) 
df.tamanho
df.cores <- c("Azul","Verde","Avelã")
df.cores
df.medias_totais <- data.frame(cores=df.cores, media_empatia=df.medias, ocorrencias=df.tamanho)
df.medias_totais
#Fazemos o teste-t
t.test(df.medias_totais$media_empatia, mu=40)

t.test(df.medias_totais$media_empatia, mu= 53.39)