#carregando os pacotes a serem utilizados
library(epiDisplay)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(data.table)
library(forcats)
#INFORMANDO ONDE ESTÁ OS ARQUIVOS BAIXADOS DO SISMIGRA
setwd("~/Documentos/data_labe/sismigra")

#trazendo a base de dados de 2021
#com todas variaveis como texto
#o separador é ;
b21 <- read.csv("sismigra_jan_out_2021.csv", sep = ";", colClasses = "character")

##trazendo os dados de 2020
b20 <- read.csv("sismigra_2020.csv", sep = ";", colClasses = "character")

#vendo os nomes das variaveis
names(b21)
##a base 2020 tem uma variavel a menos 
names(b20)
###em 20 tem amparo, amparo_descricao, amparo_grupo* e grupo_amparo_legal
###amparo_grupo seria a variavel a mais

#mudando o nome das variaveis para minusculo
names(b21) <- tolower(names(b21))
names(b20) <- tolower(names(b20))

#tabela de amparo_grupo presente apenas em b20
tab1(b20$amparo_grupo, sort.group="decreasing", graph=F)
##é mais detalhado porém será excluído para ficar coerente com a de 2021
b20 <- b20[,-4]

#juntando as duas bases pelas variáveis
migr <- rbindlist(list(b20, b21), fill = T)

#tabela da data de registro
table(b21$dataregistro) #possui apenas o ano
table(b20$dataregistro) #possui apenas o ano
#tabela da data de registro do total
tab1(migr$dataregistro, graph=F)

#tabela da data de entrada
table(b21$dataentrada)# existem apenas anos também
table(b20$dataentrada)# existem apenas anos também
##mudando para formato numérico
b21$dataentrada <- as.numeric(b21$dataentrada)
b20$dataentrada <- as.numeric(b20$dataentrada)
##nas introduzido por terem símbolos não numéricos
#repetindo a tabela
table(b21$dataentrada) #primeira entrada de 1917 até 2021
table(b20$dataentrada) #primeira entrada de 1917 até 2021
####possui erro de entrada? quem entra em 1917 teria 104 anos de diferença para data de registro
tab1(migr$dataentrada, graph=F)

#variável amparo registro qual amparo legal de entrada ou registro
tab1(b21$amparo, graph = F, sort.group = "decreasing")
tab1(b20$amparo, graph = F, sort.group = "decreasing")
#o primeiro amparo (273) refere-se ao pedido de residencia por 2 anos
#o segundo amparo (290) refere-se ao pedido de refugiado
tab1(migr$amparo, graph = F, sort.group = "decreasing")


#tabela grupo amparo
tab1(b21$grupo_amparo_legal, graph = F, sort.group = "decreasing")
tab1(b20$grupo_amparo_legal, graph = F, sort.group = "decreasing")


#cruzando o grupo amparo com o amparo
tabpct(b21$grupo_amparo_legal, b21$amparo)
tabpct(b20$grupo_amparo_legal, b20$amparo)

#tabela de classificacao de registro
##totalmente preenchido usar este para análise resumida?
tab1(b21$classificacao_registro, sort.group = "decreasing", graph=F)
tab1(b20$classificacao_registro, sort.group = "decreasing", graph=F)
tab1(migr$classificacao_registro, sort.group = "decreasing", graph=F)


#tabela do estado de residencia
tab1(b21$unidade_fed_residencia, sort.group = "decreasing")
tab1(b20$unidade_fed_residencia, sort.group = "decreasing", graph=F)

#tabela do pais de nascimento
tab1(b21$pais_nascimento, sort.group = "decreasing")
tab1(migr$pais_nascimento, sort.group = "decreasing", graph=F)

#gerando uma tabela por dplyr
tab1 <-  migr %>% group_by(pais_nascimento) %>% count() %>% arrange(desc(n))
View(tab1)
write.csv(tab1, "contagem_paisnasc.csv", row.names = T)
#contando as porcentagens
#venezuela
110225/216738
#congo
223/216738

#gráfico do pais de nascimento dos top50
tab1 <- tab1[1:50,]

#tentando um gráfico
##ficou ruim porque tem muitos valores
ggplot(tab1, aes(x=reorder(pais_nascimento, n), y=n, fill=pais_nascimento))+
  geom_bar(stat = 'identity', show.legend = F)+
  coord_flip()

#tabela do continente
tab1(b21$continente, sort.group = "decreasing", graph=F)
tab1(b20$continente, sort.group = "decreasing", graph=F)
tab1(migr$continente, sort.group = "decreasing", graph=F)

#gráfico do continente africano
##tirando apenas africa
africa <- subset(migr, continente=="ÁFRICA")

#muitos nomes, tb não usei
ggplot(africa, aes(fct_infreq(pais_nascimento)))+
  geom_bar(show.legend=F)+
  coord_flip()

#retirando apenas os do congo
congo <- subset(migr, pais_nascimento=="REPÚBLICA DEMOCRÁTICA DO CONGO")
#tabela da faixa etária
tab1(b21$faixa_etaria, graph=F)
tab1(b20$faixa_etaria, graph=F)
#FAIXA ETARIA DO CONGO
tab1(congo$faixa_etaria, graph=F)

#grafico da fx etaria dos congoleses
ggplot(congo, aes(faixa_etaria, fill=faixa_etaria))+
  geom_bar(show.legend = F)+
  labs(x="Faixa etária", y="Quantidade (n)")

#tabela do gênero
tab1(b21$sexo, sort.group = "decreasing", graph=F)
tab1(b20$sexo, sort.group = "decreasing", graph=F)
#genero dos congoleses
tab1(congo$sexo, sort.group = "decreasing", graph=F)

#grafico do genero pela idade
ggplot(congo, aes(faixa_etaria, fill=sexo))+
  geom_bar(position='fill')+
  labs(x="Faixa etária", y="Porcentagem", fill="Gênero")
#TABELA PELA COLUNA
tabpct(congo$faixa_etaria, congo$sexo, graph=F)


#tabela do estado civil
tab1(b21$estado_civil, sort.group = "decreasing", graph=F)
tab1(b20$estado_civil, sort.group = "decreasing", graph=F)
tab1(congo$estado_civil, sort.group = "decreasing", graph=F)

#tabela da profissao
tab1 <- b21 %>% group_by(profissao) %>% count()
tab2 <- b20 %>% group_by(profissao) %>% count()
tab3 <- congo %>% group_by(profissao) %>% count()
View(tab1)
View(tab2)
View(tab3)

#exportando a tabela de profissoes dos congoleses
write.csv(tab3, "prof_congo.csv", row.names = T)


#tabela do status
tab1(b21$status, sort.group="decreasing", graph=F)
tab1(b20$status, sort.group="decreasing", graph=F)
tab1(congo$status, sort.group="decreasing", graph=F)


#tabela da classificacao revisada
tab1(b21$classificacao_revisada, sort.group="decreasing", graph=F)
tab1(b20$classificacao_revisada, sort.group="decreasing", graph=F)
##CORRIGINDO ERRO DE DIGITAÇÃO
congo$classificacao_revisada[which(congo$classificacao_revisada=="NÃO APLICÁVEIS")] <- "NÃO APLICÁVEL"
#PEDINDO A TABELA
tab1(congo$classificacao_revisada, sort.group="decreasing", graph=F)


#cruzando as classificacoes
tabpct(b21$classificacao_revisada, b21$classificacao_registro, graph = F)
tabpct(b20$classificacao_revisada, b20$classificacao_registro, graph = F)
tabpct(congo$classificacao_revisada, congo$classificacao_registro, graph = F)

##exportando a tabela total de análise
write.csv2(migr, "imigrantes_20202021.csv", row.names = F)

