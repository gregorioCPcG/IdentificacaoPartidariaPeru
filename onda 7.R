#Peru onda 7

#pacotes
library(haven)
library(mice)
library(labelled)
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(mirt)
mirtCluster()
library(marginaleffects)
library(sjPlot)
library(caret)
library(gridExtra)
library(coefplot)
library(ltm)
library(lavaanPlot)
library(scales)
library(semTools)
library(gridExtra)
#### 
rm(list=ls())
df <- read_csv("df.csv")
df <- df %>%
  filter(S002VS == 7)# onda 

dfao <- read_sav("dfao.sav")
dfao <- dfao %>%
  filter(S002VS == 7)# the same

print(dfao$X001)
table(df$X001)
df$Gender <- df$X001#gender

print(dfao$X003R2)
table(df$X003R2, useNA = "always")
df$X003R2 -> df$idade_faixa#idade_faixa

print(dfao$X025R)
table(df$X025R, useNA = "always")
df$escolaridade_niveis <- ifelse(df$X025R == -1, NA, df$X025R)
table(df$escolaridade_niveis, useNA = "always")#ok - escolaridade_niveis



#renda--
dfao$X047R_WVS
table(df$X047R_WVS)
df$rend_subj <- factor(df$X047R_WVS, levels = c(-2, -1,1, 2, 3), labels = c("DK","DK", "Low", "Medium/High", "Medium/High"))
df$rend_subj <- factor(df$rend_subj, levels = c("DK", "Low", "Medium/High"))
df$rend_subj <- relevel(df$rend_subj, ref = "Medium/High")

table(df$rend_subj)
sum(is.na(df$rend_subj))#deu zero?
#acima strcuture


#Interesse
dfao$E023
table(df$E023)
df$interesse <- df$E023
df$interesse <- ifelse(df$interesse == -1, NA, df$interesse)
df$interesse <- ifelse(df$interesse == -2, NA, df$interesse)
df$interesse <- -1*df$interesse
table(df$interesse)
df$interesse <- scales::rescale(df$interesse, to = c(0, 1))
table(df$interesse)

#esquerda direita
dfao$E033
table(dfao$E033)
# Criando a coluna df$autoloc_dir_esq com base na coluna df$E033
df$autoloc_dir_esq <- factor(ifelse(df$E033 %in% c(1, 2, 3, 4), "Left",
                                    ifelse(df$E033 == 5, "Center",
                                           ifelse(df$E033 %in% c(6, 7, 8, 9, 10), "Right", "DK"))),
                             levels = c("Left", "Center", "Right", "DK"))

# Visualizando o resultado
table(df$autoloc_dir_esq)


#[,14]
#dep arrumar os 4 blocos - com o primeiro script (o antigo)
dfao$E179WVS
df$E179WVS <- ifelse(is.na(df$E179WVS), -66, df$E179WVS)
table(dfao$E179WVS,useNA = "always" )
table(df$E179WVS)
options(max.print = 4438)
print(dfao$E179WVS)


df$partidos <- memisc::recode(as.factor(df$E179WVS), "Aprista" <- c(604002),
                               "Socialistas/Progressistas" <- c(604001,604023,604040,604005,604024),
                               "Democracia Cristã/Liberais"<-c(604006,604017,604046,604036),
                               "Fujimoristas"<-c(604026,604044,604008),
                               "Outros"<-c(604007,604027,604039,604042,604047,604048,
                                           604035,604021,-1,-2,4,604025,604027,604028,604029,604030,604031,
                                           604034,604037))

table(df$partidos)

df$Aprista <- df$partidos == "Aprista"
df$SocProg <- df$partidos == "Socialistas/Progressistas"
df$CristLib <- df$partidos == "Democracia Cristã/Liberais"
df$Fuji <- df$partidos == "Fujimoristas"

table(df$Aprista)
table(df$SocProg)
table(df$CristLib)
table(df$Fuji)



#iisues
dfao$E069_07#desconfia parlamento
sum(is.na(df$E069_07))
dfao$E069_12#desconfia political parties
sum(is.na(df$E069_12))
dfao$E069_17#desconfia Courts
sum(is.na(df$E069_17))
dfao$E069_06#desconfia police
sum(is.na(df$E069_06))
table(df$E069_06)
dfao$F118#liberal casamento gay  # INVERTER!
dfao$F120#liberal aborto # INVERTER!
dfao$F028#religiosidade_invertida  # INVERTER!
dfao$E039#competicao é ruim # INVERTER! 
dfao$E037#pessoas mais responsáveis
dfao$E040#sucesso_é_sorte # INVERTER!
dfao$E036#governo(estatista)  # INVERTER!
dfao$E114#democratic maior valor (contra strong leader) # INVERTER!
dfao$E116#democratic maior valor (contra army rule) # INVERTER!
dfao$E117#valors positicos anntidemoc
dfao$E235#favor da demcorcracia - INVERTER!
table(dfao$E235)#remover



df <- subset(df, select=c(Gender,idade_faixa,escolaridade_niveis,rend_subj,
                          autoloc_dir_esq, interesse,
                          Aprista,SocProg,CristLib,Fuji,
                          E069_07,E069_12,E069_17,E069_06,F028,
                          E037,E040,E036,E116,E114,E117,F120,F118,E039,E235))
df[,11:25]#issues
summary(df[,11:25])#ver se tem que remover algo
# Atualizar valores menores que 0 por NA em cada coluna no intervalo 
for (i in 11:25) {
  df[, i][df[, i] < 0] <- NA
}#gera os NAS (menor q zero é NA no WVS)

# Visualizar o resultado
print(df[, 11:25])
summary(df[,11:25])
imp <- mice(df[,11:25], seed=23109)# o nome da base e a seed sempre essa 23109
df[,11:25] <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(df[,11:25])

df$F118_invertido <- -1*df$F118
df$F118_invertido <- scales::rescale(df$F118_invertido, to = c(0, 1))
table(df$F118)
table(df$F118_invertido)

df$F120_invertido <- -1*df$F120
df$F120_invertido <- scales::rescale(df$F120_invertido, to = c(0, 1))
df$F028_invertido <- -1*df$F028
df$F028_invertido <- scales::rescale(df$F028_invertido, to = c(0, 1))
df$E039_invertido <- -1*df$E039
df$E039_invertido <- scales::rescale(df$E039_invertido, to = c(0, 1))
df$E040_invertido <- -1*df$E040
df$E040_invertido <- scales::rescale(df$E040_invertido, to = c(0, 1))
df$E036_invertido <- -1*df$E036
df$E036_invertido <- scales::rescale(df$E036_invertido, to = c(0, 1))
df$E116_invertido <- -1*df$E116
df$E116_invertido <- scales::rescale(df$E116_invertido, to = c(0, 1))
df$E114_invertido <- -1*df$E114
df$E114_invertido <- scales::rescale(df$E114_invertido, to = c(0, 1))
df$E235_invertido <- -1*df$E235
df$E235_invertido <- scales::rescale(df$E235_invertido, to = c(0, 1))


table(df$E235)
table(df$E235_invertido)

df <- subset(df, select=c(Gender,idade_faixa,escolaridade_niveis,rend_subj,
                          autoloc_dir_esq, interesse,
                          Aprista,SocProg,CristLib,Fuji,
                          E069_07,E069_12,E069_17,E069_06,F118_invertido,
                          F120_invertido,F028_invertido,
                          E037,E040_invertido,E036_invertido,
                          E116_invertido,E114_invertido,E117,E039_invertido,E235_invertido))

summary(df[,1:10])#verificar


df$E069_07 <- scales::rescale(df$E069_07, to = c(0, 1))#colocar tudo entre 0 e 1
df$E069_12 <- scales::rescale(df$E069_12, to = c(0, 1))
df$E069_17 <- scales::rescale(df$E069_17, to = c(0, 1))
df$E069_06 <- scales::rescale(df$E069_06, to = c(0, 1))
df$E037 <- scales::rescale(df$E037, to = c(0, 1))
df$E117 <- scales::rescale(df$E117, to = c(0, 1))

summary(df[,11:25])#verificar



psych::scree(df[,11:25])
nfactors((df[,11:25]))
# Especifique o modelo de CFA com 3 fatores
model <- '
  # Especificação do modelo fatorial
  LiberalFundamentalismo =~ F118_invertido + F120_invertido + F028_invertido
  DesconfiançaInst =~ E069_07 + E069_12 + E069_17 + E069_06
  ManoDuro =~ E117 + E114_invertido + E116_invertido + E235_invertido
  ProMercado_Meritocracia =~E039_invertido+E037 + E040_invertido+ E036_invertido 

  # Especificação das variâncias dos erros
  E069_07 ~~ E069_07
  E069_12 ~~ E069_12
  E069_17 ~~ E069_17
  E069_06 ~~ E069_06
  F118_invertido ~~ F118_invertido
  F120_invertido ~~ F120_invertido
  F028_invertido ~~ F028_invertido
  E037 ~~ E037
  E039_invertido ~~ E039_invertido
  E040_invertido ~~ E040_invertido
  E036_invertido ~~ E036_invertido
  E116_invertido ~~ E116_invertido
  E114_invertido ~~ E114_invertido
  E117 ~~ E117
'
dfao$E037#


# Ajuste do modelo de CFA
fit <- cfa(model, data = df)

summary(fit)
#> ver resultado na imagem ethiopia 2.bmp


# Calcular medidas de ajuste
semTools::fitmeasures(fit, c("tli", "cfi", "rmsea", "srmr"))


lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = FALSE)
scores <- lavPredict(fit)
scores[,1] -> df$fundamentalism
scores[,2] -> df$desconfiaInst
dfao$E117#ok
scores[,3] -> df$manoduro_adptado
dfao$E039 # competição é ruim, mas tá invertido , logo é promercado
scores[,4] -> df$promercado


par(mfrow = c(2, 2)) # Define uma matriz de plotagem 2x2

hist(df$fundamentalism, breaks = 50, main = "2018 Liberal/Fundamentalismo")
hist(df$desconfiaInst, breaks = 100, main = "2018 Desconfiar de las instituciones")
hist(df$manoduro_adptado, breaks = 100, main = "2018 Mano Duro Adaptado")
hist(df$promercado, breaks = 100, main = "2018 Pro Mercado")



rm(scores,model)

df <- subset(df, select=c(Gender,idade_faixa,escolaridade_niveis,rend_subj,
                          autoloc_dir_esq, interesse,
                          Aprista,SocProg,CristLib,Fuji,desconfiaInst,manoduro_adptado,promercado,fundamentalism))
rm(dfao,fit)

#modelos
Aprista <- glm(Aprista~Gender+idade_faixa+escolaridade_niveis+rend_subj+
                 autoloc_dir_esq+interesse+
                 desconfiaInst+manoduro_adptado+promercado+fundamentalism
               ,data = df, family=binomial(link=logit))

SocProg<- glm(SocProg~Gender+idade_faixa+escolaridade_niveis+rend_subj+
                autoloc_dir_esq+interesse+
                desconfiaInst+manoduro_adptado+promercado+fundamentalism
              ,data = df, family=binomial(link=logit))

CristLib <- glm(CristLib~Gender+idade_faixa+escolaridade_niveis+rend_subj+
                  autoloc_dir_esq+interesse+
                  desconfiaInst+manoduro_adptado+promercado+fundamentalism
                ,data = df, family=binomial(link=logit))

Fuji <- glm(Fuji~Gender+idade_faixa+escolaridade_niveis+rend_subj+
              autoloc_dir_esq+interesse+
              desconfiaInst+manoduro_adptado+promercado+fundamentalism
            ,data = df, family=binomial(link=logit))

tab_model(Aprista,SocProg,CristLib,Fuji,
          show.ci = F, auto.label = F, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars",show.p=T,p.threshold = c(0.1,0.05,0.01))
plot_models(Aprista,SocProg,CristLib,Fuji)
a <- plot_models(Aprista,SocProg,CristLib,Fuji,auto.label=F,show.values = F, 
                 show.p = T, p.shape = TRUE, digits=2, std.est=TRUE, 
                 p.threshold = c(0.1, 0.05, 0.01),
                 vline.color = "black",dot.size = 3, spacing=0.7, ci.lvl=0.95, grid=F,
                 legend.title = "Bloco partidário",
                 rm.terms = c("autoloc_dir_esqCenter","autoloc_dir_esqRight",
                              "autoloc_dir_esqDK", "interesse","rend_subjDK"),
                 axis.labels = c("Fundamentalismo","Pro-Mercado","Mano Duro Adaptado",
                                 "Desconfiar de las instituciones",
                                 "Pobre (autodefinición)",
                                 "Educación","Edad","Género"),
                 m.labels = c("Aprista", "Socialista/ Progressista",
                              "Democr.Cristã / Liberais","Fujimoristas",
                              "2018"))+theme_classic()
a + labs(title = "2018",
         caption= "Nota: Se han omitido las variables de control, los modelos completos se pueden encontrar en el apéndice online")
tab_model(Fuji)#conferir


