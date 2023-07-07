library(psych)
library(memisc)
library(sjPlot)
library(tidyverse)
#1996 e 2000
#load (df1996 e df2000)#disponivel mediante contato


peru <- df1996%>%
  filter(pais == 604)#cuidado para colocar o valor correto
scree(peru[,1:4])
peruF <- fa(peru[,1:4],1)
peruF$TLI
peruF$loadings
peruF$RMSEA

TRIperu <- mirt(peru[,1:4], 1, itemtype = "grsm")
summary(TRIperu)
TRIperu


peru <- df2000%>%
  filter(pais == 604)#cuidado para colocar o valor correto
scree(peru[,1:4])
peruF <- fa(peru[,1:4],1)
peruF$TLI
peruF$loadings
peruF$RMSEA

TRIperu <- mirt(peru[,1:4], 1, itemtype = "grsm")
summary(TRIperu)
TRIperu


#2006
#load (total geral 2006)

peru <- total_geral2006 %>% #total geral 2006 disponível mediante contato
  filter(idenpa == 604)
peru <- remove_labels(peru)
peru <- subset(peru,select=c(p15st,
                             p16n,p22na.f,p17st,p24st.a,p24st.d,p25st,
                             p26st,p35st.d,p39st,p88n_recod,p30st_recod,
                             praticante,p47st, sexo, reeduc1,s7))
summary(peru)
imp <- mice(peru, seed=23109)# repetir o mesmo seed 
peru_m <-complete(imp, 1)
rm(imp)

fa.parallel(peru_m[1:12])# how much =?
perux<- fa(peru_m[1:12], nfactors = 4, rotate = "varimax")
perux$loadings  # em 
#peru_m:

xperu <- perux$scores
peru_m$desconfil<- xperu[,2] #factor 
peru_m$iliberal <-xperu[,1] #factor
summary(peru_m)
# iliberal
# desconf
xperu <- perux$scores
peru_m$desconfi<- xperu[,2] #factor 
peru_m$iliberal <-xperu[,1] #factor
summary(peru_m)

model_peru111 <- lm(iliberal ~ sexo + praticante + reeduc1+s7, data=peru_m) 
model_peru112 <- lm(iliberal ~ sexo + praticante + reeduc1+ s7+p47st, data=peru_m)
huxreg(model_peru111, model_peru112)
# idade e direita reduz

model_peru1 <- lm(desconfi ~ sexo + praticante + reeduc1+s7, data=peru_m) 
model_peru2 <- lm(desconfi ~ sexo + praticante + reeduc1+ s7+p47st, data=peru_m)
huxreg(model_peru1, model_peru2)
# idade aumenta
# 

#
df <- read_csv("only_means.csv")# base disponível mediante contato com os autores
#2015 e 2020
Peru <- df2%>%dplyr::filter(country == "Peru")
prop.table(table(Peru$fampart,Peru$ano),2)*100 # que tenha relevância nos dois
Peru15 <- df2%>%dplyr::filter(ano == "2015")
Peru20 <- df2%>%dplyr::filter(ano == "2020")
#ninugno(2),#30socialdemoc(2),,#Nacionalistas(2)
#50democcrist (2015),,#98outros(2015),#etnic(2015),#40liberias (2020)
# 
Peru_m_15_SocialDemoc <- glm(SocialDemoc ~ Desconfia+pratic+sexo+Escolaridade+Idade+ideologia, data=Peru15,
                             family=binomial(link=logit)) 
Peru_m_20_SocialDemoc <- glm(SocialDemoc ~ Desconfia+pratic+sexo+Escolaridade+Idade+ideologia, data=Peru20,
                             family=binomial(link=logit))
Peru_m_15_Nacionalista <- glm(Nacionalista ~ Desconfia+pratic+sexo+Escolaridade+Idade+ideologia, data=Peru15,
                              family=binomial(link=logit)) 
Peru_m_20_Nacionalista <- glm(Nacionalista ~ Desconfia+pratic+sexo+Escolaridade+Idade+ideologia, data=Peru20,
                              family=binomial(link=logit))
Peru_m_15_Ninguno <- glm(Ninguno ~ Desconfia+pratic+sexo+Escolaridade+Idade+ideologia, data=Peru15,
                         family=binomial(link=logit)) 
Peru_m_20_Ninguno <- glm(Ninguno ~ Desconfia+pratic+sexo+Escolaridade+Idade+ideologia, data=Peru20,
                         family=binomial(link=logit))
Peru_m_15_Cristiano <- glm(Cristiano ~ Desconfia+pratic+sexo+Escolaridade+Idade+ideologia, data=Peru15,
                           family=binomial(link=logit))
Peru_m_15_outros <- glm(outros ~ Desconfia+pratic+sexo+Escolaridade+Idade+ideologia, data=Peru15,
                        family=binomial(link=logit)) 
Peru_m_15_etnic <- glm(etnic ~ Desconfia+pratic+sexo+Escolaridade+Idade+ideologia, data=Peru15,
                       family=binomial(link=logit))
Peru_m_20_Liberais <- glm(Liberais ~ Desconfia+pratic+sexo+Escolaridade+Idade+ideologia, data=Peru20,
                          family=binomial(link=logit))
#ninugno(2),#30socialdemoc(2),,#Nacionalistas(2)
#50democcrist (2015),,#98outros(2015),#etnic(2015),#40liberias (2020)



# ñ usar ningunos

plot_models(Peru_m_15_SocialDemoc,Peru_m_15_Nacionalista,
            Peru_m_15_etnic,Peru_m_15_Cristiano,Peru_m_15_outros,Peru_m_20_SocialDemoc,
            Peru_m_20_Nacionalista,
            Peru_m_20_Liberais,
            vline.color = "black")
a <-plot_models(Peru_m_15_SocialDemoc,Peru_m_15_Nacionalista,
                Peru_m_15_etnic,Peru_m_15_Cristiano,Peru_m_15_outros,Peru_m_20_SocialDemoc,
                Peru_m_20_Nacionalista,
                Peru_m_20_Liberais,
                vline.color = "black", title="Associação entre probabilidade de votar no partido (por família partidária) e Desconfiança institucional no Perú em 2015 e 2020",
                legend.title = "Família",
                m.labels = c("Social Democracia 2015","Nacionalistas 2015",
                             "Étnicos e Regionais 2015","Democracia Cristã 2015",
                             "Alianças Eleitorais de origens diversas 2015",
                             "Social Democracia 2020","Nacionalistas 2020",
                             "Liberais 2020"
                ),rm.terms = ("ideologia"),
                axis.labels=c("Idade",                           "Escolaridade",                           "Gênero: Mulher",                           "Prática Religiosa",                           "Desconfiança Institucional"),
                std.est = T,show.p = T,ci.lvl = 0.9, p.shape = TRUE,
                colors=c("#05668d",
                                  "#f58549","pink",
                                  "#8b8c89","#86bbd8","#723d46",
                                  "#f58549","pink"))
                                  
a

#2015
load("Latinobarometro_2015_Esp.rdata")#baixar esse arquivo de forma gratuita no site do latinobarometro
peruuu <- Latinobarometro_2015_Esp%>%
  filter(idenpa == 604)
prop.table(table(peruuu$P23TGBSM))*100
table(peruuu$P16TGB.B)#POLICIA
table(peruuu$P19ST.C)#partidos
table(peruuu$P16ST.F)#congresso
table(peruuu$P19ST.F)# estado
table(peruuu$P19N.H)#justiça eleitoral
peruuu$P16TGB.B[peruuu$P16TGB.B  == -1] <- NA # gerar missing
peruuu$P16TGB.B[peruuu$P16TGB.B  == -2] <- NA
peruuu$P19ST.C[peruuu$P19ST.C  == -1] <- NA # gerar missing
peruuu$P19ST.C[peruuu$P19ST.C  == -2] <- NA
peruuu$P16ST.F[peruuu$P16ST.F  == -1] <- NA # gerar missing
peruuu$P16ST.F[peruuu$P16ST.F  == -2] <- NA
peruuu$P19ST.F[peruuu$P19ST.F  == -1] <- NA # gerar missing
peruuu$P19ST.F[peruuu$P19ST.F  == -2] <- NA
peruuu$P19N.H[peruuu$P19N.H  == -1] <- NA # gerar missing
peruuu$P19N.H[peruuu$P19N.H  == -2] <- NA


# novas de 2015

table(peruuu$P69ST.C)#desaprova casamento gay# só tem na de 2015
peruuu$P69ST.C[peruuu$P69ST.C  == -1] <- NA


#variaveis uso posterior
table(peruuu$reedad)#idade em faixas, sem NA
table(peruuu$REEDUC_1)# educ em niveis sete faixas, sem NA
table(peruuu$P27ST)# ideologia remover -8,-2,-1 - 10 direita, 0 esquerda
table(peruuu$S12)#2 mulher

peruuu$Escolaridade <- peruuu$REEDUC_1
peruuu$Idade <- peruuu$reedad 
peruuu$PP27ST[peruuu$P27ST  == -8] <- NA
peruuu$P27ST[peruuu$P27ST  == -1] <- NA # gerar missing
peruuu$P27ST[peruuu$P27ST  == -2] <- NA
peruuu$P27ST -> peruuu$Ideologia
peruuu$Mulher <- as.numeric(peruuu$S12) == 2
table(peruuu$Mulher)

peruuu <- subset(peruuu, select=c(P16TGB.B,P19ST.C,P16ST.F,
                                  P19ST.F,P19N.H,P69ST.C,Escolaridade,Idade,Ideologia,
                                  Mulher,fampart,P23TGBSM))

# partidos
table(peruuu$fampart)
peruuu$SemPartido_Nulo_Branco <- peruuu$fampart==0
peruuu$QlqerPartido <- peruuu$fampart!=0

prop.table(table(peruuu$P23TGBSM))*100
peruuu$NacionalistaPeruano<- peruuu$P23TGBSM ==604002
peruuu$Aprista<- peruuu$P23TGBSM ==604001
peruuu$catadao<- peruuu$P23TGBSM ==604011
peruuu$fujimorista<- peruuu$P23TGBSM ==604008

table(peruuu$QlqerPartido)


####
scree(peruuu[,1:5])
x <- fa(peruuu[,1:5],1)
hist(x$scores)
as.numeric(x$scores) -> peruuu$Desconfia
hist(peruuu$Desconfia)
peruuu$contraCasGay <- peruuu$P69ST.C
peruuu$ExtDireita <- peruuu$Ideologia == 10
peruuu_2015_ExtDireita_modelo <- glm(ExtDireita ~ Desconfia+
                                       contraCasGay+Escolaridade+
                                       Idade+Mulher,data=peruuu,
                                     family=binomial(link=logit))
summary(peruuu_2015_ExtDireita_modelo)
plot_model(peruuu_2015_ExtDireita_modelo, vline.color = "orange")


peruuu_2015_QlqerPartido <- glm(QlqerPartido~ Desconfia+
                                  contraCasGay+Escolaridade+
                                  Idade+Mulher+Ideologia,data=peruuu,
                                family=binomial(link=logit))
summary(peruuu_2015_QlqerPartido)
peruuu_2015_SemPartido_Nulo_Branco <- glm(SemPartido_Nulo_Branco~ Desconfia+
                                            contraCasGay+Escolaridade+
                                            Idade+Mulher+Ideologia,data=peruuu,
                                          family=binomial(link=logit))
summary(peruuu_2015_SemPartido_Nulo_Branco)

peruuu_2015_Aprista <- glm(Aprista~ Desconfia+
                             contraCasGay+Escolaridade+
                             Idade+Mulher+Ideologia,data=peruuu,
                           family=binomial(link=logit))
peruuu_2015_catadao <- glm(catadao~ Desconfia+
                             contraCasGay+Escolaridade+
                             Idade+Mulher+Ideologia,data=peruuu,
                           family=binomial(link=logit))
peruuu_2015_fujimorista <- glm(fujimorista~ Desconfia+
                                 contraCasGay+Escolaridade+
                                 Idade+Mulher+Ideologia,data=peruuu,
                               family=binomial(link=logit))

peruuu_2015_NacionalistaPeruano <- glm(NacionalistaPeruano~ Desconfia+
                                         contraCasGay+Escolaridade+
                                         Idade+Mulher+Ideologia,data=peruuu,
                                       family=binomial(link=logit))


plot_models(peruuu_2015_NacionalistaPeruano,
            peruuu_2015_Aprista,peruuu_2015_catadao,
            peruuu_2015_fujimorista,
            peruuu_2015_QlqerPartido, peruuu_2015_SemPartido_Nulo_Branco,
            vline.color = "orange")

plot_models(peruuu_2015_NacionalistaPeruano,
            peruuu_2015_Aprista,peruuu_2015_catadao,
            peruuu_2015_fujimorista,
            peruuu_2015_QlqerPartido, peruuu_2015_SemPartido_Nulo_Branco,
            vline.color = "orange",
            title="Probabilidade de votar no partido, Perú, 2015",
            legend.title = "Partidos/Atitude",m.labels = c("Partido Nacionalista Peruano (PNP)",
                                                           "Partido Aprista Peruano (PAP)",
                                                           "Unidad Nacional (Partido Popular Cristiano-Solidaridad Nacional)/Alianza por el Gran Cambio",
                                                           "Sí Cumple/Alianza Fujimorista (SC)/Fuerza 2011",
                                                           "Menção a qualquer partido",
                                                           "Nenhum/Nulo/Branco"),
            axis.labels=c("Ideologia(direita)", "Gênero: Mulher","Idade","Escolaridade",
                          "Contrariedade ao casamento gay",
                          "Desconfiança institucional"),
            std.est = T,show.p = T,ci.lvl = 0.9, p.shape = TRUE, colors=c("black","grey","green",
                                                                                 "darkblue",
                                                                                 "orange",
                                                                                 "pink"))