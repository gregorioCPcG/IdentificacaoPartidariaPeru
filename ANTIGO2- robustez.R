
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