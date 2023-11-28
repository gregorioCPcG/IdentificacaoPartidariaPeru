rm(list=ls())
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
#### 
#criando df
mergao <- read_dta("WVS_TimeSeries_4_0.dta")
print(mergao$S003)#paises lista
table(mergao$S003)
df <- mergao %>%
  filter(S003 == 604)#peru

table(df$S002VS)#ondas

df <- df %>%
  filter(S002VS != 5)#remover onda 5 inapta para uso
table(df$S002VS)
table(df$S003)#peru
table(df$S003, df$S002VS)
df -> df2

df <- remove_labels(df)
# Lista de variables deseadas
variaveis_desejadas <- c("X001", "X025R", "X048ISO", "X049", "X051", "X007", "X003R2", "X028", "F025", "E023", "E033",
                         "E179WVS", "E069_06", "E069_07", "E069_12", "E069_17", "F028", "F118", "F120", "E036",
                         "E037", "E040", "E114", "E116", "E117", "E235","X045","X047R_WVS","E039")
#29
# Comprobar si hay variables presentes en el marco de datos.
variaveis_presentes <- colnames(df) %in% variaveis_desejadas

# Mostrar las variables que están presentes.
print(colnames(df)[variaveis_presentes])
n <- colnames(df)[variaveis_presentes]
# Verifique las variables que faltan con la lista deseada
variaveis_faltando <- variaveis_desejadas[!variaveis_desejadas %in% n]

# Mostrar variables faltantes
print(variaveis_faltando)#zero!


#29 + 2 = 31
df <- subset(df, select = c(S002VS, S003, X001, X025R, X048ISO, X049, X051, X007, X003R2, X028, F025, E023,
                            E033, E179WVS, E069_06, E069_07, E069_12, E069_17, F028, F118, F120, E036,
                            E037, E040, E114, E116, E117, E235, X045, X047R_WVS,E039))

write.csv(df,"df.csv")

df2 <- subset(df2, select = c(S002VS, S003, X001, X025R, X048ISO, X049, X051, X007, X003R2, X028, F025, E023,
                              E033, E179WVS, E069_06, E069_07, E069_12, E069_17, F028, F118, F120, E036,
                              E037, E040, E114, E116, E117, E235, X045, X047R_WVS,E039))
df2 <- write_sav(df2,"dfao.sav")
#consultar todo lo referente a códigos en los archivos F00012255-WVS_TimeSeries_1981_2020_CountrySpecificCodes.xlsx e 
#F00003844-WVS_Time_Series_List_of_Variables_and_equivalences_1981_2022_v3_1.xlsx
