#Trabajo de vinculación Maestria en Ciencia de Datos
#Etapa 2: Aplicación de las correlaciones
#Maestrante: Wilson Arroyo

# Relación entre Conocimiento y Compromiso:
# Investigar la correlación entre el nivel de conocimiento sobre la gestión de residuos
# y el compromiso con el reciclaje.

setwd("C://Maestria_CIencia_Datos/Proyecto_vinculacion/Set Datos")

library(dplyr)
# Preguntas de conocimiento de gestion de residuos: 1,2,3,5
# Preguntas del compromiso con el reciclaje : 6,7,10,11,13,14,15,16,17

#https://www.medwave.cl/series/MBE04/5266.html#
# https://rpubs.com/osoramirez/111403
# https://biocosas.github.io/R/060_analisis_datos_categoricos.html
table(datos_o$Q1_Nvl_conocimiento)
table(datos_o$Q6_Compromiso_reci_ma)

table(datos_o$Q1_Nvl_conocimiento, datos_o$Q6_Compromiso_reci_ma)

#Relación 1-6
#H0: EL nivel de conocimiento de gestion de residuos Es INDEPENDIENTE del compromiso de reciclar
#H1: EL nivel de conocimiento de gestion de residuos DEPENDE del compromiso de reciclar

tabla_1<-table(datos_o$Q1_Nvl_conocimiento,datos_o$Q6_Compromiso_reci_ma)
tabla_1
plot(tabla_1, col = c("red", "blue"), main = "Nvl Cncmnt resid vs. Compromiso rcclje")

chisq.test(tabla_1)

# Pearson's Chi-squared test
# 
# data:  tabla_1
# X-squared = 3.525, df = 2, p-value = 0.1716

#Concluisión: El p-valor = 0.176 > 0.05 indica que se acepta H0: EL conocimiento es indep del compromiso de reciclar

##################
#Relación 1-7
#H0: EL nivel de conocimiento de gestion de residuos Es INDEPENDIENTE de disposición a participar en educacion en gestion de residuos
#H0: EL nivel de conocimiento de gestion de residuos Es DEPENDIENTE de disposición a participar en educacion en gestion de residuos

tabla_1<-table(datos_o$Q1_Nvl_conocimiento,datos_o$Q7_dis_capaci_gst_res_sol)
tabla_1
plot(tabla_1, col = c("red", "blue"), main = "Nvl Cncmnt resid vs. Educac rcclje")

chisq.test(tabla_1)

# > chisq.test(tabla_1)
# 
# Pearson's Chi-squared test
# 
# data:  tabla_1
# X-squared = 7.9872, df = 2, p-value = 0.01843
# 
# Aviso:
# In chisq.test(tabla_1) : Chi-squared approximation may be incorrect

#Concluisión: El p-valor = 0.01843 < 0.05 indica que se rechaza H0: EL conocimiento es indep del compromiso de reciclar

fisher.test(tabla_1)
# > fisher.test(tabla_1)
# 
# Fisher's Exact Test for Count Data
# 
# data:  tabla_1
# p-value = 0.01887
# alternative hypothesis: two.sided
