#Trabajo de vinculación Maestria en Ciencia de Datos
#Etapa 1: Cargar los datos del archivo xls
#Maestrante: WIlson Arroyo


setwd("C://Maestria_CIencia_Datos/Proyecto_vinculacion/Set Datos")

#GRUPO 1 FINAL
# https://gonzalezgouveia.com/importar-datos-de-excel-a-r/#Como_importar_de_excel_a_R_con_codigo
# library(readxl)
library(readxl)
library(dplyr)#Para sustituir los null por ceros

datos_G1_final <- read_excel("GRUPO 1 FINAL.XLSX", 
                         sheet = 'GRUPO 1',
                         range = 'C7:BP105')

#Selecciono los campos que contienen las variables:
datos_o<-datos_G1_fina9l[,-c(1:7)]

class(datos_o$`Muy informado`)

datos_o <- mutate_all(datos_o, ~replace(., is.na(.), 0))

#Para cada pregunta, Crear variables:
# https://rpubs.com/hllinas/R_Recodificar_Variables

datos_o$Q1_Nvl_conocimiento <- ifelse(datos_o$'Muy informado' == 1, 1,
                                  ifelse(datos_o$Informado==1,2,
                                         ifelse(datos_o$`Poco Informado`==1,3,
                                            ifelse(datos_o$Desconocimiento==1,4,0
                                            ))))

colnames(datos_o$Q1_Nvl_conocimiento)<-c("Q1_Nvl_conocimiento")

datos_o$Q2_calif_recol <- ifelse(datos_o$`Excelente...12` == 1, 1,
                                                 ifelse(datos_o$`Bueno...13`==1,2,
                                                        ifelse(datos_o$`Regular...14`==1,3,
                                                               ifelse(datos_o$`Malo...15`==1,4,0
                                                               ))))

colnames(datos_o$Q2_calif_recol)<-c("Q2_calif_recol")

datos_o$Q3_mejorar_recoleccion <- ifelse(datos_o[,9] == 1, 1,
                                 ifelse(datos_o[,10]==1,2,
                                      ifelse(datos_o[,11]==1,3,
                                               ifelse(datos_o[,12]==1,4,0
                                               ))))

colnames(datos_o$Q3_mejorar_recoleccion)<-c("Q3_mejorar_recoleccion")

datos_o$Q4_nvl_conc_amb_barr <- ifelse(datos_o[,13] == 1, 1,
                                         ifelse(datos_o[,14]==1,2,
                                                ifelse(datos_o[,15]==1,3,0
                                                       )))

colnames(datos_o$Q4_nvl_conc_amb_barr)<-c("Q4_nvl_conc_amb_barr")

datos_o$Q5_aplic_mas_educ_recic <- ifelse(datos_o[,16] == 1, 1,
                                       ifelse(datos_o[,17]==1,2,0
                                              ))

colnames(datos_o$Q5_aplic_mas_educ_recic)<-c("Q5_aplic_mas_educ_recic")

datos_o$Q6_Compromiso_reci_ma <- ifelse(datos_o[,18] == 1, 1,
                                          ifelse(datos_o[,19]==1,2,
                                                 ifelse(datos_o[,20]==1,3,0
                                          )))

colnames(datos_o$Q6_Compromiso_reci_ma)<-c("Q6_Compromiso_reci_ma")

datos_o$Q7_dis_capaci_gst_res_sol <- ifelse(datos_o[,21] == 1, 1, 2
                                        # ifelse(datos_o[,22]==1,2,0
                                        #        )
                                        )

colnames(datos_o$Q7_dis_capaci_gst_res_sol)<-c("Q7_dis_capaci_gst_res_sol")

datos_o$Q8_Cali_barrido_ciud <- ifelse(datos_o[,23] == 1, 1,
                                            ifelse(datos_o[,24]==1,2,
                                                   ifelse(datos_o[,25]==1,3,
                                                          ifelse(datos_o[,26]==1,4,
                                                                 ifelse(datos_o[,27]==1,5,0
                                                          )))))

colnames(datos_o$Q8_Cali_barrido_ciud)<-c("Q8_Cali_barrido_ciud")

datos_o$Q9_pnr_cntndrs_ciu <- ifelse(datos_o[,28] == 1,1,2)#La col 29 es el No de la preg 9

colnames(datos_o$Q9_pnr_cntndrs_ciu)<-c("Q9_pnr_cntndrs_ciu")

datos_o$Q10_spr_rsd_hgr <- ifelse(datos_o[,30] == 1,1,
                                  ifelse(datos_o[,31]==1,2,0))

colnames(datos_o$Q10_spr_rsd_hgr )<-c("Q10_spr_rsd_hgr")

datos_o$Q11_aprvch_dsch_hgr <- ifelse(datos_o[,32] == 1,1,
                                      ifelse(datos_o[,33]==1,2,
                                             ifelse(datos_o[,34]==1,3,0)))

colnames(datos_o$Q11_aprvch_dsch_hgr )<-c("Q11_aprvch_dsch_hgr")

datos_o$Q12_rcicldr_crc <- ifelse(datos_o[,35] == 1,1,
                                      ifelse(datos_o[,36]==1,2,
                                             ifelse(datos_o[,37]==1,3,0)))

colnames(datos_o$Q12_rcicldr_crc )<-c("Q12_rcicldr_crc")

datos_o$Q13_cntndr_crc <- ifelse(datos_o[,38] == 1,1,
                                  ifelse(datos_o[,39]==1,2,0))

colnames(datos_o$Q13_cntndr_crc )<-c("Q13_cntndr_crc")

datos_o$Q14_da_dschs_rcicldrs <- ifelse(datos_o[,40] == 1,1,
                                 ifelse(datos_o[,41]==1,2,0))

colnames(datos_o$Q14_da_dschs_rcicldrs )<-c("Q14_da_dschs_rcicldrs")

datos_o$Q15_dfclt_clsfcr_rsds <- ifelse(datos_o[,42] == 1,1,
                                        ifelse(datos_o[,43]==1,2,
                                               ifelse(datos_o[,44]==1,3,
                                                      ifelse(datos_o[,45]==1,4,0)
)))

colnames(datos_o$Q15_dfclt_clsfcr_rsds )<-c("Q15_dfclt_clsfcr_rsds")

datos_o$Q16_rsds_clas_hgr <- ifelse(datos_o[,46] == 1,1,
                                        ifelse(datos_o[,47]==1,2,
                                               ifelse(datos_o[,48]==1,3,
                                                      ifelse(datos_o[,49]==1,4,
                                                             ifelse(datos_o[,50]==1,5,
                                                                    ifelse(datos_o[,51]==1,6,
                                                                           ifelse(datos_o[,52]==1,7,
                                                                                  ifelse(datos_o[,53]==1,8,
                                                                                         ifelse(datos_o[,54]==1,9,
                                                                                                ifelse(datos_o[,55]==1,10,0
                                                                                                ))))))))))
                                                                                                       
                                                                                                       
colnames(datos_o$Q16_rsds_clas_hgr )<-c("Q16_rsds_clas_hgr")

datos_o$Q17_cmprms <- ifelse(datos_o[,56] == 1,1,
                                    ifelse(datos_o[,57]==1,2,0
                                    ))
                                           
colnames(datos_o$Q17_cmprms )<-c("Q17_cmprms")

colnames(datos_o)

table(datos_o$Q17_cmprms )
table(datos_o$Q16_rsds_clas_hgr )
table(datos_o$Q15_dfclt_clsfcr_rsds )
table(datos_o$Q14_da_dschs_rcicldrs )
table(datos_o$Q13_cntndr_crc )
table(datos_o$Q12_rcicldr_crc )
table(datos_o$Q11_aprvch_dsch_hgr )
table(datos_o$Q10_spr_rsd_hgr)
table(datos_o$Q9_pnr_cntndrs_ciu )
table(datos_o$Q8_Cali_barrido_ciud )
table(datos_o$Q7_dis_capaci_gst_res_sol )
table(datos_o$Q6_Compromiso_reci_ma )
table(datos_o$Q5_aplic_mas_educ_recic )
table(datos_o$Q3_mejorar_recoleccion )
table(datos_o$Q1_Nvl_conocimiento, datos_o$Q2_calif_recol)
table(datos_o$Q1_Nvl_conocimiento, datos_o$Q2_calif_recol)
