# Analisis DEA#

#Este script presenta un analisis DEA para las área de salud del pais, las variables contenidas se pueden consultar en la documentacion del papel del trabajo correspondiente que se encuentra en el Github#


#Asegurese de conocer en que dirección tienen la base de datos y la dirección desde la que opera R

#el siguiente código indica la dirección desde donde opera R
getwd()
## "C:/Users/humberto.perera/Documents"

#El siguiente debe modificarse según donde tiene ubicada la base de datos, la utilizada para este ejemplo se encuentra en el github

setwd("C:/Users/humberto.perera/Documents/resultados DEA Areas")
#"G:/Unidades compartidas/AO EBAIS/03_Exam/02_Proced/Creacion de bases de datos"

#se procede a instalar los paquetes necesarios y a comvocarlos para ser utilizados
install.packages("deaR")
install.packages("tidyverse")
install.packages("excelR")

library("deaR")
library("excelR")
library("readxl")


##################DEA AREAS DE SALUD##################
#se llama la base de datos para el año 2019 y se le da un nombre#
areas_dea_final19 <- read_excel("areas_dea_final19.xlsx")
View(areas_dea_final19)
#se llama la base de datos para el año 2020 y se le da un nombre#
areas_dea_final20 <- read_excel("areas_dea_final20.xlsx")
View(areas_dea_final20)

###Modelo 1####
#para el uso de los  modelos se deben quitar aquellas areas que no tengan datos pues causan error, y se proceden a crear subconjuntos solo con las variables de interes para el modelo
modelo1_19 <- areas_dea_final19 [-c(18, 27, 56, 76, 85, 101, 102, 103, 104), c("AS", "año", "Cod_Reg", "tipo_area", "pobtot2", "gasto_mill2", "medicin2", "tas_mort_inv", "tas_mort_in_inv", "emergencias2", "evitables_inv", "controles_diabetes", "controles_hipertensos", "vacu_prom")]
modelo1_20 <- areas_dea_final20 [-c(18, 27, 56, 76, 85, 101, 102, 103, 104), c("AS", "año", "Cod_Reg", "tipo_area", "pobtot2", "gasto_mill2", "medicin2", "tas_mort_inv", "tas_mort_in_inv", "emergencias2", "evitables_inv", "controles_diabetes", "controles_hipertensos", "vacu_prom")]

any(is.na(modelo1_19))
any(is.na(modelo1_20))

#acá se le indica el tipo de modelo DEA (input o output) la posicion de las variables segun sean un input o un output y si cuenta con insumos de no controlables o de ambiente, en este caso la poblacion es no controlable y el tipo  y region del area de salud es ambiental o de contexto 
model1_19 <‐read_data(modelo1_19, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 
model1_20 <‐read_data(modelo1_20, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 

#tiramos los resultados a un archivo para ser analizados
result1_19 <- model_basic(model1_19, orientation = "oo", rts = "vrs")
summary(result1_19, filename = "modelo1_19.xlsx")

result1_20 <- model_basic(model1_20, orientation = "oo", rts = "vrs")
summary(result1_20, filename = "modelo1_20.xlsx")


###Modelo 2####

modelo2_19 <- areas_dea_final19 [-c(18, 27, 56, 76, 85, 101, 102, 103, 104), c("AS", "año", "Cod_Reg", "tipo_area", "pobtot2", "gasto_mill2", "medicin2", "tas_mort_inv", "tas_mort_in_inv", "blanco_inv", "satis_glob", "controles_diabetes", "controles_hipertensos", "vacu_prom")]
modelo2_20 <- areas_dea_final20 [-c(18, 27, 56, 76, 85, 101, 102, 103, 104), c("AS", "año", "Cod_Reg", "tipo_area", "pobtot2", "gasto_mill2", "medicin2", "tas_mort_inv", "tas_mort_in_inv", "blanco_inv", "satis_glob", "controles_diabetes", "controles_hipertensos", "vacu_prom")]

any(is.na(modelo2_19))
any(is.na(modelo2_20))

model2_19 <‐read_data(modelo2_19, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 
model2_20 <‐read_data(modelo2_20, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 


result2_19 <- model_basic(model2_19, orientation = "oo", rts = "vrs")
summary(result2_19, filename = "modelo2_19.xlsx")

result2_20 <- model_basic(model2_20, orientation = "oo", rts = "vrs")
summary(result2_20, filename = "modelo2_20.xlsx")


###Modelo 3####

modelo3_19 <- areas_dea_final19 [-c(18, 27, 32, 36, 56, 76, 84, 85, 101, 102, 103, 104), c("AS", "año", "Cod_Reg", "tipo_area", "pobtot2", "gasto_mill2", "medicin2", "ausenti_inv", "porc_tam_opo", "vis_efec2", "consu_ext2", "controles_diabetes", "controles_hipertensos", "vacu_prom")]
modelo3_20 <- areas_dea_final20 [-c(18, 27, 32, 36, 56, 76, 84, 85, 101, 102, 103, 104), c("AS", "año", "Cod_Reg", "tipo_area", "pobtot2", "gasto_mill2", "medicin2", "ausenti_inv", "porc_tam_opo", "vis_efec2", "consu_ext2", "controles_diabetes", "controles_hipertensos", "vacu_prom")]

any(is.na(modelo3_19))
any(is.na(modelo3_20))

model3_19 <‐read_data(modelo3_19, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 
model3_20 <‐read_data(modelo3_20, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 


result3_19 <- model_basic(model3_19, orientation = "oo", rts = "vrs")
summary(result3_19, filename = "modelo3_19.xlsx")

result3_20 <- model_basic(model3_20, orientation = "oo", rts = "vrs")
summary(result3_20, filename = "modelo3_20.xlsx")

###Modelo 4####

modelo4_19 <- areas_dea_final19 [-c(36, 56, 83, 85, 90, 97, 101), c("AS", "año", "Cod_Reg", "tipo_area", "cant_ebais_19", "gasto_mill2", "ConsultaExterna_horas", "tas_mort_inv", "tas_mort_in_inv", "emergencias2", "evitables_inv", "controles_diabetes", "controles_hipertensos", "vacu_prom")]
modelo4_20 <- areas_dea_final20 [-c(36, 56, 83, 85, 90, 97, 101), c("AS", "año", "Cod_Reg", "tipo_area", "cant_ebais_19", "gasto_mill2", "ConsultaExterna_horas", "tas_mort_inv", "tas_mort_in_inv", "emergencias2", "evitables_inv", "controles_diabetes", "controles_hipertensos", "vacu_prom")]

any(is.na(modelo4_19))
any(is.na(modelo4_20))

model4_19 <‐read_data(modelo4_19, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 
model4_20 <‐read_data(modelo4_20, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 


result4_19 <- model_basic(model4_19, orientation = "oo", rts = "vrs")
summary(result4_19, filename = "modelo4_19.xlsx")

result4_20 <- model_basic(model4_20, orientation = "oo", rts = "vrs")
summary(result4_20, filename = "modelo4_20.xlsx")

###Modelo 5####

modelo5_19 <- areas_dea_final19 [-c(36, 56, 83, 85, 90, 97, 101), c("AS", "año", "Cod_Reg", "tipo_area", "cant_ebais_19", "gasto_mill2", "ConsultaExterna_horas", "tas_mort_inv", "tas_mort_in_inv", "blanco_inv", "satis_glob", "controles_diabetes", "controles_hipertensos", "vacu_prom")]
modelo5_20 <- areas_dea_final20 [-c(36, 56, 83, 85, 90, 97, 101), c("AS", "año", "Cod_Reg", "tipo_area", "cant_ebais_19", "gasto_mill2", "ConsultaExterna_horas", "tas_mort_inv", "tas_mort_in_inv", "blanco_inv", "satis_glob", "controles_diabetes", "controles_hipertensos", "vacu_prom")]

any(is.na(modelo5_19))
any(is.na(modelo5_20))

model5_19 <‐read_data(modelo4_19, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 
model5_20 <‐read_data(modelo4_20, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 


result5_19 <- model_basic(model5_19, orientation = "oo", rts = "vrs")
summary(result5_19, filename = "modelo5_19.xlsx")

result5_20 <- model_basic(model5_20, orientation = "oo", rts = "vrs")
summary(result5_20, filename = "modelo5_20.xlsx")

###Modelo 6####

modelo6_19 <- areas_dea_final19 [-c(32, 36, 56, 83, 84, 85, 90, 97, 101), c("AS", "año", "Cod_Reg", "tipo_area", "cant_ebais_19", "gasto_mill2", "ConsultaExterna_horas", "ausenti_inv", "porc_tam_opo", "vis_efec2", "consu_ext2", "controles_diabetes", "controles_hipertensos", "vacu_prom")]
modelo6_20 <- areas_dea_final20 [-c(32, 36, 56, 83, 84, 85, 90, 97, 101), c("AS", "año", "Cod_Reg", "tipo_area", "cant_ebais_19", "gasto_mill2", "ConsultaExterna_horas", "ausenti_inv", "porc_tam_opo", "vis_efec2", "consu_ext2", "controles_diabetes", "controles_hipertensos", "vacu_prom")]

any(is.na(modelo6_19))
any(is.na(modelo6_20))

model6_19 <‐read_data(modelo6_19, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 
model6_20 <‐read_data(modelo6_20, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 

result6_19 <- model_basic(model6_19, orientation = "oo", rts = "vrs")
summary(result6_19, filename = "modelo6_19.xlsx")

result6_20 <- model_basic(model6_20, orientation = "oo", rts = "vrs")
summary(result6_20, filename = "modelo6_20.xlsx")

###Modelo 7####

modelo7_19 <- areas_dea_final19 [-c(18, 36, 56, 83, 85, 90, 97, 101, 102, 103, 104), c("AS", "año", "Cod_Reg", "tipo_area", "pobtot2", "medicin2", "ConsultaExterna_horas", "tas_mort_inv", "tas_mort_in_inv", "emergencias2", "evitables_inv", "controles_diabetes", "controles_hipertensos", "vacu_prom")]
modelo7_20 <- areas_dea_final20 [-c(18, 36, 56, 83, 85, 90, 97, 101, 102, 103, 104), c("AS", "año", "Cod_Reg", "tipo_area", "pobtot2", "medicin2", "ConsultaExterna_horas", "tas_mort_inv", "tas_mort_in_inv", "emergencias2", "evitables_inv", "controles_diabetes", "controles_hipertensos", "vacu_prom")]

any(is.na(modelo7_19))
any(is.na(modelo7_20))

model7_19 <‐read_data(modelo7_19, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 
model7_20 <‐read_data(modelo7_20, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 

result7_19 <- model_basic(model7_19, orientation = "oo", rts = "vrs")
summary(result7_19, filename = "modelo7_19.xlsx")

result7_20 <- model_basic(model7_20, orientation = "oo", rts = "vrs")
summary(result7_20, filename = "modelo7_20.xlsx")

###Modelo 8####

modelo8_19 <- areas_dea_final19 [-c(18, 36, 56, 83, 85, 90, 97, 101, 102, 103, 104), c("AS", "año", "Cod_Reg", "tipo_area", "pobtot2", "medicin2", "ConsultaExterna_horas",  "tas_mort_inv", "tas_mort_in_inv", "blanco_inv", "satis_glob", "controles_diabetes", "controles_hipertensos", "vacu_prom")]
modelo8_20 <- areas_dea_final20 [-c(18, 36, 56, 83, 85, 90, 97, 101, 102, 103, 104), c("AS", "año", "Cod_Reg", "tipo_area", "pobtot2", "medicin2", "ConsultaExterna_horas",  "tas_mort_inv", "tas_mort_in_inv", "blanco_inv", "satis_glob", "controles_diabetes", "controles_hipertensos", "vacu_prom")]

any(is.na(modelo8_19))
any(is.na(modelo8_20))

model8_19 <‐read_data(modelo8_19, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 
model8_20 <‐read_data(modelo8_20, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 

result8_19 <- model_basic(model8_19, orientation = "oo", rts = "vrs")
summary(result8_19, filename = "modelo8_19.xlsx")

result8_20 <- model_basic(model8_20, orientation = "oo", rts = "vrs")
summary(result8_20, filename = "modelo8_20.xlsx")

###Modelo 9####

modelo9_19 <- areas_dea_final19 [-c(18, 32, 36, 56, 83, 84, 85, 90, 97, 101, 102, 103, 104), c("AS", "año", "Cod_Reg", "tipo_area", "pobtot2", "medicin2", "ConsultaExterna_horas",  "ausenti_inv", "porc_tam_opo", "vis_efec2", "consu_ext2", "controles_diabetes", "controles_hipertensos", "vacu_prom")]
modelo9_20 <- areas_dea_final20 [-c(18, 32, 36, 56, 83, 84, 85, 90, 97, 101, 102, 103, 104), c("AS", "año", "Cod_Reg", "tipo_area", "pobtot2", "medicin2", "ConsultaExterna_horas", "ausenti_inv", "porc_tam_opo", "vis_efec2", "consu_ext2", "controles_diabetes", "controles_hipertensos", "vacu_prom")]

any(is.na(modelo9_19))
any(is.na(modelo9_20))

model9_19 <‐read_data(modelo9_19, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 
model9_20 <‐read_data(modelo9_20, inputs=3:7,   outputs=8:14, nd_inputs=1:2, nc_inputs=3) 

result9_19 <- model_basic(model9_19, orientation = "oo", rts = "vrs")
summary(result9_19, filename = "modelo9_19.xlsx")

result9_20 <- model_basic(model9_20, orientation = "oo", rts = "vrs")
summary(result9_20, filename = "modelo9_20.xlsx")

help(summary)

#por último se muestra un aejemplo de la aplicación del indice de malquist para medir cambios en productividad y otras medidas
##################Ejemplo de base INDICE DE MALMQUIST##################

data("EconomyLong")
View("EconomyLong")

#iNSTRUCCION DE LECTURA, DEPENDE DEL FORMATO DE LA BASE DE DATOS
data_example_2 <‐ read_malmquist(EconomyLong,    percol=2,   arrangement="vertical",   ni=2,   no=1) 
#CORRE EL MODELO
result2 <- malmquist_index(data_example_2, orientation = "oo")
#LECTURA DE LOS RESULTADOS
mi2 <- result2$mi
effch2 <- result2$ec
tech2 <- result2$tc
help(package="deaR")
