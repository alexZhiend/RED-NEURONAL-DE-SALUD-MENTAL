#PROYECTO DE SISTEMAS EXPERTOS
#INGENIERIA DE SISTEMAS SERIE 400
#ALAIN ALEXIS CAMPOS YARANGA
#YOBETH MARIA MARQUINA CHAVEZ
#ZORAIDA JESUS SANCHEZ QUISPE
##############################
#IMPORTAMOS LA LIBREA CON LA CUAL TRABAJAREMOS
library(neuralnet)
#REDIRECCION AL FILE, PARA PODER LEER EL DATASET
setwd("C:\\Users\\HP\\Downloads")
mental1 = read.csv("SALUD.csv", header=T)
str(mental1)
summary(mental1)
head(mental1)
mental1
#YA QUE EL DATASET ESTA DEFINIDO CON VARIABLES CUALITATIVAS, TRANSFORMAMOS ESTAS VARIABLES
#A UNA VARIABLES CUATITATIVAS CON LA FUNCION SIGUIENTE
ment=model.matrix(~treatment + Age + Gender + self_employed + family_history 
                  + work_interfere + remote_work + tech_company + obs_consequence,d=mental1)

#CONVERSION DE LA MATRIZ CREADA EN UN DATAFRAME
ment=as.data.frame(ment)

#DISTRIBUIMOS LOS DATOS DE ENTRENAMIENTO Y TESTING
fold.test <- sample(nrow(ment), nrow(ment)* 0.75)
train <- ment[fold.test, ]
test <- ment[-fold.test, ]

#CREACION DEL MODELO DE LA RED NEURONAL
nuevo<-neuralnet(treatmentYes ~ Age + GenderFemale + GenderMale + self_employedOther +
                   self_employedYes + family_historyYes + work_interfereOften +
                  work_interfereRarely+work_interfereSometimes+remote_workYes+
                   tech_companyYes + obs_consequenceYes,d=train, hidden = 10,stepmax = 1e6)

#GRAFICAMOS LA RED NEURONAL
plot(nuevo)

#CAPTURAR LOS DATOS DEL DATAFRAME DE SALIDA
out = compute(nuevo, test[,c("Age", "GenderFemale","GenderMale" ,"self_employedOther",
                             "self_employedYes",
                             "family_historyYes","work_interfereOften","work_interfereRarely",
                             "work_interfereSometimes","remote_workYes",
                             "tech_companyYes","obs_consequenceYes")])$net.result

# TABLA DE CONFUSION
out = out>0.5
table(out, test[,ncol(test)])

# PORCENTAJE DE ACIERTO
mean(out==test[,2])

