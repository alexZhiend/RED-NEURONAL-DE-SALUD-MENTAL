
library(neuralnet)
setwd("C:\\Users\\user\\Documents\\R\\RED-NEURONAL-DE-SALUD-MENTAL")
mental1 = read.csv("salud1.csv", header=T)
str(mental1)
summary(mental1)
head(mental1)
mental1
ment=model.matrix(~treatment +Age + Gender + family_history + work_interfere, d=mental1)
ment=as.data.frame(ment)

fold.test <- sample(nrow(ment), nrow(ment) / 3)
test <- ment[fold.test, ]
train <- ment[-fold.test, ]

nuevo<-neuralnet(treatmentYes ~ Age + GenderFemale + GenderMale +
                  family_historyYes + work_interfereOften +
                  work_interfereRarely+work_interfereSometimes,d=train, hidden = 10,threshold = 0.01)

plot(nuevo)

out = compute(nuevo, test[,c("Age", "GenderFemale", 
                              "GenderMale", "family_historyYes","work_interfereOften"
                              ,"work_interfereRarely","work_interfereSometimes")])$net.result
# Tabla de confusión
out = out>0.5
table(out, test[,ncol(test)])

# Porcentaje de acierto
mean(out==test[,2])
