install.packages("neuralnet")
library(neuralnet)
install.packages("NeuralNetTools")
library(NeuralNetTools)
install.packages("caret")
library(caret)

df <- read.csv("C:/Users/pable/Downloads/healthcare-dataset-stroke-data.csv", sep=";")
df<-textshape::column_to_rownames(df, loc = 1)
df<- df[complete.cases(df), ]
sapply(df, class)
df<-subset(df, gender != "Other")
df$gender<-as.factor(df$gender)
nlevels(df$gender)
levels(df$gender)
df$bmi<-as.numeric(df$bmi)
df$smoking_status<-as.factor(df$smoking_status)
nlevels(df$smoking_status)
levels(df$smoking_status)
sapply(df, class)

df<- df[complete.cases(df), ]

#reducir la base de datos

soloinfarto<- df[df$stroke == 1,]
NOinfarto<- df[df$stroke == 0,]
brevessininfarto<- NOinfarto[sample(nrow(NOinfarto), 209), ]
df<- data.frame(rbind(brevessininfarto, soloinfarto))

#entrenamos y testeamos (subdata)

bound <- floor(nrow(df)/3)         #define % of training and test set
entrenamiento <- df[sample(nrow(df)), ]           #sample rows 
testset <- entrenamiento[1:bound, ]              #get training set
trainset <- entrenamiento[(bound+1):nrow(entrenamiento), ] 

#Red neuronal

str(trainset)
#stepmax = 1e6)
modelrn<-neuralnet(stroke~ age + hypertension + heart_disease + avg_glucose_level + bmi , data=trainset, hidden=c(12,5),act.fct = "logistic",linear.output = F,threshold = 0.05)
modelrn
plot(modelrn, rep="best")
print(modelrn)

#comprobando los modelos. en rf Y rl compute = predict

infartorn <- compute(modelrn, testset)

a<-infartorn$net.result
sino <- ifelse(a>0.5,1,0)


actuals_preds <- data.frame(cbind(real=testset$stroke, nn=sino))  # make actuals_predicteds dataframe.
head(actuals_preds)
actuals_preds

prop.table(table(actuals_preds))


###
install.packages("caret")
library(caret)

trainset$stroke <- as.factor(trainset$stroke)
testset$stroke <- as.factor(testset$stroke)


model <- train(stroke~ age + hypertension + heart_disease + avg_glucose_level + bmi , 
              data = trainset, 
              method = "nnet", #"neuralnet" o "mlp" 
              trControl = trainControl(method = "cv"), 
              tuneLength = 5)

print(model)
plot(model)
plotnet(model$finalModel)

predicciones <- predict(model, newdata = testset)
print(predicciones)
matriz_confusion <- confusionMatrix(predicciones, testset$stroke)
print(matriz_confusion)
