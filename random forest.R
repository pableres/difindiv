library(randomForest)
library(rpart)
library(rpart.plot)

#randomforest
#primero un arbol de decisión
first_model <- rpart(stroke~., data=trainset)
rpart.plot(first_model)

#ahora sí, el bosque aleatorio
modelorf <- randomForest(as.factor(stroke)~., data=trainset, ntree=400)

modelorf
first_model

modelorf$importance

plot(modelorf)


#comprobamos el funcionamiento del modelo
infartorf <- predict(modelorf, testset)

infartorf <- as.numeric(infartorf) - 1


actuals_preds <- data.frame(real = testset$stroke, random = infartorf)

head(actuals_preds)
actuals_preds
attach(actuals_preds)
table(real,random)
prop.table(table(actuals_preds))


