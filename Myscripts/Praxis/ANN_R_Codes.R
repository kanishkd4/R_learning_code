library(RSNNS)
library(nnet)
library(neural)
library(neuralnet)
library(NeuralNetTools)
library(e1071)
library(caret)
library(caretEnsemble)

# Dataset can be downloaded from https://www.dropbox.com/s/l7g2jk6jv1s6992/Steel%20Plate%20Faults.csv?dl=0
steel.plate=read.csv(file.choose())
#steel.plate=read.csv("/home/subhasis/Dataset/BigML datasets/Steel Plate Faults.csv")

set.seed(123457890);index=sample(nrow(steel.plate),round(nrow(steel.plate)*0.7,0))

steel.plate.train=steel.plate[index,]
library(dummies)
steel.plate.train.dummy=dummies::dummy.data.frame(data = steel.plate.train[,-28])
steel.plate.test=steel.plate[-index,]
steel.plate.test.dummy=dummies::dummy.data.frame(data = steel.plate.test[,-28])

trainClass=class.ind(steel.plate.train$Fault)
testClass=class.ind(steel.plate.test$Fault)

#model1=mlptrain(as.matrix(steel.plate.train.dummy),neurons = 5,out = trainClass,it=1000,visual = F)
models=list()
error=rep(NA,20)
for(i in 1:20){
model=mlp(x = scale(steel.plate.train.dummy),
          y = scale(trainClass),
          maxit=100,size = c(20,13),
          inputsTest = scale(steel.plate.test.dummy),
          targetsTest = scale(testClass))
avg.error=mean(model$IterativeFitError-model$IterativeTestError,na.rm = T)
error[i]=avg.error
models[[i]]=model
}
bestModel=models[[which.min(error)]]
plotIterativeError(bestModel)

fitted.values(bestModel)

x=RSNNS::confusionMatrix(testClass,predictions = predict(model,newdata = scale(steel.plate.test.dummy)))
x
e1071::classAgreement(x)

caret::confusionMatrix(x)

model2=rbf(x = scale(steel.plate.train.dummy),y = scale(trainClass),size = c(40))

plotIterativeError(model2)

model2.predict=predict(model2,newdata = scale(steel.plate.test.dummy))

RSNNS::confusionMatrix(testClass,model2.predict)

