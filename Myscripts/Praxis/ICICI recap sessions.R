head(insurance$incident_date)
as.numeric(insurance$incident_date)
s=sort(insurance$incident_date)
unique(s)
x=c(1:10)
length(x)
x[6]
x1=c("a","b","D")
x1[3]
mat=matrix(1:9,ncol=3)
mat1=matrix(c("a","b","c","d"),nrow=2)
df=data.frame(int=c(1:4),char=c("a","b","c","d"))
list=list(vec=1:6,mat.int=mat,mat.chr=mat1,df=df)

(list$df[,1])[2]
list[[4]]
list[[4]][1:3,1]
list[4][2,1]

write.csv(iris,"C:\\Users\\user\\Desktop\\ICICI\\iris.csv")
library(foreign)
iris1=read.csv(file.choose(),sep="~")
write.table(iris,"C:\\Users\\user\\Desktop\\ICICI\\iris1.txt",
            sep="~",quote = F)
try=read.table(file.choose(),sep=";",
               colClasses = c("character","character"))

aggregate(income~ed,data = bankloan,FUN = max)

t=table(bankloan$ed)

nrow(bankloan)

aggregate(cbind(creddebt,income)~ed,data = bankloan,FUN = mean)

for(i in c(6,7,8)){
  blood[,i]=as.character(blood[,i])
  blood[,i]=as.numeric(blood[,i])
}

aggregate(cbind(WBC,RBC)~Age+BloodGroup,data = blood,FUN = mean)

aggr=as.data.frame(aggregate(cbind(WBC,RBC)~Age+BloodGroup,data = blood,FUN = mean))

order=order(aggr$WBC)

aggr1=aggr[order,]

iris1=iris1[,-1]

iris2=iris1[,-match(c("Species","Petal.Length"),names(iris1))]

iris2=iris1[iris1$Species!="setosa",
            -match(c("Species","Petal.Length"),names(iris1))]
iris3=subset(iris1,(Species %in% c("setosa","versicolor"))&Sepal.Length<5,
             c("Petal.Length","Petal.Width"))



