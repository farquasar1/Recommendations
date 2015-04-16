setwd("/Users/armandovieira/Documents/Varios/Semasio")
install.packages("rjson")
library(rjson)
install.packages("dplyr")
library(plyr)
install.packages("reshape")
library(reshape)
install.packages("sqldf")
library(sqldf)
install.packages("ggplot2")
library(ggplot2)
install.packages("randomForest")
library(randomForest)
library(checkpoint)
checkpoint("2014-10-08")

#sqldf("SELECT DATEDIFF(DAY,  DATEADD(day, -1, '2013-03-13 00:00:00.000'), GETDATE())")

ys=read.csv("ys_2m.csv")
lines=readLines("semasio.csv",n=10000)
data <- lapply(X=lines, fromJSON)
rm(lines)
dd=as.data.frame(t(sapply(data, "[", c(1:4))))
rm(data)
dd1=subset(dd,dd$eventType=="pageView")
dd_vendas=subset(dd,dd$eventType=="sale")
a=unique(dd_vendas$userId)
product_id=sapply(dd1$product, "[", "id")
dd1$product=(unlist(product_id))
dd1$event=1
dd1$userId=(unlist(dd1$userId))
dd1$dateTime=unlist(dd1$dateTime)
dd1$eventType=NULL
dd1$dateTime=as.Date(dd1$dateTime,"%Y/%m/%d")
str(dd1)
difftime(strptime("26.03.2014", format = "%d.%m.%Y"), 
         strptime("14.01.2013", format = "%d.%m.%Y"),units="days")
a=subset(dd1,dd1$dateTime>"2014-06-10")
a=difftime(dd1$dateTime[1],dd1$dateTime[1000],units="days")
as.numeric(dd1$dateTime[1]-dd1$dateTime[100])

mo <- strftime(dd1$dateTime, "%d%m")
head(mo)
dd <- data.frame(data$CUSTOMER_ID_MASK,mo, data$AMOUNT,data$MERCHANT_GROUP)

ggplot(dd1, aes(dateTime, ..count..))+geom_histogram(binwidth = 30, colour="white")
  
salesr=as.data.frame(unlist(dd_vendas$userId));colnames(salesr)="id"
salesr$date=(unlist(dd_vendas$dateTime));
salesr$date=as.Date(salesr$date,"%Y/%m/%d")
ggplot(salesr, aes(date, ..count..))+geom_histogram(binwidth = 30, colour="white")
str(salesr)
colnames(dd1)[2]="id"
#venda
a=join(dd1,salesr,type="inner",by="id")
y1=as.data.frame(sqldf("select * from dd1 where dd1.id  in(select salesr.id from salesr)"));y1$flag=1
#nao venda
y0=as.data.frame(sqldf("select * from dd1 where dd1.id  not in(select salesr.id from salesr)"));y0$flag=0
ys=rbind(y1,y0)

###### so considerar eventos 1 dia antes das vendas
dd_vendas$dateTime=unlist(dd_vendas$dateTime):dd_vendas$dateTime=as.Date(dd_vendas$dateTime,"%Y/%m/%d")
a=sqldf("select * from dd1,dd_vendas where (dd1.userId = dd_vendas.userId and 

######### read categories
cat=read.csv("Douglas_productfeed.csv",header=T)
#cat=subset(cat,cat$Category!="1")
#str(cat)
cat1=as.data.frame(cbind(cat$product_id,cat$Category))
colnames(cat1)=c("id","category")
cat1$category=as.factor(cat1$category)
rm(cat)
object.size(cat1)
ddf=sqldf("select * from dd1, cat1 where dd1.product = cat1.id ")

#########agregagte
agg=aggregate(event~category,ddf,FUN="sum")
agg1=aggregate(event~userId,dd1,FUN="sum")
a=(unlist(unique(dd_vendas$userId)))
hist(log(agg$event),col="blue",main="User interactions with products",xlab="Log(#events)")
agg=agg[order(-agg$event),]
agg1=agg1[order(-agg1$event),]
agg_n=subset(agg,agg$event>1)
teste=dd1[agg_n$product,]

visits=cast(ddf, id~category, sum, value = 'event')

#venda
y1=as.data.frame(sqldf("select * from visits where visits.id  in(select salesr.id from salesr)"));y1$flag=1
#nao venda
y0=as.data.frame(sqldf("select * from visits where visits.id  not in(select salesr.id from salesr)"));y0$flag=0
y0 <- y0[sample(1:nrow(y0), nrow(y1),replace=FALSE),]
ys=rbind(y1,y0)
ys <- ys[sample(1:nrow(ys), nrow(ys),replace=FALSE),]
#a=join(ys,cat1,type="right",by="id")

#### rminer
hist(ys$X1.1)
ys$flag=ys$X1.1
ys$X1.1=NULL

m=mining(flag~.,ys[,2:247],task="prob",model="randomforest",method=c("kfold",3))
m=mining(flag~.,ys,task="prob",model="lr",method=c("kfold",3))
print(mmetric(m,metric="CONF"))
print(mmetric(m,metric="AUC"))
print(mmetric(m,metric="ALL"))

m=fit(flag~.,ys,model="lr")
P=as.data.frame(predict(m,ys))
plot(P$V2,ys$flag)

m2 <- as.matrix(visits,sparse = TRUE)
m2=as.matrix(m2,sparse=T)
object.size(m2)
library("Matrix")
m2=Matrix(m2,sparse=T)

mo <- strftime(data$TXN_DATE, "%m%Y")
yr <- strftime(data$TXN_DATE, "%Y")
dd <- data.frame(data$CUSTOMER_ID_MASK,mo, data$AMOUNT,data$MERCHANT_GROUP)

write.csv(ys,"ys_2m.csv")


##############################################
Filter(function(x){length(x)>0 && x[["b"]] > 1},z)
data[lapply(data, length) > 5]
lapply(data[10:20], length)
data[["eventType"]]
data[["userId"]]
a=sapply(dd$product,"[","id")
sapply(dd$product[78],"[[",1)

data[22][[1]]$products

d1=as.data.frame(t(sapply(dd$product[1:30], "[",c(1:4))))
d1=NULL
d1$id

dd$eventType[21]
dd$product[22]
str(data[78])

dd=as.data.frame(t(sapply(data[1:100], "[", c(1:4))))
sum(dd$eventType=="pageView")
(dd$eventType=="sale")
(dd$eventType=="basketView")
levels(df$eventType)
dd=(sapply(data[1:10], "[", c(4)))

mylist2 <- lapply(lapply(data[1:100], unlist), function(x) {
  names(x)[names(x) == "products.name"] <- "productname"
  data.frame(t(x))
})
mylist2 <- lapply(lapply(data[200:300], unlist), function(x) {
  data.frame(t(x))
})
library(reshape)
a=melt(mylist2[78])
gregexpr("product",mylist2[78])

sapply(mylist2[78], "[", 1)
sapply(dd$product, "[", 1)
data[[78]]$products[[1]]$id
sapply(data[[79]]$product, "[[", "id")
rm(adf)
adf=rbind(adf,data[[79]]$eventType,data[[79]]$dateTime,data[[79]]$userId,a[5])

df=(rbind(mylist2))
df=rbind.fill(mylist2)

mylist1=lapply(dd$product, function(x) unlist(x))
mylist1=lapply(dd, function(x) split(x,x$product))
s <- split(dd,list(dd$product))
ml=with(dd, split(product, (id,name)))
dd$product[8][[1]]$categories

mylist1[1]
data[8]

str(mylist2[77:78])

df$dateTime=as.Date(as.character(df$dateTime),format="%Y/%m/%d %H:%M:%S")
str(df$dateTime)


############
df <- data.frame(matrix(unlist(data,recursive=F), nrow=8, byrow=T))
do.call(rbind.data.frame, data)
df=matrix(unlist(data,recursive=T), nrow=8, byrow=T,ncol=7)

data=data[3:10]
dft=as.data.frame(do.call(rbind, lapply(lapply(data, unlist), "[",
                      unique(unlist(c(sapply(data,names)))))))

df=as.data.frame(as.matrix(data))
df1=as.matrix(unlist(df))
df1[1]

rbind.fill(lapply(unlist(data), as.data.frame))
