library(latticeExtra)
library(ggplot2)
library(neuralnet)
library(plotrix)
# How do plan rates vary by states between 2014,2015 and 2016 years ?

dbs <- dbConnect(SQLite(),"downloads/database.sqlite",synchronous = NULL)

rate14 <- dbGetQuery(dbs,"select a.statecode,sum(a.rt)/count(PlanId) as avg_amt_2014 from (select statecode,PlanId,sum(IndividualRate)/count(RatingareaId) as rt from 
                     rate  where BusinessYear=2014 and IndividualRate!=999999 group by statecode,planid)a group by a.statecode order by a.statecode;")


rate15 <-dbGetQuery(dbs,"select a.statecode,sum(a.rt)/count(PlanId) as avg_amt_15 from (select statecode,PlanId,sum(IndividualRate)/count(RatingareaId) as rt 
                    from  rate  where BusinessYear=2015 and IndividualRate!=999999 group by statecode,planid)a group by a.statecode order by a.statecode;")


rate16 <- dbGetQuery(dbs,"select a.statecode,sum(a.rt)/count(PlanId) as avg_amt_16 from (select statecode,PlanId,sum(IndividualRate)/count(RatingareaId)as rt from 
                     rate  where BusinessYear=2016 and IndividualRate!=999999 group by statecode,planid)a group by a.statecode order by a.statecode;")
#get common rows between two columns
com_rows_1415 <- intersect (rate14$statecode,rate15$statecode)
com_rows_1416 <-intersect(rate14$statecode,rate16$statecode)
# find data which has common states
rate14c <- rate14[rate14$statecode %in% com_rows_1415,]
rate15c <- rate15[rate15$statecode %in% com_rows_1415,]
rate16c <- rate16[rate16$statecode %in% com_rows_1416,]
#calculate percent change in rate
percent_change_14to15 <- ((rate15c$avg_amt_15- rate14c$avg_amt_2014)/rate14c$avg_amt_2014)*100
percent_change_15to16 <- ((rate16c$avg_amt_16- rate15c$avg_amt_15)/rate15c$avg_amt_15)*100

res <- data.frame(rate15c$statecode,rate14c$avg_amt_2014,rate15c$avg_amt_15,percent_change_14to15)

res1 <- data.frame(rate16c$statecode,rate15c$avg_amt_15,rate16c$avg_amt_16,percent_change_15to16)

resfinal <- data.frame(rate15c$statecode,percent_change_14to15,percent_change_15to16)

ggplot(res)+ labs(x = "States",y="2014 Average plan rate",title = "Change in plan rates from 2014 to 2015")+
  geom_bar( aes(x=rate15c$statecode, y=rate14c$avg_amt_2014), stat="identity", fill="skyblue", alpha=0.7)+
  geom_errorbar( aes(x=rate15c$statecode, ymin=rate14c$avg_amt_2014, ymax=rate15c$avg_amt_15),width=0.4, colour="orange", alpha=0.9, size=1.3)

ggplot(res)+labs(x = "States",y="2015 Average plan rate",title = "Change in plan rates from 2015 to 2016")+
  geom_bar( aes(x=rate15c$statecode, y=rate15c$avg_amt_15), stat="identity", fill="skyblue", alpha=0.7)+
  geom_errorbar( aes(x=rate15c$statecode, ymin=rate15c$avg_amt_15, ymax=rate16c$avg_amt_16),width=0.4, colour="orange", alpha=0.9, size=1.3)

---------------------------------------------------------------
#Types of plans offered across United states in 2014  

US_metallevel <- dbGetQuery(dbs,"select metallevel_2014,count(PlanId_2014) from crosswalk2015 group by  metallevel_2014;")

piepercent<- round(100*US_metallevel$`count(PlanId_2014)`/sum(US_metallevel$`count(PlanId_2014)`),0)
piepercent<- paste(piepercent, "%",sep = "")

pie3D(US_metallevel$`count(PlanId_2014)`,labels = piepercent,main="Types of plans offered")
legend("topright", c("Bronze","Catastrophic","Gold","High","Low","Platinum","Silver"),fill = rainbow(7))

---------------------------------------------------------------
#Plan rates vs Age
  
planrate <- dbGetQuery(dbs,"select a.businessyear,a.age,sum(a.bt)/count(PlanId) as avg_rate_acrossAL from (select Businessyear,Age,PlanId,sum(IndividualRate)/count(RatingareaId) as bt
                   from Rate where BusinessYear in (2014,2015,2016)  and statecode='AL' and IndividualRate!=999999  group by businessyear,age,PlanId)a group by a.businessyear,a.age")

data_2014 <-  planrate[planrate$Businessyear==2014,]

data_2015 <-  planrate[planrate$Businessyear==2015,]

data_2016 <-  planrate[planrate$Businessyear==2016,]

plot(data_2014$Age,data_2014$avg_rate_acrossAL,col="blue",type="l",xlab = "Age",ylab="Plan Rates in $",main ="Age vs plan rates for AL state")
lines(data_2015$Age,data_2015$avg_rate_acrossAL)
lines(data_2016$Age,data_2016$avg_rate_acrossAL,col="red")
legend("topleft",legend = c("2014", "2015","2016"),col=c("blue","black","red"),pch = c(19,19),bty="n")

---------------------------------------------------------------
#How plan rates vary with age in 2014,2015 & 2016?
planrate <- dbGetQuery(dbs,"select a.businessyear,a.age,sum(a.bt)/count(PlanId) as avg_rate_acrossAL from (select Businessyear,Age,PlanId,sum(IndividualRate)/count(RatingareaId) as bt
                   from Rate where BusinessYear in (2014,2015,2016)  and statecode='AL' and IndividualRate!=999999  group by businessyear,age,PlanId)a group by a.businessyear,a.age")

year <- as.numeric(planrate$Businessyear)
age <- as.numeric(planrate$Age)
rate <- planrate$avg_rate_acrossAL
data <- na.omit(data.frame(year,age,rate))
# create two datasets
train <- na.omit(data[sample(nrow(data), 50), ])
test <- na.omit(data[sample(nrow(data), 50), ])
#normalise the data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

trainnorm <- as.data.frame(lapply(train, normalize))
testnorm <-as.data.frame(lapply(test, normalize))

#create a neural network
NX <- neuralnet(rate ~ age+year, data=trainnorm, hidden=c(2,1),linear.output=FALSE, threshold=0.01)
NX$result.matrix
plot(NX)
#predict the model
predict_testNN <- compute(NX,testnorm[,2:3])
predict_testNN = ((predict_testNN$net.result) * (max(data$rate) - min(data$rate))) + min(data$rate)

plot(test$rate , predict_testNN, col='red', pch=16, ylab = "predicted rate", xlab = "real rate")
abline(0,1)
 

