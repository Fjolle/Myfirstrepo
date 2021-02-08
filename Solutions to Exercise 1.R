

rmarkdown::render('Newrmarkdown.Rmd', output_format = 'html_document') ## Replacement of the Knit button
library("tinytex", lib.loc="~/R/win-library/3.6")
rmarkdown::render('FirstDataMiningAssignment.Rmd', output_format = 'pdf_document')

#rmarkdown::render("Newrmarkdown.Rmd", md_document(variant = "markdown_github")) 


library(rmarkdown)
library(ggplot2)
library(tidyverse)
library(rsample)
library(caret)
library(modelr)
library(parallel)
library(foreach)
library(FNN)

summary(GasPrices)
    ##(a)

ggplot(data=GasPrices)+geom_boxplot(mapping=aes(x=Competitors, y=Price)) +theme_minimal()

    ##(b)
ggplot(data=GasPrices)+geom_point(mapping=aes(x=Income, y=Price))+theme_minimal() ##TRUE
cor(GasPrices$Income, GasPrices$Price)
    ##(c)

ggplot(data=GasPrices)+geom_col(mapping=aes(x=Brand, y=Price))+theme_minimal() ##TRUE???

    ##(d)

ggplot(data=GasPrices)+ geom_histogram(aes(x=Price, after_stat(density)),binwidth=0.01)+
  facet_wrap(~Stoplight) +theme_minimal()###UNCLEAR


    ##(e)

ggplot(data=GasPrices)+geom_boxplot(mapping=aes(x=Highway, y=Price)) ##TRUE





              ####PROBLEM 2

head(bikeshare)
summary(bikeshare)

        ###(a)
avgtotal= bikeshare %>%
  group_by(hr) %>% 
  summarize(meantotal=mean(total))

avgtotal

p=ggplot(avgtotal)+geom_line(aes(x=hr,y=meantotal, color="red")) +
  scale_x_continuous(breaks=1:23)
p

        ###(b)
head(bikeshare)
summary(bikeshare)
avgtotal= bikeshare %>%
  group_by(hr, workingday) %>% 
  summarize(meantotal=mean(total))

avgtotal

ggplot(avgtotal)+geom_line(aes(x=hr,y=meantotal, color="red")) +
  scale_x_continuous(breaks=1:23)+facet_wrap(~workingday)


      ###(c)

d1=bikeshare %>% filter(hr=='8') %>% group_by(weathersit, workingday) %>% summarize(meantotal=mean(total))

d1
               
ggplot(data=d1)+geom_col(mapping=aes(x=weathersit,y=meantotal))+
  facet_wrap(~workingday) +theme_minimal()





                            ####PROBLEM 3
head(ABIA)
summary(ABIA)
###(a)

##What is the average Departure Delay in minutes of flights in and out of Austin, given that the Taxi-In period in minutes is less than 70. 

avg1= ABIA %>%
  filter(Origin=='AUS', Origin=='') %>%
  group_by(Month) %>% 
  summarize(meandel=mean(DepDelay))
avg1


ggplot(avg1)+geom_line(aes(x=Month,y=meandel, color="red")) +
  scale_x_continuous(breaks=1:12)

##What is the average Departure Delay in minutes of flights in out of Austin, given that the Taxi-In period in minutes is more than or equal to 70. 


avg2= ABIA %>%
  filter(Origin=='AUS', TaxiIn>='70') %>%
  group_by(Month) %>% 
  summarize(meandel=mean(DepDelay))
avg2


ggplot(avg2)+geom_line(aes(x=Month,y=meandel, color="red")) +
  scale_x_continuous(breaks=1:12)



##What is the average Departure Delay in minutes of flights in out of Austin, given that the Taxi-Out period in minutes is less than 70. 

avg3= ABIA %>%
  filter(Origin=='AUS', TaxiOut<'70') %>%
  group_by(Month) %>% 
  summarize(meandel=mean(DepDelay))
avg3


ggplot(avg3)+geom_line(aes(x=Month,y=meandel, color="red")) +
  scale_x_continuous(breaks=1:12)

##What is the average Departure Delay in minutes of flights in out of Austin, given that the Taxi-Out period in minutes is more than or equal to 70. 


avg4= ABIA %>%
  filter(Origin=='AUS', TaxiOut>='70') %>%
  group_by(Month) %>% 
  summarize(meandel=mean(DepDelay))
avg4


ggplot(avg4)+geom_line(aes(x=Month,y=meandel, color="red")) +
  scale_x_continuous(breaks=1:12)







#### Problem 4


ggplot(data = sclass) + 
  geom_point(mapping = aes(x = mileage, y = price), color='darkgrey') + 
  ylim(500, 290000)

AMG65=filter(sclass, sclass$trim=="65 AMG") 


##80% TRAINING DATA, 20% TESTING DATA
sclass_split =  initial_split(AMG65, prop=0.8)
sclass_train = training(sclass_split)
sclass_test  = testing(sclass_split)
sclass_test = arrange(sclass_test, mileage)

lm1 = lm(price ~ mileage, data=sclass_train)
rmse(lm1 , sclass_test)

knn2=knnreg(price~mileage, data=sclass_train, k=2)

price_pred=predict(knn2, sclass_test)
price_pred
sclass_t = sclass_test %>%
  mutate(price_pred)

p_test=ggplot(data=sclass_t)+geom_point(mapping=aes(x=mileage, y=price), alpha=0.3)+ylim(500,160000)
p_test +geom_line(aes(x=mileage, y=price_pred), color='red', size=1)



knn10=knnreg(price~mileage, data=sclass_train, k=10)
rmse(knn10, sclass_test)

price_pred=predict(knn10, sclass_test)

sclass_t2 = sclass_test %>%
  mutate(price_pred)

knn15=knnreg(price~mileage, data=sclass_train, k=15)
rmse(knn15, sclass_test)

price_pred=predict(knn15, sclass_test)

sclass_t = sclass_test %>%
  mutate(price_pred)

knn20=knnreg(price~mileage, data=sclass_train, k=20)
rmse(knn20, sclass_test)

price_pred=predict(knn20, sclass_test)

sclass_t = sclass_test %>%
  mutate(price_pred)

knn21=knnreg(price~mileage, data=sclass_train, k=21)
rmse(knn21, sclass_test)

price_pred=predict(knn21, sclass_test)

sclass_t = sclass_test %>%
  mutate(price_pred)

p_test=ggplot(data=sclass_test)+geom_point(mapping=aes(x=mileage, y=price), alpha=0.3)+ylim(500,160000)
p_test +geom_line(aes(x=mileage, y=price_pred), color='red', size=1)


model <- train(price~mileage, data = sclass_train, method = "knn",
               trControl = trainControl("cv", number = 10),
               preProcess = c("center","scale"),
               tuneLength =10)
predictions <- model %>% predict(sclass_test)
RMSE(predictions, sclass_test$mileage)
plot(model)
model$bestTune



#FOR THE OPTIMAL LEVEL OF K: 

knn9=knnreg(price~mileage, data=sclass_train, k=9)
rmse(knn9, sclass_test)

price_pred=predict(knn15, sclass_test)

sclass_t = sclass_test %>%
  mutate(price_pred)

p_test=ggplot(data=sclass_test)+geom_point(mapping=aes(x=mileage, y=price), alpha=0.3)+ylim(500,160000)
p_test +geom_line(aes(x=mileage, y=price_pred), color='red', size=1)






A350=filter(sclass, sclass$trim=="350") 
A350

##80% TRAINING DATA, 20% TESTING DATA
sclass_split =  initial_split(A350, prop=0.8)
sclass_train = training(sclass_split)
sclass_test  = testing(sclass_split)
sclass_test = arrange(sclass_test, mileage)


lm1 = lm(price ~ mileage, data=sclass_train)
rmse(lm1 , sclass_test)

knn2=knnreg(price~mileage, data=sclass_train, k=2)
rmse(knn2, sclass_test)

price_pred=predict(knn2, sclass_test)

sclass_t = sclass_test %>%
  mutate(price_pred)

knn10=knnreg(price~mileage, data=sclass_train, k=10)
rmse(knn10, sclass_test)

price_pred=predict(knn10, sclass_test)

sclass_t = sclass_test %>%
  mutate(price_pred)

knn15=knnreg(price~mileage, data=sclass_train, k=15)
rmse(knn15, sclass_test)

price_pred=predict(knn15, sclass_test)

sclass_t = sclass_test %>%
  mutate(price_pred)

model <- train(price~mileage, data = sclass_train, method = "knn",
               trControl = trainControl("cv", number = 10),
               preProcess = c("center","scale"),
               tuneLength =10)
predictions <- model %>% predict(sclass_test)
RMSE(predictions, sclass_test$mileage)
plot(model)
model$bestTune


#FOR THE OPTIMAL LEVEL OF K: 

knn13=knnreg(price~mileage, data=sclass_train, k=13)
rmse(knn13, sclass_test)

price_pred=predict(knn15, sclass_test)

sclass_t = sclass_test %>%
  mutate(price_pred)

p_test=ggplot(data=sclass_test)+geom_point(mapping=aes(x=mileage, y=price), alpha=0.3)+ylim(500,160000)
p_test +geom_line(aes(x=mileage, y=price_pred), color='red', size=1)