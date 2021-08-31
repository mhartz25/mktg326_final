rm(list=ls())


#Set my working directory
getwd()
#setwd("C:/Users/matthewhartz/Documents/python_practice/projects/mktg_326/data")

# Import Libraries
library(caret)
library(cluster)
library(factoextra)
library(DescTools)
library(e1071)

nums <- c('total_socials', 'total_product_types','shopping_rate', 'buy_product_likelihood', 'click_freq', 'time_spent_on_social', 'bought_product_ad_factor')



df <- read.csv('r_mktg326.csv')


df <- tail(df, -5)


df <- df[, nums]

df$bought_product_ad_factor <- factor(x = df$bought_product_ad_factor,
                       levels = c("Below", "Above"))

#LR 1 
sub <- createDataPartition(y = df$bought_product_ad_factor, 
                           p = 0.80, 
                           list = FALSE)

train <- df[sub, ] 
test <- df[-sub, ]


logmod <- glm(formula = bought_product_ad_factor ~ ., 
              family = binomial,
              data = train)


summary(logmod)

coef(logmod)


exp(coef(logmod))


head(fitted(logmod))



train.pred <- ifelse(test = fitted(logmod) >= 0.5, # is the prediction >= .5?
                     yes = "Above", # if yes, predict "Above"
                     no = "Below") # if no, predict "Below"



train.pred <- factor(train.pred,
                     levels = c("Below", "Above"))


train_conf <- confusionMatrix(data = train.pred, # predictions
                              reference = train$bought_product_ad_factor, # actual
                              positive = "Above",
                              mode = "everything")
train_conf



LR.test <- predict(object = logmod, # LR model
                   newdata = test, # testing data
                   type = "response")


test.pred <- factor(ifelse(test = LR.test >= 0.5,
                           yes = "Above",
                           no = "Below"),
                    levels = c("Below", "Above"))



test_conf <- confusionMatrix(data = test.pred, # predictions
                             reference = test$bought_product_ad_factor, #actual
                             positive = "Above",
                             mode = "everything")
test_conf
