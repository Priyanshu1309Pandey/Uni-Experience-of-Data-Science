#Importing the datasets.
library(ggplot2)
library(corrplot)
library(Amelia)
library(plotly)
library(reshape2)
library(caret)
library(caTools)
library(dplyr)
library(mlbench)
library(caret)
library(gbm)
library(tidyverse)

library(sf)
library(tmap)
library(tidyverse)
library(caret)
library(gbm)
library(GGally)
data(BostonHousing)
head(BostonHousing)
data(BostonHousing2)
head(BostonHousing2)

#Number of complete cases.
sum(complete.cases(BostonHousing))
sum(complete.cases(BostonHousing2))

#Number of incomplete cases.
sum(!complete.cases(BostonHousing))
sum(!complete.cases(BostonHousing2))

?BostonHousing
?BostonHousing2

#Let's check the summary of the data.
summary(BostonHousing)
summary(BostonHousing2)

#Correlation between different variables.
corrplot(cor(select(BostonHousing,-chas)))
#medv decreases with increase in crim (medium), indus (High),nox(low),age(low),rad(low),tax(low),ptratio(high),
#lstat (High) and increases with increase in zn(low),rm(High).

boxplot(BostonHousing$medv)
quantile(BostonHousing$medv)

BostonHousing %>% 
  ggplot(aes(medv)) +
  stat_density() + 
  theme_bw()

?melt

#Vizualization.
BostonHousing %>%
  select(c(crim, rm, age, rad, tax, lstat, medv,indus,nox,ptratio,zn)) %>%
  melt(id.vars = "medv") %>%
  ggplot(aes(x = value, y = medv, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Median House Price ($1000s)") +
  theme_minimal()

#Creating training and testing data.
set.seed(1234) # for reproducibility
t.index = createDataPartition(BostonHousing$medv, p = 0.6, list = F)
dee.train = BostonHousing[t.index,]
dee.test = BostonHousing[-t.index,]

summary(dee.train$medv)
summary(dee.test$medv)

#
dee.train.p = dee.train %>% select(-medv) %>% 
              mutate_if(is_logical,as.character) %>%
              mutate_if(is_double, scale) %>% data.frame()
dee.test.p = dee.test %>% select(-medv) %>% 
              mutate_if(is_logical,as.character) %>%
              mutate_if(is_double, scale) %>% data.frame()
#
dee.train.p$medv = dee.train$medv
dee.test.p$medv = dee.test$medv

caretGrid <- expand.grid(interaction.depth=c(1, 2, 3), n.trees = (0:50)*50, shrinkage=c(0.01, 0.001), n.minobsinnode=10)
metric <- "RMSE"
trainControl <- trainControl(method="cv", number=10)
?train
## run the model over the grid
set.seed(99)
gbm1.caret <- train(medv ~ ., data=dee.train.p, distribution="gaussian", method="gbm", trControl=trainControl, verbose=FALSE, tuneGrid=caretGrid, metric=metric, bag.fraction=0.75)
## Examine the results
print(gbm1.caret)
ggplot(gbm1.caret)
# explore the results
names(gbm1.caret)
# see best tune
gbm1.caret[6]
# see grid results
grid_df = data.frame(gbm1.caret[4])
# check
dim(caretGrid)
dim(data.frame(gbm1.caret[4]))
plot(gbm1.caret)

#Storing the predicted values.
dee.test.p$predicted.medv <- predict(gbm1.caret,dee.test.p)
dee.test.p$predicted.medv

#Plotting actual vs predicted values.
pl1 <-dee.test.p %>% 
  ggplot(aes(medv,predicted.medv)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='black')) +
  xlab('Actual value of medv') +
  ylab('Predicted value of medv')+
  theme_bw()

ggplotly(pl1)
grid_df[which.min(grid_df$results.RMSE), ]
grid_df[which.max(grid_df$results.Rsquared), ]
grid_df[which.min(grid_df$results.MAE), ]

varImp(gbm1.caret, scale = FALSE)

data.frame(Predicted = dee.test.p$predicted.medv , Observed = dee.test.p$medv) %>%
  ggplot(aes(x = Observed, y = Predicted))+
  geom_point(size = 1, alpha = 0.5)+
  geom_smooth(method = "loess", col = "red")+
  geom_smooth(method = "lm")

gbm2 <- train(medv ~ ., data=dee.train.p, distribution="gaussian", method="gbm", trControl=trainControl, verbose=FALSE, tuneGrid= data.frame(interaction.depth = 3, n.trees = 2500, shrinkage = 0.01, n.minobsinnode = 10), metric=metric, bag.fraction=0.75)
pred101 = predict(gbm2, newdata = dee.test.p)
data.frame(Predicted = pred101, Observed = dee.test.p$medv) %>%
  ggplot(aes(x = Observed, y = Predicted))+ geom_point(size = 1, alpha = 0.5)+
  geom_smooth(method = "lm")

postResample(pred = pred101, obs = dee.test.p$medv)

