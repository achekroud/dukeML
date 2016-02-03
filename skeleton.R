### Duke workshop
### Skeleton code examples





##############
##############
######### Set working directory to the repository you downloaded

setwd("~/Documents/PhD/PhD_Core/Teaching/MLworkshop/dukeWorkshop")

##############
##############

# Load libraries
libs <- c("ggplot2", "RColorBrewer", "glmnet", "caret", "pROC", "permute", "gbm",
          "klaR", "plyr", "foreach", "dplyr", "doMC")
lapply(libs, require, character.only = TRUE)
registerDoMC(detectCores()-1)

## Read in (training) data
#     Data contains freesurfer output, along with age and gender, for a bunch
#     of people scanned in the Boston area over the last couple of years.

raw <- read.csv("data/workshopTrain.csv", as.is=TRUE)
raw$X <- NULL
raw$Sex <- as.factor(raw$Sex)
df <- raw[,-1]

## View data in Rstudio
# View(df)

## View top of data frame in console
head(df[,1:8])

## Any missing data?
complete.cases(df) %>% table()

## View ICV by gender
ggplot(data = df, aes(ICV, colour = Sex)) +
    geom_freqpoly()


## Typical approach: logistic regression
glm(Sex ~ ., family = "binomial", data = df) %>% summary
# Does not converge! Collinearity! 

# Try a PCA?
df.pc <- prcomp(df[,-1])$x %>% as.data.frame()
# Take top 60 components (guess)
df.pc <- df.pc[,1:50]
# Put sex back in
df.pc$Sex <- df$Sex

# Try the logistic regression again?
pc.LR <- glm(Sex ~ ., family = "binomial", data = df.pc)

# Yay! Loads of significant predictors too. 

# Extract the predictions
pc.LR.out <- pc.LR$fitted.values
# Threshold them
pc.LR.out <- ifelse(pc.LR.out < 0.5, "F", "M")

# Confusion Matrices are really easy! Use this function!
confusionMatrix(data = pc.LR.out, reference = df.pc$Sex)

# Accuracy of 94%. In real life, we would just stop here.


# What if we wanted to keep it in original feature space? 
# Lets try a univariate filter (Pearson correlation)

# correlate each variable with the outcome
correlations <- sapply(names(df)[3:390], function(i) cor(as.numeric(df[,i]), as.numeric(df$Sex)))
# what happened?
summary(correlations)
# plenty of signal. keep |z| > 0.5?


fs1 <- names(correlations)[(correlations > 0.5) | (correlations < -0.5)]
df.fs1 <- dplyr::select(df, one_of(c("Sex", fs1)))

# stronger predictors, logistic regression
fs1.lr <- glm(Sex ~ ., family = "binomial", data = df.fs1)
# how was it?
confusionMatrix(data = ifelse(fs1.lr$fitted.values < 0.5, "F", "M"), reference = df.fs1$Sex)
# 93%, good. 






# Code to do cross-validated univariate feature selection using ANOVA
# Really slow, computationally unstable if fitted model is complicated (only LDA ran)
# Code for RFE is similar but worse
mySBF <- caretSBF
mySBF$filter <- function(score, x, y) { score <= 0.00001 }

sbf1 <- sbf(x = as.matrix(df[,2:390]), y = as.factor(df$Sex),
            method = "lda",
            trControl = trainControl(method = "none", 
                                     classProbs = TRUE),
            sbfControl = sbfControl(functions = mySBF,
                                    method = "cv"))
# 94% average test fold performance with small SD
# almost all variables were kept
# what might we do to change this? new score, multivariate filter, RFE, different scoring function


## Better approach, since this was computationally awful, might be to just 
#     use an actual model to rank features, using just a subset of the data

# There is a function called createDataPartition that creates random 
#   splits of the data, and balances class outcomes between the two splits
set.seed(1)    # this means that we can get the same random split next time
inSubset <- createDataPartition(df$Sex, p=(250/1098), list=FALSE)
df.sub   <- df[inSubset,]
df.rest  <- df[-inSubset,]

# Fit a model in the subset
mod.sub <- train(x= as.matrix(df.sub[,2:390]), y = as.factor(df.sub$Sex),
                        method = "lda")
# How did it do? getTrainPerf will go and get the performance metrics quickly for you
getTrainPerf(mod.sub)
# can also print the model output
print(mod.sub)

# Extract variable importance from the model
plot(varImp(mod.sub))                             # woops! too many predictors!
plot(varImp(mod.sub), top = 25)                   # Just top 25, scaled
plot(varImp(mod.sub, scale = FALSE), top = 25)    # Can have raw importance

# Can extract raw coefficients for the final model
coef(mod.sub$finalModel) %>% head()
# I would usually rank/analyse these, and then take the names of the best ones for further modeling







#####

##  Next we will learn how to fit other models using cross-validation
##      tools that are built in automatically.




## First we set up cross-validation procedures that we want
#   We will give these instructions to the train command later.
cvCtrl <- trainControl(method = "cv", number = 10)   # see help for other CV


### The train command is the core function in caret
##      it ties everything together in an **extremely** convenient wrapper

## Pseudocode train command (for humans to read)
# 
# modelStructure <-             ### you need to save all the stuff you build!
#     train (                   ### caret::train function
#     predictors as a matrix    ### x-matrix
#     target as a factor        ### dependent variable/target
#     what algorithm are you using?  ### method = "thing"
#     any cross-validation instructions? 
#     ))


# lets see this in practice!

mod1 <- train(x= as.matrix(df[,2:390]),
              y = as.factor(df$Sex),
              method = "lda",
              trControl = cvCtrl)

## inspect structure of model we built (broadly, don't worry too much)

## remember getTrainPerf?
getTrainPerf(mod1) #not bad! this is the average cross-validated performance



## Once you have the framework set up, changing algorithm is trivial
#   Here is an SVM
mod2 <- train(x= as.matrix(df[,2:390]),
              y = as.factor(df$Sex),
              method = "svmLinear",
              trControl = cvCtrl)
getTrainPerf(mod2) # SVM numerically worse than LDA

#   Here is k-NN
set.seed(2)
mod3 <- train(x= as.matrix(df[,2:390]),
              y = as.factor(df$Sex),
              method = "knn",
              trControl = cvCtrl)
getTrainPerf(mod3) # kNN was fast but lame

# Sometimes you need additional libraries to run certain non-standard models
mod4 <- train(x= as.matrix(df[,2:390]),
              y = as.factor(df$Sex),
              method = "ada",
              trControl = cvCtrl)
getTrainPerf(mod4) # single decision tree also weaker.


## This is a little obscure, but here we can check what models are available
#    if we wanted to do classification/or dual use
t <- getModelInfo()
m <- list();
for (i in names(t)){
  if (t[[i]]$type != "Regression"){
    m <- c(m, t[i])
  }
}
names(m)[1:5]



## You can also change the cross-validation framework FYI

mod5 <- train(x= as.matrix(df[,2:390]),
              y = as.factor(df$Sex),
              method = "ctree",
              trControl = trainControl(method="LOOCV"))
getTrainPerf(mod5)  # I told you it was slow.

# really slow. don't run it.


# how about 5-fold validation?
mod5 <- train(x= as.matrix(df[,2:390]),
              y = as.factor(df$Sex),
              method = "ctree",
              trControl = trainControl(method="cv", number=5))
getTrainPerf(mod5)
print(mod5)

## Jae's tree plotting code
plot(mod5$finalModel)
plot(mod5$finalModel, type="simple")







#### Advanced
## We mentioned briefly in the workshop that some algorithms require tuning
#    Best way to do this is to pre-specify a grid of all the parameter
#    combinations that you want to try, and choose the best through CV

# Hyperparameter grid (aka tuning grid) can be set up easily
# Example with radial SVM 
#   - visit caret docs for detail:
#     http://topepo.github.io/caret/modelList.html

svmGrid <- expand.grid(.sigma = c(1, 0.1, 0.05),
                       .C = c(1.0, 0.5, 0.1))

# We just have to pass this tuning grid to the train command (w/ CV)

# Tying it all together

mod6 <- train(x = as.matrix(df[,2:390]),
              y = as.factor(df$Sex),
              method = "svmRadial",
              tuneGrid = svmGrid,
              trControl = cvCtrl)
getTrainPerf(mod6) # wow, that sucked. why?

print(mod6) # Inspect the model summary
# not looking pretty, seems to have defaulted to the majority class
# this is always a bad sign.

# can you think why this might have happened?

mod6.1 <- train(x = as.matrix(df[,2:390]),
              y = as.factor(df$Sex),
              method = "svmRadial",
              trControl = cvCtrl)
getTrainPerf(mod6.1) # back up to 92% !

# can you spot the problem?

# it takes time/experience to get used to tuning algorithms.
# often safer just to let caret pick some defaults for you.




## live demo we can do external validation of our fave model and see whether it was actually good or not

# read in the test data

testData <- read.csv("data/workshopTest.csv", as.is=TRUE)
testData$X <- NULL
testData$Sex <- as.factor(testData$Sex)
ext <- testData[,-1]




### The caret model structure can be used to predict new outcomes 
##       NB: can discuss exactly how offline
sexPredictions <- predict(mod6.1,
                          newdata = as.matrix(ext[,2:390]))
confusionMatrix(data = sexPredictions, reference = as.factor(ext$Sex))

# yay! this was great. our model got 93% on unseen data.
# next step would be to get a new data set to externally validate the finding
# what do you think about this model? good? real? why?




