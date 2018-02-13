####
##
##  name:  08-downsampleClassifiers.R
##  date:  2017-01-26
##  what:  this assesses various classifers on downsampled data
##
####

# set working directory
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_2/Measure/")

cat("08:  now assessing classifiers on the downsampled data.\n")
source("fn-ccodeCleaner.R")

#### packages ####
library(dplyr)
library(tidyr)
library(data.table)
library(caret)
library(e1071)
library(pROC)
library(klaR)
library(caTools)
library(doParallel)
library(beepr)

#### functions for assessment ####
myLL <- function(y, p) 
        mean(-1 * (y == "yes") * log(p + .Machine$double.eps) - 
                     (y == "no")  * log(1 - p + .Machine$double.eps))

myRate <- function(p, y, cut = 0.5){
        
        TP <- sum(p >= cut & y == "yes")
        TN <- sum(p < cut  & y == "no")
        FP <- sum(p >= cut & y == "no")
        FN <- sum(p < cut  & y == "yes")
        
        results <- vector(mode = "list")
        results$sens <- TP / (TP + FN)
        results$spec <- TP / (TP + FP)
        results$npv <- TN / (TN + FN)
        results$prec <- TP / (TP + FP)
        results$conf <- matrix(c(TP, FN, FP, TN), ncol = 2, nrow = 2, byrow = T)
        results$acc <- (TP + TN) / (TP + TN + FP + FN)
        results$logLoss <- myLL(y = y, p = p)
        results$auc <- as.numeric(pROC::auc(response = y, predictor = p))
        results
        
}

getResults <- function(p, y, cut = 0.5){
        
        foo <- myRate(p = p, y = y, cut = cut)
        data.frame(logLoss = foo$logLoss,
                   auc = foo$auc,
                   accuracy = foo$acc,
                   sensitivity = foo$sens,
                   specificity = foo$spec,
                   precision = foo$prec)
        
}


#### read in a data frame ####
dat <- as.tbl(fread(paste0("DD/", list.files("DD")[1]))) %>% 
        mutate(A_ccode = makeCCode(A_ccode), B_ccode = makeCCode(B_ccode),
               ud_contiguity = factor(ud_contiguity,
                                      levels = c("none", "water400", "water150",
                                                 "water024", "water012", 
                                                 "direct")),
               ud_jointDem = ifelse(A_polity >= 6 & B_polity >= 6, 1, 0),
               ud_distance = log(ud_distance),
               ud_peaceYears2 = ud_peaceYears^2 / 100,
               ud_peaceYears3 = ud_peaceYears^3 / 1000,
               B_attacked = factor(B_attacked, labels = c("no", "yes"))) %>%
        dplyr::select(B_attacked, everything()) %>% 
        group_by(dd) %>% 
        mutate(ud_rival_lag = lag(ud_rival),
               ud_ongoingMID_lag = lag(ud_ongoingMID),
               ud_peaceYears_lag = lag(ud_peaceYears),
               ud_peaceYears2_lag = lag(ud_peaceYears2),
               ud_peaceYears3_lag = lag(ud_peaceYears3),
               ud_midInLast10_lag = lag(ud_midInLast10)) %>% 
        ungroup()

dat$ud_rival[is.na(dat$ud_rival) & dat$year != 1870] <- 0
dat$ud_ongoingMID[is.na(dat$ud_ongoingMID)] <- 0
dat <- filter(dat, year > 1870)

omit <- c("A_cyear", "B_cyear", "A_ccode", "B_ccode", "dd",
          "A_stateabb", "B_stateabb", "A_milexNI", "B_milexNI",
          "A_polityNI", "B_polityNI", "A_attacked", "A_milex",
          "ud_ongoingMID", "ud_rival", "ud_peaceYears",
          "ud_peaceYears2", "ud_peaceYears3", "ud_midInLast10")


dat <- dplyr::select(dat, -one_of(omit)) %>% 
        dplyr::select(ddy, B_attacked, starts_with("A"), starts_with("B"), 
                      starts_with("ud"), everything())

# check for missingness
missing<-cbind(round(colMeans(is.na(dat)), 3))*100

# about 2.5% missingness in the lags, which is to be expected
nas<-which(complete.cases(dat)==F)

# should be safe to simply omit these rows
dat_omit<-dat[-nas,]

#### basic CV set-up ####

### data partitioning -- since we have so much, make a true vault data set.
### maintain class proportions with stratified random sampling
set.seed(90210)
split1 <- createDataPartition(dat_omit$B_attacked, p = .6)[[1]]
trainDat <- dat_omit[split1,]
other <- dat_omit[-split1,]

set.seed(555)
split2 <- createDataPartition(other$B_attacked, p = 1/3)[[1]]
evalDat <- other[split2,]
testDat <- other[-split2,]
rm(dat, dat_omit, other)

### to get a few different criteria
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...),
                             mnLogLoss(...))

### register parallel backend
numCore <- detectCores() - 2
registerDoParallel(cores = numCore)

### set up controls
### want to keep predictions for the validation set
ctrl <- trainControl(method = "cv", 
                     classProbs = TRUE,
                     summaryFunction = fiveStats,
                     verboseIter = TRUE,
                     allowParallel = FALSE,
                     savePredictions="final")

ctrlJustRun <- ctrl
ctrlJustRun$method <- "none"

ctrlParallel <- ctrl
ctrlParallel$verboseIter <- FALSE
ctrlParallel$allowParallel <- TRUE

### downsample: proportion set to 100 from previous script
zeroes <- filter(trainDat, B_attacked == "no")
ones <- filter(trainDat, B_attacked == "yes")

set.seed(90210)
zSamp <- sample_n(tbl = zeroes, 
                  size = nrow(ones) * 100, 
                  replace = TRUE)

oSamp <- sample_n(tbl = ones,
                  size = nrow(ones),
                  replace = TRUE)

downDat <- rbind.data.frame(zSamp, oSamp)
rm(zSamp, oSamp)

downDat <- downDat[complete.cases(downDat),]
rm(trainDat, testDat)

#### try models on downsample ####
### null ###
set.seed(555)
down_null <- suppressWarnings(                    ### 0s and 1s
        train(B_attacked ~ 1,
              data = dplyr::select(downDat, -ddy),
              method = "glm",
              trControl = ctrlParallel, 
              metric = "logLoss"))

save(down_null, file = "Models/down_null.Rdata")
rm(down_null)

### logit ###
set.seed(555)
down_glm <- suppressWarnings(                    ### 0s and 1s
        train(B_attacked ~ .,
              data = dplyr::select(downDat, -ddy),
              method = "glm",
              trControl = ctrlParallel, 
              metric = "logLoss"))

save(down_glm, file = "Models/down_glm.Rdata")
rm(down_glm)

### boosted logit ###
set.seed(555)
down_LogitBoost <- suppressWarnings(                    ### 0s and 1s
        train(B_attacked ~ .,
              data = dplyr::select(downDat, -ddy),
              method = "LogitBoost",
              trControl = ctrlParallel, 
              metric = "logLoss"))

save(down_LogitBoost, file = "Models/down_LogitBoost.Rdata")
rm(down_LogitBoost)

### Elastic Net ###
set.seed(555)
down_glmnet <- train(B_attacked ~ .,
                     data = dplyr::select(downDat, -ddy),
                     method = "glmnet",
                     trControl = ctrlParallel, 
                     tuneGrid = expand.grid(.alpha = c(0,.2,.4,.6,.8,1),
                                            .lambda = c(0.00001,
                                                        0.0001,
                                                        0.001,
                                                        0.01)),
                     tuneLength = 5,
                     metric = "logLoss",
                     preProcess=c("center", "scale"))

save(down_glmnet, file = "Models/down_glmnet.Rdata")
rm(down_glmnet)

### MARs ###
down_mars <-train(B_attacked ~ .,
                  data = dplyr::select(downDat, -ddy),
                  method = "earth",
                  trControl = ctrlParallel, 
                  metric = "logLoss")

save(down_mars, file = "Models/down_mars.Rdata")
rm(down_mars)

### CART ###
set.seed(555)
down_rpart <- train(B_attacked ~ .,
                    data = dplyr::select(downDat, -ddy),
                    method = "rpart",
                    trControl = ctrlParallel, 
                    tuneLength = 5,
                    metric = "logLoss")

save(down_rpart, file = "Models/down_rpart.Rdata")
rm(down_rpart)

### random forests ###
set.seed(555)
down_rf <- train(B_attacked ~ .,
                 data = dplyr::select(downDat, -ddy),
                 method = "ranger",
                 trControl = ctrlParallel, 
                 tuneLength = 5,
                 metric = "logLoss",
                 importance="permutation")

save(down_rf, file = "Models/down_rf.Rdata")
rm(down_rf)

### conditional inference trees ###
#set.seed(555)
#down_cforest <- train(B_attacked ~ .,
#                 data = dplyr::select(downDat, -ddy),
#                 method = "cforest",
#                 trControl = ctrlParallel, 
#                 tuneLength = 5,
#                 metric = "logLoss")

#save(down_cforest, file = "Models/down_cforest.Rdata")
#rm(down_cforest)

### SVM ###
set.seed(555)
down_svm <- train(B_attacked ~ .,
                  data = dplyr::select(downDat, -ddy),
                  method = "svmRadialWeights",
                  trControl = ctrlParallel, 
                  tuneLength = 5,
                  metric = "logLoss")

save(down_svm, file = "Models/down_svm.Rdata")
rm(down_svm)

### C5.0 ###
set.seed(555)
down_C5 <- train(B_attacked ~ .,
                 data = dplyr::select(downDat, -ddy),
                 method = "C5.0",
                 trControl = ctrlParallel, 
                 tuneLength = 5,
                 metric = "logLoss")

save(down_C5, file = "Models/down_C5.Rdata")
rm(down_C5)

### averaged neural net ###
set.seed(555)
down_avNNet <- train(B_attacked ~ .,
                   data = dplyr::select(downDat, -ddy),
                   method = "avNNet",
                   tuneLength = 5,
                   trControl = ctrl,
                   metric = "logLoss",
                   preProcess=c("center", "scale", "pca"))

save(down_avNNet, file = "Models/down_avNNet.Rdata")
rm(down_avNNet)

### multilayer perceptron ###
set.seed(555)
down_mlp <- train(B_attacked ~ .,
                      data = dplyr::select(downDat, -ddy),
                      method = "mlpSGD",
                      tuneLength = 5,
                      trControl = ctrl,
                      metric = "logLoss",
                  preProcess=c("center", "scale", "pca"))

save(down_mlp, file = "Models/down_mlp.Rdata")
rm(down_mlp)

#### load models which have been saved ###
load("Models/down_null.Rdata")
load("Models/down_glm.Rdata")
load("Models/down_LogitBoost.Rdata")
load("Models/down_glmnet.Rdata")
load("Models/down_mars.Rdata")
load("Models/down_rpart.Rdata")
load("Models/down_rf.Rdata")
#load("Models/down_cforest.Rdata")
load("Models/down_svm.Rdata")
load("Models/down_C5.Rdata")
load("Models/down_avNNet.Rdata")
#load("Models/down_rbfDDA.Rdata")
#load("Models/down_mlp.Rdata")

# change names quickly
down_null$method<-"null"

### grab all models run so far ###
models_down<-lapply(ls(pattern="down_"), get)

# wipe the individual models to save memory
rm(list=ls(pattern="down_"))

### store times
down_time<-as.tbl(foreach(i=1:length(models_down), .combine=rbind) %do% {
        ### store time to train ###
        data.frame(model=models_down[[i]]$method,
                   time=models_down[[i]]$times$everything[3]/60)
})


# Grab the outcome from the training CV
down_obs<-as.matrix((models_down[[1]]$pred) %>%
                            arrange(rowIndex) %>%
                            dplyr::select(obs))

### grab out of sample predictions from the training CV
down_preds<-as.tbl(foreach(i=1:length(models_down), .combine=cbind.data.frame) %do% {
        
        bar <- as.tbl(models_down[[i]]$pred) %>% 
                arrange(rowIndex) %>% 
                dplyr::select(yes)
        
        names(bar)<-models_down[[i]]$method
        bar
        
})

# performance from training (out of sample, down sampled)
down_results<-as.tbl(data.frame(model = names(down_preds),
                   LL=apply(down_preds, 2, myLL, y=down_obs),
                   
                   AUC = apply(down_preds, 2,
                               pROC::auc, response = as.vector(down_obs)),
                   
                   Sensitivity = apply(down_preds, 2, 
                                       ModelMetrics::sensitivity,
                                       actual = ifelse(down_obs == "yes", 1, 0)),
                   
                   Specificity = apply(down_preds, 2, 
                                       ModelMetrics::specificity,
                                       actual = ifelse(down_obs == "yes", 1, 0)),
                   
                   Precision = apply(down_preds, 2,
                                     ModelMetrics::precision,
                                     actual = ifelse(down_obs == "yes", 1, 0)),
                   
                   Minutes=(down_time$time)
))

### Ensemble
# functions for the ensemble
library(Rsolnp)
frontier<- function(weights, preds, y){
        
        if(!is.matrix(preds)) preds <- as.matrix(preds)
        wSum <- preds %*% as.matrix(weights)
        myLL(p = wSum, y = y)
        
}

constraint <- function(weights, preds, y) {
        sum(weights)
}

# assign starting weights
starts <- as.matrix(rep(1 / ncol(down_preds), ncol(down_preds)))

solver <- suppressWarnings( 
        solnp(pars = starts,
              fun = frontier,
              y = down_obs,
              preds = down_preds,
              eqfun = constraint,
              eqB = 1,
              LB = rep(0, ncol(down_preds)),
              UB = rep(1, ncol(down_preds)),
              control = list(trace = 0)))

weights<-solver$pars
names(weights)<-colnames(down_preds)


# downsample the validation set
#zeroes <- filter(evalDat, B_attacked == "no")
#ones <- filter(evalDat, B_attacked == "yes")

#set.seed(90210)
#zSamp <- sample_n(tbl = zeroes, 
#                  size = nrow(ones) * 100, 
#                  replace = TRUE)

#oSamp <- sample_n(tbl = ones,
#                  size = nrow(ones),
#                  replace = TRUE)

#downevalDat <- rbind.data.frame(zSamp, oSamp)
#rm(zSamp, oSamp)

#downevalDat <- downevalDat[complete.cases(downevalDat),]


### predict the validation set ###
validation_preds<-as.tbl(foreach(i=1:length(models_down), .combine=cbind) %do% {
        
        ### validation performance ###
        p<-as.data.frame(predict.train(models_down[[i]], 
                         newdata = dplyr::select(evalDat, -ddy),
                         type = "prob")[,2])# this might be the issue
        
        colnames(p)<-models_down[[i]]$method
        p
        
})

# bind in ensemble
ensemble<-as.matrix(validation_preds)%*%as.vector(weights)
colnames(ensemble)<-"ensemble"
validation_preds<-cbind(validation_preds, ensemble)

### actual outcome
validation_obs<-evalDat$B_attacked


# results on validation set
validation_results<-as.tbl(data.frame(model = names(validation_preds),
                   LL=apply(validation_preds, 2, myLL, y=validation_obs),
                   
                   AUC = apply(validation_preds, 2,
                               pROC::auc, response = as.vector(validation_obs)),
                   
                   Sensitivity = apply(validation_preds, 2, 
                                       ModelMetrics::sensitivity,
                                       actual = ifelse(validation_obs == "yes", 1, 0)),
                   
                   Specificity = apply(validation_preds, 2, 
                                       ModelMetrics::specificity,
                                       actual = ifelse(validation_obs == "yes", 1, 0)),
                   
                   Precision = apply(validation_preds, 2,
                                     ModelMetrics::precision,
                                     actual = ifelse(validation_obs == "yes", 1, 0))
                   )
)



# make tables here
# results from tuning
down_table<-down_results[order(down_results$LL), , drop = FALSE]

# validation
validation_table<-validation_results[order(validation_results$LL), , drop = FALSE]


# weights
weights_table<-data.frame(model=names(weights),
                          Weights=weights)

LL<-validation_results$LL[validation_results$model=="ensemble"]
PRL<-abs((LL-validation_results[which(validation_results$model=="null"),]$LL)/validation_results[which(validation_results$model=="null"),]$LL)

ensembleLL<-data.frame(model="ensembleLL",
                       Weights=LL)

ensemblePRL<-data.frame(model="ensemblePRL",
                        Weights=PRL)

ensemble_table<-as.tbl(rbind(weights_table, ensembleLL, ensemblePRL))


# clean up names
cleanMod <- function(x){
        x <- gsub(pattern = "glm", replacement = "Logit", x)
        x <- gsub(pattern = "null", replacement = "Null", x)
        x <- gsub(pattern = "glm", replacement = "Logit", x)
        x <- gsub(pattern = "Logitnet", replacement = "Elastic Net", x)
        x <- gsub(pattern = "C5.0", replacement = "C5.0", x)
        x <- gsub(pattern = "ranger", replacement = "Random Forest", x)
        x <- gsub(pattern = "rpart", replacement = "CART", x)
        x <- gsub(pattern = "LogitBoost", replacement = "Boosted Logit", x)
        x <- gsub(pattern = "svmRadialWeights", replacement = "SVM Radial", x)
        x <- gsub(pattern = "kknn", replacement = "KNN", x)
        x <- gsub(pattern = "avNNet", replacement = "Neural Nets", x)
        x <- gsub(pattern = "ensemble", replacement = "Ensemble", x)
        x <- gsub(pattern = "earth", replacement = "MARS", x)
        x
}

# for rounding with character
round_df <- function(df, digits) {
        nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
        
        df[,nums] <- round(df[,nums], digits = digits)
        
        (df)
}

toOut_down <- down_table %>%
        mutate(model = as.character(cleanMod(model))) %>%
        round_df(3) %>%
        remove_rownames %>%
        column_to_rownames(var="model")

toOut_validation<- validation_table %>%
        mutate(model = as.character(cleanMod(model))) %>%
        round_df(3) %>%
        remove_rownames %>%
        column_to_rownames(var="model")

toOut_weights<- ensemble_table %>%
        mutate(model = as.character(cleanMod(model))) %>%
        round_df(3) %>%
        remove_rownames %>%
        column_to_rownames(var="model")

# latex
library(Hmisc)
latex(toOut_down, file="")
latex(toOut_validation, file="")
latex(toOut_weights, file="")







# The performance of the ensemble is encouraging, but how well can we do without the SVM?

# drop the SVM
models_trim<-models_down[-which(down_results$model=="svmRadialWeights")]
down_trim<-dplyr::select(down_preds, -svmRadialWeights)

starts <- as.matrix(rep(1 / ncol(down_trim), ncol(down_trim)))

solver <- suppressWarnings( 
        solnp(pars = starts,
              fun = frontier,
              y = down_obs,
              preds = down_trim,
              eqfun = constraint,
              eqB = 1,
              LB = rep(0, ncol(down_trim)),
              UB = rep(1, ncol(down_trim)),
              control = list(trace = 0)))

weights_trim<-solver$pars
names(weights_trim)<-colnames(down_trim)

trim_validation<-as.tbl(foreach(i=1:length(models_trim), .combine=cbind) %do% {
                
                ### validation performance ###
                p<-as.data.frame(predict.train(models_trim[[i]], 
                                               newdata = dplyr::select(evalDat, -ddy),
                                               type = "prob")[,2])# this might be the issue
                
                colnames(p)<-models_trim[[i]]$method
                p
                
        })

ensemble_trim<-as.matrix(trim_validation)%*%weights_trim
colnames(ensemble_trim)<-"ensemble"

ensemble_trim_results<-as.tbl(
        data.frame(model = "ensemble_trim",
                   LL=apply(ensemble_trim, 2, myLL, y=validation_obs),
                   
                   AUC = apply(ensemble_trim, 2,
                               pROC::auc, response = validation_obs),
                   
                   Sensitivity = apply(ensemble_trim, 2, 
                                       ModelMetrics::sensitivity,
                                       actual = ifelse(validation_obs == "yes", 1, 0)),
                   
                   Specificity = apply(ensemble_trim, 2, 
                                       ModelMetrics::specificity,
                                       actual = ifelse(validation_obs == "yes", 1, 0)),
                   
                   Precision = apply(ensemble_trim, 2,
                                     ModelMetrics::precision,
                                     actual = ifelse(validation_obs == "yes", 1, 0))
        )
)

# Compare
ensemble_trim_results

# Lose a bit without the SVMs
# We're going to proceed to the full dataset without SVMs despite the improvement
# Make table










# results from tuning
down_table<-down_results[order(-down_results$LL), , drop = FALSE]

validation_table<-validation_results[order(-validation_results$LL), , drop = FALSE]

# 
weights_table<-data.frame(model=names(weights_foo),
                          Weights=weights_foo)

LL<-foo_results$LL
PRL<-abs((LL-validation_results[which(validation_results$model=="null"),]$LL)/validation_results[which(validation_results$model=="null"),]$LL)

ensembleLL<-data.frame(model="ensembleLL",
                       Weights=LL)

ensemblePRL<-data.frame(model="ensemblePRL",
                        Weights=PRL)

ensemble_table<-as.tbl(rbind(weights_table, ensembleLL, ensemblePRL))

# clean up names
cleanMod <- function(x){
        x <- gsub(pattern = "glm", replacement = "Logit", x)
        x <- gsub(pattern = "null", replacement = "Null", x)
        x <- gsub(pattern = "glm", replacement = "Logit", x)
        x <- gsub(pattern = "Logitnet", replacement = "Elastic Net", x)
        x <- gsub(pattern = "C5.0", replacement = "C5.0", x)
        x <- gsub(pattern = "ranger", replacement = "Random Forest", x)
        x <- gsub(pattern = "rpart", replacement = "CART", x)
        x <- gsub(pattern = "LogitBoost", replacement = "Boosted Logit", x)
        x <- gsub(pattern = "svmRadialWeights", replacement = "SVM Radial", x)
        x <- gsub(pattern = "kknn", replacement = "KNN", x)
        x <- gsub(pattern = "avNNet", replacement = "Neural Nets", x)
        x <- gsub(pattern = "ensemble", replacement = "Ensemble", x)
        x
}

# for rounding with character
round_df <- function(df, digits) {
        nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
        
        df[,nums] <- round(df[,nums], digits = digits)
        
        (df)
}

toOut_down <- down_table %>%
        mutate(model = as.character(cleanMod(model))) %>%
        round_df(3)

toOut_validation<-validation_table %>%
        mutate(model = as.character(cleanMod(model))) %>%
        round_df(3)

toOut_ensemble<-ensemble_table %>%
        mutate(model = as.character(cleanMod(model))) %>%
        round_df(3)


# export to latex








rownames(down_training) <- gsub(pattern = "glm", replacement = "Logit", rownames(down_training))
rownames(down_training) <- gsub(pattern = "null", replacement = "Null", rownames(down_training))
rownames(down_training) <- gsub(pattern = "rpart", replacement = "CART", rownames(down_training))
rownames(down_training) <- gsub(pattern = "Logitnet", replacement = "Elastic Net", rownames(down_training))
rownames(down_training) <- gsub(pattern = "LogitBoost", replacement = "Bosted Logit", rownames(down_training))
rownames(down_training) <- gsub(pattern = "earth", replacement = "MARS", rownames(down_training))
rownames(down_training) <- gsub(pattern = "nb", replacement = "Naive Bayes", rownames(down_training))
rownames(down_training) <- gsub(pattern = "ranger", replacement = "Random Forest", rownames(down_training))
rownames(down_training) <- gsub(pattern = "cforest", replacement = "Conditional Inference Trees", rownames(down_training))
rownames(down_training) <- gsub(pattern = "C5", replacement = "C5.0", rownames(down_training))
rownames(down_training) <- gsub(pattern = "avNNet", replacement = "Averaged Neural Net", rownames(down_training))
rownames(down_training) <- gsub(pattern = "mlp", replacement = "Multlayer Perceptron", rownames(down_training))
rownames(down_training) <- gsub(pattern = "rbfDDA", replacement = "Radial Basis Function Network ", rownames(down_training))
rownames(down_training) <- gsub(pattern = "knn", replacement = "KNN", rownames(down_training))
rownames(down_training) <- gsub(pattern = "svmRadialWeights", replacement = "SVM", rownames(down_training))









### evaluate models and computational time here ###
### replace names for tables ###

rownames(down_validation) <- gsub(pattern = "glm", replacement = "Logit", rownames(down_validation))
rownames(down_validation) <- gsub(pattern = "null", replacement = "Null", rownames(down_validation))
rownames(down_validation) <- gsub(pattern = "rpart", replacement = "CART", rownames(down_validation))
rownames(down_validation) <- gsub(pattern = "Logitnet", replacement = "Elastic Net", rownames(down_validation))
rownames(down_validation) <- gsub(pattern = "LogitBoost", replacement = "Bosted Logit", rownames(down_validation))
rownames(down_validation) <- gsub(pattern = "earth", replacement = "MARS", rownames(down_validation))
rownames(down_validation) <- gsub(pattern = "nb", replacement = "Naive Bayes", rownames(down_validation))
rownames(down_validation) <- gsub(pattern = "knn", replacement = "KNN", rownames(down_validation))
rownames(down_validation) <- gsub(pattern = "ranger", replacement = "Random Forest", rownames(down_validation))
rownames(down_validation) <- gsub(pattern = "cforest", replacement = "Conditional Inference Trees", rownames(down_validation))
rownames(down_validation) <- gsub(pattern = "C5", replacement = "C5.0", rownames(down_validation))
rownames(down_validation) <- gsub(pattern = "avNNet", replacement = "Averaged Neural Net", rownames(down_validation))
rownames(down_validation) <- gsub(pattern = "mlp", replacement = "Multlayer Perceptron", rownames(down_validation))
rownames(down_validation) <- gsub(pattern = "rbfDDA", replacement = "Radial Basis Function Network", rownames(down_validation))
rownames(down_validation) <- gsub(pattern = "svmRadialWeights", replacement = "SVM", rownames(down_training))


### make tables ###
training_perf<-down_training[order(down_training$logLoss), , drop = FALSE]
training_perf$time<-training_perf$time/60

valid<-dplyr::select(down_validation, auc, sensitivity, specificity, accuracy, precision, logLoss)
validation_perf<-valid[order(valid$logLoss), , drop = FALSE]

if(!dir.exists("Tables")) dir.create("Tables")
save(training_perf, file="Tables/training_perf.Rdata")
save(validation_perf, file="Tables/validation_perf.Rdata")

### output to latex ###
library(Hmisc)
latex(round(training_perf, 3), file="")
latex(round(validation_perf, 3), file="")


### re do ensemble here
pred_down <- as.matrix(
       foreach(i = models_down, .combine = cbind.data.frame) %do% {
               # grabbing out of sample predictions
               
               bar <- as.tbl(models_down$pred) %>% 
                       arrange(rowIndex) %>% 
                       dplyr::select(yes)
               bar
})

y_down<-as.tbl(models_down[[1]]$pred$obs)





### ensemble for the validation set ###
pred_validation<-foreach(i=1:length(models_down), .combine=cbind) %do% {
        
        ### validation performance ###
        pred<-predict.train(models_down[[i]], 
                         newdata = dplyr::select(downevalDat, -ddy),
                         type = "prob")# this might be the issue
        
        p<-dplyr::select(pred, yes)
        colnames(p)<-models_down[[i]]$method
        p
}

pred_validation<-as.matrix(pred_validation)

y.out<-as.matrix(ifelse(downevalDat$B_attacked=="yes", 1, 0))


        
# 
EnsembleLL<-myLL(downevalDat$B_attacked, pred_validation%*%weights)

# make table
ensemble_weights<-as.matrix(round(weights, 3))

rownames(ensemble_weights) <- gsub(pattern = "glm", replacement = "Logit", rownames(ensemble_weights))
rownames(ensemble_weights) <- gsub(pattern = "Logit1", replacement = "Null", rownames(ensemble_weights))
rownames(ensemble_weights) <- gsub(pattern = "rpart", replacement = "CART", rownames(ensemble_weights))
rownames(ensemble_weights) <- gsub(pattern = "Logitnet", replacement = "Elastic Net", rownames(ensemble_weights))
rownames(ensemble_weights) <- gsub(pattern = "LogitBoost", replacement = "Bosted Logit", rownames(ensemble_weights))
rownames(ensemble_weights) <- gsub(pattern = "earth", replacement = "MARS", rownames(ensemble_weights))
rownames(ensemble_weights) <- gsub(pattern = "nb", replacement = "Naive Bayes", rownames(ensemble_weights))
rownames(ensemble_weights) <- gsub(pattern = "knn", replacement = "KNN", rownames(ensemble_weights))
rownames(ensemble_weights) <- gsub(pattern = "ranger", replacement = "Random Forest", rownames(ensemble_weights))
rownames(ensemble_weights) <- gsub(pattern = "cforest", replacement = "Conditional Inference Trees", rownames(ensemble_weights))
rownames(ensemble_weights) <- gsub(pattern = "C5", replacement = "C5.0", rownames(ensemble_weights))
rownames(ensemble_weights) <- gsub(pattern = "avNNet", replacement = "Averaged Neural Net", rownames(ensemble_weights))
rownames(ensemble_weights) <- gsub(pattern = "mlp", replacement = "Multlayer Perceptron", rownames(ensemble_weights))
rownames(ensemble_weights) <- gsub(pattern = "rbfDDA", replacement = "Radial Basis Function Network", rownames(ensemble_weights))
rownames(ensemble_weights) <- gsub(pattern = "svmRadialWeights", replacement = "SVM", rownames(ensemble_weights))

# 
EnsemblePRL<-abs((ensembleLL-down_validation["Null",]$logLoss)/down_validation["Null",]$logLoss)
ensemble_perf<-round(rbind(EnsembleLL, EnsemblePRL),3)

#
ensemble_table<-rbind(ensemble_weights, ensemble_perf)
save(ensemble_table, file="Tables/ensemble_table.Rdata")

latex(ensemble_table, file="")

#### see if tuning parameters remain stable across imputed data ####
set.seed(90210)
seeds <- sample(1:5, size = 5, replace = TRUE)
tune_rf <- vector(mode = "list", length = 5)
tune_c5 <- tune_rf
for(i in 1:(length(list.files("UD"))-1)){
        
        dat <- as.tbl(fread(paste0(sprintf("DD/data-DD-%02d-", i),
                                   sprintf("%02d.csv", seeds[i])))) %>% 
                mutate(A_ccode = makeCCode(A_ccode), B_ccode = makeCCode(B_ccode),
                       ud_contiguity = factor(ud_contiguity,
                                              levels = c("none", "water400", "water150",
                                                         "water024", "water012", 
                                                         "direct")),
                       ud_jointDem = ifelse(A_polity >= 6 & B_polity >= 6, 1, 0),
                       ud_distance = log(ud_distance),
                       ud_peaceYears2 = ud_peaceYears^2 / 100,
                       ud_peaceYears3 = ud_peaceYears^3 / 1000,
                       B_attacked = factor(B_attacked, labels = c("no", "yes"))) %>%
                select(B_attacked, everything()) %>% 
                group_by(dd) %>% 
                mutate(ud_rival = lag(ud_rival)) %>% 
                ungroup()
        dat$ud_rival[is.na(dat$ud_rival) & dat$year != 1870] <- 0
        dat <- filter(dat, year > 1870)
        
        omit <- c("A_cyear", "B_cyear", "A_ccode", "B_ccode", "dd",
                  "A_stateabb", "B_stateabb", "A_milexNI", "B_milexNI",
                  "A_polityNI", "B_polityNI", "A_attacked", "A_milex",
                  "ud_ongoingMID")
        dat <- select(dat, -one_of(omit))
        
        #### basic CV set-up ####
        
        ### data partitioning -- since we have so much, make a true vault data set.
        ### maintain class proportions with stratified random sampling
        set.seed(90210)
        split1 <- createDataPartition(dat$B_attacked, p = .6)[[1]]
        trainDat <- dat[split1,]
        other <- dat[-split1,]
        
        set.seed(555)
        split2 <- createDataPartition(other$B_attacked, p = 1/3)[[1]]
        evalDat <- other[split2,]
        testDat <- other[-split2,]
        rm(dat, other)
        
        # downsample
        zeroes <- filter(trainDat, B_attacked == "no")
        ones <- filter(trainDat, B_attacked == "yes")
        
        set.seed(90210)
        zSamp <- sample_n(tbl = zeroes, 
                          size = nrow(ones) * 100, 
                          replace = TRUE)
        oSamp <- sample_n(tbl = ones,
                          size = nrow(ones),
                          replace = TRUE)
        downDat <- rbind.data.frame(zSamp, oSamp)
        rm(zSamp, oSamp)
        
        set.seed(555)
        down_rf <- train(B_attacked ~ .,
                         data = select(downDat, -ddy),
                         method = "rf",
                         trControl = ctrlParallel, 
                         tuneLength = 5,
                         metric = "ROC")
        
        tune_rf[[i]] <- down_rf$bestTune
        
        set.seed(555)
        down_C5 <- train(B_attacked ~ .,
                         data = select(downDat, -ddy),
                         method = "C5.0",
                         trControl = ctrlParallel, 
                         tuneLength = 5,
                         metric = "ROC")
        
        tune_c5[[i]] <- down_C5$bestTune
        
        cat(i, "out of 5 complete.\n")
        
}




