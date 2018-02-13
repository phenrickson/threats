####
##
##  name:  06-assessClassifiers.R
##  date:  2017-01-26
##  what:  this assesses various classifers
##
####

setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_2/Measure/")

cat("06:  now assessing classifiers.\n")
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

#### function ####
myLL <- function(y, p) 
  mean(-1 * (y == "yes") * log(p + .Machine$double.eps) - 
            (y == "no")  * log(1 - p + .Machine$double.eps))

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

### parallel backend
numCore <- detectCores() - 2
registerDoParallel(cores = numCore)

### controls
ctrl <- trainControl(method = "cv", 
                     classProbs = TRUE,
                     summaryFunction = fiveStats,
                     verboseIter = TRUE,
                     allowParallel = FALSE)

ctrlJustRun <- ctrl
ctrlJustRun$method <- "none"

ctrlParallel <- ctrl
ctrlParallel$verboseIter <- FALSE
ctrlParallel$allowParallel <- TRUE

### computational time of each classifier here ###
### baseline null ###
set.seed(555)
baseline_null<-suppressWarnings(### 0s and 1s
        train(B_attacked ~ 1,
              data = dplyr::select(trainDat, -ddy),
              method = "glm",
              trControl = ctrlParallel,   ### still want stats
              metric = "logLoss"))

if(!dir.exists("Models")) dir.create("Models")
save(baseline_null, file = "Models/baseline_null.Rdata")
rm(baseline_null)

### baseline logit ###
set.seed(555)
baseline_glm<-suppressWarnings(### 0s and 1s
        train(B_attacked ~ .,
              data = dplyr::select(trainDat, -ddy),
              method = "glm",
              trControl = ctrlParallel,   ### still want stats
              metric = "logLoss"))

save(baseline_glm, file = "Models/baseline_glm.Rdata")
rm(baseline_glm)

### baseline boosted logit ###
set.seed(555)
baseline_LogitBoost<-suppressWarnings(### 0s and 1s
        train(B_attacked ~ .,
              data = dplyr::select(trainDat, -ddy),
              method = "LogitBoost",
              trControl = ctrlParallel, ### still want stats
              tuneLength=5, 
              metric = "logLoss"))

save(baseline_LogitBoost, file = "Models/baseline_LogitBoost.Rdata")
rm(baseline_LogitBoost)

### baseline elastic net ###
baseline_glmnet<-suppressWarnings(
        train(B_attacked ~ .,
              data = dplyr::select(trainDat, -ddy),
              method = "glmnet",
              trControl = ctrlParallel, 
              tuneGrid = expand.grid(.alpha = c(0,.2,.4,.6,.8,1),
                                     .lambda = c(0.00001,
                                                 0.0001,
                                                 0.001,
                                                 0.01)),
              tuneLength = 5,
              metric = "logLoss",
              preProcess=c("center", "scale")))

save(baseline_glmnet, file = "Models/baseline_glmnet.Rdata")
rm(baseline_glmnet)

### baseline naive bayes ###
set.seed(555)
baseline_nb<-train(B_attacked ~ .,
                      data = dplyr::select(trainDat, -ddy),
                      method = "nb",
                      trControl = ctrlParallel, 
                      tuneLength = 5,
                      metric = "logLoss")

save(baseline_nb, file = "Models/baseline_nb.Rdata")
rm(baseline_nb)

### baseline mars ###
set.seed(555)
baseline_mars<-train(B_attacked ~ .,
                      data = dplyr::select(trainDat, -ddy),
                      method = "earth",
                      trControl = ctrlParallel, 
                      tuneLength = 5,
                      metric = "logLoss")

save(baseline_mars, file = "Models/baseline_mars.Rdata")
rm(baseline_mars)

### baseline knn ###
### THIS ONE IS MURDER ###
#set.seed(555)
#baseline_knn<-train(B_attacked ~ .,
#                      data = dplyr::select(trainDat, -ddy),
#                      method = "knn",
#                      trControl = ctrlParallel, 
#                      tuneLength = 5,
#                      metric = "logLoss")

#save(baseline_knn, file = "Models/baseline_knn.Rdata")
#rm(baseline_knn)

### baseline cart ###
set.seed(555)
baseline_rpart<-train(B_attacked ~ .,
                      data = dplyr::select(trainDat, -ddy),
                      method = "rpart",
                      trControl = ctrlParallel, 
                      tuneLength = 5,
                      metric = "logLoss")

save(baseline_rpart, file = "Models/baseline_rpart.Rdata")
rm(baseline_rpart)


# send an email
send_message(mime(from="phil.henrickson@gmail.com", to="phil.henrickson@gmail.com", subject="Code Finished", "Woo!"))





### This part is computationally infeasible at the minute ###
### baseline rf using ranger ###
set.seed(555)
baseline_rf<-train(B_attacked ~ .,
                   data = dplyr::select(trainDat, -ddy),
                   method = "ranger",
                   trControl = ctrlParallel, 
                   tuneLength = 5,
                   metric = "logLoss")

save(baseline_rf, file = "Models/baseline_rf.Rdata")
rm(baseline_rf)

# 

### baseline adaboost ###
set.seed(555)
baseline_adaboost<-train(B_attacked ~ .,
                         data = dplyr::select(trainDat, -ddy),
                         method = "adaboost",
                         trControl = ctrlParallel, 
                         tuneLength = 5,
                         metric = "logLoss")

save(baseline_adaboost, file = "Models/baseline_adaboost.Rdata")
rm(baseline_adaboost)


### baseline C5.0 ###
set.seed(555)
baseline_C5.0<-train(B_attacked ~ .,
                     data = dplyr::select(trainDat, -ddy),
                     method = "C5.0",
                     trControl = ctrlParallel, 
                     tuneLength = 5,
                     metric = "logLoss")

save(baseline_C5.0, file = "Models/baseline_C5.0.Rdata")
rm(baseline_C5.0)

### baseline svm ###
set.seed(555)
baseline_svm<-train(B_attacked ~ .,
                    data = dplyr::select(trainDat, -ddy),
                    method = "svmRadialWeights",
                    trControl = ctrlParallel, 
                    tuneLength = 5,
                    metric = "logLoss")

save(baseline_svm, file = "Models/baseline_svm.Rdata")
rm(baseline_svm)

### baseline pcaNNet neural net
set.seed(555)
#trans <- preProcess(dplyr::select(trainDat, -ddy, -B_attacked), 
#                   method = c("center", "scale", "pca"))
#transformed <- as.tbl(predict(trans, trainDat))
#transformed <- transformed[complete.cases(transformed),]

baseline_nnet <- train(B_attacked ~ .,
                       data = dplyr::select(transformed, -ddy),
                       method = "avNNet",
                       tuneLength = 5,
                       trControl = ctrl,
                       metric = "logLoss")

save(baseline_nnet, file = "Models/baseline_nnet.Rdata")
rm(baseline_nnet)


### baseline radial basis function network ###
set.seed(555)
trans <- preProcess(dplyr::select(trainDat, -ddy, -B_attacked), 
                    method = c("center", "scale", "pca"))
transformed <- as.tbl(predict(trans, trainDat))
transformed <- transformed[complete.cases(transformed),]

baseline_mlp <- train(B_attacked ~ .,
                      data = dplyr::select(transformed, -ddy),
                      method = "mlp",
                      tuneLength = 5,
                      trControl = ctrl,
                      metric = "logLoss")

save(baseline_mlp, file = "Models/baseline_mlp.Rdata")
rm(baseline_mlp)


### baseline multilayer perceptron ###
set.seed(555)
trans <- preProcess(dplyr::select(trainDat, -ddy, -B_attacked), 
                    method = c("center", "scale", "pca"))
transformed <- as.tbl(predict(trans, trainDat))
transformed <- transformed[complete.cases(transformed),]

baseline_mlp <- train(B_attacked ~ .,
                       data = dplyr::select(transformed, -ddy),
                       method = "mlp",
                       tuneLength = 5,
                       trControl = ctrl,
                       metric = "logLoss")

save(baseline_mlp, file = "Models/baseline_mlp.Rdata")
rm(baseline_mlp)


# load what has been saved to this point
load("Models/baseline_null.Rdata")
load("Models/baseline_glm.Rdata")
load("Models/baseline_LogitBoost.Rdata")
#load("Models/baseline_knn.Rdata")
load("Models/baseline_glmnet.Rdata")
load("Models/baseline_mars.Rdata")
load("Models/baseline_nb.Rdata")
load("Models/baseline_rpart.Rdata")


### grab all models run so far ###
models_baseline<-lapply(ls(pattern="baseline_"), get)

# wipe the individual models to save memory
rm(list=ls(pattern="baseline_"))

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

### store times, training performance, and then validation performance###
out_training<-foreach(i=1:length(models_baseline), .combine=rbind) %do% {
        
        ### store time to train ###
        time<-models_baseline[[i]]$times$everything[3]
        perf_min<-models_baseline[[i]]$results[which.min(models_baseline[[i]]$results[,"logLoss"]),]
        
        ### training performance ###
        training_perf<-dplyr::select(perf_min, ROC, Sens, Spec, Accuracy, Kappa, logLoss)
        out<-cbind(time, training_perf)
        rownames(out)<-models_baseline[[i]]$method
        out
}


### predict the validation set ###
out_validation<-foreach(i=1:length(models_baseline), .combine=rbind) %do% {
        
        ### validation performance ###
        p<-suppressWarnings(predict.train(models_baseline[[i]], 
                                          newdata = dplyr::select(evalDat, -ddy),
                                          type = "prob")[,2])
        
        y<-evalDat$B_attacked
        out<-getResults(y=y, p=p, cut=0.5)
        
        print("done")
        
        rownames(out)<-models_baseline[[i]]$method
        out
}

### replace names for tables ###
rownames(out_training) <- gsub(pattern = "glm", replacement = "Logit", rownames(out_training))
rownames(out_training) <- gsub(pattern = "Logit1", replacement = "Null", rownames(out_training))
rownames(out_training) <- gsub(pattern = "rpart", replacement = "CART", rownames(out_training))
rownames(out_training) <- gsub(pattern = "Logitnet", replacement = "Elastic Net", rownames(out_training))
rownames(out_training) <- gsub(pattern = "LogitBoost", replacement = "Bosted Logit", rownames(out_training))
rownames(out_training) <- gsub(pattern = "earth", replacement = "MARS", rownames(out_training))
rownames(out_training) <- gsub(pattern = "nb", replacement = "Naive Bayes", rownames(out_training))
rownames(out_training) <- gsub(pattern = "knn", replacement = "KNN", rownames(out_training))


rownames(out_validation) <- gsub(pattern = "glm", replacement = "Logit", rownames(out_validation))
rownames(out_validation) <- gsub(pattern = "Logit1", replacement = "Null", rownames(out_validation))
rownames(out_validation) <- gsub(pattern = "rpart", replacement = "CART", rownames(out_validation))
rownames(out_validation) <- gsub(pattern = "Logitnet", replacement = "Elastic Net", rownames(out_validation))
rownames(out_validation) <- gsub(pattern = "LogitBoost", replacement = "Bosted Logit", rownames(out_validation))
rownames(out_validation) <- gsub(pattern = "earth", replacement = "MARS", rownames(out_validation))
rownames(out_validation) <- gsub(pattern = "nb", replacement = "Naive Bayes", rownames(out_validation))
rownames(out_validation) <- gsub(pattern = "knn", replacement = "KNN", rownames(out_validation))


### make tables ###
training_perf<-out_training[order(out_training$logLoss), , drop = FALSE]
training_perf$time<-training_perf$time/60


valid<-dplyr::select(out_validation, auc, sensitivity, specificity, accuracy, precision, logLoss)
validation_perf<-valid[order(valid$logLoss), , drop = FALSE]

if(!dir.exists("Tables")) dir.create("Tables")
save(training_perf, file="Tables/training_perf")
save(validation_perf, file="Tables/validation_perf")

### output to latex ###
library(Hmisc)
latex(round(training_perf, 3), file="")
latex(round(validation_perf, 3), file="")
