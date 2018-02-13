####
##
##  name:  06-downsample.R
##  date:  2017-01-26
##  what:  this identifies the right proportion for downsampling
##
####

setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_2/Measure/")

cat("07:  now downsampling.\n")
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
                     allowParallel = FALSE,
                     savePredictions="final")

ctrlJustRun <- ctrl
ctrlJustRun$method <- "none"

ctrlParallel <- ctrl
ctrlParallel$verboseIter <- FALSE
ctrlParallel$allowParallel <- TRUE


#### down-sampled logit ####
## figuring out appropriate proportions to keep
mc <- 100
zeroes <- filter(trainDat, B_attacked == "no")
ones <- filter(trainDat, B_attacked == "yes")

set.seed(90210)
props <- c(1, 2, 3, 4, 5, 10, 25, 50, 100, 250, nrow(zeroes) / nrow(ones))
downSamps <- foreach(j = 1:length(props), .combine = rbind.data.frame) %do% {
        
        foreach(i = 1:mc, .combine = rbind.data.frame, .packages=c('dplyr', 'tidyr', 'caret')) %dopar% {
                
                zSamp <- dplyr::sample_n(tbl = zeroes, 
                                         size = nrow(ones) * props[j], 
                                         replace = TRUE)
                
                oSamp <- dplyr::sample_n(tbl = ones,
                                         size = nrow(ones),
                                         replace = TRUE)
                
                bootDat <- as.tbl(rbind.data.frame(zSamp, oSamp)) %>% 
                        arrange(ddy)
                
                boot_glm <- suppressWarnings(                    ### separation
                        train(B_attacked ~ .,
                              data = dplyr::select(bootDat, -ddy),
                              method = "glm",
                              trControl = ctrlJustRun,
                              metric = "logLoss"))                ### nothing to tune
                # we're evaluating on the validation set
                data.frame(multiplier = props[j],
                           logLoss = myLL(evalDat$B_attacked,
                                          predict(boot_glm, 
                                                  newdata = evalDat, type = "prob")[,2]))
                
        }
}

# set it up to email me when done #
library(gmailr)
send_message(mime(from="phil.henrickson@gmail.com", to="phil.henrickson@gmail.com", subject="Estimated Downsampling", "Done"))

save(downSamps, file="Models/downsamps.Rdata")
load("Models/downsamps.Rdata")

downSamps %>%
        dplyr::group_by(multiplier) %>%
        dplyr::summarise(avgLL = mean(logLoss),
                         se = sd(logLoss) / sqrt(n())) %>% 
        ungroup()

toGr <- downSamps %>% 
        group_by(multiplier) %>% 
        dplyr::summarise(avgLL = mean(logLoss), 
                         ub = quantile(logLoss, .95), 
                         lb = quantile(logLoss, .05)) %>% 
        ungroup()

save(toGr, file="Tables/toGr.Rdata")
latex(round(toGr, 3), file="")

plot_downSamps<-ggplot(toGr, aes(x = multiplier, y = avgLL, ymax = ub, ymin = lb)) + 
        geom_line() + 
        geom_pointrange()
plot_downSamps

if(!dir.exists("Figures")) dir.create("Figures")
ggsave("Figures/plot_downSamps.PNG")
rm(downSamps)


# the above might be an issue using log-loss as the metric
# rerun using ROC as the metric

mc <- 100
zeroes <- filter(trainDat, B_attacked == "no")
ones <- filter(trainDat, B_attacked == "yes")

set.seed(90210)
props<-c(1)

props <- c(1, 2, 3, 4, 5, 10, 25, 50, 100, 250, nrow(zeroes) / nrow(ones))

downSamps_ROC <- foreach(j = 1:length(props), .combine = rbind.data.frame) %do% {
        
        foreach(i = 1:mc, .combine = rbind.data.frame, .packages=c('dplyr', 'tidyr', 'caret')) %dopar% {
                
                zSamp <- dplyr::sample_n(tbl = zeroes, 
                                         size = nrow(ones) * props[j], 
                                         replace = TRUE)
                
                oSamp <- dplyr::sample_n(tbl = ones,
                                         size = nrow(ones),
                                         replace = TRUE)
                
                bootDat <- as.tbl(rbind.data.frame(zSamp, oSamp)) %>% 
                        arrange(ddy)
                
                boot_glm <- suppressWarnings(                    ### separation
                        train(B_attacked ~ .,
                              data = dplyr::select(bootDat, -ddy),
                              method = "glm",
                              trControl = ctrlJustRun,
                              metric = "ROC"))      
                
                y<-evalDat$B_attacked
                p<-predict(boot_glm, 
                           newdata = evalDat, type = "prob")[,2]

                ### nothing to tune
                # we're evaluating on the validation set
                data.frame(multiplier = props[j],
                           ROC=as.numeric(pROC::auc(response = y, predictor = p)))
                
        }
}

# set it up to email me when done #
library(gmailr)
send_message(mime(from="phil.henrickson@gmail.com", to="phil.henrickson@gmail.com", subject="Estimated Downsampling", "Done"))

save(downSamps_ROC, file="Models/downSamps_ROC.Rdata")
load("Models/downSamps_ROC.Rdata")

downSamps_ROC %>%
        dplyr::group_by(multiplier) %>%
        dplyr::summarise(avgLL = mean(logLoss),
                         se = sd(logLoss) / sqrt(n())) %>% 
        ungroup()

toGr <- downSamps_ROC %>% 
        group_by(multiplier) %>% 
        dplyr::summarise(avgLL = mean(logLoss), 
                         ub = quantile(logLoss, .95), 
                         lb = quantile(logLoss, .05)) %>% 
        ungroup()

save(toGr, file="Tables/toGr.Rdata")
latex(round(toGr, 3), file="")

plot_downSamps_ROC<-ggplot(toGr, aes(x = multiplier, y = avgLL, ymax = ub, ymin = lb)) + 
        geom_line() + 
        geom_pointrange()
plot_downSamps_ROC

if(!dir.exists("Figures")) dir.create("Figures")
ggsave("Figures/plot_downSamps_ROC.PNG")
rm(downSamps_ROC)


