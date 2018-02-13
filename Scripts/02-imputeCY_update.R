####
##
##  name:  02-imputeCY.R
##  date:  2017-01-23
##  what:  this addresses missingness at the CY level
##
####

setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_2/Measure/")

cat("02:  now imputing country-year data.\n")
#### packages ####
library(dplyr)
library(tidyr)
library(data.table)
library(foreach)
library(Amelia)
library(doParallel)

#### data #### 
CYdata <- as.tbl(fread("data-post-01.csv")) %>%
  select(-cinc)

#### Amelia-prep ####
toTrans <- c("irst", "milex", "milper", "pec", "tpop", "upop", "imports",
             "exports")
ks_stat <- function(x) {
  x <- x[!is.na(x)]
  ans <- ecdf(x)(x) - pnorm(x, mean = mean(x), sd = sd(x))
  max(abs(ans))
}

theta_candidates <- 2^seq(-25, 25)
theta <- foreach (name = toTrans, .combine = "c") %do% {
  x <- CYdata[[name]]
  loss <- foreach (s = theta_candidates) %do% {
    ks_stat(asinh(s * x))
  }
  theta_candidates[which.min(loss)]
}
names(theta) <- toTrans

rawCYdata <- CYdata
for (name in toTrans) {
  x <- CYdata[[name]]
  s <- theta[name]
  CYdata[[name]] <- asinh(s * x)
}
rm(name, loss, s, theta_candidates, x, toTrans)

bds <- matrix(c(
  
  which(names(CYdata) == "irst"), 0, .Machine$double.xmax,
  which(names(CYdata) == "milex"), 0, .Machine$double.xmax,
  which(names(CYdata) == "milper"), 0, .Machine$double.xmax,
  which(names(CYdata) == "pec"), 0, .Machine$double.xmax,
  which(names(CYdata) == "upop"), 0, .Machine$double.xmax,
  which(names(CYdata) == "tpop"), 0, .Machine$double.xmax,
  which(names(CYdata) == "imports"), 0, .Machine$double.xmax,
  which(names(CYdata) == "exports"), 0, .Machine$double.xmax
  
  
), ncol = 3, byrow = T)

#### set the ridge priors ####
toPrior <- c("irst", "milex", "milper", "pec", "upop", 
             "tpop", "imports", "exports")
latest_all_zero <- function(x, year)
{
  ## Strip out missing years
  year <- year[!is.na(x)]
  x <- x[!is.na(x)]
  
  ## We want the latest year at which the cumulative sum of the data series is
  ## zero
  ##
  ## Suppressing warnings because max() of a length-zero vector throws one
  suppressWarnings(max(year[cumsum(x) == 0]))
}

### Calculate the relevant "last all-zero year" for each series of each CINC
### component variable
laz <- CYdata %>% 
  group_by(ccode) %>%
  summarise_each(funs(latest_all_zero(., year)),
                 vars = one_of(toPrior))
names(laz)[2:ncol(laz)] <- toPrior
laz <- left_join(CYdata %>% select(ccode, year),
                 laz,
                 by = "ccode")

### Construct matrix of observation-level priors to pass to Amelia
prs <- foreach (v = toPrior, .combine = "rbind") %do% {
  ## Identify observations that are both missing and before the relevant year
  rows <- which(is.na(select(CYdata, matches(v))) & laz[["year"]] < laz[[v]])
  
  ## Identify corresponding column of the data being passed to amelia()
  col <- which(names(CYdata) == v)
  
  ## Prior has zero mean
  mu <- 0
  
  ## Set standard deviation of prior to the s.d. of the variable in the
  ## overall data
  sigma <- sd(CYdata[[v]], na.rm = TRUE)
  
  if (length(rows) > 0) {
    cbind(rows, col, mu, sigma)
  } else {
    matrix(nrow = 0, ncol = 4)
  }
}
rm(laz, col, mu, rows, sigma, toPrior, v)

#### run the imputation ####
numImput <- 5
CYdata$milexNI <- CYdata$milex
CYdata$polityNI <- CYdata$polity
numCore <- min(numImput, detectCores() - 2)
CYdata <- as.data.frame(CYdata)
set.seed(90210)
seeds <- sample(1:10000, numImput)
if(.Platform$OS.type == "unix"){
  
  registerDoParallel(cores = numCore)
  
}else{
  
  cl <- makeCluster(numCore)
  registerDoParallel(cl)
  
}

### levels of analysis
ids <- c("stateabb", "cyear", "milexNI", "polityNI", "occupied")
ords <- c("polity", "numMIDs")
noms <- c("ongoingCivil", "ongoingMID", "instability", "majPow")

CY_imputed <- foreach(i = 1:numImput, .packages = "Amelia") %dopar% {
  
  set.seed(seeds[i])
  amelia(x = CYdata,
         m = 1,
         ts = "year",
         cs = "ccode",
         p2s = 0,
         polytime = 3,
         intercs = T,
         bounds = bds,
         priors = prs,
         ords = ords,
         noms = noms,
         idvars = ids,
         max.resample = 1000,
         empri = 0.01 * nrow(CYdata)
  )$imputations
  
}

CY_imp <- foreach(i = 1:numImput) %do% as.tbl(CY_imputed[[i]]$imp)

save(CYdata, CY_imp, theta, file = "data-CY_imputed.RData")

#### bye ####
cat("02:  country-year data imputed.\n")
rm(list = setdiff(ls(), lsf.str()))
gc(verbose = FALSE)