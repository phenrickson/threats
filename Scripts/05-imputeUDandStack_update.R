####
##
##  name:  05-imputeUDandStack.R
##  date:  2017-01-25
##  what:  this imputes the UD data and stacks into DD, then saving into DD
##
####

setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_2/Measure/")

cat("05:  now imputing UD data and stacking to DD.\n")

#### packages ####
library(dplyr)
library(tidyr)
library(Amelia)
library(sqldf)
library(data.table)
library(foreach)
library(doParallel)

source("fn-ccodeCleaner.R")

#### Amelia prep ####
foo <- dplyr::as.tbl(fread("UD/data-UD-01.csv"))

### big ones here are the imports and exports -- severe skew.  DOE scores are
### also skewed because most density is in stalemate
#plot(density(na.omit(foo$B_imports)))

toTrans <- c("A_imports", "B_imports", "A_victory", "B_victory")

ks_stat <- function(x) {
  x <- x[!is.na(x)]
  ans <- ecdf(x)(x) - pnorm(x, mean = mean(x), sd = sd(x))
  max(abs(ans))
}

theta_candidates <- 2^seq(-25, 25)
theta <- foreach (name = toTrans, .combine = "c") %do% {
  x <- foo[[name]]
  loss <- foreach (s = theta_candidates) %do% {
    ks_stat(asinh(s * x))
  }
  theta_candidates[which.min(loss)]
}
names(theta) <- toTrans

rawFoo <- foo
for (name in toTrans) {
  x <- foo[[name]]
  s <- theta[name]
  foo[[name]] <- asinh(s * x)
}
rm(name, loss, s, theta_candidates, x)

bds <- matrix(c(
  
  which(names(foo) == "A_victory"), 0, 1,
  which(names(foo) == "B_victory"), 0, 1,
  which(names(foo) == "A_imports"), 0, .Machine$double.xmax,
  which(names(foo) == "B_imports"), 0, .Machine$double.xmax

), ncol = 3, byrow = T)

#### set the ridge priors ####
toPrior <- c("A_imports", "B_imports")
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
laz <- foo %>% 
  group_by(ud) %>%
  summarise_each(funs(latest_all_zero(., year)),
                 vars = one_of(toPrior))
names(laz)[2:ncol(laz)] <- toPrior
laz <- left_join(foo %>% select(ud, year),
                 laz,
                 by = "ud")

### Construct matrix of observation-level priors to pass to Amelia
prs <- foreach (v = toPrior, .combine = "rbind") %do% {
  ## Identify observations that are both missing and before the relevant year
  rows <- which(is.na(select(foo, matches(v))) & laz[["year"]] < laz[[v]])
  
  ## Identify corresponding column of the data being passed to amelia()
  col <- which(names(foo) == v)
  
  ## Prior has zero mean
  mu <- 0
  
  ## Set standard deviation of prior to the s.d. of the variable in the
  ## overall data
  sigma <- sd(foo[[v]], na.rm = TRUE)
  
  if (length(rows) > 0) {
    cbind(rows, col, mu, sigma)
  } else {
    matrix(nrow = 0, ncol = 4)
  }
}
rm(laz, col, mu, rows, sigma, toPrior, v)

ids <- c("A_ccode", "B_ccode", "A_cyear", "B_cyear", "udy", "A_stateabb",
         "B_stateabb", "A_occupied", "A_milexNI", "A_polityNI", "B_occupied",
         "B_milexNI", "B_polityNI", "A_attacked", "B_attacked")
ords <- c("A_polity", "B_polity", "A_numMIDs", "B_numMIDs", "ud_bilatno",
          "ud_multino", "ud_contiguity")

noms <- c("A_ongoingCivil", "B_ongoingCivil", "A_majPow", "B_majPow",
          "A_ongoingMID", "B_ongoingMID", "A_instability", "B_instability",
          "ud_ongoingMID", "ud_midInLast10", "ud_alliance", "ud_nonagg",
          "ud_rival", "A_terrChallenge", "B_terrChallenge")

rm(foo, rawFoo)

#### run the imputation ####
numImput <- 5
numCore <- min(numImput, detectCores() - 2)

if(.Platform$OS.type == "unix"){
  registerDoParallel(cores = numCore)
}else{
  cl <- makeCluster(numCore)
  registerDoParallel(cl)
}

set.seed(90210)
seeds <- sample(1:10000, numImput)

if(!dir.exists("DD")) dir.create("DD")

for(i in 1:(length(list.files("UD")) - 1)){
  
  foo <- dplyr::as.tbl(fread(paste0(sprintf("UD/data-UD-%02d", i), ".csv"))) %>%
    mutate(A_ccode = makeCCode(A_ccode), B_ccode = makeCCode(B_ccode),
           ud_contiguity = factor(ud_contiguity,
                                  levels = c("none", "water400", "water150",
                                             "water024", "water012", 
                                             "direct")))
  for (name in toTrans) {
    x <- foo[[name]]
    s <- theta[name]
    foo[[name]] <- asinh(s * x)
  }
  foo <- as.data.frame(foo)
  
  UD_imp <- foreach(j = 1:numImput, .packages = "Amelia") %dopar% {
    
    set.seed(seeds[j])
    dplyr::as.tbl(amelia(x = foo,
                  m = 1,
                  ts = "year",
                  cs = "ud",
                  p2s = 0,
                  polytime = 3,
                  intercs = FALSE,
                  bounds = bds,
                  priors = prs,
                  ords = ords,
                  noms = noms,
                  idvars = ids,
                  max.resample = 1000,
                  empri = 0.01 * nrow(foo)
           )$imputations$imp1)
    
  }
  
  cat("now about to stack and save for UD level", i, ".\n")
  
  ### stack each into a DD and save to the DD directory
  for(j in 1:length(UD_imp)){
    
    top <- dplyr::as.tbl(UD_imp[[j]])  ### this will be kept untouched
    bottom <- top 
    
    keepers <- bottom %>%
      select(-starts_with("A_"), -starts_with("B_"))
    switchers <- bottom %>% 
      select(starts_with("A_"), starts_with("B_"), udy)
    
    nowB <- switchers %>%
      select(udy, starts_with("A_"))
    
    names(nowB)[2:length(names(nowB))] <- 
      paste0("B_",
             substring(names(nowB)[2:length(names(nowB))],
                       first = 3)
      )
    
    nowA <- switchers %>%
      select(udy, starts_with("B_"))
    
    names(nowA)[2:length(names(nowA))] <- 
      paste0("A_",
             substring(names(nowA)[2:length(names(nowA))],
                       first = 3)
      )
    
    bottom <- left_join(x = keepers, y = nowA, by = "udy")
    bottom <- left_join(x = bottom,  y = nowB, by = "udy")
    rm(nowA, nowB, keepers, switchers)
    
    bottom <- bottom %>%
      mutate(ud = paste(A_ccode, B_ccode, sep = "_"),
             udy = paste(A_ccode, B_ccode, year, sep = "_")) %>%
      select(match(names(top), names(bottom)))
    
    ddj <- dplyr::as.tbl(rbind.data.frame(top, bottom)) %>% 
      rename("ddy" = udy, "dd" = ud) %>% 
      arrange(ddy)
    rm(top, bottom)
    
    fwrite(ddj, file = paste0(sprintf("DD/data-DD-%02d-", i),
                              sprintf("%02d.csv", j)))

  }

}


#### bye ####
cat("05:  UDs imputed and stacked, saved to DD directory.\n")
rm(list = setdiff(ls(), lsf.str()))
gc(verbose = FALSE)