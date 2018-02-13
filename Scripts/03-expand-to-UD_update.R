####
##
##  name:  03-expand-to-UD.R
##  date:  2017-01-23
##  what:  expands the CY-imputed data to the UD level.
##
####

setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_2/Measure/")

cat("03:  now expanding country-year data to undirected dyads.\n")

#### packages ####
library(dplyr)
library(tidyr)
library(foreach)
library(doParallel)
library(sqldf)

#### data #### 
load("data-CY_imputed.RData")

#### make universe ####
UDcases <- as.tbl(
  sqldf("select A.ccode A_ccode, B.ccode B_ccode, year,
        A.cyear A_cyear, B.cyear B_cyear
        from CYdata A join CYdata B using (year)
        where A.ccode < B.ccode
        order by A.ccode, year")
  )
UDcases <- UDcases %>%
  mutate(ud = paste(A_ccode, B_ccode, sep = "_"),
         udy = paste(A_ccode, B_ccode, year, sep = "_"))

#### add data we want ####
adat <- select(CYdata, -year, -ccode)
names(adat) <- paste0("A_", names(adat))
bdat <- select(CYdata, -year, -ccode)
names(bdat) <- paste0("B_", names(bdat))

adat <- data.table(adat)
bdat <- data.table(bdat)
UDcases <- data.table(UDcases)

UDdata <- merge(UDcases, adat, by = "A_cyear")
UDdata <- as.tbl(merge(UDdata, bdat, by = "B_cyear")) %>% arrange(udy)
rm(adat, bdat)

#### do it for the imputed sets ####

UD_imp <- foreach(i = 1:length(CY_imp), .packages = c("dplyr")) %do% {
                        
  CYi <- as.tbl(CY_imp[[i]])
  
  adat <- select(CYi, -year, -ccode)
  names(adat) <- paste0("A_", names(adat))
  adat <- data.table(adat)
  bdat <- select(CYi, -year, -ccode)
  names(bdat) <- paste0("B_", names(bdat))
  bdat <- data.table(bdat)
  
  foo <- merge(UDcases, adat, by = "A_cyear")
  as.tbl(merge(foo, bdat, by = "B_cyear")) %>% arrange(udy)
  
}
rm(CYi, UDcases, foo, adat, bdat, i)
if(!dir.exists("UD")) dir.create("UD")
write.csv(UDdata, file = "UD/data-UDmaster.csv", row.names = FALSE)
for(i in 1:length(UD_imp)){
  write.csv(UD_imp[[i]],
            file = paste0(sprintf("UD/data-UD-%02d", i), ".csv"), 
            row.names = FALSE)
}
rm(i)

#### bye ####
cat("03:  undirected dyad data constructed and saved to UD directory.\n")
rm(list = setdiff(ls(), lsf.str()))
gc(verbose = FALSE)