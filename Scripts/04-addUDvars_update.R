####
##
##  name:  04-addUDvars.R
##  date:  2017-01-24
##  what:  this adds UD-level variables to the UD data
##
####
setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_2/Measure/")

cat("04:  now adding UD variables.\n")

#### packages ####
library(dplyr)
library(tidyr)
library(foreach)
library(data.table)
library(doParallel)
library(foreign)

source("fn-ccodeCleaner.R")

#### get a running "add to" list of cases ####
ud <- as.tbl(fread("UD/data-UDmaster.csv")) %>% 
        dplyr::rename("A_totalImports" = A_imports,
               "B_totalImports" = B_imports,
               "A_totalExports" = A_exports,
               "B_totalExports" = B_exports)
add <- data.table(select(ud, udy, ud, A_ccode, B_ccode, A_cyear, B_cyear, year,
                         A_stateabb, B_stateabb))


#### MID data ####
#### update here ###
mid <- as.tbl(fread("gml-midb-2.0.csv")) %>%
  group_by(dispnum) %>%
  filter(any(hostlev >= 4)) %>%
  ungroup() %>%
  select(dispnum, ccode, styear, endyear, sidea) %>%
  rename("dispNum" = dispnum) %>%
  filter(styear >= min(ud$year)) %>%
  mutate(ccode = makeCCode(ccode),
         cyear = paste(ccode, styear, sep = "_"),
         ccode = fixCYear(cyear)$fixedCCode,
         cyear = fixCYear(cyear)$fixedCYear) %>%
  group_by(dispNum) %>%
  filter(!any(is.na(ccode))) %>%
  ungroup() %>%
  select(-cyear)

add[, ud_ongoingMID := 0]
add[, ud_midInLast10 := 0]
add[, A_attacked := 0]
add[, B_attacked := 0]

disps <- unique(mid$dispNum)
for(i in 1:length(disps)){
  
  foo <- filter(mid, dispNum == disps[i])
  fooA <- filter(foo, sidea == 1) %>% select(ccode)
  fooB <- filter(foo, sidea == 0) %>% select(ccode)
  
  bar <- expand.grid(fooA$ccode, fooB$ccode) %>%
    rename("attacker" = Var1, "target" = Var2)
  bar$stYear <- foo$styear[1]
  bar$endYear <- foo$endyear[1]
  bar$a_t_year <- paste(bar$attacker, bar$target, bar$stYear, sep = "_")
  bar$t_a_year <- paste(bar$target, bar$attacker, bar$stYear, sep = "_")
  add[udy %in% bar$a_t_year, A_attacked := 1]
  add[udy %in% bar$t_a_year, B_attacked := 1]
  
  oYears <- foreach(j = 1:nrow(bar), .combine = c) %do% {
    
    c(paste(bar$attacker[j], bar$target[j], bar$stYear[j]:bar$endYear[j],
            sep = "_"),
      paste(bar$target[j], bar$attacker[j], bar$stYear[j]:bar$endYear[j],
            sep = "_"))
    
  }
  add[udy %in% oYears, ud_ongoingMID := 1]
  
  oYears10 <- foreach(j = 1:nrow(bar), .combine = c) %do% {
    
    c(paste(bar$attacker[j], bar$target[j], bar$stYear[j]:(bar$endYear[j]+10),
            sep = "_"),
      paste(bar$target[j], bar$attacker[j], bar$stYear[j]:(bar$endYear[j]+10),
            sep = "_"))
    
  }
  add[udy %in% oYears10, ud_midInLast10 := 1]
  rm(foo, bar, oYears, oYears10, fooA, fooB)
  #cat(i, "out of", length(disps), "complete.\n")

}
rm(i, j, disps, mid)

#### add in trade data ####
trade <- as.tbl(fread("dyadic_trade_3.0.csv")) %>%
  filter(year >= min(ud$year)) %>%
  select(ccode1, ccode2, year, flow1, flow2) %>%
  rename("A_ccode" = ccode1, "B_ccode" = ccode2, "A_imports" = flow1,
         "B_imports" = flow2) %>%
  mutate(A_ccode = makeCCode(A_ccode), B_ccode = makeCCode(B_ccode),
         A_cyear = paste(A_ccode, year, sep = "_"),
         B_cyear = paste(B_ccode, year, sep = "_"),
         A_ccode = fixCYear(A_cyear)$fixedCCode,
         B_ccode = fixCYear(B_cyear)$fixedCCode,
         A_cyear = fixCYear(A_cyear)$fixedCYear,
         B_cyear = fixCYear(B_cyear)$fixedCYear,
         ud = paste(A_ccode, B_ccode, sep = "_"),
         udy = paste(ud, year, sep = "_")) %>%
  mutate(A_imports = ifelse(A_imports == -9, NA, A_imports),
         B_imports = ifelse(B_imports == -9, NA, B_imports)) %>%
  filter(!is.na(A_ccode) & !is.na(B_ccode)) %>%
  select(udy, A_imports, B_imports)
add <- as.tbl(left_join(add, trade, by = "udy"))
rm(trade)

#### alliance data ####
atop <- as.tbl(fread("atop3_0dy.csv")) %>%
  select(mem1, mem2, year, defense, offense, neutral, nonagg, consul,
         shareob, bilatno, multino) %>%
  mutate(mem1 = makeCCode(mem1),
         mem2 = makeCCode(mem2),
         A_cyear = paste(mem1, year, sep = "_"),
         B_cyear = paste(mem2, year, sep = "_"),
         A_ccode = fixCYear(A_cyear)$fixedCCode,
         B_ccode = fixCYear(B_cyear)$fixedCCode,
         A_cyear = fixCYear(A_cyear)$fixedCYear,
         B_cyear = fixCYear(B_cyear)$fixedCYear) %>%
  select(-mem1, -mem2) %>% 
  filter(!is.na(A_ccode) & !is.na(B_ccode)) %>%
  mutate(ud = paste(A_ccode, B_ccode, sep = "_"),
         udy = paste(ud, year, sep = "_")) %>% 
  select(udy, defense, offense, neutral, nonagg, consul, bilatno, multino)

add <- data.table(add)
add[, ud_alliance := 0]
add[udy %in% atop$udy, ud_alliance := 1]
add[, ud_nonagg := 0]
add[udy %in% filter(atop, nonagg == 1)$udy, ud_nonagg := 1]
add <- as.tbl(add)
add <- left_join(add, select(atop, udy, bilatno, multino), by = "udy")
add <- add %>% 
  rename("ud_bilatno" = bilatno,
         "ud_multino" = multino) %>%
  mutate(ud_bilatno = ifelse(is.na(ud_bilatno), 0, ud_bilatno),
         ud_multino = ifelse(is.na(ud_multino), 0, ud_multino))
rm(atop)

#### rivalry ####
riv <- as.tbl(read.spss("rivallev.sav",
                        to.data.frame = T)) %>% 
  select(a, b, type, strtyr, endyr) %>% 
  rename("A_ccode" = a,
         "B_ccode" = b) %>% 
  mutate(A_ccode = makeCCode(A_ccode), B_ccode = makeCCode(B_ccode))

rivYears <- foreach(i = 1:nrow(riv), .combine = c) %do% {
  
  foo <- data.frame(A_ccode = riv$A_ccode[i], B_ccode = riv$B_ccode[i],
                    year = riv$strtyr[i]:riv$endyr[i]) %>%
    mutate(A_cyear = paste(A_ccode, year, sep = "_"),
           B_cyear = paste(B_ccode, year, sep = "_"),
           A_ccode = fixCYear(A_cyear)$fixedCCode,
           B_ccode = fixCYear(B_cyear)$fixedCCode,
           A_cyear = fixCYear(A_cyear)$fixedCYear,
           B_cyear = fixCYear(B_cyear)$fixedCYear) %>% 
    filter(!is.na(A_ccode) & !is.na(B_ccode)) %>%
    mutate(udy = paste(A_ccode, B_ccode, year, sep = "_"))
  foo$udy
  
}
rm(i, foo)
add <- add %>% 
  mutate(ud_rival = ifelse(udy %in% rivYears, 1, 0))
rm(riv, rivYears)

#### DOE scores ####
doe <- as.tbl(fread("results-predict-dir-dyad.csv")) %>% 
  filter(ccode_a < ccode_b & year >= min(ud$year)) %>%
  rename("A_ccode" = ccode_a, "B_ccode" = ccode_b) %>%
  mutate(A_ccode = makeCCode(A_ccode), B_ccode = makeCCode(B_ccode),
         A_cyear = paste(A_ccode, year, sep = "_"),
         B_cyear = paste(B_ccode, year, sep = "_"),
         A_ccode = fixCYear(A_cyear)$fixedCCode,
         B_ccode = fixCYear(B_cyear)$fixedCCode,
         A_cyear = fixCYear(A_cyear)$fixedCYear,
         B_cyear = fixCYear(B_cyear)$fixedCYear) %>% 
  filter(!is.na(A_ccode) & !is.na(B_ccode)) %>%
  mutate(udy = paste(A_ccode, B_ccode, year, sep = "_")) %>% 
  dplyr::select(udy, VictoryA, VictoryB) %>% 
  rename("A_victory" = VictoryA, "B_victory" = VictoryB)
add <- left_join(add, doe, by = "udy")
rm(doe)


### DiCE scores ###
load("F:/Dropbox/FSU/FSU_Fall_2017/Paper_1/Output/dice_ud.Rdata")

# already ud# 

dice<- dice_ud %>%
        dplyr::rename("A_ccode" = A_ccode, "B_ccode" = B_ccode, "year"=year) %>%
        mutate(A_ccode = makeCCode(A_ccode), B_ccode = makeCCode(B_ccode),
               A_cyear = paste(A_ccode, year, sep = "_"),
               B_cyear = paste(B_ccode, year, sep = "_"),
               A_ccode = fixCYear(A_cyear)$fixedCCode,
               B_ccode = fixCYear(B_cyear)$fixedCCode,
               A_cyear = fixCYear(A_cyear)$fixedCYear,
               B_cyear = fixCYear(B_cyear)$fixedCYear) %>%
        filter(!is.na(A_ccode) & !is.na(B_ccode)) %>%
        mutate(udy = paste(A_ccode, B_ccode, year, sep = "_")) %>%
        dplyr::select(udy, A_Costs, B_Costs) 

add <- left_join(add, dice, by = "udy")
rm(dice)

        
        
#### ICOW territorial ####
icow <- as.tbl(fread("ICOWprovyr101.csv", showProgress = F)) %>%
  dplyr::select(chal, tgt, year) %>% 
  filter(year >= min(ud$year)) %>% 
  mutate(chal = makeCCode(chal), tgt = makeCCode(tgt),
         ch_cyear = paste(chal, year, sep = "_"),
         tg_cyear = paste(tgt, year, sep = "_"),
         chal = fixCYear(ch_cyear)$fixedCCode,
         tgt = fixCYear(tg_cyear)$fixedCCode,
         ch_cyear = fixCYear(ch_cyear)$fixedCYear,
         tg_cyear = fixCYear(ch_cyear)$fixedCYear) %>% 
  filter(!is.na(chal) & !is.na(tgt)) %>% 
  mutate(ch_tg_year = paste(chal, tgt, year, sep = "_"),
         tg_ch_year = paste(tgt, chal, year, sep = "_"))

add <- add %>% 
  mutate(A_terrChallenge = ifelse(udy %in% icow$ch_tg_year, 1, 0),
         B_terrChallenge = ifelse(udy %in% icow$tg_ch_year, 1, 0))
rm(icow)

### issue is that we dropped years <= 2001 for this.
add <- filter(add, year <= 2001)
ud <- filter(ud, year <= 2001)

#### capital to capital distance ####
dis <- as.tbl(fread("capdist.csv")) %>%
  dplyr::select(-ida, -midist, -idb) %>% 
  filter(numa < numb) %>%
  dplyr::rename("A_ccode" = numa,
         "B_ccode" = numb,
         "ud_distance" = kmdist) %>%
  mutate(A_ccode = makeCCode(A_ccode),
         B_ccode = makeCCode(B_ccode),
         ud = paste(A_ccode, B_ccode, sep = "_")) %>%
  dplyr::select(-A_ccode, -B_ccode)

### not all uds in dis are unique.  problematic?
check <- dis %>%
  group_by(ud) %>%
  summarise(num = n()) %>%
  filter(num > 1) %>%
  separate(ud, c("A_ccode", "B_ccode"))

### 345 (yugoslavia) is bad.  how many responsible for?
#nrow(filter(check, A_ccode == 345 | B_ccode == 345)) / nrow(check)
### all of them.

### let's look at yugoslavia in the original lat-long data

lat <- as.tbl(fread("latlong.csv", header = F)) %>%
  rename("ccode" = V1,
         "scode" = V2,
         "latDeg" = V3,
         "latMin" = V4,
         "lonDeg" = V5,
         "lonMin" = V6) %>%
  mutate(ccode = makeCCode(ccode))

#filter(lat, ccode == 345)
### it's the same capital, so we can drop one of the bad ones.
dis <- dis %>%
  distinct(ud, .keep_all = TRUE)
rm(check)

add <- left_join(add, dis, by = "ud")

### missingness?
bad <- add %>%
  filter(is.na(ud_distance)) %>% 
  select(ud) %>%
  distinct() %>%
  mutate(A_latDeg = NA,
         A_latMin = NA,
         A_lonDeg = NA,
         A_lonMin = NA,
         B_latDeg = NA,
         B_latMin = NA,
         B_lonDeg = NA,
         B_lonMin = NA)

scodes <- add %>% 
  mutate(ud = paste(A_ccode, B_ccode, sep = "_")) %>%
  select(ud, A_stateabb, B_stateabb, year) %>%
  separate(ud, c("A_ccode", "B_ccode"))

bad <- bad %>% 
  separate(ud, c("A_ccode", "B_ccode"))

### some of these are stuff like yugoslavia problems.
for(i in 1:nrow(bad)){
  
  forA <- filter(lat, ccode == bad$A_ccode[i])
  forB <- filter(lat, ccode == bad$B_ccode[i])
  
  if(nrow(forA) > 0){
    
    bad$A_latDeg[i] <- forA$latDeg[1]
    bad$A_latMin[i] <- forA$latMin[1]
    bad$A_lonDeg[i] <- forA$lonDeg[1]
    bad$A_lonMin[i] <- forA$lonMin[1]
    
  }
  
  if(nrow(forB) > 0){
    
    bad$B_latDeg[i] <- forB$latDeg
    bad$B_latMin[i] <- forB$latMin
    bad$B_lonDeg[i] <- forB$lonDeg
    bad$B_lonMin[i] <- forB$lonMin
    
  }
  
}

rawLat <- as.tbl(fread("latlong.csv", header = F)) %>% 
  rename("ccode" = V1,
         "stateabb" = V2,
         "latDeg" = V3,
         "latMin" = V4,
         "lonDeg" = V5,
         "lonMin" = V6)

### make a list of all bad ccodes
problems <- bad %>%
  filter(is.na(A_latDeg)) %>% 
  select(A_ccode) %>%
  distinct()
moProblems <- bad %>%
  filter(is.na(B_latDeg)) %>%
  select(B_ccode) %>%
  distinct()
problems <- union(problems$A_ccode, moProblems$B_ccode)

#filter(scodes, A_ccode == problems[5] | B_ccode == problems[5])
### 245 is Bavaria (exits 1871) capital is Munich 48°08′N 11°34′E
### 267 is Baden (exits 1871) capital is Karlsruhe 49°00′33″N 8°24′14″E
### 271 is Wuerttemberg (exits 1871) capital is Stuttgart 48°47′N 9°11′E
### 364 is earlier USSR       (should be 365)
### 529 is Ethiopia           (should be 530)
### 769 is Pakistan           (Karachi was the capital early, but Islamabad
###                            since 60s. we will just use 770, Islamabad)
### 818 is Vietnam            (use the 817 capital)

### drop the german cases -- not in any other data, either
add <- filter(add, !(A_ccode %in% c("245", "267", "271")) & 
                       !(B_ccode %in% c("245", "267", "271")))
ud <- filter(ud, !(A_ccode %in% c("245", "267", "271")) & 
                      !(B_ccode %in% c("245", "267", "271")))

fixer <- function(ccode){
  
  if(ccode == "347") "345"
  else if(ccode == "364") "365"
  else if(ccode == "529") "530"
  else if(ccode == "769") "770"
  else if(ccode == "818") "817"
  else ccode
  
}

rm(forA, forB, lat, moProblems, scodes, rawLat, i, bad)

dis <- dis %>% 
  separate(ud, c("A_ccode", "B_ccode")) %>%
  mutate(ud = paste(A_ccode, B_ccode, sep = "_"))

for(i in which(is.na(add$ud_distance))){
  
  toGet <- add[i,] %>% 
    select(A_ccode, B_ccode)
  
  if(toGet$A_ccode %in% problems) toGet$A_ccode <- fixer(toGet$A_ccode)
  if(toGet$B_ccode %in% problems) toGet$B_ccode <- fixer(toGet$B_ccode)
  
  toGet <- toGet %>% 
    mutate(ud = paste(A_ccode, B_ccode, sep = "_"))
  solution <- filter(dis, ud == toGet$ud)
  if(nrow(solution) > 0) add$ud_distance[i] <- solution$ud_distance
  
}
rm(i, toGet, solution)

### kinshasa to brazzaville is 8km, not 0
# phil note: the attention to detail here is nuts
add$ud_distance[add$ud_distance == 0] <- 8
rm(dis, problems)

### bosnia and yugoslavia have the same capital when they co-exist
add$ud_distance[add$ud == "346_347"] <- 1

#### contiguity data ####
cont <- as.tbl(fread("contdir.csv")) %>%
  select(-dyad, -notes, -version) %>%
  mutate(begin = round(begin/100),
         end = round(end/100)) %>% 
  filter(begin <= 1999 & end >= 1940) %>%
  rename("A_ccode" = statelno,
         "B_ccode" = statehno) %>% 
  mutate(A_ccode = makeCCode(A_ccode),
         B_ccode = makeCCode(B_ccode)) %>%
  select(-statelab, -statehab)

fixCont <- function(x){
  
  if(x == 1) "direct" 
  else if(x == 2) "water012" 
  else if(x == 3) "water024"
  else if(x == 4) "water150"
  else if(x == 5) "water400" 
  else NA
  
}

foo <- NULL

for(i in 1:nrow(cont)){
  
  years <- intersect(cont$begin[i]:cont$end[i], min(add$year):max(add$year))
  bar <- data.frame(A_ccode = cont$A_ccode[i],
                    B_ccode = cont$B_ccode[i],
                    cont_direct = fixCont(cont$conttype[i]),
                    year = years)
  bar <- bar %>%
    mutate(A_ccode = fixCYear(makeCountryYear(A_ccode, year))$fixedCCode,
           B_ccode = fixCYear(makeCountryYear(B_ccode, year))$fixedCCode,
           udy = paste(A_ccode, B_ccode, year, sep = "_"))
  if(!is.na(bar$A_ccode[1]) & !is.na(bar$B_ccode[1])) {
    
    bar <- select(bar, -A_ccode, -B_ccode, -year)
    foo <- rbind(foo, bar)
    
  }
  
}
cont <- as.tbl(foo)
rm(foo, bar, i, years)

cont$cont_direct <- factor(cont$cont_direct, 
                           levels = c("none", "water400", "water150",
                                      "water024", "water012", "direct"))

### some duplicates -- take the maximum
maxFact <- function(x) levels(x)[max(as.numeric(x))]

cont <- cont %>%
  group_by(udy) %>%
  summarise(num = n(),
            min_cont = maxFact(cont_direct)) %>%
  distinct(udy, .keep_all = TRUE) %>%
  rename("cont_direct" = min_cont) %>%
  select(-num)
cont$cont_direct <- factor(cont$cont_direct, 
                           levels = c("none", "water400", "water150",
                                      "water024", "water012", "direct"))
add <- left_join(add, cont, by = "udy")
add$cont_direct[is.na(add$cont_direct)] <- "none"
add <- add %>% 
  rename("ud_contiguity" = cont_direct)
rm(cont)

add$ud_contiguity[add$ud == "346_347"] <- "direct"

#### peace years ####
timeSince <- function(t, kill){ # t is assumed to be sorted
  
  foo <- data.frame(t = t, kill = kill)
  foo$x <- NA
  foo$x[1] <- ifelse(foo$kill[1] == 1, 0, 1)
  if(nrow(foo) > 1){
    for(i in 2:nrow(foo)){
      if(foo$t[i] != (foo$t[i-1] + 1)) foo$x[i] <- 0
      else if(foo$kill[i] == 1) foo$x[i] <- 0
      else foo$x[i] <- foo$x[i-1] + 1
    }
    foo$x
  }else foo$x[1]

}

add <- add %>% 
  arrange(year) %>% 
  group_by(ud) %>% 
  mutate(ud_peaceYears = timeSince(t = year, kill = ud_ongoingMID)) %>% 
  ungroup()

#### merging ####
ud <- left_join(ud, 
                select(add, -A_ccode, -B_ccode, -A_cyear, -B_cyear, -year,
                       -ud, -A_stateabb, -B_stateabb),
                by = "udy")
fwrite(ud, file = "UD/data-UDmaster.csv", row.names = FALSE)

#### do it for all the data ####
for(i in 1:(length(list.files("UD"))-1)){
  
  udi <- as.tbl(fread(paste0(sprintf("UD/data-UD-%02d", i), ".csv"))) %>% 
    rename("A_totalImports" = A_imports,
           "B_totalImports" = B_imports,
           "A_totalExports" = A_exports,
           "B_totalExports" = B_exports) %>% 
    filter(udy %in% ud$udy)
  udi <- left_join(udi, 
                   select(add, -A_ccode, -B_ccode, -A_cyear, -B_cyear, -year,
                          -ud, -A_stateabb, -B_stateabb),
                   by = "udy")
  fwrite(udi, file = paste0(sprintf("UD/data-UD-%02d", i), ".csv"))

}

#### bye ####
cat("04:  UD vars added, and results added back to UD directory.\n")
rm(list = setdiff(ls(), lsf.str()))
gc(verbose = FALSE)