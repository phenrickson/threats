####
##
##  name:  01-makeCY.R
##  date:  2017-01-20
##  what:  this creates the underlying country-year data
##
####

setwd("F:/Dropbox/FSU/FSU_Fall_2017/Paper_2/Measure/")

cat("01:  now making country-year data.\n")

#### packages ####
library(dplyr)
library(tidyr)
library(data.table)
library(foreach)

### handy function to clean country codes
source("fn-ccodeCleaner.R")

#### set the data up ####

## COW universe
## http://www.correlatesofwar.org/data-sets/state-system-membership

dat <- as.tbl(fread("system2011.csv")) %>%
  select(-version) %>% 
  filter(year >= 1870 & year <= 2001) %>%
  mutate(ccode = makeCCode(ccode),
         cyear = paste(ccode, year, sep = "_"),
         ccode = fixCYear(cyear)$fixedCCode,
         cyear = fixCYear(cyear)$fixedCYear) %>%
  select(cyear, ccode, year, stateabb) %>% 
  filter(!is.na(ccode))

#### read in polity ####
## http://www.systemicpeace.org/inscrdata.html

pol <- as.tbl(fread("p4v2015.csv")) %>%
  filter(year >= min(dat$year) & year <= max(dat$year)) %>%
  select(ccode, scode, year, polity, polity2, country) %>%
  mutate(ccode = makeCCode(ccode),
         cyear = paste(ccode, year, sep = "_"),
         ccode = fixCYear(cyear)$fixedCCode,
         cyear = fixCYear(cyear)$fixedCYear) %>%
  select(cyear, ccode, year, polity, polity2, country) %>% 
  filter(!is.na(ccode)) %>% 
  distinct(cyear, .keep_all = TRUE)

bar <- pol
pol <- select(pol, -country)

### find which COW CYears aren't in polity
filter(dat, !(cyear %in% unique(pol$cyear))) %>%
  group_by(ccode) %>%
  summarise(stateabb = first(stateabb),
            minBadYear = min(year),
            maxBadYear = max(year)) %>%
  as.data.frame() %>%
  ungroup()

## 300 (AUH).  so, COW has AUS from 1919-present and HUN from 1918-present, 
## with AUH from 1816-1918 (all missing)
## POL has Austria 1816-present and Hungary 1867-present.  
## Austria is -10, 1816-1847; -6 1848-1860; -4 1861-1917; and -88 in 1918
## Hungary is -4, 1867-1917; and -88, 1918.
## so, there is agreement between austria and hungary 1861-1917.
foo <- data.frame(ccode = "300",
                  year = 1816:1847,
                  polity = -10, polity2 = -10)
foo <- rbind.data.frame(foo,
                        data.frame(ccode = "300",
                                   year = 1848:1860,
                                   polity = -6, polity2 = -6))
foo <- rbind.data.frame(foo,
                        data.frame(ccode = "300",
                                   year = 1861:1917,
                                   polity = -4, polity2 = -4))
foo <- rbind.data.frame(foo,
                        data.frame(ccode = "300",
                                   year = 1918,
                                   polity = -88, polity2 = -4))
foo$cyear <- paste(foo$ccode, foo$year, sep = "_")
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 345 and 347 (YUG).  always a joy.  bad 1878-1920 and 2007-2011.
## polity has 347 going from 1991-2002 and 345 going from 1921-1990
## but from 1868 onward, there was Kingdom of Croatia-Slavonia not in polity
## however, serbia (kingdom of, principality of) goes back in data and seems
## to be the only long-term autonomous state in region at the time
foo <- filter(bar, country == "Serbia" & year %in% 1878:1920) %>%
  select(-country) %>%
  mutate(ccode = "345",
         cyear = paste(ccode, year, sep = "_"))
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 365 (RUS) from 1923-1939.  polity gives USSR ccode 364
foo <- filter(bar, ccode == "364" & year %in% 1923:1939) %>%
  select(-country) %>%
  mutate(ccode = "365",
         cyear = paste(ccode, year, sep = "_"))
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 367 (LAT) -- just enters data at a different year
foo <- data.frame(cyear = c("367_1918", "367_1919"),
                  ccode = "367",
                  year = 1918:1919,
                  polity = 7, polity2 = 7)
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 452 (GHA) 1957-1959 -- just enters at different time
foo <- data.frame(cyear = c("452_1957", "452_1958", "452_1959"),
                  ccode = "452",
                  year = 1957:1959,
                  polity = -8, polity2 = -8)
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 552 (ZIM), 1965-1969 -- just enters at different time
foo <- data.frame(ccode = "552",
                  year = 1965:1969,
                  polity = 4, polity2 = 4)
foo <- foo %>%
  mutate(cyear = paste(ccode, year, sep = "_")) %>%
  select(cyear, ccode, year, polity, polity2)
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 616 (TUN) 1825-1958.  Tunisia doesn't gain independence from france until
## 1958.
dat <- filter(dat, !(ccode == "616" & year < 1959))

## 651 (EGY) 1855-1882.  Egypt isn't made an autonomous khedivate until 
## 1867, though Ottoman rule was still only nominal.  British ocupation
## begins in 1882.  Ottomans influenced government, but Egypt could be 
## attacked separately.  call it a -10, a la Ottomans
foo <- filter(bar, country == "Turkey" & year %in% 1855:1882) %>%
  mutate(ccode = "651",
         cyear = paste(ccode, year, sep = "_")) %>%
  select(-country)
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 690 (KUW) 1961-2.  enters at different year
foo <- data.frame(cyear = c("690_1961", "690_1962"),
                  ccode = "690",
                  year = 1961:1962,
                  polity = -8, polity2 = -8)
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 712 (MON) 1921-1923.  enters at different year
foo <- data.frame(cyear = c("712_1921", "712_1922", "712_1923"),
                  ccode = "712",
                  year = 1921:1923,
                  polity = -7, polity2 = -7)
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 750 (IND), 1947-1949.  enters at different year
foo <- data.frame(cyear = c("750_1947", "750_1948", "750_1949"),
                  ccode = "750",
                  year = 1947:1949,
                  polity = 9, polity2 = 9)
pol <- rbind.data.frame(pol, foo)
rm(foo)

## 812 (LAO) 1953
pol <- rbind.data.frame(pol,
                        data.frame(cyear = "812_1953",
                                   ccode = "812",
                                   year = 1953,
                                   polity = -88, polity2 = -2))

## 817 (RVN) 1954
pol <- rbind.data.frame(pol,
                        data.frame(cyear = "817_1954",
                                   ccode = "817",
                                   year = 1954,
                                   polity = -3, polity2 = -3))

## 947 (TUV) 2000-2011.  Tuvalu is never in the polity data.  drop it
dat <- filter(dat, ccode != "947")

### merge!
dat <- suppressWarnings(
  left_join(dat, select(pol, cyear, polity, polity2), by = "cyear"))
rm(pol, bar)

### make some new variables
dat$occupied <- ifelse(dat$polity == -66, 1, 0)
dat <- dat %>% 
  group_by(ccode) %>% 
  arrange(year) %>%
  mutate(instability = ifelse(polity %in% c(-77, -88) | 
                                abs(polity - lag(polity)) >= 3,
                              1, 0)) %>%
  ungroup()
dat$instability[is.na(dat$instability)] <- 0
for(i in 1:nrow(dat)){
  
  if(dat$polity[i] %in% c(-66, -77, -88)) dat$polity[i] <- dat$polity2[i]
  
}

dat <- select(dat, -polity2)

#### major power status ####
rm(i)
maj <- as.tbl(fread("majors2011.csv")) %>%
  mutate(ccode = makeCCode(ccode))
powYears <- foreach(i = 1:nrow(maj), .combine = c) %do% {
  
  fixCYear(paste(maj$ccode[i], 
                 maj$styear[i]:maj$endyear[i], 
                 sep = "_"))$fixedCYear

}
rm(i, maj)
dat$majPow <- 0
dat$majPow[dat$cyear %in% powYears] <- 1
rm(powYears)

#### add in the NMC data ####
## http://www.correlatesofwar.org/data-sets/national-material-capabilities
nmc <- as.tbl(fread("NMC_v4_0.csv")) %>% 
  filter(year %in% min(dat$year):max(dat$year)) %>%
  select(-version) %>%
  mutate(ccode = makeCCode(ccode),
         cyear = paste(ccode, year, sep = "_"),
         ccode = fixCYear(cyear)$fixedCCode,
         cyear = fixCYear(cyear)$fixedCYear) %>%
  select(cyear, ccode, year, stateabb, everything()) %>%
  mutate_each(funs(ifelse(. == -9, NA, .)),
              irst, milex, milper, pec, tpop, upop)

### dat goes up to 2011, but nmc only goes to 2007.  material capabilities
### mater a lot here.  drop 2008-2011
dat <- filter(dat, year <= 2007)

### find which COW CYears aren't in nmc
#filter(dat, !(cyear %in% unique(nmc$cyear))) %>%
#  group_by(ccode) %>%
#  summarise(stateabb = first(stateabb),
#            minBadYear = min(year),
#            maxBadYear = max(year)) %>%
#  as.data.frame() %>%
#  ungroup()   ### we're good!

dat <- suppressWarnings(
  left_join(dat, select(nmc, -ccode, -year, -stateabb), by = "cyear"))
rm(nmc)

#### add in civil war data ####
# http://www.correlatesofwar.org/data-sets/COW-war

civ <- as.tbl(fread("Intra-StateWarData_v4.csv")) %>%
  select(WarName, CcodeA, CcodeB, StartYear1, EndYear1, 
         StartYear2, EndYear2, WarType) %>%
  mutate(EndYear1 = ifelse(is.na(EndYear1) | EndYear1 == -7, 
                           max(dat$year), 
                           EndYear1))

### couple of weird NAs.
civ$CcodeA[civ$WarName == "Third Lebanese"] <- 660
civ$CcodeA[civ$WarName == "Ethiopian Anyuaa-Nuer"] <- 529
civ$CcodeA[civ$WarName == "Second Nigeria Christian-Muslim"] <- 475
civ$CcodeB[civ$WarName == "Third Laotian"] <- -8

## and then weird cases with both -8
huh <- filter(civ, CcodeA == -8 & CcodeB == -8)
foo <- foreach(i = 1:nrow(huh), .combine = c) %do% nrow(filter(civ, WarName == huh$WarName[i]))
rm(i)
### all are one-liners.
rm(foo)

## Sidon-Damascus was in Ottoman empire
civ$CcodeA[civ$WarName == "Sidon-Damascus"] <- 640
## Egypt-Mehdi was in Egypt before it enters data, so Ottoman
civ$CcodeA[civ$WarName == "Egypt-Mehdi"] <- 640
## Egyptian Taka Expedition, same thing
civ$CcodeA[civ$WarName == "Egyptian Taka Expedition"] <- 640
## Egypt- Palestinian Anti-Conscription Revolt
civ$CcodeA[civ$WarName == "Egypt- Palestinian Anti-Conscription Revolt"] <- 640
## Druze Rebellion against autonomous Egpytians still coded as part of 
## ottoman
civ$CcodeA[civ$WarName == "Druze Rebellion"] <- 640
## same with Lebanon Insurgency 
civ$CcodeA[civ$WarName == "Lebanon Insurgency"] <- 640
## same with First Maronite-Druze
civ$CcodeA[civ$WarName == "First Maronite-Druze"] <- 640
## Mayan Caste War is in Yucatan, Mexico
civ$CcodeA[civ$WarName == "Mayan Caste War, Phase 1"] <- 70
## Maronites and Druze still in Ottoman empire
civ$CcodeA[civ$WarName == "Second Maronite-Druze"] <- 640
## Sioux-Minnesota (Dakota War)
civ$CcodeA[civ$WarName == "Sioux-Minnesota"] <- 2
## Western Ukranian -- this is when Ukraine is still part of Russian Republic 
## but this conflict was taking part in the Polish-influenced places with
## revolts against ethnic cleansing
civ$CcodeA[civ$WarName == "Western Ukrainian"] <- 290
## Sparticist Rising -- this is in germany, but it ends in 1919, not 2007 
## per the COW data
civ$CcodeA[civ$WarName == "Sparticist Rising"] <- 255
civ$EndYear1[civ$WarName == "Sparticist Rising"] <- 1919
## First Chinese Warlord era
civ$CcodeA[civ$WarName == "First Chinese Warlord"] <- 710
## Italian Fascist
civ$CcodeA[civ$WarName == "Italian Fascist"] <- 325
## Second Chinese Warlord era
civ$CcodeA[civ$WarName == "Second Chinese Warlord"] <- 710
## Third Chinese Warlord era
civ$CcodeA[civ$WarName == "Third Chinese Warlord"] <- 710
## Cultural Revolution
civ$CcodeA[civ$WarName == "Cultural Revolution Phase 1"] <- 710
## Eritrean Split -- fought in what is essentially ethiopia
civ$CcodeA[civ$WarName == "Eritrean Split"] <- 530
## Inkatha-ANC -- south africa
civ$CcodeA[civ$WarName == "Inkatha-ANC"] <- 560
## SPLA -- Sudan
civ$CcodeA[civ$WarName == "The SPLA Division (Dinka-Nuer) War"] <- 625
## Jukun-Tiv -- Nigeria
civ$CcodeA[civ$WarName == "Jukun-Tiv War"] <- 475
## Iraqi Kurd Internecine
civ$CcodeA[civ$WarName == "Iraqi Kurd Internecine"] <- 645
## Moluccas Sectarian -- Indonesia
civ$CcodeA[civ$WarName == "Moluccas Sectarian"] <- 850
## First Nigeria Christian-Muslim
civ$CcodeA[civ$WarName == "First Nigeria Christian-Muslim"] <- 475
rm(huh)

## is the first row in any given conflict where it was fought?  
#civ %>% 
#  group_by(WarName) %>%
#  summarise(CcodeA = first(CcodeA),
#            CcodeB = first(CcodeB)) %>%
#  ungroup() %>% 
#  as.data.frame()
## of course not.

### ok.  so, the one-liners are easy.
lengths <- civ %>% 
  group_by(WarName) %>%
  summarise(numEntries = n())

## majority are one-liners
ones <- filter(lengths, numEntries == 1)
ones <- filter(civ, WarName %in% ones$WarName)
more <- filter(lengths, numEntries > 1)
more <- filter(civ, WarName %in% more$WarName)

bad <- more %>% 
  group_by(WarName) %>% 
  summarise(bad = sum(CcodeA == -8) != n() - 1)
toOnes <- filter(bad, bad == FALSE)
foo <- foreach(i = 1:nrow(toOnes), .combine = rbind.data.frame) %do% {
  
 filter(more, WarName == toOnes$WarName[i] & CcodeA != -8)
  
}
ones <- rbind.data.frame(ones, foo)
rm(foo, toOnes, i)
toMore <- filter(bad, bad == TRUE)
more <- filter(more, WarName %in% toMore$WarName)
rm(toMore, bad, lengths)

brute <- data.frame(WarName = unique(more$WarName), locale = NA)
brute$locale[brute$WarName == "First Two Sicilies"] <- 329
brute$locale[brute$WarName == "Sardinian Revolt"] <- 325
brute$locale[brute$WarName == "First Carlist War"] <- 230
brute$locale[brute$WarName == "Second Syrian, Phase 2"] <- 640
brute$locale[brute$WarName == "Hungarian"] <- 300
brute$locale[brute$WarName == "Taiping Rebellion, Phase 2"] <- 710
brute$locale[brute$WarName == "Xinjiang Muslim Revolt"] <- 710
brute$locale[brute$WarName == "Tonghak Rebellion"] <- 730
brute$locale[brute$WarName == "Overthrow of Abd el-Aziz"] <- 600
brute$locale[brute$WarName == "Iranian Constitution War"] <- 600
brute$locale[brute$WarName == "Paraguay"] <- 150
brute$locale[brute$WarName == "Fourth Mexican"] <- 70
brute$locale[brute$WarName == "Finnish Civil War"] <- 375
brute$locale[brute$WarName == "Greek Civil War"] <- 350
brute$locale[brute$WarName == "Polish Ukrainians"] <- 365
brute$locale[brute$WarName == "First Lebanese"] <- 660
brute$locale[brute$WarName == "Vietnam Phase 1"] <- 817
brute$locale[brute$WarName == "North Yemen"] <- 678
brute$locale[brute$WarName == "Second Laotian Phase 1"] <- 812
brute$locale[brute$WarName == "Third DRC (Simba) Rebellion"] <- 490
brute$locale[brute$WarName == "Dominican Republic"] <- 42
brute$locale[brute$WarName == "First Guatemala"] <- 90
brute$locale[brute$WarName == "First Chad (FROLINAT) Rebellion"] <- 483
brute$locale[brute$WarName == "Khmer Rouge"] <- 811
brute$locale[brute$WarName == "Dhofar Rebellion Phase 2"] <- 698
brute$locale[brute$WarName == "Eritrean War"] <- 530
brute$locale[brute$WarName == "Second Lebanese"] <- 660
brute$locale[brute$WarName == "Third Laotian"] <- 812
brute$locale[brute$WarName == "Angolan Control"] <- 540
brute$locale[brute$WarName == "Second Ogaden Phase 1"] <- 530
brute$locale[brute$WarName == "Third Lebanese"] <- 660
brute$locale[brute$WarName == "Second Ogaden Phase 3"] <- 530
brute$locale[brute$WarName == "Fourth DRC (Shaba)"] <- 490
brute$locale[brute$WarName == "First Afghan Mujahideen Uprising"] <- 700
brute$locale[brute$WarName == "Mozambique"] <- 541
brute$locale[brute$WarName == "Second Chad (Habre Revolt)"] <- 483
brute$locale[brute$WarName == "Fourth Lebanese Civil"] <- 660
brute$locale[brute$WarName == "First Sri Lanka Tamil"] <- 780
brute$locale[brute$WarName == "First Liberia"] <- 450
brute$locale[brute$WarName == "Second Somalia"] <- 520
brute$locale[brute$WarName == "Tajikistan"] <- 702
brute$locale[brute$WarName == "Bosnian-Serb Rebellion"] <- 346
brute$locale[brute$WarName == "Second Liberia"] <- 450
brute$locale[brute$WarName == "Guinea-Bissau Military"] <- 404
brute$locale[brute$WarName == "Africa's World War"] <- 490
brute$locale[brute$WarName == "Cote d'Ivoire Military"] <- 437
brute$locale[brute$WarName == "Waziristan"] <- 770
brute$locale[brute$WarName == "Third Somalia"] <- 520

brutes <- foreach(i = 1:nrow(brute), .combine = rbind.data.frame) %do% {
  
  filter(more, WarName == brute$WarName[i] & CcodeA == brute$locale[i])
  
}
rm(i)

civ <- rbind.data.frame(ones, brutes) %>%
  distinct(WarName, .keep_all = TRUE)
rm(brute, brutes, more, ones)

civYears <- foreach(i = 1:nrow(civ), .combine = c) %do% {
  
  cc <- makeCCode(civ$CcodeA[i])
  cy <- fixCYear(paste(cc, 
                       civ$StartYear1[i]:civ$EndYear1[i], 
                       sep = "_"))$fixedCYear
  if(civ$StartYear2[i] != -8){
    
    add <- fixCYear(paste(cc,
                          civ$StartYear2[i]:civ$EndYear2[i],
                          sep = "_"))$fixedCYear
    cy <- c(cy, add)
    
  }
  
  cy
  
}
rm(civ, add, cc, cy, i)

dat$ongoingCivil <- 0
dat$ongoingCivil[dat$cyear %in% civYears] <- 1
rm(civYears)

#### get imports/exports ####

## NB:  cutting analysis down to 1870 onward 
dat <- filter(dat, year >= 1870 & year <= 2001)

trade <- as.tbl(fread("national_trade_3.0.csv", na.strings = "-9")) %>%
  mutate(ccode = makeCCode(ccode),
         cyear = paste(ccode, year, sep = "_"),
         ccode = fixCYear(cyear)$fixedCCode,
         cyear = fixCYear(cyear)$fixedCYear) %>%
  select(cyear, imports, exports)

dat <- suppressWarnings(
  left_join(dat, trade, by = "cyear"))
rm(trade)

#### get ongoing interstate war data ####
mids <- as.tbl(fread("MIDB_4.01.csv")) %>%
  mutate(ccode = makeCCode(ccode)) %>%
  filter(EndYear >= 1890 & StYear <= max(dat$year) & HostLev >= 4) %>%
  select(ccode, StYear, EndYear)
midYears <- foreach(i = 1:nrow(mids), .combine = c) %do% {
  
  if(mids$EndYear[i] > mids$StYear[i]){
    
    fixCYear(paste(mids$ccode[i],
                   mids$StYear[i]:(mids$EndYear[i]-1),
                   sep = "_"))$fixedCYear
    
  }else fixCYear(paste(mids$ccode[i], mids$StYear[i]-1, sep = "_"))$fixedCYear
  
}
midYears <- na.omit(midYears)
dat$ongoingMID <- 0
dat$ongoingMID[dat$cyear %in% midYears] <- 1

numMids <- data.frame(cyear = midYears) %>% 
  group_by(cyear) %>% 
  summarise(numMIDs = n())
dat <- suppressWarnings(
  left_join(dat, numMids, by = "cyear"))
dat$numMIDs[is.na(dat$numMIDs)] <- 0
rm(mids, numMids, i, midYears)

#### save it for later ####
write.csv(dat, file = "data-post-01.csv", row.names = FALSE)

#### bye ####
cat("01:  country-year data constructed.\n")
rm(list = setdiff(ls(), lsf.str()))
gc(verbose = FALSE)