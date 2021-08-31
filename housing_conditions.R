## Monetary Child Poverty - Brazil.
## Housing conditions
## Using Continuous National Household Sample Survey - 'PNAD Contínua'/IBGE
## Autor: Vitor Hugo Miro
## July/2021

###--------------------------------------------------------------------------###
###--------------------------------------------------------------------------###
# packages
library(PNADcIBGE)
library(tidyverse)
library(survey)

###--------------------------------------------------------------------------###

# 
svars <- c('Ano', 'UF', 'Estrato', 'UPA', 'V1030', 'V1031', 'V1032', 'V1022', 
           'V1008', 'V1014', 'V2005', 'VD2002', 'V2007', 'V2009', 'V2010', 
           'VD3004', 'VD4001', 'VD4002', 'VD4003', 
           'V5002A', 'V5003A', 'V5004A', 
           'VD4004A', 'VD4005', 'VD4009', 'VD4010', 
           'VD4048', 'VD4019', 'CO2e', 'CO2',
           'S01001', 'S01002', 'S01003', 'S01004', 'S01005', 'S01006',
           'S01007', 'S01007a', 'S01010', 'S01011A', 'S01012A',
           'S01017', )

# --------------------------------------------------------------------------###
# download PNADC microdata
pnadc <- get_pnadc(year = 2019,
                   vars = svars,
                   interview = 1,
                   design = TRUE,
                   labels = FALSE,
                   deflator = TRUE,
                   defyear = 2019)

# Object Class
class(pnadc)

# Structure
str(pnadc)

###--------------------------------------------------------------------------###
# srvyr brings parts of dplyr's syntax to survey analysis, 
# using the survey package.
library(srvyr)

svypnadc <- as_survey(pnadc)

class(svypnadc)


###--------------------------------------------------------------------------###
### individual characteristics
svypnadc <- svypnadc %>% 
    mutate(hh_id = as.numeric(paste(UPA,V1008,V1014, sep = "")),
           member = ifelse (VD2002 < 15, 1, 0),
           
           head = ifelse (VD2002== "01", 1, 0),
           spouse = ifelse (VD2002== "02", 1, 0),
           
           age = V2009,
           group_age = factor(case_when(V2009 %in% 0:6 ~ "0-6",
                                        V2009 %in% 7:14 ~ "7-14",
                                        V2009 %in% 15:24 ~ "15-24",
                                        V2009 %in% 25:34 ~ "25-34",
                                        V2009 %in% 35:49 ~ "35-49",
                                        V2009 %in% 50:64 ~ "50-64",
                                        V2009 >= 65 ~ "65+")),
           child = ifelse (V2009 <=14, 1, 0),
           child6 = ifelse (V2009 <=6, 1, 0),
           adult = ifelse (V2009 >14, 1, 0),
           
           gender = factor(case_when(V2007==1 ~ "male",
                                     V2007==2 ~ "female")),
           
           color = factor(case_when(V2010==1 | V2010==3 ~ "white",
                                    V2010==2 | V2010==4 | V2010==5 ~ "black/brown")),
           
           school = factor(case_when(VD3004==1 ~ "uneducated",
                                     VD3004==2 ~ "incomp elementary",
                                     VD3004==3 ~ "comp elementary",
                                     VD3004==4 ~ "incomp high",
                                     VD3004==5 ~ "comp high",
                                     VD3004==6 ~ "incomp college",
                                     VD3004==7 ~ "comp college")),
           
           region = factor(case_when(UF %in% 10:19 ~ "Norte",
                                     UF %in% 20:29 ~ "Nordeste",
                                     UF %in% 30:39 ~ "Sudeste",
                                     UF %in% 40:49 ~ "Sul",
                                     UF %in% 50:59 ~ "Centro Oeste")),
           
           area = factor(case_when(V1022==1 ~ "urban",
                                   V1022==2 ~ "rural")),
           
           ### Labor market
           workforce = factor(case_when(VD4001==1 ~ "workforce",
                                        VD4001==2 ~ "outside")),
           
           employed = factor(case_when(VD4002==1 ~ "employed",
                                       VD4002==2 ~ "unemployed")),
           
           occup_cat = factor(case_when(VD4009==01 ~ "private sector - formal",
                                        VD4009==02 ~ "private sector - informal",
                                        VD4009==03 ~ "domestic workers - formal",
                                        VD4009==04 ~ "domestic workers - informal",
                                        VD4009==05 ~ "public sector - formal",
                                        VD4009==06 ~ "public sector - informal",
                                        VD4009==07 ~ "statutory and military",
                                        VD4009==08 ~ "employer",
                                        VD4009==09 ~ "self-employed",
                                        VD4009==10 ~ "family")),
           
           retired = ifelse (V5004A==1 & !is.na(V5004A), 1, 0),
           
           ### income
           VD4048 = ifelse (is.na (VD4048), 0, VD4048),
           VD4019 = ifelse (is.na (VD4019), 0, VD4019),
           other_def = VD4048 * CO2e,
           labor_def = VD4019 * CO2,
           income = other_def + labor_def )


### household characteristics
svypnadc <- svypnadc %>% group_by(hh_id) %>% 
    mutate(hh_num = sum(member),
           child_num = sum(child),
           adults_num = sum(adult),
           # per capita household income - pchi
           hh_income = ifelse(member==1, sum(income), 0),
           pchi=hh_income/hh_num )


#---------------------------------------------------
# Create poverty and extreme poverty variables

## World Bank Poverty Lines (bm)
lepbm <- 151  # US$ 1,90/day ~ R$151/month
lpbm <- 436   # US$ 5,5/day ~ R$436/month

svypnadc <- svypnadc %>% 
    mutate(poverty = ifelse (pchi< lpbm, 1, 0),
           expoverty = ifelse (pchi< lepbm, 1, 0))

###--------------------------------------------------------------------------###
### Housing

svypnadc <- svypnadc %>% 
    mutate(hh_type = factor(case_when(S01001==1 ~ "house",
                                      S01001==2 ~ "apart",
                                      S01001==3 ~ "tenement")),
           watersource = factor(case_when(S01007==1 ~ "net",
                                          S01007==2 ~ "",
                                          S01007==3 ~ "",
                                          S01007==4 ~ "",
                                          S01007==5 ~ "",
                                          S01007==6 ~ "")),
           sewage = factor(case_when(S01012A==1 & V1022==1 ~ "adequate",
                                     S01012A==2 & V1022==1 ~ "adequate",
                                     S01012A>=3 & V1022==1 ~ "inadequate",
                                     S01012A<=3 & V1022==2 ~ "adequate",
                                     S01012A>=4 & V1022==2 ~ "inadequate")),
           walls = factor(case_when(S01002==1 | S01002==2 | S01002==4 ~ "adequate",
                                    S01002==3 | S01002>=5 ~ "inadequate")),
           home = factor(case_when(S01017==1 ~ "ownpaid",
                                   S01017==2 ~ "ownpaying",
                                   S01017==3 ~ "rent",
                                   S01017%in% 4:6 ~ "ceded",
                                   S01017==7 ~ "other")),
           region = factor(case_when(UF %in% 10:19 ~ "Norte",
                                     UF %in% 20:29 ~ "Nordeste",
                                     UF %in% 30:39 ~ "Sudeste",
                                     UF %in% 40:49 ~ "Sul",
                                     UF %in% 50:59 ~ "Centro Oeste")),
           
           roof = S01003,
           floor = S01004,
           rooms = S01005,
           adwalls = ifelse(walls== "adequate", 1, 0),
           adwater = ifelse((S01007==1 | S01007==2) & S01010==1, 1, 0),
           adsewage = ifelse(sewage== "adequate", 1, 0),
           ownhome = ifelse(S01017==1 | S01017==2, 1, 0),
           pa_mobile = S01021/adults_num,
           refrigerator = ifelse(S01023==1, 1, 0),
           wash_mach = ifelse(S01024==1, 1, 0),
           tv = ifelse(S01025%in%1:3, 1, 0),
           computer = ifelse(S01028==1, 1, 0),
           internet = ifelse(S01029==1, 1, 0),
           car = ifelse(S010311==1, 1, 0),
           motorcycle = ifelse(S010312==1, 1, 0) )           
           

###--------------------------------------------------------------------------###