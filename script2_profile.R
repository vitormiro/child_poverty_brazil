
## Monetary Child Poverty - Brazil.
## Poverty profile.
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

# Selected variables
svars <- c('Ano', 'UF', 'Estrato', 'UPA', 'V1030', 'V1031', 'V1032', 'V1022', 
           'V1008', 'V1014', 'V2005', 'VD2002', 'V2007', 'V2009', 'V2010', 
           'VD3004', 'VD4001', 'VD4002', 'VD4003', 
           'V5002A', 'V5003A', 'V5004A', 
           'VD4004A', 'VD4005', 'VD4009', 'VD4010', 
           'VD4048', 'VD4019')

#UPA - primary sampling unit (UPA - unidade primária de amostragem)
#V1008 - Household number
#V1014 - panel
#V1032 - sample weights (post-stratification by population projection)
#V1022 - urban or rural area
#V2001 - number of individuals/ household
#V2005 - status inside the household
#V2009 - age
#VD5008 - Household income per capita

###--------------------------------------------------------------------------###

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

# per capita household income
pchi <- svymean(~pchi, svypnadc, na.rm = T)
pchi

# poverty headcount index
fgt0 <- svymean(~poverty, svypnadc, na.rm = T)
fgt0

# Child Poverty
fgt0_child <- svymean(~poverty, subset(svypnadc, child ==1), na.rm = T)
fgt0_child

fgt0_child6 <- svymean(~poverty, subset(svypnadc, child6 ==1), na.rm = T)
fgt0_child6

