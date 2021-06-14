
## Monetary Child Poverty - Brazil.
## Using Continuous National Household Sample Survey - 'PNAD Contínua'/IBGE
## Autor: Vitor Hugo Miro
## June/2021

###--------------------------------------------------------------------------###
# 
rm(list = ls())
getwd()
options(scipen = 999)

###--------------------------------------------------------------------------###
# packages
library(PNADcIBGE)
library(tidyverse)
library(survey)

###--------------------------------------------------------------------------###

# 
selected_vars <- c('Ano', 'UF', 'Estrato', 'UPA', 'V1008', 'V1014', 'V1032', 
                   'V1022', 'V2001', 'V2005', 'V2007', 'V2009', 
                   'VD4001', 'VD4002', 'VD4016', 'VD4017', 
                   'VD4019', 'VD4020', 'VD4022', 'VD4048', 'VD5008')


#UPA - primary sampling unit (UPA - unidade primária de amostragem)
#V1008 - Household number
#V1014 - panel
#V1032 - sample weights (post-stratification by population projection)
#V1022 - urban or rural area
#V2001 - number of individuals/ household
#V2005 - 
#V2009 - age
#VD5008 - Household income per capita

###--------------------------------------------------------------------------###

# download PNADC microdata
pnadc <- get_pnadc(year = 2019,
                   vars = selected_vars,
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

# 'convey' library estimates measures of poverty, inequality, and wellbeing
library(convey)

svypnadc <- convey_prep(svypnadc)

class(svypnadc)

###--------------------------------------------------------------------------###
## World Bank's poverty lines
line <- 436   # US$ 5,5/day ~ R$436/month
linex <- 151  # US$ 1,90/day ~ R$151/month

# Foster, Greer, and Thorbecke (1984) indicators to measure poverty

# Poverty headcount ratio - FGT(0)
svyfgt(~VD5008, svypnadc, g=0, abs_thresh = line, na.rm = TRUE)

# Poverty gap/ poverty itensity - FGT(1)
svyfgt(~VD5008, svypnadc, g=1, abs_thresh = line, na.rm = TRUE)

# Quadratic poverty gap/ poverty severity - FGT(2)
svyfgt(~VD5008, svypnadc, g=2, abs_thresh = line, na.rm = TRUE)


###--------------------------------------------------------------------------###
# Child Poverty indicators

# create different age groups
svypnadc <- svypnadc %>% 
    mutate(child = factor(case_when(V2009 %in% 0:6 ~ "0-6",
                                    V2009 %in% 7:14 ~ "7-14")))

# Child FGT(0)
svyfgt(~VD5008, 
       subset(svypnadc, child == "0-6"), 
       g=0, 
       abs_thresh = line, 
       na.rm = TRUE)

svyfgt(~VD5008, 
       subset(svypnadc, child == "7-14"), 
       g=0, 
       abs_thresh = line, 
       na.rm = TRUE)

