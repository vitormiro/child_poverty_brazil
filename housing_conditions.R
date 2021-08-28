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