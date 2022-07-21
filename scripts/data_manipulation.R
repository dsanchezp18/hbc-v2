# ==================================== LAPOP ECUADOR DATA MANIPULATION ========================================================

# Daniel Sánchez
# USFQ Economics Undergraduate Thesis
# June 2022
# Script for the data manipulation of the LAPOP files for Ecuador.

# ================================================= Preliminaries =============================================================

# No working directory specification needed since an Rproject is being used
# Note that, to avoid overwriting of the base joining, one must run the FULL script everytime.

# Load libraries --------------------------------------------------------------------------------------------------------------

library(haven) # For reading dta files
library(tidyverse) # Ggplot and dplyr
library(survey) # Survey stuff
library(openxlsx) # Import and export XL files
library(broom) # Tidy
library(car) # for recode

# Load dataframes -------------------------------------------------------------------------------------------------------------

# I downloaded the databases for all available years until 2019 from the LAPOP website (these are the fully free)
# http://datasets.americasbarometer.org/database/index.php?freeUser=true

# These are the dta dataframes with no value labels for each year

df_2019<-read_dta('data/dta/ecu2019.dta')
df_2016<-read_dta('data/dta/ecu2016.dta')
df_2014<-read_dta('data/dta/ecu2014.dta')
df_2012<-read_dta('data/dta/ecu2012.dta')
df_2010<-read_dta('data/dta/ecu2010.dta')
df_2008<-read_dta('data/dta/ecu2008.dta')
df_2006<-read_dta('data/dta/ecu2006.dta')
df_2004<-read_dta('data/dta/ecu2004.dta')

# These are csv dataframes, exported from Stata. These do have value labels, might be necessary for some variables
# (I didn't know I could apply the value labels with the labelled package back then, sorry)

dfc_2019<-read_csv('data/csv/df2019.csv', show_col_types = F)
dfc_2016<-read_csv('data/csv/df2016.csv', show_col_types = F)
dfc_2014<-read_csv('data/csv/df2014.csv', show_col_types = F)
dfc_2012<-read_csv('data/csv/df2012.csv', show_col_types = F)
dfc_2010<-read_csv('data/csv/df2010.csv', show_col_types = F)
dfc_2008<-read_csv('data/csv/df2008.csv', show_col_types = F)
dfc_2006<-read_csv('data/csv/df2006.csv', show_col_types = F)
dfc_2004<-read_csv('data/csv/df2004.csv', show_col_types = F)

# Below the full, merged dataframe as found in the LAPOP website, loaded as an R object from the other project

load('data/rdata/LAPOP 2004-2019 Merged.Rdata') 

# This one I loaded into a separate script and then saved as an .RData file, because it was very heavy and GitHub wouldn't have it.


# ================================================= Starting with 2019 =============================================================

# I will start with the 2019 base and go backwards

df<-df_2019

#  Year -----------------------------------------------------------------------------------------------------------------------

# I need a year variable to signal CLEARLY the year the survey was responded

df$year<-2019

# Relocate it to the beginning

# Reorder to see year

df<-relocate(df,year, .after=idnum)

# Weights ----------------------------------------------------------------------------------------------------------------

# Create the weight1500 variable according to LAPOP's advice

df$weight1500<- (df$wt * 1500)/(nrow(df_2019))

# Political Parties ------------------------------------------------------------------------------------------------------

# Create the political party label, with clear text values so that it is easier to track support across time

df$party<-ifelse(df$vb11 == 913,  'PAIS',
                 ifelse(df$vb11 == 901, 'CREO', 
                        ifelse(df$vb11 == 903, 'PSC',
                               ifelse(df$vb11 == 915, 'ID', 
                                      ifelse(df$vb11 == 907, 'PK', 'Others'))))) %>% as.factor

# Variables for the exposure to corruption -------------------------------------------------------------------------------

# Later I will need to be able to difference from different kinds of NAs for these variables, namely, difference from 
# "Not applicable" of the usual "NA"

# Thus, for every year I will replace these variables from their csv counterparts, which do have applied labels.

df$exc11<-dfc_2019$exc11 %>% as.factor()
df$exc13<-dfc_2019$exc13 %>% as.factor()
df$exc14<-dfc_2019$exc14 %>% as.factor()
df$exc15<-dfc_2019$exc15 %>% as.factor()
df$exc16<-dfc_2019$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df$corr_mun<-ifelse(df$exc11 == 'Yes', 1,
                    ifelse(df$exc11 == 'Not Applicable'| df$exc11 == 'No', 0, NA))

df$corr_work<-ifelse(df$exc13 == 'Yes', 1,
                     ifelse(df$exc13== 'Not Applicable'| df$exc13 == 'No', 0, NA))

df$corr_court<-ifelse(df$exc14 == 'Yes', 1,
                      ifelse(df$exc14== 'Not Applicable'| df$exc14 == 'No', 0, NA))

df$corr_health<-ifelse(df$exc15 == 'Yes', 1,
                      ifelse(df$exc15== 'Not Applicable'| df$exc15 == 'No', 0, NA))

df$corr_school<-ifelse(df$exc16 == 'Yes', 1,
                       ifelse(df$exc16== 'Not Applicable'| df$exc16 == 'No', 0, NA))

# ================================================= Appending 2016 ============================================================

# Preliminary manipulation ----------------------------------------------------------------------------------------------------

# Add the year variable

df_2016$year<-2016

# Weights ----------------------------------------------------------------------------------------------------------------

# Create the weight variable for 2016 so that it appends correctly to the merged dataframe

df_2016$weight1500<- (df_2016$wt * 1500)/(nrow(df_2016))

# Political Sympathy -----------------------------------------------------------------------------------------------------

# Create the political sympathy variable in 2016, where numbers for parties are possibly different

df_2016$party<-ifelse(df_2016$vb11 == 913,  'PAIS',
                 ifelse(df_2016$vb11 == 901, 'CREO', 
                        ifelse(df_2016$vb11 == 903, 'PSC', 'Others'))) %>% as.factor()


# Corruption Exposure Variables ------------------------------------------------------------------------------------------

# Create the corruption exposure variables for this year so it works when joining

df_2016$exc11<-dfc_2016$exc11 %>% as.factor()
df_2016$exc13<-dfc_2016$exc13 %>% as.factor()
df_2016$exc14<-dfc_2016$exc14 %>% as.factor()
df_2016$exc15<-dfc_2016$exc15 %>% as.factor()
df_2016$exc16<-dfc_2016$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df_2016$corr_mun<-ifelse(df_2016$exc11 == 'Yes', 1,
                    ifelse(df_2016$exc11 == 'Not Applicable'| df_2016$exc11 == 'No', 0, NA))

df_2016$corr_work<-ifelse(df_2016$exc13 == 'Yes', 1,
                     ifelse(df_2016$exc13== 'Not Applicable'| df_2016$exc13 == 'No', 0, NA))

df_2016$corr_court<-ifelse(df_2016$exc14 == 'Yes', 1,
                      ifelse(df_2016$exc14== 'Not Applicable'| df_2016$exc14 == 'No', 0, NA))

df_2016$corr_health<-ifelse(df_2016$exc15 == 'Yes', 1,
                       ifelse(df_2016$exc15== 'Not Applicable'| df_2016$exc15 == 'No', 0, NA))

df_2016$corr_school<-ifelse(df_2016$exc16 == 'Yes', 1,
                       ifelse(df_2016$exc16== 'Not Applicable'| df_2016$exc16 == 'No', 0, NA))

# Append  ---------------------------------------------------------------------------------------------------------------------

# Append the 2016 observations to the 2019 base, giving NA values for all columns not included in both

df<-bind_rows(df,df_2016)

# ================================================= Appending 2014 ============================================================

# Preliminary manipulation ----------------------------------------------------------------------------------------------------

# Add the year variable

df_2014$year<-2014

# Change the data format of the ID column to character, so it is compatible with the bases of the following years

df$idnum<-df$idnum %>% as.character()

# Weights ----------------------------------------------------------------------------------------------------------------

# Create the weight variable for 2016 so that it appends correctly to the merged dataframe

df_2014$weight1500<- (df_2014$wt * 1500)/(nrow(df_2014))

# Political Sympathy --------------------------------------------------------------------------------------------------------------

# Create the political sympathy variable in 2014, where numbers for parties are possibly different

df_2014$party<-ifelse(df_2014$vb11 == 913,  'PAIS',
                      ifelse(df_2014$vb11 == 901, 'CREO', 
                             ifelse(df_2014$vb11 == 903, 'PSC',
                                    ifelse(df_2014$vb11 == 907, 'PK' ,'Others')))) %>% as.factor()
# Corruption Exposure Variables ------------------------------------------------------------------------------------------

# Create the corruption exposure variables for this year so it works when joining

df_2014$exc11<-dfc_2014$exc11 %>% as.factor()
df_2014$exc13<-dfc_2014$exc13 %>% as.factor()
df_2014$exc14<-dfc_2014$exc14 %>% as.factor()
df_2014$exc15<-dfc_2014$exc15 %>% as.factor()
df_2014$exc16<-dfc_2014$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df_2014$corr_mun<-ifelse(df_2014$exc11 == 'Yes', 1,
                         ifelse(df_2014$exc11 == 'Not Applicable'| df_2014$exc11 == 'No', 0, NA))

df_2014$corr_work<-ifelse(df_2014$exc13 == 'Yes', 1,
                          ifelse(df_2014$exc13== 'Not Applicable'| df_2014$exc13 == 'No', 0, NA))

df_2014$corr_court<-ifelse(df_2014$exc14 == 'Yes', 1,
                           ifelse(df_2014$exc14== 'Not Applicable'| df_2014$exc14 == 'No', 0, NA))

df_2014$corr_health<-ifelse(df_2014$exc15 == 'Yes', 1,
                            ifelse(df_2014$exc15== 'Not Applicable'| df_2014$exc15 == 'No', 0, NA))

df_2014$corr_school<-ifelse(df_2014$exc16 == 'Yes', 1,
                            ifelse(df_2014$exc16== 'Not Applicable'| df_2014$exc16 == 'No', 0, NA))

# Append  ---------------------------------------------------------------------------------------------------------------------

# Append the 2014 observations to the base, giving NA values for all columns not included in both

df<-bind_rows(df,df_2014)

# ================================================= Appending 2012 ============================================================

# Preliminary manipulation ----------------------------------------------------------------------------------------------------

# Add the year variable

df_2012$year<-2012

# Change the idnum variable to character in the 2012 base

df_2012$idnum<-df_2012$idnum %>% as.character()

# Eliminate dates as format is conflicting with the other dataframes, fix later if needed

df_2012$fecha<-NA

# Weights ----------------------------------------------------------------------------------------------------------------

# Create the weight variable for 2016 so that it appends correctly to the merged dataframe

df_2012$weight1500<- (df_2012$wt * 1500)/(nrow(df_2012))

# Political Sympathy -----------------------------------------------------------------------------------------------------

# Create the political sympathy variable in 2012, where numbers for parties are possibly different

df_2012$party<-ifelse(df_2012$vb11 == 913,  'PAIS',
                      ifelse(df_2012$vb11 == 904, 'ID', 
                             ifelse(df_2012$vb11 == 903, 'PSC',
                                    ifelse(df_2012$vb11 == 907, 'PK' ,'Others')))) %>% as.factor()

# Note that there is no CREO movement in this year, only for 2014, 2016 and 2019. Movement founded January 20th, 2012.

# Corruption Exposure Variables ------------------------------------------------------------------------------------------

# Create the corruption exposure variables for this year so it works when joining

df_2012$exc11<-dfc_2012$exc11 %>% as.factor()
df_2012$exc13<-dfc_2012$exc13 %>% as.factor()
df_2012$exc14<-dfc_2012$exc14 %>% as.factor()
df_2012$exc15<-dfc_2012$exc15 %>% as.factor()
df_2012$exc16<-dfc_2012$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df_2012$corr_mun<-ifelse(df_2012$exc11 == 'Si', 1,
                         ifelse(df_2012$exc11 == 'N/A'| df_2012$exc11 == 'No', 0, NA))

df_2012$corr_work<-ifelse(df_2012$exc13 == 'Si', 1,
                          ifelse(df_2012$exc13== 'N/A'| df_2012$exc13 == 'No', 0, NA))

df_2012$corr_court<-ifelse(df_2012$exc14 == 'Yes', 1,
                           ifelse(df_2012$exc14== 'N/A'| df_2012$exc14 == 'No', 0, NA))

df_2012$corr_health<-ifelse(df_2012$exc15 == 'Yes', 1,
                            ifelse(df_2012$exc15== 'N/A'| df_2012$exc15 == 'No', 0, NA))

df_2012$corr_school<-ifelse(df_2012$exc16 == 'Yes', 1,
                            ifelse(df_2012$exc16== 'N/A'| df_2012$exc16 == 'No', 0, NA))

# Append  ---------------------------------------------------------------------------------------------------------------------

# Append the 2012 observations to the base, giving NA values for all columns not included in both

df<-bind_rows(df,df_2012)

# ================================================= Appending 2010 ============================================================

# Preliminary manipulation ----------------------------------------------------------------------------------------------------

# Add the year variable

df_2010$year<-2010

# Change the idnum variable to character in the 2012 base

df_2010$idnum<-df_2010$idnum %>% as.character()

# Eliminate dates as format is conflicting with the other dataframes, fix later if needed

df_2010$fecha<-NA

# Political Sympathy -----------------------------------------------------------------------------------------------------

# Create the political sympathy variable in 2010, where numbers for parties are possibly different

df_2010$party<-ifelse(df_2010$vb11 == 913,  'PAIS',
                      ifelse(df_2010$vb11 == 904, 'ID', 
                             ifelse(df_2010$vb11 == 903, 'PSC',
                                    ifelse(df_2010$vb11 == 907, 'PK' ,'Others')))) %>% as.factor()
# Weights ----------------------------------------------------------------------------------------------------------------

# Create the weight variable for 2016 so that it appends correctly to the merged dataframe

df_2010$weight1500<- (df_2010$wt * 1500)/(nrow(df_2010))

# Corruption Exposure Variables ------------------------------------------------------------------------------------------

# Create the corruption exposure variables for this year so it works when joining

df_2010$exc11<-dfc_2010$exc11 %>% as.factor()
df_2010$exc13<-dfc_2010$exc13 %>% as.factor()
df_2010$exc14<-dfc_2010$exc14 %>% as.factor()
df_2010$exc15<-dfc_2010$exc15 %>% as.factor()
df_2010$exc16<-dfc_2010$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df_2010$corr_mun<-ifelse(df_2010$exc11 == 'Sí', 1,
                         ifelse(df_2010$exc11 == 'N/A'| df_2010$exc11 == 'No', 0, NA))

df_2010$corr_work<-ifelse(df_2010$exc13 == 'Sí', 1,
                          ifelse(df_2010$exc13== 'N/A'| df_2010$exc13 == 'No', 0, NA))

df_2010$corr_court<-ifelse(df_2010$exc14 == 'Sí', 1,
                           ifelse(df_2010$exc14== 'N/A'| df_2010$exc14 == 'No', 0, NA))

df_2010$corr_health<-ifelse(df_2010$exc15 == 'Sí', 1,
                            ifelse(df_2010$exc15== 'N/A'| df_2010$exc15 == 'No', 0, NA))

df_2010$corr_school<-ifelse(df_2010$exc16 == 'Sí', 1,
                            ifelse(df_2010$exc16== 'N/A'| df_2010$exc16 == 'No', 0, NA))

# Append  ---------------------------------------------------------------------------------------------------------------------

# Append the 2012 observations to the base, giving NA values for all columns not included in both

df<-bind_rows(df,df_2010)

# ================================================= Appending 2008 ============================================================

# Preliminary manipulation ----------------------------------------------------------------------------------------------------

# Add the year variable

df_2008$year<-2008

# Change the idnum variable to character in the 2012 base

df_2008$idnum<-df_2008$idnum %>% as.character()

# Eliminate dates as format is conflicting with the other dataframes, fix later if needed

df_2008$fecha<-NA

# Political Sympathy -----------------------------------------------------------------------------------------------------

# Create the political sympathy variable in 2008, where numbers for parties are possibly different

df_2008$party<-ifelse(df_2008$vb11 == 913,  'PAIS',
                      ifelse(df_2008$vb11 == 904, 'ID', 
                             ifelse(df_2008$vb11 == 903, 'PSC',
                                    ifelse(df_2008$vb11 == 907, 'PK' ,'Others')))) %>% as.factor()

# Weights ----------------------------------------------------------------------------------------------------------------

# Create the weight variable for 2016 so that it appends correctly to the merged dataframe

df_2008$weight1500<- (df_2008$wt * 1500)/(nrow(df_2008))

# Corruption Exposure Variables ------------------------------------------------------------------------------------------

# Create the corruption exposure variables for this year so it works when joining

df_2008$exc11<-dfc_2008$exc11 %>% as.factor()
df_2008$exc13<-dfc_2008$exc13 %>% as.factor()
df_2008$exc14<-dfc_2008$exc14 %>% as.factor()
df_2008$exc15<-dfc_2008$exc15 %>% as.factor()
df_2008$exc16<-dfc_2008$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df_2008$corr_mun<-ifelse(df_2008$exc11 == 'Sí', 1,
                         ifelse(df_2008$exc11 == 'Not Applicable'| df_2008$exc11 == 'No', 0, NA))

df_2008$corr_work<-ifelse(df_2008$exc13 == 'Sí', 1,
                          ifelse(df_2008$exc13== 'Not Applicable'| df_2008$exc13 == 'No', 0, NA))

df_2008$corr_court<-ifelse(df_2008$exc14 == 'Sí', 1,
                           ifelse(df_2008$exc14== 'Not Applicable'| df_2008$exc14 == 'No', 0, NA))

df_2008$corr_health<-ifelse(df_2008$exc15 == 'Sí', 1,
                            ifelse(df_2008$exc15== 'Not Applicable'| df_2008$exc15 == 'No', 0, NA))

df_2008$corr_school<-ifelse(df_2008$exc16 == 'Sí', 1,
                            ifelse(df_2008$exc16== 'Not Applicable'| df_2008$exc16 == 'No', 0, NA))

# Append  ---------------------------------------------------------------------------------------------------------------------

# Append the 2012 observations to the base, giving NA values for all columns not included in both

df<-bind_rows(df,df_2008)

# ================================================= Appending 2006 ===========================================================

# Preliminary manipulation ----------------------------------------------------------------------------------------------------

# Add the year variable

df_2006$year<-2006

# Eliminate dates as format is conflicting with the other dataframes, fix later if needed

df_2006$fecha<-NA

# Weights ----------------------------------------------------------------------------------------------------------------

# Create the weight variable for 2016 so that it appends correctly to the merged dataframe

df_2006$weight1500<- (df_2006$wt * 1500)/(nrow(df_2006))

# Sampling design --------------------------------------------------------------------------------------------------------

# Change the name of the strata variable for it to be consequent with other years

df_2006<-rename(df_2006, 'estratopri'= estrato)

# Corruption Exposure Variables ------------------------------------------------------------------------------------------

# Create the corruption exposure variables for this year so it works when joining

df_2006$exc11<-dfc_2006$exc11 %>% as.factor()
df_2006$exc13<-dfc_2006$exc13 %>% as.factor()
df_2006$exc14<-dfc_2006$exc14 %>% as.factor()
df_2006$exc15<-dfc_2006$exc15 %>% as.factor()
df_2006$exc16<-dfc_2006$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df_2006$corr_mun<-ifelse(df_2006$exc11 == 'Sí', 1,
                         ifelse(df_2006$exc11 == 'Not Applicable'| df_2006$exc11 == 'No', 0, NA))

df_2006$corr_work<-ifelse(df_2006$exc13 == 'Sí', 1,
                          ifelse(df_2006$exc13== 'Not Applicable'| df_2006$exc13 == 'No', 0, NA))

df_2006$corr_court<-ifelse(df_2006$exc14 == 'Si', 1,
                           ifelse(df_2006$exc14== 'Not Applicable'| df_2006$exc14 == 'No', 0, NA))

df_2006$corr_health<-ifelse(df_2006$exc15 == 'Si', 1,
                            ifelse(df_2006$exc15== 'Not Applicable'| df_2006$exc15 == 'No', 0, NA))

df_2006$corr_school<-ifelse(df_2006$exc16 == 'Si', 1,
                            ifelse(df_2006$exc16== 'N/A'| df_2006$exc16 == 'No', 0, NA))

# Append  ---------------------------------------------------------------------------------------------------------------------

# Append the 2006 observations to the base, giving NA values for all columns not included in both

df<-bind_rows(df,df_2006)

# ================================================= Appending 2004 ===========================================================

# We will add the 2004 variable from the merged dataframe, it has all observations and includes the UPM, which ain't present
# in the sole 2004 dataframe.

# Preliminary manipulation ----------------------------------------------------------------------------------------------------

# Subset the Ecuador 2004 entries from the merged dataframe

df_2004_ec<-subset(df_all, pais == 9 & year == 2004)

# Remove empty columns in this df
emptycols<- sapply(df_2004_ec, function (k) all(is.na(k)))
df_2004_ec<- df_2004_ec[!emptycols]

# Change the id variable so it does not give out a problem 

df_2004_ec$idnum<-NA

# Corruption Exposure Variables ------------------------------------------------------------------------------------------

# Create the corruption exposure variables for this year so it works when joining

df_2004_ec$exc11<-dfc_2004$exc11 %>% as.factor()
df_2004_ec$exc13<-dfc_2004$exc13 %>% as.factor()
df_2004_ec$exc14<-dfc_2004$exc14 %>% as.factor()
df_2004_ec$exc15<-dfc_2004$exc15 %>% as.factor()
df_2004_ec$exc16<-dfc_2004$exc16 %>% as.factor()

# Create the corruption variables which make sense for later analysis of the corruption exposure variable 

df_2004_ec$corr_mun<-ifelse(df_2004_ec$exc11 == 'Yes', 1,
                         ifelse(df_2004_ec$exc11 == 'Not Applicable'| df_2004_ec$exc11 == 'No', 0, NA)) 

df_2004_ec$corr_work<-ifelse(df_2004_ec$exc13 == 'Yes', 1,
                          ifelse(df_2004_ec$exc13== 'Not Applicable'| df_2004_ec$exc13 == 'No', 0, NA))

df_2004_ec$corr_court<-ifelse(df_2004_ec$exc14 == 'Yes', 1,
                           ifelse(df_2004_ec$exc14== 'Not Applicable'| df_2004_ec$exc14 == 'No', 0, NA))

df_2004_ec$corr_health<-ifelse(df_2004_ec$exc15 == 'Yes', 1,
                            ifelse(df_2004_ec$exc15== 'Not Applicable'| df_2004_ec$exc15 == 'No', 0, NA))

df_2004_ec$corr_school<-ifelse(df_2004_ec$exc16 == 'Yes', 1,
                            ifelse(df_2004_ec$exc16== 'Not Applicable'| df_2004_ec$exc16 == 'No', 0, NA))

# Append  ---------------------------------------------------------------------------------------------------------------------

# Append the 2004 observations to the base, giving NA values for all columns not included in both

df<-bind_rows(df,df_2004_ec)

# ================================================ 2014 and 2016 base ========================================================

# Create a base with only 2014 and 2016

df46<-subset(df, df$year == 2014 | df$year == 2016)

# ================================================ Data Manipulation ========================================================

# Now that I have my merged dataframe, I can perform data manipulation as needed in this script.

# ================================================= GENERAL  =============================================================

# Year -------------------------------------------------------------------------------------------------------------------

# Make the year variable, created or remapped previously, a factor so it works in models

df$year<- as.factor(df$year)

# Create year dummies ----------------------------------------------------------------------------------------------------

df$y16<-ifelse(df$year == '2014',1,0)

# ================================================= Basic Questions ================================================================

# Rename and remap some basic questions for analysis

# Gender -----------------------------------------------------------------------------------------------------------------

df$gndr<-ifelse(df$q1==1,'Man', 'Woman') %>% as.factor()

# I also create a logical so that I can use it for my descriptive table.

df$gndr_log<-df$q1 == 2

# Age --------------------------------------------------------------------------------------------------------------------

df<-rename(df, 'age'=q2) 

df$age<-as.integer(df$age) # Transform to a number

# Years of Schooling -----------------------------------------------------------------------------------------------------

# We change the format of years of schooling

df<-
  df %>% 
  mutate(ed = as.numeric(ed))

# ================================================= Sociodemographic ==============================================================

# Urban/Rural ------------------------------------------------------------------------------------------------------------

# Relabel variable 

df$ur<-ifelse(df$ur == 1, 'Urban', 'Rural')

# Logical

df$ur_log<-df$ur == 'Urban'

# Age-Related Variables --------------------------------------------------------------------------------------------------

# A dummy variable signalling the people who are less than 18 (16 or 17) at the time of the survey

df$new<-ifelse(df$age < 18, 1, 0) 

# Same but as a factor

df$new_f<-ifelse(df$age < 18, 'New', 'Older') %>% as.factor()

# Age groups:

agegroups<-c(16,19,30,40,50,60,70,80,90) # 16-19, 20-30, 31-40,41-50, etc.

df$age_group<-cut(df$age,agegroups)

# Region -----------------------------------------------------------------------------------------------------------------

# Relabel the region variables 

df$region<- ifelse(df$estratopri == 901, 'Costa', 
                    ifelse ( df$estratopri == 902, 'Sierra', 'Oriente'))

# Religion ---------------------------------------------------------------------------------------------------------------

# Relabel the kind of religion 

df$rlg<-ifelse(df$q3c == 1, 'Catholic',
               ifelse(df$q3c == 2 | df$q3c == 5, 'Christian',
                      ifelse(df$q3c == 3 | df$q3c == 4 | df$q3c == 77, 'Other',
                             ifelse(df$q3c == 6 | df$q3c == 12, 'JW/MOR', 'Atheist'))))

df$cthlic<-ifelse(df$q3c == 1, 1, 0) # Catholic Dummy 

df$christian<-ifelse(df$q3c == 2 | df$q3c == 5, 1, 0) # Christian Dummy

# Relabel the importance of religion, dichotomizing it 

df$rlg_imp<-ifelse(df$q5b < 2, 1, 0) # Very and somewhat important are labelled as 1, little and nothing are 0. 

# Race -------------------------------------------------------------------------------------------------------------------

# Create a white dummy

df$white<-ifelse(df$etid == 1, 1, 0)

# News -------------------------------------------------------------------------------------------------------------------

# Rename and relabel the exposure to news variable

df<-rename(df, 'news_exp'=gi0n)

df$news_exp<- factor(df$news_exp,
                     levels = c(1,2,3,4,5),
                     labels = c('Daily', 'Weekly', 'Monthly', 'Yearly', 'Never'))

# Same but with the other question for previous years

df<-rename(df, 'news_exp1'=gi0)

df$news_exp1<- factor(df$news_exp1,
                      levels = c(1,2,3,4,5),
                      labels = c('Daily', 'Weekly', 'Monthly', 'Rarely', 'Never'))

# ================================================= Economic Variables========================================================

# idio2 Question ---------------------------------------------------------------------------------------------------------

# Question which asks if the economic situation of the respondent is the same, worse or better

# Make a dummy that equals 1 if the economical situation of the person answering is WORSE than the one a year ago

df$econ_sit<-ifelse(df$idio2==3,'Worse','Same or Better') %>% as.factor()

# Logical

df$econ_sit_log<- df$idio2 == 3

# q10e, income question --------------------------------------------------------------------------------------------------

# Only available up to 2008, make a dummy which equals down if income has gone down relative to last year

df$inc_sit<-ifelse(df$q10e==3, 'Decreased', 'Same or Increased') %>% as.factor()

# Relevel to show lesser income as the one in the regressions

df$inc_sit<-relevel(df$inc_sit, 'Same or Increased') # We change to the reference level that we want to compare to

# Government Aid ---------------------------------------------------------------------------------------------------------

# Rename and relabel the government aid variables

df<-rename(df, 'govaid'=wf1, 'hdb'=cct1b)

df$govaid<-ifelse(df$govaid == 2, 'No', 'Yes') # Any kind of government aid

df$hdb<- ifelse(df$hdb == 2, 'No', 'Yes') # Bono de Desarrollo Humano

# The reference group (base group) is always "No", those who do not receive any aid

# Economic evaluation of the economy -------------------------------------------------------------------------------------

# Rename and relabel the evaluation of the economy of the respondent, turn into a factor

df<-rename(df, 'ec_eval'=soct2)

df$ec_eval<-ifelse(df$ec_eval == 3, 'Worse', 'Same or Better') %>% as.factor()

# Relevel so the reference group is same or better 

df$ec_eval<-relevel(df$ec_eval, 'Same or Better')

# ================================================= Corruption ================================================================

# Exposure to corruption  ------------------------------------------------------------------------------------------------

# My own version of the corruption victimization variable
# Will equal 1 if in any of the corruption question the person answers having been offered to bribe or actually bribed
# Will equal 0 for all people who answer "No" or who have not used the services

# First, rename the variables that were not manipulated in the join bases script

df<-rename(df, 'corr_pol'= exc2, 'corr_pub' = exc6)

# All the variables have already been created, so just create the conditional at this point 

df$corr_exp<-ifelse(df$corr_pol == 1| df$corr_pub == 1| df$corr_mun == 1| df$corr_work == 1| df$corr_court== 1
                    | df$corr_health == 1 | df$corr_school== 1, 1, 0)

# Corruption Tolerance ----------------------------------------------------------------------------------------------------

# Key variable for the study

# Rename it and turn to a factor

df<-rename(df, 'ctol'= exc18)

# Corruption Perceptions -------------------------------------------------------------------------------------------------

# Dichotomize the corruption perceptions variable in a way comparable for 2014 and 2016

# First for 2019, 2014 and all previous years

df$corrper<- ifelse(df$exc7 < 3, 1, 0) # Equals 1 if the person determines a higher corruption perception 

# Now for 2016, but create a new variable to not overwrite the other variable

df$corrper16<-ifelse(df$exc7new < 3, 0, 1 )

# Now, I will join the two variables in a sole column to use it in a regression

df$corrper<- ifelse(is.na(df$corrper) == T, df$corrper16, df$corrper)

# Now use the EXC7NEW variable to do this:

df$corrper_new<-ifelse(df$exc7new >2, 1, 0) # DUmmy variable equaling 1 for all who consider at least half politicians are corrupt

# Corruption as the country's most important problem ---------------------------------------------------------------------

df$corprob<-ifelse(df$a4 == 13, 1, 0) # Extract a dummy equaling 1 if the respondent thinks corruption is the most important problem in the country. 

# ================================================= Labor Market ==============================================================

# Labor Market Status OCUP4A  --------------------------------------------------------------------------------------------

# For the ocup4a question, available 2008 through 2019, convert the numbers to factors for regression analysis

df$ocup4a <- as.factor(df$ocup4a)

# Create a variable that signals employment, 1 if employed 

df$em_4a<-ifelse(df$ocup4a == 1 | df$ocup4a == 2 ,1,0)

# Create a variable that signals open unemployment (only those that are looking for job)

df$unem_4a<-ifelse(df$ocup4a == 3,1,0 ) %>% as.factor()

# Create a variable that signals both kinds of unemployed, looking and not looking

df$unem2_4a<-ifelse(df$ocup4a == 3 | df$ocup4a == 7, 1, 0)

# Create a new labor market variable, with the following categories

# Works (either working right now or has a job but not working right now)
# Unemployed, either type (looking or not looking)
# Not in the workforce (Student, Retired, Works the home)

df$work_2a<-ifelse(df$ocup4a == 1 | df$ocup4a == 2, 'Employed',
                   ifelse(df$ocup4a == 3 | df$ocup4a == 7, 'Unemployed','Not WF')) %>% as.factor()

# Relevel so reference is not on the workforce

df$work_2a<-relevel(df$work_2a, 'Not WF')

# Create a dummy that indicates if the person is or not in the economically active population (works or able to work)

df$eap<-ifelse(df$ocup4a == 1 | df$ocup4a == 2 | df$ocup4a == 3 | df$ocup4a == 7, 'EAP', 'Not EAP')

# Labor Market Sector -------------------------------------------------------------------------------------------------

# Create a new factor, with all 5 groups in them. 

df$ocup_sec<-factor(df$ocup1a,
                    levels = c(seq(1:5)),
                    labels = c('Public', 'Private','Owner/Partner','Self-Employed','Unpaid')) %>% as.factor()

# Create a self-employed dummy for the DiD

df$self_emp<-ifelse(df$ocup_sec == 'Self-Employed', 1, 0) %>%  as.factor()

# Create a new group/factor variable simply separating private from public. 

df$privpub<-ifelse(df$ocup_sec == 'Public', 'Public', 'Private') %>% as.factor()

# Relevel from public sector. 

df$privpub<-relevel(df$privpub, 'Private') 

# ================================================= Political vars ==============================================================

# Political Party and Ideals --------------------------------------------------------------------------------------------------------

# Rename the political ideology discrete variable, an make it numeric

df<-
  df %>% 
  rename('polscore'=l1) %>% 
  mutate(polscore = as.numeric(polscore))

# The larger the value answered, the more to the right the person is

# Add a LEFT/RIGHT dummy, which will equal 1 if the person identifies with the right wing scores 5 and higher, left otherwise

df$pol_wing<-ifelse(df$polscore < 5, 'Left', 'Right')

# Add another, similar dummy but the 5 score is now given to the left

df$pol_wing2<-ifelse(df$polscore < 6, 'Left', 'Right')

# Add other classification, with Left, Center and Right (As in the LAPOP report)

# First, I need to add the NA values as a "None" so that the %'s are computed correctly. 

df$pol_group<-ifelse(is.na(df$polscore), 'None',
                     ifelse(df$polscore < 4, 'Left', 
                            ifelse(df$polscore < 8, 'Center', 'Right')))

# Right and Left according to this grouping, dichotomized 

df$leftist<-ifelse(df$pol_group == 'Left', 1, 0)

df$rightist<-ifelse(df$pol_group == 'Right', 1, 0)

# Add a politicized dummy (if he answered or not the polscore question)

df$plscr_na<-ifelse(is.na(df$polscore), 1, 0)

# Political right wing dummy, as considered by LAPOP

df$rwing<-ifelse(df$polscore > 6, 1, 0)

# Rename the vb10 question, which shows if there is any sympathy with political parties

df<-rename(df, 'pol_symp'=vb10)

df$pol_symp<-ifelse(df$pol_symp == 1, 'Yes', 'No') 

# For year 2019, rename the vb11neg variable and apply labels, what party is DISLIKED

df<-rename(df, 'pol_dis'=vb11neg)

df$pol_dis<-as.factor(df$pol_dis)

# Political Efficiency ---------------------------------------------------------------------------------------------------

# Make a dichotomized version of the internal political efficiency variable

df$knowspol<-ifelse(df$eff2> 4, 1, 0)

# Change the format of the variables eff1 and eff2 so that I can use them in the models and then compute APEs

df<-
  df %>% 
  mutate(eff1 = as.numeric(eff1),
         eff2 = as.numeric(eff2))

# Interest in Politics -----------------------------------------------------------------------------------------------------

# Create an interest in politics factor to difference groups of interest

df$pol_int<-ifelse(df$pol1 == 1, 'A lot', 
                   ifelse(df$pol1 == 2, 'Some',
                          ifelse(df$pol1 == 3, 'Little', 'None'))) %>% as.factor()

df$pol_int<-relevel(df$pol_int, 'None')

# Create a dummy which equals 1 if interest is a lot or some, and 0 if interest is little or none

df$pint_dic<-ifelse(df$pol_int == 'A lot' | df$pol_int == 'Some', 1, 0)

# Participation in protests ---------------------------------------------------------------------------------------------

# Relabel the variable

df$prot3<- ifelse(df$prot3 == 1, 'Yes','No') %>% as.factor()

# Logical

df$prot_log<-df$prot3 == 'YeS'

# Confidence in President ------------------------------------------------------------------------------------------------

# Rename the variable and make it a numeric variable so it works with the APE model calculation.

df<-
  df %>% 
  rename('pres_conf' = b21a) %>% 
  mutate(pres_conf = as.numeric(pres_conf))

# Dichotomize the variable as the LAPOP articles do

df$pres_conf_dic<-ifelse(df$pres_conf >= 5, 'Yes', 'No') %>% as.factor()

# Job Approval Rating ----------------------------------------------------------------------------------------------------

# Rename the variable

df<-rename(df, 'pres_aprov' = m1)

# Recode it so that a higher number means more approval

df$pres_aprov<-6-df$pres_aprov

# Dichotomize so the two higher scores represent a lot of confidence

df$pres_aprov_dic<-ifelse(df$pres_aprov > 3, 'Yes', 'No')

# ================================================= Exporting/Saving =======================================================

# Export all changes done to the dataframes in this script

# Dataframe Overwriting -------------------------------------------------------------------------------------------------

# Rewrite the dataframes so that all changes are also saved to the dataframes on their own 

df_2019<-subset(df, year == 2019)
df_2016<-subset(df, year == 2016)
df_2014<-subset(df, year == 2014)
df_2012<-subset(df, year == 2012)
df_2010<-subset(df, year == 2010)
df_2008<-subset(df, year == 2008)
df_2006<-subset(df, year == 2006)
df_2004<-subset(df, year == 2004)
df46<-subset(df, year == 2014 | year == 2016)

# Save them as R objects to use them later

save(df_2019,file = 'data/rdata/LAPOP 2019 Manipulated Dataframe.Rdata')
save(df_2016,file = 'data/rdata/LAPOP 2016 Manipulated Dataframe.Rdata')
save(df_2014,file = 'data/rdata/LAPOP 2014 Manipulated Dataframe.Rdata')
save(df_2012,file = 'data/rdata/LAPOP 2012 Manipulated Dataframe.Rdata')
save(df_2010,file = 'data/rdata/LAPOP 2010 Manipulated Dataframe.Rdata')
save(df_2008,file = 'data/rdata/LAPOP 2008 Manipulated Dataframe.Rdata')
save(df_2006,file = 'data/rdata/LAPOP 2006 Manipulated Dataframe.Rdata')
save(df_2004,file = 'data/rdata/LAPOP 2008 Manipulated Dataframe.Rdata')
save(df46, file = 'data/rdata/LAPOP 2014-2016 Manipulated Dataframe.Rdata')
save(df, file = 'data/rdata/LAPOP 2004-2019 Manipulated Dataframe.Rdata')

# Delete everything but what is needed ------------------------------------------------------------------------------------------------------

# Here I will remove all the unnecessary objects from my workspace

rm(list = setdiff(ls(), 
                  c('df', 'df_2014', 'df_2016', 'df46')))

# Survey design objects --------------------------------------------------------------------------------------------------

# Full 2004-2019 design

lapop_des<-svydesign(ids = ~ upm, 
                     strata = ~ estratopri, 
                     weights = ~ weight1500, 
                     nest = TRUE,
                     na.action = 'na.exclude',
                     data = df)

# 2014-2016 Survey Design

lapop_des46<-svydesign(ids = ~ upm, 
                       strata = ~ estratopri, 
                       weights = ~ weight1500, 
                       nest = TRUE,
                       na.action = 'na.exclude',
                       data = df46)

# 2016 free dataset

lapop_des16<-svydesign(ids = ~ upm, 
                       strata = ~ estratopri, 
                       weights = ~ weight1500, 
                       nest = TRUE,
                       na.action = 'na.exclude',
                       data = df_2016)

# 2014 free dataset

lapop_des14<-svydesign(ids = ~ upm, 
                       strata = ~ estratopri, 
                       weights = ~ weight1500, 
                       nest = TRUE,
                       na.action = 'na.exclude',
                       data = df_2014)




