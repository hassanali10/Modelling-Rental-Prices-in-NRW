library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(epiDisplay)

# load data set into data-frame 'df1'
df1<-read.csv("C:/Users/Hassan Ali/Desktop/ICS/Project 3/ImmoDataNRW.csv")

sum(df1$energyEfficiencyClass=="NO_INFORMATION")
sum(is.na(df1$yearConstructed))
unique(df1$noParkSpaces)


# ------------------------------------------------------------------------------
# TASK 1
# ------------------------------------------------------------------------------

# create a new variable named 'parking_transf'
# by grouping the variable 'noParkSpaces'
df1 = df1 %>%mutate(
  parking_transf = case_when(
    is.na(noParkSpaces)  ~ "0 or no information",
    noParkSpaces == 0 ~ "0 or no information",
    TRUE                      ~  "1+"
    )
  )

# create a new variable named 'type_flat_transf'
# by grouping the variable 'typeOfFlat' 
# missing values are grouped separately into the class 'no information'
df1 = df1%>%mutate(
  type_flat_transf = case_when(
    typeOfFlat %in% ("apartment")  ~ "apartment",
    typeOfFlat %in% c('loft', 
                      'maisonette',
                      'penthouse',
                      'terraced_flat',
                      'other') ~ "luxurious_artistic_other",
    typeOfFlat %in% c('ground_floor',
                      'raised_ground_floor') ~ "r_ground_floor",
    typeOfFlat %in% c('roof_storey',
                      'half_basement') ~ "roof_halfBasement",
    TRUE                      ~  "no information"
    )
  )

# create a new variable named 'yearConstructed_transf'
# by subtracting each year from 2020 
df1$yearConstructed_transf<-(2020-df1$yearConstructed)

# create a new variable named 'CityType'
# by grouping the variable 'regio2' 
df1 = df1 %>%mutate(
  CityType = case_when(
    regio2 %in% c('Köln', 
                  'Düsseldorf',
                  'Dortmund',
                  'Essen',
                  'Duisburg') ~ "1-5",
    regio2 %in% c('Bochum',
                  'Wuppertal',
                  'Bielefeld',
                  'Bonn',
                  'Münster') ~ "6-10",
    TRUE                      ~  "elsewhere"
  )
)

# drop the variable 'ID' and the above (four) modified variables
drop<-c( "ID", "noParkSpaces", "typeOfFlat", "yearConstructed", "regio2")
df1 = df1[,!names(df1)%in%drop]

# evaluate the structure of the data-frame
str(df1)

# convert the following variables into type 'factor'
df1$newlyConst <- as.factor(df1$newlyConst)
df1$balcony <- as.factor(df1$balcony)
df1$parking_transf <- as.factor(df1$parking_transf)
df1$hasKitchen <- as.factor(df1$hasKitchen)
df1$lift <- as.factor(df1$lift)
df1$type_flat_transf <- as.factor(df1$type_flat_transf)
df1$garden <- as.factor(df1$garden)
df1$CityType <- as.factor(df1$CityType)
df1$condition <- as.factor(df1$condition)
df1$lastRefurbish <- as.factor(df1$lastRefurbish)
df1$energyEfficiencyClass <- as.factor(df1$energyEfficiencyClass)


# ------------------------------------------------------------------------------
# UNIVARIATE ANALYSIS
# ------------------------------------------------------------------------------

#discrete variables with 2 levels
summary(df1$newlyConst)
summary(df1$balcony)
summary(df1$hasKitchen)
summary(df1$lift)
summary(df1$garden)
summary(df1$parking_transf)

# discrete variables with more than 2 levels
floor = df1$floor
ggplot(df1, aes(df1$floor)) +
  geom_bar() + xlab("Floor") + ylab("Frequency") 

tab1(df1$floor, xlab = "Floor")
tab1(df1$condition,
     names.arg=c("Average","Good", "No information"))
tab1(df1$lastRefurbish,
     names.arg=c("Last 5 years","No information", "Over 5 years"))
tab1(df1$energyEfficiencyClass,
     names.arg=c("A+/A/B/C", "D/E/F/G/H", "No information"))
tab1(df1$type_flat_transf,
     names.arg=c("Apartment","Luxurious/Artistic/Other",
                 "No information", "Raised ground floor/ Ground floor",
                 "Roof storey/ Half basement"))
tab1(df1$CityType,
     names.arg=c("1-5", "6-10","Elsewhere"))

# continuous variables
summary(df1$totalRent)
boxplot(df1$totalRent, ylab="Total rent (???)")
summary(df1$livingSpace)
boxplot(df1$livingSpace, ylab="Property size (m²)")
summary(df1$yearConstructed_transf)
boxplot(df1$yearConstructed_transf, ylab="Property age (years)")


# ------------------------------------------------------------------------------
# TASK 2
# ------------------------------------------------------------------------------

# create a new data-frame 'df2' from 'df1' for this task only
# remove rows where the total rent is missing
df2<-subset(df1,  !totalRent=="NA")

# create a new variable named 'sqmPrice'
# this contains rental price per square meter for each row
df2$sqmPrice<-(df2$totalRent/df2$livingSpace)

# drop the variable 'totalRent'
drop<-c("totalRent")
df2 = df2[,!names(df2)%in%drop]

# Analysis of the Price per square meter
summary(df2$sqmPrice)
boxplot(df2$sqmPrice, ylab="Total rent/ area (???/m²)")

# Estimate linear model for 'sqmPrice' using the remaining variables
linearmodel = lm(sqmPrice ~ . , data=df2)
summary(linearmodel)

# apply backward stepwise variable selection using the AIC
linearmodel = step(linearmodel, direction = "backward")
summary(linearmodel)

# some plots of the fitted model
plot(linearmodel)


# ------------------------------------------------------------------------------
# TASK 3
# ------------------------------------------------------------------------------

# create a new data-frame 'df3' from 'df1' for this task only
# remove the variables 'yearConstructed_transf' and 'lastRefurbish'
drop<-c("yearConstructed_transf", "lastRefurbish")
df3 = df1[,!names(df1)%in%drop]

# check the number of missing values in the continuous variables
sum(is.na(df3$totalRent))
sum(is.na(df3$livingSpace))

# only 'totalRent' has missing values
# replace these with the mean of the remaining rows
df3$totalRent <- na.aggregate(df3$totalRent)

# summary of the variable 'newlyConst' 
# Only around 7% of the houses are newly constructed
summary(df3$newlyConst)

# Estimate logistic model for 'newlyConst' using the remaining variables
logisticmodel = glm(newlyConst ~ ., data=df3, family = "binomial")
summary(logisticmodel)

# apply backward stepwise variable selection using the AIC
logisticmodel = step(logisticmodel, direction = "backward")
summary(logisticmodel)

# some plots of the fitted model
plot(logisticmodel)

# create a confusion matrix
# A probability of greater than 0.5 is mapped to 1 and vice versa
conf_matrix = table(logisticmodel$fitted.values>0.5, df3$newlyConst)
conf_matrix

# overall prediction accuracy
(sum(diag(conf_matrix))/sum(conf_matrix))*100

