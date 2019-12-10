#Data Analysis of Guatemala DHS 2014-15 Data


##***GUATE-DHS-2015-Data********************************************************************
###Birth Recode Import*****
#BirthData <- haven::read_dta("C:/Users/eel_v/Documents/Work/Analyses/Guate_DHS_2015/Data/GUBR71DT_BirthRecode/GUBR71FL.DTA")
##Removing Empty Columns
#BirthData2 <- BirthData[!sapply(BirthData, function (x) all(is.na(x) | x == ""))]
##Removing Columns with >30% of Data Missing
#BirthData3 <- BirthData2[colSums(is.na(BirthData2))/nrow(BirthData2) < .3]
#lapply(BirthData3[, c(92:106)], table)
##Remove rows/cases if 0 is present (i.e. no children under 5 = v137)
#BirthData4 <- BirthData3[apply(BirthData3, 1, function(v137) all(v137 !=0 )),]



###Household Recode Import****
HHData <- haven::read_dta("C:/Users/eel_v/Documents/Work/Guatemala/Guate_DHS_2015/Data/GUHR71DT_HouseholdRecode/GUHR71FL.DTA")
library(dplyr)
HHData2 <- select(HHData, hv001, hv002, hv003, hv230a, hv230b, hv232, hv235, hv237, hv237a, hv237b, hv237c, hv237d, hv237e, hv237f, hv237x, hv237z, hv238, hv241, hv242, hv246, hv246b, hv246c, hv246d, hv246f, hv246g, hc61_01, hc62_01)
colnames(HHData2)[1] <- "Cluster_ID"
colnames(HHData2)[2] <- "Home_ID"
HHData2$Child_ID <- with(HHData2, paste0(Cluster_ID, sep="_", Home_ID))


###Individual Recode Import****
#IndData <- haven::read_dta("C:/Users/eel_v/Documents/Work/Guatemala/Guate_DHS_2015/Data/GUIR71DT_IndividualRecode/GUIR71FL.DTA")
#head(IndData$ha40, n=30)


###Childrens Recode Import****
ChildData <- haven::read_dta("C:/Users/eel_v/Documents/Work/Guatemala/Guate_DHS_2015/Data/GUKR71DT_ChildrensRecode/GUKR71FL.DTA")
ChildData2 <- select(ChildData, caseid, v001, v002, v003, v005, v024, v025, v113, v115, v116, v122, v127, v160, v465, v161, v190, v191, hw1, m19, m19a, hw2, hw3, hw4, hw5, hw6, hw7, hw8, hw9, hw10, v444a, v444, v445, v446, hw15, b4, v166, h33, h34, h42, h43, h11, h11b, h22, h31, v106, v107, v447a, v130, v131, v701, v702, v704, v714, v151, v024, sdepto, v025, v136, v137, v201, b11, bord, h2, h3, h4, h5, h6, h7, h8, h9, h10, m14, m4, m5, m39a, v409, v410, v411, v411a, v412a, v412b, v412c, v413, v413a, v413b, v414a, v414e, v414f, v414g, v414h, v414i, v414j, v414k, v414l, v414m, v414n, v414o, v414p, v414q, v414r, v414s, v414t, v414v, m39)
colnames(ChildData2)[2] <- "Cluster_ID"
colnames(ChildData2)[3] <- "Home_ID"
ChildData2$Child_ID <- with(ChildData2, paste0(Cluster_ID, sep="_", Home_ID))
#ChildData2$Mom_ID <- with(ChildData2, paste0(Cluster_ID, sep="_", Home_ID,sep="_", v107))
ChildData2$Mom_ID2 <- with(ChildData2, paste0(Cluster_ID, sep="_", Home_ID,sep="_", v107, sep="_", v106))

#Remove rows/cases if 0 is present (i.e. no children under 5 = v137)
ChildData3 <- ChildData2[!(ChildData2$v137==0),]



###Household Member Recode Import****
HHMemberData <- haven::read_dta("C:/Users/eel_v/Documents/Work/Guatemala/Guate_DHS_2015/Data/GUPR71DT_HouseholdMemberRecode/GUPR71FL.DTA")
HHMemberData2 <- select(HHMemberData, hv001, hv002, ha40, ha41, ha1, ha66, ha67)
colnames(HHMemberData2)[1] <- "Cluster_ID"
colnames(HHMemberData2)[2] <- "Home_ID"
HHMemberData2$Child_ID<- with(HHMemberData2, paste0(Cluster_ID, sep="_", Home_ID))

library(stats)
#final[complete.cases(final[ , 5:6]),]
HHMemberData3 <- HHMemberData2[complete.cases(HHMemberData2[ , 3:5]),]

HHMemberData3$Mom_ID <- with(HHMemberData3, paste0(Cluster_ID, sep="_", Home_ID, sep="_", ha67))
HHMemberData3$Mom_ID2 <- with(HHMemberData3, paste0(Cluster_ID, sep="_", Home_ID, sep="_", ha67, sep="_", ha66))



###Geographic Data Import****
GeographData <- read.csv("C:/Users/eel_v/Documents/Work/Guatemala/Guate_DHS_2015/Data/GUGC71FL_GeospatialCovariates/GUGC71FL.csv")
GeographData2 <- select(GeographData, DHSCLUST, All_Population_Density_2015, Aridity, BUILT_Population_2014, Enhanced_Vegetation_Index_2015, Growing_Season_Length, Potential_Evapotranspiration, Rainfall_2015, Temperature_Average )
colnames(GeographData2)[1] <- "Cluster_ID"

library("xlsx")
GeospatialData <- read.xlsx("C:/Users/eel_v/Documents/Work/Guatemala/Guate_DHS_2015/Data/GUGC71FL_GeospatialCovariates/GUGE71FL_lat_long.xlsx", sheetIndex = 1, header = TRUE)
GeospatialData2 <- select(GeospatialData, DHSCLUST, LATNUM, LONGNUM, ALT_DEM)
colnames(GeospatialData2)[1] <- "Cluster_ID"



###Join Dataset for Individual Child Analysis****
library(plyr)
Joined1 <- join(ChildData2, HHData2, by = c("Child_ID"), type = "left")
Joined2 <- join(Joined1, HHMemberData3, by = c("Mom_ID2"), type = "left", match = "first")
Joined3 <- join(Joined2, GeographData2, by = c("Cluster_ID"), type = "left")
Guate2015 <- join(Joined3, GeospatialData2, by = c("Cluster_ID"), type = "left")


###Join Dataset for Household Analysis**** CHANGE this so the necessary variables end up at the household level
library(plyr)
Joined1 <- join(ChildData2, HHData2, by = c("Child_ID"), type = "right")
Joined2 <- join(Joined1, GeographData2, by = c("Cluster_ID"), type = "left")
GuateHH2015 <- join(Joined2, GeospatialData2, by = c("Cluster_ID"), type = "left")




###Continued prep of Individual child analysis dataset*****
#remove unecessary variables after joining Guate2015 datasets
Guate2015_2 <- subset(Guate2015, select=-c(hv003,ha66,Child_ID,Home_ID,Mom_ID2,Cluster_ID,v003,caseid,ha1,ha67))

#rename all variables in Guate2015_2 using a key (exported above dataset, pulled variables and matched with new name using recode book)
Key2015 <- read.xlsx("C:/Users/eel_v/Documents/Work/Guatemala/Guate_DHS_2015/Variables_to_Analyze.xlsx", sheetIndex = 3, header = TRUE)
#View(Key)
names(Guate2015_2) <- Key2015$New_ID[match(names(Guate2015_2),Key2015$Variable_ID)]

#remove column NAs; triple check the columns you are removing are the NA columns
#names(Guate2015_2)                            #see what column number they are
Guate2015_3 <- Guate2015_2[, -c(126:128)]
Guate2015_3 <- Guate2015_3[, -c(123:124)]

#Remove any variables with complete missing data
Guate2015_4 <- Guate2015_3[!sapply(Guate2015_3, function (x) all(is.na(x) | x == ""))]

#Remove additional variables deemed unecessary
Guate2015_5 <- subset(Guate2015_4, select=-c(Mothers_Highest_Ed_Level, Fathers_Highest_Ed_Level, Birth_Weight_Recall, Child_Weight_Kgs, Child_Height_Cms, Child_Height_Age_Percentile, Child_Height_Age_Percent_RefMedian, Child_Weight_Age_Percent, Child_Weight_Age_Ref_Med, Child_Weight_Height_Percentile, Child_Weight_Height_Percent_RefMedian, Child_BMI, Child_Measure_Stand_Lying, Vitamin_A_Received, Guate_Region, Total_Children_Born, Birth_Order, Vacc_Ever_Had, Mothers_BMI ))


#*********ready to analyze; recode values, conduct baseline descriptives, check distributions, etc. 




###Continued prep of Household analysis dataset*****
#Remove rows/cases if 0 is present (i.e. no children under 5 = v137)
#looking at all households???
GuateHH2015_2 <- GuateHH2015
#GuateHH2015_2 <- GuateHH2015[!is.na(GuateHH2015$v025),]
names(GuateHH2015_2) <- Key2015$New_ID[match(names(GuateHH2015_2),Key2015$Variable_ID)]

#columns labeled NA
#Cols1-5: "Child_ID","caseid","Cluster_ID","Home_ID","v003","v005"
#Cols104-107: "Mom_ID2", "Cluster_ID", "Home_ID", "hv003"
names(GuateHH2015_4)

GuateHH2015_3 <- subset(GuateHH2015_2, select=-c(61:105))
GuateHH2015_4 <- subset(GuateHH2015_3, select=-c(20:45))

names(GuateHH2015_4)[1:5] <- c("Child_ID","caseid","Cluster_ID","Home_ID","Respondant_Line")
names(GuateHH2015_4)[35:36] <- c("Home_ID2", "Respondant_Line2")

#Remove any variables with complete missing data
GuateHH2015_5 <- GuateHH2015_4[!sapply(GuateHH2015_4, function (x) all(is.na(x) | x == ""))]


#*********ready to analyze; recode values, conduct baseline descriptives, check distributions, etc. 






####Guate2015 Weighting factors for dataset; if analysis is univariate, use weighting, if multivariable, don't use weighting
#instructions for DHS weighting can be found in handbook
library(foreign)
GuateData$v005 <- GuateData$v005 / 1000000
#Tabulate indicator by region
GuateData$urb_rural <-ifelse(GuateData$v025==1,1,0)   #1 = urban; 0 = rural
ddply(GuateData,~v024,summarise,mean=weighted.mean(urb_rural, v005))

install.packages("survey")
library(survey)
# Complex sample design parameters
DHSdesign<-svydesign(id=GuateData$Cluster_ID, strata=GuateData$v023, weights=GuateData$v005, data=GuateData)
# tabulate indicator by region
svyby(~urb_rural, ~v024, DHSdesign, svymean, vartype=c("se","ci"))










##***Guate-2013-WHIP-Data****************************************************************
#technically data collected from July-Dec 2013
HH13_Data <- haven::read_dta("C:/Users/eel_v/Documents/Work/Guatemala/Guate_WHIP_2013/GTM_base_hh_20180517.dta")

Child13_Health <- haven::read_dta("C:/Users/eel_v/Documents/Work/Guatemala/Guate_WHIP_2013/GTM_base_child_20180517.dta")

HHMemb13_Health <- haven::read_dta("C:/Users/eel_v/Documents/Work/Guatemala/Guate_WHIP_2013/GTM_base_hhmember_20180517.dta")

Comm13_Data <- haven::read_dta("C:/Users/eel_v/Documents/Work/Guatemala/Guate_WHIP_2013/GTM_base_community_20180517.dta")

#HH13_Weights <- haven::read_dta("C:/Users/eel_v/Documents/Work/Guatemala/Guate_WHIP_2013/GUATEMALA_weights_annotated.csv")

#make IDs to combine datasets; combine datasets based on individual children and HHs; remove unneeded variables






##***Guate-2012-FFP-Data****************************************************************
#technically data collected from Jan-July 2013
HH12_Data <- read.csv("C:/Users/eel_v/Documents/Work/Guatemala/Guate_FFP_2012/Guatemala_HH Description_Data.csv")
library(dplyr)
HH12_Data2 <- select(HH12_Data, ID, A05, A02, HOG, cluster, strata, B04, B05, B19A, member_id, HHWT, education_level)


Child12_Health <- read.csv("C:/Users/eel_v/Documents/Work/Guatemala/Guate_FFP_2012/Guatemala_ChildHealth_Data.csv")
Child12_Health2 <- select(Child12_Health, member_idunique, AT_HOG, total_members, agedays, agemos, urbanr, sex, lorh, length, weight2, CHWT, diarrhea, ORT, breastfeeding, exclusive_breast, DD1,	DD2,	DD3,	DD4,	DD5,	DD6,	DD7,	DDseven,	DDsix,	meal_frequency,	total_milk_feeds,	total_feeds,	MAD,	DDseven_rec,	DDsix_rec,	Meal_frequency_rec1,	Meal_frequency_rec2,	nonbreast_feeds,	zhaz,	zBMI,	zwaz,	zwhz)
colnames(Child12_Health2)[1] <- "member_id"


HHMom12_Health <- read.csv("C:/Users/eel_v/Documents/Work/Guatemala/Guate_FFP_2012/Guatemala_Maternal Health and HH Sanitation_Data.csv")
HHMom12_Health2 <- select(HHMom12_Health, ID, A03, A05, A02, A01, A15_3, F01A, F04, F05, F06, F07, F08, F09, F10_1, F10_2, F10_3, F10_4, F10_5, F11, F12, F13, F14, F15, F16, C16, C17, C18, C19, C20, C21, E36, E37, E38, cluster, strata, HHWT, HDDS, HHS, improved_water, improved_sanitation, soap_water)
#colnames(HHData2)[1] <- "Cluster_ID"
#colnames(HHData2)[2] <- "Home_ID"
#HHData2$Child_ID <- with(HHData2, paste0(Cluster_ID, sep="_", Home_ID))


#HH12_Weights <- read.csv("C:/Users/eel_v/Documents/Work/Guatemala/Guate_FFP_2012/GUATEMALA_weights_annotated.csv")



#Individual Dataset
library(plyr)
Joined1 <- join(Child12_Health2, HH12_Data2, by = c("member_id"), type = "left")
Guate2012 <- join(Joined1, HHMom12_Health2, by = c("ID"), type = "left")


#Household Dataset
###Join Dataset for Household Analysis**** CHANGE this so the necessary variables end up at the household level
library(plyr)
GuateHH2012 <- HHMom12_Health2



#Relabel Individual Dataset**********
#rename all variables in Guate2012 using a key (exported above dataset, pulled variables and matched with new name using recode book)
library("xlsx")
Key2012 <- read.xlsx("C:/Users/eel_v/Documents/Work/Guatemala/Guate_FFP_2012/Variable_Key2012.xlsx", sheetIndex = 1, header = TRUE)
names(Guate2012) <- Key2012$New_ID[match(names(Guate2012),Key2012$Variable_ID)]
#Remove any variables with complete missing data
Guate2012_2 <- Guate2012[!sapply(Guate2012, function (x) all(is.na(x) | x == ""))]



#Relabel Household Dataset***********
#rename all variables in Guate2012 using a key (exported above dataset, pulled variables and matched with new name using recode book)
#library("xlsx")
#Key2012 <- read.xlsx("C:/Users/eel_v/Documents/Work/Guatemala/Guate_FFP_2012/Variable_Key2012.xlsx", sheetIndex = 1, header = TRUE)
names(GuateHH2012) <- Key2012$New_ID[match(names(GuateHH2012),Key2012$Variable_ID)]
#Remove any variables with complete missing data
GuateHH2012_2 <- GuateHH2012[!sapply(GuateHH2012, function (x) all(is.na(x) | x == ""))]







##***Guate-1999-DHS-Data*****************************************************************
###Household Recode Import****
HH99Data <- haven::read_dta("C:/Users/eel_v/Documents/Work/Guatemala/Guate_DHS_1999/GUHR41DT/GUHR41FL.DTA")
HH99Data2 <- select(HH99Data, hhid,	hv001,	hv002,	hv003,	hv005,	hv012,	hv014,	hv024,	hv025,	hv201,	hv202,	hv204,	hv205,	hv213,	shdepto,	shmpio,	shaltura,	sh26,	sh28)
#colnames(HHData2)[1] <- "Cluster_ID"
#colnames(HHData2)[2] <- "Home_ID"
#HHData2$Child_ID <- with(HHData2, paste0(Cluster_ID, sep="_", Home_ID))


###Childrens Recode Import****
Child99Data <- haven::read_dta("C:/Users/eel_v/Documents/Work/Guatemala/Guate_DHS_1999/GUKR41DT/GUKR41FL.DTA")
#adjust variables
#ChildData2 <- select(ChildData, caseid, v001, v002, v003, v005, v024, v025, v113, v115, v116, v122, v127, v160, v465, v161, v190, v191, hw1, m19, m19a, hw2, hw3, hw4, hw5, hw6, hw7, hw8, hw9, hw10, v444a, v444, v445, v446, hw15, b4, v166, h33, h34, h42, h43, h11, h11b, h22, h31, v106, v107, v447a, v130, v131, v701, v702, v704, v714, v151, v024, sdepto, v025, v136, v137, v201, b11, bord, h2, h3, h4, h5, h6, h7, h8, h9, h10, m14, m4, m5, m39a, v409, v410, v411, v411a, v412a, v412b, v412c, v413, v413a, v413b, v414a, v414e, v414f, v414g, v414h, v414i, v414j, v414k, v414l, v414m, v414n, v414o, v414p, v414q, v414r, v414s, v414t, v414v, m39)
#colnames(ChildData2)[2] <- "Cluster_ID"
#colnames(ChildData2)[3] <- "Home_ID"
#ChildData2$Child_ID <- with(ChildData2, paste0(Cluster_ID, sep="_", Home_ID))
#ChildData2$Mom_ID <- with(ChildData2, paste0(Cluster_ID, sep="_", Home_ID,sep="_", v107))
#ChildData2$Mom_ID2 <- with(ChildData2, paste0(Cluster_ID, sep="_", Home_ID,sep="_", v107, sep="_", v106))

names(Child99Data)

#Remove rows/cases if 0 is present (i.e. no children under 5 = v137)
#ChildData3 <- ChildData2[!(ChildData2$v137==0),]

####import more as necessary


#adjust this import and prep as it should be the same as the 1995 survey





 
##Pick up here

#install.packages("car")
# Load the car package
library(car)
#DataSet$VariableNEW <- recode(DataSet$VariableOLD,"99=10") #change value from 99 to 10
#DataSet$VariableNEW <- recode(DataSet$VariableOLD,"99=NA") #or NA

#recode HAZ values
GuateData5$HAZ <- recode(GuateData5$Child_Height_Age_Std_Dev, "9999=NA")
GuateData5$HAZ <- recode(GuateData5$Child_Height_Age_Std_Dev, "9998=NA")

GuateData5$WHZ <- recode(GuateData5$Child_Weight_Age_Std_Dev, "9999=NA")
GuateData5$WHZ <- recode(GuateData5$Child_Weight_Age_Std_Dev, "9998=NA")

GuateData5$HAZ <- GuateData5$HAZ / 100
GuateData5$WHZ <- GuateData5$WHZ / 100




count(GuateData5, 'Livestock_Owns_Chicken')
GuateData5$Livestock_Owns_Chicken <- recode(GuateData5$Livestock_Owns_Chicken,"99=NA") #or NA




###Trim and Recode Dataset
#Remove NAs and other missing values/columns/rows
#Removing Empty Columns
#GuateData2 <- GuateData[!sapply(GuateData, function (x) all(is.na(x) | x == ""))]
#names(GuateData2)
#View(GuateData2)

#Removing Columns with >30% of Data Missing
#GuateData3 <- GuateData2[colSums(is.na(GuateData2))/nrow(GuateData2) < .3]
#names(GuateData3)
#View(GuateData3)
#names(GuateData2)
#names(GuateData3)

#investigate frequency tables for variables
#count(GuateData3, 'b4')

#write.xlsx(GuateData3, "C:/Users/eel_v/Documents/Work/Analyses/Guate_DHS_2015/Data/GuateData_D1.xlsx", sheetName = "Sheet1" )
#write.csv(GuateData, "C:/Users/eel_v/Documents/Work/Analyses/Guate_DHS_2015/Data/GuateData_D1.xlsx")




####Manipulate values in data frames
#Create smaller dataset
SubDataSet <- DataSet[,c("name of row", "name of row")]

#Rescale variables (1/2s -> 0/1s, etc.)
VariableNEW <- (DataSet$VariableOLD)-1
DataSet <- cbind(DataSet,VariableNEW)  #bind back to dataset

#Recode variables (website for this: http://rprogramming.net/recode-data-in-r/)
DataSet$VariableNEW <- recode(DataSet$VariableOLD,"99=10") #change value from 99 to 10
DataSet$VariableNEW <- recode(DataSet$VariableOLD,"99=NA") #or NA

#view variable value details (no names or lables though)
lapply(DataSet[, c(38:50)], table) #38:50 refer to location range in data frame





#Geographic Data Import: https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
#install.packages("rgdal")
#library(rgdal)
#require(rgdal)
install.packages("ggmap")
library(scales)
library(ggplot2)
library(ggthemes)
library(data.table)
library(ggmap)   ##load the ggmap package so we can access the crime data
library(plyr)

GuateData5_1 <- read.csv("C:/Users/eel_v/Documents/Work/Analyses/Guate_DHS_2015/Data/GuateData5_1.csv")

ggplot() +
  geom_point(data = GuateData5_1, aes(x = Longitude, y = Latitude), alpha = .3)

register_google(key = "AIzaSyDwaUwm8oqetkDbYUUSoYmidmlv7E-VJdk")

Guate_Map <- get_googlemap(center = c(lon = -90.315954, lat = 15.243531), zoom = 8) ##Get the Guate map
#ggmap(Guate_Map)
Guate_Map2 <-ggmap(Guate_Map, extent = "device")    

Guate_MapPoints <- ggmap(Guate_Map) + geom_point(aes(x = Longitude, y = Latitude), data = GuateData5_1, alpha = .35)
Guate_MapPoints









##regression machine learning; food type consumption - growth outcomes
#subset dataset
names(GuateData5)
GuateData_Food <- subset(GuateData5, select=c("Child_FR_Water", "Child_FR_Juice" ,"Child_FR_Milk","Child_FR_BabyFormula", "Child_FR_Fortified_BabyFood", "Child_FR_PorridgeGruel","Child_FR_Soup", "Child_FR_OtherLiquid",
                                              "Child_FR_Instant_Soup", "Child_FR_Soda", "Child_FR_Maize_based", "Child_FR_Bread_Grains", "Child_FR_Tubers" , "Child_FR_Eggs", "Child_FR_Meat ", "Child_FR_VegYellowOrange",
                                              "Child_FR_Green_Veggies", "Child_FR_FruitsVitA" , "Child_FR_OtherFruits" , "Child_FR_MeatOrgans", "Child_FR_Fish", "Child_FR_BeansPeasLentils", "Child_FR_Cheese_Yogurt", 
                                              "Child_FR_OilsFatsButter", "Child_FR_Sweets_Candies", "Child_FR_SemiSolidFood", "Child_FR_Honey_Sugar", "Child_FR_Yogurt", "HAZ", "WHZ" ))

#box plot for HAZ
boxplot(GuateData_Food$HAZ, main="HAZ", sub=paste("Outlier rows: ", boxplot.stats(GuateData_Food$HAZ)$out))


#see if HAZ is normal
install.packages("e1071")
library(e1071)
GuateData_Food2 <- GuateData_Food[!is.na(GuateData_Food$HAZ), ]
plot(density(GuateData_Food2$HAZ), main="Density Plot: HAZ", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(GuateData_Food2$HAZ), 2)))  # density plot for 'HAZ'
polygon(density(GuateData_Food2$HAZ), col="red")


#https://www.dataquest.io/blog/statistical-learning-for-predictive-modeling-r/

install.packages("GGally")
library(GGally) 
ggpairs(data=GuateData_Food2, columns= c(1,29,30), title="GuateData_Food2 data")


#build model
fit_1 <- lm(HAZ ~ Child_FR_Water, data = GuateData_Food2)

#assess summary
summary(fit_1)

#mmodel residuals
ggplot(data=trees, aes(fit_1$residuals)) +
  geom_histogram(binwidth = 1, color = "black", fill = "purple4") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line()) +
  ggtitle("Histogram for Model Residuals")


