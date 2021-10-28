# Preparing the Workspace

rm(list = ls()) #Clear objects from environment
cat("\014") #Clear console
setwd("~/Desktop/IST 707 Data Analytics/Project/FINAL FILES") #Set working directory

# Loading Packages

EnsurePackage <- function(myPackage)
{
  myPackage <- as.character(myPackage)
  if (!require(myPackage, character.only = TRUE))
  {
    install.packages(pkgs=myPackage,repos="http://cran.r-project.org")
    require(myPackage,character.only=TRUE)
  }
}
EnsurePackage("countrycode")
EnsurePackage("ggplot2")
EnsurePackage("randomForest")
EnsurePackage("sqldf")
EnsurePackage("lubridate") #
EnsurePackage("dplyr")
EnsurePackage("plyr")
EnsurePackage("tidyverse")
EnsurePackage("data.table")
EnsurePackage("cluster")
EnsurePackage("psych")
EnsurePackage("doParallel")
EnsurePackage("klaR")
EnsurePackage("rattle")
EnsurePackage("parallel")
EnsurePackage("kernlab")
EnsurePackage("caret")
EnsurePackage("caretEnsemble")
EnsurePackage("e1071") 
EnsurePackage("rpart")
EnsurePackage("rpart.plot")
EnsurePackage("factoextra")
EnsurePackage("hrbrthemes")
EnsurePackage("viridis")
EnsurePackage("forcats")

# Distribute processing work over CPU cores
starTime <- Sys.time()
print(detectCores()) # Prints the total CPU cores available
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Load Data

urlToRead <- "http://registration.baa.org/2019/cf/Media/Runners_all_results.csv"
BAA2019Results <- read.csv(url(urlToRead),na.strings = c(""))
str(BAA2019Results)

#Remove unnecessary columns, preliminary
InitialColumnRemoval <- c("AgeOnRaceDay","Gender","CountryOfResName","CountryOfCtzName","OfficialTime","RankOverall")
BAA2019Results <- BAA2019Results[InitialColumnRemoval]
str(BAA2019Results)

#NAs
sum(is.na(BAA2019Results))


# Clean up data entry error. 
# Corrected by referencing: http://registration.baa.org/cfm_Archive/iframe_ArchiveSearch.cfm?mode=results&RequestTimeout=600&snap=64065302&
unique(BAA2019Results$Gender)
length(unique(BAA2019Results$Gender))
BAA2019Results[BAA2019Results$Gender=="Leighton Buzzard",]
BAA2019Results$Gender[26143]="F"
BAA2019Results$OfficialTime[26143]="6:04:34"
BAA2019Results$RankOverall[26143]="26143"
BAA2019Results$CountryOfCtzName[26143]="United Kingdom"
BAA2019Results$AgeOnRaceDay[26143]="62"
BAA2019Results[BAA2019Results$BibNumber=='28913',]
table(BAA2019Results$Gender)
barplot(table(BAA2019Results$Gender), col = c("#FFDD00", "#00478D"), main="Number of Runners by Gender")

# Gender finish times 
hist(BAA2019Results$OTSeconds[BAA2019Results$Gender == "M"], col = "#00478D", border= "#FFDD00", main = "Distribution of Finishing Times | Males")
hist(BAA2019Results$OTSeconds[BAA2019Results$Gender == "F"], col = "#00478D", border= "#FFDD00", main = "Distribution of Finishing Times | Females")

# Convert Finish Time from HH:MM:SS format to number of seconds so that it is numeric continuous variable
BAA2019Results$OTSeconds <- BAA2019Results$OfficialTime
BAA2019Results$OTSeconds <-hms(BAA2019Results$OTSeconds)
BAA2019Results$OTSeconds <- as.numeric(BAA2019Results$OTSeconds)
summary(BAA2019Results$OTSeconds)
hist(BAA2019Results$OTSeconds, col = "#00478D", border= "#FFDD00", main = "Distribution of Finish Time | All Runners")

# Pace in Seconds - I thought I might use Pace, but it's a derivative variable from OfficialTime and doesn't add much value to models. 
#It's typical for runners to speak in terms of "Pace" but it's only interesting if data had information about splits, in which TS analysis could be possible, but it is lacking.
#BAA2019Results$PaceSeconds <- BAA2019Results$OTSeconds/26
#summary(BAA2019Results$PaceSeconds)
#hist(BAA2019Results$PaceSeconds, col = "#FFDD00")

# Country Mismatches
unique(BAA2019Results$CountryOfResName)
length(unique(BAA2019Results$CountryOfResName))
unique(BAA2019Results$CountryOfCtzName)
length(unique(BAA2019Results$CountryOfCtzName))
CountryMismatches <- sqldf("select count(*), CountryOfResName, CountryOfCtzName from BAA2019Results where CountryOfResName != CountryOfCtzName group by CountryOfResName")
CountryMismatches <- CountryMismatches[order(-CountryMismatches$`count(*)`),]
CountryMismatches

# Make Regional Variable from Country Code
BAA2019Results$Region <- countrycode(BAA2019Results$CountryOfCtzName, 
                                     "country.name", "region")
#par(mar=c(11,4,4,4))
#barplot(table(BAA2019Results$Region), col = "#00478D", border= "#FFDD00", horiz = F, las=2)
(table(BAA2019Results$Region))

histRegion <- BAA2019Results %>%
  mutate(text = fct_reorder(Region, OTSeconds)) %>%
  ggplot( aes(x=OTSeconds, color=Region, fill=Region)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Finish Times by Region") +
  facet_wrap(~Region)

histRegion

# Make 25% cuts from AgeOnRaceDay as potential alternative to Generation in case of skew
BAA2019Results$AgeOnRaceDay <- as.numeric(BAA2019Results$AgeOnRaceDay)
BAA2019Results$AgeQuartiles <- cut(BAA2019Results$AgeOnRaceDay,breaks = 4,labels = c("Youngest","Second Youngest","Older","Oldest"))
summary(BAA2019Results$AgeOnRaceDay[(BAA2019Results$AgeQuartiles=="Youngest")])
summary(BAA2019Results$AgeOnRaceDay[(BAA2019Results$AgeQuartiles=="Second Youngest")])
summary(BAA2019Results$AgeOnRaceDay[(BAA2019Results$AgeQuartiles=="Older")])
summary(BAA2019Results$AgeOnRaceDay[(BAA2019Results$AgeQuartiles=="Oldest")])

table(BAA2019Results$AgeQuartiles)

histByAge <- BAA2019Results %>%
  mutate(text = fct_reorder(AgeQuartiles, OTSeconds)) %>%
  ggplot( aes(x=OTSeconds, color=AgeQuartiles, fill=AgeQuartiles)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Finish Times by Age Group") +
  facet_wrap(~AgeQuartiles)

histByAge

# Make Categorical Variable from 25% cuts of RankOrder
BAA2019Results$RankOverall <- as.numeric(BAA2019Results$RankOverall)
BAA2019Results$RankOrder <- cut(BAA2019Results$RankOverall,breaks = 4, labels = c("Top 25%","Upper Middle 25%","Lower Middle 25%","Bottom 25%"))
hist(BAA2019Results$OTSeconds[(BAA2019Results$RankOrder=="Top 25%")],col = "#00478D", border= "#FFDD00")
hist(BAA2019Results$OTSeconds[(BAA2019Results$RankOrder=="Upper Middle 25%")],col = "#00478D", border= "#FFDD00")
hist(BAA2019Results$OTSeconds[(BAA2019Results$RankOrder=="Lower Middle 25%")],col = "#00478D", border= "#FFDD00")
hist(BAA2019Results$OTSeconds[(BAA2019Results$RankOrder=="Bottom 25%")],col = "#00478D", border= "#FFDD00")

table(BAA2019Results$RankOrder)

# Qualifying Times Data Frame
Age_Group <- c("18-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80 and Over")
MenQT <- c("3:00:00","3:05:00","3:10:00","3:20:00","3:25:00","3:35:00","3:50:00","4:05:00","4:20:00","4:35:00","4:50:00")
WomenQT <- c("3:30:00","3:35:00","3:40:00","3:50:00","3:55:00","4:05:00","4:20:00","4:35:00","4:50:00","5:05:00","5:50:00")
BAA2019QT <- data.frame(MenQT,WomenQT, row.names = Age_Group)
BAA2019QT$MenQTSeconds <- hms(BAA2019QT$MenQT)
BAA2019QT$MenQTSeconds <- as.numeric(BAA2019QT$MenQTSeconds)
BAA2019QT$WomenQTSeconds <- hms(BAA2019QT$WomenQT)
BAA2019QT$WomenQTSeconds <- as.numeric(BAA2019QT$WomenQTSeconds)

#Qualifyingg Times by Gender/Age Group Each As Variable
M1834QT <- BAA2019QT[1,3]
M3539QT <- BAA2019QT[2,3]
M4044QT <- BAA2019QT[3,3]
M4549QT <- BAA2019QT[4,3]
M5054QT <- BAA2019QT[5,3]
M5559QT <- BAA2019QT[6,3]
M6064QT <- BAA2019QT[7,3]
M6569QT <- BAA2019QT[8,3]
M7074QT <- BAA2019QT[9,3]
M7579QT <- BAA2019QT[10,3]
M80PLUSQT <- BAA2019QT[11,3]
F1834QT <- BAA2019QT[1,4]
F3539QT <- BAA2019QT[2,4]
F4044QT <- BAA2019QT[3,4]
F4549QT <- BAA2019QT[4,4]
F5054QT <- BAA2019QT[5,4]
F5559QT <- BAA2019QT[6,4]
F6064QT <- BAA2019QT[7,4]
F6569QT <- BAA2019QT[8,4]
F7074QT <- BAA2019QT[9,4]
F7579QT <- BAA2019QT[10,4]
F80PLUSQT <- BAA2019QT[11,4]

#Make AgeOnRaceDay Numeric

BAA2019Results$QTClass <- ""

#Create Classification Variable of Each QT Group by Gender/Age
BAA2019Results$QTClass[(BAA2019Results$Gender=='M' & BAA2019Results$AgeOnRaceDay>=18 & BAA2019Results$AgeOnRaceDay<=34)] <- "M1834"
BAA2019Results$QTClass[(BAA2019Results$Gender=='M' & BAA2019Results$AgeOnRaceDay>=35 & BAA2019Results$AgeOnRaceDay<=39)] <- "M3539"
BAA2019Results$QTClass[(BAA2019Results$Gender=='M' & BAA2019Results$AgeOnRaceDay>=40 & BAA2019Results$AgeOnRaceDay<=44)] <- "M4044"
BAA2019Results$QTClass[(BAA2019Results$Gender=='M' & BAA2019Results$AgeOnRaceDay>=45 & BAA2019Results$AgeOnRaceDay<=49)] <- "M4549"
BAA2019Results$QTClass[(BAA2019Results$Gender=='M' & BAA2019Results$AgeOnRaceDay>=50 & BAA2019Results$AgeOnRaceDay<=54)] <- "M5054"
BAA2019Results$QTClass[(BAA2019Results$Gender=='M' & BAA2019Results$AgeOnRaceDay>=55 & BAA2019Results$AgeOnRaceDay<=59)] <- "M5559"
BAA2019Results$QTClass[(BAA2019Results$Gender=='M' & BAA2019Results$AgeOnRaceDay>=60 & BAA2019Results$AgeOnRaceDay<=64)] <- "M6064"
BAA2019Results$QTClass[(BAA2019Results$Gender=='M' & BAA2019Results$AgeOnRaceDay>=65 & BAA2019Results$AgeOnRaceDay<=69)] <- "M6569"
BAA2019Results$QTClass[(BAA2019Results$Gender=='M' & BAA2019Results$AgeOnRaceDay>=70 & BAA2019Results$AgeOnRaceDay<=74)] <- "M7074"
BAA2019Results$QTClass[(BAA2019Results$Gender=='M' & BAA2019Results$AgeOnRaceDay>=75 & BAA2019Results$AgeOnRaceDay<=79)] <- "M7579"
BAA2019Results$QTClass[(BAA2019Results$Gender=='M' & BAA2019Results$AgeOnRaceDay>=80)] <- "M80PLUS"
BAA2019Results$QTClass[(BAA2019Results$Gender=='F' & BAA2019Results$AgeOnRaceDay>=18 & BAA2019Results$AgeOnRaceDay<=34)] <- "F1834"
BAA2019Results$QTClass[(BAA2019Results$Gender=='F' & BAA2019Results$AgeOnRaceDay>=35 & BAA2019Results$AgeOnRaceDay<=39)] <- "F3539"
BAA2019Results$QTClass[(BAA2019Results$Gender=='F' & BAA2019Results$AgeOnRaceDay>=40 & BAA2019Results$AgeOnRaceDay<=44)] <- "F4044"
BAA2019Results$QTClass[(BAA2019Results$Gender=='F' & BAA2019Results$AgeOnRaceDay>=45 & BAA2019Results$AgeOnRaceDay<=49)] <- "F4549"
BAA2019Results$QTClass[(BAA2019Results$Gender=='F' & BAA2019Results$AgeOnRaceDay>=50 & BAA2019Results$AgeOnRaceDay<=54)] <- "F5054"
BAA2019Results$QTClass[(BAA2019Results$Gender=='F' & BAA2019Results$AgeOnRaceDay>=55 & BAA2019Results$AgeOnRaceDay<=59)] <- "F5559"
BAA2019Results$QTClass[(BAA2019Results$Gender=='F' & BAA2019Results$AgeOnRaceDay>=60 & BAA2019Results$AgeOnRaceDay<=64)] <- "F6064"
BAA2019Results$QTClass[(BAA2019Results$Gender=='F' & BAA2019Results$AgeOnRaceDay>=65 & BAA2019Results$AgeOnRaceDay<=69)] <- "F6569"
BAA2019Results$QTClass[(BAA2019Results$Gender=='F' & BAA2019Results$AgeOnRaceDay>=70 & BAA2019Results$AgeOnRaceDay<=74)] <- "F7074"
BAA2019Results$QTClass[(BAA2019Results$Gender=='F' & BAA2019Results$AgeOnRaceDay>=75 & BAA2019Results$AgeOnRaceDay<=79)] <- "F7579"
BAA2019Results$QTClass[(BAA2019Results$Gender=='F' & BAA2019Results$AgeOnRaceDay>=80)] <- "F80PLUS"

table(BAA2019Results$QTClass)

BAA2019Results$AchievedQT <- "No"

BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="M1834"&BAA2019Results$OTSeconds>=M1834QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="M3539"&BAA2019Results$OTSeconds>=M3539QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="M4044"&BAA2019Results$OTSeconds>=M4044QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="M4549"&BAA2019Results$OTSeconds>=M4549QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="M5054"&BAA2019Results$OTSeconds>=M5054QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="M5559"&BAA2019Results$OTSeconds>=M5559QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="M6064"&BAA2019Results$OTSeconds>=M6064QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="M6569"&BAA2019Results$OTSeconds>=M6569QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="M7074"&BAA2019Results$OTSeconds>=M7074QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="M7579"&BAA2019Results$OTSeconds>=M7579QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="M80PLUS"&BAA2019Results$OTSeconds>=M80PLUSQT)] <- "Yes"

BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="F1834"&BAA2019Results$OTSeconds>=F1834QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="F3539"&BAA2019Results$OTSeconds>=F3539QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="F4044"&BAA2019Results$OTSeconds>=F4044QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="F4549"&FAA2019Results$OTSeconds>=F4549QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="F5054"&BAA2019Results$OTSeconds>=F5054QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="F5559"&BAA2019Results$OTSeconds>=F5559QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="F6064"&BAA2019Results$OTSeconds>=F6064QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="F6569"&BAA2019Results$OTSeconds>=F6569QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="F7074"&BAA2019Results$OTSeconds>=F7074QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="F7579"&BAA2019Results$OTSeconds>=F7579QT)] <- "Yes"
BAA2019Results$AchievedQT[(BAA2019Results$QTClass=="F80PLUS"&BAA2019Results$OTSeconds>=F80PLUSQT)] <- "Yes"

table(BAA2019Results$AchievedQT)

barplot(table(BAA2019Results$AchievedQT), col = c("#FFDD00", "#00478D"),main="Number of Runners Who Achieved QT vs Didn't")

# Visualizations

FTGenBox <- BAA2019Results %>%
  ggplot( aes(x=Gender, y=OTSeconds, fill=Gender)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Finish Time by Gender") +
  xlab("")

FTRegBox <- BAA2019Results %>%
  ggplot( aes(x=Region, y=OTSeconds, fill=Region)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Finish Time by Region") +
  xlab("")

FTAgeBox <- BAA2019Results %>%
  ggplot( aes(x=AgeQuartiles, y=OTSeconds, fill=AgeQuartiles)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Finish Time by Age Group") +
  xlab("")

FTGenBox
FTRegBox
FTAgeBox


# Evaluate Data
str(BAA2019Results)

# Pair down columns to those needed
Final <- c("OTSeconds","RankOrder","QTClass","AchievedQT","AgeOnRaceDay","AgeQuartiles","Gender","Region")
BAA2019FinalDF <- BAA2019Results[Final]
FactorCols <- c("Gender", "Region","QTClass","AchievedQT","AgeQuartiles") 
BAA2019FinalDF[FactorCols] <- lapply(BAA2019FinalDF[FactorCols], factor) #Make Gender and Region Factors
str(BAA2019FinalDF)


# Visualizations by Region - Included in Code but not in Analysis to prevent over-analysis

# Create Region DFs
SAfr <- sqldf("select * from BAA2019Results where Region == 'Sub-Saharan Africa'") 
hist(SAfr$OTSeconds)
boxplot(OTSeconds~Gender,data = SAfr)
boxplot(OTSeconds~AgeQuartiles,data = SAfr)
EAsPac <- sqldf("select * from BAA2019Results where Region == 'East Asia & Pacific'") 
hist(EAsPac$OTSeconds)
boxplot(OTSeconds~Gender,data = EAsPac)
boxplot(OTSeconds~AgeQuartiles,data = EAsPac)
EurAs <- sqldf("select * from BAA2019Results where Region == 'Europe & Central Asia'")
hist(EurAs$OTSeconds)
boxplot(OTSeconds~Gender,data = EurAs)
boxplot(OTSeconds~AgeQuartiles,data = EurAs)
SAsia <- sqldf("select * from BAA2019Results where Region == 'South Asia'")
hist(SAsia$OTSeconds)
boxplot(OTSeconds~Gender,data = SAsia)
boxplot(OTSeconds~AgeQuartiles,data = SAsia)
NAmer <- sqldf("select * from BAA2019Results where Region == 'North America'") 
hist(NAmer$OTSeconds)
boxplot(OTSeconds~Gender,data = NAmer)
boxplot(OTSeconds~AgeQuartiles,data = NAmer)
MENAfr <- sqldf("select * from BAA2019Results where Region == 'Middle East & North Africa'")
hist(MENAfr$OTSeconds)
boxplot(OTSeconds~Gender,data = MENAfr)
boxplot(OTSeconds~AgeQuartiles,data = MENAfr)
LATAmer <- sqldf("select * from BAA2019Results where Region == 'Latin America & Caribbean'")
hist(LATAmer$OTSeconds)
boxplot(OTSeconds~Gender,data = LATAmer)
boxplot(OTSeconds~AgeQuartiles,data = LATAmer)

## End Data Exploration

## First Modeling

# Create clean data frames of factor variables that are independent

ForModels1 <- c("AchievedQT","RankOrder","AgeQuartiles","Gender","Region")
BAA2019ModelsDF <- BAA2019FinalDF[ForModels1]
table(BAA2019ModelsDF$AchievedQT)
(YesProb <- 16535/sum(table(BAA2019ModelsDF$AchievedQT)))
(NoProb <- 10122/sum(table(BAA2019ModelsDF$AchievedQT)))

# Approximate 62/38% skew towards Yes

# Randomize row order and create samples for training/testing set based on 70/30 df split

set.seed(123)
ind <- sample(nrow(BAA2019ModelsDF), replace = T)
BAA2019_1 <- BAA2019ModelsDF[ind,]

dt = sort(sample(nrow(BAA2019_1), nrow(BAA2019_1)*.7))
train<-BAA2019_1[dt,]
test<-BAA2019_1[-dt,]

# Creating a control with cross validation starting at 10
control <- trainControl(method ='cv',number = 10)
metric <-  "Accuracy" # Metric for comparison will be accuracy for this project

## Train models to predict whether Runner Achieved QT

tree.model1 <- train(AchievedQT ~ ., data = train, method="rpart", metric=metric, trControl=control,
                    tuneLength = 10) # Decision Tree

svm.model1 <- train(AchievedQT ~ ., data = train, method="svmRadial",metric=metric,trControl=control,
                   tuneLength = 10) # Support Vector Machine (SVM)

rf.model1 <- train(AchievedQT ~ ., data = train, method="rf", metric=metric, trControl=control,
                  tuneLength = 10) # Random Forest

# summarize accuracy of models
results1 <- resamples(list(Decision_Tree=tree.model1,
                          SVM=svm.model1,Random_Forest=rf.model1))

dotplot(results1)

# Evaluate Accuracy of each Model

print(tree.model1)

tree1_acc <- 80.57767
Accuracy <- data.frame(tree1_acc)
plot(tree.model1) 

print(svm.model1)
svm_acc <- 80.38475
Accuracy <- data.frame(cbind(Accuracy,svm_acc))
plot(svm.model1)

print(rf.model1)
rf_acc <- 80.58849
Accuracy <- data.frame(cbind(Accuracy,rf_acc))
plot(rf.model1)

table(Accuracy)

# Predictions

dt <- predict(tree.model1, test)
prediction <- data.frame(dt)

# SVM
svm <- predict(svm.model1, test)
prediction <- data.frame(cbind(prediction, svm))

# RF
random_f <- predict(rf.model1, test)
prediction <- data.frame(cbind(prediction, random_f))

predictionTest <- data.frame(cbind(prediction, test$AchievedQT))
head(predictionTest,10)

(DTCM <- confusionMatrix(test$AchievedQT,predictionTest$dt))
(SVMCM <- confusionMatrix(test$AchievedQT,predictionTest$svm))
(RFCM <- confusionMatrix(test$AchievedQT,predictionTest$random_f))

plot(DTCM$table)
plot(SVMCM$table)
plot(RFCM$table)

# Creating a control with cross validation this time at 3
control3 <- trainControl(method ='cv',number = 3)

## Train models to predict whether Runner Achieved QT

tree.model2 <- train(AchievedQT ~ ., data = train, method="rpart", metric=metric, trControl=control3,
                     tuneLength = 5) # Decision Tree

svm.model2 <- train(AchievedQT ~ ., data = train, method="svmRadial",metric=metric,trControl=control3,
                    tuneLength = 5) # Support Vector Machine (SVM)

rf.model2 <- train(AchievedQT ~ ., data = train, method="rf", metric=metric, trControl=control3,
                   tuneLength = 5) # Random Forest

# summarize accuracy of models
results2 <- resamples(list(Decision_Tree=tree.model2,
                           SVM=svm.model2,Random_Forest=rf.model2))

dotplot(results2)

# Evaluate Accuracy of each Model

print(tree.model2)

tree2_acc <- 80.36337
Accuracy2 <- data.frame(tree2_acc)
plot(tree.model2) 

print(svm.model2)
svm2_acc <- 80.36337
Accuracy2 <- data.frame(cbind(Accuracy2,svm2_acc))
plot(svm.model2)

print(rf.model2)
rf2_acc <- 80.37409
Accuracy2 <- data.frame(cbind(Accuracy2,rf2_acc))
plot(rf.model2)

table(Accuracy2)

# Predictions

dt2 <- predict(tree.model2, test)
prediction2 <- data.frame(dt2)

# SVM
svm2 <- predict(svm.model2, test)
prediction2 <- data.frame(cbind(prediction2, svm2))

# RF
random_f2 <- predict(rf.model2, test)
prediction2 <- data.frame(cbind(prediction2, random_f2))

predictionTest2 <- data.frame(cbind(prediction2, test$AchievedQT))
head(predictionTest2, 10)

(DT2CM <- confusionMatrix(test$AchievedQT,predictionTest2$dt))
(SVM2CM <- confusionMatrix(test$AchievedQT,predictionTest2$svm))
(RF2CM <- confusionMatrix(test$AchievedQT,predictionTest2$random_f))

plot(DT2CM$table)
plot(SVM2CM$table)
plot(RF2CM$table)