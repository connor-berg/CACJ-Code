################
#              #
#     CACJ     #
#    Courts    #
#              #
################



####################
#                  #
#  Load Libraries  #
#                  #
####################

library(tidyverse)
library(lme4)
library(lmerTest)
library(nlme)
library(emmeans)
library(ggmosaic)
library(ggplot2)
library(GGally)
library(dplyr)
library(cowplot)
library(visdat) 
library(readxl)
library(MASS)
library(car)
library(ggeffects)
library(nortest)
library(sjPlot)
library(emmeans)



#################
#               #
#   Load Data   #
#               #
#################

demographicInfo <- read.csv("AllParticipants_ParticipantProgramIntake.csv")
asam <- read.csv("ASAM.csv")
substanceAssess <- read.csv("Assessment.csv")
military <- read.csv("MilitaryService.csv")
diagnosis <- read.csv("PrimaryDiagnosis.csv")
drugChoice <- read.csv("PrimaryDrugOfChoice.csv")



##################
#                #
#   Merge Files  #
#                #
##################

cacjData_list <- list(demographicInfo,asam,substanceAssess,military,diagnosis,drugChoice)
cacjData <- Reduce(function(x,y) merge(x,y,by="RID",all=T),cacjData_list)



#####################
#                   #
#   Data Cleaning   #
#                   #
#####################

# remove participants with age < 16
for (i in 1:nrow(cacjData)) {
  if (!is.na(cacjData$Age.at.Acceptance[i]) && cacjData$Age.at.Acceptance[i] < 16) {
    cacjData$Age.at.Acceptance[i] <- NA
  }
}

# remove duplicate columns
cacjData <- cacjData[,!duplicated(names(cacjData))]

# remove other columns and rename
cacjData$PROGRAM_ID.y <- NULL
colnames(cacjData)[2] <- "PROGRAM_ID"

# remove non-binary and trans 
for (i in 1:nrow(cacjData)) {
  if (!is.na(cacjData$GENDER[i]) && (cacjData$GENDER[i] == 'non-binary'|cacjData$GENDER[i] == 'transgender')) {
    cacjData$GENDER[i] = NA
  }
}



#################
#               #
#  Create  New  #
#   Variables   #
#               #
#################

# create Success column based on EXIT_STATUS
cacjData <- cacjData %>%
  mutate(
    Success = case_when(
      EXIT_STATUS %in% c('Completed', 'Graduated') ~ 'Completed',
      !is.na(EXIT_STATUS) ~ 'Not-Completed',
      TRUE ~ NA_character_ # Assign NA for missing EXIT_STATUS
    )
  )

# DURATION: Contains the amount of time between acceptance and exit dates
  # make columns from character to date
cacjData$ACCEPTANCE_DATE <- as.Date(cacjData$ACCEPTANCE_DATE,format="%m/%d/%Y")
cacjData$EXIT_DATE <- as.Date(cacjData$EXIT_DATE,format="%m/%d/%Y")
  # make 'duration' column
cacjData$DURATION <- as.numeric(cacjData$EXIT_DATE-cacjData$ACCEPTANCE_DATE)

# Remove rows with negative or excessive DURATION
cacjData <- cacjData %>% filter(DURATION>=0) %>% filter(DURATION<1000)

# FISCAL: Contains the fiscal year in which the participant exited the program
cacjData <- cacjData %>%
  mutate(FISCAL=case_when(
    EXIT_DATE>= as.Date("2021-07-01") & EXIT_DATE <= as.Date("2022-06-30") ~ 2021,
    EXIT_DATE >= as.Date("2022-07-01") & EXIT_DATE <= as.Date("2023-06-30") ~ 2022,
    EXIT_DATE >= as.Date("2023-07-01") & EXIT_DATE <= as.Date("2024-06-30") ~ 2023,
    #TRUE~NA_real_
  ) 
  %>% as.factor())
cacjData <- cacjData %>% filter(!is.na(FISCAL)) #removes participants who are not in any of the three fiscal years

# Removes rows if certain variables are missing
columns_to_check = c('GENDER','RACE','COURT_TYPE','EXIT_STATUS', 'Age.at.Acceptance','DURATION')
cacjData = cacjData[complete.cases(cacjData[,columns_to_check]), ]



##################
#                #
#   Reclassify   #
#   Variables    #
#                #
##################

# Reclassify RACE 
cacjData <- cacjData %>% 
  mutate(RACE = case_when(RACE == "Caucasian/White" ~ "White", 
                          RACE == "Black/African American" ~ "Black",
                          TRUE ~ "Other"))

# Reclassify GENDER
cacjData <- cacjData %>% filter(GENDER %in% c("male","female"))


# Reclassify EDUCATION_LEVEL
cacjData <- cacjData %>%
  mutate(EDUCATION_LEVEL_GROUPED = case_when(
    EDUCATION_LEVEL %in% c("high school/GED", "some college") ~ "High School/Some College",
    EDUCATION_LEVEL %in% c("associates degree", "bachelors degree") ~ "Associates/Bachelors Degree",
    EDUCATION_LEVEL == "professional or graduate degree" ~ "Professional/Graduate Degree",             #no participants in data had data here
    EDUCATION_LEVEL %in% c("some high", "middle", "elementary") ~ "Other",
    TRUE~"Other"
  ))

# Reclassify ENTRY_EMPLOYMENT
cacjData <- cacjData %>%
  mutate(ENTRY_EMPLOYMENT_GROUPED = case_when(
    ENTRY_EMPLOYMENT %in% c("part-time more than 20 hours","part-time less than 20 hours","student/training")~"part-time",
    ENTRY_EMPLOYMENT == "full-time"~"full-time",
    ENTRY_EMPLOYMENT == "unemployed"~"unemployed",
    ENTRY_EMPLOYMENT %in% c("disability","retired","vocational rehab")~"Other",
    TRUE~"Other"
  ))

# Reclassify EXIT_EMPLOYMENT
cacjData <- cacjData %>%
  mutate(EXIT_EMPLOYMENT_GROUPED = case_when(
    EXIT_EMPLOYMENT %in% c("part-time more than 20 hours","part-time less than 20 hours","student/training")~"part-time",
    EXIT_EMPLOYMENT == "full-time"~"full-time",
    EXIT_EMPLOYMENT == "unemployed"~"unemployed",
    EXIT_EMPLOYMENT %in% c("disability","retired","vocational rehab")~"Other",
    TRUE~"Other"
  ))



################################################################################

#################
#               #
#   Aggregate   #
#   Functions   #
#               #
#################

# Regular Aggregate Function
aggregate_func <- function(DATA){
  DATA %>% group_by(PROGRAM_ID) %>%
    summarise(numParticipants=n(), courtType = unique(COURT_TYPE),
              meanAge = mean(Age.at.Acceptance),
              propMale = mean(as.numeric(GENDER=="male")),
              
              propWhite = mean(as.numeric(RACE=="White")),
              propBlack = mean(as.numeric(RACE=="Black")),
              
              
              propCompleted = mean(as.numeric(Success=="Completed")),
              propNotCompleted = mean(as.numeric(Success=="Not-Completed")),
              
              meanDuration = mean(DURATION),
              
              propHighSchool = mean(as.numeric(EDUCATION_LEVEL_GROUPED=="High School/Some College")),
              propCollege = mean(as.numeric(EDUCATION_LEVEL_GROUPED=="Associates/Bachelors Degree")),
              
              propEntryFullTime = mean(as.numeric(ENTRY_EMPLOYMENT_GROUPED=="full-time")),
              propEntryPartTime = mean(as.numeric(ENTRY_EMPLOYMENT_GROUPED=="part-time")),
              propEntryUnemployed = mean(as.numeric(ENTRY_EMPLOYMENT_GROUPED=="unemployed")),
              
              propExitFullTime = mean(as.numeric(EXIT_EMPLOYMENT_GROUPED=="full-time")),
              propExitPartTime = mean(as.numeric(EXIT_EMPLOYMENT_GROUPED=="part-time")),
              propExitUnemployed = mean(as.numeric(EXIT_EMPLOYMENT_GROUPED=="unemployed"))
    )
}

# Fiscal Year Aggregate Function
aggregate_funcFISC <- function(DATA){
  DATA %>% group_by(PROGRAM_ID,FISCAL) %>%
    summarise(numParticipants=n(), courtType = unique(COURT_TYPE),
              meanAge = mean(Age.at.Acceptance),
              propMale = mean(as.numeric(GENDER=="male")),
              
              propWhite = mean(as.numeric(RACE=="White")),
              propBlack = mean(as.numeric(RACE=="Black")),
              
              propGraduate = mean(as.numeric(Success=="Completed")),
              propDischarge = mean(as.numeric(Success=="Not-Completed")),
              
              meanDuration = mean(DURATION),
              
              propHighSchool = mean(as.numeric(EDUCATION_LEVEL_GROUPED=="High School/Some College")),
              propCollege = mean(as.numeric(EDUCATION_LEVEL_GROUPED=="Associates/Bachelors Degree")),
              
              propEntryFullTime = mean(as.numeric(ENTRY_EMPLOYMENT_GROUPED=="full-time")),
              propEntryPartTime = mean(as.numeric(ENTRY_EMPLOYMENT_GROUPED=="part-time")),
              propEntryUnemployed = mean(as.numeric(ENTRY_EMPLOYMENT_GROUPED=="unemployed")),
              
              propExitFullTime = mean(as.numeric(EXIT_EMPLOYMENT_GROUPED=="full-time")),
              propExitPartTime = mean(as.numeric(EXIT_EMPLOYMENT_GROUPED=="part-time")),
              propExitUnemployed = mean(as.numeric(EXIT_EMPLOYMENT_GROUPED=="unemployed"))
    )}
  
# Fit Aggregated
cacjDataAgg <- aggregate_func(cacjData)

################################################################################



###############
#             #
#  EDA Plots  #
#             #
###############

#  Violin Plots of Duration
#   Participant Level
ggplot(cacjData)+
  geom_violin(aes(x=DURATION,y=COURT_TYPE,fill=COURT_TYPE),alpha=.5)+
  guides(alpha="none")+
  geom_boxplot(aes(x=DURATION,y=COURT_TYPE,fill=COURT_TYPE,alpha=.5),width=.2)+
  theme(legend.position="none")+
  labs(title="Violin Plot of Days in Program by Court Type (Participant Level)",x="DURATION (days)",y="COURT TYPE")

#   Court Level (Duration/Court Type)
ggplot(cacjDataAgg)+
  geom_violin(aes(x=meanDuration,y=courtType,fill=courtType),alpha=.5)+
  guides(alpha="none")+
  geom_boxplot(aes(x=meanDuration,y=courtType,fill=courtType,alpha=.5),width=.2)+
  theme(legend.position="none")+
  labs(title="Figure 1. Violin Plot of Average Duration in Program by Court Type\n(Court Level)",x="Average Duration (days)",y="Court Type")

#   Court Level (Duration/Exit Status)
ggplot(cacjData)+
  geom_violin(aes(x=DURATION,y=EXIT_STATUS,fill=EXIT_STATUS),alpha=.5)+
  guides(alpha="none")+
  geom_boxplot(aes(x=DURATION,y=EXIT_STATUS,fill=EXIT_STATUS,alpha=.5),width=.2,)+
  theme(legend.position="none")+
  labs(title="Figure 2. Violin Plot of Average Duration in Program by Exit Status",x="Average Duration (days)",y="Exit Status")

# Mosaic Plot of Success Rate by Court Type
ggplot(data = cacjData)+
  geom_mosaic(aes(x = product(COURT_TYPE), fill = Success))+
  labs(title = "Figure 3. Mosaic Plot of Success Rates by Court Type",x = "Court Type",y = "Program Completion",fill = "Program Status")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Mosaic plot of Gender by Court Type
ggplot(data = cacjData)+
  geom_mosaic(mapping = aes(x = product(COURT_TYPE), fill = GENDER))+
  labs(title = "Figure 4. Mosaic Plot of Gender by Court Type",x = "Court Type",y = "Gender", fill= 'Gender')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Mosaic Plot of Race by Court Type
ggplot(data = cacjData) +
  geom_mosaic(mapping = aes(x = product(COURT_TYPE), fill = RACE)) +
  scale_fill_discrete(breaks=c("White","Other","Black"))+
  labs(
    title = "Figure 5. Distribution of Race by Court Type", x = "Court Type", y = "Race",fill = "Race") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



########################
#                      #
#   Add Program Data   #
#                      #
########################

Programs <- read.csv("programs.csv")
Programs <- Programs %>% dplyr::select(PROGRAM_ID,AveragePop)

cacjData_list <- list(cacjDataAgg,Programs)
cacjDataAgg <- Reduce(function(x,y) merge(x,y,by="PROGRAM_ID",all=T),cacjData_list)
cacjDataAgg <- cacjDataAgg %>% filter(!(is.na(numParticipants)))
cacjDataAgg$AveragePop <- as.numeric(gsub(",", "", cacjDataAgg$AveragePop))
cacjDataAgg$AveragePop <- as.numeric(cacjDataAgg$AveragePop)

cacjDataAgg <- cacjDataAgg %>%
  mutate(Category = case_when(
    AveragePop >= 50000 ~ "Metropolitan",
    TRUE ~ "Non-Metropolitan"  
  ))
cacjDataAgg$Category=as.factor(cacjDataAgg$Category)



########################
#                      #
#   Make Fiscal Data   #
#                      #
########################

cacjDataAggFisc <- aggregate_funcFISC(cacjData)

################ ********************************
#              #
#   Category   # 
#   Analysis   # 
#              #
################ ********************************

Model1<-lm(propCompleted~propWhite+propHighSchool+propExitFullTime+numParticipants+propBlack+propCollege+propExitPartTime+propEntryFullTime+
             propExitUnemployed+meanAge+propEntryPartTime+propMale+meanDuration+propEntryUnemployed+courtType+Category, data=cacjDataAgg)

NewModel<-stepAIC(Model1)
summary(NewModel)
NewModel <- update(NewModel,~.+Category)

add1(NewModel,scope=~.+(.)^2,test="F")

ModelI <- update(NewModel,.~. +propExitFullTime:propEntryUnemployed+propExitFullTime:Category+propCollege:Category+ Category:courtType+ Category)
vif(ModelI)
summary(ModelI)

#Normality Check: 
qqPlot(ModelI,main="Q-Q plot for model I")
shapiro.test(rstandard(ModelI))#Fails Shapiro-Wilkes
ad.test(residuals(ModelI)) #Passes  Anderson-Darling Test


#Constant Variance
ncvTest(ModelI)


#Residual plot
residualPlot(ModelI,variable="fitted",type="rstudent",
             main="ext. Stud'zed resids vs fitteds, ModelI")

##########Estimate Confidence Interval SJ plot############
plot_model(ModelI, type = "est",show.values = TRUE, value.size = 2.75, value.offset = 0.45, 
           title = "Figure 5. Confidence Intervals for Model Estimates in Population Area Model", digits = 3)



###############Marginal Means for court types in population model######
emmsC <- emmeans(ModelI, ~ courtType, weights= "proportional")
summary(emmsC)
pairs(emmsC)
emm_dfC <- as.data.frame(emmsC)

ggplot(emm_dfC, aes(x = emmean, y = courtType)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.2, color = "#0072B2") +
  theme_minimal(base_size = 14) + labs(title = "Figure 6. Marginal Means for Court Types for \nPopulation Area Model", x="Marginal Mean Success Rate", y= "Court Type" )+
  coord_flip() +  geom_text(aes(label = round(emmean, 2)),hjust = -0.2, size = 4) 


##################Marginal Mean for Category#################
emmspop <- emmeans(ModelI, ~ Category, weights= "proportional")
emm_dfpop <- as.data.frame(emmspop)
ggplot(emm_dfpop, aes(x = emmean, y = Category)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.2, color = "#0072B2") +
  theme_minimal(base_size = 14) + labs(title = "Figure 7. Marginal Means for Population Areas in the \nPopulation Area Model", x="Marginal Mean Success Rate", y= "Population Area" )+
  coord_flip() +  geom_text(aes(label = round(emmean, 2)),hjust = -0.2, size = 4) 
pairs(emmspop)


############ Category by court type Interaction Term ############
emmeans_data <- ggemmeans(ModelI, terms = c("courtType", "Category"))

ggplot(emmeans_data, aes(x = x, y = predicted, color = group)) +
  geom_point() + 
  geom_line(aes(group = group)) + 
  labs(title = "Figure 2a. Interaction Plot Between Court Types by Population Area \nPopulation Model", x = "Court Type",
       y = "Mean Proportion of Success", color = "Population Area"
  ) + theme_minimal()


######ModelI
############ propExitFullTime:propEntryUnemployed Interaction Term ############
ggp2 <- ggpredict(ModelI,
                  terms=c("propExitFullTime","propEntryUnemployed [.2,.4,.6,.8,1]")) 
ggplot(ggp2,mapping=aes(x=x,y=predicted,color=group)) + 
  geom_line()  +
  labs(title="Figure 2a.Effect of Proportion of Unemployed When Entering Program \non Proportion of Participants Exit Program With Full Time Job \nin the Population Model",
       x="Proportion of Full time When Exiting",
       y="Mean Proportion of Successes",
       color="Proportion of Unemployed \n When Entering Program")



############ propExitFullTime:Category Interaction Plot ############
ggp3 <- ggpredict(ModelI,
                  terms=c("propExitFullTime","Category")) 
ggplot(ggp3, mapping = aes(x = x, y = predicted, color = group, fill = group)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) + 
  geom_line() +
  labs(
    title = "Figure 8. Interaction Plot of Population Area and Proportion \nof Participants Exit Program With Full Time Job",
    x = "Proportion of Full time When Exiting",
    y = "Mean Proportion of Successes",
    color = "Population Area",
    fill = "Population Area"
  )

############ propCollege:Category Interaction Plot ############

ggp4 <- ggpredict(ModelI,
                  terms=c("propCollege","Category")) 
ggplot(ggp4, mapping = aes(x = x, y = predicted, color = group, fill = group)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) + 
  geom_line() +
  labs(
    title = "Figure 9. Interaction Plot of Population Area and Proportion of \nParticipants with College Degrees",
    x = "Proportion of Participants with College Degrees",
    y = "Mean Proportion of Successes",
    color = "Population Area",
    fill = "Population Area"
  )

Anova(ModelI)



################### ********************************
#                 #
#   Fiscal Year   # 
#    Analysis     # 
#                 #
################### ********************************

############ Linear Mixed Effects Model ############

ggplot(cacjDataAggFisc, aes(x = FISCAL, y = propGraduate)) + geom_boxplot() + 
  labs(title = "Box Plot of Proportion Successes For Each Fiscal Year", x = "Fiscal Year", y = "Proportion of Successes") + theme_minimal()

cacjDataAggFisc$FISCAL= as.factor(cacjDataAggFisc$FISCAL)

#Fits model with all possible predictors
LME<-lmer(propGraduate ~ FISCAL+ numParticipants+courtType+meanAge+propMale+ propWhite+propBlack+ meanDuration+
            propHighSchool+propCollege+propEntryFullTime+propEntryPartTime+propEntryUnemployed+propExitFullTime+propExitPartTime+ 
            propExitUnemployed+ (1|PROGRAM_ID), data = cacjDataAggFisc)


#step uses backward elim for lmm
step.LME <- step(LME, ddf = "Kenward-Roger")%>% get_model()
anova(step.LME,ddf="Kenward-Roger")
formula(step.LME)

#Add all possible interaction terms w reduced model
LMEi <- lmer(propGraduate ~ (courtType + meanAge + meanDuration + propCollege + 
                               propEntryFullTime + propEntryUnemployed + propExitFullTime + 
                               propExitPartTime + propExitUnemployed +FISCAL )^2 + (1 | PROGRAM_ID),
             data = cacjDataAggFisc)
anova(LMEi,ddf="Kenward-Roger")
coef(summary(LMEi))

#Step function to reduce model with all interactions
step.LMEi <- get_model(step(LMEi, ddf = "Kenward-Roger"))

#add FISCAL back into model
step.LMEi<-update(step.LMEi,.~.+FISCAL)

anova(step.LMEi,ddf="Kenward-Roger")
summary(step.LMEi)

#################Confidence Intervals for Estimates#############
plot_model(step.LMEi, type = "est",show.values = TRUE, value.size = 2.5, value.offset = 0.45, 
           title = "Figure 10. Confidence Intervals for Model Estimates in Fiscal Year Model", digits = 3)

###################Marginal Means for Court Type###########
emmsF <- emmeans(step.LMEi, ~ courtType, weights= "proportional")
summary(emmsF)
pairs(emmsF)
emm_dfF <- as.data.frame(emmsF)
ggplot(emm_dfF, aes(x = emmean, y = courtType)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.2, color = "blue") +
  theme_minimal(base_size = 14) + labs(title = "Figure 11. Marginal Means for Court Types for \nFiscal Year Model", x="Marginal Mean Success Rate", y= "Court Type" )+
  coord_flip() +  geom_text(aes(label = round(emmean, 2)),hjust = -0.2, size = 4) 

##############Marginal Means for Fiscal Year###########3
emmsFis <- emmeans(step.LMEi, ~ FISCAL, weights= "proportional")
summary(emmsFis)
pairs(emmsFis)
emm_dfFis <- as.data.frame(emmsFis)
ggplot(emm_dfFis, aes(x = emmean, y = FISCAL)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.2, color = "blue") +
  theme_minimal(base_size = 14) + labs(title = "Figure 3a. Marginal Means for Fiscal Year in \nFiscal Model", x="Marginal Mean Success Rate", y= "Fiscal Year" )+
  coord_flip() +  geom_text(aes(label = round(emmean, 2)),hjust = -0.2, size = 4) 
############ Interaction Plots ############

#courtType:meanAge
ggp1 <- ggpredict(step.LMEi,
                  terms=c("meanAge","courtType")) 
ggplot(ggp1, mapping=aes(x=x,y=predicted,color=group)) + 
  geom_line()  +
  labs(title="Figure 12. Interaction Plot of Average Age of Participants and Court Type",
       x="Average Age of Participants",
       y="Mean Proportion of Successes",
       color="Court Type")

#courtType:meanDuration
ggp2 <- ggpredict(step.LMEi,
                  terms=c("meanDuration","courtType")) 
ggplot(ggp2,mapping=aes(x=x,y=predicted,color=group)) + 
  geom_line()  +
  labs(title="Figure 13. Interaction Plot of Average Duration Participant \nStayed in Program and Court Type",
       x="Average Duration Participant Stayed in Program (Days)",
       y="Mean Proportion of Successes",
       color="Court Type")

#courtType:propExitPartTime
ggp3 <- ggpredict(step.LMEi,
                  terms=c("propExitPartTime","courtType")) 
ggplot(ggp3,mapping=aes(x=x,y=predicted,color=group)) + 
  geom_line()  +
  labs(title="Figure 14. Interaction Plot of Court Type and Proportion of \nParticipants Exiting Program With Part Time Job ",
       x="Proportion of Participants Exiting Program with Part Time Jobs",
       y="Mean Proportion of Successes",
       color="Court Type")

#courtType:propExitUnemployed
ggp4 <- ggpredict(step.LMEi,
                  terms=c("propExitUnemployed ","courtType")) 
ggplot(ggp4,mapping=aes(x=x,y=predicted,color=group)) + 
  geom_line()  +
  labs(title="Figure 15. Interaction Plot of Court Type and Proportion \nof Participants Exiting Program Unemployed ",
       x="Proportion of Participants Exiting Program Unemployed",
       y="Mean Proportion of Successes",
       color="Court Type")

#meanAge:propExitFullTime
ggp5 <- ggpredict(step.LMEi,
                  terms=c("propExitFullTime","meanAge [20,30,40, 50, 60]")) 
ggplot(ggp5,mapping=aes(x=x,y=predicted,color=group)) + 
  geom_line()  +
  labs(title="Figure 4a. Effect of Average Age of Particiant in Program on \nProportion of Participants Exiting Program With Full Time Job \nin the Fiscal Model",
       x="Proportion of Participants With Full Time Jobs When Exiting Program",
       y="Mean Proportion of Successes",
       color="Average Age of Participants")

#propCollege:propEntryUnemployed
ggp6 <- ggpredict(step.LMEi,
                  terms=c("propCollege","propEntryUnemployed [.2,.4,.6,.8,1]")) 
ggplot(ggp6,mapping=aes(x=x,y=predicted,color=group)) + 
  geom_line()  +
  labs(title= "Figure 5a. Effect of Proportion of Unemployed When Entering \nProgram on Proportion of Participants with College Degrees \nin the Fiscal Model" ,
       x="Proportion of Participant with College Degrees",
       y="Mean Proportion of Successes",
       color="Proportion of \nParticipants Unemployed \nWhen Entering Program")

#propCollege:propExitUnemployed
ggp7 <- ggpredict(step.LMEi,
                  terms=c("propCollege","propExitUnemployed [.2,.4,.6,.8,1]")) 
ggplot(ggp7,mapping=aes(x=x,y=predicted,color=group)) + 
  geom_line()  +
  labs(title="Figure 6a. Effect of Proportion of Unemployed When Exiting \nProgram on Proportion of Participants with College Degrees \nin the Fiscal Model",
       x="Proportion of Participant with College Degrees",
       y="Mean Proportion of Successes",
       color="Proportion of \nParticipants Unemployed \nWhen Exiting Program")

#propEntryUnemployed:propExitUnemployed
ggp8 <- ggpredict(step.LMEi,
                  terms=c("propExitUnemployed","propEntryUnemployed [.2,.4,.6,.8,1]")) 
ggplot(ggp8,mapping=aes(x=x,y=predicted,color=group)) + 
  geom_line()  +
  labs(title="Figure 7a. Effect of Proportion of Unemployed Participants When \nEntering Program on Proportion of Participants Exiting Program Unemployed \nin the Fiscal Model",
       x="Proportion of Unemployed Participants When Exiting Program",
       y="Mean Proportion of Successes",
       color="Proportion of \nParticipants Unemployed \nWhen Entering Program")

#propExitFullTime:propExitUnemployed
ggp9 <- ggpredict(step.LMEi,
                  terms=c("propExitFullTime","propExitUnemployed [.2,.4,.6,.8,1]")) 
ggplot(ggp9,mapping=aes(x=x,y=predicted,color=group)) + 
  geom_line()  +
  labs(title="Figure 8a. Effect of Proportion of Participant With Full-TIme Jobs When Exiting \nProgram on Proportion of Participants Exiting Program Unemployed \nin the Fiscal Model",
       x="Proportion of Full time When Exiting",
       y="Mean Proportion of Successes",
       color="Proportion of \nParticipants Unemployed \nWhen Exiting Program")

Anova(step.LMEi, test.statistic = 'F')