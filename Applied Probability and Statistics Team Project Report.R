#Applied Probability and Statistics Team Project Report - R code
#1.Packages used in the final project
install.packages("PerformanceAnalytics")
install.packages("pROC")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("MASS")
install.packages("ggmap")
install.packages("gridExtra")
install.packages("readr")
install.packages('arm')
library(dplyr)
library(ggplot2)
library(tidyr)
library(PerformanceAnalytics)
library(MASS)
library(plyr)
library(ggmap)
library(gridExtra)
library(readr)
library(arm)

#2.Datasets
#Before, we have done the data cleaning utilizing R code and excel, details are not shown in this document
#2.1Read the csv
HMDA<-read.csv("HMDAFull.csv")
head(HMDA)
#2.2Add a column to categorize every row into whether getting apporved or not
HMDA1 <- HMDA %>%
  filter(Applicant_Income != '')
summary(HMDA1$Loan._Status)
HMDA_Denied <- HMDA1 %>%
  filter(Loan._Status != 'Application approved but not accepted',
         Loan._Status != 'Application withdrawn by applicant',
         Loan._Status != 'File closed for incompleteness',
         Loan._Status != 'Loan originated',
         Loan._Status != 'Loan purchased by the institution')
HMDA_Denied$Approval <- 0
HMDA_Denied
HMDA_Approved <- HMDA1 %>%
  filter(Loan._Status != 'Application denied by financial institution',
         Loan._Status != 'Application withdrawn by applicant',
         Loan._Status != 'File closed for incompleteness',
         Loan._Status != 'Preapproval request approved but not accepted',
         Loan._Status != 'Preapproval request denied by financial institution')
HMDA_Approved$Approval <- 1
HMDA_Approved
HMDA_Clean <- rbind(HMDA_Approved,HMDA_Denied)
HMDA_Clean

#3.Data visualization
#3.1 Approval vs. Race
COUNT_DENIED_BY_RACE <- HMDA_Denied %>% 
  group_by(Applicant_Race) %>% 
  summarise(count = n()) 

COUNT_APPROVED_BY_RACE <- HMDA_Approved %>% 
  group_by(Applicant_Race) %>% 
  summarise(count = n())

COUNT_BY_RACE <- union(COUNT_DENIED_BY_RACE, COUNT_APPROVED_BY_RACE)
COUNT_DENIED_BY_RACE$status <- with(COUNT_BY_RACE, 'denied')
COUNT_APPROVED_BY_RACE$status <- with(COUNT_BY_RACE, 'approved')
COUNT_BY_RACE <- union(COUNT_DENIED_BY_RACE, COUNT_APPROVED_BY_RACE)
COUNT_BY_RACE

p2 <- ggplot() + geom_bar(aes(y = count, x = Applicant_Race, fill = status), data = COUNT_BY_RACE,
                          stat="identity", position='fill', width = 0.5) + ylab('Percentage of approval') + scale_y_continuous(labels = scales::percent)
P3 <- p2 + theme(axis.text.x = element_text(angle = 30, hjust = 1))
P3

#3.2 Approval vs. Co_appllicant race
COUNT_DENIED_BY_CORACE <- HMDA_Denied %>% 
  group_by(Co_Applicant_Race) %>% 
  summarise(count = n()) 

COUNT_APPROVED_BY_CORACE <- HMDA_Approved %>% 
  group_by(Co_Applicant_Race) %>% 
  summarise(count = n())
COUNT_BY_CO_RACE <- union(COUNT_DENIED_BY_CORACE, COUNT_APPROVED_BY_CORACE)
COUNT_DENIED_BY_CORACE$status <- with(COUNT_BY_CO_RACE, 'denied')
COUNT_APPROVED_BY_CORACE$status <- with(COUNT_BY_CO_RACE, 'approved')

COUNT_BY_CO_RACE <- union(COUNT_DENIED_BY_CORACE, COUNT_APPROVED_BY_CORACE)

COUNT_BY_CO_RACE
p4 <- ggplot() + geom_bar(aes(y = count, x = Co_Applicant_Race, fill = status), data = COUNT_BY_CO_RACE,
                          stat="identity", position='fill', width = 0.5) + ylab('Percentage of approval') + scale_y_continuous(labels = scales::percent)
P5 <- p4 + theme(axis.text.x = element_text(angle = 30, hjust = 1))
P5

#3.3 Approval vs. income_tier
HMDA_Clean$Income_Tier <- with(HMDA_Clean, ifelse(Applicant_Income < 500, 'Low', ifelse(Applicant_Income < 2000, 'Medium', 'High')))
HMDA_Low = HMDA_Clean[HMDA_Clean$Income_Tier=='Low', ]
HMDA_NotLow = HMDA_Clean[HMDA_Clean$Income_Tier!='Low', ]

HMDA_Low = HMDA_Low[seq(1, nrow(HMDA_Clean), 100), ]
HMDA_Sampled = union(HMDA_Low, HMDA_NotLow)
HMDA_Sampled = HMDA_Sampled[!is.na(HMDA_Sampled$Income_Tier),]
nrow(HMDA_Sampled)
HMDA_Sampled$Approval <- as.character(HMDA_Sampled$Approval)

p6 <- ggplot(HMDA_Sampled, aes(x=Income_Tier, y=Loan_Amount)) +
  geom_boxplot(aes(fill=Approval), alpha=0.5) + ylim(0,4000)

p6 

#3.4 Approval vs. County: Percentage of loan approval in Washington's Counties
COUNT_ALL <- HMDA_Clean %>% 
  group_by(County_Name) %>% 
  summarise(count_all = n()) 

COUNT_APPROVED <- HMDA_Approved %>% 
  group_by(County_Name) %>% 
  summarise(count_approved = n())

COUNT <- merge(x=COUNT_ALL, y=COUNT_APPROVED, by="County_Name")

COUNT
COUNT$percent_approval <- with(COUNT, count_approved/count_all * 100)
COUNT[] <- lapply(COUNT, gsub, pattern = " County", replacement = "", fixed = TRUE)
COUNT$subregion <- tolower(COUNT$County_Name)
COUNT$percent_approval <- as.numeric(COUNT$percent_approval)

citation("ggmap")

states <- map_data("state")
wa_df <- subset(states, region == "washington")

counties <- map_data("county")
wa_county <- subset(counties, region == "washington")

wa_base <- ggplot(data = wa_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

wa_base + theme_nothing() + 
  geom_polygon(data = wa_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top

COUNT$percent_approval
COUNT[order(COUNT$percent_approval),] 

wa_data_plot <- inner_join(wa_county, COUNT, by = "subregion")

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank()
)

map <- wa_base + 
  geom_polygon(data = wa_data_plot, aes(fill = percent_approval ), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  ggtitle("Percentage of loan approval in Washington's Counties") +
  theme_bw() +
  ditch_the_axes

#3.5 Approval vs. Property type
COUNT_DENIED_BY_PROPERTY <- HMDA_Denied %>% 
  group_by(Property_Type) %>% 
  summarise(count = n()) 
COUNT_APPROVED_BY_PROPERTY <- HMDA_Approved %>% 
  group_by(Property_Type) %>% 
  summarise(count = n())

COUNT_BY_PROPERTY <- union(COUNT_DENIED_BY_PROPERTY, COUNT_APPROVED_BY_PROPERTY)
COUNT_DENIED_BY_PROPERTY$status <- with(COUNT_BY_PROPERTY, 'denied')
COUNT_APPROVED_BY_PROPERTY$status <- with(COUNT_BY_PROPERTY, 'approved')
COUNT_BY_PROPERTY <- union(COUNT_DENIED_BY_PROPERTY, COUNT_APPROVED_BY_PROPERTY)
COUNT_BY_PROPERTY
p7 <- ggplot() + geom_bar(aes(y = count, x = Property_Type, fill = status), data = COUNT_BY_PROPERTY,
                          stat="identity", position='fill', width = 0.5) + 
  ylab('Percentage of approval') + 
  scale_y_continuous(labels = scales::percent)
p8 <- p7 + theme(axis.text.x = element_text(angle = 25, hjust = 1))
p8

#3.6 Approval vs. loan purpose
HDMA_Clean$Approval.factor<-factor(HDMA_Clean$Approval)
HDMA_Purpose_Approved<-HDMA_Approved %>% group_by(Loan_Purpose) %>% summarize(n())
HDMA_Purpose_Approved
HDMA_Purpose_Denied<-HDMA_Denied %>% group_by(Loan_Purpose) %>% summarize(n())
HDMA_Purpose_Denied
HDMA_Purpose_Approved$percentage<-(HDMA_Purpose_Approved$`n()`/(HDMA_Purpose_Approved$`n()`+HDMA_Purpose_Denied$`n()`))*100

p9<-ggplot(HDMA_Purpose_Approved, aes(x=Loan_Purpose,y=percentage,fill=Loan_Purpose))+
  geom_col()+
  xlab("Loan Purpose")+
  ylab("Percentage Approval")
p9

#3.7 Approval vs. Minority Population
p2<-ggplot(HDMA_Clean,aes(x=Approval.factor,y=Minority_Population))+
  geom_boxplot()+
  xlab("Approval Status")+
  ylab("Minority Population")
p2

#3.8 Approval vs. HOEPA
HDMA_HOEPA_Approved<-HDMA_Approved %>% group_by(HOEPA_Status) %>% summarize(n())
HDMA_HOEPA_Approved
HDMA_HOEPA_Denied<-HDMA_Denied %>% group_by(HOEPA_Status) %>% summarize(n())
HDMA_HOEPA_Denied
HDMA_HOEPA_Approved$percentage<-(HDMA_HOEPA_Approved$`n()`/(HDMA_HOEPA_Approved$`n()`+HDMA_HOEPA_Denied$`n()`))*100

p11<-ggplot(HDMA_HOEPA_Approved, aes(x=HOEPA_Status,y=percentage,fill=HOEPA_Status))+
  geom_col()+
  xlab("HOEPA Status")+
  ylab("Percentage Approval")+
  theme(legend.position = "none")+
  geom_text(aes(x=HOEPA_Status,y=percentage,label=round(percentage,2)))
p11

#3.9 Approval vs. Liean status
HDMA_Lien_Approved<-HDMA_Approved %>% group_by(Lien_Status) %>% summarize(n())
HDMA_Lien_Approved
HDMA_Lien_Denied<-HDMA_Denied %>% group_by(Lien_Status) %>% summarize(n())
HDMA_Lien_Denied
HDMA_Lien_Approved$percentage<-(HDMA_Lien_Approved$`n()`/(HDMA_Lien_Approved$`n()`+HDMA_Lien_Denied$`n()`))*100

HDMA_Lien_Approved
p12<-ggplot(HDMA_Lien_Approved, aes(x=Lien_Status,y=percentage,fill=Lien_Status))+
  geom_col()+xlab("Lien Status")+
  ylab("Percentage Approval")+
  theme(legend.position = "none")+
  geom_text(aes(x=Lien_Status,y=percentage,label=round(percentage,2)))
p12

#3.10 Approval vs. Median family income
p6<-ggplot(HDMA_Clean,aes(x=County_Name,y=Median_County_Family_Income, fill=Approval.factor))+
  geom_boxplot()+
  theme(legend.pos= "none")+
  xlab(Median )
p6

#3.11 Approval vs. Country Name
HDMA_County_Approved<-HDMA_Approved %>% group_by(County_Name) %>% summarize(n())
HDMA_County_Approved
HDMA_County_Denied<-HDMA_Denied %>% group_by(County_Name) %>% summarize(n())
HDMA_County_Denied
Count_by_county<-union(HDMA_County_Approved,HDMA_County_Denied)
HDMA_County_Approved$percentage<-(HDMA_County_Approved$`n()`/(HDMA_County_Approved$`n()`+HDMA_County_Denied$`n()`))*100

p5<-ggplot(HDMA_County_Approved,aes(x=County_Name,y=percentage, fill=County_Name, palette(Blues)))+
  geom_col()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  theme(legend.position = "none")+xlab("County Name")+
  ylab("Percentage Approval")
p5

#3.12 Approval vs. Owner occupied units
p4<-ggplot(HDMA_Clean,aes(x=Approval.factor,y=Owner_Occupied_Units_Number))+
  geom_boxplot()+
  xlab("Approval Status")+
  ylab("# Owner Occupied Units")
p4

#3.13 Approval vs. tract population
p3<-ggplot(HDMA_Clean,aes(x=Approval.factor,y=Tract_Population))+
  geom_boxplot()+
  xlab("Approval Status")+
  ylab("Tract Population")
p3

#3.14 Approval vs. loan type
HDMA_Type_Approved<-HDMA_Approved %>% group_by(Loan_Type) %>% summarize(n())
HDMA_Type_Approved
HDMA_Type_Denied<-HDMA_Denied %>% group_by(Loan_Type) %>% summarize(n())
HDMA_Type_Denied
HDMA_Type_Approved$percentage<-(HDMA_Type_Approved$`n()`/(HDMA_Type_Approved$`n()`+HDMA_Type_Denied$`n()`))*100

p8<-ggplot(HDMA_Type_Approved, aes(x=Loan_Type,y=percentage,fill=Loan_Type))+
  geom_col()+xlab("Loan Type")+
  ylab("Percentage Approval")
p8

#3.15 Approval vs. loan issuer
p10<-ggplot(HDMA_Clean, aes(x=Loan_Issuer, y=stat(count),fill=Approval.factor))+
  geom_bar()+xlab("Loan Issuer")+
  ylab("Count")+theme(axis.text.x = element_text(angle = 90, hjust = 1))
p10

#3.16 Evidence of why Loan_Issuer should not be added to the regression
HMDA_Clean %>% ggplot(aes(x=as.factor(Loan_Issuer),fill=as.factor(Approval)))+
  geom_histogram(stat="count")+theme(axis.text.x=element_text(angle = -90, hjust = 0))

#4.Regression
#4.1.1 Correlation matrix of all continous variables 
Sub_HMDA<-cbind(Applicant_Income = HMDA_Clean$Applicant_Income,
                Loan_Amount = HMDA_Clean$Loan_Amount,                
                Tract_Population = HMDA_Clean$Tract_Population, 
                Minority_Population = HMDA_Clean$Minority_Population,
                Owner_Occupied_Units_Number = HMDA_Clean$Owner_Occupied_Units_Number,
                Median_County_Family_Income = HMDA_Clean$Median_County_Family_Income)
Sub_HMDA

cor(Sub_HMDA,use="everything",method=c("pearson","kendall","spearman"))
chart.Correlation(Sub_HMDA)
#4.1.2 Evaluation for Model Fit
#original 16 variable's model
glm1 = glm(formula = Approval ~ as.factor(Applicant_Gender) + as.factor(Applicant_Race) + 
             as.factor(Co_Applicant_Gender) + as.factor(Co_Applicant_Race) + 
             Applicant_Income + as.factor(Property_Type) + as.factor(County_Name) + 
             Loan_Amount + as.factor(Loan_Type) + as.factor(Loan_Purpose) + as.factor(Lien_Status) + as.factor(HOEPA_Status) + 
             Tract_Population + Minority_Population + Owner_Occupied_Units_Number + Median_County_Family_Income, 
           family = binomial(link = "logit"), data = HDMA_Clean)

binnedplot(fitted(glm1), 
           residuals(glm1, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot for original model", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")

#log only applicant_income
glm2 = glm(formula = Approval ~ as.factor(Applicant_Gender) + as.factor(Applicant_Race) + 
             as.factor(Co_Applicant_Gender) + as.factor(Co_Applicant_Race) + 
             Applicant_Income_log + as.factor(Property_Type) + as.factor(County_Name) + 
             Loan_Amount + as.factor(Loan_Type) + as.factor(Loan_Purpose) + as.factor(Lien_Status) + as.factor(HOEPA_Status) + 
             Tract_Population + Minority_Population + Owner_Occupied_Units_Number + Median_County_Family_Income, 
           family = binomial(link = "logit"), data = HDMA_Clean)

binnedplot(fitted(glm2), 
           residuals(glm2, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot for log income model", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")

#log only loan_amount
glm3 = glm(formula = Approval ~ as.factor(Applicant_Gender) + as.factor(Applicant_Race) + 
             as.factor(Co_Applicant_Gender) + as.factor(Co_Applicant_Race) + 
             Applicant_Income + as.factor(Property_Type) + as.factor(County_Name) + 
             Loan_Amount_log + as.factor(Loan_Type) + as.factor(Loan_Purpose) + as.factor(Lien_Status) + as.factor(HOEPA_Status) + 
             Tract_Population + Minority_Population + Owner_Occupied_Units_Number + Median_County_Family_Income, 
           family = binomial(link = "logit"), data = HDMA_Clean)

binnedplot(fitted(glm3), 
           residuals(glm3, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot for log loan amount model", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")

#log both income and loan amount
glm4 = glm(formula = Approval ~ as.factor(Applicant_Gender) + as.factor(Applicant_Race) + 
             as.factor(Co_Applicant_Gender) + as.factor(Co_Applicant_Race) + 
             Applicant_Income_log + as.factor(Property_Type) + as.factor(County_Name) + 
             Loan_Amount_log + as.factor(Loan_Type) + as.factor(Loan_Purpose) + as.factor(Lien_Status) + as.factor(HOEPA_Status) + 
             Tract_Population + Minority_Population + Owner_Occupied_Units_Number + Median_County_Family_Income, 
           family = binomial(link = "logit"), data = HDMA_Clean)

binnedplot(fitted(glm4), 
           residuals(glm4, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot for log both", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")

#interaction between loan amount and income
glm5 = glm(formula = Approval ~ as.factor(Applicant_Gender) + as.factor(Applicant_Race) + 
             as.factor(Co_Applicant_Gender) + as.factor(Co_Applicant_Race) + 
             Applicant_Income*Loan_Amount + as.factor(Property_Type) + as.factor(County_Name) + 
             as.factor(Loan_Type) + as.factor(Loan_Purpose) + as.factor(Lien_Status) + as.factor(HOEPA_Status) + 
             Tract_Population + Minority_Population + Owner_Occupied_Units_Number + Median_County_Family_Income, 
           family = binomial(link = "logit"), data = HDMA_Clean)

binnedplot(fitted(glm5), 
           residuals(glm5, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot for interaction", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")

#4.2 Customer Driven Model
#4.2.1 Model
lm_base <- glm(Approval~as.factor(Applicant_Gender)+as.factor(Applicant_Race)+
                 log10(Applicant_Income)+as.factor(Property_Type)+as.factor(County_Name)+
                 log10(Loan_Amount),HMDA_Clean,family=binomial(link='logit'))
summary(lm_base)
#4.2.2 Calculate McFadden's R Squared comparing to the base model y=1
nullmod<-glm(HMDA_Clean$Approval~1,family="binomial")
1-logLik(lm_base)/logLik(nullmod)

#4.3 Intuitive Model
#4.3.1 Model
glm1<-glm(HMDA_Clean$Approval~
            log10(HMDA_Clean$Applicant_Income)+
            log10(HMDA_Clean$Loan_Amount)+
            HMDA_Clean$Minority_Population+
            HMDA_Clean$Applicant_Gender+
            HMDA_Clean$Applicant_Race+
            HMDA_Clean$Loan_Type+
            HMDA_Clean$Loan_Purpose+
            HMDA_Clean$Property_Type+
            HMDA_Clean$Co_Applicant_Gender,
          family=binomial(link="logit"))
summary(glm1)
#4.3.2 Calculate McFadden's R Squared comparing to the base model y=1
nullmod<-glm(HMDA_Clean$Approval~1,family="binomial")
1-logLik(glm1)/logLik(nullmod)

#4.4 Automatic selection model
#4.4.1 Model
lm0 <- glm(Approval~as.factor(Applicant_Gender)+as.factor(Applicant_Race)+
             as.factor(Co_Applicant_Gender)+as.factor(Co_Applicant_Race)+
             log10(Applicant_Income)+as.factor(Property_Type)+as.factor(County_Name)+
             log10(Loan_Amount)+as.factor(Loan_Type)+as.factor(Loan_Purpose)+
             as.factor(Lien_Status)+as.factor(HOEPA_Status)+
             Tract_Population+Minority_Population+Owner_Occupied_Units_Number+
             Median_County_Family_Income,HMDA_Clean,family=binomial(link='logit'))
lm01 <- step(lm0,direction='both')
summary(lm01)
#4.4.2 Calculate McFadden's R Squared comparing to the base model y=1
lm_step <- glm(Approval~as.factor(Applicant_Gender)+as.factor(Applicant_Race)+
                 as.factor(Co_Applicant_Gender)+as.factor(Co_Applicant_Race)+
                 log10(Applicant_Income)+as.factor(Property_Type)+as.factor(County_Name)+
                 log10(Loan_Amount)+as.factor(Loan_Type)+as.factor(Loan_Purpose)+
                 as.factor(Lien_Status)+as.factor(HOEPA_Status)+Owner_Occupied_Units_Number+
                 Minority_Population,hmda_Clean,family=binomial(link='logit'))
summary(lm_step)
nullmod<-glm(hmda_Clean$Approval~1,family="binomial")
1-logLik(lm_step)/logLik(nullmod)

#5.Prediction & Empirical
#5.1 Prediciton
#5.1.1 Customer driven model
pr1<-data.frame(predict(lm_base,hmda_Clean,type="response"))
pr1
#5.1.2 Intuitive model
pr2<-data.frame(predict(glm1,HMDA_Clean,type="response"))
pr2
#5.1.3 Automatic selection model
pr3<-data.frame(predict(lm_step,HMDA_Clean,type="response"))
pr3

#5.2 Accuracy rate calculation
#5.2.1 Customer driven model
names(pr1)='Prob'
HMDA_Clean_base <- cbind(HMDA_Clean,pr1)
head(HMDA_Clean_base)
x1 <- HMDA_Clean_base %>% filter(Approval==1) %>% summarize(sum(Prob))
x2 <- HMDA_Clean_base %>% summarize(sum(Approval))
x1/x2
#5.2.2 Intuitive model
ct_yes <- HMDA_Clean %>% summarize(sum=sum(Approval))
HMDA_pr <- cbind(HMDA_Clean$Approval,pr1)
names(HMDA_pr)[1]<-"Approval_Rate"
names(HMDA_pr)[2]<-"Prob"
head(HMDA_pr)
HMDA_pr1<-mutate(HMDA_pr,cross_prod=Approval_Rate*Prob)
ct_accuracy<-HMDA_pr1 %>% summarize(sum=sum(cross_prod))
ct_accuracy/ct_yes
ct_all <- HMDA_Clean %>% summarize(n())
#5.2.3 Automatic selection model
names(pr1)='Prob'
HMDA_Clean_lm01 <- cbind(HMDA_Clean,pr1)
head(HMDA_Clean_lm01)
x1 <- HMDA_Clean_lm01 %>% filter(Approval==1) %>% summarize(sum(Prob))
x2 <- HMDA_Clean_lm01 %>% summarize(sum(Approval))
x1/x2

#5.3 ROC & AUC
#5.3.1 Customer driven model
glm.fit = glm(Approval~as.factor(Applicant_Gender)+
                as.factor(Applicant_Race)+
                Applicant_Income+
                as.factor(Property_Type)+
                as.factor(County_Name)+
                Loan_Amount,hmda_Clean,family=binomial(link='logit'))
roc(hmda_Clean$Approval,glm.fit$fitted.values,plot=TRUE,legacy.axes=TRUE,percent=TRUE,
    xlab="False Positive PErcentage", ylab="True Positive PErcentage",print.auc=TRUE)
#5.3.2 Intuitive model
glm.fit = glm(HMDA_Clean$Approval~
                log10(HMDA_Clean$Applicant_Income)+
                log10(HMDA_Clean$Loan_Amount)+
                HMDA_Clean$Minority_Population+
                HMDA_Clean$Applicant_Gender+
                HMDA_Clean$Applicant_Race+
                HMDA_Clean$Loan_Type+
                HMDA_Clean$Loan_Purpose+
                HMDA_Clean$Property_Type+
                HMDA_Clean$Co_Applicant_Gender,
              family=binomial(link="logit"))
roc(hmda_Clean$Approval,glm.fit$fitted.values,plot=TRUE,legacy.axes=TRUE,percent=TRUE,
    xlab="False Positive PErcentage", ylab="True Positive PErcentage",print.auc=TRUE)
#5.3.3 Automatic selection model
glm.fit = glm(Approval~as.factor(Applicant_Gender)+
                as.factor(Applicant_Race)+
                as.factor(Co_Applicant_Gender)+
                as.factor(Co_Applicant_Race)+
                Applicant_Income+
                as.factor(Property_Type)+
                as.factor(County_Name)+
                Loan_Amount+
                as.factor(Loan_Type)+
                as.factor(Loan_Purpose)+
                as.factor(Lien_Status)+
                as.factor(HOEPA_Status)+
                Minority_Population+
                Owner_Occupied_Units_Number,
              hmda_Clean,family=binomial(link='logit'))
roc(hmda_Clean$Approval,glm.fit$fitted.values,plot=TRUE,legacy.axes=TRUE,percent=TRUE,
    xlab="False Positive PErcentage", ylab="True Positive PErcentage",print.auc=TRUE)