
# Experiment: Paper-Based and Online-Based 
# 2x2 (Between Subject) ANOVA
# Author:Kübra Mahmut Önalan 

# 1. Import Package 

library (tidyverse) 
library (ggpubr) 
library (rstatix)
library(stats) 
library (dplyr) 
library(haven) 
library(ggplot2)
library(gtsummary)
library(flextable)
library(psych)

setwd("")

# Organize Dataset

All_data <- read_sav("")

View(All_data)

summary(All_data) #summary() to see which variables have missing (NA) values

MISSING <- is.na(All_data$DASS_Total) |
  is.na(All_data$Depression) |
  is.na(All_data$Anxiety) |
  is.na(All_data$AQ_Total) |
  is.na(All_data$Stress) |
  is.na(All_data$Social_Skills) |
  is.na(All_data$Attention_Switching) |
  is.na(All_data$Attention_toDetail) |
  is.na(All_data$Communication)|
  is.na(All_data$Imagination)

sum(MISSING) # Count the number of rows flagged for deletion

df_alldata<- subset(All_data, 
                    subset = !MISSING) #Use ! to include those that are NOT missing


View(df_alldata) # new alldataset 

summary(df_alldata)# Check again


# Create df_descriptive

df_descriptive <- df_alldata %>%
  select(TypeofPresentation, TypeofControl,
         Yaşınız, Sex, Education, RelationshipStatus, HavingKids)

df_descriptive

df_descriptive$TypeofPresentation <- factor(df_descriptive$TypeofPresentation ,levels = c(1,2), labels = c("Paper", "Online"))


df_descriptive$TypeofControl <- factor(df_descriptive$TypeofControl, levels = c(1,2), labels = c("Control", "NoControl"))

df_descriptive$Sex <- factor(df_descriptive$Sex ,levels = c(1,2), labels = c("Female", "Male"))


df_descriptive$Education <- factor(df_descriptive$Education, levels = c(1,2), labels = c("University", "Master/Ph.D"))

df_descriptive$RelationshipStatus <- factor(df_descriptive$RelationshipStatus, levels = c(1,2,3), labels = c("Single", "Married", "In relationship"))

df_descriptive$HavingKids <- factor(df_descriptive$HavingKids, levels = c(1,2), labels = c("Yes", "No"))

View(df_descriptive)

#  Participant Characteristics

tbl <-
  df_descriptive %>%
  tbl_strata (
    strata = TypeofPresentation,
    .header = "**{strata}** <br> (N = {n})",
    .tbl_fun =
      ~ .x %>% tbl_summary(
        by = TypeofControl,
        statistic = all_continuous() ~ "ORT. ={mean}, ST= {sd},Min= {min}, Max={max})",
        include = c(Yaşınız,Sex,Education,RelationshipStatus,HavingKids),
        label = list(
          Yaşınız = "Age",
          Sex = "Sex",
          Education = "Education Level",
          RelationshipStatus = "Relationship Status",
          HavingKids= "Having Kids"
        ) 
        
      ) %>% modify_header(label = "**.**", all_stat_cols() ~      "**{level}**<br>N ={n}")%>%
      bold_labels() ) 
tbl


# Create df_analyses


df_analyses <- df_alldata %>%
  select(participan_ID,TypeofPresentation,TypeofControl,DASS_Total,Depression, Anxiety,AQ_Total,
         Stress, Social_Skills, Attention_Switching, Attention_toDetail, Communication, Imagination)



View(df_analyses)

df_analyses$TypeofPresentation <- factor(df_analyses$TypeofPresentation ,levels = c(1,2), labels = c("Paper", "Online"))


df_analyses$TypeofControl <- factor(df_analyses$TypeofControl, levels = c(1,2), labels = c("Control", "NoControl"))

df_analyses$participan_ID <- factor(df_analyses$participan_ID)

View(df_analyses)


# df_analyses 

# means and sd

variables_means <- df_analyses %>% group_by(TypeofPresentation,TypeofControl)  %>%  
  get_summary_stats(DASS_Total, Depression, Anxiety,Stress,AQ_Total, Social_Skills, 
                    Attention_Switching, Attention_toDetail, Communication, Imagination, type = "mean_sd")

variables_means


# Assumptions

# Check Outliers

df_analyses   %>% group_by(TypeofPresentation, TypeofControl)  %>% identify_outliers(DASS_Total)
df_analyses   %>% group_by(TypeofPresentation, TypeofControl)  %>% identify_outliers(Depression)
df_analyses   %>% group_by(TypeofPresentation, TypeofControl)  %>% identify_outliers(Anxiety)
df_analyses   %>% group_by(TypeofPresentation, TypeofControl)  %>% identify_outliers(Stress)
df_analyses   %>% group_by(TypeofPresentation, TypeofControl)  %>% identify_outliers(AQ_Total)
df_analyses   %>% group_by(TypeofPresentation, TypeofControl)  %>% identify_outliers(Social_Skills)
df_analyses   %>% group_by(TypeofPresentation, TypeofControl)  %>% identify_outliers(Attention_Switching)
df_analyses   %>% group_by(TypeofPresentation, TypeofControl)  %>% identify_outliers(Attention_toDetail)
df_analyses   %>% group_by(TypeofPresentation, TypeofControl)  %>% identify_outliers(Communication)
df_analyses   %>% group_by(TypeofPresentation, TypeofControl)  %>% identify_outliers(Imagination)

#Check Normality 

df_analyses %>% group_by(TypeofPresentation,TypeofControl) %>% shapiro_test(DASS_Total)
df_analyses %>% group_by(TypeofPresentation,TypeofControl) %>% shapiro_test(Depression)
df_analyses %>% group_by(TypeofPresentation,TypeofControl) %>% shapiro_test(Anxiety)
df_analyses %>% group_by(TypeofPresentation,TypeofControl) %>% shapiro_test(Stress)
df_analyses %>% group_by(TypeofPresentation,TypeofControl) %>% shapiro_test(AQ_Total)
df_analyses %>% group_by(TypeofPresentation,TypeofControl) %>% shapiro_test(Social_Skills)
df_analyses %>% group_by(TypeofPresentation,TypeofControl) %>% shapiro_test(Attention_Switching)
df_analyses %>% group_by(TypeofPresentation,TypeofControl) %>% shapiro_test(Attention_toDetail)
df_analyses %>% group_by(TypeofPresentation,TypeofControl) %>% shapiro_test(Communication)
df_analyses %>% group_by(TypeofPresentation,TypeofControl) %>% shapiro_test(Imagination)

# qqplots

ggqqplot(df_analyses, "DASS_Total", ggtheme = theme_bw()) +
  facet_grid (TypeofPresentation ~ TypeofControl, labeller = "label_both")

ggqqplot(df_analyses, "Depression", ggtheme = theme_bw()) +
  facet_grid (TypeofPresentation ~ TypeofControl, labeller = "label_both")

ggqqplot(df_analyses, "Anxiety", ggtheme = theme_bw()) +
  facet_grid (TypeofPresentation ~ TypeofControl, labeller = "label_both")

ggqqplot(df_analyses, "Stress", ggtheme = theme_bw()) +
  facet_grid (TypeofPresentation ~ TypeofControl, labeller = "label_both")

ggqqplot(df_analyses, "AQ_Total", ggtheme = theme_bw()) +
  facet_grid (TypeofPresentation ~ TypeofControl, labeller = "label_both")

ggqqplot(df_analyses, "Social_Skills", ggtheme = theme_bw()) +
  facet_grid (TypeofPresentation ~ TypeofControl, labeller = "label_both")

ggqqplot(df_analyses, "Attention_Switching", ggtheme = theme_bw()) +
  facet_grid (TypeofPresentation ~ TypeofControl, labeller = "label_both")

ggqqplot(df_analyses, "Attention_toDetail", ggtheme = theme_bw()) +
  facet_grid (TypeofPresentation ~ TypeofControl, labeller = "label_both")

ggqqplot(df_analyses, "Communication", ggtheme = theme_bw()) +
  facet_grid (TypeofPresentation ~ TypeofControl, labeller = "label_both")

ggqqplot(df_analyses, "Imagination", ggtheme = theme_bw()) +
  facet_grid (TypeofPresentation ~ TypeofControl, labeller = "label_both")

# Between Subj 2 x 2 ANOVA

#IV : 
# 1. factor - typeofpresentation : Online vs. in-Paper (2 level) /
# 2. factor - typeofsupervision : Control vs.noControl (2 level) 
#DV: AQ_total,DASS_total,Depression, Anxiety,Stress, Social_Skills, Attention_Switching, Attention_toDetail, Communication, Imagination



model1 <- aov(DASS_Total~ TypeofPresentation * TypeofControl, data = df_analyses)
model2 <- aov(Depression~ TypeofPresentation * TypeofControl, data = df_analyses)
model3 <- aov(Anxiety~ TypeofPresentation * TypeofControl, data = df_analyses)
model4 <- aov(Stress~ TypeofPresentation * TypeofControl, data = df_analyses)

model5 <- aov(AQ_Total~ TypeofPresentation * TypeofControl, data = df_analyses)
model6 <- aov(Social_Skills~ TypeofPresentation * TypeofControl, data = df_analyses)
model7 <- aov(Attention_Switching~ TypeofPresentation * TypeofControl, data = df_analyses)
model8 <- aov(Attention_toDetail~ TypeofPresentation * TypeofControl, data = df_analyses)
model9 <- aov(Communication~ TypeofPresentation * TypeofControl, data = df_analyses)
model10 <- aov(Imagination~ TypeofPresentation * TypeofControl, data = df_analyses)

#summary()


#Visualition of Data - İnteraction plots

#plot(AQ_Total~ factor (TypeofPresentation) + factor (TypeofControl), data = df_data)

interaction.plot(x.factor = df_analyses$TypeofPresentation, #x-axis variable
                 trace.factor = df_analyses$TypeofControl, #variable for lines
                 response = df_analyses$DASS_Total, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "DASS Scale",
                 xlab = "Presantation Type",
                 col = c("pink", "blue"),
                 lty= 4, #line type
                 lwd = 2, #line width
                 trace.label = "Control")



interaction.plot(x.factor = df_analyses$TypeofPresentation, #x-axis variable
                 trace.factor = df_analyses$TypeofControl, #variable for lines
                 response = df_analyses$Depression, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "Depression",
                 xlab = "Presantation Type",
                 col = c("pink", "blue"),
                 lty= 4, #line type
                 lwd = 2, #line width
                 trace.label = "Control")

interaction.plot(x.factor = df_analyses$TypeofPresentation, #x-axis variable
                 trace.factor = df_analyses$TypeofControl, #variable for lines
                 response = df_analyses$Anxiety, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "Anxiety",
                 xlab = "Presantation Type",
                 col = c("pink", "blue"),
                 lty= 4, #line type
                 lwd = 2, #line width
                 trace.label = "Control")

interaction.plot(x.factor = df_analyses$TypeofPresentation, #x-axis variable
                 trace.factor = df_analyses$TypeofControl, #variable for lines
                 response = df_analyses$Anxiety, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "Anxiety",
                 xlab = "Presantation Type",
                 col = c("pink", "blue"),
                 lty= 4, #line type
                 lwd = 2, #line width
                 trace.label = "Control")


interaction.plot(x.factor = df_analyses$TypeofPresentation, #x-axis variable
                 trace.factor = df_analyses$TypeofControl, #variable for lines
                 response = df_analyses$AQ_Total, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "AQ Scale",
                 xlab = "Presantation Type",
                 col = c("pink", "blue"),
                 lty= 4, #line type
                 lwd = 2, #line width
                 trace.label = "Control")

interaction.plot(x.factor = df_analyses$TypeofPresentation, #x-axis variable
                 trace.factor = df_analyses$TypeofControl, #variable for lines
                 response = df_analyses$Social_Skills, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "Social Skills",
                 xlab = "Presantation Type",
                 col = c("pink", "blue"),
                 lty= 4, #line type
                 lwd = 2, #line width
                 trace.label = "Control")


interaction.plot(x.factor = df_analyses$TypeofPresentation, #x-axis variable
                 trace.factor = df_analyses$TypeofControl, #variable for lines
                 response = df_analyses$Attention_Switching, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "Attention Switching",
                 xlab = "Presantation Type",
                 col = c("pink", "blue"),
                 lty= 4, #line type
                 lwd = 2, #line width
                 trace.label = "Control")


interaction.plot(x.factor = df_analyses$TypeofPresentation, #x-axis variable
                 trace.factor = df_analyses$TypeofControl, #variable for lines
                 response = df_analyses$Attention_toDetail, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "Attention to Detail",
                 xlab = "Presantation Type",
                 col = c("pink", "blue"),
                 lty= 4, #line type
                 lwd = 2, #line width
                 trace.label = "Control")

interaction.plot(x.factor = df_analyses$TypeofPresentation, #x-axis variable
                 trace.factor = df_analyses$TypeofControl, #variable for lines
                 response = df_analyses$Communication, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "Communication",
                 xlab = "Presantation Type",
                 col = c("pink", "blue"),
                 lty= 4, #line type
                 lwd = 2, #line width
                 trace.label = "Control")



interaction.plot(x.factor = df_analyses$TypeofPresentation, #x-axis variable
                 trace.factor = df_analyses$TypeofControl, #variable for lines
                 response = df_analyses$Imagination, #y-axis variable
                 fun = mean, #metric to plot
                 ylab = "Imagination",
                 xlab = "Presantation Type",
                 col = c("pink", "blue"),
                 lty= 4, #line type
                 lwd = 2, #line width
                 trace.label = "Control")




# Additional - Internal Consistency 


df_consistency <- df_alldata  %>%
  select(TypeofPresentation, TypeofControl, AQ1:AQ50, Dass1 : Dass21)

df_consistency

df_consistency  %>% select(TypeofPresentation, TypeofControl,contains("DASS")) %>% group_by(TypeofPresentation, TypeofControl) %>% do(alpha(.)$total)

df_consistency  %>% select(TypeofPresentation, TypeofControl,contains("AQ")) %>% group_by(TypeofPresentation, TypeofControl) %>% do(alpha(.)$total)





