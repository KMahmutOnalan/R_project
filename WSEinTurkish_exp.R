
# Experiment : WSE - Reicher Wheeler Paradigm
# Paired Sample t -test 
# Author : Kübra Mahmut Önalan 

# 1. Packages
library (tidyverse)
library (ggpubr) 
library (rstatix) 
library(stats) 
library (dplyr) 
library(readxl) 
library(ggplot2)

# 2. Set Working Direct
setwd("")

# 3.Create Df

Rw <- read_excel("")

View(Rw)

df_groups <- data.frame( 
  condition = rep(c("Word", "Non Word"), each = 34),
  average = c(Rw$word_average, Rw$non_word_average)
)


average1 <- data.frame (df_groups$average * 100)


df_groups1 <- cbind.data.frame(df_groups,average1)


df_groups1$condition<-as.factor(df_groups1$condition)

# Check outliers and normallity with shapiro_w test 

df_groups1   %>% group_by(condition)  %>% identify_outliers(df_groups.average...100)

df_groups1  %>% group_by(condition) %>% shapiro_test(df_groups.average...100)

#perform paired t-test

result_ttest <- t.test(df_groups.average...100 ~ condition, data = df_groups1, paired = TRUE)
result_ttest


# Create a function data_summary for bar plot (error bar)

#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

# Data_summary fonksiyonuyla mean and sd hesaplandı


df3 <- data_summary(df_groups1, varname="df_groups.average...100", 
                    groupnames=c("condition"))
# Convert groups to a factor variable

df3$condition=as.factor(df3$condition)
head(df3)


# create a barplot 

p <- ggplot(df3, aes(x=condition, y= df_groups.average...100, fill=condition)) + 
  geom_bar(stat="identity",color = "black", width=.5, position=position_dodge(.9)) +
  geom_errorbar(aes(ymin=df_groups.average...100-sd, ymax=df_groups.average...100+sd), width=.2,
                position=position_dodge(.9)) + 
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
  
  xlab("Stimulie Types")+
  ylab ("Accurancy Rate %") + 
  theme(legend.position =  "none", plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette="Paired") + 
  scale_x_discrete(limits=c("Word", "Non Word"))+
  theme_classic()

p



