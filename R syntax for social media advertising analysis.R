# Social Media Advertising Dataset

#clearing R environment
rm(list = ls())

library(tidyverse)
library(dplyr)
library(patchwork) #package to create layout of multiple figures on single page
library(scales) #package used to add commas to text in ggplot
library(sf) # For working with spatial data
library(ggpubr) #used to output table
library(grid) #used for adding text annotation to table
library(patchwork) #package to create layout of multiple figures on single page
library(corrplot) #package for correlation plots

#setting working directory
setwd('/Users/josephrios/Desktop/Data Science Portfolio Projects/Social Media Advertising Data/')
#reading in dataset
data<-read.csv('Advertising_data_V2.csv')
View(data)


#################################
# CAMPAIGN PERFORMANCE ANALYSIS #
#################################

#  Objective: Evaluate the performance of advertising campaigns across different social media platforms.

# Are there differences in ROI by channel

#STEP 1 group data by channel

roi_grouped <- data %>%
  group_by(Channel_Used) %>%
  summarize(avg_roi=as.numeric(round(mean(ROI),2)),
            avg_clicks=as.numeric(round(mean(Clicks),2)),
            avg_impressions=as.numeric(round(mean(Impressions),2)),
            avg_conversion=as.numeric(round(mean(Conversion_Rate),2)),
            n = n())

plot_impressions<-ggplot(roi_grouped,aes(x=Channel_Used,y=avg_impressions)) +
  geom_bar(stat="identity",fill="#003542") +
  geom_text(aes(label=comma(avg_impressions)),hjust=.5,vjust=2.5,size=3.5,color="white")+
 scale_y_continuous(limits=c(0,80000),breaks=c(0,20000,40000,60000,80000),labels=c("0","20,000","40,000","60,000","80,000")) +
  labs(title="Average Impressions",
       x="Social Media Channel",
       y="Average Impressions") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering

plot_clicks<-ggplot(roi_grouped,aes(x=Channel_Used,y=avg_clicks)) +
  geom_bar(stat="identity",fill="#003542") +
  geom_text(aes(label=comma(avg_clicks)),hjust=.5,vjust=2.5,size=3.5,color="white")+
  scale_y_continuous(limits=c(0,30000),breaks=c(0,10000,20000,30000),labels=c("0","10,000","20,000","30,000")) +
  labs(title="Average Clicks",
       x="Social Media Channel",
       y="Average Clicks") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering


plot_conversions<-ggplot(roi_grouped,aes(x=Channel_Used,y=as.numeric(avg_conversion))) +
  geom_bar(stat="identity",fill="#00b3c1") +
  geom_text(aes(label=format(avg_conversion,nsmall=2)),hjust=.5,vjust=2.5,size=3.5,color="white")+
  scale_y_continuous(limits=c(0,.2),breaks=c(.0,.05,.1,.15,.20)) +
  labs(title="Average Conversion Rate",
       x="Social Media Channel",
       y="Average Conversion Rate") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering

plot_roi<-ggplot(roi_grouped,aes(x=Channel_Used,y=avg_roi)) +
  geom_bar(stat="identity",fill="#da4554") +
  geom_text(aes(label=format(avg_roi,nsmall=2)),hjust=.5,vjust=2.5,size=3.5,color="white")+
  scale_y_continuous(limits=c(0,25),breaks=c(0,5,10,15,20,25)) +
  labs(title="Average ROI",
       x="Social Media Channel",
       y="Average ROI") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering

combined_plot<-(plot_impressions + plot_clicks) / (plot_conversions + plot_roi)

# Saving table

png("Plot - social media outcomes differences.png",
    width = 2700,      # Width in pixels
    height = 3200,     # Height in pixels
    res = 300,        # Resolution in DPI (dots per inch)
    units = "px",     # Units for width and height (pixels)
    bg = "white")      # Background color
print(combined_plot)        # Draw the plot
dev.off()             # Close the graph

#### correlation amongst dependent variables

cor_data<-data %>%
  select(Duration,Impressions,Clicks,Conversion_Rate, ROI) %>%
  rename('Conversion Rate'=Conversion_Rate)
 

correlation_matrix<-cor(cor_data[,sapply(cor_data,is.numeric)],use="pairwise.complete.obs")

# Saving table

png("Plot - correlation matrix of outcome variables.png",
    width = 2000,      # Width in pixels
    height = 1500,     # Height in pixels
    res = 300,        # Resolution in DPI (dots per inch)
    units = "px",     # Units for width and height (pixels)
    bg = "white")      # Background color

corr_plot<-corrplot(correlation_matrix, method = "color", type = "upper",
         order = "original", tl.col = "black", tl.srt = 45, addCoef.col = "red",number.cex=.8,
      )


dev.off()             # Close the graph



####################################
#.   CUSTOMER SEGMENTATION         #
####################################

#recode age and gender 

recoded_demo_data<-data %>%
  mutate(gender=case_when(
    Target_Audience=="Men 18-24" | Target_Audience=="Men 25-34" | Target_Audience=="Men 35-44" | Target_Audience=="Men 45-60" ~ "Men",
    Target_Audience=="Women 18-24" | Target_Audience=="Women 25-34" | Target_Audience=="Women 35-44" | Target_Audience=="Women 45-60"~"Women",
    Target_Audience=="All Ages"~"All"),
    age=case_when(
      Target_Audience=="Men 18-24" | Target_Audience=="Women 18-24" ~ "18-24",
      Target_Audience=="Men 25-34" | Target_Audience=="Women 25-34" ~ "25-34",
      Target_Audience=="Men 35-44" | Target_Audience=="Women 35-44" ~ "35-44",
      Target_Audience=="Men 45-60" | Target_Audience=="Women 45-60" ~ "45-60",
      Target_Audience=="All Ages" ~ "All"
          ))
                    
print(recoded_demo_data)

#performing ANOVA to evaluate whether ROI differs by age and/or gender
anova_gender_roi <- aov(ROI ~ gender, data=recoded_demo_data)
summary(anova_gender_roi)

# If ANOVA is significant, perform post-hoc tests
if (summary(anova_gender_roi)[[1]]$'Pr(>F)'[1] < 0.05) {
  TukeyHSD(anova_gender_roi) # Or another post-hoc test
}

#performing ANOVA to evaluate whether ROI differs by age and/or gender
anova_age_roi <- aov(ROI ~ age, data=recoded_demo_data)
summary(anova_age_roi)

# If ANOVA is significant, perform post-hoc tests
if (summary(anova_age_roi)[[1]]$'Pr(>F)'[1] < 0.05) {
  TukeyHSD(anova_age_roi) # Or another post-hoc test
}

################################
# PREPPING DATA FOR HISTOGRAMS #
################################

conversion_gender_data<-recoded_demo_data %>%
  filter(Campaign_Goal=="Brand Awareness" | Campaign_Goal=="Product Launch") %>%
  group_by(gender) %>%
  summarize(gender_conversion=mean(Conversion_Rate,na.rm=TRUE),
            gender_n=n()) 

roi_gender_data<-recoded_demo_data %>%
  filter(Campaign_Goal=="Increase Sales" | Campaign_Goal=="Market Segmentation") %>%
  group_by(gender) %>%
  summarize(gender_roi=mean(ROI,na.rm=TRUE),
            gender_n=n()) 

conversion_age_data<-recoded_demo_data %>%
  filter(Campaign_Goal=="Brand Awareness" | Campaign_Goal=="Product Launch") %>%
  group_by(age) %>%
  summarize(age_conversion=mean(Conversion_Rate,na.rm=TRUE),
            age_n=n())

roi_age_data<-recoded_demo_data %>%
  filter(Campaign_Goal=="Increase Sales" | Campaign_Goal=="Market Segmentation") %>%
  group_by(age) %>%
  summarize(age_roi=mean(ROI,na.rm=TRUE),
            age_n=n()) 

conversion_language_data<-recoded_demo_data %>%
  filter(Campaign_Goal=="Brand Awareness" | Campaign_Goal=="Product Launch") %>%
  group_by(Language) %>%
  summarize(language_conversion=mean(Conversion_Rate,na.rm=TRUE),
            language_n=n())

roi_language_data<-recoded_demo_data %>%
  filter(Campaign_Goal=="Increase Sales" | Campaign_Goal=="Market Segmentation") %>%
  group_by(Language) %>%
  summarize(language_roi=mean(ROI,na.rm=TRUE),
            language_n=n()) 

conversion_location_data<-recoded_demo_data %>%
  filter(Campaign_Goal=="Brand Awareness" | Campaign_Goal=="Product Launch") %>%
  group_by(Location) %>%
  summarize(location_conversion=mean(Conversion_Rate,na.rm=TRUE),
            location_n=n())

roi_location_data<-recoded_demo_data %>%
  filter(Campaign_Goal=="Increase Sales" | Campaign_Goal=="Market Segmentation") %>%
  group_by(Location) %>%
  summarize(location_roi=mean(ROI,na.rm=TRUE),
            location_n=n()) 

conversion_industry_data<-recoded_demo_data %>%
  filter(Campaign_Goal=="Brand Awareness" | Campaign_Goal=="Product Launch") %>%
  group_by(Customer_Segment) %>%
  summarize(industry_conversion=mean(Conversion_Rate,na.rm=TRUE),
            industry_n=n())

roi_industry_data<-recoded_demo_data %>%
  filter(Campaign_Goal=="Increase Sales" | Campaign_Goal=="Market Segmentation") %>%
  group_by(Customer_Segment) %>%
  summarize(industry_roi=mean(ROI,na.rm=TRUE),
            industry_n=n()) 

#########################
#. PLOTTING RESULTS 
#########################

#gender 
plot_gender_conversions<-ggplot(conversion_gender_data,aes(x=gender,y=as.numeric(gender_conversion))) +
  geom_bar(stat="identity",fill="#00b3c1") +
  geom_text(aes(label=format(round(gender_conversion,2),nsmall=2)),hjust=.5,vjust=1.5,size=3.5,color="white",fontface="bold")+
  scale_y_continuous(limits=c(0,.35),breaks=c(.0,.05,.1,.15,.20,.25,.30,.35)) +
  labs(title="Conversion Rate by Gender",
       x="Gender",
       y="Avg Conversion") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering

plot_gender_roi<-ggplot(roi_gender_data,aes(x=gender,y=as.numeric(gender_roi))) +
  geom_bar(stat="identity",fill="#da4554") +
  geom_text(aes(label=format(round(gender_roi,2),nsmall=2)),hjust=.5,vjust=1.5,size=3.5,color="white",fontface="bold")+
  scale_y_continuous(limits=c(0,25),breaks=c(0,5,10,15,20,25)) +
  labs(title="ROI by Gender",
       x="Gender",
       y="Avg ROI") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering

#age
plot_age_conversions<-ggplot(conversion_age_data,aes(x=age,y=as.numeric(age_conversion))) +
  geom_bar(stat="identity",fill="#00b3c1") +
  geom_text(aes(label=format(round(age_conversion,2),nsmall=2)),hjust=.5,vjust=1.5,size=3.5,color="white",fontface="bold")+
  scale_y_continuous(limits=c(0,.35),breaks=c(.0,.05,.1,.15,.20,.25,.30,.35)) +
  labs(title="Conversion Rate by Gender",
       x="Gender",
       y="Avg Conversion") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering

plot_age_roi<-ggplot(roi_age_data,aes(x=age,y=as.numeric(age_roi))) +
  geom_bar(stat="identity",fill="#da4554") +
  geom_text(aes(label=format(round(age_roi,2),nsmall=2)),hjust=.5,vjust=1.5,size=3.5,color="white",fontface="bold")+
  scale_y_continuous(limits=c(0,25),breaks=c(0,5,10,15,20,25)) +
  labs(title="ROI by Age",
       x="Age",
       y="Avg ROI") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering


#language
plot_language_conversions<-ggplot(conversion_language_data,aes(x=Language,y=as.numeric(language_conversion))) +
  geom_bar(stat="identity",fill="#00b3c1") +
  geom_text(aes(label=format(round(language_conversion,2),nsmall=2)),hjust=.5,vjust=1.5,size=3.5,color="white",fontface="bold")+
  scale_y_continuous(limits=c(0,.35),breaks=c(.0,.05,.1,.15,.20,.25,.30,.35)) +
  labs(title="Conversion Rate by Language",
       x="Language",
       y="Avg Conversion") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering

plot_language_roi<-ggplot(roi_language_data,aes(x=Language,y=as.numeric(language_roi))) +
  geom_bar(stat="identity",fill="#da4554") +
  geom_text(aes(label=format(round(language_roi,2),nsmall=2)),hjust=.5,vjust=1.5,size=3.5,color="white",fontface="bold")+
  scale_y_continuous(limits=c(0,25),breaks=c(0,5,10,15,20,25)) +
  labs(title="ROI by Language",
       x="Language",
       y="Avg ROI") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering


#location
plot_location_conversions<-ggplot(conversion_location_data,aes(x=Location,y=as.numeric(location_conversion))) +
  geom_bar(stat="identity",fill="#00b3c1") +
  geom_text(aes(label=format(round(location_conversion,2),nsmall=2)),hjust=.5,vjust=1.5,size=3.5,color="white",fontface="bold")+
  scale_y_continuous(limits=c(0,.35),breaks=c(.0,.05,.1,.15,.20,.25,.30,.35)) +
  labs(title="Conversion Rate by Location",
       x="Location",
       y="Avg Conversion") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering

plot_location_roi<-ggplot(roi_location_data,aes(x=Location,y=as.numeric(location_roi))) +
  geom_bar(stat="identity",fill="#da4554") +
  geom_text(aes(label=format(round(location_roi,2),nsmall=2)),hjust=.5,vjust=1.5,size=3.5,color="white",fontface="bold")+
  scale_y_continuous(limits=c(0,25),breaks=c(0,5,10,15,20,25)) +
  labs(title="ROI by Location",
       x="Location",
       y="Avg ROI") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))  

#industry
plot_industry_conversions<-ggplot(conversion_industry_data,aes(x=Customer_Segment,y=as.numeric(industry_conversion))) +
  geom_bar(stat="identity",fill="#00b3c1") +
  geom_text(aes(label=format(round(industry_conversion,2),nsmall=2)),hjust=.5,vjust=1.5,size=3.5,color="white",fontface="bold")+
  scale_y_continuous(limits=c(0,.35),breaks=c(.0,.05,.1,.15,.20,.25,.30,.35)) +
  labs(title="Conversion Rate by Industry",
       x="Industry",
       y="Avg Conversion") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold"))   #modifying main title by bolding and centering

plot_industry_roi<-ggplot(roi_industry_data,aes(x=Customer_Segment,y=as.numeric(industry_roi))) +
  geom_bar(stat="identity",fill="#da4554") +
  geom_text(aes(label=format(round(industry_roi,2),nsmall=2)),hjust=.5,vjust=1.5,size=3.5,color="white",fontface="bold")+
  scale_y_continuous(limits=c(0,25),breaks=c(0,5,10,15,20,25)) +
  labs(title="ROI by Industry",
       x="Industry",
       y="Avg ROI") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) + #removing minor gridlines
  theme(plot.title = element_text(face="bold", hjust=.5,size=16),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        axis.title.x=element_text(margin=margin(t=10,unit="pt"),size=12,face="bold"),
        axis.title.y=element_text(margin=margin(r=10,unit="pt"),size=12,face="bold")) 


#combining plots
combined_demo_plots<-(plot_gender_conversions + plot_gender_roi)/
  (plot_age_conversions + plot_age_roi)/
  (plot_language_conversions + plot_language_roi)/
  (plot_location_conversions + plot_location_roi)/
  (plot_industry_conversions + plot_industry_roi)
  

# Saving table

png("Plot - customer segmentation differences.png",
    width = 2700,      # Width in pixels
    height = 5000,     # Height in pixels
    res = 300,        # Resolution in DPI (dots per inch)
    units = "px",     # Units for width and height (pixels)
    bg = "white")      # Background color
print(combined_demo_plots)        # Draw the plot
dev.off()             # Close the graph



##############################################
#.  PREDICTING FUTURE CAMPAIGN PERFORMANCE.  #
##############################################
data_conversion_regression<-recoded_demo_data %>%
  filter(Campaign_Goal=="Brand Awareness" | Campaign_Goal=="Product Launch") %>%
  mutate(age=factor(age),gender=factor(gender),industry=factor(Customer_Segment),
  location=factor(Location),platform=factor(Channel_Used),duration=as.numeric(Duration)
         ) %>% select(age,gender,industry,location,platform,duration,Conversion_Rate,Impressions,Clicks)

data_roi_regression<-recoded_demo_data %>%
  filter(Campaign_Goal=="Increase Sales" | Campaign_Goal=="Market Segmentation") %>%
  mutate(age=factor(age),gender=factor(gender),industry=factor(Customer_Segment),
         location=factor(Location),platform=factor(Channel_Used),duration=as.numeric(Duration)
  ) %>% select(age,gender,industry,location,platform,duration,ROI,Conversion_Rate,Impressions,Clicks)


#subsetting training and testing data for both conversion and roi
# Train-test split for conversion data
train_conversion_size <- floor(0.8 * nrow(data_conversion_regression))
train_conversion_index <- sample(seq_len(nrow(data_conversion_regression)), size = train_conversion_size)
train_conversion_data <- data_conversion_regression[train_conversion_index, ]
test_conversion_data <- data_conversion_regression[-train_conversion_index, ]

# Train-test split for roi data
train_roi_size <- floor(0.8 * nrow(data_roi_regression))
train_roi_index <- sample(seq_len(nrow(data_roi_regression)), size = train_roi_size)
train_roi_data <- data_roi_regression[train_roi_index, ]
test_roi_data <- data_roi_regression[-train_roi_index, ]


# Model training conversion
model_conversion_regression <- lm(Conversion_Rate ~ age + gender + location + industry + platform + duration, data = train_conversion_data)
summary(model_conversion_regression)

# Model evaluation for conversion
predictions_conversion_regression <- predict(model_conversion_regression, newdata = test_conversion_data)
rmse_conversion <- sqrt(mean((predictions_conversion_regression - test_conversion_data$Conversion_Rate)^2))
print(paste("RMSE:", rmse_conversion))

# Model training roi
model_roi_regression <- lm(ROI ~ age + gender + location + industry + platform + duration + Conversion_Rate + Impressions + Clicks, data = train_roi_data)
summary(model_roi_regression)

# Model evaluation for roi
predictions_roi_regression <- predict(model_roi_regression, newdata = test_roi_data)
rmse_roi <- sqrt(mean((predictions_roi_regression - test_roi_data$ROI)^2))
print(paste("RMSE:", rmse_roi))


