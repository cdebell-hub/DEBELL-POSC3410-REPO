# Title: POSC-3410-DAL3 ####
# Author: Curtis DeBell
# Date: January 25, 2021

# Lesson 1 ####
library(tidyverse)

# read_csv practice
read_csv("gtd_raw.csv")

# read gtd_raw.csv assign to "gtd_raw"
gtd_raw <- read_csv("gtd_raw.csv")

# check str 
str(gtd_raw)

# view gtd_raw
View(gtd_raw)

# assign gtd_raw to gtd_df
gtd_df <- gtd_raw

# Select the columns we will need to make our visualizations - List these in the prompt. 
gtd_df <- gtd_df %>% 
  select(eventid, iyear, imonth, iday, summary, country, country_txt, region, region_txt, provstate, city, crit1, crit2, crit3, doubtterr, success, suicide, attacktype1, attacktype1_txt, attacktype2, attacktype2_txt, attacktype3, attacktype3_txt, targtype1, targsubtype1_txt, targtype2, targtype2_txt, targtype3, targtype3_txt, gname, gname2, gname3, nperps, nperpcap, motive, nkill, nkillus, nkillter, nwound, nwoundus, nwoundte, property, propextent, propextent_txt, propvalue, nhostkid, nhostkidus, nhours, ndays, ransom, ransomamt, ransompaid, hostkidoutcome, hostkidoutcome_txt, nreleased)

# check the structure of gtd_df
str(gtd_df)

# QuestionA: What do the rows (be careful and specific) and columns represent in this data set? Code a text string describing what you see to `Answer1` ####
AnswerA <- "the rows represent incident. the columns represent variables"

# QuestionB: What years does the data set span? Code the first year and last year in the numeric string ####
#HINT: Call summary(gtd_df$iyear)
summary(gtd_df$iyear)
AnswerB <- c(1970, 2018)

# Create Visualization of Number of Terrorist Attacks per Year - Fill in the blanks on the Bar Chart ####
gtd_df %>% 
  filter(crit1 == 1 & crit2 == 1 & crit3 == 1) %>% 
  group_by(iyear) %>% # Be sure to add the correct group_by variable. 
  count() %>% 
  rename(`Number of Attacks` = n, Year = iyear) %>% 
  ggplot(aes(x=Year, y=`Number of Attacks`))+ # Be sure to add the correct x and y variables. 
  geom_bar(stat="identity")

# Question 1: What trends do we see? Does anything look peculiar? Code a text string describing what you see to `Answer1`####
Answer1 <- "Data is missing for 1993."

# Use a table to identify how many terrorist attacks there were per year 
gtd_df %>% 
  filter(crit1 == 1 & crit2 == 1 & crit3 ==1) %>% 
 group_by(iyear)%>% 
  count()%>% 
  View()

# Question 2: What year shows no terrorist attacks? Why are we missing it. (HINT: refer to the GTD Code Book). Code your answer as a text string called 'Answer2'.  ####
Answer2 <- "1993, the data was lost prior to when START began compiling the data."

# Question 3: In how many incidents is there doubt as to whether the incident is terrorism? Code answer as numeric vector.####
# View the code book to find which variable tells us if there is doubt whether the indicident is terorism. We will need to group and count. 

# Code goes here
gtd_df %>%
  group_by(doubtterr)%>%
  count()%>%
  view()

# Write your answer.
Answer3<-c(31060)

# Question 4: What percent of the total number of incidents in the data frame is there doubt about whether it is terrorism. Show both code and assign answer to a numeric vector? ####
13785+146619+31060
31060/191464*100

Answer4<- c(16.22237)

# Question 5:  What were the top 3 years in terms of number of terrorist attacks.Show both code and answer. Assign a numeric vector to Answer5. ####

gtd_df %>% 
  group_by(iyear)%>% 
  count()%>%  
  arrange(desc(n))

Answer5 <- c(2014,2015,2016) 


gtd_casualties_df <- gtd_df %>% 
  filter(iyear >= 1970, crit1 == 1 & crit2 == 1 & crit3 == 1) %>% 
  group_by(iyear) %>%
  summarize(incidents = n(), casualities = (sum(nkill, na.rm = TRUE) + sum(nwound, na.rm = TRUE)), victims_killed = sum(nkill, na.rm = TRUE), victims_wounded = sum(nwound, na.rm = TRUE)) 

# Question 6: How many incidents, per year, did we exclude because they did not have casualty data? What percentage have we been missing per year? Answer this question by creating a dataframe with year, number of incidents missing data in nkill or nwound,  number of incidents, and calculate the percent per year that are missing. #####

# Create a dataframe with year and count of missing data. HINT: You will have to add conditions to filter.
missing_casualties_data <- gtd_df %>% 
  filter(iyear >= 1970, crit1 == 1 & crit2 == 1 & crit3 == 1 & (is.na(nkill) | is.na(nwound))) %>% 
  group_by(iyear)%>%  
  count()%>%  
  rename(missing = n)

# Add total incidents per year to missing_casualties_data
missing_casualties_data <- left_join(missing_casualties_data, gtd_casualties_df, by = "iyear" ) 

missing_casualties_df <- missing_casualties_data %>% 
  select(iyear : incidents) %>% 
  mutate(perc_missing = missing/incidents*100 ) # Fill in this blank with the correct formul

Answer6 <- missing_casualties_df


# Question7: What is the overall trend in the lethality of terrorist incidents? You need to answer this question with both a plot and a text description ####

# Create a data frame with year, number of incidents, number of casualties, number of victims killed, number of victims wounded, number of casualties / incident, number of victims killed /incident, number of victims wounded / incident. 
gtd_casualties_df <- gtd_casualties_df %>% 
  mutate(casualties_per_inc = casualities / incidents, victims_killed_per_inc = victims_killed / incidents, victims_wounded_per_inc = victims_wounded / incidents) 

# Create a line plot of victims_killed_per_inc by year
gtd_casualties_df %>% 
  rename(year = iyear) %>% 
  ggplot(aes(x=year)) + 
  geom_line(aes(y=victims_killed_per_inc), color = "red") 

# Create a line plot of victims_wounded_per_inc by year
gtd_casualties_df %>% 
  rename(year = iyear) %>% 
  ggplot(aes(x=year)) + 
  geom_line(aes(y=victims_wounded_per_inc), color = "darkred")

# Create a plot that Combines all these lines
casualties_plot <- gtd_casualties_df %>% 
  rename(year = iyear) %>% 
  ggplot(aes(x=year))

# Add lines to casualties_plot
casualties_plot <- casualties_plot +
  geom_line(aes(y=victims_killed_per_inc, color = "Victims Killed per Incident")) + # Add the correct ggplot geometric layer. 
  geom_line(aes(y=victims_wounded_per_inc, color = "Victims Wounded per Incident")) # Add the correct ggplot geometric layer. 

# Add a legend 
colors <- c("Victims Killed per Incident" = "red", "Victims Wounded per Incident" = "darkred")
casualties_plot +
  scale_color_manual(values = colors )+
  labs(color = "",
       y="Number") +
  theme(legend.position = "bottom")

Answer7_plot <- casualties_plot
Answer7 <- "The gap between victims killed an victims wounded drasticaly widened in the late 90s and early 2000s."



# Question8: What is the relationship between amount of ransom terrorists demanded and the actual amount of ransom paid. Do incidents in all regions follow the same pattern (i.e., add region as color or facet wrap, which one works better?)? Answer this question using a scatterplot and a text vector description? HINT: You may need to make 2 scatter plots (1 that includes all data and one that filters out outliers) ####
# Base Scatterplot 
gtd_df %>% 
  ggplot(aes(x=ransomamt, y=ransompaid))+
  geom_point()

# Color Approach 
# HINT: the format for scientific notation in R is 1e03 = 1000
gtd_df %>% 
  filter(ransompaid<1e08 & ransomamt < 2.5e08) %>% 
  ggplot(aes(x=ransomamt, y=ransompaid, color=region_txt))+
  geom_point()

# Facet Wrap Approach 
gtd_df %>% 
  filter(ransompaid<1e08 & ransomamt < 2.5e08) %>%  
  ggplot(aes(x=ransomamt, y=ransompaid))+
  geom_point()+ 
  facet_wrap(~region_txt, scales = "free", nrow=4)

Answer8_plot <- gtd_df %>% 
  filter(ransompaid<1e08 & ransomamt < 2.5e08) %>%  
  ggplot(aes(x=ransomamt, y=ransompaid))+
  geom_point()+ 
  facet_wrap(~region_txt, scales = "free", nrow=4) 

Answer8<- "Ransom amounts to do not achieve the same success rates in different regions. They tend to be the most succesful in the Middle East/North Africa and Southeast Asia. They are the least succesful in Western Europe and North America"

