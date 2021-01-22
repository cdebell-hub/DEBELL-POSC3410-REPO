# Title: POSC-3410 ####
# Author: Curtis DeBell
# Date: January 20, 2021

# Lesson 1 ####

# install.package("tidyverse")

# Lesson 2 ####

# Load tidyverse package
library(tidyverse)
## -- Attaching packages ---------------------------------- tidyverse 1.3.0 --
## v ggplot2 3.3.1     v purrr   0.3.4
## v tibble  3.0.1     v dplyr   1.0.0
## v tidyr   1.1.0     v stringr 1.4.0
## v readr   1.3.1     v forcats 0.5.0
## -- Conflicts ------------------------------------- tidyverse_conflicts() --
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()

# Load data
data(mpg)

# Check the structure of mpg using str()
# Answer below (using a comment) what type of data structure mpg is?
# Vector

# Create analysis of data frame: We do this so we preserve a raw version of the dataframe to which we can
mpg_df<-mpg

# Check the structure of mpg_df using str()

# Scatter plot of displ x hwy
ggplot(mpg_df, aes(x=displ , y=hwy ))+
  geom_point()

# Scatter plot of displ x hwy by fuel type (fl)
ggplot(mpg_df, aes(x=displ , y=hwy, color=fl ))+
  geom_point()

# On Your Own, Scatter plot of displ x hwy by class

# Scatter plot of displ x hwy
ggplot(mpg_df, aes (x=displ , y=class ))+
  geom_point()

# Lesson 3 ####

# Call help for filter
?filter()

# Call help for geom_bar()
?geom_bar()

# Call help for facet_wrap()
?facet_wrap()

# ggplot - data layer, assign it to mpg_data
mpg_data<- ggplot(data= mpg)

# Call mpg_data

# Describe the output you see in the console.
Vector

# Now, let's add an aesthetic layer to the plot.

# Put engine size on the x-axis, highway mileage on y-axis, and color by class.
mpg_plot<-mpg_data+geom_point(mapping =aes(x=displ , y=hwy, color=class))

# Call mpg_plot.

# What do you see?
Error code, unable to get plot

# Now, let's add an aesthetic layer to change the axis labels
mpg_axis <- mpg_plot+
xlab("Engine Size in Liters")+
ylab("Highway Mileage")+
ggtitle("Engine Size and Mileage by Vehicle Class")

# Call mpg_axis

# What do you see?
Error: object 'mpg_plot' not found

# Create different graphs for each class
mpg_wrap<-mpg_axis+
  facet_wrap(~class, nrow=2)

# Call mpg_wrap

# What do you see?

# Re-assign mpg_wrap by adding facet wrapping, change nrow to 3.

# What do you see?

# Lesson 4 ####

library(tidyverse)
library(nycflights13)

# Filter to keep only the flights that happened on 3-14.
filter(flights, month==3, day==14)

# Assign flights that happened on 3-14 to new variable:`Mar14`.

# Select flights that happened in either May or June
filter(flights, month==5|month==6)

# Filter flights  whose arrival was delayed over 60 minutes AND departed on time.
filter(flights, (arr_delay >  60 & dep_delay < 1))

# Filter flights whose arrival was delayed over 60 minutes AND departure was delayed less than 15 minutes.
filter(flights, (arr_delay > 60 & dep_delay < 15))

# Filter flights whose arrival was delayed over 60 minutes AND departure was delayed 15 minutes or more (i.e., remember to use your Greater Than or Equal To Operator).
filter(flights, (arr_delay > 60 & dep_delay > 15))

# Filter flights whose destination is Greenville-Spartanburg International Airport (GSP)
filter(flights, arr_location == GSP)

# Filter flights that were in the air over 3 hours (180 mins)
filter(flights, lenght > 180)

# Effects of NAs on Logical Operations
NA==99

NA>3
  
NA<3

# Number of Wins for Clemson Football in the 2021 season
Clemson <- NA

# Number of Wins for Ohio State football in the 2021 season
OSU <- NA
# Do Clemson and OSU have the same number of wins
Clemson== OSU
FALSE

# Interpret this result using a comment your script file.
FALSE

# use is.na(varName) to find whether it is NA. This command returns a logical vector of either TRUE or FALSE. See the example below.
is.na(Clemson)

is.na(OSU)

is.na(flights)

# Describe what you see.
Error codes

# Let's see what happens when we combine filter() and is.na(). We will search for NAs in arrival delay.
filter(flights,is.na(arr_delay))

# We will search for observations with NO NAs in arrival delay.
filter(flights,!is.na(arr_delay))

# In the real world, we often want to know how many observations are in each subset data frame. We will do this by calling nrow() outside of the filter commands.
# Number of observations in flights data set with missing data for arrival delay.
nrow(filter(flights,is.na(arr_delay)))

# Number of observations in flights data set with NO missing data for arrival delay.
nrow(filter(flights,!is.na(arr_delay)))

# On your own, number of observations in flights data set with missing data for departure delay.
nrow(filter(flights, is.na(dep_delay)))

# On your own, number of observations in flights data set with no missing data for departure delay.
nrow(filter(flights,!is.na(dep_delay)))

# In the real world, we often will filter out NAs across several variables and assign the resulting data frame to a new variable.
flights_df<-filter(flights, (!is.na(arr_delay)& !is.na(dep_delay)))

# Call flights_df
flights_df

# On your own, filter out NAs across year, month, dep_delay, arr_delay, carrier, flight, air_time. Assign the resulting data frame to a new variable: flights_df
nrow(filter(flights, ! is.na(year, month, dep_delay, arr_delat, carrier, flight, air_time)))

# Call flights_df

# Create sample dataframe  using sequence (use the help command to learn more about sequence)
sample<-sequence(10, from=10L, by= - 1L )
sample_df <-as_tibble(sample)

# Call sample_df

# Describe what the results that were returned.

# Arrange from 1 to 10.
arrange(sample_df, value)

# Describe what the results that were returned.
# Create sample dataframe with values from 1:100  using sequence.
sample<-sequence
(100, from=1L, by=1L )sample_df <-as_tibble(sample)

# Call sample_df

# Describe what the results that were returned.

# Arrange sample_df$value in descending order using the syntax, arrange(df, desc(varible))
arrange(df, desc(order))

# Describe what the results that were returned.

# What happens when we use arrange on a real life data frame (i.e., one that has missing values).
It won't run

# Arrange flights by arrival delay and assign to arrange_df
arrange_df<-arrange(flights, arr_delay)

# Call the last six rows of  (call help on tail() to find out more)
tail(arrange_df)

# Lets try arranging data on your own.
# Arrange flights to find the most delayed flights.
arrange(df, desc(dep_delay))

# Arrange the flights that left earliest
arrange(df, asc(dep_delay))

# Example use of select
select(flights, year, month, day, carrier, flight)

# Select call columns between year and arrival delay
select(flights, year:arr_delay)

# Select all columns except tailnum
select(flights,tailnum)

# Select all columns between dep_delay and time_hour.

# Select the following columns: year, month, day, dep_delay, arr_delay, dest, distance. Assign to flights_df.
# call flights_df

# Check the column names
names(sample_df)

# Rename sample_df$value as sample_df$count
rename(sample_df, count = value)

# Rename sample_df$value as sample_df$count. Assign to sample_df
sample_df <-
rename(sample_df, count = value)

# To be completed on your own.
rename(sample_df, count = value)

# Check column names in flights
names(flights)

# Rename arr_delay as`arrival_delay`. Assignt to flights.

# Rename dep_delay as`departure_delay'. Assign to flights.

# Call flights
Call(flight_df)

# Create dataframe with fewer variables:  flights_small
flights_small <-
  select(flights,
  year:day,
  ends_with
  ("delay"),
  distance,
  air_time,
  dest
)

# Create new column: avg_speed.
mutate(flights_small,
  speed = distance/ air_time*60)

# You can also use mutate to change existing variables.
# Change destination to factor
mutate(flights_small,dest =factor(dest))

# Create column`gain`using formula`dep_delay`- `arr_delay`.
mutate(flights_small)
  gain =  dep_delat - arr_delay

# Create new column for arrival delay in hours (i.e., divide by 60)
mutate(flights_small)
  arr_delay hours = arr_delay/60
  
# Create new column for departure delay in hours (i.e., divide by 60)
mutate(flights_small)
  dep_delay hours = dep_delay/60
  
#Change year to character
  
# Create new character variable using paste0 (call help to learn more and check the hint below) to combine 
  
# Verse 1 as string
almaMater1<-"Where the Blue Ridge yawns its greatness; Where the Tigers play; Here the sons of dear Old Clemson, Reign supreme alway."

# Verse 2 as string
almaMater2<-"We will dream of great conquests For our past is grand, And her sons have fought and conquered Every foreign land."

# Verse 3 as string
almaMater3 <- "Where the mountains smile in grandeur O'er the hill and dale; Here the Tiger lair is nestling Swept by storm and gale."
  
# Verse 4 as string
almaMater4 <- "We are brothers strong in manhood For we work and strive; and our Alma Mater reigneth Forever in our lives."
  
# Chorus as string
almaMaterChorus <- "Dear Old Clemson, we will triumph And with all our might That the Tiger's roar may echo O'er the mountain height."
  
# Combine vectors to create almaMater
almaMater<-paste0(almaMater1," ", almaMater2," ", almaMater3," ", almaMater4," ", almaMaterChorus)

# print almaMater to console.
almaMater  

# Use summarise to calculate avg (mean) arrival delay.
summarise(flights, avgArrDelay=mean(arr_delay, na.rm=TRUE))

# Use group_by and summarise to calculate avg arrival delay for all the carriers.

# Group flights by carrier: flights_carrier
flights_carrier <-group_by(flights, carrier)

#summarise average delay
summarise(flights_carrier, avgArrDelay=mean(arr_delay, na.rm=TRUE))

# Summarise standard deviation of distance to destinations
summarise(flights, )

# Create new data frame: not_cancelled
not_cancelled <-filter(flights,!is.na(dep_delay) & !is.na(arr_delay))

# Group by month, mean dep_delay
not_cancelled_year<-group_by(not_cancelled, month)

# Group by carrer, summarise mean dep_delay and mean arrival delay
not_cancelled<-group_by(mean dep_delay, mean arr_delay)

# Group by carrier, summarise sd of distance traveled
not_cancelled<-group_by(summarise distance)

# Filter for origin=="LGA". When do the first flights depart? when do they arrive?
filter(origin == LGA)

# Lesson 5 ####

# Load nycflights13 packages
library(nycflights13)

# Load data
data("flights")

# Filter the data for November and December flights, count number of flights that arrived later than 59 minutes. Which destination airports are worst?
# Filter, assign new varName
flights_extract<-filter(flights, month==11|month==12&arr_delay >60)

# Group by destination
flights_extract <-group_by(flights_extract,dest )

# Count
flights_extract <-count(flights_extract)

# Rename
flights_extract <-rename(flights_extract, number = n )

# Arrange
flights_extract <-arrange(flights_extract,desc(number))

# Keep only top 6 rows
flights_extract <-head(flights_extract, n=6)

# Make ggplot bar graph
ggplot(flights_extract,aes(x=reorder(dest,-number), y=number))+
  geom_bar(stat="identity")

# Now look at the same operations with pipes.
flights%>%
  filter(month==11|month==12&arr_delay>60)%>%
  group_by(dest)%>%count()%>%
  rename(number=n)%>%
  arrange(desc(number))%>%
  head(6)%>%
  ggplot(aes(x=reorder(dest,-number), y=number))+     
  geom_bar(stat="identity")     
       
# Filter the flights data to keep only May and June flights
filter(flights, month =5|6)

# Filter the flights data to keep only April
filter(flights, month =4)

# Filter the flights data to keep only Delta flights
filter(flights, carrier =  delta)

# Which airport had the most flights from it to NYC's 3 airports? Filter flights data to keep only flights that arrived on-time or early, group by origin airport, arrange in descending order.
filter(flights, arr_delay < 0, group(origin), arrange(desc))

# Filter flights to keep only flights that departed JFK and arrived at ATL. Keep only origin, destination, arrival delay,  departure delay,and carrier. Create a ggplot scatterplot showing the relationship between departure delay and  arrival delay. Add carrier as color.

# To which airports did the most flights from NYC go? Make a ggplot visualization.

# Which carrier had the shortest mean arrival delay?

# Which carrier experienced the most cancelled flights?

# On average, which carrier flew fastest.
