#============================Library
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsci)
library(tidyverse)
library(readr)
library(readxl)
library(viridis)
#==================Loading_data
data <- read.delim("data.csv")
cholera_reported_deaths <- read.csv("CHOLERA_0000000002.csv")
fatalitydata=read_xlsx("cholera outbreak data.xlsx")

head(data)
#==================cleaning cases data
#1.sperate coulmns and clean data
data_clean <- data %>%
  separate(col =   country.year.Number_of_reported_cases, into = c("country", "year", "Number_of_reported_cases"), sep = ",|\\s+", extra = "merge")

# Convert 'year' and 'Number_of_reported_cases' to appropriate data types if needed
data_clean$year <- as.numeric(as.character(data_clean$year))

data_clean$Number_of_reported_cases <- as.numeric(as.character(data_clean$Number_of_reported_cases))

#delete "n/a'
data_clean<- data_clean %>%
  filter(year !='N/A')%>%
  filter(Number_of_reported_cases !='N/A')
# Check the structure of the dataset
str(data_clean)


#==============================================================cholera deaths data cleaning
cholera_reported_deaths$Number.of.reported.deaths.from.cholera <- as.numeric(cholera_reported_deaths$Number.of.reported.deaths.from.cholera)
glimpse(cholera_reported_deaths)
cleaned_cholera_reported_deaths <- cholera_reported_deaths[complete.cases(cholera_reported_deaths$Number.of.reported.deaths.from.cholera),] 
summary(cleaned_cholera_reported_deaths) #Min_year= 1949 and Max_year= 2016



#Grouping countries with the number of their mean deaths from 1949 to 2016
mean_deaths_per_country <- cleaned_cholera_reported_deaths %>% 
  group_by(`Countries..territories.and.areas`) %>% 
  summarise(mean_deaths = mean(`Number.of.reported.deaths.from.cholera`))
str(mean_deaths_per_country)
summary(mean_deaths_per_country) #Mean: 140.316

#Filtering the number of the death that has mean values that are equal to or more than the total means
mean_deaths_per_country <- mean_deaths_per_country[mean_deaths_per_country["mean_deaths"] >= 140.316,]
Countries_vector <- mean_deaths_per_country[["Countries..territories.and.areas"]]

# Grouping deaths per year
High_deaths_countries <- cleaned_cholera_reported_deaths %>% 
  filter(Countries..territories.and.areas %in% Countries_vector) %>% 
  group_by(`Year`, `Countries..territories.and.areas`)
High_deaths_countries

#===============================================================================
#Fatality data cleaning

fatalityclean <- fatalitydata %>%
  filter(`Cholera case fatality rate` != "Unknown" & `Cholera case fatality rate` != "0.0 0.0")

# change `Cholera case fatality rate` to numeric
cholera_data <- fatalityclean %>%
  mutate(`Cholera case fatality rate` = as.numeric(`Cholera case fatality rate`))

cholera_data

# Round cholera case fatality rates to whole numbers
cholera_data <- cholera_data %>%
  mutate(`Cholera case fatality rate` = round(`Cholera case fatality rate`, 0))
cholera_data

# Check for NAs in the 'Cholera case fatality rate' column
na_count <- sum(is.na(cholera_data$`Cholera case fatality rate`))
na_count

#=================================================================================
# Scatter plot for reported cases over years
ggplot(data_clean, aes(x = year, y = Number_of_reported_cases)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE) +
  ggtitle("Scatter Plot of Reported Cases Over the Years") +
  xlab("Year") +
  ylab("Number of Reported Cases") +
  theme_minimal()


# Scatter plot for fatality rates over the years
ggplot(cholera_data, aes(x = Year, y = `Cholera case fatality rate`, color = factor(Year))) +
  geom_point(alpha = 0.7) +  
  geom_smooth(method = "loess", se = FALSE, color = "black",  linewidth= 0.5) +  
  ggtitle("Cholera Fatality Rates Over the Years") +
  xlab("Years") +
  ylab("Cholera Case Fatality Rate") +
  theme_minimal() +
  scale_color_discrete(name = "Year") +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



### 1: Identify the Year with the Maximum Number of Cases for Each Country
outbreak_year_max <- data_clean %>%
  group_by(country) %>%
  filter(Number_of_reported_cases == max(Number_of_reported_cases)) %>%
  select(country, year, Number_of_reported_cases) %>%
  arrange(Number_of_reported_cases) %>% top_n(10)

## Top 10 countries in reported cases
top_countries <- outbreak_year_max %>%
  group_by(country) %>%
  select(country, year, Number_of_reported_cases) %>%
  summarize(Number_of_reported_cases = sum(Number_of_reported_cases, na.rm = TRUE)) %>%
  arrange(desc(Number_of_reported_cases)) %>%    # Sort in descending order of total cases
  top_n(10,Number_of_reported_cases)            # Select the top 10 countries

#Top 10 Countries in deaths deaths
Top10_deaths <-  arrange(mean_deaths_per_country, desc(mean_deaths)) %>% 
  top_n(10, mean_deaths)

# Top 10 countries in Fatalities
total_fatalities <- cholera_data %>%
  group_by(`Countries, territories and areas`) %>%
  summarise(Total_Fatalities = sum(`Cholera case fatality rate`, na.rm = TRUE)) %>%
  arrange(desc(Total_Fatalities)) %>%
  slice_head(n = 10) # Get top 10 countries

## Visualize Top 10 countries reported cases 
ggplot(top_countries, aes(x = reorder(country ,-Number_of_reported_cases), y = Number_of_reported_cases)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Countries of Reported Cases ", x = "Country", y = "Number of Reported Cases") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Bar chart for top 10 deaths
ggplot(Top10_deaths, aes( x = reorder(Countries..territories.and.areas, -(mean_deaths)), y = `mean_deaths`)) +
  geom_col( fill = "skyblue") +coord_flip()+
  labs(title = "Top 10 Countries in Reported Deaths",
       x ="Country",
       y = "Averge Reported Death Numbers") +
  theme(plot.title = element_text(hjust = 0.2)) 

# Create a bar chart for top 10 fatalities countries
ggplot(total_fatalities, aes(x = reorder(`Countries, territories and areas`, -Total_Fatalities), y = Total_Fatalities)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Countries by Total Cholera Fatalities",
       x = "Countries",
       y = "Total Fatalities") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

--------------------------------------------------------------------------------

#study the highest country with cases Between 1950 to 1960 "India", "Bangladesh", "Myanmar"  [OutBreak]
countries <- c("India", "Bangladesh", "Myanmar")  # Replace 'Meynmar' with correct spelling 'Myanmar'
data_filtered <- data_clean %>%
  filter(year >= 1950 & year <= 1960, country %in% countries)
# Stacked area chart to show the trends over the years 1950-1960
  ggplot(data_filtered, aes(x = year, y = Number_of_reported_cases, fill = country)) +
    geom_area(alpha = 0.7, color = "black") +  # Creates the stacked area chart
    ggtitle("Reported Cases in India, Bangladesh, and Myanmar (1950-1960)") +
    xlab("Year") +
    ylab("Number of Reported Cases") +
    theme_minimal()
  
 ------------------------------------------------------------------------------- 
  #study the highest country with Fatalities Between 1950 to 1960 "India", "Bangladesh", "Myanmar" 
  # Countries
  countries <- c("India", "Bangladesh", "Myanmar")
  
  # Filter the dataset for years between 1950 and 1960 and specified countries
  data_filtered <- cholera_data %>%
    filter(Year >= 1950 & Year <= 1960,
           `Countries, territories and areas` %in% countries)
  
  colnames(cholera_data)
  # line plot for fatality rates over the years by country
  ggplot(data_filtered, aes(x = Year, y = `Cholera case fatality rate`, group = `Countries, territories and areas`, color = `Countries, territories and areas`)) +
    geom_line(size = 1) + 
    geom_point(size = 2) + 
    labs(title = "Cholera Fatality Rates in India, Bangladesh and Myanmar (1950-1960)",
         x = "Year",
         y = "Fatality Rate (%)") +
    facet_wrap(~ `Countries, territories and areas`) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45)) 
  
#============================================================================"India"
countries <- c("India")  
data_filtered <- data_clean %>%
  filter(year >= 1950 & year <= 1960, country %in% countries)
# Stacked bar chart of India Between 1950 and 1960
ggplot(data_filtered, aes(x = factor(year), y = Number_of_reported_cases, fill = country)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  ggtitle("Stacked Bar Chart of Reported Cases in india (1950-1960)") +
  xlab("Year") +
  ylab("Number of Reported Cases") +scale_fill_manual(values = c("steelblue"))
  theme_minimal()+theme(axis.text.x = element_text(angle = 45))+scale_x_continuous(breaks = seq(1949, 1960, by = 1))
#============================================================================"Bangladesh"
countries <- c("Bangladesh")  
data_filtered <- data_clean %>%
  filter(year >= 1950 & year <= 1960, country %in% countries)
# Stacked bar chart of India Between 1950 and 1960
ggplot(data_filtered, aes(x = factor(year), y = Number_of_reported_cases, fill = country)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  ggtitle("Reported Cases in Bangladesh (1950-1960)") +
  xlab("Year") +
  ylab("Number of Reported Cases") +scale_fill_manual(values = c("darkgreen"))+
  
  theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
#============================================================================"Myanmar"
countries <- c("Myanmar")  
data_filtered <- data_clean %>%
  filter(year >= 1950 & year <= 1960, country %in% countries)
# Stacked area chart to show the trends over the years 1950-1960 In india
ggplot(data_filtered, aes(x = year, y = Number_of_reported_cases, fill = country)) +
  geom_area(alpha = 0.7, color = "black") +  # Creates the stacked area chart
  ggtitle("Reported Cases in India(1950-1960)") +
  xlab("Year") +
  ylab("Number of Reported Cases") +
  theme_minimal()

--------------------------------------------------------------------------------
#Specifying Indian Population deaths
India_deaths <- filter(cleaned_cholera_reported_deaths, Countries..territories.and.areas == "India")
fifties_India_deaths <- filter(cleaned_cholera_reported_deaths, Countries..territories.and.areas == "India" & Year >= 1950 & Year <= 1960)

#Specifying Bangladesh deaths from 1950 to 1960
fifties_Bangladesh_deaths <- filter(cleaned_cholera_reported_deaths, Countries..territories.and.areas == "Bangladesh" & Year >= 1950 & Year <= 1960)

#Specifying Myanmar deaths from 1950 to 1960
fifties_Myanmar_deaths <- filter(cleaned_cholera_reported_deaths, Countries..territories.and.areas == "Myanmar" & Year >= 1950 & Year <= 1960)
###Visualization
#Colors
colors_vector <- c("#17becf", # teal
                   "darkgreen",
                   "#c49c94", # light brown
                   "#d62728", # soft red
                   "#9467bd",# lavender purple
                   "#1f77b4", # muted blue
                   "darkcyan", 
                   "#e377c2", # pastel pink
                   "#7f7f7f", # grey
                   "#bcbd22", # olive green
                   "#aec7e8", # light blue
                   "#ff9896", # light red
                   "#ffbb78", # light orange
                   "#98df8a", # light green
                   "#c5b0d5", # light purple
                   "#ff7f0e", # soft orange
                   "#f7b6d2", # light pink
                   "#c7c7c7", # light grey
                   "#dbdb8d", # light olive
                   "#9edae5", # light teal
                   "#6b6ecf", # soft indigo
                   "#e7969c", # soft coral
                   "#17becf" )

##General Visualization
#Bar chart of Average reported deaths per country from 1949 to 2016
ggplot(mean_deaths_per_country, aes( x = `Countries..territories.and.areas`, y = `mean_deaths`)) +
  geom_col( fill = "lightblue") +
  coord_flip() +
  labs(title = "Reported Deaths per Country (1949 - 2016)",
       x = "Country",
       y = "Averge Reported Death Numbers") +
  theme_classic()

#Bar Plot of Reported deaths over the years and each country contribution
ggplot(High_deaths_countries, aes(x = `Year`, y = `Number.of.reported.deaths.from.cholera`, fill = `Countries..territories.and.areas`)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(1949, 2016, by = 5)) +
  scale_y_continuous(breaks = seq(0, 150000, by = 20000)) +
  scale_fill_manual(values = colors_vector)+
  labs (title = "Reported deaths over the years",
        x = "Year",
        y = "Number of reported deaths")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.6)) 

---------------------------------------------------------------------------------
# Calculate average fatality rate by country
average_fatalities <- cholera_data %>%
  group_by(`Countries, territories and areas`) %>%
  summarise(Average_Fatality_Rate = mean(`Cholera case fatality rate`, na.rm = TRUE)) %>%
  arrange(desc(Average_Fatality_Rate))%>%head(n=10)
average_fatalities

# Create a bar chart for average fatality rates
ggplot(average_fatalities, aes(x = reorder(`Countries, territories and areas`, -Average_Fatality_Rate), y = Average_Fatality_Rate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Cholera Case Fatality Rates by Country",
       x = "Countries",
       y = "Average Fatality Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
--------------------------------------------------------------------------------

### India Deaths ###
#Point and smooth plots of the Number of Reported Deaths in India from 1949 to 2016
ggplot(India_deaths, aes(x = `Year`, y = `Number.of.reported.deaths.from.cholera`)) +
  geom_point( color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "lightblue") +
  scale_x_continuous(breaks = seq(1949, 2016, by = 5)) +
  scale_y_continuous(breaks = seq(0, 130000, by = 10000) ) +
  labs (title = "Number of Reported Deaths in India",
        x = "Year",
        y = "Number of Reported Deaths") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.4)) 
#Stacked area chart
ggplot(fifties_India_deaths, aes(x = `Year`, y = `Number.of.reported.deaths.from.cholera`, fill = Countries..territories.and.areas)) +
  geom_area(alpha = 0.9, color = "black")+
  labs (title = "Reported Deaths in India (1950-1960)",
        x = "Year",
        y = "Number of Reported Deaths") +
  scale_x_continuous(breaks = seq(1950, 1960, by = 2)) +
  scale_y_continuous(breaks = seq(0, 130000, by = 10000) ) +
  scale_fill_manual(values = c("steelblue")) + 
  theme(plot.title = element_text(hjust = 0.4)) +
  theme_minimal() 

### Bangladesh Deaths (1950-1960) ###
#Stacked area chart
ggplot(fifties_Bangladesh_deaths, aes(x = `Year`, y = `Number.of.reported.deaths.from.cholera`, fill = Countries..territories.and.areas)) +
  geom_area(alpha = 0.9, color = "black")+
  labs (title = "Reported Deaths in Bangladesh (1950-1960)",
        x = "Year",
        y = "Number of Reported Deaths") +
  scale_x_continuous(breaks = seq(1950, 1960, by = 2)) +
  scale_y_continuous(breaks = seq(0, 17000, by = 2000) ) +
  scale_fill_manual(values = c("darkgreen")) +
  theme_minimal()

### Myanmar Deaths (1950-1960) ###

ggplot(fifties_Myanmar_deaths, aes(x = `Year`, y = `Number.of.reported.deaths.from.cholera`, fill = Countries..territories.and.areas)) +
  geom_area(alpha = 0.9, color = "black")+
  labs (title = "Reported Deaths in Myanmar (1950-1960)",
        x = "Year",
        y = "Number of Reported Deaths") +
  scale_x_continuous(breaks = seq(1950, 1960, by = 2)) +
  scale_y_continuous(breaks = seq(0, 5000, by = 1000) ) +
  scale_fill_manual(values = c("#ff9896"))
theme_minimal()


### India fatality (1950-1960) ###

# Filter for India and limit the years to between 1949 and 1960
india_data <- cholera_data %>%
  filter(`Countries, territories and areas` == "India", Year >= 1949 & Year <= 1960)

# Create a bar chart for cholera case fatality rates over the years
ggplot(india_data, aes(x = Year, y = `Cholera case fatality rate`, fill = "India")) +
  geom_bar(stat = "identity") + 
  labs(title = "Cholera Case Fatality Rate in India (1949-1960)",
       x = "Year",
       y = "Cholera Case Fatality Rate (%)") +
  scale_fill_manual(name = "Country", values = c("India" = "steelblue"), labels = c("India")) + 
  scale_x_continuous(breaks = seq(1949, 1960, by = 1)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) 



### myanmar fatality (1950-1960) ###

myanmar_data <- cholera_data %>%
  filter(`Countries, territories and areas` == "Myanmar", Year >= 1949 & Year <= 1960)

# Create a bar chart for cholera case fatality rates over the years
ggplot(myanmar_data, aes(x = Year, y = `Cholera case fatality rate`, fill = "Myanmar")) +
  geom_bar(stat = "identity") + # Bar chart
  labs(title = "Cholera Case Fatality Rate in Myanmar (1949-1960)",
       x = "Year",
       y = "Cholera Case Fatality Rate (%)") +
  scale_fill_manual(name = "Country", values = c("#ff9896"), labels = c("Myanmar")) + 
  scale_x_continuous(breaks = seq(1949, 1960, by = 1)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) 


## Bangladesh fatality (1949 - 1960) ##

# Filter for Bangladesh and limit the years to between 1949 and 1960
bangladesh_data <- cholera_data %>%
  filter(`Countries, territories and areas` == "Bangladesh", Year >= 1949 & Year <= 1960)

# Create a bar chart for cholera case fatality rates over the years
ggplot(bangladesh_data, aes(x = Year, y = `Cholera case fatality rate`, fill = "Bangladesh")) +
  geom_bar(stat = "identity") + 
  labs(title = "Cholera Case Fatality Rate in Bangladesh (1949-1960)",
       x = "Year",
       y = "Cholera Case Fatality Rate (%)") +
  scale_fill_manual(name = "Country", values = c("Bangladesh" =c("darkgreen")), labels = c("Bangladesh")) + 
  scale_x_continuous(breaks = seq(1949, 1960, by = 1)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

