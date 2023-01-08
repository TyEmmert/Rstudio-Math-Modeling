library(ggplot2)
library(dplyr)

getwd()
setwd("C:\\Users\\Noky\\Documents\\rstudio - Inflation") #Set new working directory

Energy <- read.csv("Energy.csv") #Data cleaned in Excel, and imported
Food <- read.csv("Food.csv")
Gasoline <- read.csv("Gasoline.csv")

names(Energy)[names(Energy) == "X"] <- "Month" #rename the months column to months
names(Gasoline)[names(Gasoline) == "Year"] <- "Month"
names(Food)[names(Food) == "X"] <- "Month"

Energy$Month = factor(Energy$Month, levels = month.abb) #Make it so that it reads the months in order on the graph
Food$Month = factor(Food$Month, levels = month.abb)
Gasoline$Month = factor(Gasoline$Month, levels = month.abb)

Gas_Jun <- Gasoline %>% #Create a variable with Gas CPI for gas prices in June
  filter(Month == "Jun") %>%
  select(X2022)

Gas_Jan <- Gasoline %>% #Create a variable with Gas CPI for gas prices in January
  filter(Month == "Jan") %>%
  select(X2022)

Gas_Nov <- Gasoline %>% #Create a variable with Energy CPI for energy prices in June
  filter(Month == "Nov") %>%
  select(X2022)

Gas_Inflation_Jun <- signif(((Gas_Jun - Gas_Jan)/Gas_Jan)*100, 3) #Calculate inflation for gas. CPI June - CPI Jan all divided by CPI Jan and multiply by 100 to make a percent
Gas_Inflation_Jun

Gas_Inflation_Nov <- signif(((Gas_Nov - Gas_Jan)/Gas_Jan)*100, 3) #Calculate inflation for gas. CPI June - CPI Jan all divided by CPI Jan and multiply by 100 to make a percent
Gas_Inflation_Nov


Food_Jun <- Food %>% #Create a variable with Food CPI for food prices in June
  filter(Month == "Jun") %>%
  select(X2022)

Food_Jan <- Food %>% #Create a variable with Food CPI for food prices in January
  filter(Month == "Jan") %>%
  select(X2022)

Food_Nov <- Food %>% #Create a variable with Energy CPI for energy prices in June
  filter(Month == "Nov") %>%
  select(X2022)

Food_Inflation_Jun <- signif(((Food_Jun - Food_Jan)/Food_Jan)*100, 3) #Calculate inflation for food. CPI June - CPI Jan all divided by CPI Jan and multiply by 100 to make a percent
Food_Inflation_Jun

Food_Inflation_Nov <- signif(((Food_Nov - Food_Jan)/Food_Jan)*100, 3) #Calculate inflation for food. CPI June - CPI Jan all divided by CPI Jan and multiply by 100 to make a percent
Food_Inflation_Nov

Energy_Jun <- Energy %>% #Create a variable with Energy CPI for energy prices in June
  filter(Month == "Jun") %>%
  select(X2022)
Energy_Jun

Energy_Jan <- Energy %>% #Create a variable with Energy CPI for energy prices in Jan
  filter(Month == "Jan") %>%
  select(X2022)

Energy_Nov <- Energy %>% #Create a variable with Energy CPI for energy prices in June
  filter(Month == "Nov") %>%
  select(X2022)

Energy_Inflation_Jun <- signif(((Energy_Jun - Energy_Jan)/Energy_Jan)*100, 3) #Calculate inflation for energy. CPI June - CPI Jan all divided by CPI Jan and multiply by 100 to make a percent
Energy_Inflation_Jun

Energy_Inflation_Nov <- signif(((Energy_Nov - Energy_Jan)/Energy_Jan)*100, 3) #Calculate inflation for energy. CPI June - CPI Jan all divided by CPI Jan and multiply by 100 to make a percent
Energy_Inflation_Nov

ggplot(data = Food, aes(x = Month, y = X2022)) + #create plot with Food data
  geom_point(aes(color = "Food")) + #Create points with Food data
  geom_smooth(aes(x = as.numeric(Month), color = "Food")) + #create line with Food data
  geom_point(data = Gasoline, aes(x = Month, y = X2022, color = "Gasoline")) + #Create points with Gasoline data
  geom_smooth(data = Gasoline, aes(x = as.numeric(Month), y = X2022, color = "Gasoline"), se = FALSE) + #Create line with Gasoline data
  geom_point(data = Energy, aes(x = Month, y = X2022, color = "Energy")) + #Create points with Energy data
  geom_smooth(data = Energy, aes(x = as.numeric(Month), y = X2022, color = "Energy"), se = FALSE) + #Create line with Energy data
  ylim(0, 450) + #y axis limits
  xlab("Months") + #x label
  ylab("Consumer Price Index") + #y label
  geom_text(aes(color = "Gasoline"),label = paste0(Gas_Inflation_Jun, "% inflation"), show.legend = FALSE, x = 6, y = 450) + #Label highest inflation point in June for Gas
  geom_text(aes(color = "Gasoline"),label = paste0(Gas_Inflation_Nov, "% inflation"), show.legend = FALSE, x = 11.8, y = 330) + #label final inflation point in Nov for Gas
  geom_text(aes(color = "Energy"),label = paste0(Energy_Inflation_Jun, "% inflation"), show.legend = FALSE, x = 6, y = 360) + #label highest inflation point in June for Energy
  geom_text(aes(color = "Energy"),label = paste0(Energy_Inflation_Nov, "% inflation"), show.legend = FALSE, x = 11.8, y = 290) + # label final inflation point in Nov for Energy
  geom_text(aes(color = "Food"), label = paste0(Food_Inflation_Nov, "% inflation"), show.legend = FALSE, x = 11.8, y = 310) # label final inflation point in Nov for Food