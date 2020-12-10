library(DT)
library(googleVis)
library(leaflet)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(maps)
library(rgdal)
library(sp)
library(plotly)
library(ggpubr)
library(corrplot)
library(tidyr)

state_stat <- data.frame(state.name = rownames(state.x77), state.x77)

covid_data <- readr::read_csv("covid_data.csv")
death <- readr::read_csv("death_ethnicity.csv") 
choices_death <- unique((death)[1])
class(covid_data$date)
covid_data$date <- as.Date(covid_data$date, format = "%m/%d/%y")
choice<-colnames(covid_data)[11]
choices_state <- unique((covid_data)[1])
choices_state
unique(covid_data$state["hospitalizedPerMil" == 22.84230
                 ])

ethnicity <- c("Hispanic/Latino",
               "Asian", "Black", "White", "Other")
death_count <- c(23229, 6750,	28945, 89940, 7818)
death_percentage <- c(14.8,	4.3,	18.5,	57.4,	5.0)

death_data <- data.frame(ethnicity, death_count, death_percentage)

ethnicity1 <- c("White", "Black", "Hispanic/Latino", "Asian", "Other", "Overall")

death_permil <- c(770, 1310, 910, 580, 490, 850)

adjusted_death <- data.frame(ethnicity1, death_permil)
adjusted_death$ethnicity1 <- factor(adjusted_death$ethnicity1, 
                                    levels = c("White", "Black", "Hispanic/Latino", "Asian", "Other", "Overall"))
   

# d <- na.omit(covid_data[, c(2,3,4,5,6,7,11,13)])
# res <- cor(d)
# round(res,2)
# library(corrplot)
# corrplot(res, type = "upper", order = "hclust",
#          tl.col = "black", tl.srt = 45)

correlation <- covid_data %>%
  group_by(., state) %>%
  filter(., date == "2020-11-25") %>%
  select(., COPD, Hypertension, Obesity, Flu, Cancer, Diabetes, hospitalizedPerMil, deathPerMil)

correlation<- correlation[, c(2,3,4,5,6,7,8,9)]
re <- cor(correlation)
round(re,2)
corrplot(re, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)


cor.test(correlation$Obesity, correlation$hospitalizedPerMil,  method = "pearson", use = "complete.obs")
cor.test(correlation$Cancer, correlation$hospitalizedPerMil,  method = "pearson", use = "complete.obs")
cor.test(correlation$COPD, correlation$hospitalizedPerMil,  method = "pearson", use = "complete.obs")
cor.test(correlation$Hypertension, correlation$hospitalizedPerMil,  method = "pearson", use = "complete.obs")
cor.test(correlation$Diabetes, correlation$hospitalizedPerMil,  method = "pearson", use = "complete.obs")
cor.test(correlation$Flu, correlation$hospitalizedPerMil,  method = "pearson", use = "complete.obs")

cor.test(correlation$Diabetes, correlation$deathPerMil,  method = "pearson", use = "complete.obs")
cor.test(correlation$Hypertension, correlation$deathPerMil,  method = "pearson", use = "complete.obs")
cor.test(correlation$COPD, correlation$deathPerMil,  method = "pearson", use = "complete.obs")
cor.test(correlation$Obesity, correlation$deathPerMil,  method = "pearson", use = "complete.obs")
cor.test(correlation$Cancer, correlation$deathPerMil,  method = "pearson", use = "complete.obs")
cor.test(correlation$Flu, correlation$deathPerMil,  method = "pearson", use = "complete.obs")

