rm(list=ls())
setwd("~/Dropbox/covid_book_chapter/")

# Libraries ####
library(tidyverse)
library(scales)

# Load Data ####
state_data <- readRDS("./Data/epi_df.rds")

head(state_data)
str(state_data)
summary(state_data)

## Date Range ####

unique(state_data$`End Date`)


## Age Range ####

unique(state_data$Age)

## Race/Ethnicity Info ####

unique(state_data$Race)

# Select States ####

states <- c("California", "Washington")

lt_data <- state_data %>%
    filter(State %in% states) %>%
    rename("lx" = "Population",
           "dx" = "COVID") %>%
    mutate("ax" = case_when(Age == "Under 5 years" ~ 1.5,
                            Age == "85 years and over" ~ 0,
                            TRUE ~ 2.5),
           lx = ifelse(is.na(lx), 0, lx),
           "lxn" = lx - dx,
           "Lx" = lxn*5 + dx*ax,
           "qx" = dx/lx,
           "mx" = dx/Lx) %>%
    group_by(State, Race) %>%
    mutate("cx" = lx/sum(lx))


par(mfrow = c(2,2))

barplot(lx ~ Age,
        data = lt_data %>%
         filter(State == "California" &
                    Race == "Hispanic"),
     border = FALSE,
     xlab = "",
     ylab = "Population",
     main = "Hispanic",
     col = alpha("navy", .25))

barplot(lx ~ Age,
        data = lt_data %>%
            filter(State == states[2] &
                       Race == "Hispanic"),
        border = FALSE,
        xlab = "",
        ylab = "Population",
        add = TRUE,
        col = alpha("forestgreen", .25))
legend("topright",
       bty = 'n',
       fill = alpha(c("navy",
                      "forestgreen"), 0.25),
       border = FALSE,
       legend = states)

barplot(cx ~ Age,
        data = lt_data %>%
            filter(State == "California" &
                       Race == "Hispanic"),
        border = FALSE,
        xlab = "",
        ylab = "Age Distribution",
        col = alpha("navy", .25))

barplot(cx ~ Age,
        data = lt_data %>%
            filter(State == states[2] &
                       Race == "Hispanic"),
        border = FALSE,
        xlab = "",
        ylab = "Age Distribution",
        add = TRUE,
        col = alpha("forestgreen", .25))


barplot(lx ~ Age,
        data = lt_data %>%
            filter(State == "California" &
                       Race == "Non-Hispanic White"),
        border = FALSE,
        xlab = "",
        ylab = "Population",
        main = "Non-Hispanic, White",
        col = alpha("navy", .25))

barplot(lx ~ Age,
        data = lt_data %>%
            filter(State == states[2] &
                       Race == "Non-Hispanic White"),
        border = FALSE,
        xlab = "",
        ylab = "Population",
        add = TRUE,
        col = alpha("forestgreen", .25))
legend("topright",
       bty = 'n',
       fill = alpha(c("navy",
                      "forestgreen"), 0.25),
       border = FALSE,
       legend = states)

barplot(cx ~ Age,
        data = lt_data %>%
            filter(State == "California" &
                       Race == "Non-Hispanic White"),
        border = FALSE,
        xlab = "",
        ylab = "Age Distribution",
        col = alpha("navy", .25))

barplot(cx ~ Age,
        data = lt_data %>%
            filter(State == states[2] &
                       Race == "Non-Hispanic White"),
        border = FALSE,
        xlab = "",
        ylab = "Age Distribution",
        add = TRUE,
        col = alpha("forestgreen", .25))



