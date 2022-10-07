rm(list=ls())
# Libraries ####
library(tidyverse)
library(scales)
library(gridExtra)
library(kableExtra)

# Load Data ####
state_data <- readRDS("./Data/epi_df.rds")

head(state_data)
str(state_data)
summary(state_data)

## Date Range ####

unique(state_data$`End Date`)

us_nat_avr <- state_data %>%
    filter(!(State %in% c("Puerto Rico",
                          "New York City"))) %>%
    group_by(Age) %>%
    summarise(Population = sum(Population, na.rm = T), .groups = "drop") %>%
    mutate(cx = Population/sum(Population)) %>%
    pull(cx)


## Age Range ####

unique(state_data$Age)

## Race/Ethnicity Info ####

unique(state_data$Race)

# Select States ####

states <- c("FL" = "Florida", "WA" = "Washington")
races <- c("Hispanic", "Non-Hispanic White")

hisp_cols <- c("navy", "forestgreen")
white_cols <- alpha(hisp_cols, 0.35)
cols <- c(hisp_cols, white_cols)[c(1,3,2,4)]

lt_data <- state_data %>%
    rename("lx" = "Population",
           "dx" = "COVID") %>%
    mutate("ax" = case_when(Age == "Under 5 years" ~ 1.5,
                            Age == "85 years and over" ~ 0,
                            TRUE ~ 5),
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
            filter(State == states[1] &
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
            filter(State == states[1]  &
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
            filter(State == states[1] &
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
            filter(State == states[1] &
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


# Chapter Plots/Tables ####
## Population & Age ####
pop <- lt_data %>%
    filter(State %in% states) %>%
    filter(Race %in% races) %>%
    mutate(StateRace = interaction(State, Race),
           Age = as.character(Age)) %>%
    mutate(Age = case_when(Age == "Under 5 years" ~ "0-5",
                           Age == "85 years and over" ~ "85+",
                           TRUE ~ Age),
           Age = gsub(" years", "", Age)) %>%
    mutate(Age = factor(
        Age,
        levels = c(
            '0-5', '5-14', '15-24', '25-34', '35-44', '45-54',
            '55-64', '65-74', '75-84', '85+')
    )) %>%
    ggplot(aes(x = Age, y = lx, fill = StateRace)) +
    geom_bar(stat = "identity", position = "dodge",
             show.legend = FALSE) +
    scale_fill_manual(values = cols,
                      labels = c(
                          paste0(names(states)[1], ", Hispanic"),
                          paste0(names(states)[1], ", Non-Hispanic White"),
                          paste0(names(states)[2], ", Hispanic"),
                          paste0(names(states)[2], ", Non-Hispanic White"))) +
    theme(legend.position = "none") +
    xlab("Age Group") +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    scale_y_continuous(name = "Population",
                       breaks = c(0, 500000, 1000000,
                                  1500000, 2000000,
                                  2500000, 3000000),
                       labels = c("0", "0.5M", "1M",
                                  "1.5M", "2M", "2.5M",
                                  "3M")) +
    theme_classic()

dist <- lt_data %>%
    filter(State %in% states) %>%
    filter(Race %in% races) %>%
    mutate(StateRace = interaction(State, Race),
           Age = as.character(Age)) %>%
    mutate(Age = case_when(Age == "Under 5 years" ~ "0-5",
                           Age == "85 years and over" ~ "85+",
                           TRUE ~ Age),
           Age = gsub(" years", "", Age)) %>%
    mutate(Age = factor(
        Age,
        levels = c(
            '0-5', '5-14', '15-24', '25-34', '35-44', '45-54',
            '55-64', '65-74', '75-84', '85+')
    )) %>%
    ggplot(aes(x = Age, y = cx, fill = StateRace)) +
    geom_bar(stat = "identity", position = "dodge",
             show.legend = FALSE) +
    scale_fill_manual(values = cols,
                      labels = c(
                          paste0(names(states)[1], ", Hispanic"),
                          paste0(names(states)[1], ", Non-Hispanic White"),
                          paste0(names(states)[2], ", Hispanic"),
                          paste0(names(states)[2], ", Non-Hispanic White"))) +
    theme(legend.position = "none") +
    xlab("Age Group") +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    scale_y_continuous(name = "Age Distribution",
                       breaks = c(0, 0.05, 0.1,
                                  0.15, 0.2)) +
    theme_classic()

if(!dir.exists("Figs/")){
    dir.create("Figs/")
}

pdf("Figs/Pop.pdf",
    width = 6, height = 3)
grid.arrange(pop, dist, nrow = 1)
dev.off()



## Deaths & nMx ####
deaths <- lt_data %>%
    filter(State %in% states) %>%
    filter(Race %in% races) %>%
    mutate(StateRace = interaction(State, Race),
           Age = as.character(Age)) %>%
    mutate(Age = case_when(Age == "Under 5 years" ~ "0-5",
                           Age == "85 years and over" ~ "85+",
                           TRUE ~ Age),
           Age = gsub(" years", "", Age)) %>%
    mutate(Age = factor(
        Age,
        levels = c(
            '0-5', '5-14', '15-24', '25-34', '35-44', '45-54',
            '55-64', '65-74', '75-84', '85+')
    )) %>%
    ggplot(aes(x = Age, y = dx, fill = StateRace)) +
    geom_bar(stat = "identity", position = "dodge",
             show.legend = FALSE) +
    scale_fill_manual(values = cols,
                      labels = c(
                          paste0(names(states)[1], ", Hispanic"),
                          paste0(names(states)[1], ", Non-Hispanic White"),
                          paste0(names(states)[2], ", Hispanic"),
                          paste0(names(states)[2], ", Non-Hispanic White"))) +
    theme(legend.position = "none") +
    xlab("Age Group") +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    scale_y_continuous(name = "COVID-19 Deaths") +
    theme_classic()

nmx <- lt_data %>%
    filter(State %in% states) %>%
    filter(Race %in% races) %>%
    mutate(StateRace = interaction(State, Race),
           Age = as.character(Age),
           mx = 1000*mx) %>%
    mutate(Age = case_when(Age == "Under 5 years" ~ "0-5",
                           Age == "85 years and over" ~ "85+",
                           TRUE ~ Age),
           Age = gsub(" years", "", Age)) %>%
    mutate(Age = factor(
        Age,
        levels = c(
            '0-5', '5-14', '15-24', '25-34', '35-44', '45-54',
            '55-64', '65-74', '75-84', '85+')
    )) %>%
    ggplot(aes(x = Age, y = mx, fill = StateRace)) +
    geom_bar(stat = "identity", position = "dodge",
             show.legend = FALSE) +
    scale_fill_manual(values = cols,
                      labels = c(
                          paste0(names(states)[1], ", Hispanic"),
                          paste0(names(states)[1], ", Non-Hispanic White"),
                          paste0(names(states)[2], ", Hispanic"),
                          paste0(names(states)[2], ", Non-Hispanic White"))) +
    theme(legend.position = "none") +
    xlab("Age Group") +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    scale_y_continuous(name = "Mortality Rate (per 1000 people)",
                       breaks = c(0, 2.5, 5,
                                  7.5, 10)) +
    theme_classic()

pdf("Figs/Deaths.pdf",
    width = 6, height = 3)
grid.arrange(deaths, nmx, nrow = 1)
dev.off()

## Crude Death Rates ####

lt_data %>%
    filter(State %in% states) %>%
    filter(Race %in% races) %>%
    group_by(State, Race) %>%
    summarize(CDR = 100000*sum(cx*mx)) %>%
    mutate(CDR = round(CDR, 1)) %>%
    ungroup() %>%
    select(-State) %>%
    kbl(.,
        align = c("l", "l", "r"),
        valign = "!ht",
        position = "center",
        booktabs = TRUE,
        format = "latex",
        caption = paste0("The crude death rate for the Hispanic",
                         " and Non-Hispanic white populations ",
                         "in ", states[1], " and ", states[2], "."),
        col.names = c("Population", "CDR")) %>%
    pack_rows(states[1], 1, 2) %>%
    pack_rows(states[2], 3, 4)

## ASMRs ####

us_cx <- lt_data %>%
    filter(!(State %in% c("Puerto Rico",
                          "New York City"))) %>%
    ungroup() %>%
    group_by(Age, Race) %>%
    summarise(lx = sum(lx)) %>%
    ungroup() %>%
    group_by(Race) %>%
    mutate("cx" = lx/sum(lx))

us_cx %>%
    filt
    group_by(Age) %>%
    summarize(cx = mean(cx))

us_hisp_cx <- us_cx %>%
    filter(Race == "Hispanic") %>%
    ungroup() %>%
    select(cx) %>%
    unlist()
us_white_cx <- us_cx %>%
    filter(Race == "Non-Hispanic White") %>%
    ungroup() %>%
    select(cx) %>%
    unlist()

ny_hisp_cx <- lt_data %>%
    filter(State == states[2],
           Race == "Hispanic") %>%
    ungroup() %>%
    select(cx) %>%
    unlist()

ny_white_cx <- lt_data %>%
    filter(State == states[2],
           Race == "Non-Hispanic White") %>%
    ungroup() %>%
    select(cx) %>%
    unlist()

ca_white_cx <- lt_data %>%
    filter(State == states[1],
           Race == "Non-Hispanic White") %>%
    ungroup() %>%
    select(cx) %>%
    unlist()

ca_hisp_cx <- lt_data %>%
    filter(State == states[1],
           Race == "Hispanic") %>%
    ungroup() %>%
    select(cx) %>%
    unlist()


### Hispanic within ####
lt_data %>%
    filter(State %in% states) %>%
    filter(Race == "Hispanic") %>%
    group_by(State, Race) %>%
    summarize(NY = 100000*sum(ny_hisp_cx*mx),
              CA = 100000*sum(ca_hisp_cx*mx),
              US = 100000*sum(us_hisp_cx*mx),) %>%
    rename(!!names(states)[2] := "NY", !!names(states)[1] := "CA") %>%
    mutate(across(where(is.numeric), ~round(., 1))) %>%
    ungroup() %>%
    select(-State) %>%
    kbl(.,
        align = c("l", "l", "r"),
        valign = "!ht",
        position = "center",
        booktabs = TRUE,
        format = "latex",
        caption = paste0(
            "The age-standardized mortality rates for the Hispanic",
            " populations in ", states[1], " and ", states[2],
            " standardized using the Hispanic age distributions of ",
            states[2], " (Column 2), ", " ", states[1],
            " (Column 3), and the US (Column 4)."),
        col.names = c("Population", names(states)[2], names(states)[1], "US")) %>%
    pack_rows(states[1], 1, 1) %>%
    pack_rows(states[2], 2, 2)


### White across ####
lt_data %>%
    filter(State %in% states) %>%
    filter(Race %in% races) %>%
    group_by(State, Race) %>%
    summarize(NY = 100000*sum(ny_white_cx*mx),
              CA = 100000*sum(ca_white_cx*mx),
              US = 100000*sum(us_white_cx*mx),) %>%
    rename(!!names(states)[2] := "NY", !!names(states)[1] := "CA") %>%
    mutate(across(where(is.numeric), ~round(., 1))) %>%
    ungroup() %>%
    select(-State) %>%
    kbl(.,
        align = c("l", "l", "r"),
        valign = "!ht",
        position = "center",
        booktabs = TRUE,
        format = "latex",
        caption = paste0("The age-standardized mortality rates for the Hispanic",
                         " and Non-Hispanic white populations ",
                         "in ", states[1], " and ", states[2], " standardized using ",
                         "the Non-Hispanic white age distributions of ",
                         states[2], " (Column 2), ", " ", states[1],
                         " (Column 3), and the US (Column 4)."),
        col.names = c("Population", names(states)[2], names(states)[1], "US")) %>%
    pack_rows(states[1], 1, 2) %>%
    pack_rows(states[2], 3, 4)

### state specific ####
lt_data %>%
    filter(State %in% states) %>%
    mutate(State = factor(State, levels = unname(states))) %>%
    filter(Race %in% races) %>%
    arrange(State, Race, Age) %>%
    ungroup() %>%
    mutate(white_cx = c(ca_white_cx, ca_white_cx, ny_white_cx, ny_white_cx)) %>%
    mutate(hisp_cx = c(ca_hisp_cx, ca_hisp_cx, ny_hisp_cx, ny_hisp_cx)) %>%
    mutate(us_cx = c(us_nat_avr, us_nat_avr, us_nat_avr,us_nat_avr)) %>%
    group_by(State, Race) %>%
    summarize(Hispanic = 100000*sum(hisp_cx*mx),
              White = 100000*sum(white_cx*mx),
              US = 100000*sum(us_cx*mx),) %>%
    mutate(across(where(is.numeric), ~round(., 1))) %>%
    ungroup() %>%
    select(-State) %>%
    kbl(.,
        align = c("l", "l", "r"),
        valign = "!ht",
        position = "center",
        booktabs = TRUE,
        format = "latex",
        caption = paste0("The age-standardized mortality rates for the Hispanic",
                         " and Non-Hispanic White populations ",
                         "in ", states[1], " and ", states[2], " standardized using ",
                         "the Hispanic age distribution of the state (Column 2),",
                         "the Non-Hispanic White age distribution of the state (Column 3), ",
                         "and the US overall Age Distribution (Column 4)."),
        col.names = c("Population", "Hispanic", "White", "US")) %>%
    pack_rows(states[1], 1, 2) %>%
    pack_rows(states[2], 3, 4)

lt_data %>%
    filter(State %in% states) %>%
    filter(Race %in% races)
