# SETUP

  # setwd("/Volumes/SANDISK 1TB/code/wildlife strikes")
  
  # install.packages("Rmisc")
  # install.packages("effectsize")
  # install.packages("tidyverse")
  
  library(Rmisc)
  library(effectsize)
  library(tidyverse)

    # creating NWSD ("National Wildlife Strike Database") data frame
  
  NWSD <- read_csv("NWSD202404.csv") %>%
    select(INCIDENT_DATE, INCIDENT_MONTH, INCIDENT_YEAR, TIME_OF_DAY, STATE, HEIGHT, AIRPORT_ID, LATITUDE, LONGITUDE) %>%
    filter(AIRPORT_ID != "ZZZZ", INCIDENT_YEAR > 2013, INCIDENT_YEAR < 2024,
           STATE %in% c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL",
           "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
           "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD",
           "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")) %>%
    arrange(INCIDENT_YEAR, INCIDENT_MONTH)
  
  NWSD$INCIDENT_DATE <- as.factor(NWSD$INCIDENT_DATE)
  NWSD$INCIDENT_MONTH <- as.factor(NWSD$INCIDENT_MONTH)
  NWSD$INCIDENT_YEAR <- as.factor(NWSD$INCIDENT_YEAR)
  NWSD$STATE <- as.factor(NWSD$STATE)

# OVER TIME
 
  # DATA: number of incidents on each day
  
  NWSD_D_all <- NWSD %>%
    group_by(INCIDENT_YEAR, INCIDENT_DATE) %>%
    summarize(count = n())
  
  # PLOT: scatter plot of each day's incident counts
  
  yearcolors <- c(
    "2014" = "firebrick4", # 8B1A1A
    "2015" = "#9F1E1E",
    "2016" = "firebrick", # B22222
    "2017" = "#C02424",
    "2018" = "firebrick3", # CD2625
    "2019" = "#DE2929",
    "2020" = "firebrick2", # EE2C2C
    "2021" = "#F72E2E",
    "2022" = "firebrick1", # FF3030
    "2023" = "#FF3232"
  )
  
  ggplot(NWSD_D_all, aes(x = INCIDENT_DATE, y = count, color = INCIDENT_YEAR)) +
    geom_point(alpha = 0.9) +
    theme_bw() +
    scale_color_manual(values = yearcolors) +
    scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01", "05-01", "06-01", "07-01", "08-01", "09-01", "10-01", "11-01", "12-01"),
                     labels = c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec.")) +
    labs(title = "Incident frequency by date",
         subtitle = "Incident reports on each day from 2014 through 2023.",
         x = "Date",
         y = "Incident reports",
         color = "Year"
    ) +
    theme(plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(face = "italic"),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
    )

# BY MONTH

  # DATA: number of incidents in each month
  
  NWSD_M_all <- NWSD %>%
    group_by(INCIDENT_YEAR, INCIDENT_MONTH) %>%
    summarize(count = n())
  
  # PLOT: box plots of each month's incident counts
  
  ggplot(NWSD_M_all, aes(x = INCIDENT_MONTH, y = count)) +
    geom_boxplot(fill = "firebrick3") +
    theme_bw() +
    scale_x_discrete(breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                     labels = c("Jan.", "Feb.", "March", "April", "May", "June", "July", "Aug.", "Sept.", "Oct.", "Nov.", "Dec.")) +
    labs(title = "Incident frequency by month",
         subtitle = "Mean monthly incident reports from 2014 through 2023.",
         x = "Month",
         y = "Incident reports",
         color = "Year"
    ) +
    theme(plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(face = "italic"),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
    )
  
  # ANALYSIS: significance of variance between months
  
  NWSD_M_aov <- aov(count ~ INCIDENT_MONTH, data = NWSD_M_all)
  
  summary(NWSD_M_aov)
  
# BY ALTITUDE
  
  # DATA: number of incidents at each reported altitude
  
  NWSD_H <- NWSD %>%
    filter(HEIGHT != "is.NA") %>%
    group_by(HEIGHT) %>%
    summarize(count = n())

  # PLOT: distribution of reported altitudes' counts
  
  ggplot(NWSD_H, aes(x = HEIGHT)) +
    geom_histogram(binwidth = 500, fill = "firebrick3") +
    theme_bw() +
    scale_x_continuous(breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000)) +
    scale_y_continuous(breaks = c(1, 25, 50, 75)) +
    theme(plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(face = "italic"),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
    labs(title = "Collision frequency by altitude",
         x = "Altitude (ft.)",
         y = "Incident reports") +
    coord_flip()
  
  # ANALYSIS: creating and testing a linear model by altitude
  
  ggplot(NWSD_H, aes(x = HEIGHT, y = count)) +
    geom_point(color = "firebrick3") +
    geom_smooth(method = "lm", color = "salmon", fill = "gray85") +
    theme_bw() +
    scale_x_continuous(breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000)) +
    scale_y_continuous(breaks = c(1, 5000, 10000, 15000, 20000, 25000)) +
    theme(plot.title = element_text(face = "bold", size = 15),
          plot.subtitle = element_text(face = "italic"),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
    labs(title = "Collision frequency by altitude",
         x = "Altitude (ft.)",
         y = "Incident reports") +
    coord_flip()
  
  NWSD_H_lm <- lm(count ~ HEIGHT, data = NWSD_H)
  summary(NWSD_H_lm)

  # BY AIRPORT
  
  # DATA: number of incidents at each set of lat-long coordinates
  
  NWSD_LL <- NWSD %>%
    group_by(LATITUDE, LONGITUDE) %>%
    summarize(count = n()) %>%
    arrange(count)

  # PLOT: incident frequency at each airport
  
  ggplot(NWSD_LL, aes(x = LONGITUDE, y = LATITUDE, size = count, color = count)) +
    geom_point() +
    scale_color_gradient(low = "gray85", high = "firebrick2") +
    theme_bw() +
    xlim(-175, -67) +
    ylim(20, 70) +
    theme(plot.title = element_text(face = "bold", size = 15),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank()
          ) +
    labs(title = "Collision frequency by airport",
         x = "Longitude",
         y = "Latitude",
         ) +
    guides(
      size = guide_legend(title = "Incident reports"),
      color = guide_legend(title = "Incident reports")
    ) +
    coord_fixed(ratio = 1.5)
  
  # DATA: comparing distribution of airports and incidents by latitude
  
  latlong <- left_join(NWSD, NWSD_LL, by = "LATITUDE") %>%
    filter(is.finite(LATITUDE)) %>%
    mutate(lat_bin = cut(LATITUDE, breaks = 25), include.lowest = TRUE) %>%
    group_by(lat_bin) %>%
    summarize(incidents = n(),
              airports = n_distinct(count)*mean(NWSD_LL$count))
  
  latlong_summary_long <- latlong %>%
    pivot_longer(cols = c(airports, incidents), names_to = "count_type", values_to = "count") %>%
    mutate(count_type = factor(count_type, levels = c("incidents", "airports"))) %>%
    filter(complete.cases(.))
  
  # PLOT: comparing distribution of airports and incidents by latitude
  
  ggplot(latlong_summary_long, aes(x = lat_bin, y = count, fill = count_type)) +
    geom_bar(stat = "identity", position = "stack") +
    theme_bw() +
    scale_fill_manual(values = c("firebrick3", "gray85"),
                      breaks = c("incidents", "airports"),
                      labels = c("Number of incidents", "Number of airports × mean incident count")) +
    scale_y_continuous(breaks = c(1, 5000, 10000, 15000, 20000, 25000, 30000, 35000),
                       labels = c(1, 5000, 10000, 15000, 20000, 25000, 30000, 35000)) +
    labs(title = "Distribution by latitude",
         x = "Latitude bin",
         y = "Count",
         fill = element_blank()) +
    theme(plot.title = element_text(face = "bold", size = 15),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank()) +
    coord_flip()
  
  # ANALYSIS: comparing airport and incident distributions by latitude
  
  wilcox.test(count ~ count_type, data = latlong_summary_long)
  
  cohens_d(count ~ count_type, data = latlong_summary_long)
  
# BY TIME OF DAY
  
  # DATA: number of incidents each month by time of day
  
  NWSD_T <- NWSD %>%
    filter(complete.cases(.)) %>%
    group_by(TIME_OF_DAY, INCIDENT_MONTH) %>%
    summarize(HEIGHT, count = n())
  
  # PLOT: number of incidents each month during each reported phase
  
  ggplot(NWSD_T, aes(x = count)) +
    geom_boxplot(fill = "firebrick3") +
    theme_bw() +
    facet_wrap(TIME_OF_DAY~., ncol = 4) +
    labs(title = "Incidents per month by time of day",
         x = "Incident reports per month",
         y = "",
         fill = element_blank()) +
    scale_x_continuous(breaks = c(1, 1000, 2000, 3000, 4000, 5000, 6000),
                       labels = c(1, 1000, 2000, 3000, 4000, 5000, 6000)) +
    theme(plot.title = element_text(face = "bold", size = 15),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank()) +
    coord_flip()
  
  # PLOT: correlation between time of day and altitude
  
  ggplot(NWSD_T, aes(x = TIME_OF_DAY, y = HEIGHT)) +
    geom_boxplot(fill = "firebrick") +
    theme_bw() +
    labs(title = "Variation in incident altitude by time of day",
         x = "Time of day",
         y = "Altitude (ft.)",
         fill = element_blank()) +
    scale_y_continuous(breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000),
                       labels = c(0, 5000, 10000, 15000, 20000, 25000, 30000)) +
    theme(plot.title = element_text(face = "bold", size = 15),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank())
  
  # DATA: number of incidents each month by time of day (day and night only)
  
  NWSD_T_DN <- NWSD_T %>%
    filter(TIME_OF_DAY %in% c("Day", "Night"))
  
  # ANALYSIS: altitude during day vs. night
  
  wilcox.test(HEIGHT ~ TIME_OF_DAY, NWSD_T_DN)