# OCNMS Killer Whale Detection graphs

# RUN this to make sure latest updates for PAMscapes
devtools::install_github('TaikiSan21/PAMscapes')

#install.packages("rJava") make sure Java is installed for xlsx to work

library(PAMscapes)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)
#library(rJava)
library(xlsx)
library(openxlsx)
library(devtools)
library(plotly)
library(tidyr)
library(grid)
library(patchwork)

rm(list=ls()) 

#Load in
OC02_01 <- read.csv('C:/Users/pam_user/Desktop/SoundscapesWebsiteDev/code/plot_Humpback&Orca/data/OCNMS ECHO/OC-02/OC-02-01.csv', stringsAsFactors = FALSE)

OC02_02 <- read.csv('C:/Users/pam_user/Desktop/SoundscapesWebsiteDev/code/plot_Humpback&Orca/data/OCNMS ECHO/OC-02/OC-02-02.csv', stringsAsFactors = FALSE)

OC02_04 <- read.csv('C:/Users/pam_user/Desktop/SoundscapesWebsiteDev/code/plot_Humpback&Orca/data/OCNMS ECHO/OC-02/OC-02-04.csv', stringsAsFactors = FALSE)

OC02_05 <- read.xlsx('C:/Users/pam_user/Desktop/SoundscapesWebsiteDev/code/plot_Humpback&Orca/data/OCNMS ECHO/OC-02/OC-02-05.xlsx')


#Clean up! 
#fixing format and error found in OC02_01 log
OC02_01$end_time[5] <- "2019-03-10T21:50:28Z"

# remove columns killerwhale_presence (always 1) and killerwhale_echo_types (always NaN) 
OC02_01 <- OC02_01[,c(1, 4, 5, 6)]

OC02_01 <- OC02_01[-1,]
OC02_02 <- OC02_02[-1,]
OC02_04 <- OC02_04[-1,]
OC02_05 <- OC02_05[-1,]

OC02_05 <- OC02_05 %>%
  rename(X = X3)

OC02_05 <- OC02_05 %>%
  rename(X.1 = X4)

OC02 <- bind_rows(OC02_01, OC02_02, OC02_04, OC02_05)


#format
OC02 <- OC02 %>%
  rename(Echotype = X)

OC02 <- OC02 %>%
  rename(Subtype = X.1)

OC02 <- OC02 %>%
  rename(Comments = X.2)

OC02$start_time <- ymd_hms(OC02$start_time)
OC02$end_time <- ymd_hms(OC02$end_time)

OC02$start_time <- as.POSIXct(OC02$start_time)
OC02$end_time <- as.POSIXct(OC02$end_time)

#removing rows where find wouldnt open, didnt exists, or no kw calls were found
OC02 <- OC02[-192, ]
OC02 <- OC02[-190, ]
OC02 <- OC02[-11, ]
OC02 <- OC02[-10, ]

#rename logs 
OC02$Echotype <- gsub('T','Transient', OC02$Echotype)
OC02$Echotype <- gsub(' Transient','Transient', OC02$Echotype)
OC02$Echotype <- gsub('NRKW/SRKW','NRKW,SRKW', OC02$Echotype)
OC02$Echotype <- gsub('SRKW,NRKW','NRKW,SRKW', OC02$Echotype)
OC02$Echotype <- gsub("\\?", "", OC02$Echotype)
OC02$Echotype <- gsub('UNK','Unknown killer whales', OC02$Echotype)
OC02$Echotype <- gsub('SRKW','Southern resident killer whales', OC02$Echotype)
OC02$Echotype <- gsub('NRKW','Northern resident killer whales', OC02$Echotype)
OC02$Echotype <- gsub('URK','Unknown resident killer whales', OC02$Echotype)
OC02$Echotype <- gsub('Unknown resident killer whalesW','Unknown resident killer whales', OC02$Echotype)


unique(OC02$Echotype)


#creating a function that will expand each log so that we have a row for each hour where a kw was singing 
generate_hours <- function(start_time, end_time) {
  # Create a sequence of hours between start_time and end_time
  seq_hours <- seq(from = floor_date(start_time, "hour"), 
                   to = ceiling_date(end_time, "hour") - 1, 
                   by = "hour")
  
  # Return a data frame with one row per hour
  data.frame(hour_presence = seq_hours)
}

# Apply the function to each row of the data
OC02Hour <- OC02 %>%
  rowwise() %>%
  do(data.frame(Echotype = .$Echotype,
                generate_hours(.$start_time, .$end_time))) %>%
  ungroup() %>%
  distinct() 


#need to add a end detection time column
OC02Hour <- OC02Hour %>% mutate(end_time = hour_presence + hours(1))

OC02Hour$Orca <- 1



#LINE GRAPH ####

#NEW STUFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF


OC02Hour <- OC02Hour %>%
  separate_rows(Echotype, sep = "\\s*[,/]\\s*") %>%   # split on comma or slash
 # mutate(Echotype = trimws(gsub("\\?", "", Echotype))) %>%   # drop "?" and stray spaces
  filter(Echotype != "") %>%
  distinct()   # avoid duplicate hour + echotype rows

OC02HourT %>% count(Echotype, sort = TRUE)   # check the result


OC02_weekly <- OC02Hour %>%
  mutate(week = as.Date(floor_date(hour_presence, "week"))) %>%
  group_by(week, Echotype) %>%
  summarise(total_hours_present = sum(Orca, na.rm = TRUE), .groups = "drop") %>%
  complete(
    week = seq(min(week), max(week), by = "1 week"),
    Echotype,
    fill = list(total_hours_present = 0)
  ) %>%
  arrange(Echotype, week)




OC02_offeffort <- data.frame(
  start = as.Date(c("2019-04-25", "2019-10-31", "2020-10-02")),
  end   = as.Date(c("2019-07-10", "2020-07-11", "2021-06-05"))
)

orca_complete <- OC02_weekly %>%
  rowwise() %>%
  mutate(
    is_off = any(week >= OC02_offeffort$start & week <= OC02_offeffort$end),
    total_hours_present = if_else(is_off, NA_real_, as.numeric(total_hours_present))
  ) %>%
  ungroup() %>%
  select(-is_off)


orca_complete$Echotype <- factor(orca_complete$Echotype,
                              levels = c("Northern resident killer whales", 
                                         "Southern resident killer whales", 
                                         "Unknown resident killer whales", 
                                         "Transient", 
                                         "Unknown killer whales"))



curveOC02 <- ggplot(orca_complete,
                    aes(x = week, y = total_hours_present,
                        color = Echotype, fill = Echotype)) +
  geom_point(alpha = 0.6, na.rm = TRUE) +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 20),
              se = TRUE, alpha = 0.2, na.rm = TRUE) +
  geom_rect(
    data = OC02_offeffort,
    aes(xmin = start, xmax = end - days(4), ymin = 0, ymax = 72),
    inherit.aes = FALSE, fill = "gray95") +
  scale_x_date(date_labels = "%b %Y",
               date_breaks = "3 months",
               date_minor_breaks = "1 month") +
  coord_cartesian(ylim = c(0, 72)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(24, 48, 72, 96, 120, 144, 168)) +
  labs(title = "OC02", x = "Week",
       y = "Total hours with orca call presence",
       color = "Echotype", fill = "Echotype") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

curveOC02



curveOC02 <- curveOC02 + facet_wrap(~ Echotype, ncol = 2)


#END NEW STUFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

###plot ####

####curve graph MB01 ####
#with GAM curve of best fit
curveOC02 <- ggplot(orca_complete, aes(x = week, y = total_hours_present)) +
  geom_point() +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 20),
              se = TRUE,
              color = "steelblue",
              fill = "steelblue",
              na.rm = FALSE,
              alpha = 0.2) +
  geom_rect(
    data = OC02_offeffort,
    aes(xmin = start , xmax = end - days(4), ymin = 0, ymax = 72),
    inherit.aes = FALSE,
    fill = "white") +
  scale_x_date(
    date_labels = "%b %Y",   # shows abbreviated month + year on x-axis
    date_breaks = "3 months",
    date_minor_breaks = "1 month"  # one tick per month
  ) +
  geom_rect(
    data = OC02_offeffort,
    aes(xmin = start , xmax = end - days(4), ymin = 0, ymax = 2),
    inherit.aes = FALSE,
    fill = "grey") +
  scale_x_date(
    date_labels = "%b %Y",   # shows abbreviated month + year on x-axis
    date_breaks = "3 months",
    date_minor_breaks = "1 month"  # one tick per month
  ) +
  theme_classic() +
  #theme_minimal() +
  coord_cartesian(ylim = c(0, 72))+
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(24, 48, 72, 96, 120, 144, 168))+
  labs(
    title = "OC02",
    x = "Week",
    y = "Total hours with orca call presence"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")


curveOC02


curvewtitle <- (curveOC02) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Passive acoustic monitoring of orca in Olympic Coast National Marine Sanctuary",
    theme = theme(
      plot.title = element_text(face = "bold")
    )
  )

curvewtitle


###plot ####

####curve graph MB01 ####
#with GAM curve of best fit
curveOC02 <- ggplot(OC02_weekly, aes(x = week, y = total_hours_present)) +
  geom_point() +
  stat_smooth(method = "gam", formula = y ~ s(x, k = 20),
              se = TRUE,
              color = "steelblue",
              fill = "steelblue",
              alpha = 0.2) +
  geom_rect(
    data = OC02_offeffort,
    aes(xmin = start , xmax = end - days(4), ymin = 0, ymax = 72),
    inherit.aes = FALSE,
    fill =  "gray95") +
  scale_x_date(
    date_labels = "%b %Y",   # shows abbreviated month + year on x-axis
    date_breaks = "3 months",
    date_minor_breaks = "1 month"  # one tick per month
  ) +
  coord_cartesian(ylim = c(0, 72))+
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(24, 48, 72, 96, 120, 144, 168))+
  labs(
    title = "OC02",
    x = "Week",
    y = "Total hours with orca call presence"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")


curveOC02


curvewtitle <- (curveOC02) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Passive acoustic monitoring of killer whales in Olympic Coast National Marine Sanctuary",
    theme = theme(
      plot.title = element_text(face = "bold")
    )
  )

curvewtitle




#save
outDir = "C:/Users/pam_user/Desktop/SoundscapesWebsiteDev/code/plot_Humpback&Orca/plots"

ggsave(filename = paste0(outDir, "/plot_OC02OrcaEchotypeDet.jpg"), plot = curvewtitle, dpi = 300)






# Save a data frame to CSV
write.csv(orca_complete, ".csv", row.names = FALSE)


#chcking if GAM is appropraite
#acf(MB01_weekly_clean$total_hours_present)

#library(mgcv)

# Create a time variable if you don't already have one
#MB01_weekly_clean$time <- 1:nrow(MB01_weekly_clean)

# Fit the GAM with AR(1) correlation
#gamm_fit <- gamm(total_hours_present ~ s(week, k=20), 
# correlation = corAR1(form = ~ week),
# data = MB01_weekly_clean)

#MB01_weekly_clean$fit <- gamm_fit$gam$fitted.values

#ggplot(MB01_weekly_clean, aes(x = week, y = total_hours_present)) +
# geom_point(alpha = 0.5) +
#geom_line(aes(y = fit), color = "blue", size = 1)



#PAMSCAPES graph
#From Taiki - make detection datasets
detData <- loadDetectionData(x= OC02Hour,
                             source='csv', detectionType='auto', wide=TRUE,
                             tz='UTC',
                             columnMap=list(UTC='hour_presence', end='end_time'),
                             speciesCols='Orca',
                             detectedValues='1')

runDetectionExplorer(detData)



#NEW PLOTS - INDICATOR GRAPHICS

#BOXPLOT ####
# 03/08/2019 to 10/21/2021

plotDetectionBoxplot(x=detData, group='species', bin='hour/week', combineYears=TRUE)+
  theme(legend.position = "none",
        plot.title = element_text(face = "bold")) +
  labs(title = " OC02: Weekly variation of killer whale calls b/w Mar 2019 and Oct 2021")



