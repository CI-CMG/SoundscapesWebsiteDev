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
OC02_01$end_time[4] <- "2019-03-10T21:50:28Z"

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


unique(OC02$Echotype)


OC02$Echotype <- gsub('T','Transient', OC02$Echotype)
OC02$Echotype <- gsub(' Transient','Transient', OC02$Echotype)
OC02$Echotype <- gsub('NRKW/SRKW','NRKW,SRKW', OC02$Echotype)
OC02$Echotype <- gsub('SRKW,NRKW','NRKW,SRKW', OC02$Echotype)
OC02$Echotype <- gsub('UNK','Unknown killer whales', OC02$Echotype)
OC02$Echotype <- gsub('SRKW','Southern resident killer whales', OC02$Echotype)
OC02$Echotype <- gsub('NRKW','Northern resident killer whales', OC02$Echotype)
OC02$Echotype <- gsub('URK','Unknown resident killer whales', OC02$Echotype)
OC02$Echotype <- gsub('Unknown resident killer whalesW','Unknown resident killer whales', OC02$Echotype)


OC02$Echotype <- gsub('NRKW?','Northern resident killer whales', OC02$Echotype)


#questions for Lindsey: include the ? logs and what to do about hours that had two echotypes? probably split and add one point to each echoype line instead of having them joined



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






#LINE GRAPH ####

##initial formatting####
#get data ready for line graph 
#WATCH OUT because this makes Song and NonSong numeric, which is needed for line graph, but character is needed for effort graphs
# otherCsv1 <- otherCsv1 %>%
#   mutate(
#     datetime = mdy_hm(Hour),  # convert to POSIXct
#     Song = as.numeric(Song),
#     NonSong = as.numeric(NonSong)
#   )
# 
# 
# 
# #get detection data for all three sites to then be able to get off effort
# # Put the three data frames into a list:
# otherCsv_list <- list(otherCsv1, otherCsv2, otherCsv3)
# 
# # List to store outputs
# detData_list <- list()
# 
# #format data to get detection dataset
# for (i in seq_along(otherCsv_list)) { #i = 1
#   
#   otherCsv <- otherCsv_list[[i]]
#   
#   otherCsv <- otherCsv %>%
#     rename(HourStart = Hour)
#   
#   otherCsv <- otherCsv %>% 
#     mutate(HourEnd = lead(HourStart))
#   
#   #filling in end hour for last hour bin 
#   otherCsv$HourEnd[26520] <- "11/22/2021 8:00"
#   
#   otherCsvSong <- otherCsv %>%
#     select(-NonSong) %>%
#     mutate(Humpback = Song) %>%
#     select(-Song)
#   
#   # Create detection dataset w/ effort
#   detData_list[[i]] <- loadDetectionData(
#     x = otherCsvSong,
#     source = "csv",
#     detectionType = "auto",
#     wide = TRUE,
#     tz = "UTC",
#     columnMap = list(UTC = "HourStart", end = "HourEnd"),
#     speciesCols = "Humpback",
#     detectedValues = c("1", "0", NA))
# }
# 
# names(detData_list) <- c("detData1E", "detData2E", "detData3E")
# 
# 

###format data ####
#format data for Line graph 

OC02_weekly <- OC02Hour %>%
  mutate(
    week = floor_date(hour_presence, "week")
  ) %>%
  group_by(week) %>%
  summarise(
    total_hours_present = sum(Orca, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  complete(
    week = seq(min(week), max(week), by = "1 week"),
    fill = list(total_hours_present = 0)
  ) %>%
  arrange(week)




OC02_weekly <- OC02_weekly %>%
  filter(
    !(
      (week >= as.Date("2019-04-25") & week <= as.Date("2019-07-10")) |
        (week >= as.Date("2019-10-31") & week <= as.Date("2020-07-11")) |
        (week >= as.Date("2020-10-02") & week <= as.Date("2021-06-05"))
    )
  )





###effort ####

#OLD WAY to get effort when off effort week = when 1 hour of na or more
#use detection dataset to mark when off effort
#effort <- detData_list[[1]]
# mark which times on vs off effort
#effort$on_effort <- !is.na(effort$detectedFlag)
# note when effort changes from on to off so we can group them
#effort$eff_change <- FALSE
#effort$eff_change[2:nrow(effort)] <- effort$on_effort[1:(nrow(effort)-1)] != effort$on_effort[2:nrow(effort)]
# label the groups with numbers
#effort$eff_group <- cumsum(effort$eff_change)
# get the start and end times of each off effort group
#effortOff1 <- effort %>%
# filter(on_effort == FALSE) %>%
# group_by(eff_group) %>%
# summarise(start=min(UTC), end=max(end))
#off_effort_weeks <- effortOff1 %>%
# rowwise() %>%
# mutate(
#  week_seq = list(seq(
#    from = floor_date(start, "week"),
#    to   = floor_date(end,   "week"),
#     by   = "1 week"
#   ))
#  ) %>%
# unnest(week_seq) %>%
# ungroup() %>%
# distinct(week_seq) %>%     # avoid duplicates
# mutate(
#   xmin = week_seq,
#   xmax = week_seq + weeks(1)
# )
#plot after removing data from off effort weeks
#MB01_weekly_clean <- MB01_weekly %>%
# anti_join(off_effort_weeks, by = c("week" = "week_seq"))

# 
# #NEW EFFORT for MB01 when na > 75% of hours in week
# effort <- detData_list[[1]]
# 
# # Mark on-effort (non-NA)
# effort$on_effort <- !is.na(effort$detectedFlag)
# 
# # Extract week
# effort$week <- floor_date(effort$UTC, "week")
# 
# # Count NA hours per week
# weekly_effort_summary <- effort %>%
#   group_by(week) %>%
#   summarize(
#     na_hours = sum(is.na(detectedFlag)),
#     total_hours = n()
#   ) %>%
#   mutate(
#     off_effort = na_hours >= 126 
#   )
# 
# #dataset with off effort weeks
# off_effort_weeksMB01 <- weekly_effort_summary %>%
#   filter(off_effort) %>%
#   mutate(
#     xmin = week,
#     xmax = week + weeks(1)
#   )
# 
# 
# #get rid of off effort data points
# MB01_weekly_clean <- MB01_weekly %>%
#   anti_join(weekly_effort_summary %>% filter(off_effort),
#             by = c("week"))
# 
# 
# 



#off effort weeks
OC02_offeffort <- data.frame(
  start = c("04-25-2019", "10-31-2019", "10-02-2020"),
  end = c("07-10-2019", "07-11-2020", "06-05-2021")
)


OC02_offeffort$start <- as.Date(OC02_offeffort$start, format = "%m-%d-%Y")
OC02_offeffort$end <- as.Date(OC02_offeffort$end, format = "%m-%d-%Y")

# Create a cleaned version of your data
# daily_cleaned <- OC02_weekly
# 
# for(i in 1:nrow(OC02_offeffort)) {
#   gap_start <- OC02_offeffort$start[i]
#   gap_end   <- OC02_offeffort$end[i]
#   
#   # Set the sound level to NA for these Julian days
#   daily_cleaned$HMD_50[daily_cleaned$Julian >= start & 
#                          daily_cleaned$Julian <= end] <- NA
# }



library(dplyr)
library(ivreg) # Only if using interval joins, otherwise base dplyr works

# Assume primary data is 'orca_data' and off-effort is 'off_effort'
orca_cleaned <- OC02_weekly %>%
  # Create a temporary flag by checking if 'week_start' is between any off-effort start/end
  rowwise() %>%
  mutate(is_off_effort = any(week >= OC02_offeffort$start & 
                               week <= OC02_offeffort$end)) %>%
  ungroup() %>%
  # Set hours to NA if the flag is TRUE
  mutate(total_hours_present = ifelse(is_off_effort, NA, total_hours_present)) %>%
  select(-is_off_effort)


# 1. Create a complete sequence of weeks from your data's start to end
full_weeks <- data.frame(
  week = seq(min(OC02_weekly$week), 
             max(OC02_weekly$week), 
             by = "1 week")
)

# 2. Join existing data and identify off-effort weeks
orca_complete <- full_weeks %>%
  left_join(OC02_weekly, by = "week") %>%
  rowwise() %>%
  mutate(
    # Check if this week_start falls within any off-effort start/end window
    is_off = any(week >= OC02_offeffort$start & week <= OC02_offeffort$end),
    # Set hours to NA if it was missing from orca_data OR if it's an off-effort week
    total_hours_present = if_else(is_off, NA_real_, total_hours_present)
  ) %>%
  ungroup() %>%
  select(-is_off)






# 1. Identify continuous blocks of data
orca_complete <- orca_complete %>%
  mutate(has_data = !is.na(total_hours_present),
         # Create a group ID that changes every time a gap (NA) occurs
         group_id = cumsum(is.na(total_hours_present) != lag(is.na(total_hours_present), default = FALSE)))

# 2. Add 'group = group_id' to your ggplot mapping
ggplot(orca_complete, aes(x = week, y = total_hours_present, group = group_id)) +
  geom_point() +
  coord_cartesian(ylim = c(0, 168))+
  stat_smooth(method = "gam", formula = y ~ s(x, k = 8), # k adjusted for segments
              na.rm = FALSE)









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



curveOC02 + facet_wrap(~ Echotype, ncol = 1)


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
    title = "Passive acoustic monitoring of orca in Olympic Coast National Marine Sanctuary",
    theme = theme(
      plot.title = element_text(face = "bold")
    )
  )

curvewtitle




#save
outDir = "C:/Users/embe5980/SoundscapesWebsite/code/plot_Humpback&Orca/plots"

ggsave(filename = paste0(outDir, "/plot_OC02OrcaDetectionsV4.jpg"), plot = curvewtitle, dpi = 300)






# Save a data frame to CSV
write.csv(OC02_weekly, ".csv", row.names = FALSE)


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


