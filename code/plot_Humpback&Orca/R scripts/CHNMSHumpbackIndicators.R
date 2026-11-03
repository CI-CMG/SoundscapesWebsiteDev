#Second Draft of CHNMS Humpback Detection graphs


# RUN this to make sure latest updates for PAMscapes
devtools::install_github('TaikiSan21/PAMscapes')

#install.packages("rJava") make sure Java is installed for xlsx to work

library(PAMscapes)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)
library(rJava)
library(xlsx)
library(openxlsx)
library(devtools)
library(plotly)
library(tidyr)
library(readxl)
library(purrr)
library(grid)
library(patchwork)

rm(list=ls()) 


#Load in
folder <- "X:\\Emma_Beretta\\SoundscapesWebsiteDev\\code\\plot_Humpback&Orca\\data\\CHNMS\\absence_formattedforplotting"

# Get all .xlsx files, skipping Excel's temp lock files (~$...)
files <- list.files(folder, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)
files <- files[!grepl("^~\\$", basename(files))]

dfs <- files |>
  set_names(basename(files)) |>
  map(read_excel)

combined <- bind_rows(dfs, .id = "source_file")

# 
# #Clean up! FOR BOXPLOTS
# #uu = 1
# 
# #for (uu in 3) {
# #change for what site u want
# otherCsv <- otherCsv1
# 
# #need to add a end detection time column
# otherCsv <- otherCsv %>%
#   dplyr::rename(HourStart = Hour) #need dplyr:: bc there is also a rename function in the reshape package
# 
# otherCsv <- otherCsv %>% mutate(HourEnd = lead(HourStart))
# 
# #filling in nd hour for last hour bin (row in dataset)
# otherCsv$HourEnd[26520] <- "11/22/2021 8:00"
# 
# #for Song data
# otherCsvSong <- otherCsv %>% select(-NonSong)
# otherCsvSong$Humpback <- otherCsvSong$Song
# otherCsvSong <- otherCsvSong %>% select(-Song)
# 
# #for Non Song data
# # otherCsvNonSong <- otherCsv %>% select(-Song)
# # otherCsvNonSong$Humpback <- otherCsvNonSong$NonSong
# # otherCsvNonSong <- otherCsvNonSong %>% select(-NonSong)
# 
# #From Taiki - make detection datasets
# #change x= for if you want Song or NonSong detection data
# #detData1 = MB01, etc
# detData1 <- loadDetectionData(x=otherCsvSong,
#                               source='csv', detectionType='auto', wide=TRUE,
#                               tz='UTC',
#                               columnMap=list(UTC='HourStart', end='HourEnd'),
#                               speciesCols='Humpback',
#                               detectedValues='1')
# 
# 
# #effort detection dataset, doesnt work for making presence graphs
# detData1EN <- loadDetectionData(x=otherCsvNonSong,
#                                 source='csv', detectionType='auto', wide=TRUE,
#                                 tz='UTC',
#                                 columnMap=list(UTC='HourStart', end='HourEnd'),
#                                 speciesCols='Humpback',
#                                 detectedValues=c('1', '0', 'N/A'))
# 
# #}
# 
# 
# 
# MBHump_allDet <- bind_rows(detData1, detData2, detData3)
# MBHump_allDetN <- bind_rows(detData1N, detData2N, detData3N)
# 
# 
# 
# #NEW PLOTS
# 
# #BOXPLOTS ####
# 
# ##MB01 vs MB02 vs MB03 ####
# #boxplot showing all three sites for Hump SONG hrs/wk   
# plotDetectionBoxplot(x=MBHump_allDet, group='species', facet='Site', bin='hour/week', combineYears=TRUE) + theme(legend.position = "none")
# 
# #boxplot showing all three sites for Hump SONG hrs/month 
# plotDetectionBoxplot(x=MBHump_allDet, group='species', facet='Site', bin='hour/month', combineYears=TRUE) + theme(legend.position = "none")
# 
# #boxplot showing all three sites for Hump NON SONG hrs/wk   
# plotDetectionBoxplot(x=MBHump_allDetN, group='species', facet='Site', bin='hour/week', combineYears=TRUE) + theme(legend.position = "none")
# 
# #boxplot showing all three sites for Hump NON SONG hrs/month 
# plotDetectionBoxplot(x=MBHump_allDetN, group='species', facet='Site', bin='hour/month', combineYears=TRUE) + theme(legend.position = "none")
# 
# 
# ## Song vs NonSong ####
# #trying to get song and non song on one graph
# #YOU CANT^ because you can only facet by one variable at a time on PAMscapes
# MBHump_allDet$Call <- "Song"
# MBHump_allDetN$Call <- "Non Song"
# 
# MBHump_Detections <- bind_rows(MBHump_allDet, MBHump_allDetN)
# MB01Hump_Detections <- filter(MBHump_Detections, Site == "MB01")
# MB02Hump_Detections <- filter(MBHump_Detections, Site == "MB02")
# MB03Hump_Detections <- filter(MBHump_Detections, Site == "MB03")
# 
# 
# #Each site, showing song vs non song in facetted boxplot
# 
# plotDetectionBoxplot(x = MB01Hump_Detections, group = 'species', facet = 'Call',  bin = 'hour/week', combineYears = TRUE) +
#   theme(legend.position = "none",
#         plot.title = element_text(face = "bold")) +
#   labs(title = " MB01: Variation of weekly humpback whale song calls vs non song calls b/w 2018 and 2021")
# 
# #2018-11-15 and 2021-11-20
# 
# plotDetectionBoxplot(x=MB02Hump_Detections, group='species', facet='Call', bin='hour/week', combineYears=TRUE) + 
#   theme(legend.position = "none",
#         plot.title = element_text(face = "bold")) +
#   labs(title = " MB02: Variation of weekly humpback whale song calls vs non song calls b/w 2018 and 2021")
# #2018-11-15 and 2021-04-17
# 
# plotDetectionBoxplot(x=MB03Hump_Detections, group='species', facet='Call', bin='hour/week', combineYears=TRUE) + 
#   theme(legend.position = "none",
#         plot.title = element_text(face = "bold")) +
#   labs(title = " MB03: Variation of weekly humpback whale song calls vs non song calls b/w 2018 and 2021")
# #2018-11-15 and 2021-05-31
# 
# runDetectionExplorer(MB01Hump_Detections)
# 
# 
# ##Everything ####
# #song vs non song for all sites NOT using pamscapes
# ggplot(MBHump_Detections, aes(x = as.factor(Week), y = CallCount, fill = CallType)) +
#   geom_boxplot(outlier.alpha = 0.3) +
#   facet_wrap(~ Site, ncol = 1) +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
#   xlab("Week of Year")






#LINE GRAPHS ####

##initial formatting####
#get data ready for line graph (NRS13 song)

  combined <- combined %>%
    rename(HourStart = Hour)
  
  combined <- combined %>%
    rename(Song = "Song Presence")
  
  combined <- combined %>% 
    mutate(HourEnd = lead(HourStart))
  

  combinedT <- combined |>
    mutate(Song = if_else(NoData == 1, NA_real_, Song))
  
  # Create detection dataset w/ effort
  detData <- loadDetectionData(
    x = combinedT,
    source = "csv",
    detectionType = "auto",
    wide = TRUE,
    tz = "UTC",
    columnMap = list(UTC = "HourStart", end = "HourEnd"),
    speciesCols = "Song",
    detectedValues = c("1", "0", NA))



##MB01 ####

###format data ####
#format data for Line graph with ONLY MB01

#song
NRS13Song_weekly <- combinedT %>%
  mutate(
    week = floor_date(HourStart, "week"),  # get the start of each week
    year = year(HourStart)
  ) %>%
  group_by(week, year) %>%
  summarise(
    total_hours_present = sum(Song, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(CallType = "Song")




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


#NEW EFFORT for MB01 when na > 75% of hours in week
effort <- detData

# Mark on-effort (non-NA)
effort$on_effort <- !is.na(effort$detectedFlag)

# Extract week
effort$week <- floor_date(effort$UTC, "week")

# Count NA hours per week
weekly_effort_summary <- effort %>%
  group_by(week) %>%
  summarize(
    na_hours = sum(is.na(detectedFlag)),
    total_hours = n()
  ) %>%
  mutate(
    off_effort = na_hours >= 126 
  )

#dataset with off effort weeks
off_effort_weeksNRS13 <- weekly_effort_summary %>%
  filter(off_effort) %>%
  mutate(
    xmin = week,
    xmax = week + weeks(1)
  )


#get rid of off effort data points
NRS13_weekly_clean <- NRS13Song_weekly %>%
  anti_join(weekly_effort_summary %>% filter(off_effort),
            by = c("week"))


###plot ####

####line graph MB01 ####
# lineMB01 <- ggplot(NRS13_weekly_clean, aes(x = week, y = total_hours_present, color = CallType)) +
#   geom_line(size = 1) +
#   geom_point(size = 2)+
#   geom_rect(
#     data = off_effort_weeksNRS13,
#     aes(xmin = xmin - as.difftime(3.5, units = "days"), xmax = xmax- as.difftime(3.5, units = "days"), ymin = -1, ymax = Inf),
#     inherit.aes = FALSE,
#     fill = "grey"
#   ) + 
#   scale_x_date(
#     date_labels = "%b %Y",   # shows abbreviated month + year on x-axis
#     date_breaks = "3 months",
#     date_minor_breaks = "1 month"  # one tick per month
#   ) +
#   scale_color_manual(
#     values = c(
#       "Song" = "steelblue",
#       "NonSong" = "firebrick"
#     )
#   )+
#   labs(
#     title = "NRS13",
#     x = "Week",
#     y = "",
#     color = "Call Type"
#   ) +
#   coord_cartesian(ylim = c(0, 168))+
#   theme_minimal() +
#   scale_y_continuous(expand = c(0,0), breaks = c(24, 48, 72, 96, 120, 144, 168))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   theme(axis.title.x = element_blank(),
#         axis.text.x  = element_blank(),
#         axis.ticks.x = element_blank())
# 
# 
# lineMB01


####curve graph NRS13 ####
#with GAM curve of best fit
curveNRS13 <- ggplot(NRS13_weekly_clean, aes(x = week, y = total_hours_present, color = CallType)) +
  geom_point() +
  scale_color_manual(
    values = c(
      "Song" = "steelblue"
    )
  )+
  scale_fill_manual(values = c("Song" = "steelblue")) + 
  stat_smooth(method = "gam", formula = y ~ s(x, k = 20), se = TRUE,
              aes(fill = CallType),   # NEW: fill follows CallType
              alpha = 0.2  ) +
  geom_rect(
    data = off_effort_weeksNRS13,
    aes(xmin = xmin - days(4), xmax = xmax-days(4), ymin = 0, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#E5E5E5"
  ) + scale_x_date(
    date_labels = "%b %Y",   # shows abbreviated month + year on x-axis
    date_breaks = "3 months",
    date_minor_breaks = "1 month"  # one tick per month
  ) +
  coord_cartesian(ylim = c(0, 168))+
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(24, 48, 72, 96, 120, 144, 168))+
  labs(
    title = "NRS13",
    x = "Week",
    y = "Total hours with humpback whale call presence",
    color = "Call Type"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") +
  guides(fill = "none")

curveNRS13


# Save a data frame to CSV
write.csv(MB01_weekly_clean, "MB01_weekly_clean.csv", row.names = FALSE)


patchedC <- (curveNRS13) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Passive acoustic monitoring of humpback whales in Chumash Heritage National Marine Sanctuary",
    theme = theme(
      plot.title = element_text(face = "bold")
    )
  )

patchedC


#save
outDir = "X:/Emma_Beretta/SoundscapesWebsiteDev/code/plot_Humpback&Orca/plots"

ggsave(filename = paste0(outDir, "/plot_CHNMSHumpbackWhaleDetections.jpg"), plot = patchedC, dpi = 300)




