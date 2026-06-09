#use this script after running plot_ONMS-conditions_HMD.R

library(htmlwidgets)
library(htmltools)
library(plotly)

# 
# # 1. Build the plot as you did, but add a 'text' aesthetic for the hover tooltip
# pInt <- ggplot() +
#   # Wind models (keep as is)
#   geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], aes(x = variable, y = value), color = "black", linewidth = 1) +
#   geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], aes(x = variable, y = value), color = "black", linewidth = 1) +
#   
#   # Frequency Ranges (Shaded Regions)
#   geom_rect(data = FOIs, 
#             aes(xmin = FQstart, xmax = FQend), 
#             ymin = 30, ymax = 100, # Use actual numbers instead of Inf for plotly stability
#             fill = "gray", alpha = 0.2,
#             inherit.aes = FALSE) + # This prevents it from looking for 'Year' or 'text' in FOIsgeom_rect(data = FOIsRange, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.2) +
#   
#   # Ribbons (25%-75% quantiles)
#   geom_ribbon(data = mallData %>% filter(Year != oldest_year) %>% pivot_wider(names_from = Quantile, values_from = SoundLevel),
#               aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year), alpha = 0.1) +
#   geom_ribbon(data = mallData %>% filter(Year == oldest_year) %>% pivot_wider(names_from = Quantile, values_from = SoundLevel),
#               aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year), alpha = 0.3) +
#   
#   # MAIN DATA LINE (Adding the 'text' aesthetic here for the tooltip)
#   geom_line(data = mallData[mallData$Quantile == "50%",], 
#             aes(x = Frequency, y = SoundLevel, color = Year, 
#                 text = paste("Year:", Year, "<br>Freq:", Frequency, "Hz<br>Level:", round(SoundLevel, 1), "dB")), 
#             linewidth = 1.5) +
#   
#   # Median of all data
#   geom_line(data = mALL[mALL$Quantile == "50%",], aes(x = Frequency, y = SoundLevel), 
#             color = "black", linewidth = 0.8, linetype = "dotted") +
#   
#   # Scales & Themes
#   scale_x_log10(labels = scales::label_number(), limits = c(10, fqupper)) +
#   scale_y_continuous(limits = c(30, NA)) +
#   scale_color_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(mallData$Year))))) +
#   scale_fill_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(mallData$Year))))) +
#   theme_minimal() +
#   labs(x = "Frequency (Hz)", y = "dB re 1 uPa^2/Hz", color = legend_label, fill = legend_label)
# 
# # 2. Convert to plotly
# # 'tooltip = "text"' tells plotly to ONLY show what we defined in the paste() function above
# p_interactive <- ggplotly(pInt, tooltip = "text") %>% 
#   layout(legend = list(orientation = "v", x = 1, y = 0.5))
# 
# # Display
# p_interactive
# 
# 
# #INTERACTIVE PLOT
# #changes to make to ggplot above
# #still trying to figure out FOI vert lines and labels in plotly
# #move geom_ribbon to after geom_lines
# #make geom_rect y bounds: ymin = 35, ymax = 90
# #add fill = Year to geom_line 
# #add color = Year to geom_ribbon
# #add "name = "Year", " to  scale_fill_manual and scale_color_manual
# #then run code below
# 
# mallData_recent <- mallData %>%
#   filter(Year != oldest_year) %>%
#   pivot_wider(names_from = Quantile, values_from = SoundLevel)
# 
# mallData_oldest <- mallData %>%
#   filter(Year == oldest_year) %>%
#   pivot_wider(names_from = Quantile, values_from = SoundLevel)
# 
# pInt  = ggplot() +
#   #wind model
#   geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], aes(x = variable, y = value), color = "black", linewidth = 1) +
#   geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], aes(x = variable, y = value), color = "black", linewidth = 1) +
#   scale_x_log10(labels = label_number(),limits = (c(10,fqupper)), guide = "axis_logticks") +  # Log scale for x-axis
#   
#   scale_color_manual(name = "Year", values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
#   scale_fill_manual(name = "Year", values =  rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
#   # 
#   # # Add vertical lines at FOIs, label on right side
#   # geom_vline(data = FOIs, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5, inherit.aes = FALSE) +
#   # geom_text(data = FOIs, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 1, hjust = 0.45, size = 4, inherit.aes = FALSE) +
#   # geom_rect(data = FOIs, aes(xmin = FQstart, xmax = FQend, ymin = 30, ymax = 90),
#   #           fill = "gray", alpha = 0.2, inherit.aes = FALSE) +  # Adjust alpha for transparency
#   # 
#   # # Add vertical lines at FOIs, label on left side
#   # geom_vline(data = FOIsL, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5, inherit.aes = FALSE) +
#   # geom_text(data = FOIsL, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 0, hjust = 0.5, size = 4, inherit.aes = FALSE) +
#   # 
#   # # Add vertical set dash lines and grey shaded region at FOI ranges
#   # #geom_vline(data = FOIsRange, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "red",linewidth = .5) +
#   # #geom_vline(data = FOIsRange, aes(xintercept = FQend, color = Label), linetype = "dashed", color = "red",linewidth = .5) +
#   # geom_rect(data = FOIsRange, aes(xmin = FQstart, xmax = FQend, ymin = 30, ymax = 90), 
#   #           fill = "gray", alpha = 0.2, inherit.aes = FALSE)+  # Adjust alpha for transparency
#   # geom_text(data = FOIsRange, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 1, hjust = 0.45, size = 4, inherit.aes = FALSE) +
#   # 
#   # # Add vertical set dash lines and grey shaded region at FOI ranges, label on left
#   # #geom_vline(data = FOIsRangeL, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "red",linewidth = .5) +
#   # #geom_vline(data = FOIsRangeL, aes(xintercept = FQend, color = Label), linetype = "dotdash", color = "red",linewidth = .5) +
#   # geom_rect(data = FOIsRangeL, aes(xmin = FQstart, xmax = FQend, ymin = 30, ymax = 90), 
#   #           fill = "gray", alpha = 0.2, inherit.aes = FALSE)+  # Adjust alpha for transparency
#   # geom_text(data = FOIsRangeL, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 0, hjust = 0.5, size = 4, inherit.aes = FALSE) +
#   # 
#   # # geom_ribbon(data = mallData %>% pivot_wider(names_from = Quantile, values_from = SoundLevel),
#   # #             aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year), alpha = 0.2) +
#   # 
#  
#   #median HMD values- each year
#   geom_line(data = mallData[mallData$Quantile == "50%",], 
#             aes(x = Frequency, y = SoundLevel, color = Year, fill = Year, 
#                 text = paste("Year: ", Year)), linewidth = 2) +
#   
#   #median HMD values- all data
#   geom_line(data = mALL[mALL$Quantile == "50%",], 
#             aes(x = Frequency, y = SoundLevel), color = "black", linewidth = 1,
#             linetype = "dotted") +
#   scale_y_continuous(limits = c(30, NA)) +  # use to manually scale y minimum so vert line labels are visible
#   #for the geom_ribbons below, if data only has one year (ch01 and fk08), comment out the first geom ribbon and change alpha of second from .3 to .1
#  
#    geom_ribbon(data = mallData %>%
#                 filter(Year != oldest_year) %>%
#                 pivot_wider(names_from = Quantile, values_from = SoundLevel),
#               aes(x = Frequency, ymin = `25%`, ymax = `75%`, 
#                   fill = Year, color = Year, text = paste("Year: ", Year)),
#               alpha = 0.1) +
#   
#   #for the oldest year, make the shading darker since it is hard to see at alpha = .1 for lightblue
#   geom_ribbon(data = mallData %>% 
#                 filter(Year == oldest_year) %>% 
#                 pivot_wider(names_from = Quantile, values_from = SoundLevel),
#               aes(x = Frequency, ymin = `25%`, ymax = `75%`, 
#                   fill = Year, color = Year, text = paste("Year: ", Year)), 
#               alpha = 0.3) + # High alpha for visibility
#   
#   # Additional aesthetics
#   theme_minimal() +
#   labs(
#     #title = paste0(toupper(site), "(",siteInfo$`Oceanographic category`, ")"), 
#     caption  = caption_text,
#     color = legend_label,        #IF biological then change to Year*
#     fill = legend_label,        #IF biological then change to Year*
#     x = "Frequency Hz",
#     y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa"^2, "/Hz)" ) ),
#     subtitle = subtitle_text) +
#   theme(legend.position = "right",
#         plot.caption = ggtext::element_markdown(hjust = 0, size = 12),
#         axis.title.x = element_text(size = 14),           # X-axis label size
#         axis.title.y = element_text(size = 14),           # Y-axis label size
#         axis.text = element_text(size = 14),
#         legend.text = element_text(size = 12),
#         axis.ticks.length.x = unit(0.25, "cm"), 
#         axis.ticks.x = element_line(color = "grey", linewidth = 0.3), 
#         axis.line.x = element_line(color = "grey", linewidth = 0.3)    
#   ) 
# 
# pInt
# 
# #make plot interactive
# interactive_plot <- ggplotly(pInt)
# 
# #remove hover info over grey shading
# #interactive_plot$x$data[[14]]$hoverinfo <- "skip"
# 
# interactive_plot
# 
# 
# p_interactive <- ggplotly(pInt, tooltip = "text")
# 
# p_interactive
# 
# #  interactive_plot$x$layout$xaxis$type
# # interactive_plot$x$data[[18]]$textangle <- 90  
# 
# #figure out which trace has the grey rectangles show "trace 13" when you hover
# # for (i in seq_along(interactive_plot$x$data)) {
# #  cat("TRACE", i, "\n")
# #print(interactive_plot$x$data[[i]][c("name","fill","mode","hoverinfo")])
# #}
# 


#TRYING NEW METHOD using ggiraph




# 
# #plot
# p = ggplot() +
#   #wind model
#   geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], aes(x = variable, y = value), color = "black", linewidth = 1) +
#   geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], aes(x = variable, y = value), color = "black", linewidth = 1) +
#   scale_x_log10(labels = label_number(),limits = (c(10,fqupper)), guide = "axis_logticks") +  # Log scale for x-axis
#   
#   scale_color_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
#   scale_fill_manual(values =  rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
#   
#   # Add vertical lines at FOIs, label on right side
#   geom_vline(data = FOIs, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
#   geom_text(data = FOIs, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 1, hjust = 0.45, size = 4) +
#   geom_rect(data = FOIs, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf),
#             fill = "gray", alpha = 0.2) +  # Adjust alpha for transparency
#   
#   # Add vertical lines at FOIs, label on left side
#   geom_vline(data = FOIsL, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "black",linewidth = .5) +
#   geom_text(data = FOIsL, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 0, hjust = 0.5, size = 4) +
#   
#   # Add vertical set dash lines and grey shaded region at FOI ranges
#   #geom_vline(data = FOIsRange, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "red",linewidth = .5) +
#   #geom_vline(data = FOIsRange, aes(xintercept = FQend, color = Label), linetype = "dashed", color = "red",linewidth = .5) +
#   geom_rect(data = FOIsRange, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf), 
#             fill = "gray", alpha = 0.2)+  # Adjust alpha for transparency
#   geom_text(data = FOIsRange, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 1, hjust = 0.45, size = 4) +
#   
#   # Add vertical set dash lines and grey shaded region at FOI ranges, label on left
#   #geom_vline(data = FOIsRangeL, aes(xintercept = FQstart, color = Label), linetype = "dashed", color = "red",linewidth = .5) +
#   #geom_vline(data = FOIsRangeL, aes(xintercept = FQend, color = Label), linetype = "dotdash", color = "red",linewidth = .5) +
#   geom_rect(data = FOIsRangeL, aes(xmin = FQstart, xmax = FQend, ymin = -Inf, ymax = Inf), 
#             fill = "gray", alpha = 0.2)+  # Adjust alpha for transparency
#   geom_text(data = FOIsRangeL, aes(x = FQstart, y = label_height, label = Label), angle = 90, vjust = 0, hjust = 0.5, size = 4) +
#   
#   # geom_ribbon(data = mallData %>% pivot_wider(names_from = Quantile, values_from = SoundLevel),
#   #             aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year), alpha = 0.2) +
#   
#   #for the geom_ribbons below, if data only has one year (ch01 and fk08), comment out the first geom ribbon and change alpha of second from .3 to .1
#   geom_ribbon_interactive(data = mallData %>%
#                 filter(Year != oldest_year) %>%
#                 pivot_wider(names_from = Quantile, values_from = SoundLevel),
#               aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year, data_id = Year,           # <-- must match the line's data_id
#                   tooltip = paste0("Year: ", Year)),
#               alpha = 0.1) +
#   
#   #for the oldest year, make the shading darker since it is hard to see at alpha = .1 for lightblue
#   geom_ribbon_interactive(data = mallData %>% 
#                 filter(Year == oldest_year) %>% 
#                 pivot_wider(names_from = Quantile, values_from = SoundLevel),
#               aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year, data_id = Year,           # <-- must match the line's data_id
#                   tooltip = paste0("Year: ", Year)), 
#               alpha = 0.3) + # High alpha for visibility
#   
#   #median HMD values- each year
#   geom_line_interactive(data = mallData[mallData$Quantile == "50%",], 
#                         aes(x = Frequency, y = SoundLevel, color = Year, group = Year,
#                             tooltip = paste0("Year: ", Year),                                      
#                             data_id = Year), 
#                         linewidth = 2) +
#   
#   # geom_point_interactive(data = mallData[mallData$Quantile == "50%",],
#   #                        aes(x = Frequency, y = SoundLevel, 
#   #                            color = Year,
#   #                            data_id = Year,
#   #                              #paste0(Year, "_", Frequency),  # unique per point
#   #                            tooltip = paste0("Year: ", Year,
#   #                                             "<br>Frequency: ", round(Frequency, 1), " Hz",
#   #                                             "<br>Sound Level: ", round(SoundLevel, 1), " dB")),
#   #                        size = 1,      # small points
#   #                        alpha = 0) +   # fully transparent
#   
#   #median HMD values- all data
#   geom_line_interactive(data = mALL[mALL$Quantile == "50%",], 
#                         aes(x = Frequency, y = SoundLevel, group = 1,
#                                                                   
#                             # tooltip = paste0("All Years Median<br>Freq: ", 
#                             #                  Frequency, " Hz<br>Level: ", 
#                             #                  round(SoundLevel, 1), " dB"),
#                             tooltip = paste0("Year: ", Year),
#                                              # "<br>Frequency: ", round(Frequency, 1), " Hz",
#                                              # "<br>Sound Level: ", round(SoundLevel, 1), " dB"),                                      
#                             data_id = "all"), 
#                         color = "black", linewidth = 1,
#                             linetype = "dotted") +
#   
#   # geom_point_interactive(data = mALL[mALL$Quantile == "50%",],
#   #                        aes(x = Frequency, y = SoundLevel, 
#   #                            group = 1,
#   #                            data_id = "all",
#   #                            tooltip = paste0("Year: ", Year,
#   #                                             "<br>Frequency: ", round(Frequency, 1), " Hz",
#   #                                             "<br>Sound Level: ", round(SoundLevel, 1), " dB")),
#   #                        size = 1,      # small points
#   #                        alpha = 0) +   # fully transparent
#   
#   scale_y_continuous(limits = c(30, NA)) +  # use to manually scale y minimum so vert line labels are visible
#   
#   # Additional aesthetics
#   theme_minimal() +
#   labs(
#     #title = paste0(toupper(site), "(",siteInfo$`Oceanographic category`, ")"), 
#     caption  = caption_text,
#     color = legend_label,        #IF biological then change to Year*
#     fill = legend_label,        #IF biological then change to Year*
#     x = "Frequency Hz",
#     y = expression(paste("Sound Levels (dB re 1 ", mu, " Pa"^2, "/Hz)" ) ),
#     subtitle = subtitle_text) +
#   theme(legend.position = "right",
#         plot.caption = ggtext::element_markdown(hjust = 0, size = 12),
#         axis.title.x = element_text(size = 14),           # X-axis label size
#         axis.title.y = element_text(size = 14),           # Y-axis label size
#         axis.text = element_text(size = 14),
#         legend.text = element_text(size = 12),
#         axis.ticks.length.x = unit(0.25, "cm"), 
#         axis.ticks.x = element_line(color = "grey", linewidth = 0.3), 
#         axis.line.x = element_line(color = "grey", linewidth = 0.3)    
#   ) 
# p
# 
# 
# 
# 
# g <- girafe(ggobj = p,
#        width_svg = 9,
#        height_svg = 9,
#        options = list(
#          opts_hover(css = "stroke-width:3;opacity:1;"),
#          opts_hover_inv(css = "opacity:0.1;"),
#          opts_tooltip(css = "background:white;padding:6px;border-radius:4px;border:1px solid #ccc;font-size:12px;", use_cursor_pos = TRUE),
#          opts_sizing(rescale = TRUE, width = 1)
#        ))
# 
# g
# 
# 
# 
# 
# saveWidget(g, file = "myFK06plot.html", selfcontained = TRUE)
# 
# 
# browseURL("myFK06plot.html")
# 
# 
# #Adding effort graph
# 
# p1 = ggplot(summary, aes(x = month, y = dy, fill = as.factor(year))) +
#   geom_col_interactive(aes(tooltip = paste0("Year: ", year, 
#                                             #"<br>Month: ", month.abb[as.integer(month)], 
#                                             "<br>Days: ", dy),
#                            data_id = as.factor(year)), position = "dodge", width = .4) +  # Use dodge to separate bars for each year within the same month
#   #coord_flip() +
#   labs(
#     title = effort_title,
#     subtitle = paste0(toupper(site), " has ", udaysAG, 
#                       " unique days: ", as.character(stAG), " to ", as.character(edAG)),
#     x = "",
#     y = "Days",
#     fill = legend_label,
#     # Update your p1 labs call:
#     #caption = "Data&nbsp;from&nbsp;months&nbsp;with&nbsp;effort&nbsp;below&nbsp;the&nbsp;red&nbsp;horizontal&nbsp;line&nbsp;are&nbsp;excluded&nbsp;from&nbsp;annual&nbsp;sound&nbsp;levels&nbsp;figure&nbsp;above"
#     caption = "Data from months with effort below the red horizontal line are excluded from annual sound levels figure above"
#   ) +
#   scale_x_discrete(labels = month.abb[month_nums]) +  # Show month names instead of numbers
#   #scale_fill_manual(values = rev(gray.colors(length(unique(summary$year))))) +  # Create grayscale colors
#   scale_fill_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(size = 16, face = "bold", hjust = 0),
#     axis.title.y = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     axis.text.x = element_text(size = 14, hjust = 1, angle = 30),  
#     plot.subtitle = element_text(size = 14),
#     legend.text = element_text(size = 12),
#     plot.caption = ggtext::element_markdown(hjust = 0, size = 11),
#     legend.position = "right" 
#   ) +
#   #adding marker for cutoff threshold (months need more than 23 days of data to be kept in line graph)
#   geom_hline(yintercept = siteInfo$MThreshold,    
#              linetype = "dashed",
#              color = "red",
#              linewidth = .5)
# 
# p1
# 
# 
# 
# g1 <- girafe(ggobj = p1,
#                    width_svg = 9,
#                    height_svg = 2.25,
#                    options = list(
#                      opts_hover(css = "opacity:1;stroke-width:2;"),
#                      opts_hover_inv(css = "opacity:0.3;"),
#                      opts_tooltip(css = "background:white;padding:6px;border-radius:4px;border:1px solid #ccc;font-size:12px;"),
#                      opts_sizing(rescale = TRUE)
#                    ))
# 
# g1
# 
# 
# 
# # 
# # #combine effort and SPL interactive graphs
# # combined <- browsable(
# #   tagList(
# #     as.tags(g),
# #     tags$hr(),
# #     as.tags(g1)
# #   )
# # )
# # 
# # #preview
# # html_print(combined)
# # 
# # #save
# # save_html(combined, "combined_plotFK06.html")
# 
# 
# saveWidget(g, "C:/Users/embe5980/SoundscapesWebsiteDev/content/resources/plot_FK06_HMDYearSPLInteractive.html", 
#            selfcontained = TRUE)
# 
# 
# saveWidget(g1, "C:/Users/embe5980/SoundscapesWebsiteDev/content/resources/plot_FK06_HMDEffortInteractive.html", 
#            selfcontained = TRUE)
# 
# 
# 
# # saveWidget(g, file = "myFK06plot.html", selfcontained = TRUE)
# # saveWidget(g1, file = "myFK06effortplot.html", selfcontained = TRUE)
# 
# 
# 
# 
# browseURL("combined_plotFK06.html")
# 
# 
# 
# 
# getwd()
# 
# 
# 
# 
# 
# 




#TRYING plotly again- have to use becuase ggirafe you cant select years 




# Transform your data dynamically into a coordinate path
polygon_data <- FOIsRange %>%
  mutate(id = row_number()) %>%  # Give each unique range row its own tracking ID
  uncount(4) %>%                # Split each row into 4 coordinate points
  group_by(id) %>%
  mutate(
    corner = row_number(),
    # Map out the 4 corners of each rectangle clockwise
    x = case_when(
      corner == 1 ~ FQstart,    # Bottom-Left
      corner == 2 ~ FQstart,    # Top-Left
      corner == 3 ~ FQend,      # Top-Right
      corner == 4 ~ FQend       # Bottom-Right
    ),
    y = case_when(
      corner == 1 ~ 27,         # Bottom boundaries matching your scale min
      corner == 2 ~ 85,         # Top boundaries matching your scale max
      corner == 3 ~ 85,         
      corner == 4 ~ 27          
    )
  ) %>%
  ungroup()



ribbonData <- mallData %>% mutate(is_na = is.na(`SoundLevel`) ,
         # 2. Detect a change: did we just transition into or out of an NA block?
         gap = is_na != lag(is_na, default = first(is_na)),
         # 3. Create a unique segment ID every time a change happens
         segment = cumsum(gap)) %>%
  ungroup()



pl = ggplot() +
  #wind model
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windUpp,], 
            aes(x = variable, y = value, 
                text = "Modeled Max Wind Noise"), color = "black", linewidth = 1) +
  
  geom_line(data = mwindInfo[as.character(mwindInfo$windSpeed) == windLow,], 
            aes(x = variable, y = value, 
                text = "Modeled Min Wind Noise"), color = "black", linewidth = 1) +
 
  geom_polygon(data = polygon_data,
               aes(x = x, y = y, group = id,  
                   text = paste0("Min Freq: ", round(FQstart, 2) , " Hz<br>Max Freq: ", round(FQend, 2), " Hz")), # 
               fill = "gray",
               alpha = 0.2,
               inherit.aes = FALSE) +
  
  # geom_line(data = polygon_data,
  #           aes(x = x, 
  #               y = label_height,    # <-- Hard-coded to your precise label text elevation
  #               group = id,
  #               text = paste0("Start FQ: ", round(FQstart, 2) , " Hz<br>End FQ: ", round(FQend, 2), " Hz")),   # <-- Plotly maps the tooltip directly here!
  #           color = "transparent",   # <-- Makes the tracking line invisible
  #           linewidth = 5,           # <-- Generates a wide invisible target area for the mouse
  #           inherit.aes = FALSE) +
  # 
  scale_x_log10(labels = label_number(),limits = (c(10,fqupper)), guide = "axis_logticks") +  # Log scale for x-axis
  #scale_x_continuous(limits = c(10, fqupper)) +
  
  
  scale_color_manual(name = "Year", values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
  scale_fill_manual(name = "Year", values =  rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
  
  
  #median HMD values- each year
  geom_line(data = mallData[mallData$Quantile == "50%",],
            aes(x = Frequency, y = SoundLevel, color = Year, fill = Year,
                text = paste0("Year: ", Year), ),
            linewidth = 2) +

  #median HMD values- all data
  geom_line(data = mALL[mALL$Quantile == "50%",],
            aes(x = Frequency, y = SoundLevel),
            color = "black", linewidth = 1,
            linetype = "dotted") +
  
  #for the oldest year, make the shading darker since it is hard to see at alpha = .1 for lightblue
  geom_ribbon(data = ribbonData %>% 
                filter(Year == oldest_year & segment == 0)%>%
                pivot_wider(names_from = Quantile, values_from = SoundLevel),
              #%>%
                #filter(!is.na(`25%`) & !is.na(`75%`)),
              aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year, color = Year), 
              alpha = 0.3, 
              show.legend = FALSE) + # High alpha for visibility
 
  #for the geom_ribbons below, if data only has one year (ch01 and fk08), comment out the first geom ribbon and change alpha of second from .3 to .1
  geom_ribbon(data = ribbonData %>%
                filter(Year != oldest_year & segment == 0) %>%
                pivot_wider(names_from = Quantile, values_from = SoundLevel),
              # %>%
                #filter(!is.na(`25%`) & !is.na(`75%`)),
                aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year, color = Year),
                alpha = 0.1, 
              show.legend = FALSE) 
  
  #only sites with a data gap need the following ribbons
  if (2 %in% ribbonData$segment){
 
  #for the oldest year, make the shading darker since it is hard to see at alpha = .1 for lightblue
  pl <- pl + geom_ribbon(data = ribbonData %>% 
                filter(Year == oldest_year & segment == 2)%>%
                pivot_wider(names_from = Quantile, values_from = SoundLevel),
              #%>%
              #filter(!is.na(`25%`) & !is.na(`75%`)),
              aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year, color = Year), 
              alpha = 0.3, 
              show.legend = FALSE) + 
  
  #for the geom_ribbons below, if data only has one year (ch01 and fk08), comment out the first geom ribbon and change alpha of second from .3 to .1
  geom_ribbon(data = ribbonData %>%
                filter(Year != oldest_year & segment == 2) %>%
                pivot_wider(names_from = Quantile, values_from = SoundLevel),
              # %>%
              #filter(!is.na(`25%`) & !is.na(`75%`)),
              aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year, color = Year),
              alpha = 0.1, 
              show.legend = FALSE) 
  
  }
  
  pl <- pl +
    
  scale_y_continuous(limits = c(27, NA),          
                     breaks = seq(30, 80, by = 10)) + 
  
  # Additional aesthetics
  theme_minimal() +
  labs(
    #title = paste0(toupper(site), "(",siteInfo$`Oceanographic category`, ")"), 
    caption  = caption_text,
    color = legend_label,        #IF biological then change to Year*
    fill = legend_label,        #IF biological then change to Year*
    x = "Frequency (Hz)",
    y = "Sound Levels (dB re 1 &#956; Pa<sup>2</sup>/Hz)",
    subtitle = subtitle_text) +
  theme(legend.position = "right",
        plot.caption = ggtext::element_markdown(hjust = 0, size = 12),
        axis.title.x = element_text(size = 14),           # X-axis label size
        axis.title.y = element_text(size = 14),           # Y-axis label size
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12),
        axis.ticks.length.x = unit(0.25, "cm"), 
        axis.ticks.x = element_line(color = "grey", linewidth = 0.3), 
        axis.line.x = element_line(color = "grey", linewidth = 0.3)    
  ) 

pl


# ggplotly(pl)
# pl_interactive <- ggplotly(pl)

#making logarithmic tick marks
# my_custom_x_positions <- c(
#   10, 20, 30, 40, 50, 60, 70, 80, 90, 
#   100, 200, 300, 400, 500, 600, 700, 800, 900, 
#   1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 
#   10000, 20000, 30000
# )
# 
# log_tick_positions <- log10(my_custom_x_positions)
# 
# # 2. Match every single position with a text label (32 items total)
# my_custom_x_labels <- c(
#   "10", "", "", "", "", "", "", "", "", 
#   "100", "", "", "", "", "", "", "", "", 
#   "1000", "", "", "", "", "", "", "", "", 
#   "10000", "", ""
# )


foi_labels <- lapply(1:nrow(FOIsRange), function(i) {
  list(
    # Find the horizontal midpoint in log-space for the text to sit perfectly center
    x = (log10(FOIsRange$FQstart[i]) + log10(FOIsRange$FQend[i])) / 2,
    
    y = label_height, 
    
    xref = "x",
    yref = "y",
    
    text = FOIsRange$Label[i], # Pulls your text label string dynamically
    
    textangle = -90,           # -90 reads cleanly from bottom-to-top (or use 90)
    
    showarrow = FALSE,
    xanchor = "center",        # Centers the text horizontal anchor point
    yanchor = "middle",        # Centers the text vertical anchor point
    
    font = list(
      size = 13, 
      color = "black",         # Match your layout aesthetic
      family = "sans-serif"
    )
  )
})


pl_interactive <- ggplotly(pl, tooltip = "text", height = 800, width = 800) %>%  
  
  #style(hoverinfo = "none", traces = c(1, 2, 3))  %>%
  
  layout(

    # xaxis = list(
    #   type      = "linear",
    #   autorange = FALSE,
    #   range     = c(log10(10), log10(fqupper)),  # Absolute boundary constraints
    #   tickvals  = log_tick_positions,  
    #   ticktext  = my_custom_x_labels,     
    #   ticks     = "outside",              
    #   ticklen   = 6,                      
    #   tickcolor = "grey",
    #   showgrid  = FALSE                 
    # ),
    
    autosize = TRUE,
    
    yaxis = list(
      autorange = FALSE,       # Prevents Plotly from adding its own padding
      range     = c(27, 85),   # Hard-locks the frame exactly to your polygon edges
      ticks     = "outside",
      tickvals = c(30, 40, 50, 60, 70, 80)
    ),
    
    legend = list(
      orientation    = "v",        # Keeps items stacked as a vertical column
      x              = 1.02,       # Leaves it just past the right axis line
      y              = 0.5,        # <--- Position coordinate set precisely at 50% height
      # xanchor        = "left",     
      yanchor        = "middle"   # <--- Locks the center of the legend block to that 50% mark
      # entrywidth     = 100,       
      # entrywidthmode = "pixels"   
    ),

    # shapes = foi_shapes,
    annotations = foi_labels,
    
    # Increase the bottom margin (b) to ensure there is room for the caption text
    margin = list(b = 50, l = 50, r = 50, t = 50)
    
    #graph caption
    # annotations = list(
    #   x = 0, 
    #   y = -0.15,            # Push it down into the new margin space
    #   text = caption_text, 
    #   showarrow = FALSE, 
    #   xref = 'paper', 
    #   yref = 'paper', 
    #   xanchor = 'left', 
    #   yanchor = 'top',
    #   align = 'left',      # Multi-line alignment if caption wraps
    #   font = list(size = 12, color = "black")
    #)
  )

# Display the interactive plot
pl_interactive




#c("colour", "x", "y")



#effort graph interactive


# 1. Rebuild the plot with the HTML title trick and custom hover text
p1 = ggplot(summary, aes(x = month, y = dy, fill = as.factor(year),
                         # Define your custom hover layout here:
                         text = paste0("Year: ", year, "<br>Days: ", dy))) +
  geom_col(position = "dodge", width = .4) + 
  labs(
    # Combine title and subtitle using HTML line breaks (<br>)
    title = paste0("<b>", effort_title, "</b><br>",
                   "<span style='font-size: 13px; font-weight: normal; color: black;'>",
                   toupper(site), " has ", udaysAG, " unique days: ", 
                   as.character(stAG), " to ", as.character(edAG), "</span>"),
    x = "",
    y = "Days",
    fill = legend_label,
    #caption = "Data from months with effort below the red horizontal line are excluded from annual sound levels figure above"
  ) +
  scale_x_discrete(labels = month.abb[month_nums]) + 
  scale_fill_manual(values = rev(colorRampPalette(c("darkblue", "lightblue"))(length(unique(summary$year))))) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14, hjust = 1, angle = 30),  
    legend.text = element_text(size = 12),
    legend.position = "right" 
  ) +
  geom_line(data = data.frame(
    x = seq_along(month_nums), 
    y = rep(siteInfo$MThreshold, length(month_nums))),
  aes(x = x, y = y, 
      text = paste0("Min # of Days Threshold = ", siteInfo$MThreshold, " Days")), 
  color = "red", 
  linetype = "dashed", 
  linewidth = 0.5,
  inherit.aes = FALSE)


p1

# change height based on how many years are in this sites dataset
p1_interactive <- ggplotly(p1, tooltip = c("text", "group"), height = 260, width = 800) %>% 
  layout(
    autosize = TRUE,
    
    margin = list(t = 50,          # Tucks the title tightly above the bars
                  b = 40,          # Leaves just enough room for the angled month text (Jan, Feb...)
                  l = 50,          # Aligns perfectly with the top plot's left axis
                  r = 50), # Ensure room for your caption at the bottom
    
    legend = list(
      font = list(size = 13) # to make legend slightly shorter, less spacing between years didnt work
    ),
    
    annotations = list(
      x = 0, y = -0.3, 
      text = "Data from months with effort below the red horizontal line are excluded from annual sound levels figure above", 
      showarrow = FALSE, 
      xref = 'paper', yref = 'paper', 
      xanchor = 'left', yanchor = 'top',
      font = list(size = 13)
    )
  )

# View the final result
p1_interactive


#ggplotly(p1)




#combine effort and line


combined_layout <- browsable(
  div(
    style = "display: flex; flex-direction: column; gap: 10px; font-family: sans-serif; padding: 10px;",
    
    # Top Plot (Spectrum) - Grand and tall
    div(style = "height: 800px; width: 800px;", pl_interactive),
    
    # Top Chart Caption
    p(HTML(caption_text), style = "font-size: 13px; color: black; margin: 0; padding-left: 5px; line-height: 1.4;"),
    
    # Elegant Divider Line
    tags$hr(style = "width: 800px; border: none; border-top: 1.5px solid black; margin: 5px 0;"),
    
    # Bottom Plot (Monthly Effort Bars) - Clean, short, and compact!
    div(style = "height: 275px; width: 800px;", p1_interactive) 
  )
)

# View the perfectly balanced application layout
combined_layout


#save html file to Dev contents folder
#outDir = "X:/Emma_Beretta/SoundscapesWebsiteDev/" #for GCP workstation remote desktop Emma
outDir   =  "C:/Users/embe5980/SoundscapesWebsiteDev/" #local


outDirG  =  paste0(outDir,"content/resources/") #where save graphics
outDirGe =  paste0(outDir,"content/resources/extra") #where extra save graphics
outDirC  =  paste0(outDir,"context/") #where to get context


# Save the entire HTML layout bundle natively
htmltools::save_html(combined_layout, paste0(outDirG, "/plot_", toupper(site), "_interactiveAnnualSPL.html"))


