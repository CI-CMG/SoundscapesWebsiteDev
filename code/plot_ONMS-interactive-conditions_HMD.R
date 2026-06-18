#use this script after running plot_ONMS-conditions_HMD.R

library(htmlwidgets)
library(htmltools)
library(plotly)


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


if (site == 'fk08'){
  segment1 = 0
  segment2 = 2
} else {
  segment1 = 1
  segment2 = 3
}



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
            aes(x = Frequency, y = SoundLevel, color = Year, fill = Year, group = Year,
                text = paste0("Year: ", Year, "<br>Freq: ", Frequency, "<br>Sound Level: ", round(SoundLevel,2)) ),
            linewidth = 2) +

  #median HMD values- all data
  geom_line(data = mALL[mALL$Quantile == "50%",],
            aes(x = Frequency, y = SoundLevel, group = Quantile,
                text = paste0("Median across all years<br>Freq: ", Frequency, "<br>Sound Level: ", round(SoundLevel,2) ) ),
            color = "black", linewidth = 1,
            linetype = "dotted") +
  
  #for the oldest year, make the shading darker since it is hard to see at alpha = .1 for lightblue
  geom_ribbon(data = ribbonData %>% 
                filter(Year == oldest_year & segment == segment1)%>%
                pivot_wider(names_from = Quantile, values_from = SoundLevel),
              #%>%
                #filter(!is.na(`25%`) & !is.na(`75%`)),
              aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year, color = Year), 
              alpha = 0.3, 
              show.legend = FALSE) + # High alpha for visibility
 
  #for the geom_ribbons below, if data only has one year (ch01 and fk08), comment out the first geom ribbon and change alpha of second from .3 to .1
  geom_ribbon(data = ribbonData %>%
                filter(Year != oldest_year & segment == segment1) %>%
                pivot_wider(names_from = Quantile, values_from = SoundLevel),
              # %>%
                #filter(!is.na(`25%`) & !is.na(`75%`)),
                aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year, color = Year),
                alpha = 0.1, 
              show.legend = FALSE) 
  
  #only sites with a data gap need the following ribbons
  if (segment2 %in% ribbonData$segment){
 
  #for the oldest year, make the shading darker since it is hard to see at alpha = .1 for lightblue
  pl <- pl + geom_ribbon(data = ribbonData %>% 
                filter(Year == oldest_year & segment == segment2)%>%
                pivot_wider(names_from = Quantile, values_from = SoundLevel),
              #%>%
              #filter(!is.na(`25%`) & !is.na(`75%`)),
              aes(x = Frequency, ymin = `25%`, ymax = `75%`, fill = Year, color = Year), 
              alpha = 0.3, 
              show.legend = FALSE) + 
  
  #for the geom_ribbons below, if data only has one year (ch01 and fk08), comment out the first geom ribbon and change alpha of second from .3 to .1
  geom_ribbon(data = ribbonData %>%
                filter(Year != oldest_year & segment == segment2) %>%
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
      # font.weight = 
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

summary <- gpsAG %>%
  mutate(
    year  = year(UTC),  # Extract Year
    month = format(UTC, "%m")  # Extract Month (numeric format)
  ) %>%
  count(year, month)  # Count occurrences (hours) in each year-month
summary$dy = round(summary$n/ 24)



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


if (length(years_to_keep) > 3){

    height_int = 280
  
} else if (length(years_to_keep) <= 3){
  
    height_int = 260
  
}

# change height based on how many years are in this sites dataset
p1_interactive <- ggplotly(p1, tooltip = c("text", "group"), height = height_int, width = 800) %>% 
  layout(
    autosize = TRUE,
    
    margin = list(t = 50,          # Tucks the title tightly above the bars
                  b = 40,          # Leaves just enough room for the angled month text (Jan, Feb...)
                  l = 50,          # Aligns perfectly with the top plot's left axis
                  r = 50), # Ensure room for your caption at the bottom
    
    # legend = list(
    #   font = list(size = 13) # to make legend slightly shorter, less spacing between years didnt work
    #   #, groupclick = "toggleitem"
    #   ),
    
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


