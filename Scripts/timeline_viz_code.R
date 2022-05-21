### timeline viz

##Add source file reading in data
source('Scripts/read_data.R')

library(scales)
library(lubridate)
library(tidyverse)
library(knitr)
library(timevis)
library(readxl)
library(dplyr)
library(ggplot2)

## Create new column in data for event type:
# IBS diagnosis, ED visit, (hospitalization), colonoscopy, Esophagogastroduodenoscopy, surgery (Enterectomy, Closure Of Enterostomy), imaging (ultrasound, tomography)
# can also add labs, or any other things


#Initialize new column, event_type that labels events based on procedure description
# can also add labs, or any other things


events_timeline_procedure <- ibs_dat %>% 
  # IBS-- need to include this separately because of possibility of overwriting
  #mutate(event_type = ifelse(Primary_diag == "Irritable Bowel Syndrome", "IBS_diagnosis", NA)) %>%
  
  ### Create event_type as column of NAs
  mutate(event_type = NA) %>%
  
  mutate(event_type = case_when(
    grepl("emergency department", PROCEDURE_DESC, ignore.case = TRUE) ~ "ED_visit",
    grepl("colonoscopy", PROCEDURE_DESC, ignore.case = TRUE) ~ "colonoscopy",
    grepl("Esophagogastroduodenoscopy", 
          PROCEDURE_DESC, ignore.case = TRUE) ~ "esophagogastroduodenoscopy",
    (grepl("Enterectomy", PROCEDURE_DESC, ignore.case = TRUE) | 
       grepl("Enterostomy", PROCEDURE_DESC, ignore.case = TRUE)) ~ "surgery",
    (grepl("ultrasound", PROCEDURE_DESC, ignore.case = TRUE) | 
       grepl("tomography", PROCEDURE_DESC, ignore.case = TRUE) ~ "imaging")))


# Add a specified order to these event type labeles
event_type_levels <- c("ED_visit", "colonoscopy", "esophagogastroduodenoscopy", "imaging", "surgery") 

# Define the colors for the event types in the specified order. 
## These hashtagged codes represent the colors as hexadecimal color codes.
event_type_colors <- c("#FF9933", "#33FF99",  "#FF99CC", "#CC99FF", "#A0A0A0" ) 

# Make the Event_type vector a factor using the levels we defined above
events_timeline_procedure$event_type <- factor(events_timeline_procedure$event_type, levels= event_type_levels, ordered=TRUE)


# Set the heights we will use for our milestones
## need positive and negative set for each event
positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5, 2.0, -2.0, 2.5, -2.5, 3.0, -3.0, 3.5, -3.5)

# Set the directions we will use for our milestone, for example above and below.
directions <- c(1, -1) 


# Assign the positions & directions to each date from those set above
## only for non-NA event types

subs_events_only <- events_timeline_procedure %>% filter(!is.na(event_type))

line_pos <- data.frame(
  "from_dt"=unique(subs_events_only$from_dt),
  "position"=rep(positions, length.out=length(unique(subs_events_only$from_dt))),
  "direction"=rep(directions, length.out=length(unique(subs_events_only$from_dt))))

# Create columns with the specified positions and directions for each milestone event
merged_pos_dir <- merge(x=subs_events_only, y=line_pos, by="from_dt", all = TRUE) 

###########

# Create the timeline axis 
#### we want this to be from the overall dataset and have our events fall in between

# Months range
month_date_range <- seq(min(subs_events_only$from_dt),
                        max(subs_events_only$from_dt), by='month')


## We want the format of the months to be in the 3 letter abbreviations of each month.
month_format <- format(month_date_range, '%b') 
month_df <- data.frame(month_date_range, month_format)


# Year range
year_date_range <- seq(min(subs_events_only$from_dt),
                       max(subs_events_only$from_dt), by='year')

# We will only show the years for which we have a december to january transition.
# year_date_range <- as.Date(
#     intersect(
#         ceiling_date(year_date_range, unit="year"),
#         floor_date(year_date_range, unit="year")),  
#         origin = "1970-01-01") 

# We want the format to be in the four digit format for years.
year_format <- format(year_date_range, '%Y') 
year_df <- data.frame(year_date_range, year_format)

# Create timeline coordinates with an x and y axis
timeline_plot<-ggplot(merged_pos_dir,aes(x=from_dt,y= position, col=event_type, label= event_type)) 

# Add the label Milestones
timeline_plot<-timeline_plot+labs(col="event_type") 

# Print plot
#timeline_plot

# Assigning the colors and order to the milestones
timeline_plot <- timeline_plot+scale_color_manual(values=event_type_colors, labels=event_type_levels, drop = FALSE) 

# Using the classic theme to remove background gray
timeline_plot<-timeline_plot+theme_classic() 

# Plot a horizontal line at y=0 for the timeline
timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                                        color = "black", size=0.3)
# Print plot
#timeline_plot


# Plot the vertical lines for our timeline's milestone events
timeline_plot<-timeline_plot+geom_segment(data=merged_pos_dir, aes(y=merged_pos_dir$position,yend=0,xend=merged_pos_dir$from_dt), color='black', size=0.2) 


# Now let's plot the scatter points at the tips of the vertical lines and date
timeline_plot<-timeline_plot+geom_point(aes(y=merged_pos_dir$position), size=3) 

# Let's remove the axis since this is a horizontal timeline and postion the legend to the bottom
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "bottom"
) 
# Print plot
#timeline_plot

# Let's add the text for each month
timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.15,label=month_format),size=3.5,vjust=0.5, color='black', angle=90) 


# Let's add the years
timeline_plot<-timeline_plot+geom_text(data=year_df, aes(x=year_date_range,y=-0.25,label=year_format, fontface="bold"),size=3.5, color='black') 

# Print plot
#print(timeline_plot)

# We need to add the labels of each milestone now. 
## To do this we have to define the text position. A clean timeline should have the labels situatuated a bit above the scatter points.
### Since we have the positions of the points already defined, we will place the labels 0.2 pts away from the scatter points.


# Lets offset the labels 0.2 away from scatter points
text_offset <- 0.2 

# Let's use the absolute value since we want to add the text_offset and increase space away from the scatter points 
absolute_value<-(abs(merged_pos_dir$position)) 
text_position<- absolute_value + text_offset

# Let's keep the direction above or below for the labels to match the scatter points
merged_pos_dir$text_position<- text_position * merged_pos_dir$direction

# Now we can add the labels to the timeline for our milestones.
timeline_plot<-timeline_plot+geom_text(aes(y=merged_pos_dir$text_position,label=merged_pos_dir$event_type), size=3.5, vjust=0.6)

# Print plot
#print(timeline_plot)

