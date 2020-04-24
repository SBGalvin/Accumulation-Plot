#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# This plot visualises accumulated accuracy ad accumulated time
#
# Step 1: identify an order
# Step 2: 
        # perform a cumsum of RT 
        # perform a cumsum of response/accuracy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# 00) Set options Load packages -------------------------------------------

options(
  scipen = 999,
  stringsAsFactors = FALSE
)

# Packages
library(tidyverse)
library(viridis)
#install.packages("prepdat")
library(prepdat) # for stroop data



# 01) Load data -----------------------------------------------------------
data("stroopdata")



# 02) Inspect data --------------------------------------------------------


# 15 unique id values
length(unique(stroopdata$subject))
unique(speed_acc$block)


# How many rows per id?
id_df <- data.frame()
idx <- NULL
number_r <- NULL

for (i in seq(1:15)) {
  
  idx <- i
  
  number_r <- (nrow(stroopdata[stroopdata$subject == i,]))
  
  id_df <- rbind(id_df, data.frame(id = idx, nrows = number_r))
  
}

id_df 
  



# 03) Prepare data --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3.1
# Create a 0 origin point
# Multiple trials (this is not necessary for this data, I think).
# if you do have item response data, create an origin point like so:

stroopdata %>% 
  select(subject) %>% 
  unique() %>% 
  mutate(trial_num = "000",
         ac = 0,
         rt = 0) -> origin_point
# then use full_join to merge this into the main data before any
# transformations/ cumsum etc.
# if there are multiple blocks then you will have to create a 00 row
# for each block.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3.2
# Cumsum data

stroopdata %>% 
  filter(target_type == 1) %>% 
  filter(block ==1) %>% 
  select(subject, trial_num, rt, ac) %>%
  
  mutate(subject = paste0("id_", subject)) %>% 
  
  mutate(trial_num = sprintf("%03d", trial_num)) %>% 
  
  group_by(subject) %>% 
  
  arrange(trial_num) %>% 
  
  mutate(rt_cs = cumsum(rt),
         ac_cs = cumsum(ac)) %>% 
  ungroup()-> cumsum_data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3.3
# Maximum values data (this is used for geom_point locations)
cumsum_data %>% 
  group_by(subject) %>% 
  filter(ac_cs == max(ac_cs)) %>% 
  ungroup() %>% 
  unique()

cumsum_data %>% 
  group_by(subject) %>% 
  filter(rt_cs == max(rt_cs)) %>% 
  ungroup() %>% 
  unique() #%>% 
  select(subject, )

full_join(
  
  cumsum_data %>% 
    group_by(subject) %>% 
    filter(ac_cs == max(ac_cs)) %>% 
    ungroup() %>% 
    unique(),
  
  cumsum_data %>% 
    group_by(subject) %>% 
    mutate(avg_ac = mean(ac)) %>% 
    ungroup() %>% 
    select(subject, avg_ac)
  
  ) %>% 
  unique() %>% 
  select(subject,rt, ac, rt_cs, ac_cs, avg_ac)-> cumsum_max_df
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3.4
# Add a score average to the data set
# if this were psychometric data I would collate a total
# score/ Sum Score variable instead of an average

full_join(
  cumsum_data,
  
  cumsum_data %>% 
    group_by(subject) %>% 
    mutate(avg_ac = mean(ac)) %>% 
    ungroup() %>% 
    select(subject, avg_ac)
) -> cumsum_data_2


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# 04) plot ----------------------------------------------------------------

cumsum_data_2 %>% 
  
  
  ggplot(aes(x = rt_cs, group = subject))+
  
  # background cumsum line is slightly larger than the coloured lines
  # this is to give a background effect for the lines
  geom_line(aes(y = ac_cs), 
            colour = "grey10", 
            
            # make this size smaller if there are lots of participants/ lines
            size = 2.0, 
            alpha = .6)+
  
  # Coloured line layer
  geom_line(aes(colour = avg_ac, y = ac_cs), 
            size  = 1)+
  
  # point layer for Maxmimum RT and Max accumulated score
  geom_point(data = cumsum_max_df,
             aes(x = rt_cs, y = ac_cs, colour = avg_ac), 
             # if you have a wider scale range you can add size to the point aes
             size = 6,
             alpha = .6)+
  
  
  scale_color_gradient(low = "#249787", high = "#FDE725")+
  
  # when you have a wider scale range 
  # add viridis colour scale because it is pretty
  #scale_color_viridis(direction = -1)+
  
  # if geom_point size is an aesthetic use this to take it out of the legend
  guides(size = FALSE)+
  
  # axis labels
  xlab("")+
  ylab("")+
  
  # theme settings
  # remove to see axis values etc.
  theme_minimal()+
  
  # manual theme settings
  theme(legend.position = 'none',
        # axis settings
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        
        # plot background
        plot.background = element_blank(),
        # panel background
        panel.background = element_blank(),
        panel.grid = element_blank()) -> uh_oh_spaghetti_ooo