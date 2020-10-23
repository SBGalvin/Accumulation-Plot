#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Set of nested categories
# Factor 1 is used for faceting the ggplot
# Factor 2 is used to for the x-axis

# (1) Read in your data and mimic the structure in 1 B Fake Data
# (1 B) Create appropriate data frame structure
#   (1 B.1) Asssuming there is Paired Data (this doesn't have to be limited to pairs, could be 3 , 4... conditions)

#   (1 B.2) Example Data Columns
#           Measurement data at timepoint data goes into Time 1, Time 2 etc.
#           |   1   |   2   |           3           |   4     |   5     |
#           | RowID | Group | Place of Articulation | Time 1  | Time 2  |
#

#           RowID is used to plot the lines between points representing response value in Time 1 (Slow) or Time 2( Fast)
#           Group is used to colour the connected points and their lines.
#           Place of Articulation is used to facet the plot into three columns
#           Time 1 and time 2 are later gathered into a stacked portion of the data frame
#           They become Time

# (2) Create Summary Dataframe (this forms the base of the plot and mean/error bar component)

# (3) start the Plot
#   (3.1) Add the Mean and Error Bars
#   (3.2) Add the connection lines
#   (3.3) Add the points (colouring by item and participant)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#





# 00) Set options Load packages -------------------------------------------

options(
  scipen = 999,
  stringsAsFactors = FALSE
)

# Packages
library(tidyverse) # data analysis plotting, everything really.



#  01) Generate fake data -------------------------------------------------
# 3 conditions (Coronal, Labial, Velar), each with 2 levels (Fast, Slow)
# 20 participants 
# 20 items
fake_data <- data.frame(
  
  # You need an ID number (row number for each pair to plot a link/line)
  rowID = c(1:20, 1:20, 1:20, LETTERS[1:20], LETTERS[1:20], LETTERS[1:20]),
  
  group = c(rep("Participants", 60), rep("Items", 60)),
  
  
  Plc_Artic = c(
    # Participants
    rep("Labial", 20), rep("Coronal", 20), rep("Velar", 20), 
    # Items
    rep("Labial", 20), rep("Coronal", 20), rep("Velar", 20)
  ),
  
  Slow = c(
    # Participants
    rnorm(n = 20, mean = -0.1, sd = .15),
    rnorm(n = 20, mean = -0.18, sd = .15),
    rnorm(n = 20, mean = 0.18, sd = .15),
    # Items
    rnorm(n = 20, mean = 0.1, sd = .15),
    rnorm(n = 20, mean = -0.17, sd = .15),
    rnorm(n = 20, mean = 0.19, sd = .15)
    
  ),
  
  Fast = c(
    # Participants
    rnorm(n = 20, mean = -0.15, sd = .15),
    rnorm(n = 20, mean = 0.1, sd = .15),
    rnorm(n = 20, mean = 0.15, sd = .15),
    # Items
    rnorm(n = 20, mean = -0.15, sd = .15),
    rnorm(n = 20, mean = 0.12, sd = .15),
    rnorm(n = 20, mean = 0.11, sd = .15)
  )
) 

# take a look at the fake data dataframe
fake_data


# convert to longer format dataframe for ggplot2 (repeated measures)
fake_data_long <- fake_data %>% 
  # ggplot2 works better with one value column 
  # if the data points need to be conditionally coloured
  gather(Time, VOT, Slow:Fast)




# 02) Summary table for boxplots ------------------------------------------

fake_data_summary <- fake_data_long %>% 
  
  # This will help you to filter out the items from the data summary 
  # If you just wanted to focus on participants
  #filter(!rowID %in%LETTERS)
  
  
  group_by(Plc_Artic, Time) %>% 
  summarise(N = n(),
            Mean = mean(VOT),
            StDev = sd(VOT),
            StdError = (StDev/sqrt(N))) %>% 
  ungroup()



# 03) Plot ----------------------------------------------------------------


connection_plot <- 
  # Use the summary data  frame to start the plot
  fake_data_summary %>% 
  
  # Set the X Axis as the sub-level of the conditions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ggplot(aes(x = Time))+ 
  
  # Boxplot/ Mean and error bars ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  geom_errorbar(aes(ymin = Mean - (StdError * 1.96), 
                    ymax = Mean + (StdError * 1.96)),
                size = 1.5, 
                width = .5)+
  
  geom_point(aes(y = Mean), shape = 15,
             size  = 4) +
  
  ## Connected observations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Line geometry
  geom_line(data = fake_data_long,                                    # declare data source
            aes(group = rowID, y = VOT, colour = group),              # declare aesthetics for mapping
            position = position_jitter(width = 0.02, height = 0))+    # extra options
  
  
  # Jittered point geometry
  geom_jitter(data = fake_data_long,                                  # declare data source
              aes(group = rowID, y = VOT, colour = group),            # declare aesthetics for mapping
              width = 0.02, height = 0, shape = 1)+                   # extra options
  
  
  
  # Set colouring by group, Item or Participant ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  scale_colour_manual(values = c("steelblue", "tomato"), name = "")+
  
  # Titles ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ggtitle("Selected connected observations")+
  xlab("Place of Articulation")+
  ylab("VOT (ms)")+
  
  # Scales ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set y-axis limits
  scale_y_continuous(limits = c(
    min(fake_data_long$VOT) - .11, 
    max(fake_data_long$VOT) + .11
  ))+
  
  # Facet the Plot so that The Main Conditions can be displayed
  facet_wrap(~Plc_Artic, strip.position = "bottom")+
  
  
  # Theme Settings ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # theme settings
  theme_minimal()+
  
  theme(
    
    legend.position = "top",
    legend.direction = "horizontal"
  )
