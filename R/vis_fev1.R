#       _/_/    _/  _/      _/_/      _/   
#    _/    _/  _/  _/    _/    _/  _/_/    
#       _/    _/_/_/_/    _/_/_/    _/     
#    _/          _/          _/    _/      
# _/_/_/_/      _/    _/_/_/    _/_/_/     

# Exploratory Data Analysis of FEV1 data

library(tidyverse)

# read the data in
fev1 <- read_csv("data/fev1.csv", col_types = list('id' = 'f'))

# sample the data so that we have 20 patients with more than 6 observations

set.seed(10)

fev1_sampled <- fev1 %>% 
    count(id) %>%
    filter(n > 6) %>%
    slice_sample(n = 20) %>%
    select(id) %>%
    inner_join(fev1)

fev1_sampled

# Activity 5 - A simple scatter plot

scatterPlot_age_FEV1 <- fev1_sampled %>% ggplot(mapping=aes(x=age, y=FEV1))+
    geom_point(color='blue')

scatterPlot_age_FEV1

# Calculate the correlation between age and FEV1
# (yes, this isn't strictly correct because there's repeated measures)

fev1 %>% summarise(cor(age,FEV1))

# Build a plot that shows the relationship between FEV1 and age
fev1_plot <- ggplot(data = fev1_sampled, 
                    aes(x = age, y = FEV1)) +
  geom_point()

fev1_plot

# Activity 6 - Improving the plot

# Add meaningful labels for the $x$ and $y$ axes, including units, and change the plot's colour theme from the default.

# Add a smooth line of best fit to the plot. 

theme_set(theme_classic())

fev1_plot +
    geom_point(aes(alpha = 0.3)) +
    geom_smooth(method = 'loess') +
    ggtitle("Correlation between age and FEV1") +
    xlab("Age (years)") +
    ylab("FEV1 (liters)") +
    scale_x_continuous(limits = c(5, 20), breaks = seq(5, 20, 5)) +
    scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1)) +
    theme(
        legend.position = "bottom",
        plot.title = element_text(
            hjust = 0.5,
            size = 18,
            margin = margin(
                t = 5,
                r = 0,
                b = 30,
                l = 0
            )
        )
    )

# Activity 7

# Activity 7a - Showing further structure

# Determine a way to highlight which observations belong to the same individual in your plot
fev_grouped_plot <- ggplot(data = fev1_sampled, 
                      aes(x = age, y = FEV1)) +
  geom_point(aes(color=id)) +
  geom_smooth(method = 'loess') +
  ggtitle("Correlation between age and FEV1") +
  xlab("Age (years)") +
  ylab("FEV1 (liters)") +
  scale_x_continuous(limits = c(5, 20), breaks = seq(5, 20, 5)) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1)) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(
      hjust = 0.5,
      size = 18,
      margin = margin(
        t = 5,
        r = 0,
        b = 30,
        l = 0
      )
    )
  ) + scale_color_manual(values =c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))

fev_grouped_plot
# Activity 7b - How many observations per individual?

# Count the number of times that each `id` is measured and make a bar plot 
count_by_id <- fev1_sampled %>% count(id)

count_plot <- ggplot() + geom_col(data=count_by_id,aes(x=id,y=n),fill='dark blue') +
  xlab('ID') +
  ylab('No. of Observations')
count_plot
# Activity 7c - Incorporating height

# Make a plot that shows both FEV1 and age but also includes height
library(plotly)
?plot_ly
plotly::plot_ly(data=fev1_sampled,x=~age, y=~FEV1, z=~height, type="scatter3d", mode="markers",color=~FEV1)

fev_al_plot <- ggplot(data=fev1_sampled,aes(x=age,y=FEV1,color=height)) + geom_point()
fev_al_plot

# Activity 7d - skimr

# Use skimr::skim() to generate a summary table of the data.
# You'll need to install skimr if you don't already have it
library(skimr)

skimr::skim(fev1_sampled)

# Activity 7e - GGally

# Generate a pairs plot with GGally::ggpairs(), for all columns except id
# You'll need to install GGally if you don't already have it
library(GGally)

GGally::ggpairs(fev1_sampled,columns = 2:4)

# Activity 7f - Accounting for repeat measurement

# Build a regression model to look at how FEV1 varies with age, accounting for the
# structure by including a random effect mean for each id and a spline curve for
# the effect of age

#look at trajectories for each individual
ggplot(data=fev1,aes(x=age,y=FEV1, fill=id)) + geom_line(alpha=0.2)
