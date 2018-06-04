# Exploring Polling Data in R from Data camp
# https://campus.datacamp.com/courses/exploring-polling-data-in-r

library(ggplot2)
library(gridExtra)
library(ggthemes)
library(maps)
# Check out the structure of polls
str(polls)

# Select polls for the Democratic primaries only: dem_polls
dem_polls <- subset(polls, polls$type == "dem")

# Using dem_polls, plot polldate on the x-axis, percent (i.e. percent of voters supporting a candidate) on the y-axis, and set the color using candidate
plot(dem_polls$polldate, dem_polls$percent, col = dem_polls$candidate)


### 2
# Load the ggplot2 package
library(ggplot2)

# Create a plot of Democratic candidate support (percent) over time (polldate), setting color to candidate
dem_plot <- ggplot(data = subset(polls, polls$type == "dem"), aes(y = percent, x = polldate, col = candidate)) + 
  geom_point(alpha = 0.5) 

# View your new plot
dem_plot

# Add a trend line for each candidate using geom_smooth()
dem_plot + 
  geom_smooth(span = 0.5, se = FALSE) # use se = true to have the C.I shown
  

### 3 
# Summarize samplesize (contained in the polls data frame)
summary(polls$samplesize)

# Create a histogram showing the distribution of polls with a sample size below 1000. Use the vector `breaks` for your histogram breaks.
hist(polls$samplesize[polls$samplesize < 1000], breaks)

# Create a new data frame that contains only polls with a sample size greater than 400: polls2
polls2 <-polls[polls$samplesize > 400,]



### 4
# Recreate your plot of the Demcoratic polling data using polls2: dem_plot2
dem_plot2 <- ggplot(data = subset(polls2, polls2$type == "dem"), aes(y = percent, x = polldate, col = candidate)) +
  geom_point(alpha = 0.5, na.rm = TRUE) +
  geom_smooth(span = 0.5, se = FALSE, na.rm = TRUE)

# Create a similar plot for the Republican primaries: rep_plot2
rep_plot2 <- ggplot(data = subset(polls2, polls2$type == "rep"), aes(y = percent, x = polldate, col = candidate)) +
  geom_point(alpha = 0.5, na.rm = TRUE) +
  geom_smooth(span = 0.5, se = FALSE, na.rm = TRUE)

# View both plots together (do not modify this command)
grid.arrange(dem_plot2, rep_plot2, nrow = 2)


### 5 
# Create a plot that includes only polling data for Republicans in Iowa: ia_plot
ia_plot <- ggplot(data = subset(polls2, polls2$type == "rep" & location == "IA"), aes(y = percent, x = polldate, col = candidate)) +
  geom_point(alpha = 0.5, na.rm = TRUE) +
  geom_smooth(span = 0.7, na.rm = TRUE) +
  labs(title="Iowa")

# Create another plot that includes only polling data for Republicans in New Hampshire: nh_plot
nh_plot <- ggplot(data = subset(polls2, polls2$type == "rep" & location == "NH"), aes(y = percent, x = polldate, col = candidate)) +
  geom_point(alpha = 0.5, na.rm = TRUE) +
  geom_smooth(span = 0.7, na.rm = TRUE) +
  labs(title="New Hampshire")

# Create another plot that includes only polling data for Republicans in South Carolina: sc_plot
sc_plot <- ggplot(data = subset(polls2, polls2$type == "rep" & location == "SC"), aes(y = percent, x = polldate, col = candidate)) +
  geom_point(alpha = 0.5, na.rm = TRUE) +
  geom_smooth(span = 0.7, na.rm = TRUE) +
  labs(title="South Carolina")

# Take a look at all three plots together (do not modify this command)
grid.arrange(ia_plot, nh_plot, sc_plot, nrow=3)


### 6
# Load the maps package
library(maps)
states <- map_data("state")

# Merge states and sanders by region: sanders_map
sanders_map <- merge(states, sanders, by = "region", all = TRUE)

# Reorder Sanders map according to the maps package order column
sanders_map <- sanders_map[order(sanders_map$order), ]

# Use ggplot() to produce a map of Bernie Sanders polling data (do not modify this command)
ggplot() +
  geom_polygon(data = sanders_map, aes(x = long, y = lat, group = group, fill = percent)) +
  labs(title = "Support for Bernie Sanders at Last Poll Before Primary") + 
  theme_map()


### 7
# Load the googleVis package
library(googleVis)

# Generate a googleVis map object: sanders_gvis
sanders_gvis <- gvisGeoChart(data = sanders, locationvar = "location", colorvar = "percent",
                          options=list(region="US", 
                                       displayMode="regions", 
                                       resolution="provinces"))

# Plot your googleVis object
plot(sanders_gvis)





