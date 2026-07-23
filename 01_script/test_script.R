###################################################################################
## _______  _____  _____  ______    _______  _______    __      _______  _______ ##
##|       ||     ||  _  ||    _ |  |       ||       |  |  |    |   _   ||  _    |##
##|  _____|| _   || |_| ||   | ||  |    ___||  _____|  |  |    |  |_|  || |_|   |##
##| |_____ || |  ||     ||   |_||_ |   |___ | |_____   |  |    |       ||       |##
##|_____  |||_|  ||     ||    __  ||    ___||_____  |  |  |___ |       ||  _   | ##
## _____| ||     ||  _  ||   |  | ||   |___  _____| |  |      ||   _   || |_|   |##
##|_______||_____||_| |_||___|  |_||_______||_______|  |______||__| |__||_______|##
##|                                                                              ##
###################################################################################
## This script is written by Rodrigo Martin de Oliveira
## Working on my Master thesis, and learning skill necessary to do so.
## 
#### LIBRARIES ####
install.packages(ggplot)

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ggplot)


renv::init()

renv::snapshot()

## Load data, using a random dataset to test the script
datasets::iris #info about 3 species of iris flowers

iris_data <- datasets::iris

ggplot(data = iris, mapping = aes(x = Petal.Width, y = Petal.Length, colour = class))+
  geom_point()+
  geom_smooth(formula = y-x, method = "lm")

diamonds

ggplot(data = diamonds, mapping = aes(x = colour, fill = cut))+
  geom_bar(position = "dodge")
  


ggplot (data = iris_data, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(title = "Sepal Length vs Sepal Width by Species",
       x = "Sepal Length (cm)",
       y = "Sepal Width (cm)") +
  theme_minimal()

ggplot(data = iris_data, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  facet_grid(Petal.Width~))


ggplot()+
  geom_point(data = vrun, colour = 'grey70')


ggplot(map_data("state"), aes(long, lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "white")


rod_plot <- ggplot(data = iris_data, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  facet_wrap(~Species) +
  labs(title = "Sepal Length vs Sepal Width by Species",
       x = "Sepal Length (cm)",
       y = "Sepal Width (cm)")+
  annotate("text", x = 5, y = 4, label = "Rodrigo", size = 4, color = "black")

  ggsave(filename = "03_images/rod_plot.svg", width = 8, height = 6, dpi = 300, device = "svg")




ggplot



#### romulo  exercise biol835AQ #####
# change something in the plot, to show that you can manupulate the datasetts
# I choose the Iris datase, because I like flowers
datasets::iris
iris


# my goal is to create a scatter plot of Sepal.Length vs Sepal.Width, colored by Species, and add a linear regression line for each species.
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species,)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  
  labs(title = "setal Length vs Sepal Width by Species",
       x = "setal Length",
       y = "setal Width") +
  annotate ("text", x = 5, y = 4, label = "Rodrigo", size = 4, color = "pink") +
  annotate ("pointrange" , x = 7, y = 2.5, ymin = 2.5, ymax = 3, color = "green") +
  insert_element(p = img, left = 0.5, bottom = 0.5, right = 1, top = 1)



lty <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
linetypes <- data.frame(
  y = seq_along(lty),
  lty = lty
)
ggplot(linetypes, aes(0, y)) +
  geom_segment(aes(xend = 5, yend = y, linetype = lty)) +
  scale_linetype_identity() +
  geom_text(aes(label = lty), hjust = 0, nudge_y = 0.2) +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_reverse(NULL, breaks = NULL)







