library(imager)
library(tidyverse)
library(TSP)

# Read and convert to grayscale
load.image("giraffe.jpg") %>% grayscale() -> x

#will something as simple as boundary() work?
#nope!
px.diamond(10,30,30) %>% imager::boundary() %>% plot
x %>% imager::boundary(depth = 0.5) %>% plot
x %>% plot

#let's try canny edge detection
x %>% cannyEdges() %>% plot
#that seems to work well... how to get the points now
x %>% cannyEdges() -> x_edges
str(x_edges)
#seems like a pixel matrix
#logical... outline is true maybe?

#convert logical to numerical
edge_mat <- drop(x_edges)
class(edge_mat) <- "array"
edge_mat <- edge_mat*1

##ok this works, mess around w/ canny edge params
x %>% cannyEdges() %>% plot
#no change
x %>% cannyEdges(alpha = 0.4) %>% plot
#more smooth, seems like more points
x %>% cannyEdges(sigma = 10) %>% plot
#really rough, no apparent change, must be a threshold
x %>% cannyEdges(sigma = 0.001) %>% plot

#get points which are edges as dataframe
df <-
  x %>% 
  cannyEdges() %>% 
  as.cimg() %>% 
  as.data.frame() %>%
  filter(value == 1) %>%
  mutate(id = 1:nrow(.))

#get rid of some junk for later calc
data <-
  df %>%
  select(x, y)

#we can see these points define the border really well!
ggplot(data) +
  geom_point(aes(x = x, y = y), color = "black") +
  scale_y_reverse()

#but how do we order the points so that we can pass an ordered set to RTriangle?
#we basically need to solve the travelling salesman problem
#find the shortest path between points w/o visiting any point twice
#this should define the border

###solve TSP###
#we actually find the shortest Hamilton path b/c pure TSP gives a weird line at the end
#when it tries to connect the final point to the original... idk why 
#insert dummy pt to make the problem Hamilton (ELIZAAAAA!)
tsp <- TSP(dist(data)) 
tsp <- insert_dummy(tsp, label = "cut")

solve_TSP(tsp, method = "arbitrary_insertion", control = "two_opt") %>% 
  as.integer() -> solution

# Rearrange the original points according the TSP output
data_to_plot <- data[solution,]

#it works!
ggplot(data_to_plot, aes(x,y)) +
  geom_path() +
  scale_y_reverse() +
  theme_void()

#later on figure out best way to sample points to make more sparse

