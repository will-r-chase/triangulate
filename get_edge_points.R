library(imager)
library(tidyverse)
library(TSP)

# Read and convert to grayscale
load.image("inputs/giraffe.jpg") %>% grayscale() -> x

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

#you might have to iterate this a couple times to get a smooth TSP solution without rando lines
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
#random sample
set.seed(123)
points_sparse <- 
  data_to_plot %>%
  tibble::rowid_to_column("id") %>%
  sample_n(100) %>%
  arrange(id)

ggplot(points_sparse) +
  geom_point(aes(x = x, y = y), color = "black") +
  scale_y_reverse()
ggplot(points_sparse, aes(x,y)) +
  geom_path() +
  scale_y_reverse() +
  theme_void()

#that doesn't work
#new plan: walk along points, remove any within certain distance
#this will be super dooper slow probably, can fix later if necessary

#actually, this is probably not best
for(i in 1:nrow(data_to_plot)) {
  if(
    
  )
  current <- i 
}

#I think it's better to just do interactive selection w/ Shiny
#use nearPoints() function in Shiny app
saveRDS(data_to_plot, file = "giraffe_arranged.rds")

#paste from app test
#had to download csv then copy paste
sparse_points <- 
  tibble::tribble(
         ~"",       ~"x",       ~"y",
  "\"5479\"",  "\"393\"",  "\"893\"",
  "\"5623\"",  "\"381\"",  "\"963\"",
  "\"5800\"",  "\"397\"", "\"1043\"",
  "\"6126\"",  "\"365\"", "\"1105\"",
  "\"6189\"",  "\"433\"", "\"1114\"",
  "\"7072\"",  "\"534\"", "\"1529\"",
  "\"7705\"",  "\"638\"", "\"1804\"",
  "\"8075\"",  "\"629\"", "\"1988\"",
  "\"8652\"",  "\"718\"", "\"2235\"",
  "\"9023\"",  "\"880\"", "\"2303\"",
  "\"8596\"", "\"1053\"", "\"2216\"",
  "\"7923\"", "\"1129\"", "\"1912\"",
  "\"7682\"", "\"1118\"", "\"1792\"",
  "\"6749\"", "\"1261\"", "\"1381\"",
  "\"6182\"", "\"1323\"", "\"1110\"",
  "\"6143\"", "\"1409\"", "\"1105\"",
  "\"5737\"", "\"1360\"", "\"1013\"",
  "\"5639\"", "\"1374\"",  "\"970\"",
  "\"5501\"", "\"1365\"",  "\"903\"",
  "\"4778\"", "\"1592\"",  "\"732\"",
  "\"2749\"", "\"1747\"",  "\"431\"",
  "\"2657\"", "\"1485\"",  "\"423\"",
  "\"3615\"", "\"1354\"",  "\"549\"",
  "\"4465\"", "\"1228\"",  "\"648\"",
  "\"2898\"", "\"1191\"",  "\"450\"",
  "\"1352\"", "\"1180\"",  "\"260\"",
   "\"575\"", "\"1207\"",   "\"70\"",
   "\"117\"", "\"1133\"",    "\"3\"",
   "\"759\"", "\"1000\"",  "\"115\"",
  "\"1484\"",  "\"991\"",  "\"291\"",
  "\"2036\"",  "\"884\"",  "\"389\"",
  "\"1609\"",  "\"777\"",  "\"321\"",
   "\"621\"",  "\"750\"",   "\"81\"",
    "\"16\"",  "\"643\"",    "\"2\"",
   "\"602\"",  "\"548\"",   "\"77\"",
  "\"1512\"",  "\"577\"",  "\"298\"",
  "\"3584\"",  "\"562\"",  "\"545\"",
  "\"4500\"",  "\"518\"",  "\"650\"",
  "\"2849\"",  "\"313\"",  "\"444\"",
  "\"2758\"",    "\"7\"",  "\"433\"",
  "\"3317\"",   "\"11\"",  "\"508\"",
  "\"4947\"",  "\"210\"",  "\"785\""
  )
sparse_points2 <- sparse_points %>%
  select(2:3) %>%
  map_df(., ~readr::parse_number(.x))

ggplot(sparse_points2, aes(x,y)) +
  geom_path() +
  scale_y_reverse() +
  theme_void()

#wow that worked
#save sparse points as rds and try triangulate
saveRDS(sparse_points2, "sparse_girrafe_points.rds")
