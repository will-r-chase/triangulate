library(RTriangle)
library(tidyverse)
library(colourlovers)
library(gganimate)
library(mgcv)

#######################################################################
##attempt at making giraffe
#######################################################################
giraffe <- readRDS("sparse_girrafe_points.rds")

#border points
x <- giraffe$x
y <- giraffe$y
bounds <- cbind(x, y)

#S tells the order of how the points are connected, required for concavity
##to do: function to make S automatically
s1 <- 1:nrow(giraffe)
s2 <- 2:(nrow(giraffe)+1)
s2[nrow(giraffe)] <- 1

S <- as.matrix(cbind(s1, s2))

ps <- pslg(P = bounds, S = S)
plot(ps)
rtriang <- triangulate(ps)
plot(rtriang)

#generate some points inside boundary
x <- runif(100, min = min(giraffe$x), max = max(giraffe$x))
y <- runif(100, min = min(giraffe$y), max = max(giraffe$y))

is_inside <- inSide(giraffe, x, y)

fill <- tibble(x, y, is_inside) %>%
  filter(is_inside)

inners <- cbind(fill$x, fill$y)

#PB defines which poins are borders, which are not (1 is border, 0 not)
df <- giraffe
df2 <- fill[, 1:2]
df$PB <- 1
df2$PB <- 0

x_comb <- c(df$x, df2$x)
y_comb <- c(df$y, df2$y)
PB <- c(df$PB, df2$PB)
xy_comb <- cbind(x_comb, y_comb)

ps <- pslg(P = xy_comb, PB = PB, S = S)
plot(ps)
rtriang <- triangulate(ps)
plot(rtriang)

################################################################
##getting polygons from triangles
## T variable gives vertex indicies of triangles, P gives points
################################################################

#extract points
pts_df <- tibble(id = 1:(length(rtriang$P)/2), x = rtriang$P[, 1], y = rtriang$P[, 2])

#get triangle vertex indices
triangles <- as.data.frame(rtriang$T)
tri_df <- tibble(id_1 = triangles$V1, id_2 = triangles$V2, id_3 = triangles$V3, group = 1:nrow(triangles))

#join triangle vertex indices with vertex points
tri_list <- split(tri_df, tri_df$group) %>%
  map( ~select(., 1:3)) %>%
  map( ~as.tibble(t(.))) %>%
  map( ~inner_join(., pts_df, by = c("V1" = "id"))) %>%
  map( ~select(., x, y))

#function to get area of triangle from 3 points
triang_area <- function(data) {
  x <- data$x
  y <- data$y
  mat <- matrix(data = c(1,1,1,x[1],x[2],x[3],y[1],y[2],y[3]), nrow = 3, ncol = 3, byrow = TRUE)
  area <- 0.5*det(mat)
  return(area)
}

#add area to each triangle and do some reshuffling
tri_list %>%
  map( ~mutate(.x, area = triang_area(.x))) %>%
  bind_rows(.id = "id") %>%
  select(id, x, y, area) -> triang_df

#colors
palette <- 
  sample(clpalettes('top'), 1)[[1]] %>% 
  swatch %>% 
  .[[1]]

good_pal <- c("#ECD078", "#D95B43", "#C02942", "#542437", "#53777A")

#plot w/ geom_polygon
ggplot(triang_df, aes(x = x, y = y)) +
  geom_polygon(aes(fill = area, color=area, group = id), 
               show.legend = FALSE, size=0)+
  scale_fill_gradientn(colors=sample(good_pal, length(good_pal))) + 
  scale_color_gradientn(colors="gray30") +   
  theme_void() +
  scale_y_reverse()

#plot w/ geom_polygon, random color
ggplot(triang_df, aes(x = x, y = y)) +
  geom_polygon(aes(fill = area, color=area, group = id), 
               show.legend = FALSE, size=0)+
  scale_fill_gradientn(colors=sample(palette, length(palette))) + 
  scale_color_gradientn(colors="gray30") +   
  theme_void() +
  scale_y_reverse()

#messing with look of plot, I like this, it's cool
ggplot(triang_df, aes(x = x, y = y)) +
  geom_polygon(aes(fill = area, group = id), 
               color = "white", show.legend = FALSE, size=1)+
  scale_fill_gradientn(colors=sample(good_pal, length(good_pal))) + 
  scale_color_gradientn(colors="gray30") +   
  theme_void() +
  scale_y_reverse()

ggsave("giraffe_constrained.png", device = "png", type = "cairo")

####stopped here with giraffe stuff#####
#plan next: do it with two diff images and animate between
#also tri voronoi on giraffe, triangulate on stag






#############################################################
##testing animate############################################
#############################################################

#make top figure state 1, bottom figure state 2
triang_state_df <-
  triang_df %>%
  mutate(state = ifelse(y > 2, as.integer(1), as.integer(2)), 
         id = as.numeric(.$id)) %>%
  arrange(id)

#make each state have the same group indices for triangles
anim_df <- split(triang_state_df, triang_state_df$state) %>%
  map( ~mutate(., group = group_indices(., id))) %>%
  map( ~select(., x, y, area, state, group)) %>%
  bind_rows()

#animate between two states with transition_states
bowtie <- 
  anim_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_polygon(aes(fill = area, color=area, group = group), 
               show.legend = FALSE, size=0)+
  scale_fill_gradientn(colors=sample(palette, length(palette))) + 
  scale_color_gradientn(colors="gray30") +   
  theme_void() + 
  ease_aes('cubic-in-out') +
  transition_states(state, transition_length = 2, state_length = 1, wrap = TRUE)

animate(bowtie, nframes = 100, fps = 10, detail = 2, type = "cairo")
anim_save("bowtie3.gif")

#try with white borders, kinda interesting
#looks kinda weird when tweening? maybe better for static plot
bowtie <- 
  anim_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_polygon(aes(fill = area, group = group), 
               color = "white", show.legend = FALSE, size=2)+
  scale_fill_gradientn(colors=sample(palette, length(palette))) + 
  scale_color_gradientn(colors="gray30") +   
  theme_void() + 
  ease_aes('cubic-in-out') +
  transition_states(state, transition_length = 2, state_length = 1, wrap = TRUE)

animate(bowtie, nframes = 100, fps = 10, detail = 2, type = "cairo")