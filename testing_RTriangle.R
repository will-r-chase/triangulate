library(RTriangle)

## Create an object with a concavity
p <- pslg(P=rbind(c(0, 0), c(0, 1), c(0.5, 0.5), c(1, 1), c(1, 0)),
          S=rbind(c(1, 2), c(2, 3), c(3, 4), c(4, 5), c(5, 1)))
## Plot it
plot(p)
## Triangulate it
tp <- triangulate(p)
plot(tp)
## Triangulate it subject to minimum area constraint
tp <- triangulate(p, a=0.01)
plot(tp)
## Load a data set containing a hole
A <- read.pslg(file.path(system.file(package = "RTriangle"), "extdata", "A.poly"))
plot(A)
## Triangulate the PSLG
tA <- triangulate(A)
plot(tA)
## Triangulate the PSLG with triangles in which no angle
## is smaller than 20 degrees
tA <- triangulate(A, q=20)
plot(tA)
## Triangulate the PSLG with triangles in which no triangle has
## area greater than 0.001
tA <- triangulate(A, a=0.001)
plot(tA)


library(ggplot2)
#try to make our own plot using ggplot2

x = tA$P[tA$E[,1],1]
y = tA$P[tA$E[,1],2]
xend = tA$P[tA$E[,2],1]
yend = tA$P[tA$E[,2],2]

poly_x <- c(x, xend)
poly_y <- c(y, yend)

df <- data.frame(x, y, xend, yend)
df_poly <- data.frame(x = poly_x, y = poly_y)
df_poly <- unique(df_poly)

ggplot(df) + 
  geom_segment(aes(x=x, y=y, xend=xend, yend=yend))

ggplot(df_poly) +
  geom_polygon(aes(x = x, y = y))

## I think what needs to be done is get the verticies of the triangles and have object like returned by deldir
## for each edge, find edges that share endpoints
## group into 3s or something

