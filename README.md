# Triangulate

Here I'm testing lots of stuff for representing images as Delaunay triangulation or Voronoi tesselation. Idea was my own, LOTS of technical bits borrowed from Antonio Sánchez Chinchón (@aschinchon) and Thomas Lin Pederson (@thomasp85).

Ordinary triangulation and voronoi tesselation were easy by basically recycling code from @aschinchon. 

But I want constrained Dalaunay triangulation of any image I find online. I'm starting with binary clip art to save myself some pain. 

Currently working is toy example of constrained triangulation w/ RTriangle, and animation of constrained triangulation. I also have a good start on getting ordered boundary points from images as input for RTriangle, but that part is still in progress.
