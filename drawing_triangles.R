png(filename = "ACE.png", width = 5, height = 15, units = "cm", pointsize = 12, res = 300, type = "cairo")
par(bg = "white", usr = c(0, 51, 0, 451)) #make the plot window a certain size?

plot(x=NULL, y=NULL , type = "n", axes = F, xlab = "", ylab = "", xlim=c(0,51), ylim=c(0,450)) #set up the plot?

#draw polys
polygon(x = c(0, 10, 20), y = c(0, 10, 200), col = "#c00000", border = "black")
polygon(x = c(10, 20, 30), y = c(10, 200, 0), col = "#ed7d31", border = "black")

dev.off()

#draw rectangles for thermometer
rect(0, 0, 50, 148, col = "#c00000", border = "black")     #red
rect(0, 148, 50, 225, col = "#ed7d31", border = "black") #orange
rect(0, 225, 50, 297, col = "#ffc000", border = "black") #gold
rect(0, 297, 50, 360, col = "#92d050", border = "black") #lgreen
rect(0, 360, 50, 450, col = "#00b050", border = "black") #dgreen

dev.off()
