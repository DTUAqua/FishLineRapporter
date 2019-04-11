library(marmap)

#  Fetch data on NOAA servers and write on disk
bat <- getNOAA.bathy(5, 16, 52, 60, res = 1, keep=TRUE)

bat2 <- bat
# bat2[bat2 < -200] <- -200
bat2[bat2 > 1] <- 1

# Create nice looking color palettes
watercol <- colorRampPalette(c("blue4", "azure"))(40)
# watercol <- grDevices::heat.colors(40)
landcol <- colors()[25]



# Plot
plot(bat2, image = TRUE, land = TRUE, lwd = 0.1, bpal = list(c(0, max(bat2), landcol), c(min(bat2), 0, watercol)))
plot(bat2, lwd = 0.8, deep = -200, shallow = 0, step = 10, add = TRUE) # highlight coastline


# 
# 
# library(ggplot2)
# 
# # Load North West Atlantic dataset
# data(bat)
# atl <- as.bathy(bat)
# atl<-bat
# 
# # Plot with ggplot2
# autoplot(atl, geom=c("raster", "contour"), colour="white", size=0.1) + 
#   scale_fill_gradient2(low="dodgerblue4", mid="gainsboro", high=NA)
# 
# 
# 
