plotFootprint <-
function (footprint, main.title = NULL, key.title = NULL, color.palette = NULL, 
    ...) 
{
    new.red = rgb(255/255, 0/255, 0/255)
    new.yellow = rgb(255/255, 255/255, 0/255)
    new.blue = rgb(0/255, 0/255, 255/255)
    if (is.null(color.palette)) {
        rgb.palette <- colorRampPalette(c(new.blue, new.yellow, 
            new.red))
        color.palette = rgb.palette
    }
    ftpr = t(apply(footprint[[1]], 2, rev))
    cord = unique(as.vector(footprint[[2]]))
    filled.contour(cord, cord, ftpr/max(ftpr,na.rm = TRUE)*100, color.palette = color.palette,
        plot.title = title(main = main.title), key.title = title(main = key.title,
            adj = 0.2), ...)
   # par(bg='gray')
    #contour(cord,cord, ftpr/max(ftpr,na.rm = TRUE)*100,col = 'white',add=F)
}