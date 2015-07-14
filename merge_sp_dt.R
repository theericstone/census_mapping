#join spatial data w/ cols from a data.frame

#x=sp SpatialDataFrame object
#y=dataframe object to merge with x
#xcol=Merge column name in sp object (need to quote)
#ycol=Merge column name in dataframe object (need to quote)

JoinSpDf <- function(x, y, xcol, ycol) {
  x$sort_id <- 1:nrow(as(x, "data.frame"))  
  x.dat <- as(x, "data.frame")  
  x.dat2 <- merge(x.dat, y, by.x = xcol, by.y = ycol)  
  x.dat2.ord <- x.dat2[order(x.dat2$sort_id), ]  
  x2 <- x[x$sort_id %in% x.dat2$sort_id, ]  
  x2.dat <- as(x2, "data.frame") 
  row.names(x.dat2.ord) <- row.names(x2.dat)  
  x2@data <- x.dat2.ord  
  return(x2)
}