library("RCurl")

# Download official geologic time scale colors from macrostrat
ColorssURL<-"https://macrostrat.org/api/defs/intervals?true_clors&format=csv"
GotURL<-getURL(ColorsURL)
TimeScaleColors<-read.csv(text=GotURL,header=TRUE)


# EPOCHS BAR PLOT 
# Make a bar plot showing the numer of units in the UnitMatrix that fall into each epoch category
EpochSums<-apply(AgeMatrix,2,sum)

# 
