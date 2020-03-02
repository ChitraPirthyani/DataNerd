# From Graphics for Statistics and Data Analysis with R
# Mosaic Plot (two categorical and a numerical)
# hair color and eye color
haireye<-matrix(data=c(20,5,15,68,84,29,54,119,17,14,14,26,94,16,10,7),
                nrow=4,ncol=4,byrow=TRUE,
                dimnames=list(c("Black","Brunette","Red","Blond"),
                              c("Blue","Green","Hazel","Brown")))
head(haireye)

## two-way dot chart
library(lattice)
dotplot(haireye,xlab="Frequency",ylab="Eye Color",as.table=TRUE,
        groups=FALSE,stack=FALSE,layout=c(1,4),scales=list(alternating=3))


## mosaic plot
# built-in to R (not ggplot)
# las=1 : axis labels horizontal for both axes
# cex=0.75 : tightens the size of the label box
# color=TRUE : misleading, just mean grayscaling
mosaicplot(t(haireye),main=" ",las=1,cex=0.75,color=TRUE)


## graph drawing
# from R Graphics Cookbook
# May need to install first, with install.packages("igraph")
library(igraph)
# Specify edges for a directed graph
gd <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6))
str(gd)
plot(gd)
# For an undirected graph
gu <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6), directed=FALSE)
str(gu)
# No labels
plot(gu, vertex.label=NA)

library(gcookbook) # For the data set
head(madmen2)
# Create a graph object from the data set
g <- graph.data.frame(madmen2, directed=TRUE)
str(g)
# Remove unnecessary margins
par(mar=c(0,0,0,0))
# plot with FR layout, control line/arrow properties, no labels
plot(g, layout=layout.fruchterman.reingold)
plot(g, layout=layout.fruchterman.reingold, vertex.size=8, edge.arrow.size=0.5,
     vertex.label=NA)
# circular layout, undirected
g <- graph.data.frame(madmen, directed=FALSE)
par(mar=c(0,0,0,0)) # Remove unnecessary margins
plot(g, layout=layout.circle, vertex.size=8, vertex.label=NA)
plot(g, layout=layout.circle, vertex.size=8)
