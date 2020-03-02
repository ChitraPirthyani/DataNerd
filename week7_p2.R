library(ggplot2) # aside from ggplot itself, this contains 'diamonds'

# if you are using traditional R functions like 'hist' to make graphs,
# this will create a grid and the next set of graphs you make will 
# fill it in
# * this specifies 3 rows, 1 col
par(mfrow=c(3, 1))
hist(diamonds$price)
hist(diamonds$depth)
hist(diamonds$table)

# prep some data to show the grids
ds = as.data.frame(EuStockMarkets)
ds$day = seq(nrow(ds))

# If you're using ggplot, use the 'gridExtra' library.
# Store each plot and then use the grid.arrange command
# which lets you specify number of rows and columns of
# graphs
library(gridExtra)
p1 <- ggplot(ds, aes(day, DAX)) + geom_line(aes(size=DAX))
p2 <- ggplot(ds, aes(day, DAX)) + geom_line(aes(size=CAC))
p3 <- ggplot(ds, aes(day, DAX)) + geom_line(aes(color=CAC))
grid.arrange(p1,p2,p3,ncol=3)
grid.arrange(p1,p2,p3,nrow=3)
grid.arrange(p1,p2,p3,nrow=2, ncol=2)

# color brewer palette
# (use sample of diamonds data)
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
d <- ggplot(dsamp, aes(carat, price)) + geom_point(aes(color=clarity))
d
# use default color brewer palette and give the name of the legend
d + scale_colour_brewer("Diamond\nclarity")

# Select brewer palette to use, see ?scales::brewer_pal for more details
d + scale_colour_brewer(palette = "Greens")
d + scale_colour_brewer(palette = "Set1")

# scale_fill_brewer works just the same as
# scale_colour_brewer but for fill colours
p <- ggplot(diamonds, aes(x = price, fill = cut)) +
  geom_histogram(position = "dodge", binwidth = 1000)
p
p + scale_fill_brewer()
p + scale_fill_brewer(type = "seq", palette = 2)

# the order of colour can be reversed
p + scale_fill_brewer(direction = -1)

# the brewer scales look better on a darker background
p + scale_fill_brewer(direction = -1) + theme_dark()

library(scales)
d <- ggplot(dsamp, aes(carat, price)) + geom_point(aes(color=depth))
d + scale_colour_gradient2(low = muted("red"),
                         mid = "white",
                         high = muted("blue"),
                         midpoint = 63,
                         space = "Lab",
                         na.value = "grey50",
                         guide = "colorbar")
