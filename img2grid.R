# Copyright (c) 2018 Jan "JayVii" <jayvii@posteo.de>
# SPDX-License-Identifier: GPL-3.0

# Load Packages {{{ ----

library("imager")
library("ggplot2")
library("dplyr")
library("geosphere")

# }}}

# Import image, grayscale, filter, and sample image {{{ ----
file <- "~/face.jpg" # not contained in repo ;)

# Kudos: @aschinchon
# https://github.com/aschinchon/travelling-salesman-portrait/
load.image(file) %>% 
  grayscale() %>%
  threshold("35%") %>% 
  as.cimg() %>% 
  as.data.frame()  %>% 
  sample_n(10000, weight=(1-value)) %>% 
select(x,y) -> data

# }}}

# Scale data and calculate distances between each point
data_st <- data / max(data)
distances <- distm(x = data_st) # dirty. FIXME (assumes ellipsoid or sphere)

# find nearest point for each pair of x/y
near <- matrix(data = NA, nrow = length(data_st$x), ncol = length(data_st$x))
for(i in 1:length(data_st$x)){
    near[i,] <- order(distances[i,])
}

# plot the result {{{ ----

# we want a PDF file from that (10" x 10")
pdf(file = "~/out/me.pdf", height = 10, width = 10)

    # without margins around plot
    par(mar=c(0, 0, 0, 0))

    # dummy (empty) plot, with correct xylims
    plot(-data_st, type = "l", col = rgb(0,0,0,alpha = 0), axes = FALSE,
         xlab = "", ylab = "", main = "")

    # add lines connecting the nearest 20 xy-pairs to current point
    # also mirror data points (-)
    # color is black with some low alpha value
    for(i in 1:length(data_st$x)){
        lines(-data_st[near[i,1:20],], type = "l",
              col = rgb(0, 0, 0, alpha = 0.05))
    }

dev.off()

# }}}
