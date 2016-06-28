## This file is part of Codeface. Codeface is free software: you can
## redistribute it and/or modify it under the terms of the GNU General Public
## License as published by the Free Software Foundation, version 2.
##
## This program is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##
## Copyright 2016 by Wolfgang Mauerer <wolfgang.mauerer@oth-regensburg.de>
## All Rights Reserved.

## Interactive analysis example: Explore time evolution of global coordination structures
#########################################################################################

## NOTE: The web frontend is limited to presenting individual cooperations clusters;
## for larger projects, it makes little visual sense to show clusters with several
## hundreds of contributors. For small projects with only tens or less contributors,
## individual clusters usually only comprise a handful of members -- the global
## structure of a project may be more interesting in this case. This script
## shows how to plot the temporal evolution of the coordination structure for
## small projects.

library(igraph)
library(Hmisc) # for %nin%
library(RColorBrewer)
library(reshape2)
source("interactive.r")
source("../clusters.r", chdir=TRUE)
source("../utils.r", chdir=TRUE)
source("../query.r", chdir=TRUE)

## Adapt this line to select a project of interest
conf <- create.conf("/vagrant/codeface.conf", "/vagrant/conf/PROJECT.conf")

range.ids <- query.range.ids.con(conf$con, conf$pid)

## Determine the person ids of all contributors to a project
person.ids.global <- unique(do.call(rbind,
                       lapply(range.ids,
                             function(range.id) {
                                 get.range.stats(conf$con, range.id)[c("ID", "Name")]})))

## Assign a different color for each person (the color as such has
## no significance, we just use it to visually distinguish them in the graph)
num.cols <- dim(person.ids.global)[1]
cols <- colorRampPalette(brewer.pal(9, "Blues"))(floor(num.cols/3))
cols <- c(cols, colorRampPalette(brewer.pal(9, "Reds"))(floor(num.cols/3)))
cols <- c(cols, colorRampPalette(brewer.pal(9, "Greens"))(floor(num.cols/3) + num.cols%%3))
person.ids.global$color <- cols

## Provide short (partly anonymised) identifiers for contributing persons
person.ids.global$short <-
    as.character(abbreviate(person.ids.global$Name))

get.short <- function(id) {
    return(person.ids.global[person.ids.global$ID==id,]$short)
}

get.color <- function(id) {
    return(as.character(person.ids.global[person.ids.global$ID==id,]$color))
}


## Create a global collaboration graph for a given range ID
create.collaboration.graph <- function(conf, range.id) {
    cluster.id <- query.global.collab.con(conf$con, conf$pid, range.id)

    if (!is.null(cluster.id)) {
        g <- construct.cluster(conf$con, cluster.id)
        g <- annotate.cluster(g)
    } else {
        g <- make_empty_graph()
    }

    stats <- get.range.stats(conf$con, range.id)

    ## Add contributors that are unconnected to everyone else to the graph
    ## (for larger persons, such contributors are usually outliers of some
    ## sort and are therefore not stored in the database, the same way
    ## as one-person clusters are not stored)
    missing.ids <- stats$ID[stats$ID %nin% as.numeric(attr(V(g), "name"))]
    g <- g + vertices(as.character(missing.ids))

    ## Use the number of commits as scale for the vertices (contributors)
    num.commits <- sapply(attr(V(g), "name"), function(id) {
        return(stats[stats$ID==id,]$numcommits)
    })
    V(g)$size <- sqrt(as.numeric(num.commits))*6

    return(g)
}

## Construct a panel plot of all clusters
do.cluster.plots <- function(conf, range.ids) {
  par(mfrow=c(ceil(length(range.ids)/8),8))
  for (i in 1:length(range.ids)) {
      range.id <- range.ids[[i]]
      g <- create.collaboration.graph(conf, range.id)

      V(g)$color <- as.character(sapply(attr(V(g), "name"), get.color))
      V(g)$name <- sapply(attr(V(g), "name"), get.short)
      E(g)$arrow.width <- 0.35
      E(g)$arrow.size <- 0.35
      plot(g, layout=layout_nicely, margin=-0.15, vertex.label.dist=1,
           vertex.label.cex=0.8, frame=TRUE,
           main=substr(conf$tstamps.release$date[[i]], 0, 10))
  }
}

## Use A4 landscape size to plot the resulting graphs
pdf("communities.pdf", width=29.7/2.54, height=20.0/2.54)
do.cluster.plots(conf, range.ids)
dev.off()
