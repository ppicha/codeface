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

## Interactive analysis example: Relation between number of commits and code changes
####################################################################################

source("interactive.r")
source("../utils.r", chdir=TRUE)
source("../query.r", chdir=TRUE)
library(doBy)

## Adapt this line to select a project of interest
conf <- create.conf("/vagrant/codeface.conf", "/vagrant/conf/PROJECT.conf")

## Query all per-author contributions for the complete project history,
## and compute cumulative numbers
range.ids <- query.range.ids.con(conf$con, conf$pid)
all.stats <- do.call(rbind, lapply(range.ids, function(range.id) {
    return(get.range.stats(conf$con, range.id))
}))

all.stats <- summaryBy(added+deleted+total+numcommits~ID+Name,
                       data=all.stats, FUN=sum)
colnames(all.stats)[3:5] <- c("Added", "Deleted", "Modified")

all.stats.molten <- melt(all.stats,
                         id.vars=c("Name", "ID", "numcommits.sum"))

## Visualise the result in a scatter plot
g <- ggplot(all.stats.molten, aes(x=numcommits.sum, y=value)) +
    geom_point() + geom_smooth() + facet_wrap(~variable) +
    scale_y_sqrt("# LoC") + scale_x_sqrt("# Commits") + theme_bw()
ggsave("scatter.pdf", g, width=10, height=5)
