#! /usr/bin/env Rscript

## This file is part of prosoda.  prosoda is free software: you can
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
## Copyright 2013 by Siemens AG, Wolfgang Mauerer <wolfgang.mauerer@siemens.com>
## All Rights Reserved.

## Create activity punchcards for mailing list activity data

s <- suppressPackageStartupMessages
s(library(ggplot2))
s(library(shiny))
s(library(logging))
s(library(lubridate))
s(library(xts))
rm(s)
source("config.r")
source("utils.r")
source("query.r")
source("ts_utils.r")
source("vis.ports.r")

## Global variables
conf <- config.from.args(require_project=FALSE)
projects.list <- query.projects(conf$con)
#####

## Generate commity activity punch card datasets for all cycles
## of a given project
gen.punchcard <- function(con, ml.id, range.id) {
##  dat <- query.ml.activity(conf$con, ml.id, range.id)
  dat <- query.ml.activity(conf$con, 9, range.id)

  if (!is.null(dat)) {
    act.ts <- xts(x=1:length(dat), order.by=dat)
    res <- compute.hourly.statistics(act.ts, FUN=length)
  } else {
    res <- NULL
  }

  return(res)
}

gen.punchcards <- function(con, pid) {
  range.ids.list <- query.range.ids.con(con, pid)
  cycles <- get.cycles.con(con, pid)

  res <- lapply(range.ids.list, function(range.id) {
    res <- gen.punchcard(con, pid, range.id)
    if (!is.null(res)) {
      res <- cbind(res, cycle=cycles$cycle[cycles$range.id==range.id])
    }

    return(res)
  })

  res <- do.call(rbind, res)
  return(res)
}


vis.punchcard.server <- function(input, output, clientData, session) {
  pid <- reactive({projects.list[projects.list$name==input$project,]$id})
  res <- reactive({gen.punchcards(conf$con, pid())})

  output$punchCardPlot <- renderPlot({
    g <- ggplot(res(), aes(x=hour, y=day, size=size)) + geom_point() +
      facet_wrap(~cycle)

    print(g)
  })
}

vis.punchcard.ui <-
  pageWithSidebar(
    headerPanel("ML activity punch cards"),

    sidebarPanel(
      selectInput("project", "Project",
                  choices = projects.list$name),
      
      submitButton("Update View")
      ),
    
    mainPanel(
      plotOutput("punchCardPlot")
      )
    )

## Dispatch the shiny server
runApp(list(ui=vis.punchcard.ui, server=vis.punchcard.server),
       port=PORT.PUNCHCARD.ML)
