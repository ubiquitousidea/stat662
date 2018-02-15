requireNamespace(grid, quietly = FALSE)

#' Compute the time range from the block parameters
#' @param block.parms the data frame containing the geometric information 
#' about the time blocks to be plotted
#' @return a vector of (min time , max time)
compute.time.range <- function(block.parms){
  min.time <- min(
    min(block.parms$y1),
    min(block.parms$y2)
  )
  max.time <- max(
    max(block.parms$y1), 
    max(block.parms$y2)
  )
  return(c(min.time, max.time))
}

#' Get the names in the header column values from a time table data frame
#' @param time.table a data frame with columns start.time, end.time, day.of.week
#' @param group.by name of the grouping variable column in time.table data frame
#' @return vector of unique column names
get.header.names <- function(time.table, group.by="day.of.week"){
  names <- unique(time.table[,group.by])
}


#' Draw the header on the plot using grid
#' @param header.names Names of the columsn to label in the table header
#' @return NULL
make.header <- function(header.names){
  n.names <- length(header.names)
  x.vect <- seq(
    0.5 * n.names, 
    1 - (0.5 * n.names), 
    n.names)
  for(i in 1:n.names){
    grid.text(
      header.names[i], 
      x=x.vect[i], y=.5, 
      just='center', 
      gp=gpar(col="black", fontsize=18)
    )
  }
}

#' Draw the time blocks according to block.parms
#' @param block.params data frame of time block information
#' @return NULL
make.blocks <- function(block.parms){
  # expect columns of block.parms to have
  # x, y, height, width, rgba
  for(i in 1:nrow(block.parms)){
    block.data <- block.parms[i,]
    grid.rect(
      x = block.data$x,
      y = block.data$y,
      height = block.data$height,
      width = block.data$width,
      just=c('top', 'left'),
      gp=gpar(col="black", fill=block.data$rgba)
    )
  }
}

#' Determine the times at which a line marking should be drawn
#' @param min.max.time a vector of (min time, max time)
#' @param n.marks number of marks to produce. If a non-integer is provided
#' marks are made at each hour
#' @return vector of times
get.marked.times <- function(min.max.time, n.marks=NULL){
  min.time <- min.max.time[1]
  max.time <- min.max.time[2]
  if(!is.integer(n.marks)){
    n.marks <- as.integer(max.time - min.time + 1)  # default is 1 mark per hour
  }
  marked.times <- seq(min.max.time[1], 
                      min.max.time[2], 
                      n.marks)
  return(marked.times)
}

#' Mark the hour lines on the main plot area
#' @param min.max.time vector of (min time, max time)
#' @param n.marks the number of time marking to draw
#' @param alpha transparency of the marked lines
#' @return NULL
make.hour.lines <- function(min.max.time, n.marks, alpha=0.5){
  marked.times <- get.marked.times(min.max.time, n.marks)
  for(marked.time in marked.times){
    grid.lines(
      x=c(0,1), 
      y=c(marked.time, marked.time),
      gp=gpar(lty=2, alpha=alpha)
    )
  }
}

#' Draw the time scale on the plot
#' @param min.max.time vector of (min time, max time)
#' @param n.marks number of markings to make
#' @param alpha transparency for the time marking names
#' @return NULL
make.time.scale <- function(min.max.time, n.marks, alpha=0.5){
  marked.times <- get.marked.times(min.max.time, n.marks)
  for(marked.time in marked.times){
    hr <- as.integer(marked.time)
    mn <- (marked.time - hr) %/% 60  # floor divide
    if(hr >= 12){
      md <- "PM"
      hr <- hr - 12
    } else {
      md <- "AM"
    }
    time.string <- sprintf("%02d:%02d %s", hr, mn, md)
    grid.text(
      time.string, x=.5, 
      y=unit(marked.time, 'native'),
      just="center", gp=gpar(alpha=alpha)
    )
  }
}

#' Parse Class times from CSV in a specified format
#' 
#' @param filename name of the csv file to read
#' @return data frame with columns start.time, end.time, day.of.week and school indicators
parse.class.times <- function(filename){
  # same as def from 2/1/18 class (parseclasstimes.R)
  days.of.week <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  schools <- c("arch","div","fes","som","sph","yc")  # suffix for school column names
  raw.data <- read.csv(filename, as.is=TRUE)
  n.rows <- nrow(raw.data)
  output <- data.frame()
  for(i.row in 1:n.rows){
    class.name <- sprintf("class.%04d", i.row)
    row.data <- raw.data[i.row,]
    for(school.name in schools){
      full.school.name <- paste("School", school.name, sep="_")
      indicator <- !is.na(row.data[,full.school.name])
      assign(school.name, indicator)  # dynamic variable name assignment ^__^
    }
    for(d.o.w in days.of.week){
      # detect start/duration times for each day of week
      # assign start.time, end.time, and day.of.week variables
      start.time <- row.data[,paste(d.o.w, "time", sep="_")]
      dur.time <- row.data[,paste(d.o.w, "dur", sep="_")]
      if(!is.na(start.time)){
        end.time <- start.time + dur.time / 60
        new.row <- data.frame(
          class.name = class.name,
          start.time = start.time, 
          end.time = end.time, 
          day.of.week = d.o.w,
          arch = arch,
          div = div,
          fes = fes,
          som = som,
          sph = sph,
          yc = yc
        )
        output <- rbind(output, new.row)
      }
    }
  }
  return(output)
}

#' Compute the time block parameters given the time table
#' This involves assigning the courses to non-overlapping
#' 
#' @param time.table a time table data frame of the format that is produced by
#' \link{parse.class.times} function.
#' @return block parameters; a data frame
compute.blocks <- function(time.table){
  # multiple approaches possible here. if input/output is the same
  # the options could be interchangeable
  # input: table with columns:
  # class.name, start.time, end.time, day.of.week, and 
  # a series of indicators of each school name:
  # arch, div, fes, som, sph, yc.
  
  block.parms <- data.frame(
    x1=c(.2, .4, .6),
    y1=c(8, 9, 10),
    x2=c(.1,.1,.1), 
    y2=c(9.25,10.25,11.25),
    rgba=rep("dodgerblue2", 3)
  )
  return(block.parms)
}

#' Plot the time blocks using grid
#' @param block.parms data frame of block parameters
#' @param header.names vector of grouping variable names
#' @return NULL
plot.time.blocks <- function(block.parms, header.names){
  min.max.time <- compute.time.range(block.parms)
  min.time <- min.max.time[1]
  max.time <- min.max.time[2]
  aspect.ratio <- 16./9.  # assumed. figure out a way to query this.
  padding.y <- .02
  padding.x <- padding.y * aspect.ratio
  time.scale.width <- 0.07
  header.height <- 0.12
  grid.newpage()
  
  # viewport for padding the edges of the page
  pushViewport(
    viewport(
      x = padding.x, y = padding.y,
      height = 1 - (2 * padding.y),
      width = 1 - (2 * padding.x),
      just = c('top', 'left')
    )
  )
  grid.rect()
  # viewport for the time.scale
  pushViewport(
    viewport(
      x = 0.0, y = 1.0,
      width = time.scale.width, 
      height = 1.0,
      just = c('top', 'left')
    )
  )
  make.time.scale(min.max.time) 
  popViewport() 
  # done making time scale. back to the padded full page
  
  # viewport for the column block and header
  pushViewport(
    viewport(
      x = time.scale.width,
      y = 1.0,
      width = 1.0 - time.scale.width,
      height = 1.0,
      just = c('top', 'left')
    )
  )
  # child viewport for the header
  pushViewport(
    viewport(
      x = 0.0, y = 1.0,
      height = header.height,
      width = 1.0,
      just = c('top', 'left')
    )
  )
  make.header(header.names)
  # done making the header. back to the column block viewport
  popViewport()
  
  # ----------------------------------------------------------------------------
  # ------------------ Main column block viewport with hour units --------------
  # ----------------------------------------------------------------------------
  pushViewport(
    viewport(
      x=0.0, y=0,0, just=c('bottom', 'left'),
      width=1.0, height=1-header.height,
      yscale=c(max.time, 
               min.time)
    )
  )
  grid.rect(gp=gpar(border="black"))  # border around column blocks area
  # plot time blocks with specified parameters (position, size, color...)
  make.blocks(block.parms)
  # same viewport
  make.hour.lines(min.max.time)
}

#' Main function that plots times given the name of the file that contains the
#' course times
#' 
#' @param f.name name of the csv file containing the course time data
#' @export
display.times <- function(f.name){
  time.table <- parse.class.times(f.name)  # this function exists
  block.parms <- compute.blocks(time.table)  # this function doesn't exist yet
  header.names <- get.header.names(time.table)  # needs to exists
  plot.time.blocks(block.parms, header.names)  # this will probably be the only function with calls to grid
}

# call the function
# display.times('course_times.csv')
# time.table <- parse.class.times("course_times.csv")
