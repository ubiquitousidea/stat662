# Quickly hashing out idea for how the functions could be laid out

# Outermost function call
display.times('course_time.csv')
# The functions may need to be reordered for execution to work. 
# Unsure how R evaluates function definitions.

display.times <- function(f.name){
  time.table <- parse.class.times(f.name)  # this function exists
  block.prms <- compute.blocks(time.table)  # this function doesn't exist yet
  header.names <- get.header.names(time.table)  # needs to exists
  plot.time.blocks(block.parms, header.names)  # this will probably be the only function with calls to grid
}

  
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

compute.blocks <- function(time.table){
  # multiple approaches possible here. if input/output is the same
  # the options could be interchangeable
  # input: table with columns:
  # class.name, start.time, end.time, day.of.week, and 
  # a series of indicators of each school name:
  # arch, div, fes, som, sph, yc.
  #
  # this is where most of the code will be...
  # again, multiple interchangeable approaches are possible.
}

plot.block.times <- function(block.parms, header.names){
  # block.parms: data.frame with columns: c(min.x, max.x, min.y, max.y, color)
  # header.names: names that are shown above each table column
  # -----------------------------------
  # make a viewport for the header here
  make.header(header.names)
  min.max.time <- compute.time.range(block.parms) # use block.parms to determine min and max time plotted
  # make a viewport for the timescale here
  make.time.scale(min.max.time, per.hour = 1) 
  # make viewport for the column blocks (by day or school) taking up the majority of the plot
  make.blocks(block.parms)  # plot time blocks with specified parameters (position, size, color...)
  # same viewport
  make.hour.lines(min.max.time, per.hour = 1)
}

make.header <- function(header.names){
  # does not make a new viewport
  # make the header with grid
}

make.time.scale <- function(min.max.time, per.hour){
  # does not create a new viewport
  # make the time scale markings at the desired rate
}

make.blocks <- function(block.parms){
  # preprocessing?? keep it to a minimum
  # iterate through values of the grouping variable (day or school name)
  # make a new viewport for each value, then plot the class block requested
}

make.hour.lines <- function(min.max.time, per.hour){
  # does not create a new viewport
  # make horizontal dashed lines at the requested rate
}