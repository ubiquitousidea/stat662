library(grid)
library(colormap)

day.panel <- function(stime, etime, group=rep(1, length(stime))) {

  if ( length(stime) != length(etime) ||
       length(stime) != length(group) ||
       !is.numeric(stime) ||
       !is.numeric(etime) ) stop("input problem")
  mintime <- 9.0   # 9:00 AM
  maxtime <- 18.0  # 6:00 PM
  daytime <- maxtime - mintime
  nclass <- length(stime)
  sorting <- order(stime)
  stime <- stime[sorting]
  etime <- etime[sorting]
  group <- group[sorting]
  concurrence <- list()
  widths <- rep(1, nclass)
  # Find with which courses is each course concurrent
  for(iclass in 1:nclass){
    concurrence[iclass] <- c(iclass)  # the ith course is concurrent with itself
    not_i <- setdiff(1:nclass, iclass)
    for(jclass in not_i){
      disjoint <- stime[jclass] >= etime[iclass] |
                  etime[jclass] <= stime[iclass]  # consider implementing this check as two boolean outer products
      if(!disjoint){
        concurrence[[iclass]] <- c(concurrence[[iclass]], jclass)
      } else {
        break
      }
    }
  }
  ccgroups <- list()  # concurrence groups
  nextgroup <- 1
  group <- NULL
  for(iclass in 1:nclass){
    cc <- concurrence[[iclass]]  # the class indices which are concurrent with the ith class
    if(is.null(group)){
      group <- cc
    } else if (length(intersect(cc, group)) > 0) {
      group <- union(concurrence[[iclass]], group)
    } else {
      ccgroups[[nextgroup]] <- group
      group <- cc
      nextgroup <- nextgroup + 1
    }
  }
  # at this point, the ith element of ccgroups should be a vector of
  # indices of the courses that are in the ith concurrence group. Logic 
  # has an error in need of fixing.
  colors <- colormap(nshades=nclass, alpha=.7)
  grid.text("Course Times",
            x=.5, y=1.02,
            just=c("center", "bottom"))
  for(hourtime in mintime:maxtime){
    timestr <- paste(as.integer(hourtime), ":00", sep="")
    ypos <- 1 - (hourtime - mintime) / (maxtime - mintime)
    grid.text(timestr, x=-.05, y=ypos,
              just=c("right", "center"))
    grid.lines(x=c(0,1), y=c(ypos, ypos), gp=gpar(lty=2)
    )
  }
  for(iclass in 1:nclass){
    # whichgroup <- which(lapply(a, function(elem) length(which(elem==iclass)))==1)
    # groupmembers <- ccgroups[whichgroup]
    # ccgroupsize <- length(groupmembers)
    xpos <- (iclass - 1) / nclass
    ypos <- 1 - (stime[iclass] - mintime) / (maxtime - mintime)
    width <- 1 / nclass
    height <- (etime[iclass] - stime[iclass]) / (maxtime - mintime)
    grid.rect(x=xpos, y=ypos, 
              width=width, height=height,
              just=c("left", "top"), 
              gp=gpar(col="black",fill=colors[iclass]))
  }
}

plot.times <- function(fname){
  grid.newpage()
  grid.rect()
  pushViewport(viewport(width=0.2, height=0.9,
                        yscale=c(18, 8)))
  grid.rect()
  day.panel(stime=c(9, 10.25, 12.25, 14.5),
            etime=c(10.26, 11.3333, 13.5, 15.75),
            group=c(1,2,3,4))
  popViewport(1)
}

