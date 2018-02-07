###########################################################################
# Author: Leila Bengali
# Date: 2/4/2018
# Description: this file has a program shell showing what options I think a
# flexible course display program should have
# Inputs: none
# Outputs: none 
###########################################################################




inner.panel <- function(stime, etime, durs, panelLabel, byBlock = TRUE, displayType, courseInfo) {
  
  # Description: this function creates rectangles in a viewport. Must be called 
  # from inside a viewport with natively set up y axis units on a 24 hour clock.
  # Inputs: 
  #     - a vector of class start times
  #     - a vector of class end times or durations
  #     - a label for the panel (text)
  #     - whether to display each course or each unique start-end pair
  #     - display type: as vertical lines or stacked rectangles?
  #     - a matrix or dataframe of course attributes (location, prof, name). This
  #       is optional, and if included the info in this matrix/ dataframe will
  #       be displayed for each course
  # Outputs: rectangles
  # Things to handle:
  #     - times in 12 or 24 hour clocks
  #     - end times or duration of the course (in min or hours)

  
  # Check the input:
  #   - start, end/duration, courseInfo the same length? numeric (times only)?
  #   - if using ends, are end times > start times?
  #   - Called from inside a viewport?
  #   - Native y axis units on a 24 hour or 12 hour clock?

  # Process the input
  # convert durations to end times
  # make unique start-end pairs if byBlock == TRUE 
  
  # Create the rectangles for each start-end pair
  # for each start-end pair or course {create visual display}
  
} 

outer.panel <- function(stime, etime, durs, innerGroup, courseInfo, title) {
  
  # Description: this function creates a course schedule - a one page pdf
  # Inputs: 
  #     - a vector of class start times by course-innerGroup for one
  #       outer group (e.g. so a MW [inner group] course in FES [outer group] 
  #       will have 2 rows: M FES, W FES)
  #     - a vector of class end times or durations by course-innerGroup for one
  #       outer group
  #     - a vector saying to which inner group the course belongs
  #     - a matrix or dataframe of course attributes (location, prof, name). This
  #       is optional 
  #     - a string title for the pdf, will default to the outer group name
  # Outputs: a pdf schedule
  # Things to handle:
  #     - times in 12 or 24 hour clocks
  #     - end times or duration of the course (in min or hours)
  
  
  # Set up the viewport
  # title, axis labels, open pdf
  
  # Create panels, one for each unique value of the inner group
  # for each unique value of the inner group {call inner.panel}
  # close pdf and export
  
}


main <- function(processData, dataset, title) {
  
  # Description: this function creates a course schedule for each of the outer
  # groups indicated (possibly more than one)
  # Inputs: 
  #     - a function that is specific to this dataset that will process the data
  #       so that it's in the format needed and outputs stime, etime, durs, 
  #       outerGroup, innerGroup, courseInfo in the format main() wants:
  #         - a vector of class start times by course-innerGroup-outerGroup (e.g. so
  #           a MW [inner group] course in FES and SOM [outer group] will have 4
  #           rows: M FES, M SOM, W FES, W SOM)
  #         - a vector of class end times or durations with the same structure as
  #           the start time vector
  #         - a vector saying to which outer group the course belongs (optional)
  #         - a vector saying to which inner group the course belongs
  #         - a matrix or dataframe of course attributes (location, prof, name). This
  #       is optional
  #     - a dataframe of course times, info etc
  #     - a string title for the pdf
  # Outputs: a pdf schedule
  # Things to handle:
  #     - times in 12 or 24 hour clocks
  #     - end times or duration of the course (in min or hours)
  
  
  # Set up and load the grid library
  
  
  # Create pdfs, one for each unique value of the outer group
  # for each unique value of the outer group {
  #   subset stime, etime, durs, innerGroup, courseInfo to the outer group in
  #   this loop
  #   call outer.panel
  # }
  
}







