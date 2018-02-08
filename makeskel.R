# build the package skeleton
library(roxygen2)
code_file = "displaytimes.R"
source(code_file)
package.skeleton("CourseCollider", 
                 code_files = code_file, 
                 force = TRUE)
roxygenise()
