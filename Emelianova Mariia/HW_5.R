# FUNCTION

library(dplyr)

type_analysis <- function(column_to_check, calculation_function) {
  
  ifelse(is.numeric(column_to_check), 
         do.call(calculation_function, list(column_to_check)), 
         table(column_to_check))
}


analysis_function <- function(dat_fr, row_selection, column_selection, 
                          calculation_function, split_argument) {
    
  # SUBSETTING DATAFRAME
  
  dat_fr %>%
    select(column_selection) %>%
    slice(row_selection) -> subsetted_dat_fr
  
  
  # CREATING A LIST FOR COMPUTATIONS & SUBSETTED DATAFRAME
  
  subsetted_dat_fr %>%
    list() -> final_list  # :)
  
  
  # PERFORMANCE OF COMPUTATIONS
  
  subsetted_dat_fr %>%
    split(as.factor(subsetted_dat_fr[ , split_argument]), drop = T) %>%
    lapply(function(x) {lapply(x, type_analysis, calculation_function)}) %>%
    c(final_list, .) -> final_list
  
  return(final_list)
}


# EXAMPLES

analysis_function(iris, c(1:100), c(2, 3, 4, 5),  mean, 'Species')
analysis_function(airquality, c(93:153), c(1, 3, 4, 5), mean, 'Month')
analysis_function(mtcars, 1:25, c('disp', 'hp', 'carb', 'cyl'), sd, 'cyl')
