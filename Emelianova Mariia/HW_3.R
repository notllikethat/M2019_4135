# FUNCTION

type_analysis <- function(column_to_check, calculation_function) {
  
  ifelse(is.numeric(column_to_check), 
         do.call(calculation_function, list(column_to_check)), 
         table(column_to_check))
}


analysis_function <- function(dat_fr, row_selection, column_selection, 
                              calculation_function, split_argument) {
  
  # SUBSETTING DATAFRAME
  subsetted_dat_fr <- dat_fr[row_selection, column_selection, drop = F]

  
  # CREATING A LIST FOR COMPUTATIONS & SUBSETTED DATAFRAME
  final_list <- list(subsetted_dat_fr)
  
  
  # PERFORMANCE OF COMPUTATIONS
  
  ## We need to apply our calculation function to all parts of subsetted data
  final_list <- list(c(final_list, 
                       lapply(split(subsetted_dat_fr, 
                                    as.factor(subsetted_dat_fr[ , split_argument]), 
                                    drop = T), 
                              function(x) {lapply(x, type_analysis, calculation_function)})))
  
  return(final_list)
}


# EXAMPLES

analysis_function(iris, c(1:100), c(2:5),  mean, 'Species')
analysis_function(airquality, c(93:153), c(1, 3, 4, 5), mean, 'Month')
analysis_function(mtcars, 1:25, c('disp', 'hp', 'carb', 'cyl'), sd, 'cyl')
