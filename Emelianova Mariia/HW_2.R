# FUNCTION

type_analysis <- function(column_to_check, calculation_function) {
  
  ifelse(is.numeric(column_to_check),
         
         return(list("Sum" = sum(column_to_check, na.rm = T), 
                     "Mean value" = mean(column_to_check, na.rm = T), 
                     "Standard deviation" = sd(column_to_check, na.rm = T))),
         
         return(list("Frequency table" = table(column_to_check))))
}


analysis_function <- function(dat_fr, row_selection, column_selection) {
  
  # SUBSETTING DATAFRAME
  
  subsetted_dat_fr <- dat_fr[row_selection, column_selection, drop = FALSE]
  
  
  # CREATING A LIST FOR COMPUTATIONS & SUBSETTED DATAFRAME
  
  final_list <- list(subsetted_dat_fr)
  
  
  # PERFORMANCE OF COMPUTATIONS
  
  for (i in (1:ncol(subsetted_dat_fr))) {
    
    final_list[[length(final_list) + 1]] <- type_analysis(subsetted_dat_fr[ , i])
  }
  
  return(final_list)
}


# EXAMPLES

analysis_function(iris, c(1:100), c(2, 3, 4, 5))
analysis_function(airquality, c(93:153), c(1, 3, 4, 5))
analysis_function(mtcars, 1:25, c('disp', 'hp', 'carb', 'cyl'))
