#' print_hej
#' Example of doc string
#'
#'@return Nothing at all.
#'@export

print_hej <- function(){
	print("hej")
}


#' bin_by_rt
#' Adds retention time bins with width of the passed WIDTH_MIN to the DIA-NN report. 
#'
#'@return Dataframe containing an added column with created retention time bins
#'@export

bin_by_rt <- function(df, WIDTH_MIN = 1){
  
  return(
    
    df %>%
      mutate(
        RT_Bin = cut_width(
          RT, width=WIDTH_MIN, closed = "right",
          ordered_result=T, labels = F
        )
      )
    
  )
  
}

