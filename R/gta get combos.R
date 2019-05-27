# Roxygen documentation

#' Get all combinations of the elements in a vector.
#'
#' This function produces a list all combinations of the elements in a vector. You can choose the tuple size, too (i.e. how many of the original elements are counted as one combination).
#' Note that this function does not consider the order of the combinations i.e. the combo c("red","green) is the same as c("green","red"). This function will only return one of the two.
#'
#' @param recombine.me Supply a vector of any type. 
#' @param tuple.range Specify the range for the number of elements added into one combinations. Supply the minimum and maximum value of that range. Permissible values are integers plus "nr.elements". The latter is the number of unique values in the supplied vector. Default is c(1, nr.elements).
#'
#' @return Returns a list containing all combinations within the given range. Each item in the list contains one combintion.
#' @references www.globaltradealert.org
#' @author Global Trade Alert

gta_get_combos=function(recombine.me=NULL,
                        tuple.range=c(1,"nr.elements")){
  
  if(is.null(recombine.me)){
    stop("Please add a vector to generate the combinations out of (recombine.me).")
  }
  
  if(length(recombine.me)<2){
    stop("The supplied vector has less than two elements.")
  }
  
  recombine.me=list(recombine.me)
  
  ## setting the minimum number of elements inside a combination.
  combo.tuple.min=tuple.range[1]
  
  if(combo.tuple.min=="nr.elements"){
    combo.tuple.min=length(unique(recombine.me))
  } else {
    combo.tuple.min=as.numeric(as.character(combo.tuple.min))
  }
  
  if(is.na(combo.tuple.min)){
    stop("The minimum value for your range is neither an integer nor 'nr.elements'.")
  }
  
  ## setting the maximum number of elements inside a combination.
  combo.tuple.max=tuple.range[2]
  
  if(combo.tuple.max=="nr.elements"){
    combo.tuple.max=length(unique(recombine.me))
  }else {
    combo.tuple.max=as.numeric(as.character(combo.tuple.max))
  }
  
  if(is.na(combo.tuple.max)){
    stop("The maximum value for your range is neither an integer nor 'nr.elements'.")
  }
  
  
  combo.base=list(unique(recombine.me))
  
  combos=list()
  for(i in combo.tuple.min:combo.tuple.max){
    
    combo.cols=as.data.frame(combn(combo.base[[1]],i))
    
    for(j in 1:ncol(combo.cols)){
      combos=append(combos,
                         list(paste(combo.cols[,j])))
    }
  }
  
  return(combos)
  
  
  
}
