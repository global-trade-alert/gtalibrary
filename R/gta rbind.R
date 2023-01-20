# Roxygen documentation

#' Rbind two dataframes with different columns.
#'
#' This function rbinds two dataframes with a different set of columns and fills unmatched cells with NA.
#'
#' @param list Supply a list of dataframes. E.g. list = list(df1, df2, df3).
#'
#' @references www.globaltradealert.org
#' @author Global Trade Alert
#' @export
gta_rbind=function(list){

  diff = c()
  for (i in 1:length(list)) {
    diff <- append(diff, colnames(list[[i]]))
    diff <- unique(diff)
  }

  for (i in 1:length(list)) {
    cols <- colnames(list[[i]])
    temp.diff <- setdiff(diff, cols)
    list[[i]][, c(as.character(temp.diff))] <- NA
  }


  result <- as.data.frame(list[[1]])

  for (i in 2:length(list)){
    result <- rbind(result, as.data.frame(list[[i]]))
  }

  return(result)
}
