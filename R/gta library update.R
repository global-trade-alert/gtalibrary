gta_library_update = function(x){
  devtools::install_github("global-trade-alert/gtalibrary", force=T)
  library("gtalibrary")
  print("You are up to date.")
}
