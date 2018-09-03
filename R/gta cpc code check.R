gta_cpc_code_check=function(codes){
  code.correspondence=read.csv("R help files/cpc 2.1 to HS 2012.csv", sep=";")
  names(code.correspondence)=c("cpc.3digit", "hs.6digit") ## JF has some importing issue, thus the renaming here.

  ## NOTES
  # see gta_hs_code_check notes
  rm(codes)
}
