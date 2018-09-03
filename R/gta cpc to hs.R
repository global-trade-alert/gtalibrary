gta_cpc_to_hs=function(codes){
  code.correspondence=read.csv("R help files/cpc 2.1 to HS 2012.csv", sep=";")
  names(code.correspondence)=c("cpc.3digit", "hs.6digit") ## JF has some importing issue, thus the renaming here.

  codes=gta_cpc_code_check(codes)
  codes=code.correspondence$hs.6digit[code.correspondence$cpc.3digit %in% codes]
  return(codes)
  rm(codes, code.correspondence)
}
