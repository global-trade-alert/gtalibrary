gta_hs_code_check=function(codes){
  code.correspondence=read.csv("R help files/cpc 2.1 to HS 2012.csv", sep=";")
  names(code.correspondence)=c("cpc.3digit", "hs.6digit") ## JF has some importing issue, thus the renaming here.

  ## NOTES:
  # check whether there are zeros at the front
  # check whether you can find all codes in the first place; provide error message for those you cannot find.
  # cut 6+ digits
  rm(codes)
}
