## Create necessary dataframes for function

# load csv
cpc.names <- read.csv("../R help files/cpc names.csv", sep=";")

hs.names <- read.csv("../R help files/hs2012 names.csv", sep=";")

cpc.to.hs <- read.csv("../R help files/cpc 2.1 to HS 2012.csv", sep=";")
colnames(cpc.to.hs) <- c("cpc","hs")

# Saving datasets in the a sysdata.R file in the R folder. This way it will be only available inside the package.
# Call the internally saved packages from inside the function like this: gtalibrary:::name_of_dataframe
devtools::use_data(cpc.names, hs.names, cpc.to.hs, internal = T, overwrite = T)
