
library("tm")
library("stats")
library("httr")
library("data.table")
# library(factoextra)
library(tm)
library(tokenizers)
library(splitstackshape)
library(stringr)
# library(spacyr)
library(randomForest)
library(xgboost)
rm(list = ls())
load("content/0 core/leads core.Rdata")




## lead evaluations
GET("https://gta-backup.s3.eu-west-1.amazonaws.com/gta_leads.csv", write_disk("data/database replica/gta_leads.csv", overwrite=TRUE))
GET("https://gta-backup.s3.eu-west-1.amazonaws.com/gta_lead_jurisdiction.csv", write_disk("data/database replica/gta_lead_jurisdiction.csv", overwrite=TRUE))

leads=read.csv("data/database replica/gta_leads.csv")
setnames(leads, "bastiat_id", "bid")

trained=read.csv("data/training/bid training.csv", sep=";")
trained=merge(leads.core, trained, by="bid")

training=merge(leads.core, subset(leads, is_remove==1)[,c("bid","removal_reason")], by="bid")
training$evaluation=1
training$evaluation[training$removal_reason=="IRREVELANT"]=0
training$removal_reason=NULL

training=rbind(subset(training, !bid %in% trained$bid), trained)

training$text=training$act.title.en
training$text[is.na(training$act.description.en)==F]=paste(training$text[is.na(training$act.description.en)==F], training$act.description.en[is.na(training$act.description.en)==F], sep=" ")
training$text[is.na(training$act.values)==F]=paste(training$text[is.na(training$act.values)==F], training$act.values[is.na(training$act.values)==F], sep=" ")

training$has.value=as.numeric(is.na(training$act.values)==F)
training=training[,c("bid","acting.agency", "relevant", "evaluation", "text", "has.value")]
training$is.td=as.numeric(grepl("-[(TD)|(SG)|(AD)|(CVD)]+-",training$bid))
training$acting.agency=as.factor(training$acting.agency)


### Generate features:
## keyword vs delta.tfidf: keywords should by definition have strong d.tfidf values;
## They should only have  positive occurence in one of the two sets.
## create a "only in sample" variable


## simple dictionary

### TODOs
## (1) add all keywords
## (2) do the same for phrase
## (3) my.key vs other.key

keys=read.csv("data/training/keywords.csv", sep=";")
keys$source=as.character(keys$source)
keys$key=as.character(keys$key)

negative=unique(keys$key[keys$type=="negative"])
positive=unique(keys$key[keys$type=="positive"])


training$pos.word=0
training$pos.word.char=0

## y/n; chars
for(i in 1:length(positive)){
  word=positive[i]
  training$pos.word=training$pos.word+str_count(training$text, word)
  training$pos.word.char=training$pos.word.char+str_count(training$text, word)*nchar(word)
  print(i/length(positive))
}
training$pos.word.char=training$pos.word.char/nchar(enc2native(training$text))


training$neg.word=0
training$neg.word.char=0


for(i in 1:length(negative)){
  word=negative[i]
  training$neg.word=training$neg.word+str_count(training$text, word)
  training$neg.word.char=training$neg.word+str_count(training$text, word)*nchar(word)
  print(i/length(negative))
}
training$neg.word.char=training$neg.word.char/nchar(enc2native(training$text))

# training=subset(training, is.td==0)
save(training, file="data/classifier/training data.Rdata")
