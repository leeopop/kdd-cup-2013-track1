memory.limit(100000)
library(rjson)
#library(RPostgreSQL)
library(data.table)
library(hexbin)
library(gbm)
library(tm)
stopwords_web <- c('www','html','com','org','journal','journals','index','openurl','url','htm','shtml','php')

#############################################################
# build submission array
#############################################################
fn.print.map.err <- function(actual, pred, do.print = T) { 
  actual <- data.table(actual)
  pred <- data.table(pred)
  actual.pred <- merge(actual[,list(authorid,paperid,target)],pred[,list(authorid,paperid,pred)],by=c("authorid","paperid"))
  actual.pred <- data.frame(actual.pred)
  authors.unique <- unique(actual.pred$authorid)
  actual.list <- list()
  predicted.list <- list()
  apks <- c()
  for (j in 1:length(authors.unique)) {
    author <- authors.unique[j]
    ix <- which(actual.pred$authorid==author)
    actual.j <- as.numeric(actual.pred$paperid[which(actual.pred$authorid==author & actual.pred$target==1)])
    predicted.j <- as.numeric(actual.pred$paperid[ix][sort(actual.pred$pred[ix],decreasing=TRUE,index.return=TRUE)$ix])
    actual.list[[j]] <- actual.j
    predicted.list[[j]] <- predicted.j
    apks <- c(apks,apk_m(actual.j,predicted.j))
  }
  df <- data.frame(Length = nrow(actual.pred),
                   MAPerror = mapk_m(actual.list,predicted.list))
  
  if (do.print) {
    print (df)
  }
  
  invisible(df)
}

apk_m <- function(actual, predicted) {
  score <- 0.0
  cnt <- 0.0
  for (i in 1:length(predicted)) {
    if (predicted[i] %in% actual && !(predicted[i] %in% predicted[0:(i-1)])) {
      cnt <- cnt + 1
      score <- score + cnt/i
    }
  }
  score <- score / length(actual)
  return (score)
}

mapk_m <- function (actual, predicted) {
  scores <- rep(0, length(actual))
  for (i in 1:length(scores)){
    scores[i] <- apk_m(actual[[i]], predicted[[i]])
  }
  score <- mean(scores)
  return (score)
}

cleanTextField <- function(field) {
  field1 <- tolower(gsub("[^A-Za-z0-9 _]"," ",field))
  field1 <- gsub(" . |^. | .$"," ",field1)
  field1 <- gsub("^ +","",field1)
  field1 <- gsub(" +$","",field1)
  field1 <- gsub(" +"," ",field1)
  return (field1)
}

cleanWebField <- function(field) {
  field1 <- tolower(gsub("http://","",field))
  field1 <- gsub("/"," ",field1)
  field1 <- gsub("\\."," ",field1)
  field1 <- gsub(" . |^. | .$"," ",field1)
  field1 <- gsub("^ +","",field1)
  field1 <- gsub(" +$","",field1)
  field1 <- gsub(" +"," ",field1)
  return (field1)
}

chooseTextField <- function(field) {
  if (length(field)==1) return (field[1])
  ix <- which(nchar(field)>0)
  if (length(ix) == 0) {
    return ("")
  } else {
    return (field[ix][which.min(nchar(field[ix]))[1]])
  }
}

checkEquals <- function(field1,field2) {
  field1 <- field1[!is.na(field1)]
  field1 <- field1[which(nchar(field1)>0)]
  field2 <- field2[!is.na(field2)]
  field2 <- field2[which(nchar(field2)>0)]
  if (length(intersect(field1,field2))>0) return (1)
  return (0)
}


yearFeature <- function(years) {
  years_sorted <- unique(sort(years))
  years_codes <- unlist(sapply(years,function(x) which(years_sorted==x)))-1
  return (years_codes)
}

distCenter <- function(fea) {
  sds <- apply(fea,2,sd)
  ix <- which(sds!=0)
  fea_scaled <- scale(fea[,ix])
  center <- matrix(rep(colMeans(fea_scaled), nrow(fea_scaled)), byrow=T, ncol=ncol(fea_scaled))
  return (sqrt(rowSums((fea_scaled-center)^2)))
}

export2libfm <- function(df,targ,filename) {
  if (nchar(targ) > 0) {
    df_libfm <- data.frame(target=df[,targ])
  } else {
    df_libfm <- data.frame(target=rep(-1,nrow(df)))
  }
  cols <- setdiff(colnames(df),targ)
  for (i in 1:length(cols)) {
    df_libfm <- cbind(df_libfm, paste(i,":",format(df[,cols[i]],trim=TRUE,scientific=FALSE),sep=""))
  }
  #if (nchar(targ) > 0) {
    write.table(df_libfm,file=filename,quote=FALSE,sep=" ",row.names=FALSE,col.names=FALSE)
  #} else {
  #write.table(df_libfm[,-1],file=filename,quote=FALSE,sep=" ",row.names=FALSE,col.names=FALSE)
  #}
}

export2ranklib <- function(df,targ,groupname,filename) {
  if (nchar(targ) > 0) {
    df_ranklib <- data.frame(target=df[,targ])
  } else {
    df_ranklib <- data.frame(target=rep(-1,nrow(df)))
  }
  df_ranklib[,'group'] <- paste("qid:",format(df[,groupname],trim=TRUE,scientific=FALSE),sep="")

  cols <- setdiff(colnames(df),c(targ,groupname))
  for (i in 1:length(cols)) {
    df_ranklib <- cbind(df_ranklib, paste(i,":",format(df[,cols[i]],trim=TRUE,scientific=FALSE),sep=""))
  }
  write.table(df_ranklib,file=filename,quote=FALSE,sep=" ",row.names=FALSE,col.names=FALSE)
}
