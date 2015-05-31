#rm(list = ls())

source("fn.base.R")

##############################################################
## load data
##############################################################
library("data.table")

tic()
cat("Loading csv data... ")

#data.author <- fn.read.input.csv("Author.csv", key = "id")
data.author <- fn.read.input.csv("Author.csv", key=c("id"))#data.table(dbGetQuery(con, statement = "SELECT * FROM Author"),key=c("id"))
setnames(data.author, c("authorid", "a_name", "a_affiliation"))

#data.conference <- fn.read.input.csv("Conference.csv", key = "id")
data.conference <- fn.read.input.csv("Conference.csv", key=c("id"))#data.table(dbGetQuery(con, statement = "SELECT * FROM Conference"),key=c("id"))
setnames(data.conference, c("conferenceid", "conf_shortname", 
                        "conf_fullname", "conf_homepage"))
data.conference$conf_hpdomain <- 
  gsub("^http://([^/]+).*$", "\\1", data.conference$conf_homepage)

#data.journal <- fn.read.input.csv("Journal.csv", key = "id")
data.journal <- fn.read.input.csv("Journal.csv",key=c("id"))#data.table(dbGetQuery(con, statement = "SELECT * FROM Journal"),key=c("id"))
setnames(data.journal, c("journalid", "j_shortname", 
                        "j_fullname", "j_homepage"))
data.journal$j_hpdomain <- 
  gsub("^http://([^/]+).*$", "\\1", data.journal$j_homepage)

data.train.ids <- fn.read.input.csv("Train.csv")
data.train.confirm <- data.train.ids[
  ,list(paperid = as.integer(unlist(strsplit(confirmedpaperids, split = "\\s+"))),
        confirmed = 1),
  by="authorid"]
data.train.delete <- data.train.ids[
  ,list(paperid = as.integer(unlist(strsplit(deletedpaperids, split = "\\s+"))),
        confirmed = 0),
  by="authorid"]
data.train.ids <- rbind(data.train.confirm,
                        data.train.delete)

data.train.ids.feat <- fn.build.load.feat(data.train.ids)

data.train.ids.map <- data.table(data.train.ids)
data.train.ids.map <- data.train.ids.map[
  data.train.ids.map$confirmed == 1,]
setkeyv(data.train.ids.map, c("authorid", "paperid"))

data.train.ids <- unique(data.train.ids)
setkeyv(data.train.ids, c("authorid", "paperid"))
data.train.ids <- unique(data.train.ids)

data.valid.ids.map <- fn.read.input.csv("ValidSolution.csv")
data.valid.ids.map <- data.valid.ids.map[
  ,list(paperid = as.integer(unlist(strsplit(paperids, split = "\\s+"))),
        confirmed = 1),
  by="authorid"]
setkeyv(data.valid.ids.map, c("authorid", "paperid"))

data.valid.ids <- fn.read.input.csv("Valid.csv")
data.valid.ids <- data.valid.ids[
  ,list(
    paperid = as.integer(unlist(strsplit(paperids, split = "\\s+")))),
  by="authorid"]

data.valid.ids.feat <- fn.build.load.feat(data.valid.ids)

data.valid.ids <- unique(data.valid.ids)
setkeyv(data.valid.ids, c("authorid", "paperid"))

data.test.ids <- fn.read.input.csv("Valid.csv")
data.test.ids <- data.test.ids[
  ,list(paperid = as.integer(unlist(strsplit(paperids, split = "\\s+")))),
  by="authorid"]
data.test.ids.feat <- fn.build.load.feat(data.test.ids)

data.test.ids <- unique(data.test.ids)
setkeyv(data.test.ids, c("authorid", "paperid"))

#data.paper <- fn.read.input.csv("Paper.csv", key = "id")
data.paper <- fn.read.input.csv("Paper.csv", key="id")#data.table(dbGetQuery(con, statement = "SELECT * FROM Paper"),key="id")
setnames(data.paper, c(
  "paperid", "p_title", "p_year", "conferenceid", "journalid", "p_keyword"))


#data.paper.author <- fn.read.input.csv("PaperAuthor.csv", 
#                                       key = c("paperid","authorid"))
#data.paper.author <- data.frame(data.paper.author)

data.paper.author <- data.frame(read.csv("PaperAuthor.csv"))#data.frame(dbGetQuery(con, statement = "SELECT * FROM PaperAuthor"))
setnames(data.paper.author, c(
  "paperid", "authorid", "pa_name", "pa_affiliation"))
data.paper.author <- data.paper.author[,c(
  "authorid", "paperid", "pa_name", "pa_affiliation")]
data.paper.author <- data.table(data.paper.author, 
                                key = c("authorid", "paperid"))

data.paper.author.clean <- fn.clean.paper.author(data.paper.author)

fn.save.data("data.author")
fn.save.data("data.conference")
fn.save.data("data.journal")
fn.save.data("data.paper")
fn.save.data("data.paper.author")
fn.save.data("data.paper.author.clean")

fn.save.data("data.train.ids")
fn.save.data("data.train.ids.map")
fn.save.data("data.train.ids.feat")

fn.save.data("data.valid.ids")
fn.save.data("data.valid.ids.map")
fn.save.data("data.valid.ids.feat")

fn.save.data("data.test.ids")
fn.save.data("data.test.ids.feat")

cat("done \n")
toc()

##############################################################
## loading author duplicates
##############################################################
source("fn.base.R")
library("data.table")
library("stringr")

tic()
cat("Building duplication stats... ")

data.author.dup <- fn.read.input.csv("Track2_Dup.csv")
data.author.dup <- unique(data.author.dup[
  ,list(dupauthorids = as.integer(unlist(strsplit(duplicateauthorids, split = "\\s+")))),
  by="authorid"])
data.author.dup <- data.author.dup[
  data.author.dup$authorid != data.author.dup$dupauthorids,]

data.author.dup.greedy <- fn.read.input.csv("Track2_Dup_Greedy.csv")
data.author.dup.greedy <- unique(data.author.dup.greedy[
  ,list(dupauthorids = as.integer(unlist(strsplit(duplicateauthorids, split = "\\s+")))),
  by="authorid"])
data.author.dup.greedy <- data.author.dup.greedy[
  data.author.dup.greedy$authorid != data.author.dup.greedy$dupauthorids,]

data.author.dup.fake <- data.table(data.author.dup,
                                   key = c("authorid", "dupauthorids"))
data.author.dup.fake$dummy <- 1

data.author.dup.fake <- fn.join(data.author.dup.greedy, data.author.dup.fake)
data.author.dup.fake <- data.author.dup.fake[is.na(data.author.dup.fake$dummy),]
data.author.dup.fake$dummy <- NULL

setkeyv(data.author.dup, "authorid")
setkeyv(data.author.dup.fake, "authorid")

fn.save.data("data.author.dup")
fn.save.data("data.author.dup.fake")

cat("done \n")
toc()

##############################################################
## cross validation indexes
##############################################################

source("fn.base.R")
fn.load.data("data.train.ids")

tic()
cat("Building lucas cv... ")


data.cv.folds <- fn.build.cv(data.train.ids)
# Instance CV distribution: 
# 
#     1     2     3     4     5 
# 49303 47194 42476 47258 45102 
# Author CV distribution: 
# 
#   1   2   3   4   5 
# 748 748 748 748 747 
fn.save.data("data.cv.folds")

data.cv.folds10 <- fn.build.cv(data.train.ids, K=10)
# Instance CV distribution: 
# 
#     1     2     3     4     5     6     7     8     9    10 
# 27801 24265 21286 22020 20775 21502 22929 21190 25238 24327 
# Author CV distribution: 
# 
#   1   2   3   4   5   6   7   8   9  10 
# 374 374 374 374 374 374 374 374 374 373
fn.save.data("data.cv.folds10")

cat("done \n")
toc()

##############################################################
## build likelihood features
##############################################################
source("fn.base.R")
library("data.table")

tic()
cat("creating likelihood features... ")

fn.load.data("data.author")
fn.load.data("data.conference")
fn.load.data("data.journal")
fn.load.data("data.paper")
fn.load.data("data.train.ids")
fn.load.data("data.valid.ids")
fn.load.data("data.test.ids")
fn.load.data("data.train.ids.map")
fn.load.data("data.valid.ids.map")
fn.load.data("data.paper.author.clean")
data.paper.author <- data.paper.author.clean

data.cv.folds.like.tr <- fn.build.cv(data.train.ids, K = 100)
data.cv.folds.like.tr.year <- fn.build.cv(data.train.ids, K = 20)
data.cv.folds.like.test <- fn.build.cv(data.train.ids, K = 10)

col.key <- c("authorid", "paperid")
data.feat.likelihood.dt <- data.table(
  rbind(data.train.ids, data.frame(data.test.ids, confirmed = NA)),
  key = col.key)

data.feat.likelihood.stats.dt <- fn.join(data.feat.likelihood.dt, data.paper)
data.feat.likelihood.stats.dt <- fn.join(data.feat.likelihood.stats.dt, data.conference)
data.feat.likelihood.stats.dt <- fn.join(data.feat.likelihood.stats.dt, data.journal)
data.feat.likelihood.stats.dt <- fn.join(data.feat.likelihood.stats.dt, data.author)
data.feat.likelihood.stats.dt <- fn.join(data.feat.likelihood.stats.dt, data.paper.author)

if (!is.null(data.feat.likelihood.dt$confirmed)) {
  data.feat.likelihood.dt$confirmed  <- NULL
}

# fn.load.data("data.feat.likelihood.dt")

# data.feat.likelihood.stats.dt$dummy <- 1
# feats.dummy <- fn.build.confirm.stats(data.cv.folds.like, 
#                                     data.feat.likelihood.stats.dt, "dummy")
# feats.dummy <- fn.log.likelihood(feats.dummy)
# fn.log.likelihood.print(data.train.ids.map, feats.dummy)
# #   size       map
# # 1 3739 0.6747384
# fn.log.likelihood.print(data.valid.ids.map, feats.dummy)
# #   size       map
# # 1 1496 0.6843276

feats.jid.tr <- fn.build.confirm.stats(
  data.cv.folds.like.tr, 
  data.feat.likelihood.stats.dt, "journalid")
feats.jid.tr <- fn.log.likelihood(feats.jid.tr, "tr")
fn.log.likelihood.print(data.train.ids.map, feats.jid.tr)
#   size       map
# 1 3739 0.7611996
# data.cv.folds.like.test <- data.cv.folds.like.tr
feats.jid.test <- fn.build.confirm.stats(
  data.cv.folds.like.test, 
  data.feat.likelihood.stats.dt, "journalid")
feats.jid.test <- fn.log.likelihood(feats.jid.test, "test")
fn.log.likelihood.print(data.valid.ids.map, feats.jid.test)
#   size       map
# 1 1496 0.7587642
data.feat.likelihood.dt <- fn.join2(data.feat.likelihood.dt, 
                                    fn.rbind(feats.jid.tr,feats.jid.test))

feats.confid.tr <- fn.build.confirm.stats(
  data.cv.folds.like.tr, 
  data.feat.likelihood.stats.dt, "conferenceid")
feats.confid.tr <- fn.log.likelihood(feats.confid.tr, "tr")
fn.log.likelihood.print(data.train.ids.map, feats.confid.tr)
#   size       map
# 1 3739 0.7353445 0.7362321
# data.cv.folds.like.test <- data.cv.folds.like.tr.conf
feats.confid.test <- fn.build.confirm.stats(
  data.cv.folds.like.test, 
  data.feat.likelihood.stats.dt, "conferenceid")
feats.confid.test <- fn.log.likelihood(feats.confid.test, "test")
fn.log.likelihood.print(data.valid.ids.map, feats.confid.test)
#   size      map
# 1 1496 0.738077
data.feat.likelihood.dt <- fn.join2(data.feat.likelihood.dt, 
                                    fn.rbind(feats.confid.tr,feats.confid.test))


feats.paperid.tr <- fn.build.confirm.stats(
  data.cv.folds.like.tr, 
  data.feat.likelihood.stats.dt, "paperid")
feats.paperid.tr <- fn.log.likelihood(feats.paperid.tr, "tr")
fn.log.likelihood.print(data.train.ids.map, feats.paperid.tr)
#   size       map
# 1 3739 0.6980741
# data.cv.folds.like.test <- data.cv.folds.like.tr
feats.paperid.test <- fn.build.confirm.stats(
  data.cv.folds.like.test, 
  data.feat.likelihood.stats.dt, "paperid")
feats.paperid.test <- fn.log.likelihood(feats.paperid.test, "test")
fn.log.likelihood.print(data.valid.ids.map, feats.paperid.test)
#   size       map
# 1 1496 0.7035882
data.feat.likelihood.dt <- fn.join2(data.feat.likelihood.dt, 
                                    fn.rbind(feats.paperid.tr,feats.paperid.test))

library("stringr")
data.feat.likelihood.stats.dt$p_title <- 
  str_trim(tolower(data.feat.likelihood.stats.dt$p_title))
data.feat.likelihood.stats.dt$p_title <- 
  gsub("[^a-z]", " ", data.feat.likelihood.stats.dt$p_title)
data.feat.likelihood.stats.dt$p_title <- 
  gsub("\\s+", " ", data.feat.likelihood.stats.dt$p_title)
data.feat.likelihood.stats.dt$p_title <- 
  str_trim(data.feat.likelihood.stats.dt$p_title)
small.title <- nchar(data.feat.likelihood.stats.dt$p_title) < 10
data.feat.likelihood.stats.dt$p_title[small.title] <-
  paste0("_small_", which(small.title))
feats.title.tr <- fn.build.confirm.stats(
  data.cv.folds.like.tr, 
  data.feat.likelihood.stats.dt, "p_title")
feats.title.tr <- fn.log.likelihood(feats.title.tr, "tr")
fn.log.likelihood.print(data.train.ids.map, feats.title.tr)
#   size       map
# 1 3739 0.6975573
# data.cv.folds.like.test <- data.cv.folds.like.tr
feats.title.test <- fn.build.confirm.stats(
  data.cv.folds.like.test, 
  data.feat.likelihood.stats.dt, "p_title")
feats.title.test <- fn.log.likelihood(feats.title.test, "test")
fn.log.likelihood.print(data.valid.ids.map, feats.title.test)
#   size       map
# 1 1496 0.7030586
data.feat.likelihood.dt <- fn.join2(data.feat.likelihood.dt, 
                                    fn.rbind(feats.title.tr,feats.title.test))

feats.year.tr <- fn.build.confirm.stats(
  data.cv.folds.like.tr.year, 
  data.feat.likelihood.stats.dt, "p_year")
feats.year.tr <- fn.log.likelihood(feats.year.tr, "tr")
fn.log.likelihood.print(data.train.ids.map, feats.year.tr)
#   size       map
# 1 3739 0.7513211
# data.cv.folds.like.test <- data.cv.folds.like.tr.year
feats.year.test <- fn.build.confirm.stats(
  data.cv.folds.like.test, 
  data.feat.likelihood.stats.dt, "p_year")
feats.year.test <- fn.log.likelihood(feats.year.test, "test")
fn.log.likelihood.print(data.valid.ids.map, feats.year.test)
#   size       map
# 1 1496 0.7580354
data.feat.likelihood.dt <- fn.join2(data.feat.likelihood.dt, 
                                    fn.rbind(feats.year.tr,feats.year.test))

feats.paff <- fn.build.confirm.stats(
  data.cv.folds.like.test, 
  data.feat.likelihood.stats.dt, "pa_affiliation")
feats.paff <- fn.log.likelihood(feats.paff)
fn.log.likelihood.print(data.train.ids.map, feats.paff)
#   size       map
# 1 3739 0.6990365
# data.cv.folds.like.test <- data.cv.folds.like.tr.paff
fn.log.likelihood.print(data.valid.ids.map, feats.paff)
#   size       map
# 1 1496 0.7091226
data.feat.likelihood.dt <- fn.join2(data.feat.likelihood.dt, 
                                    feats.paff)

# feats.paff <- fn.build.confirm.stats(
#   data.cv.folds.like.test, 
#   data.feat.likelihood.stats.dt, "pa_name")
# feats.paff <- fn.log.likelihood(feats.paff)
# fn.log.likelihood.print(data.train.ids.map, feats.paff)
# #   size       map
# # 1 3739 0.6990365
# # data.cv.folds.like.test <- data.cv.folds.like.tr.paff
# fn.log.likelihood.print(data.valid.ids.map, feats.paff)
# #   size       map
# # 1 1496 0.7091226
# data.feat.likelihood.dt <- fn.join2(data.feat.likelihood.dt, 
#                                     feats.paff)

fn.save.data("data.feat.likelihood.dt")

cat("done \n")
toc()


# ##############################################################
# ## build likelihood features
# ##############################################################
# source("fn.base.R")
# library("data.table")
# 
# tic()
# cat("creating likelihood features... ")
# 
# fn.load.data("data.author")
# fn.load.data("data.conference")
# fn.load.data("data.journal")
# fn.load.data("data.paper")
# fn.load.data("data.train.ids")
# fn.load.data("data.valid.ids")
# fn.load.data("data.test.ids")
# fn.load.data("data.train.ids.map")
# fn.load.data("data.valid.ids.map")
# fn.load.data("data.paper.author.clean")
# data.paper.author <- data.paper.author.clean
# 
# data.cv.folds.like <- fn.build.cv(data.train.ids, K = 10)
# 
# col.key <- c("authorid", "paperid")
# data.feat.likelihood.dt <- data.table(
#   rbind(data.train.ids, data.frame(data.test.ids, confirmed = NA)),
#   key = col.key)
# 
# data.feat.likelihood.stats.dt <- fn.join(data.feat.likelihood.dt, data.paper)
# data.feat.likelihood.stats.dt <- fn.join(data.feat.likelihood.stats.dt, data.conference)
# data.feat.likelihood.stats.dt <- fn.join(data.feat.likelihood.stats.dt, data.journal)
# data.feat.likelihood.stats.dt <- fn.join(data.feat.likelihood.stats.dt, data.author)
# data.feat.likelihood.stats.dt <- fn.join(data.feat.likelihood.stats.dt, data.paper.author)
# 
# if (!is.null(data.feat.likelihood.dt$confirmed)) {
#   data.feat.likelihood.dt$confirmed  <- NULL
# }
# 
# # fn.load.data("data.feat.likelihood.dt")
# 
# # data.feat.likelihood.stats.dt$dummy <- 1
# # feats.dummy <- fn.build.confirm.stats(data.cv.folds.like, 
# #                                     data.feat.likelihood.stats.dt, "dummy")
# # feats.dummy <- fn.log.likelihood(feats.dummy)
# # fn.log.likelihood.print(data.train.ids.map, feats.dummy)
# # #   size       map
# # # 1 3739 0.6747384
# # fn.log.likelihood.print(data.valid.ids.map, feats.dummy)
# # #   size       map
# # # 1 1496 0.6843276
# 
# feats.jid <- fn.build.confirm.stats(
#   data.cv.folds.like, 
#   data.feat.likelihood.stats.dt, "journalid")
# feats.jid <- fn.log.likelihood(feats.jid)
# fn.log.likelihood.print(data.train.ids.map, feats.jid)
# #   size       map
# # 1 3739 0.7609984
# fn.log.likelihood.print(data.valid.ids.map, feats.jid)
# #   size      map
# # 1 1496 0.7588719
# data.feat.likelihood.dt <- fn.join2(data.feat.likelihood.dt, feats.jid)
# 
# feats.confid <- fn.build.confirm.stats(
#   data.cv.folds.like, 
#   data.feat.likelihood.stats.dt, "conferenceid")
# feats.confid <- fn.log.likelihood(feats.confid)
# fn.log.likelihood.print(data.train.ids.map, feats.confid)
# #   size       map
# # 1 3739 0.7340335
# fn.log.likelihood.print(data.valid.ids.map, feats.confid)
# #   size       map
# # 1 1496 0.7387446
# data.feat.likelihood.dt <- fn.join2(data.feat.likelihood.dt, feats.confid)
# 
# feats.paperid <- fn.build.confirm.stats(
#   data.cv.folds.like, 
#   data.feat.likelihood.stats.dt, "paperid")
# feats.paperid <- fn.log.likelihood(feats.paperid)
# fn.log.likelihood.print(data.train.ids.map, feats.paperid)
# #   size       map
# # 1 3739 0.6979986
# 
# fn.log.likelihood.print(data.valid.ids.map, feats.paperid)
# #    size      map
# # 1 1496 0.7035882
# data.feat.likelihood.dt <- fn.join2(data.feat.likelihood.dt, feats.paperid)
# 
# library("stringr")
# data.feat.likelihood.stats.dt$p_title <- 
#   str_trim(tolower(data.feat.likelihood.stats.dt$p_title))
# data.feat.likelihood.stats.dt$p_title <- 
#   gsub("[^a-z]", " ", data.feat.likelihood.stats.dt$p_title)
# data.feat.likelihood.stats.dt$p_title <- 
#   gsub("\\s+", " ", data.feat.likelihood.stats.dt$p_title)
# data.feat.likelihood.stats.dt$p_title <- 
#   str_trim(data.feat.likelihood.stats.dt$p_title)
# small.title <- nchar(data.feat.likelihood.stats.dt$p_title) < 10
# data.feat.likelihood.stats.dt$p_title[small.title] <-
#   paste0("_small_", which(small.title))
# 
# feats.title <- fn.build.confirm.stats(
#   data.cv.folds.like, 
#   data.feat.likelihood.stats.dt, "p_title")
# feats.title <- fn.log.likelihood(feats.title)
# fn.log.likelihood.print(data.train.ids.map, feats.title)
# #   size       map
# # 1 3739 0.6972487
# fn.log.likelihood.print(data.valid.ids.map, feats.title)
# #   size      map
# # 1 1496 0.7030621
# data.feat.likelihood.dt <- fn.join2(data.feat.likelihood.dt, feats.title)
# 
# feats.year <- fn.build.confirm.stats(
#   data.cv.folds.like, 
#   data.feat.likelihood.stats.dt, "p_year",
#   fn.weight = function (x) log(x+1))
# feats.year <- fn.log.likelihood(feats.year)
# fn.log.likelihood.print(data.train.ids.map, feats.year)
# #   size       map
# # 1 3739 0.7513211 # 0.6823192
# fn.log.likelihood.print(data.valid.ids.map, feats.year)
# #   size       map
# # 1 1496 0.7580893 # 0.6911853
# data.feat.likelihood.dt <- fn.join2(data.feat.likelihood.dt, feats.year)
# 
# feats.paff <- fn.build.confirm.stats(
#   data.cv.folds.like, 
#   data.feat.likelihood.stats.dt, "pa_affiliation")
# feats.paff <- fn.log.likelihood(feats.paff)
# fn.log.likelihood.print(data.train.ids.map, feats.paff)
# #   size       map
# # 1 3739 0.6990365
# # data.cv.folds.like.test <- data.cv.folds.like.tr.paff
# fn.log.likelihood.print(data.valid.ids.map, feats.paff)
# #   size       map
# # 1 1496 0.7091226
# data.feat.likelihood.dt <- fn.join2(data.feat.likelihood.dt, 
#                                     feats.paff)
# 
# fn.save.data("data.feat.likelihood.dt")
# 
# cat("done \n")
# toc()

##############################################################
## source related
##############################################################
source("fn.base.R")
library("data.table")

tic()
cat("creating source related features... ")

fn.load.data("data.cv.folds")
fn.load.data("data.author")
fn.load.data("data.conference")
fn.load.data("data.journal")
fn.load.data("data.paper")
fn.load.data("data.train.ids")
fn.load.data("data.test.ids")
fn.load.data("data.train.ids.feat")
fn.load.data("data.test.ids.feat")
fn.load.data("data.paper.author.clean")
fn.load.data("data.paper.author")
fn.load.data("data.author.dup.fake")
fn.load.data("data.author.dup")
# fn.load.data("data.author.dup.greedy")

col.key <- c("authorid", "paperid")

data.ids.feat <- rbind(data.train.ids.feat,
                       data.test.ids.feat)
setkeyv(data.ids.feat, col.key)

library("stringr")
data.paper.rep <- data.table(data.paper)
data.paper.rep$p_title <-str_trim(tolower(data.paper.rep$p_title))
data.paper.rep$p_title <- gsub("[^a-z0-9]", " ", data.paper.rep$p_title)
data.paper.rep$p_title <- gsub("\\s+", " ", data.paper.rep$p_title)

small.title <- nchar(data.paper.rep$p_title) < 5
data.paper.rep$p_title[small.title] <- 
  paste("empty", data.paper.rep$paperid[small.title],
        which(small.title))

        
data.paper.rep <- data.paper.rep[
  , list(
    paperid = paperid,
    apf_dupsourcenomatch = length(unique(paperid))-1,
    pf_paperrepid = max(paperid, na.rm = T)
         ), by=c("p_title")]
data.paper.rep$p_title <- NULL
setkeyv(data.paper.rep, "paperid")
data.paper.rep <- fn.join(data.paper.author.clean, data.paper.rep)
data.paper.rep <- data.paper.rep[
  ,list(
    authorid = unique(authorid),
    apf_dupsourcenomatch = unique(apf_dupsourcenomatch),
    pf_paperrepid = unique(pf_paperrepid)),
  ,by="paperid"]
data.paper.rep <- data.paper.rep[
  ,c("authorid", "paperid", 
     "pf_paperrepid", "apf_dupsourcenomatch"),with = F]

data.paper.repid <- data.paper.rep[
  ,list(pf_paperrepid = unique(pf_paperrepid)), by="paperid"]
setkeyv(data.paper.repid, "paperid")

data.author.dup.fake.id <- data.author.dup.fake[
  ,list(authorfakedupid = min(authorid, dupauthorids)), by="authorid"]
setkeyv(data.author.dup.fake.id, "authorid")

data.paper.rep <- fn.join(data.paper.rep, data.author.dup.fake.id)
data.paper.fake.rep <- data.paper.rep[
  !is.na(data.paper.rep$authorfakedupid),]
data.paper.fake.rep <- data.paper.fake.rep[,
                    list(apf_fakesource = length(unique(authorid))-1),
                    by=c("pf_paperrepid", "authorfakedupid")]
setkeyv(data.paper.fake.rep, c("pf_paperrepid", "authorfakedupid"))


data.paper.rep$authorfakedupid[is.na(data.paper.rep$authorfakedupid)] <- -1
data.paper.rep <- fn.join(data.paper.rep, data.paper.fake.rep)

no.fakedup <- is.na(data.paper.rep$apf_fakesource)
data.paper.rep$apf_fakesource[no.fakedup] <- -1
data.paper.rep$authorfakedupid <- NULL
data.paper.rep$apf_dupsourcenomatch[!no.fakedup] <- 
  data.paper.rep$apf_dupsourcenomatch[!no.fakedup] -
  data.paper.rep$apf_fakesource[!no.fakedup]

data.author.dup.id <- data.author.dup[
  ,list(authorupid = min(authorid, dupauthorids)), by="authorid"]
setkeyv(data.author.dup.id, "authorid")

data.paper.rep <- fn.join(data.paper.rep, data.author.dup.id)
data.paper.dup.rep <- data.paper.rep[
  !is.na(data.paper.rep$authorupid),]
data.paper.dup.rep <- data.paper.dup.rep[,
                    list(apf_dupsource = length(unique(authorid))-1),
                    by=c("pf_paperrepid", "authorupid")]
setkeyv(data.paper.dup.rep, c("pf_paperrepid", "authorupid"))

data.paper.rep$authorupid[is.na(data.paper.rep$authorupid)] <- -1
data.paper.rep <- fn.join(data.paper.rep, data.paper.dup.rep)

no.dup <- is.na(data.paper.rep$apf_dupsource)
data.paper.rep$apf_dupsource[no.dup] <- -1
data.paper.rep$authorupid <- NULL
data.paper.rep$apf_dupsourcenomatch[!no.dup] <- 
  data.paper.rep$apf_dupsourcenomatch[!no.dup] -
  data.paper.rep$apf_dupsource[!no.dup]

data.paper.author.src.ratio <- data.paper.author[
  ,list(apf_sourceratio = length(authorid)/length(unique(authorid))),
  by="paperid"]
setkeyv(data.paper.author.src.ratio, "paperid")

data.paper.rep <- fn.join(data.paper.rep, data.paper.author.src.ratio)

data.paper.author.stats <- data.paper.author.clean[
  ,list(
    pf_authorall = paste(c(" ", sort(unique(authorid)), " "), collapse = " "),
#     pf_authorpattern = paste(c("\\s", sort(unique(authorid)), "\\s"), collapse = ""),
    pf_authorcount = length(unique(authorid))),
  by="paperid"]
data.paper.author.stats <- fn.join(data.paper.author.stats, data.paper.repid)

data.paper.author.stats.same.set <- data.paper.author.stats[
  ,list(pf_countsameset = length(unique(pf_paperrepid))),
  by="pf_authorall"]
setkeyv(data.paper.author.stats.same.set, "pf_authorall")

data.paper.author.stats <- fn.join(data.paper.author.stats,
                                   data.paper.author.stats.same.set)

data.paper.author.stats$pf_countsameset[
  data.paper.author.stats$pf_authorcount == 1] <- 0

data.paper.author.stats$pf_authorall <- NULL
data.paper.author.stats$pf_authorcount <- NULL
data.paper.author.stats$pf_paperrepid <- NULL
setkeyv(data.paper.author.stats, "paperid")

data.paper.rep <- fn.join(data.paper.rep, data.paper.author.stats)

data.paper.rep <- fn.join(data.paper.rep, data.ids.feat)

data.paper.rep$pf_paperrepid <- NULL
data.paper.rep$apf_dupsourcenomatch <- NULL
# data.paper.rep$apf_dupsource <- NULL
# data.paper.rep$apf_fakesource <- NULL

setkeyv(data.paper.rep, c("authorid", "paperid"))

col.key <- c("authorid", "paperid")
paper.rep.dt <- unique(rbind(data.train.ids[,col.key,with=F],
                  data.test.ids[,col.key,with=F]))

data.dup.source.dt <- fn.join(paper.rep.dt, data.paper.rep)
setkeyv(data.dup.source.dt, c("authorid", "paperid"))

# fn.load.data("data.dup.source.dt")

fn.save.data("data.dup.source.dt")

cat("done... \n")
toc()

# ##############################################################
# ## authorset2
# ##############################################################
# source("fn.base.R")
# library("data.table")
# 
# tic()
# cat("creating source related features... ")
# 
# fn.load.data("data.train.ids")
# fn.load.data("data.test.ids")
# fn.load.data("data.paper.author.clean")
# 
# data.ids <- 
#   unique(rbind(data.train.ids[,c("authorid", "paperid"),with=F],
#                data.test.ids[,c("authorid", "paperid"),with=F]))
# 
# data.ids.expand <- data.paper.author.clean[
#   data.paper.author.clean$paperid %in% data.ids$paperid,
#   c("authorid", "paperid"), with = F]
# 
# data.paper.author.set <- data.ids.expand[
#   ,list(authorset = paste(sort(unique(authorid)), collapse = " "),
#         setsize = length(unique(authorid))),
#   by="paperid"]
# setkeyv(data.paper.author.set, "paperid")
# 
# data.authorset.dt <- fn.join(data.ids, data.paper.author.set)
# 
# data.authorset.dt.2 <- data.authorset.dt[
#   ,list(authorsetsize = length(unique(paperid))),
#   by=c("authorid", "authorset")]
# setkeyv(data.authorset.dt.2, c("authorid", "authorset"))
# 
# data.authorset.dt <- fn.join(data.authorset.dt, data.authorset.dt.2)
# 
# # data.authorset.dt$pf_sameauthorset1 <- 0
# # data.authorset.dt$pf_sameauthorset1[data.authorset.dt$setsize == 1] <- 
# #   data.authorset.dt$authorsetsize[data.authorset.dt$setsize == 1]
# # 
# # data.authorset.dt$pf_sameauthorset2 <- 0
# # data.authorset.dt$pf_sameauthorset2[data.authorset.dt$setsize %in% 2:6] <- 
# #   data.authorset.dt$authorsetsize[data.authorset.dt$setsize %in% 2:6]
# # 
# # data.authorset.dt$pf_sameauthorset3 <- 0
# # data.authorset.dt$pf_sameauthorset3[data.authorset.dt$setsize %in% 7:10] <- 
# #   data.authorset.dt$authorsetsize[data.authorset.dt$setsize %in% 7:10]
# # 
# # data.authorset.dt$pf_sameauthorset4 <- 0
# # data.authorset.dt$pf_sameauthorset4[data.authorset.dt$setsize > 10] <- 
# #   data.authorset.dt$authorsetsize[data.authorset.dt$setsize > 10]
# 
# data.authorset.dt$pf_sameauthorsetalt <- 
#   data.authorset.dt$authorsetsize
# data.authorset.dt$authorset <- NULL
# data.authorset.dt$setsize <- NULL
# data.authorset.dt$authorsetsize <- NULL
# 
# setkeyv(data.authorset.dt, c("authorid", "paperid"))
# 
# fn.save.data("data.authorset.dt")
# 
# ##############################################################
# ## source related
# ##############################################################
# source("fn.base.R")
# library("data.table")
# 
# tic()
# cat("creating misc source features... ")
# 
# fn.load.data("data.train.ids")
# fn.load.data("data.test.ids")
# fn.load.data("data.paper.author.clean")
# col.key <- c("authorid", "paperid")
# 
# data.aff.paper <- data.paper.author.clean[
#   data.paper.author.clean$pa_affiliation != "",]
# data.aff.paper <- data.aff.paper[
#   ,list(apf_sameaffsrc = length(authorid)),
#   by = c("paperid", "pa_affiliation")]
# setkeyv(data.aff.paper, c("paperid", "pa_affiliation"))
# 
# data.aff.name <- data.paper.author.clean[
#   data.paper.author.clean$pa_name != "",]
# data.aff.name <- data.aff.name[
#   ,list(apf_samenamesrc = length(authorid)),
#   by = c("paperid", "pa_name")]
# setkeyv(data.aff.name, c("paperid", "pa_name"))
# 
# col.key <- c("authorid", "paperid")
# data.misc.source.dt <- unique(rbind(data.train.ids[,col.key,with=F],
#                   data.test.ids[,col.key,with=F]))
# 
# data.misc.source.dt <- fn.join(data.misc.source.dt, data.paper.author.clean)
# 
# data.misc.source.dt <- fn.join(data.misc.source.dt, data.aff.paper)
# data.misc.source.dt <- fn.join(data.misc.source.dt, data.aff.name)
# 
# data.misc.source.dt$pa_name <- NULL
# data.misc.source.dt$pa_affiliation <- NULL
# data.misc.source.dt$apf_sameaffsrc[is.na(data.misc.source.dt$apf_sameaffsrc)] <- -1
# data.misc.source.dt$apf_samenamesrc[is.na(data.misc.source.dt$apf_samenamesrc)] <- -1
# 
# setkeyv(data.misc.source.dt, c("authorid", "paperid"))
# fn.save.data("data.misc.source.dt")
# 
# cat("done... \n")
# toc()

# #############################################################
# # analyse features
# #############################################################
# source("fn.base.R")
# library("data.table")
# library("stringr")
# 
# fn.load.data("data.cv.folds")
# fn.load.data("data.author")
# fn.load.data("data.conference")
# fn.load.data("data.journal")
# fn.load.data("data.paper")
# fn.load.data("data.train.ids")
# fn.load.data("data.test.ids")
# # fn.load.data("data.paper.author.clean")
# fn.load.data("data.paper.author")
# # data.paper.author <- data.paper.author
# 
# dt.analisys <- fn.join(data.table(data.paper.author), data.paper)
# dt.analisys <- fn.join(dt.analisys, data.author)
# dt.analisys <- fn.join(dt.analisys, data.train.ids)
# dt.analisys$p_title <- str_trim(tolower(dt.analisys$p_title))
# dt.analisys$p_title <- gsub("[^a-z0-9]", " ", dt.analisys$p_title)
# dt.analisys$p_title <- gsub("\\s+", " ", dt.analisys$p_title)
# setkeyv(dt.analisys, "authorid")
# 
# dt.analisys.cur  <- dt.analisys[J(1857848)]
# # dt.analisys.cur <- fn.join(dt.analisys.cur, data.train.ids)
# dt.analisys.cur$confirmed[is.na(dt.analisys.cur$confirmed)] <- -1
# dt.analisys.cur <- dt.analisys.cur[order(-dt.analisys.cur$confirmed),]
# head(dt.analisys.cur, n = 20)

