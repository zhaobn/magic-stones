setwd('bramleylab/magic_stones/analysis')

library(RPostgreSQL)
library(rjson)
rm(list=ls())

exp = 'bn_magic_stones'
n_tests = 6

taskTableName = paste0(exp, '_', "task")
participantTableName = paste0(exp, '_', "participant")

# First run this in the terminal to connect to the database:

# ssh -L 1111:localhost:5432 wwwbramleylabppl@chost4.is.ed.ac.uk

#Then you should be able to connect via an ssh port
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = 'wwwbramleylabppl_flask',
                 host = 'localhost', port = 1111,
                 password = 'testpassword',
                 user = 'wwwbramleylabppl_flaskuser')

# If its worked you should be able to detect these databases
dbExistsTable(con, taskTableName)
dbExistsTable(con, participantTableName)

# Then you can pull the task data from postgreSQL 
td <- dbGetQuery(con, paste("SELECT * from ", taskTableName))
#Here it is as json
td$subject
td$trials

#Un-jsonify it
inv_fromJSON<-function(js)
{
  js <- chartr("\'\"","\"\'",js)
  fromJSON(js)
}

# Drop clicks data for now because I don't know how to process them
# TODO: analyze clicks data
drop_clicks <- function(text) {
  return (paste0(substr(x, 0, 399), '}"'))
}

#and turn each subject into a dataframe
### Manual debug..
td$subject[7] = sub("\"Let's talk in person\"", "'Lets talk in person'", td$subject[7])
sw<-sapply(sapply(td$subject, inv_fromJSON, simplify=F), as.data.frame, simplify=F)
# tw<-sapply(sapply(td$trials, inv_fromJSON, simplify=F), as.data.frame, simplify=F)
tw<-sapply(sapply(sapply(td$trials, drop_clicks), inv_fromJSON, simplify=F), as.data.frame, simplify=F)

N<-length(tw[[1]]$trial)

#Combine them
df.sw.aux<-sw[[n_tests]]
df.tw.aux<-tw[[n_tests]]
for (i in (n_tests+1):length(sw)) {
  df.sw.aux<-rbind(df.sw.aux, sw[[i]])
  df.tw.aux<-rbind(df.tw.aux, tw[[i]])
}
#And append them to the id and upis
df.sw<-data.frame(ix=td$id,
                  id=td$participant)
df.sw <- df.sw[c(n_tests:nrow(df.sw)),]
df.sw<-cbind(df.sw, df.sw.aux)
df.tw<-cbind(ix=rep(df.sw$ix, each=N), id=rep(df.sw$id, each=N), df.tw.aux)

save(file='../data/pilot_20191121', df.sw, df.tw)
