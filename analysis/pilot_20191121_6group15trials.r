setwd('bramleylab/magic_stones/analysis')

library(RPostgreSQL)
library(rjson)
rm(list=ls())

exp = 'bn_magic_stones'
pilot_start = 6
pilot_end = 11

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
# Trim off test data
subject<-td$subject[c(pilot_start:pilot_end)]
trials<-td$trials[c(pilot_start:pilot_end)]

#Un-jsonify it
inv_fromJSON<-function(js)
{
  js <- chartr("\'\"","\"\'",js)
  fromJSON(js)
}

# Drop clicks data for now because I don't know how to process them
# TODO: analyze clicks data
drop_clicks <- function(text) {
  return (paste0(substr(text, 0,605), '}"'))
}

#and turn each subject into a dataframe
### Manual debug..
subject[2] = sub("\"Let's talk in person\"", "'Lets talk in person'", subject[2])
sw<-sapply(sapply(subject, inv_fromJSON, simplify=F), as.data.frame, simplify=F)
# tw<-sapply(sapply(td$trials, inv_fromJSON, simplify=F), as.data.frame, simplify=F)
tw<-sapply(sapply(sapply(trials, drop_clicks), inv_fromJSON, simplify=F), as.data.frame, simplify=F)

N<-length(tw[[1]]$trial) # 15 trials

#Combine them
df.sw.aux<-sw[[1]]
df.tw.aux<-tw[[1]]
for (i in 2:length(sw)) {
  df.sw.aux<-rbind(df.sw.aux, sw[[i]])
  df.tw.aux<-rbind(df.tw.aux, tw[[i]])
}
#And append them to the id and upis
df.sw<-data.frame(ix=td$id,
                  id=td$participant)
df.sw <- df.sw[c(pilot_start:pilot_end),]
df.sw<-cbind(df.sw, df.sw.aux)
df.tw<-cbind(ix=rep(df.sw$ix, each=N), id=rep(df.sw$id, each=N), df.tw.aux)

save(file='../data/pilot_20191121.Rdata', df.sw, df.tw)
