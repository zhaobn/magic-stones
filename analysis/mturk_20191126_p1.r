setwd('bramleylab/magic_stones/analysis')
options("scipen" = 10)
options()$scipen

library(RPostgreSQL)
#library(RJSONIO)
library(rjson)
rm(list=ls())

exp = 'bn_magic_stones'
pilot_start = 1
pilot_end = 8

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

subject<-td$subject[c(pilot_start:pilot_end)]
trials<-td$trials[c(pilot_start:pilot_end)]

# Create dataframes
df.sw.aux = as.data.frame(inv_fromJSON(subject[1]))
df.tw.aux = as.data.frame(inv_fromJSON(drop_clicks(trials[1])))
for (i in 2:pilot_end) {
  sj = as.data.frame(inv_fromJSON(subject[i]))
  tj = as.data.frame(inv_fromJSON(drop_clicks(trials[i])))
  df.sw.aux = rbind(df.sw.aux, sj)
  df.tw.aux = rbind(df.tw.aux, tj)
}


#Un-jsonify it
inv_fromJSON<-function(js)
{
  js <- chartr("\'\"","\"\'",js)
  return(fromJSON(js))
}

# Drop clicks data for now because I don't know how to process them
# TODO: analyze clicks data
drop_clicks <- function(text) {
  return (paste0(substr(text, 0,605), '}"'))
}


N<-max(tw$trial) # 15 trials


#And append them to the id and upis
df.sw<-data.frame(ix=td$id,
                  id=td$participant)
df.sw<-cbind(df.sw, df.sw.aux)
df.tw<-cbind(ix=rep(df.sw$ix, each=N), id=rep(df.sw$id, each=N), df.tw.aux)

save(file='../data/mturk_20191126_p1.Rdata', df.sw, df.tw)
