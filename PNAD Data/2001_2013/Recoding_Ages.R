# name the database (.db) file that should have been saved in the working directory
pnad.dbname <- "pnad.db"

year.to.analyze <- c(2002)

library(downloader)	# downloads and then runs the source() function on scripts from github
library(survey)		# load survey package (analyzes complex design surveys)
library(RSQLite) 	# load RSQLite package (creates database files in R)
library(stringr) 	# load stringr package (manipulates character strings easily)

# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN


###############################################################
# step 1: connect to the pnad data table you'd like to recode # 
# then make a copy so you don't lose the pristine original.   #

# the command 
db <- dbConnect( SQLite() , pnad.dbname )
# connects the current instance of r to the sqlite database

############################################
# step 2: make all of your recodes at once #

# add a new column.  call it, oh i don't know, agecat?
# since it's actually a categorical variable, make it VARCHAR( 255 )
dbSendQuery( db , "ALTER TABLE pnad2002 ADD COLUMN agecat VARCHAR( 255 )" )

# if you wanted to create a numeric variable, substitute VARCHAR( 255 ) with DOUBLE PRECISION like this:
# dbSendQuery( db , "ALTER TABLE recoded_pnad2002 ADD COLUMN agecatx DOUBLE PRECISION" )
# ..but then agecat would have to be be numbers (1 - 8) instead of the strings shown below ('01' - '08')

# to automate things, just create a vector of each age bound
agebounds <- c( seq(0,80,1), 200 )
# and loop through each interval, plugging in a new agecat for each value

# start at the value '0' and end at the value '200'.
for ( i in 1:( length( agebounds ) - 1 ) ){
  
  # build the sql string to pass to sqlite
  update.sql.string <- paste0( "UPDATE pnad2002 SET agecat = '" , str_pad( i-1 , 2 , pad = '0' ) , "' WHERE v8005 >= " , agebounds[ i ] , " AND v8005 < " , agebounds[ i + 1 ] )
  
  # take a look at the update.sql.string you've just built.  familiar?  ;)
  print( update.sql.string )
  
  # now actually run the sql string
  dbSendQuery( db , update.sql.string )
}

dbGetQuery( db , "SELECT agecat , v8005 , COUNT(*) as number_of_records from pnad2002 GROUP BY agecat , v8005 ORDER BY v8005" )