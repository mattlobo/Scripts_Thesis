# Don't mind the warnings
# Be patient, it takes time to generate all of the files
# 2009 is not working, I used STATA instead
# Loading necessary packages
library(downloader)	# downloads and then runs the source() function on scripts from github
library(survey)		# load survey package (analyzes complex design surveys)
library(RSQLite) 	# load RSQLite package (creates database files in R)
library(stringr) 	# load stringr package (manipulates character strings easily)

setwd("~/Documents/Monografia/PNAD/Microdata/2001 - 2013")

# set R to produce conservative standard errors instead of crashing
# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options( survey.lonely.psu = "adjust" )
# this setting matches the MISSUNIT option in SUDAAN

# load pnad-specific functions (to remove invalid SAS input script fields and postStratify a database-backed survey object)
source( "/Users/mlobo/Documents/Monografia/Scripts_Thesis/PNAD Data/2001_2013/Function2.R" , prompt = FALSE )

if ( .Platform$OS.type != 'windows' ){
	options( encoding="windows-1252" )
}

pnad.dbname <- "pnad.db"

# Insert year you want to calculate statistics for

years <- c(2001)

for (year.to.analyze in years){

 if (year.to.analyze > 2001){
 	sample.pnad <-
  		svydesign(
    	id = ~v4618 ,
    	strata = ~v4617 ,
    	data = paste0( "pnad" , year.to.analyze ) ,
    	weights = ~pre_wgt ,
    	nest = TRUE ,
    	dbtype = "SQLite" ,
    	dbname = "pnad.db"
  		)
# note that the above object has been given the unwieldy name of `sample.pnad`
# so that it's not accidentally used in analysis commands.
# this object has not yet been appropriately post-stratified, as necessitated by IBGE
# in order to accurately match the brazilian 2010 census projections

# this block conducts a post-stratification on the un-post-stratified design
# and since the R `survey` package's ?postStratify currently does not work on database-backed survey objects,
# this uses a function custom-built for the PNAD.
y <- 
  	pnad.postStratify( 
    	design = sample.pnad ,
    	strata.col = 'v4609' ,
    	oldwgt = 'pre_wgt'
  		)
 }
  else{
  	sample.pnad <-
  		svydesign(
    	id = ~psu ,
    	strata = ~strat ,
    	data = paste0( "pnad" , year.to.analyze ) ,
    	weights = ~pre_wgt ,
    	nest = TRUE ,
    	dbtype = "SQLite" ,
    	dbname = "pnad.db"
  		)
# note that the above object has been given the unwieldy name of `sample.pnad`
# so that it's not accidentally used in analysis commands.
# this object has not yet been appropriately post-stratified, as necessitated by IBGE
# in order to accurately match the brazilian 2010 census projections

# this block conducts a post-stratification on the un-post-stratified design
# and since the R `survey` package's ?postStratify currently does not work on database-backed survey objects,
# this uses a function custom-built for the PNAD.
y <- 
  	pnad.postStratify( 
    	design = sample.pnad ,
    	strata.col = 'v4609' ,
    	oldwgt = 'pre_wgt'
  		)
  }

new.y <- subset(y, v8005 >= 10 & v0302 == 2)

data <- svyby(~one, ~agecat+v9001+v9115, new.y , svytotal)
write.csv(data, paste0("ages.working.males.", year.to.analyze, ".csv"))
}