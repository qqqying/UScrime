setwd("~/Documents/GitHub/UScrime/raw data")

library(xlsx)
library(zoo) # for na.locf()


## import data
for(i in 1995:2019){
  if(i < 1999){
    assign(paste0("crime",i), read.xlsx(paste0("crime",i,".xlsx"), 
                                        sheetIndex=1, header=TRUE))
  } else if(i %in% c(2001,2003)) {
    assign(paste0("crime",i), read.xlsx(paste0("crime",i,".xls"), 
                                        sheetIndex=1, header=TRUE, startRow=5))
  } else {
    assign(paste0("crime",i), read.xlsx(paste0("crime",i,".xls"), 
                                        sheetIndex=1, header=TRUE, startRow=4))
  }
  
}


## generate dataset (rate and total population) by state (1995-2004)
for(i in 1995:2004){
  # assign dataset to "crime"
  assign("crime", get(paste0("crime", i)))
  
  # create state column (all capital letters) from Area
  states <- as.data.frame(ifelse(grepl('([A-Z]|[A-Z][0-9])$',crime$Area), crime$Area, NA))
  # fill missing value with previous values
  states <- na.locf(states, na.rm=FALSE)
  # combine states column and crime data
  crime <- cbind(states, crime)
  # change column name to state
  colnames(crime)[1] <- "State"
  # change column name to Data
  colnames(crime)[2] <- "Data"
  # change column name to Total
  colnames(crime)[3] <- "Total"
  # extract rate rows and state total rows for each state
  if(i %in% 1995:1997){
    final <- crime[grepl('^(inhabitants|State Total)',crime$Data),]
    final[grepl('inhabitants',final$Data), 2] <- "Rate"
    final[grepl('State Total',final$Data), 2] <- "Population"
  } else {
    final <- crime[grepl('(Rate|State Total)',crime$Data),]
    final[grepl('Rate',final$Data), 2] <- "Rate"
    final[grepl('State Total',final$Data), 2] <- "Population"
  }
  
  # assign "final" to a new dataset
  assign(paste0("cr",i), final)
  
}



## generate dataset (rate and total population) by state (2005-2019)
for(i in 2005:2019){
  # assign dataset to "crime"
  assign("crime", get(paste0("crime", i)))
  
  # fill missing value with previous values
  crime$State <- na.locf(crime$State, na.rm=FALSE)
  # extract rate rows and state total rows for each state
  final <- crime[which(grepl('Rate',crime[,3]) | grepl('State Total',crime[,2])),]
  # change column name to Data
  colnames(final)[2] <- "Data"
  final <- final[,-3]
  # change column name to Total
  colnames(final)[3] <- "Total"
  #
  final[which(grepl('Rate',final$Data)|is.na(final$Data)), 2] <- "Rate"
  final[grepl('State Total',final$Data), 2] <- "Population"
  # assign "final" to a new dataset
  assign(paste0("cr",i), final)
}


## select specific columns and add "Year" column
for(i in 1995:2019){
  # assign dataset to "crime"
  assign("crime", get(paste0("cr", i)))
  # select specific columns
  if(i %in% 1995:1998){
    crime <- crime[,c(1:3,6,8:11,7,12:14)]
  } else if(i %in% 1999:2002) {
    crime <- crime[,c(1:3,6,8:11,7,13:15)]
  } else if(i %in% c(2003:2010, 2012, 2017:2018)) {
    crime <- crime[,c(1:12)]
  } else if(i %in% 2013:2016) {
    crime <- crime[,c(1:6, 8:13)]
  }
  # add "Year" column
  crime$Year <- i
  # remove numbers from state names
  crime$State <-  gsub('[0-9]+', '', crime$State)
  # remove Not Word from state names
  crime$State <-  gsub('\\W', '', crime$State)
  # set row names to NULL
  rownames(crime) <- NULL
  # set column names
  colnames(crime) <- c("State","Data", "Total","Violent.crime","Murder.and.nonnegligent.manslaughter"
                       ,"Rape" ,"Robbery" ,"Aggravated.assault" ,"Property.crime"                       
                       ,"Burglary" ,"Larceny.theft" ,"Motor.vehicle.theft" ,"Year" )
  
  # assign "crime" to a new dataset
  assign(paste0("cr",i), crime)
  
}


## combine data by columns from 1995 to 2019
uscrime <- rbind(cr1995, cr1996, cr1997, cr1998, cr1999,
                 cr2000, cr2001, cr2002, cr2003, cr2004,
                 cr2005, cr2006, cr2007, cr2008, cr2009,
                 cr2010, cr2011, cr2012, cr2013, cr2014,
                 cr2015, cr2016, cr2017, cr2018, cr2019)

# remove typo in two cells
uscrime$Property.crime[uscrime$State=="NEWHAMPSHIRE" & uscrime$Year==1995 & uscrime$Data=="Rate"] <- "2540.9"
uscrime$Property.crime[uscrime$State=="ALABAMA" & uscrime$Year==1995 & uscrime$Data=="Population"] <- "179294"


## convert some character variables to numeric variables
uscrime <- data.frame(uscrime$State, uscrime$Data, lapply(uscrime[,3:12], function(x) as.numeric(x)), uscrime$Year)


## Rename column name
colnames(uscrime)[1] <- "State"
colnames(uscrime)[2] <- "Data"
colnames(uscrime)[13] <- "Year"

# calculate one missing variable by definition in Year 2000
uscrime$Property.crime[uscrime$State=="CONNECTICUT" & uscrime$Year==2000 & uscrime$Data=="Population"] <- 99033
uscrime$Property.crime[uscrime$State=="CONNECTICUT" & uscrime$Year==2000 & uscrime$Data=="Rate"] <-   round((99033/3405565)*10^5, 1)





## save file
save(uscrime, file="uscrime.rda")

