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


## generate dataset (rate) by state (1995-2004)
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
  # extract rate rows for each state
  final <- crime[grepl('(Rate|inhabitants)',crime$Area),]
  
  # assign "final" to a new dataset
  assign(paste0("cr",i), final)
  
}


## remove NA row from 1995-1997
cr1995 <- cr1995[duplicated(cr1995[,1])==TRUE,]
cr1996 <- cr1996[duplicated(cr1996[,1])==TRUE,]
cr1996 <- cr1996[-52,]
cr1997 <- cr1997[duplicated(cr1997[,1])==TRUE,]


## generate rate dataset by state (2005-2019)
for(i in 2005:2019){
  # assign dataset to "crime"
  assign("crime", get(paste0("crime", i)))
  
  # fill missing value with previous values
  crime$State <- na.locf(crime$State, na.rm=FALSE)
  # extract rate rows for each state
  final <- crime[grepl('Rate',crime[,3]),]
  
  # assign "final" to a new dataset
  assign(paste0("cr",i), final)
}


## select specific columns and add "Year" column
for(i in 1995:2019){
  # assign dataset to "crime"
  assign("crime", get(paste0("cr", i)))
  # select specific columns
  if(i %in% 1995:1998){
    crime <- crime[,c(1,6,8:11,7,12:14)]
  } else if(i %in% 1999:2002) {
    crime <- crime[,c(1,6,8:11,7,13:15)]
  } else if(i %in% c(2003,2004)) {
    crime <- crime[,c(1,4:12)]
  } else if(i %in% c(2005:2012, 2017:2019)) {
    crime <- crime[,c(1,5:13)]
  } else if(i %in% 2013:2016) {
    crime <- crime[,c(1,5:7,9:14)]
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
  colnames(crime) <- c("State","Violent.crime","Murder.and.nonnegligent.manslaughter"
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

# remove . in one cell
uscrime$Property.crime[uscrime$State=="NEWHAMPSHIRE" & uscrime$Year==1995] <- "2540.9"

# calculate one missing variable by definition in Year 2000
na20 <- crime2000[which(crime2000$Area=="CONNECTICUT")+8,]
na20$Burglary <- as.numeric(na20$Burglary)
na20$Larceny.theft <- as.numeric(na20$Larceny.theft)
na20$Motor.vehicle.theft <- as.numeric(na20$Motor.vehicle.theft)
na20$Population <- as.numeric(na20$Population)
uscrime$Property.crime[uscrime$State=="CONNECTICUT" & uscrime$Year==2000] <- 
  round(((na20$Burglary+na20$Larceny.theft+na20$Motor.vehicle.theft)/na20$Population)*10^5, 1)


## convert some character variables to numeric variables
uscrime <- data.frame(uscrime$State, lapply(uscrime[,2:10], function(x) as.numeric(x)), uscrime$Year)
colnames(uscrime)[1] <- "State"
colnames(uscrime)[11] <- "Year"


## save file
save(uscrime, file="uscrime.rda")

