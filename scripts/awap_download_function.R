awap_data_download<-function(datefinish_string,datestart_string,lat,lon,months_activity=NA){

#connect to awap sql data
new_db_connection <- function() {
  # open a new database connection to AWAP
  channel <- DBI::dbConnect(RMySQL::MySQL(),
    user = "general",
    password = "predecol",
    host = "115.146.93.180", # Updated server 2021
    dbname = "AWAPDaily",
    port = 3306
  )
  return(channel)
}


killDbConnections <- function () {

  all_cons <- DBI::dbListConnections(RMySQL::MySQL())

  print(all_cons)

  for(con in all_cons)
    +  DBI::dbDisconnect(con)

  print(paste(length(all_cons), " connections killed."))

}


#define the time frame
datestart <- strptime(datestart_string, "%d/%m/%Y") # convert to date format
yearstart <- as.numeric(format(datestart, "%Y")) # get year start
datefinish <- strptime(datefinish_string, "%d/%m/%Y") # convert to date format
yearfinish <- as.numeric(format(datefinish, "%Y")) # yet year finish
number_of_years <- yearfinish - yearstart
years <- seq(yearstart, yearfinish, 1) # get sequence of years to do
dates <- seq(datestart, datefinish + 3600, "DSTday") # sequence of dates
juldaystart <- datestart$yday + 1 # get Julian day of year at start
juldayfinish <- datefinish$yday + 1 # get Julian day of year at finish

lat1 <- lat - 0.05
lat2 <- lat + 0.05
lon1 <- lon - 0.05
lon2 <- lon + 0.05
loc <- c(lon, lat)

# Start connection
channel <- new_db_connection()

## download the data from awap
for (i in seq_len(length(years))) { # start loop through years
            # syntax for query
            if (length(years) == 1) { # doing a period within a year
                query <- paste("SELECT a.latitude, a.longitude, b.*
        FROM AWAPDaily.latlon as a
      , AWAPDaily.", years[i], " as b
      where (a.id = b.id) and (a.latitude between ",
                    lat1, " and ", lat2,
                    ") and (a.longitude between ", lon1, " and ", lon2,
                    ") and (b.day between ", juldaystart, " and ",
                    juldayfinish, ")
      order by b.day",
                    sep = ""
                )
            } else if (i == 1) { # doing first year, start at day requested
                query <- paste("SELECT a.latitude, a.longitude, b.*
          FROM AWAPDaily.latlon as a
          , AWAPDaily.", years[i], " as b
          where (a.id = b.id) and (a.latitude between ",
                    lat1, " and ", lat2, ") and (a.longitude between ",
                    lon1, " and ", lon2, ") and (b.day >= ", juldaystart, ")
          order by b.day",
                    sep = ""
                )
            } else if (i == length(years)) {
                # doing last year, only go up to last day requested
                query <- paste("SELECT a.latitude, a.longitude, b.*
            FROM AWAPDaily.latlon as a
            , AWAPDaily.", years[i], " as b
            where (a.id = b.id) and (a.latitude between ",
                    lat1, " and ", lat2, ") and (a.longitude between ",
                    lon1, " and ", lon2, ") and (b.day <= ", juldayfinish, ")
            order by b.day",
                    sep = ""
                )
            } else { # doing in between years, so get all data for this year
                query <- paste("SELECT a.latitude, a.longitude, b.*
        FROM AWAPDaily.latlon as a
        , AWAPDaily.", years[i], " as b
        where (a.id = b.id) and (a.latitude between ",
                    lat1, " and ", lat2, ") and (a.longitude between ",
                    lon1, " and ", lon2, ")
        order by b.day",
                    sep = ""
                )
            }

            response <- DBI::dbGetQuery(channel, query)

            # Detection of broken records

            if (nrow(response) == 0) {
                fail_flag <- 1
                print("error with this observation")
                next
            }

            fail_flag <- 0
            # If the query worked properly, add it to the output table
            # This output table contains all the variables for each ind, but without the necessary processing. 
            # We will use it as a base to populate the empty individual data afterwards
            if (i == 1) {
                output <- response 
            } else {
                output <- rbind(output, response)
            } # end of loop years

        }

        killDbConnections()

        if (fail_flag == 1) {
            result<-data.frame(latitude=lat,longitude=lon,precipitation=NA,vapor_pressure=NA,max_temperature=NA,min_temperature=NA,solar_radiation=NA)
        return(result)
        }

        output <- cbind(dates, output)
        if(is.na(months_activity)==FALSE){ 
        months<-month.name[!month.name %in% months_activity]
        months<-match(months, month.name)
        output <- output[as.numeric(format(output$dates, "%m")) %in% months, ]
        }
        #calculate average time of activity
        avg_max_temp <- mean(as.numeric(output$tmax), na.rm = TRUE)
        avg_min_temp <- mean(output$tmin, na.rm = TRUE)
        avg_rr <- mean(output$rr, na.rm = TRUE)
        avg_vpr <- mean(output$vpr, na.rm = TRUE)
        avg_sol <- mean(as.numeric(output$sol), na.rm = TRUE)
        result<-data.frame(latitude=lat,longitude=lon,precipitation=avg_rr,vapor_pressure=avg_vpr,max_temperature=avg_max_temp,min_temperature=avg_min_temp,solar_radiation=avg_sol)
        return(result)

}