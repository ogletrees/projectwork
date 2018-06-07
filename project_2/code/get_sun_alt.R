library(oce)

# to get this myself and not have to assume that the saftware is getting it, will need the tz for each cbg too

dd <- "06/07/2018 13:30:00"
tt <- "US/Central"


out <- as.POSIXct("06/07/2018 13:30:00 PM", format="%m/%d/%Y %H:%M:%S", tz = "US/Central")
out
attr(out, "tzone") <- "UTC" 
out


t <- as.POSIXct(dd, format="%m/%d/%Y %H:%M:%S", tz = tt)
t
sunAngle(t, longitude = -82.8226, latitude =  34.6840)$altitude


d1 <- "06/07/2018 15:12:00"
d2 <- "US/Eastern"
t <- as.POSIXct(d1, format="%m/%d/%Y %H:%M:%S", tz = d2)
attr(t, "tzone") <- "UTC"
sunAngle(t, longitude = -82.843 , latitude =  34.679 )
