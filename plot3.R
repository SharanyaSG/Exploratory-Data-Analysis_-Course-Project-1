##-----------------------------------------------------------##
## READ DATA FROM THE ELECTRIC POWER CONSUMPTION DATA SET
##-----------------------------------------------------------##

  elec_pow_con <- read.table("household_power_consumption.txt", sep=";", na.strings = "?", header=T,stringsAsFactors = F)

##-----------------------------------------------------------##
## PREPARE AND CLEAN DATA
##-----------------------------------------------------------##
  ## STEP 1 - Convert the Date and Time variables to Date/Time classes in R using the as.Date() function
    elec_pow_con$Date <- as.Date(elec_pow_con$Date, "%d/%m/%Y")

  ## STEP 2 - Read data only from the dates 2007-02-01 and 2007-02-02
    elec_pow_con <- subset(elec_pow_con,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))

  ## STEP 3 - Create a single column for Date and Time and store it using the POSIXct function
    D_and_T <- paste(elec_pow_con$Date, elec_pow_con$Time)
    D_and_T <- setNames(D_and_T, "Date and Time")
    elec_pow_con <- elec_pow_con[ ,!(names(elec_pow_con) %in% c("Date","Time"))]
    elec_pow_con <- cbind(D_and_T, elec_pow_con)
    elec_pow_con$D_and_T <- as.POSIXct(D_and_T)

##-----------------------------------------------------------##
## PROJECT REQUIREMENT 3
##-----------------------------------------------------------##
  ## STEP 1 - Construct "plot3" - Energy Sub Metering Line Plot
    with(elec_pow_con,plot(Sub_metering_1~D_and_T,type="l", ylab="Energy sub metering", xlab="", col = "black"))
    with(elec_pow_con,lines(Sub_metering_2~D_and_T, ylab="Energy sub metering", xlab="", col = "red"))
    with(elec_pow_con,lines(Sub_metering_3~D_and_T, ylab="Energy sub metering", xlab="", col = "blue"))
 
  ## STEP 2 - Create legend for the Energy Sub Metering Line Plot
    legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

  ## STEP 3 - Save "plot3" to a PNG file with a width of 480 pixels and a height of 480 pixels
    dev.copy(png,"plot3.png", width=480, height=480)
    dev.off()