# Author: Benjamin Mohn 
# ======
# Date: 11.06.2018
# =====
# General Information:
# ====================
# The skript was wriiten in order to pass the first assignment of the 
# "Exploratory Data Analysis" cours. 
# Expected setting: 
# =================
# In order to get the requiered output the file, which is contained in this 
# zip: https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
# should be placed in the working directory. 
# Result: 
# =======
# The script will create a Graphik called "plot2.png" in the 
# current workingdirectory. 
# Additional Information: 
# =======================
# The data contained in the above stated link contains 
# 2.075.259 rows and 9 columns, the entire data set will be read, so make sure, 
# that your Computer is able to handle this amount of data, before running the
# Skript. 
###############################################################################
loadingData <- function(path){
        # This function reads in the data at the path. 
        # param path: [str] - A string representing the file to be read in
        # return rawData: [data.frame] - The data stored at the given path.
        #######################################################################
        rawData <- read.table(path, header=TRUE, sep=";", na.strings = "?", stringsAsFactors = FALSE)
        return(rawData)
}

restructureData <- function(data, start, end){
        # This function will filter the data set such that the given start 
        # time is included and the given and time is expluded. 
        # In addition this function creates a new Column called "CompleteDate"
        # which is a concatination o fthe Date and Time function. 
        # param data: [data.frame] data frame which should be filtered
        # param start: [str] A string representing a date in the format: 
        #                    "%Y-%m-%d %H:%M:%S" Representing the first
        #                    point of time to be included in the data. 
        # param end: [str] A string representing a date in the format: 
        #                  "%Y-%m-%d %H:%M:%S" Representing the first
        #                  point of time to be excluded in the data. 
        # return smaller_data: [data.frame] A data.frame filtered for the start
        #                                   and end time. 
        #######################################################################
        startDate <- strptime(start, format="%Y-%m-%d %H:%M:%S")
        endDate <- strptime(end, format="%Y-%m-%d %H:%M:%S")
        data$CompleteDate <- strptime(paste(data$Date, data$Time), format= "%d/%m/%Y %H:%M:%S")
        
        smaller_data <- subset(data, (CompleteDate >= startDate) & (CompleteDate < endDate))
        return(smaller_data)
}

# In this part the two previously defined functions are executed. 
###############################################################################
entire_data <- loadingData("household_power_consumption.txt")
plot_data <- restructureData(entire_data, "2007-02-01 00:00:00", "2007-02-03 00:00:00")


# In this part the plot is created and stored afterwards. 
# The png() function is called without any parameter because the default 
# settings for height and width are as desired by the task description. 
###############################################################################
par(mfrow=c(1,1))
plot(plot_data$CompleteDate, plot_data$Global_active_power, type="l", ylab = "Global Active Power (kilowatts)", xlab="")
dev.copy(png, "plot2.png")
dev.off()