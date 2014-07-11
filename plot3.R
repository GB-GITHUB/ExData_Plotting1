#######################
# COMPANION FUNCTIONS #
#######################

################################################################################
# Function: get_raw_data
################################################################################
# DESCRIPTION:  If the raw data zip is not already downloaded, download it from the web and unzip it
# INPUT:        
#       rawData --> File to be downloaded
################################################################################
get_raw_data <- function(rawData = "exdata_data_household_power_consumption.zip") {
        # If file does not already exist
        if (!file.exists(rawData)) {
                # Set file URL
                fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
                # Download File
                download.file(fileUrl, rawData, method = "auto")
        }
        
        # Un-zip file
        writeLines("INFO: Unzipping File(s)")
        unzip(rawData)
}

#######################
#    MAIN FUNCTION    #
#######################

################################################################################
# Function: make_plot3
################################################################################
# DESCRIPTION:  Instructions for creating plot 3
################################################################################
make_plot3 <- function() {
        
        # Get data
        writeLines("INFO: Looking for data files")
        get_raw_data()
        
        # Load data
        writeLines("INFO: Loading data")
        plot_data <- read.csv2("./household_power_consumption.txt", stringsAsFactors = FALSE)
        
        writeLines("INFO: Preparing data")
        # Subsetting data, keeping only targeted dates
        plot_data <- subset(plot_data, Date == "1/2/2007" | Date =="2/2/2007")
        
        # Combine Date and Time
        plot_data$datetime <- with(plot_data, paste(Date, Time, sep=" "))
        
        # Convert from character to Date/Time format
        plot_data$datetime <- strptime(plot_data$datetime, "%d/%m/%Y %H:%M:%S")
        
        # Transform targeted measures as numeric
        plot_data$Global_active_power <- as.numeric(plot_data$Global_active_power)
        
        # Create final PNG file (by default size is 480x480)
        png(file="plot3.png")
        
        writeLines("INFO: Creating Plot")
        # Set locale to C in order to have days' names in english
        Sys.setlocale("LC_TIME", "C")
        
        # Set plot
        with(plot_data, {
                plot(datetime, Sub_metering_1, type='l', xlab='', ylab="Energy sub metering")
                lines(datetime, Sub_metering_2, type='l', col='red')
                lines(datetime, Sub_metering_3, type='l', col='blue')
        })
        
        # Set legend
        legend('topright', legend=c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), lwd='1', col=c('black', 'red', 'blue'))
        
        # Close device
        dev.off()
        
}