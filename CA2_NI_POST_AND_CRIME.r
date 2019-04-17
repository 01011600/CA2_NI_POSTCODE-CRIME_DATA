# Read in the NI post code file and assign to variable NIPostcodes 
NIPostcodes <-read.csv("NIPostcodes.csv",header = T,sep=",")
# using str print the structure
str(NIPostcodes)
# Print the first 10 rows of NIPostcodes
head(NIPostcodes, 10)

colnames(NIPostcodes) <- c("Organisation Name", "Sub-building Name", "Building Name",
                           "Number", "Primary Thorfare", "Alt Thorfare",
                           "Secondary Thorfare", "Locality", "Townland",
                           "Town", "County", "Postcode", "x-coordinates",
                           "y-coordinates", "Primary Key") 
str(NIPostcodes)

# replace all the blank cell vaules with NA
NIPostcodes[NIPostcodes == ""] <- NA

# count the number of missing values by column
colSums(is.na(NIPostcodes))

# Calculate the mean number of missing values for each column
colMeans(is.na(NIPostcodes))
str(NIPostcodes)

# Get the class of each Column
sapply(NIPostcodes,class)

#Change the class of County to be a categorising factor
NIPostcodes$County <- as.factor(NIPostcodes$County)

# Check to see what class County is post the change
class(NIPostcodes$County)

# Print the column names of the dataframe NIPostcodes
colnames(NIPostcodes)
# Change the order of the columns, move Primary Key to the first column
NIPostcodes <- NIPostcodes[,c("Primary Key","Organisation Name", "Sub-building Name", "Building Name",
                              "Number", "Primary Thorfare", "Alt Thorfare",
                              "Secondary Thorfare", "Locality", "Townland",
                              "Town", "County", "Postcode", "x-coordinates",
                              "y-coordinates")]

# Print the column names of the dataframe NIPostcodes
colnames(NIPostcodes)

# Create a new dataframe called Limavady_data that contains records that have 
# Limavady in the column Locality,Townland and Town
Limavady_data<-NIPostcodes[grepl("LIMAVADY",NIPostcodes$Locality) & 
                           grepl("LIMAVADY",NIPostcodes$Townland) & 
                           grepl("LIMAVADY",NIPostcodes$Town),]

# Write Limavady data to CSV file
write.csv(Limavady_data, file = "Limavady.csv")

# rename the NIPostcodes dataframe as CleanNIPostcodeData
CleanNIPostcodeData <- NIPostcodes

# Write the NIPostcodes data to CSV file called CleanNIPostcodeData
write.csv(NIPostcodes, file = "CleanNIPostcodeData.csv")

# combine all of the crime data from each csv file into one dataset called AllNICrimeData.
crime_data <- list.files('Crime/', recursive = TRUE, full.names = TRUE)
AllNICrimeData <- do.call(rbind, lapply(crime_data, read.csv))

# Write the Crime data files to CSV file called AllNICrimeData
write.csv(AllNICrimeData, file = "AllNICrimeData.csv")
# 
nrow(AllNICrimeData)
# Remove the columns Crime.ID, Reported.by, Falls.within, LSOA.code, LSOA.name,Last.outcome.category and Context
Delete_Cols <- c("Crime.ID","Reported.by","Falls.within","LSOA.code","LSOA.name","Last.outcome.category","Context")
#Delete all the columns that are in the vector called Delete_Cols
AllNICrimeData <- AllNICrimeData[,!(names(AllNICrimeData) %in% Delete_Cols)]

# Check to see what class crime.type is
class(AllNICrimeData$Crime.type)
# Assign factor type to crime.type
AllNICrimeData$Crime.type <- as.factor(AllNICrimeData$Crime.type)
# Check to see what class crime.type is post the change
class(AllNICrimeData$Crime.type)

head(AllNICrimeData, 5)
# Using the gsub function to remove the "On or near" from the Location column
AllNICrimeData$Location <- gsub("On or near ", "", AllNICrimeData$Location)
#After removing the "On or near", replace empty cells with NA in the location column
AllNICrimeData[AllNICrimeData$Location == "", c("Location")] <- NA 

# Creating a dataframe called AllNICrimeData_noNA which has the NA's removed
AllNICrimeData_noNA <- na.omit(AllNICrimeData)
# Create dataframe called random_crime_sample and Choose a random sample of 1000 entries
# from the AllNICrimeData_noNA dataframe 
random_crime_sample_no_PC <- AllNICrimeData_noNA[c(as.integer(runif(1000,1,nrow(AllNICrimeData_noNA)))),]

# Declare a function called find_a_postcode
# pass the dataframe "random_crime_sample_no_PC" to the function
find_a_postcode <- function(random_crime_sample_no_PC){
  library(doBy)
  # Read in CleanNIPostcodeData csv file and populate the dataframe CleanNIPostcodeData
  CleanNIPostcodeData <- read.csv("CleanNIPostcodeData.csv")
  # Summary of the postcode data grouped by Primary.Thorfare to get the most popular postcode
  Postcode_Summary <- summaryBy(data = CleanNIPostcodeData, Primary.Thorfare ~ Primary.Thorfare+Postcode,FUN=length)
  # Remove all the rows with NA's
  Postcode_Summary <- na.omit(Postcode_Summary)
  
  #Getting the maximum value for each location giving the most popular postcode
  Most_Freq_Address_by_Postcode<-summaryBy(Primary.Thorfare.length ~ Primary.Thorfare, data = Postcode_Summary,FUN = max)
  Most_Freq_Address_by_Postcode$Postcode<-""
  # Create a for loop to iterate though the postcode and crime data
  # so that every entry primary thorfare has a postcode attached
  for (i in 1:nrow(Most_Freq_Address_by_Postcode)){
    #Create temporary variable which reads the dataframe Most_Freq_Address_by_Postcode and outputs the primary thorfare 
    temp_Primary.Thorfare <- as.character(Most_Freq_Address_by_Postcode[i,c("Primary.Thorfare")])
    #Create temporary varable which outputs the corresponding value of the count of Primary thorfare
    temp_Primary.Thorfare.length.max <- as.character(Most_Freq_Address_by_Postcode[i,c("Primary.Thorfare.length.max")])
    # Using the postcode_summary join the postcode data to the crime data by Primary Thorfare and max length
    temp_Postcode <- as.character(Postcode_Summary[which(Postcode_Summary$Primary.Thorfare==temp_Primary.Thorfare &
                                                   Postcode_Summary$Primary.Thorfare.length==temp_Primary.Thorfare.length.max),c("Postcode")])
    # If multiple postcodes appear with the same length.max, choose the first one
    Most_Freq_Address_by_Postcode[i,c("Postcode")]<-temp_Postcode[1]
    # Print the progress of the for loop as a percentage
    print(paste0((i/nrow(Most_Freq_Address_by_Postcode)*100),"%"))
    }
  return(Most_Freq_Address_by_Postcode)  
}


#Calling function find_a_postcode passing in "random_crime_sample_no_PC"
Most_Freq_Address_by_Postcode <- find_a_postcode(random_crime_sample_no_PC)

# Change all the location in random crime sample s to upper case so it can be merged
random_crime_sample_no_PC$Location <- toupper(random_crime_sample_no_PC$Location)

# Join the two dataframes together by Location and Primary Thorfare so that the random crime sample contains a postcode
random_crime_sample <- merge(random_crime_sample_no_PC,Most_Freq_Address_by_Postcode,by.x = c("Location"),by.y = c("Primary.Thorfare"),
                             all.x = TRUE)

# Write the random_crime_sample data files to CSV file called random_crime_sample
write.csv(random_crime_sample, file = "random_crime_sample.csv")


# Rename the random_crime_sample dataframe that it only contains the columns given below
updated_random_sample <- random_crime_sample[,c("Month","Longitude","Latitude",
                                                "Location","Crime.type","Postcode")]
#Rename the dataframe updated_random_sample to chart_data.
chart_data <- updated_random_sample

library(dplyr)

# Sort/filter the data where Postcode contains the postcode BT1.
chart_data <- arrange(chart_data[grepl("BT1",chart_data$Postcode),],Postcode) 
# Creata a dataframe called chart_data_summary and create a summary by crime type
chart_data_summary <- summaryBy(data = chart_data, Crime.type ~ Crime.type,FUN=length)
str(chart_data_summary)
chart_data_summary
str(chart_data_summary)

# Create a bar graph using ggplot
ggplot(data=chart_data_summary,aes(Crime.type,Crime.type.length))+
  theme(panel.grid.minor = element_line(colour = "grey"), plot.title = element_text(size = rel(1)),axis.text.x = element_text(angle=90, vjust=1), strip.text.x = element_text(size = 12, colour = "black", face = "bold")) +  
  geom_point(alpha = 0.6, position = position_jitter(w = 0.1, h = 0.0), aes(colour=factor(Crime.type)), size =4)+
  geom_bar(stat="identity",fill="steelblue")+
  ggtitle("Crime Summary for Postcode BT1") +
  scale_size_area() + 
  xlab("Crime Type")+ 
  ylab("Crime Count") 