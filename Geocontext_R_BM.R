##################################################
# Creating geocontexts in R
# Chris Fowler October 17, 2019
#
# This script provides a demonstration run through of a set of functions contained in the file
# geocontext_functions.R These functions replicate the outputs of the Equipop software as modified
# by Pontus Hennerdal. The primary difference between the functions employed here and those done by Pontus
# are the use of fast distance matrix construction and a rethinking of the function structure to employ
# fast joins rather than record by record loops. The time savings of this approach are significant
#
#  The example here uses registry data located in the folder Bo/Geocontext/Kernels 
##############################################
##############################################
#Setup: All variable names and case by case changes to the function calls are made in this section.
# to run the geocontext function on your own data, just change code within this code block
##############################################
# Load required packages
suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(ggplot2)
})

# Set the path to your data directory
# Users should change this to match their own system
data_dir <- "path/to/your/data/directory"

# Define input/output paths
#input data
#Note that R requires forward slashes / when used on Windows machines. I think it has slightly different behavior on Mac or Linux
pop.file <- file.path(data_dir, "sample_data.csv")
#Output file location
outfile <- data_dir


#will have k range and year appended to it. Separate files for each year
#Years for which we will conduct analysis
yearList=c(2019)
#Names of the columns in the above that contain population counts for groups
#Code below is more complex than necessary with smaller variable lists or a single year you could just type:
#groupList = c("[variable1]","[variable2]") #single year
#groupList<-list(c("[var1year1]","[var2year1]"),c("[var1year2]","[var2year2]")) #multiple years requires list form
if(length(yearList)>1){
  groupList<-list(paste0("cl", 1:40, yearList[1]))
  for(i in 2:length(yearList)){
    tmp<-paste0("cl",1:40, "_sum")
    groupList[[i]]<-tmp
  }
}else{
  groupList<-list(paste0("cl", 1:40, "_sum"))
}

#Name of the column containing total population for the cell
#totalID = "sum_1990" #single year format
totalID = paste0('population',yearList) #note creates separate values for multiple years
#Variable names for the X and Y coordinates in the population file
Y.coord = 'ykoordsw'
X.coord = 'xkoordsw'
#Name of unique id column
rnID=NA #this data set does not have a unique id, so the program will create one.

#The list of k-values contains the size of the context. If k=1000, statistics will be calculated for the 1000 nearest neighbours for every point in pointsNoContextDF
kValueList = c(400, 1600, 6400, 25600)

#Should the process be run on a small sample to quickly check if results are reasonable?
samp.le=FALSE
samp.size= 10000
#Can the output be truncated to just the Stockholm Labor Market Area placed in a convex hull then +20km in each direction
#be sure to remove the 20km at the end
StHolm=FALSE
################################################
#That is all. With those variables set the code below should run for most cases
################################################
###############################################
#Run analysis: this section operates automatically based on the values entered above.


# Source geocontext functions
source(file.path(data_dir, "geocontext_functions.R"))


###Read in the file with the population locations and point locations
popLocationsDF = read.csv(pop.file,sep =',')

#subset to predetermined bounding box
if(StHolm==TRUE){
  xmin=1529184
  ymin=6494424
  xmax=1724522
  ymax=6742651
  popLocationsDF<-popLocationsDF[(popLocationsDF[,X.coord]>xmin & popLocationsDF[,X.coord]<xmax) &
                                 (popLocationsDF[,Y.coord]>ymin & popLocationsDF[,Y.coord]<ymax),]
}

#if running on a sample, reduce file size
if(samp.le==TRUE){
  popLocationsDF<-popLocationsDF[sample(nrow(popLocationsDF),samp.size),] #sample for testing
}
#print first rows of file just read in
head(popLocationsDF)

##Standardize certain column names to simplify code
#create unique id if one isn't present, standardize column name otherwise
if(is.na(rnID)){
  popLocationsDF$rn<-1:length(popLocationsDF[,1])
}else{
  colnames(popLocationsDF)[colnames(popLocationsDF)==rnID]<-"rn"
}
colnames(popLocationsDF)[colnames(popLocationsDF)==X.coord]<-"X"
colnames(popLocationsDF)[colnames(popLocationsDF)==Y.coord]<-"Y"

#Run the function
for(i in 1:length(yearList)){
  print(paste0("Begin calculations for year ",yearList[i]))
  #subset data columns
  popUse<-popLocationsDF[,colnames(popLocationsDF)%in%c("rn","X","Y","RutStl",unlist(c(totalID[i],groupList[[i]]))) ]
  colnames(popUse)[colnames(popUse)==unlist(totalID[i])]<-"Total"
  pointsWithContext <- geocontext(
    popLocations=popUse,
    groups=unlist(groupList[[i]]) ,
    kValues=kValueList
  )
  #output result to file
  save(pointsWithContext,file=paste0(outfile,"geocontext_k",kValueList[[1]],"_to_",kValueList[length(kValueList)],"_",yearList[[i]],".Rdata"))
}
# Save as CSV
csv_filename <- file.path(data_dir, "result.csv")
fwrite(pointsWithContext, file = csv_filename)
message("Results saved as CSV to: ", csv_filename)
