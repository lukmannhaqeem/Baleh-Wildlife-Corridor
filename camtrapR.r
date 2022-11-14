########## Loading/Attaching camtrapR package
#install.packages("camtrapR")
library(camtrapR)
citation("camtrapR")





##########1. ORGANISING RAW CAMERA TRAP IMAGES IN camtrapR

##1.1 Load camera trap station information
camtrap <- read.csv("camtrap.csv")                      
camtrap[is.na(camtrap)] <- ""
str(camtrap)

##1.2 Adjust date and time of images (If time set wrongly!)
TimeShiftTable <- camtrap[,c(1,8,9)]
head(TimeShiftTable)

##1.3 Copy sample images to another location
wd_images_raw <- paste(getwd(), "/named", sep = "")

##1.4 Apply time shift to images
timeshift_run <- timeShiftImages(inDir                = wd_images_raw,
                                 timeShiftTable       = TimeShiftTable,
                                 stationCol           = "station",
								 cameraCol            = "camera",
                                 hasCameraFolders     = FALSE,
                                 timeShiftColumn      = "timeshift",
                                 timeShiftSignColumn  = "sign")

##1.5 Renaming images
#Destination for renamed images to be copied to
wd_images_renamed <- file.path("camtrapR", "renamed")       
#Apply renaming of files with format = StationID__Date__Time(X).JPG
# ---> ONLY RUN THIS WHEN EXIFTOOL IS AVAILABLE
if (Sys.which("exiftool") != ""){        
	renaming.table2 <- imageRename(inDir     = wd_images_raw,
						outDir               = wd_images_renamed,       
						hasCameraFolders     = FALSE,
				        keepCameraSubfolders = FALSE,
						copyImages           = TRUE,
						writecsv             = FALSE
	)
}							   
list.files(wd_images_renamed, recursive = TRUE)
head(renaming.table2)





##########2. IDENTIFYING SPECIES AND INDIVIDUALS

##2.1 Species identification via drag & drop of images
wd_createSpeciesFolders <- file.path("camtrapR", "createSpeciesFolders")
#create the station directories
StationFolderCreate <- createStationFolders (inDir       = wd_createSpeciesFolders,
                                             stations    = as.character(camtrap$station), 
											 createinDir = TRUE)

##2.2 Species names for which we want to create subdirectories
species <- c("Banded Palm Civet", "Great Argus Pheasant", "Leopard Cat",
			 "Mongoose sp", "Sunbear", "Thick-spined Porcupine")

##2.3 Create species directories (folders)
SpeciesFolderCreate1 <- createSpeciesFolders (inDir               = wd_createSpeciesFolders,
                                              species             = species,
                                              hasCameraFolders    = FALSE,
                                              removeFolders       = FALSE)

##################### NOW IS THE TIME TO MOVE (NOT COPY) ######################
########### IMAGES OF SPECIES INTO THE PREPARED SPECIES DIRECTORIES ###########
############FROM "RENAMED" TO "CREATESPECIESFOLDERS"###########################

##2.4 Delete empty species directories
SpecFolderCreate2 <- createSpeciesFolders (inDir               = wd_createSpeciesFolders,
                                           species             = species,
                                           hasCameraFolders    = FALSE,
                                           removeFolders       = TRUE)





##########3. EXTRACTING DATA FROM CAMERA TRAPPING IMAGES
 
##3.1 Tabulating species records
#find the directory with sample images contained in the package
wd_images_ID <- file.path("camtrapR", "createSpeciesFolders")

##3.2 See how many independent observations (images) we have
length(list.files(wd_images_ID, pattern 	    = "JPG", recursive = TRUE))
rec_table <- recordTable(inDir                	= wd_images_ID,
						 IDfrom                 = "directory",
						 minDeltaTime           = 60,
						 stationCol             = "station",
						 writecsv 		    	= TRUE,
						 outDir 		    	= file.path("camtrapR", "createSpeciesFolders"),
						 camerasIndependent	    = FALSE,
						 deltaTimeComparedTo    = "lastIndependentRecord",
						 exclude                = "NO_ID",
						 timeZone               = "Asia/Kuala_Lumpur",
						 removeDuplicateRecords = TRUE)
#check if there is any more duplicates (output should be [1] 0, meaning no duplicates)
anyDuplicated(rec_table[, c("station", "Species", "DateTimeOriginal")])   

##3.3 Load the camera trap station table
camop_problem <- cameraOperation(CTtable      = camtrap,								
                                 stationCol   = "station",
								 setupCol     = "setup_date",
                                 retrievalCol = "retrieval_date",
                                 writecsv     = FALSE,
                                 hasProblems  = FALSE,				# ---> hasProblems=FALSE (NO FAULTY CAMERA)
                                 dateFormat   = "%d/%m/%Y")
								 
##3.4 Plot the camera operation matrix.
camopPlot <- function(camOp){
	which.tmp <- grep(as.Date(colnames(camOp)), pattern = "01$")
	label.tmp <- format(as.Date(colnames(camOp))[which.tmp], "%Y-%m")
	at.tmp <- which.tmp / ncol(camOp)
	cex = 0.5  
	image(t(as.matrix(camOp)), xaxt = "n", yaxt = "n", col = c("red", "grey70"))
	axis(1, at = at.tmp, labels = label.tmp)
	axis(2, at = seq(from = 0, to = 1, length.out = nrow(camOp)), labels = rownames(camOp), las = 1)
	abline(v = at.tmp, col = rgb(0,0,0, 0.2))
	box()
}
par(mfrow = c(1,1), mar = c(3, 10, 2, 2))
camopPlot(camOp = camop_problem)

##3.5 Create camera operation matrix
camop_no_problem <- cameraOperation(CTtable      = camtrap,
                                    stationCol   = "station",
                                    setupCol     = "setup_date",
                                    retrievalCol = "retrieval_date",
									hasProblems  = FALSE,
                                    dateFormat   = "%d/%m/%Y")

##3.6 Make detection history (without trapping effort)
dir.create(paste(getwd(), "/camtrapR/detectionHistories", sep = ""))

for(i in 1:length(species)){
	detHist <- detectionHistory(recordTable          = rec_table,						 
								camOp                = camop_no_problem,
								stationCol           = "station",
								speciesCol           = "Species",
								recordDateTimeCol    = "DateTimeOriginal",
								species              = species[i],                            
								occasionLength       = 1,
								day1                 = "station",
								includeEffort        = FALSE,
								timeZone             = "Asia/Kuala_Lumpur")
	dh <- (detHist$detection_history)
	write.csv(dh, paste(getwd(), "/camtrapR/detectionHistories/", "dh_", species[i], ".csv", sep = "")) 
}
 
 
 
 
 
##########4. EXPLORATORY DATA ANALYSIS AND VISUALISATION 

##4.1 Species presence maps
dir.create(paste(getwd(), "/camtrapR/EDA", sep = ""))

#create a map of the number of observed species
Mapstest <- detectionMaps(CTtable           = camtrap,
                          recordTable       = rec_table,
                          Xcol              = "brsoX_m",
                          Ycol              = "brsoY_m",
                          stationCol        = "station",
                          speciesCol        = "Species",
                          writePNG          = TRUE,
						  plotDirectory	  	= paste(getwd(), "/camtrapR/EDA/", sep = ""),
                          plotR             = TRUE,
                          printLabels       = TRUE,
                          richnessPlot      = TRUE,
                          addLegend         = TRUE)

##4.2 Visualising species activity data
#all species
activityDensity(recordTable 		= rec_table,
                allSpecies 			= TRUE,
                writePNG    		= TRUE,
				plotDirectory		= paste(getwd(), "/camtrapR/EDA/", sep = ""),
                plotR       		= TRUE,
                add.rug     		= TRUE)


#single species activity plots
#define species of interest
speciesA_activity <- "Banded Civet"

#...visualise as density plot
activityDensity(recordTable = rec_table,
                species     = speciesA_activity)
	
#...visualise as histogram
activityHistogram (recordTable = rec_table,
                   species     = speciesA_activity)

#...visualise as radial plot
activityRadial(recordTable  = rec_table,
               species      = speciesA_activity,
               lwd          = 3,      			    
			   cex	    	= 0.5)

#two species activity overlap plot
#define species of interest
speciesA_for_activity <- "Banded Civet"    
speciesB_for_activity <- "Great Argus Pheasant"    

# create activity overlap plot
activityOverlap (recordTable   = rec_table,
                 speciesA      = speciesA_for_activity,
                 speciesB      = speciesB_for_activity,
                 writePNG      = TRUE,
				 plotDirectory = paste(getwd(), "/camtrapR/EDA/", sep = ""),
                 plotR         = TRUE,
                 createDir     = FALSE,
                 pngMaxPix     = 1000,
                 linecol       = c("red", "blue"),
                 linewidth     = c(3, 3),
                 add.rug       = TRUE,
				 main 	       = paste(speciesA_for_activity, " & ", speciesB_for_activity, sep = ""))
				
##4.3 Survey summary report
dir.create(paste(getwd(), "/camtrapR/summaryReports", sep = ""))

#without camera problems
reportTest <- surveyReport (recordTable          = rec_table,
                            CTtable              = camtrap,
                            speciesCol           = "Species",
                            stationCol           = "station",
							setupCol             = "setup_date",
                            retrievalCol         = "retrieval_date",
                            CTDateFormat         = "%d/%m/%Y", 
                            recordDateTimeCol    = "DateTimeOriginal",
                            recordDateTimeFormat = "%Y-%m-%d %H:%M:%S")
length(reportTest)  
# camera trap operation times and image date ranges
write.csv(file = paste(getwd(), "/camtrapR/summaryReports/", "camera trap operation times and image date ranges.csv", sep = ""), reportTest[[1]])
# number of species by station
write.csv(file = paste(getwd(), "/camtrapR/summaryReports/", "number of species by station.csv", sep = ""), reportTest[[2]])
# number of events and number of stations by species
write.csv(file = paste(getwd(), "/camtrapR/summaryReports/", "number of events and number of stations by species.csv", sep = ""), reportTest[[3]])
# number of species events by station
write.csv(file = paste(getwd(), "/camtrapR/summaryReports/", "number of species events by station.csv", sep = ""), reportTest[[4]])
# number of species events by station including 0s (non-observed species)
write.csv(file = paste(getwd(), "/camtrapR/summaryReports/", "number of species events by station including 0s (non-observed species).csv", sep = ""), reportTest[[5]])

#with camera problems
reportTest_problem <- surveyReport (recordTable          = rec_table,
                                    CTtable              = camtrap,
                                    speciesCol           = "Species",
                                    stationCol           = "station",
									setupCol             = "setup_date",
                                    retrievalCol         = "retrieval_date",
									CTDateFormat         = "%d/%m/%Y", 
                                    recordDateTimeCol    = "DateTimeOriginal",
									recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                                    CTHasProblems        = TRUE)
length(reportTest_problem)
# camera trap operation times and image date ranges
write.csv(file = paste(getwd(), "/camtrapR/summaryReports/", "camera trap operation times and image date ranges_with camera problems.csv", sep = ""), reportTest_problem[[1]])
# number of species by station
write.csv(file = paste(getwd(), "/camtrapR/summaryReports/", "number of species by station_with camera problems.csv", sep = ""), reportTest_problem[[2]])
# number of events and number of stations by species
write.csv(file = paste(getwd(), "/camtrapR/summaryReports/", "number of events and number of stations by species_with camera problems.csv", sep = ""), reportTest_problem[[3]])
# number of species events by station
write.csv(file = paste(getwd(), "/camtrapR/summaryReports/", "number of species events by station_with camera problems.csv", sep = ""), reportTest_problem[[4]])
# number of species events by station including 0s (non-observed species)
write.csv(file = paste(getwd(), "/camtrapR/summaryReports/", "number of species events by station including 0s (non-observed species)_with camera problems.csv", sep = ""), reportTest_problem[[5]])

	
