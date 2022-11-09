##i. Loading/Attaching camtrapR package
install.packages("camtrapR")
library(camtrapR)

##1. ORGANISING RAW CAMERA TRAP IMAGES IN camtrapR
##1.1 Camera trap station information
camtrap <- read.csv("camtrap_mp.csv")                      
str(camtrap)

#1.2 Change 'NA' in data to 'blank' (if camera operation is not affected)
camtrap[is.na(camtrap)] <- ""
head(camtrap)

#1.3 Adjust date and time of images (if time set wrongly)
TimeShiftTable <- read.csv("timeshift_mp - copy.csv")
head(TimeShiftTable)

#1.4 Copy sample images to another location
wd_images_raw <- "D:/Melatai Para FMU/Raw_mp"

#1.5 Apply time shift to images
timeshift_run <- timeShiftImages(inDir                = wd_images_raw,
                                 timeShiftTable       = TimeShiftTable,
                                 stationCol           = "station",
				         cameraCol            = "camera",
                                 hasCameraFolders     = TRUE,
                                 timeShiftColumn      = "timeshift",
                                 timeShiftSignColumn  = "sign")

#1.6 Renaming images
#Destination for renamed images to be copied to
wd_images_renamed <- file.path("EDA", "Renamed")       

#Apply renaming of files with format = StationID__Date__Time(X).JPG
# ---> ONLY RUN THIS WHEN EXIFTOOL IS AVAILABLE
if (Sys.which("exiftool") != ""){        
	renaming.table2 <- imageRename(inDir               = wd_images_raw,
						outDir               = wd_images_renamed,       
						hasCameraFolders     = TRUE,
					      keepCameraSubfolders = TRUE,
						copyImages           = TRUE,
						writecsv             = FALSE
	)
}							   
list.files(wd_images_renamed, recursive = TRUE)
head(renaming.table2)

##2. IDENTIFYING SPECIES AND INDIVIDUALS
##################################################
##2.1 Species identification
##2.2 Species identification via drag & drop of images
wd_createSpeciesFolders <- file.path("EDA", "createSpeciesFolders")

#Create the station directories
StationFolderCreate <- createStationFolders (inDir       = wd_createSpeciesFolders,
                                             stations    = as.character(camtrap$station), 
           						   cameras     = as.character(camtrap$camera),
							   createinDir = TRUE)

# 2.2 Species names for which we want to create subdirectories
species <- c("banded civet", "bearded pig", "bird sp", "bay cat", "mongoose sp", "dog",  
             "leopard cat", "buffy fish owl", "long-tailed porcupine", "malayan civet", 
		 "mousedeer sp", "human", "muntjac sp", "pangolin", "pig-tailed macaque", 
             "sambar deer", "greater coucal", "treeshrew sp", "thick-spined porcupine", 
             "rat sp", "yellow-throated marten", "great argus pheasant", "hose civet", 
             "crested partridge", "eagle sp", "giant squirrel", "malayan porcupine", 
             "squirrel sp", "crested serpent eagle", "tufted ground squirrel",
		 "long-tailed macaque", "white-fronted langur", "langur sp", "sun bear", 
             "snake sp", "clouded leopard", "marbled cat", "monitor lizard", "owl sp", 
             "binturong", "crested partridge")

# 2.3 Create species directories (folders)
SpeciesFolderCreate1 <- createSpeciesFolders (inDir               = wd_createSpeciesFolders,
                                              species             = species,
                                              hasCameraFolders    = TRUE,
                                              removeFolders       = FALSE)

##################### NOW IS THE TIME TO MOVE (NOT COPY) ######################
########### IMAGES OF SPECIES INTO THE PREPARED SPECIES DIRECTORIES ###########
############FROM "RENAMED" TO "CREATESPECIESFOLDERS"########################
# 2.4 Delete empty species directories
SpecFolderCreate2 <- createSpeciesFolders (inDir               = wd_createSpeciesFolders,
                                           species             = species,
                                           hasCameraFolders    = TRUE,
                                           removeFolders       = TRUE)

##3. EXTRACTING DATA FROM CAMERA TRAPPING IMAGES 
##3.1 Tabulating species records
#find the directory with sample images contained in the package
wd_images_ID <- "D:/Melatai Para FMU/EDA (Phase 1 & 2)/createSpeciesFolders" #file.path("EDA", "sample_images")

# 3.2 See how many JPG images we have
length(list.files(wd_images_ID, pattern 	    = "JPG", recursive = TRUE))
rec_table <- recordTable(inDir                = wd_images_ID,
                       IDfrom                 = "directory",
                       minDeltaTime           = 60,
			     stationCol             = "station",
	                 writecsv 		    = TRUE,
			     outDir 		    = "D:/Melatai Para FMU/EDA (Phase 1 & 2)",
			     camerasIndependent	    = FALSE,
                       deltaTimeComparedTo    = "lastIndependentRecord",
                       exclude                = "NO_ID",
                       timeZone               = "Asia/Kuala_Lumpur",
                       removeDuplicateRecords = TRUE)

length(list.files(wd_images_ID, pattern 	    = "JPG", recursive = TRUE))
rec_table <- recordTable(inDir                = wd_images_ID,
                       IDfrom                 = "directory",
                       minDeltaTime           = 30,
			     stationCol             = "station",
	                 writecsv 		    = TRUE,
			     outDir 		    = "D:/Melatai Para FMU/EDA (Phase 1 & 2)",
			     camerasIndependent	    = FALSE,
                       deltaTimeComparedTo    = "lastIndependentRecord",
                       exclude                = "NO_ID",
                       timeZone               = "Asia/Kuala_Lumpur",
                       removeDuplicateRecords = TRUE)

length(list.files(wd_images_ID, pattern 	    = "JPG", recursive = TRUE))
rec_table <- recordTable(inDir                = wd_images_ID,
                       IDfrom                 = "directory",
                       minDeltaTime           = 0,
			     stationCol             = "station",
	                 writecsv 		    = TRUE,
			     outDir 		    = "D:/Melatai Para FMU/EDA (Phase 1 & 2)",
			     camerasIndependent	    = FALSE,
                       deltaTimeComparedTo    = "lastIndependentRecord",
                       exclude                = "NO_ID",
                       timeZone               = "Asia/Kuala_Lumpur",
                       removeDuplicateRecords = TRUE)

anyDuplicated(rec_table[, c("station", "Species", "DateTimeOriginal")])   # to check if there is any more duplicates, output should be [1] 0, meaning no duplicates

tmp <- camtrap[-c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,
                  40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,
			74,76,78,80,82,84,86,88,90,92,94,96,98,100),-2]
tmp
View(tmp)
write.csv(tmp, file = "species richness.csv")


A <- read.csv("D:/Melatai Para FMU/EDA/number of species by station.csv")
A

# 3.3 Load the camera trap station table
camop_problem <- cameraOperation(CTtable      = tmp,								#hasProblems=FALSE (NO FAULTY CAMERA)
                                 stationCol   = "station",
                                 #cameraCol   = "camera",
					   setupCol     = "setup_date",
                                 retrievalCol = "retrieval_date",
					   #byCamera    = TRUE,
                                 writecsv     = FALSE,
                                 hasProblems  = FALSE,
                                 dateFormat   = "%d/%m/%Y")
								 
tmp[,-which(colnames(tmp) %in% c("brso_y", "brso_x"))]

# 3.4 Plot the camera operation matrix.
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

# 3.5 Create camera operation matrix
camop_no_problem <- cameraOperation(CTtable      = tmp,
                                    stationCol   = "station",
						#cameraCol	 = "camera",
                                    setupCol     = "setup_date",
                                    retrievalCol = "retrieval_date",
                                    #byCamera 	 = TRUE,
						hasProblems  = FALSE,
                                    dateFormat   = "%d/%m/%Y")

# 3.6 Make detection history (without trapping effort)
#st <- "PT01"
#c.no <- camop_no_problem[rownames(camop_no_problem) == st,]
#station <- recordTableSample.pt[recordTableSample.pt$Station == st, ]

##1. pig-tailed macaque
DetHist1 <- detectionHistory(recordTable         = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "pig-tailed macaque",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist1$detection_history)
dh <- (DetHist1$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/pig-tailed macaque.csv") 

###make the same dh for other species that are detected

##2. long-tailed macaque
DetHist2 <- detectionHistory(recordTable         = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "long-tailed macaque",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist2$detection_history)
dh <- (DetHist2$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/long-tailed macaque.csv") 


## 3. banded civet
DetHist3 <- detectionHistory(recordTable         = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "banded civet",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist3$detection_history)
dh <- (DetHist3$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/banded civet.csv")

## 4. bearded pig
DetHist4 <- detectionHistory(recordTable         = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "bearded pig",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist4$detection_history)
dh <- (DetHist4$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/bearded pig.csv")

## 5. bird sp
DetHist5 <- detectionHistory(recordTable         = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "bird sp",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist5$detection_history)
dh <- (DetHist5$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/bird sp.csv")

## 6. bay cat
DetHist6 <- detectionHistory(recordTable         = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "bay cat",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist6$detection_history)
dh <- (DetHist6$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/bay cat.csv")

## 7. clouded leopard
DetHist7 <- detectionHistory(recordTable         = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "clouded leopard",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist7$detection_history)
dh <- (DetHist7$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/clouded leopard.csv")

## 8. crested serpent eagle
DetHist8 <- detectionHistory(recordTable         = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "crested serpent eagle",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist8$detection_history)
dh <- (DetHist8$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/crested serpent eagle.csv")

## 9. dog
DetHist9 <- detectionHistory(recordTable         = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "dog",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist9$detection_history)
dh <- (DetHist9$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/dog.csv")

## 10. eagle sp
DetHist10 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "eagle sp",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist10$detection_history)
dh <- (DetHist10$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/eagle sp.csv")

## 11. giant squirrel
DetHist11 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "giant squirrel",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist11$detection_history)
dh <- (DetHist11$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/giant squirrel.csv")

## 12. crested partridge
DetHist12 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "crested partridge",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist12$detection_history)
dh <- (DetHist12$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/crested partridge.csv")

## 13. great argus pheasant
DetHist13 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "great argus pheasant",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist13$detection_history)
dh <- (DetHist13$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/great argus pheasant.csv")

## 14. greater coucal
DetHist14 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "greater coucal",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist14$detection_history)
dh <- (DetHist14$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/greater coucal.csv")

## 15. human
DetHist15 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "human",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist15$detection_history)
dh <- (DetHist15$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/human.csv")

## 16. hose's civet
DetHist16 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "hose civet",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist16$detection_history)
dh <- (DetHist16$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/hose's civet.csv")

## 17. binturong
DetHist17 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "binturong",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist17$detection_history)
dh <- (DetHist17$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/binturong.csv")

## 18. leopard cat
DetHist18 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "leopard cat",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist18$detection_history)
dh <- (DetHist18$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/leopard cat.csv")

## 19. malayan civet
DetHist19 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "malayan civet",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist19$detection_history)
dh <- (DetHist19$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/malayan civet.csv")

## 20. malayan porcupine
DetHist20 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "malayan porcupine",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist20$detection_history)
dh <- (DetHist20$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/malayan porcupine.csv")

## 21. mongoose sp
DetHist21 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "mongoose sp",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist21$detection_history)
dh <- (DetHist21$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/mongoose sp.csv")

## 22. mousedeer sp
DetHist22 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "mousedeer sp",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist22$detection_history)
dh <- (DetHist22$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/mousedeer sp.csv")

## 23. muntjac sp
DetHist23 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "muntjac sp",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist23$detection_history)
dh <- (DetHist23$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/muntjac sp.csv")

## 24. marbled cat
DetHist24 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "marbled cat",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist24$detection_history)
dh <- (DetHist24$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/marbled cat.csv")

## 25. monitor lizard
DetHist25 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "monitor lizard",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist25$detection_history)
dh <- (DetHist25$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/monitor lizard.csv")

## 26. owl sp
DetHist26 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "owl sp",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist26$detection_history)
dh <- (DetHist26$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/owl sp.csv")

## 27. pangolin
DetHist27 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "pangolin",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist27$detection_history)
dh <- (DetHist27$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/pangolin.csv")

## 28. langur sp
DetHist28 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "langur sp",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist28$detection_history)
dh <- (DetHist28$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/langur sp.csv")

## 29. sambar deer
DetHist29 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "sambar deer",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist29$detection_history)
dh <- (DetHist29$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/sambar deer.csv")

## 30. snake sp
DetHist30 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "snake sp",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist30$detection_history)
dh <- (DetHist30$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/snake sp.csv")

## 31. squirrel sp
DetHist31 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "squirrel sp",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist31$detection_history)
dh <- (DetHist31$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/squirrel sp.csv")

## 32. sun bear
DetHist32 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "sun bear",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist32$detection_history)
dh <- (DetHist32$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/sun bear.csv")

## 33. rat sp
DetHist33 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "rat sp",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist33$detection_history)
dh <- (DetHist33$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/rat sp.csv")

## 34. thick-spined porcupine
DetHist34 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "thick-spined porcupine",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist34$detection_history)
dh <- (DetHist34$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/thick-spined porcupine.csv")

## 35. treeshrew sp
DetHist35 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "treeshrew sp",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist35$detection_history)
dh <- (DetHist35$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/treeshrew sp.csv")

## 36. tufted ground squirrel
DetHist36 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "tufted ground squirrel",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist36$detection_history)
dh <- (DetHist36$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/tufted ground squirrel.csv")

## 37. white-fronted langur
DetHist37 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "white-fronted langur",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist37$detection_history)
dh <- (DetHist37$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/white-fronted langur.csv")

## 38. yellow-throated marten
DetHist38 <- detectionHistory(recordTable        = rec_table,						#ERROR STARTS HERE
                            camOp                = camop_no_problem,
                            stationCol           = "station",
                            speciesCol           = "Species",
                            recordDateTimeCol    = "DateTimeOriginal",
                            species              = "yellow-throated marten",                            
				    occasionLength       = 1,
                            day1                 = "station",
                            includeEffort        = FALSE,
                            timeZone             = "Asia/Kuala_Lumpur"
)

#View(DetHist38$detection_history)
dh <- (DetHist38$detection_history)
write.csv(dh, "D:/Melatai Para FMU/EDA/yellow-throated marten.csv")


##4. EXPLORATORY DATA ANALYSIS AND VISUALISATION 
##################################################
##4.1 Species presence maps
#Create a map of the number of observed species.

Mapstest <- detectionMaps(CTtable           = camtrap,
                          recordTable       = rec_table,
                          Xcol              = "brso_x",
                          Ycol              = "brso_y",
                          stationCol        = "station",
                          speciesCol        = "Species",
                          writePNG          = TRUE,
				  plotDirectory	  = "D:/Melatai Para FMU/EDA",
                          plotR             = TRUE,
                          printLabels       = TRUE,
                          richnessPlot      = TRUE,
                          addLegend         = TRUE
)

# 4.2 Visualising all species activity data
# all species at once
rec_table$DateTimeOriginal <- as.factor(rec_table$DateTimeOriginal)
activityDensity(recordTable 		= rec_table,
                allSpecies 		= TRUE,
                writePNG    		= TRUE,
		    plotDirectory		= "D:/Melatai Para FMU/EDA",
                plotR       		= TRUE,
                add.rug     		= TRUE)

#Single-species activity plots
speciesA_activity <- "bearded pig"
activityDensity(recordTable = rec_table,
                species     = speciesA_activity)
	
#Histogram
activityHistogram (recordTable = rec_table,
                   species     = speciesA_activity)

#Radial plot
activityRadial(recordTable  = rec_table,
               species      = speciesA_activity,
               lwd          = 3,      			    
		   cex	    = 0.5)
				
#Single-species activity plots
species4activity <- x <- "muntjac sp"
activityDensity(recordTable = rec_table,
                species     = species4activity)
		    day <- 6.42	# Mean sunrise time in Limbang 2015
		  night <- 18.3	# Mean sunset time in Limbang 2015
		abline(v=c(day, night), lwd=1, col="red")

	#Proportion of records occuring during the day or night	
	sp <- rec_table[rec_table$Species == x,]
	head(sp)
	sp.time <- ClocktimeToTimeRad(sp$DateTimeOriginal)
	x.lt <- as.POSIXlt(sp$DateTimeOriginal)
	x.hhdd <- x.lt$hour + x.lt$min/60 + x.lt$sec/3600
	mean(x.hhdd < day)	# time of sunrise=6.29am; sunset=18.45pm 
	mean(x.hhdd >= day & x.hhdd <= night)
	mean(x.hhdd > night)
	mean(x.hhdd < day) + mean(x.hhdd > night)
	
#Histogram
activityHistogram (recordTable = recordTableSample,
                   species     = speciesA_activity)

#Radial plot
activityRadial(recordTable  = recordTableSample,
               species      = speciesA_activity,
               lwd          = 3,     # adjust line with of the plot
		   cex	    = 0.5)

#Two-species activity plots
#Define species of interest
speciesA_activity <- "bearded pig"
speciesB_activity <- "pig-tailed macaque"

#Create activity overlap plot
# define species of interest
speciesA_for_activity <- "bearded pig"    
speciesB_for_activity <- "people"    

# create activity overlap plot
activityOverlap (recordTable = rec_table,
                 speciesA    = speciesA_for_activity,
                 speciesB    = speciesB_for_activity,
                 writePNG    = FALSE,
                 plotR       = TRUE,
                 createDir   = FALSE,
                 pngMaxPix   = 1000,
                 linecol     = c("red", "blue"),
                 linewidth   = c(3,3),
                 add.rug     = TRUE,
		     main 	     = "bearded pigs and humans"
)

# 4.3 Survey summary report
#without camera problems
# Survey summary report
reportTest <- surveyReport (recordTable          = rec_table,
                            CTtable              = camtrap,
                            speciesCol           = "Species",
                            stationCol           = "station",
                            cameraCol		 = "camera",
				    setupCol             = "setup_date",
                            retrievalCol         = "retrieval_date",
                            CTDateFormat         = "%d/%m/%Y", 
                            recordDateTimeCol    = "DateTimeOriginal",
                            recordDateTimeFormat = "%Y-%m-%d %H:%M:%S")

class(reportTest)  # a list with
length(reportTest) # 5 elements

reportTest[[1]]    # camera trap operation times and image date ranges
reportTest[[2]]    # number of species by station
reportTest[[3]]    # number of events and number of stations by species
reportTest[[4]]    # number of species events by station
reportTest[[5]]    # number of species events by station including 0s (non-observed species)
write.csv(file="D:/Melatai Para FMU/EDA/camera trap operation times and image date ranges.csv", reportTest[[1]])
write.csv(file="D:/Melatai Para FMU/EDA/number of species by station.csv", reportTest[[2]])
write.csv(file="D:/Melatai Para FMU/EDA/number of events and number of stations by species.csv", reportTest[[3]])
write.csv(file="D:/Melatai Para FMU/EDA/number of species events by station.csv", reportTest[[4]])
write.csv(file="D:/Melatai Para FMU/EDA/number of species events by station including 0s (non-observed species).csv", reportTest[[5]])

### Create bar chart on number of events by species ###
dat <- read.csv("D:/Melatai Para FMU/EDA/Survey Summary Report/number of events and number of stations by species.csv")
dat <- dat[order(dat$n_events),]
str(dat)
png("NumberOfEventsBySpecies.png", res=300, height=2000, width=4000)
par(mar=c(6,10,3,3))
barplot(height=dat$n_events[1:39], names=dat$species, cex.lab=0.7, 
		cex.axis=0.7, cex.names=0.7,
        	col="skyblue", las=1, horiz=TRUE, xlim=c(0,500),
		ylab="", 
		xlab="Frequency",
		main="Number of events by species")
text(dat$n_events, seq(0.7,46.4, length=39), labels=dat$n_events, 
cex=0.6, pos=4)

dev.off()

# with camera problems

reportTest_problem <- surveyReport (recordTable          = rec.db1,
                                    CTtable              = camtrap,
                                    speciesCol           = "Species",
                                    stationCol           = "Station",
                                    cameraCol  		   = "Camera",
						setupCol             = "Setup_date",
                                    retrievalCol         = "Retrieval_date",
                                    CTDateFormat         = "%d-%b-%y", 
                                    recordDateTimeCol    = "DateTimeOriginal",
                                    recordDateTimeFormat = "Y-%m-%d %H:%M:%S",
                                    CTHasProblems        = TRUE)

reportTest_problem$survey_dates



#Extra
#--------------------
ClocktimeToTimeRad <- function(Clocktime,
                               timeformat = "%Y-%m-%d %H:%M:%S"){

  DateTime2 <- strptime(as.character(Clocktime), format = timeformat, tz = "UTC")
  Time2     <- format(DateTime2, format = "%H:%M:%S", usetz = FALSE)
  Time.rad  <- (as.numeric(as.POSIXct(strptime(Time2, format = "%H:%M:%S", tz = "UTC"))) -
                  as.numeric(as.POSIXct(strptime("0", format = "%S", tz = "UTC")))) / 3600 * (pi/12)
  return(Time.rad)
}

recordTableSample.pm$TimeRad <- ClocktimeToTimeRad(recordTableSample.pm$DateTimeOriginal)
head(recordTableSample.pm$TimeRad)
# choose species
speciesA <- "Bearded Pig"
speciesB <- "Clouded Leopard"

# set legend parameters. You can play around with these
legendPosition <- "topright"
legendText <- c(speciesA)
lty.tmp <- c(1,2)
lwd.tmp <- c(1,2)
col.tmp <- c("red","blue")

# do plot
overlapPlot(A = recordTableSample.pm[recordTableSample.pm$Species == speciesA, "TimeRad"], 
            B = recordTableSample.pm[recordTableSample.pm$Species == speciesB, "TimeRad"],
            rug = TRUE, main = "Activity overlap of sp. A and sp. B in Payeh Maga")
			abline(v=c(day, night), lty=3)

# add legend
legend(x = legendPosition,
       legend = legendText,
       lty = lty.tmp,
       lwd = lwd.tmp,
       col = col.tmp,
       bg = "white")

#do plot with more than two species
min_number_records <- 2                                                                                               		# minimum number of records per species
species_to_keep <- names(table(recordTableSample.pm$Species))[table(recordTableSample.pm$Species) >= min_number_records]    # list all those species
recordTableSample.pm2 <- recordTableSample.pm[recordTableSample.pm$Species %in% species_to_keep,]                           # subset

list_records_radial_time <- activityDensity(recordTable = recordTableSample.pm2,
                allSpecies  = TRUE,
                writePNG    = FALSE,
                plotR       = F)

str(list_records_radial_time)
names(list_records_radial_time)

densities <- lapply(list_records_radial_time, densityPlot)
plot(densities[[1]], type = "l", ylim = c(0, max(sapply(densities, FUN = function(df){df$y}))))   # plot densities of first species in list
lines(densities[[2]], col = 2)
lines(densities[[3]], col = 3)
lines(densities[[4]], col = 4)
lines(densities[[5]], col = 5)
#lines(densities[[6]], col = 7)
#lines(densities[[7]], col = 8)
#lines(densities[[8]], col = 9)
#lines(densities[[9]], col = 10)
#lines(densities[[10]], col = 11)


# Get example data:
data(simulatedData)

# Do basic plot with defaults:
overlapPlot(pigObs, tigerObs)
# Make it prettier:
overlapPlot(tigerObs, pigObs, linet = c(1,1), linec = c("red", "blue"),
  rug=TRUE, extend="lightgreen", main="Simulated data")
legend("topleft", c("Tiger", "Pig"), lty=1, col=c("red", "blue"), bg="white")
# Add vertical dotted lines to mark sunrise (05:30) and sunset (18:47):
# (times must be in hours if the x-axis is labelled in hours)
abline(v=c(5.5, 18+47/60), lty=3)

# A plot centered on midnight:
overlapPlot(pigObs, tigerObs, xcenter = "m", rug=TRUE)
# Mark sunrise/sunset; values to the left of "00:00" are negative
# so subtract 24:
abline(v=c(6.17, (18+47/60) - 24), lty=3)

# Do initial plot with Hunter data:
pm <- data.frame(recordTableSample.pm$Station, recordTableSample.pm$Species, recordTableSample.pm$TimeRad)

	# Time is in days, ie. 0 to 1: Convert to radians:
	timeRad <- pm$recordTableSample.pm.TimeRad # * 2*pi  # already covert to radians

densityPlot(timeRad[pm$recordTableSample.pm.Species == "Hunter"], ylim=c(0,0.35), main="Activity patterns for civets")
densityPlot(timeRad[pm$recordTableSample.pm.Species == "Muntjac sp"], lty=2, col="red", add=TRUE)
densityPlot(timeRad[pm$recordTableSample.pm.Species == "Sambar Deer"], lty=3, col="blue", add=TRUE)
#densityPlot(timeRad[pm$recordTableSample.pm.Species == "Bornean Banded Pitta"], lty=4, col="dark green", add=TRUE)
densityPlot(timeRad[pm$recordTableSample.pm.Species == "Malay Civet"], lty=4, col="orange", add=TRUE)
#densityPlot(timeRad[pm$recordTableSample.pm.Species == "Great Argus Pheasant"], lty=4, col="magenta", add=TRUE)
densityPlot(timeRad[pm$recordTableSample.pm.Species == "Clouded Leopard"], lty=4, col="cyan", add=TRUE)

source("[function]_labelArrow.R")
labelArrow("Hunter", 4)
    #text(9.949370, 0.068748, "Masked Palm Civet", pos=4) 
    #arrows(9.949370, 0.068748, 7.780431, 0.063898, length=0.1) 
labelArrow("Hose's Civet", 4)
    #text(14.090072, 0.064418, "Hose's Civet", pos=4) 
    #arrows(14.090072, 0.064418, 11.395329, 0.058530, length=0.1) 
labelArrow("Malay Civet", 4)
    #text(14.418699, 0.011769, "Malay Civet", pos=4) 
    #arrows(14.418699, 0.011769, 13.301367, 0.020428, length=0.1) 
	
	
	
