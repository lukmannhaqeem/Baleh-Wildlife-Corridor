###LOAD DATA
###LOAD DATA
###LOAD DATA
###LOAD DATA
###LOAD DATA
#load sampling site information
st <- read.csv(file.choose())
#check for correlation between covariates
correlation <- cor(st[,c(4:9)])
round(correlation, 2)
#install.packages("corrplot")
library(corrplot)
corrplot(correlation, method = "color", diag = FALSE,
		 type = "upper", order = "hclust", 
         tl.col = "black", addCoef.col = "black")
#load sampling site observations
dat <- read.csv(file.choose())
#truncate observations to 90 days
dat1 <- dat[1:42, 54:143]
colnames(dat1) <- 1:90
dat2 <- dat[43:63, 2:91]
colnames(dat2) <- 1:90
heh <- rbind(dat1, dat2)
rownames(heh) <- st$station
heh
#calculate naive occupancy 
mean(rowSums(heh, na.rm = TRUE) > 0) * 100

### MODELING
#install.packages("unmarked")
library(unmarked)
umf <- unmarkedFrameOccu(
    y = heh[,1:90],                              
    siteCovs = data.frame(elevation = st$elevation_m,
						  elevation2 = st$elevation_m^2,
						  slope = st$slope_deg,
						  canopy = st$canopyTopHeight_m,
                          dSettlement = st$distanceToNearestSettlement_m,
                          dWater = st$ distanceToNearestMainRiver_m,
                          dWater2 = st$ distanceToNearestRiverAndStream_m))
summary(umf)
#run a few models
m1 <- occu(~1 ~1, data = umf)
m2 <- occu(~1 ~elevation, data = umf)
m3 <- occu(~1 ~slope, data = umf)
m4 <- occu(~1 ~canopy, data = umf)
m5 <- occu(~1 ~dSettlement, data = umf)
m6 <- occu(~1 ~dWater, data = umf)
m7 <- occu(~1 ~dWater2, data = umf)
#model selection
#install.packages("AICcmodavg")
library(AICcmodavg)
fit.list <- aictab(c(m1, m2, m3, m4, m5, m6, m7))
fit.list


### MAKING PREDICTIONS
### FOR ELEVATION
pred.e <- data.frame(
    elevation = seq(min(umf@siteCovs$elevation, na.rm = TRUE),  # min elevation cover
                    max(umf@siteCovs$elevation, na.rm = TRUE),  # max elevation cover
                    length = 100))                              # number of points in range
pred.elevation <- predict(
    m2,                        # model to predict from
    type = "state",             # sub model to predict from
    newdata = pred.e,           # new values to use for predictions
    append = TRUE)              # add new data to prediction data frame
#install.packages("ggplot2")
library(ggplot2)
ggplot() +
    geom_line(aes(x = pred.elevation$elevation, y = pred.elevation$Predicted*100),
              color = "blue", size = 1) +
    geom_ribbon(aes(x = pred.elevation$elevation, ymin = pred.elevation$lower*100,
                ymax = pred.elevation$upper*100), fill = "skyblue", alpha = 0.1) +
    ylab("Pr(Occupancy)") +
    xlab("Elevation (m)") + 
    ylim(0, 100) +    
    theme_minimal()

### FOR SLOPE
pred.s <- data.frame(
    slope = seq(min(umf@siteCovs$slope, na.rm = TRUE),  		# min elevation cover
                max(umf@siteCovs$slope, na.rm = TRUE),  		# max elevation cover
                length = 100))                              	# number of points in range
pred.slope <- predict(
    m3,                        # model to predict from
    type = "state",             # sub model to predict from
    newdata = pred.s,           # new values to use for predictions
    append = TRUE)              # add new data to prediction data frame
#install.packages("ggplot2")
library(ggplot2)
ggplot() +
    geom_line(aes(x = pred.slope$slope, y = pred.slope$Predicted*100),
              color = "blue", size = 1) +
    geom_ribbon(aes(x = pred.slope$slope, ymin = pred.slope$lower*100,
                ymax = pred.slope$upper*100), fill = "skyblue", alpha = 0.1) +
    ylab("Pr(Occupancy)") +
    xlab("Slope (deg)") + 
    ylim(0, 100) +    
    theme_minimal()


### FOR CANOPY TOP HEIGHT
pred.c <- data.frame(
    canopy = seq(min(umf@siteCovs$canopy, na.rm = TRUE),  		# min elevation cover
                    max(umf@siteCovs$canopy, na.rm = TRUE),  	# max elevation cover
                    length = 100))                              # number of points in range
pred.canopy <- predict(
    m4,                        # model to predict from
    type = "state",             # sub model to predict from
    newdata = pred.c,           # new values to use for predictions
    append = TRUE)              # add new data to prediction data frame
ggplot() +
    geom_line(aes(x = pred.canopy$canopy, y = pred.canopy$Predicted*100),
              color = "blue", size = 1) +
    geom_ribbon(aes(x = pred.canopy$canopy, ymin = pred.canopy$lower*100,
                ymax = pred.canopy$upper*100), fill = "skyblue", alpha = 0.1) +
    ylab("Pr(Occupancy)") +
    xlab("Canopy top height (m)") + 
    ylim(0, 100) +    
    theme_minimal()


### FOR DISTANCE TO NEAREST RIVER
pred.w <- data.frame(
    dWater = seq(min(umf@siteCovs$dWater, na.rm = TRUE),  		# min elevation cover
                    max(umf@siteCovs$dWater, na.rm = TRUE),  	# max elevation cover
                    length = 100))                              # number of points in range
pred.dWater <- predict(
    m6,                        	# model to predict from
    type = "state",             # sub model to predict from
    newdata = pred.w,           # new values to use for predictions
    append = TRUE)              # add new data to prediction data frame
ggplot() +
    geom_line(aes(x = pred.dWater$dWater, y = pred.dWater$Predicted*100),
              color = "blue", size = 1) +
    geom_ribbon(aes(x = pred.dWater$dWater, ymin = pred.dWater$lower*100,
                ymax = pred.dWater$upper*100), fill = "skyblue", alpha = 0.1) +
    ylab("Pr(Occupancy)") +
    xlab("Distance to nearest main river (m)") + 
    ylim(0, 100) +    
    theme_minimal()


### FOR DISTANCE TO NEAREST RIVER
pred.w2 <- data.frame(
#    dWater2 = seq(min(umf@siteCovs$dWater2, na.rm = TRUE),  		# min elevation cover
#                    max(umf@siteCovs$dWater2, na.rm = TRUE),  	# max elevation cover
#                    length = 100))    
     dWater2 = water2_x[,3])                              # number of points in range
pred.dWater2 <- predict(
    m7,                        	# model to predict from
    type = "state",             # sub model to predict from
    newdata = pred.w2,           # new values to use for predictions
    append = TRUE)              # add new data to prediction data frame
ggplot() +
    geom_line(aes(x = pred.dWater2$dWater2, y = pred.dWater2$Predicted*100),
              color = "blue", size = 1) +
    geom_ribbon(aes(x = pred.dWater2$dWater2, ymin = pred.dWater2$lower*100,
                ymax = pred.dWater2$upper*100), fill = "skyblue", alpha = 0.1) +
    ylab("Pr(Occupancy)") +
    xlab("Distance to nearest main river (m)") + 
    ylim(0, 100) +    
    theme_minimal()

X <- water2_x[,1]
Y <- water2_x[,2]
Z <- pred.dWater2$Predicted
r <- rasterFromXYZ(data.frame(x = X, y = Y, z = Z))
plot(r)

vertex_hl <- read.csv(file.choose())
vertex_km <- read.csv(file.choose())
vertices_hl <- cbind(vertex_hl[,2], vertex_hl[,3])
vertices_km <- cbind(vertex_km[,2], vertex_km[,3])
samp <- nrow(vertices_hl)

vertices_km <- vertices_km[sample(1:nrow(vertices_km), samp, replace = FALSE),]
tr <- transition(r, transitionFunction = f, directions = 16)
slope <- geoCorrection(tr, type = "c", multpl = FALSE, scl = FALSE)
plot(elevation, main = "Which path does a mongoose take going from Hose-Laga NP to Kayan-Mentarang NP?")
points(settlement[,2], settlement[,3], pch = 16, col = "red")
for(i in 1:samp){
	for(j in 1:samp){
		#catch error
		tryCatch({
			tmp <- shortestPath(slope, vertices_hl[i,], vertices_km[j,])
			tmp2 <- shortestPath(slope, vertices_hl[j,], vertices_km[i,])
			if (is.infinite(tmp@extent@xmin))
				stop("Well, shit happens sometime!")
			if (is.infinite(tmp2@extent@xmin))
				stop("Well, shit happens sometime!")
		
		#plot if error-free
		plot(shortestPath(slope, vertices_hl[i,], vertices_km[j,], output = "SpatialLines"),
			col = adjustcolor("black", 0.01), lwd = 1, add = TRUE)
		plot(shortestPath(slope, vertices_hl[j,], vertices_km[i,], output = "SpatialLines"),
			col = adjustcolor("black", 0.01), lwd = 1, add = TRUE)
		}, error=function(e){})
	}
}


