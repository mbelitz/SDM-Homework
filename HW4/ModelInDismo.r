## SEt the folder to the species folder
setwd(choose.dir())


library(raster)
library(dismo)
library(maxnet)


## Read the occurrence table
occtbl = read.csv("./Occurrences/cleaned_Chaetodipus_baileyi.csv")
dim(occtbl)

## Generate random number to separate training and testing occurrence data
i = sample(seq(1,nrow(occtbl)), nrow(occtbl)* 0.30, replace = FALSE)

occtbl[, "scientificName"] = "C_baileyi"

## training and testing data separated, only 3 
test_occr = occtbl[i, c("scientificName", "decimalLongitude", "decimalLatitude")]
train_occr = occtbl[-i, c("scientificName", "decimalLongitude", "decimalLatitude")]


write.csv(test_occr, "./Occurrences/test_C_baileyi.csv", row.names =FALSE)
write.csv(train_occr, "./Occurrences/train_C_baileyi.csv", row.names =FALSE)

## Get the list of .asc files which are used as predictors
files <- list.files("./CalibrationFiles/", pattern='asc', full.names=TRUE )

## create a stack of predictors
predictors <- stack(files)
crs(predictors)="+proj=longlat +datum=WGS84"

## Get presences data but only latitiude and longitude. 
occur <- train_occr[,-1]
names(occur)<-c("lon","lat")


### Models in Maxnet

## Here I am converting all grids to points, so that I can take background points randomly. 
All_pts = rasterToPoints(predictors)
bkg_ind = sample(seq(1,nrow(All_pts)), 4000, replace = FALSE)
bkg_val = All_pts[bkg_ind, c(3:ncol(All_pts))]


pres_val = extract(predictors, occur)
preds1 = rbind(pres_val, bkg_val)


pres = c(rep(1,nrow(occur)), rep(0,nrow(bkg_val)))
preds1 = data.frame(preds1)


# where 'pres' is a vector of 0's / 1's and preds1 is a matrix of the relevant covariates at those locations. 

# To tune, we can add some statements:
modeltune <- maxnet(pres, preds1,  regmult=3, f = maxnet.formula(pres, preds1, classes="lq"))

# where 'regmult' is the regularization beta and 'classes' refers to the features, here linear and quadratic. 
# The full set can be written as  classes=‘lqhpt’ (linear, quadratic, hinge, product, threshold).

# Response curves:
plot(modeltune , type="cloglog") #can also do "link" or "logistic"

# Map it:
predict(predictors, modeltune, clamp=T, type="cloglog")




### Models in Dismo

## Maxent arguments for dismo is listed here. https://urldefense.proofpoint.com/v2/url?u=https-3A__groups.google.com_forum_-23-21topic_maxent_yRBlvZ1-5F9rQ&d=DwIGaQ&c=sJ6xIWYx-zLMB3EPkvcnVg&r=nNnY6X5WY58RgeQXOKDQ61FWZnWJi1m_cldgGu59VxM&m=skM0YtecCzIghUzqVpUqwFdpGX3fReLIMh72UsSFje8&s=Ug_bbiC0ZCyJyEIlwiEMNQh8aW-vBSsd6uTAqY755Qw&e= . 


## only linear and regularization is 1 
arguments <- c("-J", "-P", "-q", "-p", "-h", "randomtestpoints=30", "replicates=3", "betamultiplier=1.0", "askoverwrite=false", "threads=6")
mod_lin <- maxent(x=predictors, p=coordinates(occur), args=arguments) 
mod_lin

CaliPred1 = predict(mod_lin,predictors)
plot(CaliPred2)


## only linear and regularization is 0.5 
arglist <- c("-J", "-P", "-q", "-p", "-h", "randomtestpoints=30", "replicates=3", "betamultiplier=0.5", "askoverwrite=false", "threads=6")
mod_lin_reg.05 <- maxent(x=predictors, p=coordinates(occur), args=arglist) 
mod_lin_req.05
CaliPred2 = predict(mod_lin_req.05,predictors)
plot(CaliPred2)


## only linear and regularization is 5 
arglist <- c("-J", "-P", "-q", "-p", "-h", "randomtestpoints=30", "replicates=10", "betamultiplier=5", "askoverwrite=false", "threads=6")
mod_lin_reg5 <- maxent(x=predictors, p=coordinates(occur), args=arglist) 
mod_lin_req5



## only product and regularization is 1 
argumemts <- c("-J", "-P", "-q", "-l", "-h", "product=true", "autofeature=false", "randomtestpoints=30", "betamultiplier=1.0", "askoverwrite=false", "threads=6")
mod_pro <- maxent(x=predictors, p=coordinates(occur), args=argumemts) 
mod_pro


## only product and regularization is 0.5 
arglist <- c("-J", "-P", "-q", "-l", "-h", "product=true", "autofeature=false", "randomtestpoints=30", "replicates=10", "betamultiplier=0.5", "askoverwrite=false", "threads=6")
mod_pro_reg.05 <- maxent(x=predictors, p=coordinates(occur), args=arglist) 
mod_pro_reg.05


## only product and regularization is 5 
arglist <- c("-J", "-P", "-q", "-l", "-h", "product=true", "autofeature=false", "randomtestpoints=30", "replicates=10", "betamultiplier=5", "askoverwrite=false", "threads=6")
mod_pro_reg5 <- maxent(x=predictors, p=coordinates(occur), args=arglist) 
mod_pro_reg5



## Default parameters
argumemts <- c("-J", "-P", "randomtestpoints=30", "replicates=10", "betamultiplier=1.0", "askoverwrite=false", "threads=6")
mod_aut <- maxent(x=predictors, p=coordinates(occur), args=argumemts) 
mod_aut


## Default parameters and regularization is 0.5 
argumemts <- c("-J", "-P", "randomtestpoints=30", "replicates=10", "betamultiplier=0.5", "askoverwrite=false", "threads=6")
mod_aut_reg.05 <- maxent(x=predictors, p=coordinates(occur), args=argumemts) 
mod_aut_reg.05


## Default parameters and regularization is 5 
argumemts <- c("-J", "-P", "randomtestpoints=30", "replicates=10", "betamultiplier=5", "askoverwrite=false", "threads=6")
mod_aut_reg5 <- maxent(x=predictors, p=coordinates(occur), args=argumemts) 
mod_aut_reg5



