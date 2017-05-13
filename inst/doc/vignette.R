## ---- fig.show='hold'----------------------------------------------------
library(mbgraphic)

## ----echo=TRUE,cache = TRUE,fig.width=1000-------------------------------
data(Election2013)
dim(Election2013)

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  iaunivariate(Election2013)

## ----fig.width=7, fig.height=4.5,echo=FALSE------------------------------
library(png)
library(grid)
img <- readPNG("iaunivariate.png")
 grid.raster(img)

## ----echo=TRUE-----------------------------------------------------------
election_num <- Election2013[,sapply(Election2013,is.numeric)] 
dim(election_num)

## ---- eval=FALSE---------------------------------------------------------
#  iacorrgram(election_num)

## ----fig.width=7, fig.height=3.5,echo=FALSE------------------------------
library(png)
library(grid)
img <- readPNG("iacorrgram.png")
 grid.raster(img)

## ----eval=TRUE-----------------------------------------------------------
vc <- varclust(Election2013,mincor=0.8)
summary(vc)
# the reduced data set
election_reduced <- vc$dfclusrep
dim(election_reduced)

## ----eval=TRUE-----------------------------------------------------------
scagdf <- sdf(election_reduced)
# List of class "sdfdata"
class(scagdf)
# Entries 
summary(scagdf)

## ----eval=TRUE-----------------------------------------------------------
addscag <- scag2sdf(election_reduced,scagfun.list=list(dcor2d=dcor2d,splines2d=splines2d))
# merge 'addscag' and 'scagsdf'
scagdf2 <- mergesdfdata(scagdf,addscag)
# merged list contains	11 scagnostics
names(scagdf2$sdf)

## ----eval=FALSE----------------------------------------------------------
#  iascagpcp(scagdf2)

## ----fig.width=7, fig.height=4.5,echo=FALSE------------------------------
library(png)
library(grid)
img <- readPNG("iascagpcp.png")
grid.raster(img)

## ----results='hide',eval=FALSE-------------------------------------------
#  iascaggram(scagdf)

## ----fig.width=7, fig.height=4.5,echo=FALSE------------------------------
library(png)
library(grid)
img <- readPNG("iascaggram.png")
grid.raster(img)

## ----fig,echo=FALSE, results='hide',eval=FALSE---------------------------
#  set.seed(4768987)
#  # choose some of the demographic and structural variables of the reduced data frame
#  election_ds <- election_reduced[,sample(1:35,20)]
#  scagdf_ds <- sdf(election_ds)
#  addscag_ds <- scag2sdf(election_ds,scagfun.list=list(dcor2d=dcor2d,splines2d=splines2d))
#  scagdf2_ds <- mergesdfdata(scagdf_ds,addscag_ds)
#  iascaggram(scagdf2_ds)

## ----fig.width=7, fig.height=4,echo=FALSE--------------------------------
library(png)
library(grid)
img <- readPNG("iascaggram2.png")
grid.raster(img)

