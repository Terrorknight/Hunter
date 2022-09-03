## how to install MPZAnalyticsR from Github
## Need devtools package
## 
## 
## # To install from a private repo, use auth_token with a token
# from https://github.com/settings/applications. You only need the
# repo scope. Best practice is to save your PAT in env var called
# GITHUB_PAT.
# 
# current PAT generated from Settings/Developer settings 
# d8f1e509586ad2acb1de732a6220b1228c985c3e
# 
# 

# install_github("hadley/private", auth_token = "abc")
# 
# GITHUB_PAT <- "d8f1e509586ad2acb1de732a6220b1228c985c3e"
# 
# 
# 
# install.packages("devtools")
# 
# require(devtools)
# install_github('Terrorknight/MPZAnalyticsR', auth_token = "d8f1e509586ad2acb1de732a6220b1228c985c3e", force = TRUE)
# devtools::install_github('Terrorknight/MPZAnalyticsR')
# install_github('Terrorknight/MPZAnalyticsR')

install.packages("tidyverse")
install.packages(c("XML", "httr", "sp"))
install.packages(c("xml2", "DT", "rvest", "measurements"))
install.packages("RSelenium")
install.packages("data.table")

require(tidyverse)
require(DT)
require(measurements)
#require(rvest)
#require(sf)
#require(httr)
require(XML)
require(xml2)

install.packages(c("waffle","magrittr", "ggplot2"))
library(waffle)
library(magrittr)
library(hrbrthemes)
library(ggplot2)
## Step 1 Well Index & Shape Files from ND ------------------------------
## source(file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsR/R/premiumServicesFunctions.R')
## get the wellIndex File

## AGEO ENERGY new login
## MPZAnalyticsR package github not working for now source the files directly
source("~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsR/R/premiumServicesFunctions.R")

## AGEO ENERGY new login
get.wellIndexFile(ndUserName            = "ageoenergy",
                  ndPassword            = "Wood4Lumber",
                  wellIndexDownloadPath = '~/Downloads/NDWell_Index.zip')


## cleanup the wellIndex file
tbl.wellIndex <- cleanupNDWellIndexFile(currentWellIndexFileName = "~/Downloads/NDWell_Index.zip")
# tbl.wellIndex <- cleanupNDWellIndexFile(currentWellIndexFileName = "~/Downloads/Well_Index.zip")
## save the wellIndex file
saveRDS(tbl.wellIndex, file = "~/Downloads/MPZAnalyticsRrdsFiles/tbl.NDwellIndex.rds")

## read in tbl.WellIndex
tbl.wellIndex              <- readRDS(file = "~/Downloads/MPZAnalyticsRrdsFiles/tbl.NDwellIndex.rds")

## Step 2 Download all shape files ------------------
system.time(get.wellShapeFiles(ndUserName       = "ageoenergy",
                               ndPassword       = "Wood4Lumber",
                               ndShapeFilesPath = '~/Downloads/NDShapeFiles'))

# system.time(get.wellShapeFiles(ndUserName       = "iddgroupllc",
#                                ndPassword       = "Snow2Day",
#                                ndShapeFilesPath = '~/Downloads/NDShapeFiles'))
                   

### Step 2a Create all nd shape files ---------------------
###  these files require tbl.wellIndex to be downloaded first
source(file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsR/R/CreateShapeFileRDS.R')

system.time(create.OGD_GasPlants.shapeRDS())

system.time(create.OGD_DrillingSpacingUnits.shapeRDS())
system.time(create.OGD_Horizontals.shapeRDS())
system.time(create.OGD_Horizontals_Line.shapeRDS())
system.time(create.OGD_OilFields.shapeRDS())
system.time(create.OGD_Wells.shapeRDS())
system.time(create.Sections.shapeRDS())
system.time(create.OGD_Seismic2DPreplot.shapeRDS())
system.time(create.OGD_Seismic3DPreplot.shapeRDS())
system.time(create.OGD_Rigs.shapeRDS())
system.time(create.Townships.shapeRDS())
system.time(create.OGD_Directionals_Line.shapeRDS())
system.time(create.OGD_Directionals.shapeRDS())
system.time(create.CountyBoundaries.shapeRDS())
system.time(create.OGD_UnitBoundaries.shapeRDS())


## read in all sf shape files
OGD_GasPlants.shape            <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/OGD_GasPlants.shape.rds')
OGD_Horizontals_Line.shape     <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/OGD_Horizontals_Lines.shape.rds')
OGD_Wells.shape                <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/OGD_Wells.shape.rds')
Sections.shape                 <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/Sections.shape.rds')
OGD_OilFields.shape            <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/OGD_OilFields.shape.rds')
OGD_DrillingSpacingUnits.shape <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/OGD_DrillingSpacingUnits.shape.rds')
OGD_Seismic2DPreplot.shape     <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/OGD_Seismic2DPreplot.shape.rds')
OGD_Seismic3DPreplot.shape     <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/OGD_Seismic3DPreplot.shape.rds')
Rigs.shape                     <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/Rigs.shape.rds')
Townships.shape                <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/Townships.shape.rds')
OGD_Directionals_Line.shape    <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/OGD_Directionals_Line.shape.rds')
OGD_Directionals.shape         <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/OGD_Directionals.shape.rds')
OGD_Horizontals.shape          <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/OGD_Horizontals.shape.rds')
CountyBoundaries.shape         <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/CountyBoundaries.shape.rds')
OGD_UnitBoundaries.shape       <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/OGD_UnitBoundaries.shape.rds')



## Step 2b Create HorizontalsEndPoints -----------------

## 
source(file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsR/R/DSUShapeFunctions.R')

## this function is used primarily for 3D calculations on what DSU a well ends in
system.time(create.OGD_HorizontalsEndPoints.shape(OGD_Horizontals.shape = OGD_Horizontals.shape,
                                                             rdsFilePath       = '~/Downloads/MPZAnalyticsRrdsFiles',
                                                             listName          = '/OGD_HorizontalsEndPoints.shape.rds'))
## Read in the HorizontalsEndPoints.shape
OGD_HorizontalsEndPoints.shape <- readRDS(file = "~/Downloads/MPZAnalyticsRrdsFiles/OGD_HorizontalsEndPoints.shape.rds")




## Step 3 Get Log Tops data ----------------------------------
## For KBElev and GLElev by FileNo
## and for future Formation tops plotting in 3D using
## tbl.FormationTopDefinitions <- paste_from_clipboard()  Downloaded Manually with copy paste from Definitions
##  https://www.dmr.nd.gov/oilgas/FeeServices/codehelpss.asp (Code Type Log Formation)
##  
## saveRDS(tbl.FormationTopDefinitions, file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.FormationTopDefinitions.rds')
## tbl.FormationTopDefinitions <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.FormationTopDefinitions.rds')
## These match up to the column headers in tbl.LogTops
## 
## 
#tbl.LogTops <- MPZAnalyticsR::get.logTops(ndUserName = "iddgroupllc", ndPassword = "Snow2Day", logTopsDownloadPath = '~/Downloads/LogTops.zip')
system.time(get.logTops(ndUserName          = "ageoenergy", 
                                       ndPassword          = "Wood4Lumber", 
                                       logTopsDownloadPath = '~/Downloads/LogTops.zip'))

cleanup.LogTops(currentFileName = "~/Downloads/LogTops.zip",
                csvFileName     = "LogTops.csv",
                rdsFilePath     = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.LogTops.rds')

## read in tbl.LogTops
tbl.LogTops <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.LogTops.rds')

install.packages("netstat")
require(RSelenium)
require(tidyverse)
require(xml2)
## Step 4 Scrape Bakken Horizontal Wells By Producing Zone -------------------------------
#' scrape.bakkenwells.asp
#' uses docker and Rselenium to access data tables
#' "https://www.dmr.nd.gov/oilgas/bakkenwells.asp"
#'
#' tbl.targetFormations <- dplyr::tribble( ~tgtF,  ~targetFormation,
#'                                         "MB",   "Middle Bakken",
#'                                         "TF",   "Three Forks",
#'                                         "MBTF", "Middle Bakken / Three Forks",
#'                                         "LP",   "Lodgepole",
#'                                         "UBS",  "Upper Bakken Shale",
#'                                         "LPMB", "Lodgepole / Middle Bakken")
#'
#' @import RSelenium
#' @import rvest
#' @import xml2
#'
#' @param rdsFilePath normally '~/Downloads/MPZAnalyticsRrdsFiles'
#'
#' @return nothing saves file tbl.nd
#' @export
#'
#' @examples
#' \dontrun{
#'
#' Must install docker!!
#' and execute commands in terminal
#'
#' KnightAwesome:~ doucetteemail$ docker pull selenium/standalone-chrome
#' Using default tag: latest
#' latest: Pulling from selenium/standalone-chrome
#' Digest: sha256:c478febf26729b7cc27e2fac7d10fa79cf2519b5ab83f9cf82705248258b6495
#' Status: Image is up to date for selenium/standalone-chrome:latest
#' KnightAwesome:~ doucetteemail$ docker run -d -p 4445:4444 selenium/standalone-chrome
#'
#' I used chrome, make sure these commands are executed BEFORE running
#'
#' ## UPDATE 09-01-2022 ##
#' Docker no longer needed
#'
#'  scrape.bakkenwells.asp(rdsFilePath = '~/Downloads/MPZAnalyticsRrdsFiles')
#'  tbl.ndTargetFormationByWell <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.ndTargetFormationByWell.rds')
#' }
#'
#'
#'
scrape.bakkenwells.asp <- function(rdsFilePath = '~/Downloads/MPZAnalyticsRrdsFiles'){
  # library(RSelenium)
  # library(rvest)
  # library(xml2)
  # library(tidyverse)
  #
  #
  # 
  # KnightAwesome:~ doucetteemail$ docker pull selenium/standalone-chrome
  # Using default tag: latest
  # latest: Pulling from selenium/standalone-chrome
  # Digest: sha256:c478febf26729b7cc27e2fac7d10fa79cf2519b5ab83f9cf82705248258b6495
  # Status: Image is up to date for selenium/standalone-chrome:latest
  # KnightAwesome:~ doucetteemail$ docker run -d -p 4445:4444 selenium/standalone-chrome
  library(RSelenium)
  
  ## update with new M1 chip on apple
  ## download the latest chrome
  ## see RSelenium-troubleshooting.pdf
  
  rD <- rsDriver(browser = "chrome",
                 port    = 4545L,
                 check   = TRUE,
                 verbose = F)
  # rD$server$log()
  remDr <- rD[["client"]]
  remDr$navigate("https://www.dmr.nd.gov/oilgas/bakkenwells.asp") #Entering our URL gets the browser to navigate to the page
  
  # remDr$screenshot(display = TRUE) #This will take a screenshot and display it in the RStudio viewer
  
  tbl.selectFormation <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
    rvest::html_nodes("select") %>%
    rvest::html_children() %>%
    rvest::html_text() %>%
    dplyr::tibble(FormationName = ., listOption = 1:length(.) )
  
  
 
  
  # remDr$screenshot(display = TRUE) #This will take a screenshot and display it in the RStudio viewer
  
  
  
  # {xml_nodeset (7)}
  # [1] <option selected value="SF">Select Formation</option>\n
  # [2] <option value="B">Middle Bakken</option>\n
  # [3] <option value="TF">Three Forks</option>\n
  # [4] <option value="BTF">Middle Bakken / Three Forks</option>\n
  # [5] <option value="L">Lodgepole</option>\n
  # [6] <option value="UB">Upper Bakken Shale</option>\n
  # [7] <option value="LB">Lodgepole / Middle Bakken</option>
  
  ##element <- remDr$findElement(using = 'css selector', "select > option:nth-child(2)")
  
  
  #' scrape.tableData
  #'
  #' scrape.tableData takes tbl.SelectFormation and interates
  #' clickElement() and rvest::htm_table(fill = TRUE) for each
  #' target formation in the list
  #'
  #'
  #' @param FormationName
  #' @param listOption
  #'
  #' @return tbl of all pages data
  #' @export
  #'
  #' @examples
  scrape.tableData <- function(FormationName, listOption){
    ## get time stamp for cat later
    myStartTime = Sys.time()
    ## select selected formation dropdown box list option
    
    element <- remDr$findElement(using = 'css selector', paste0("select > option:nth-child(",listOption,")"))
    cat('===load element$clickElement() for ',FormationName,":Formation webpage \n")
    ## click on the option
    element$clickElement()
    myEndTime.Element = Sys.time()
    cat("Tick Tock Tick Tock === element$clickElement() total time: ",myEndTime.Element - myStartTime, " \n")
    ## use xml2::read_html to read in the page file data
    bakkenwells_html <- xml2::read_html(remDr$getPageSource()[[1]])
    
    ## 3 tables in the list, target table is the [[3]]
    # MiddleBakken_html %>% rvest::html_nodes("table")
    
    cat('===  rvest::html_table(fill = TRUE) for ',FormationName,":Formation webpage \n")
    ## this will create a list of all tables
    system.time(ls.scrapedTables <- bakkenwells_html %>%
                  rvest::html_table(fill = TRUE))
    myEndtime.html_tableFill = Sys.time()
    cat("Tick Tock Tick Tock === rvest::html_table(fill = TRUE) total time: ",myEndtime.html_tableFill - myEndTime.Element, " \n")
    
    
    ##use gsub to remove the spaces in the column names
    ## ls.scrapedTables[[3]] used to be the table before "Data Services Notification..."
    colnames(ls.scrapedTables[[4]]) <- gsub(" ","",colnames(ls.scrapedTables[[4]]))
    ## make it a tibble
    ## ## fix datetimefields with lubridate's parse_date_time
    ## and add targetFormation field
    tbl.scrapedData <- dplyr::as_tibble(ls.scrapedTables[[4]]) %>%
      dplyr::mutate(CompletionDate  = lubridate::parse_date_time(CompletionDate, "mdy"),
                    LastProdRptDate = lubridate::parse_date_time(LastProdRptDate,"mdy"),
                    targetFormation = FormationName)
    
    return(tbl.scrapedData)
    
  }
  
  # FormationName = "Middle Bakken"
  # listOption = 2
  # ## use scrape.tabelData
  tbl.ndTargetFormationByWell <- tbl.selectFormation[2:7,] %>% purrr::pmap_dfr(scrape.tableData)
  
  ## now use the following table definition for ND
  
  ## spacingLanes Construction
  ## ND  https://www.dmr.nd.gov/oilgas/bakkenwells.asp
  ## this webpage uses 6 categories to classify what targetFormation each well is
  ## drilling in.  It is not complete, however
  tbl.targetFormations <- dplyr::tribble( ~tgtF,  ~targetFormation,
                                          "MB",   "Middle Bakken",
                                          "TF",   "Three Forks",
                                          "MBTF", "Middle Bakken / Three Forks",
                                          "LP",   "Lodgepole",
                                          "UBS",  "Upper Bakken Shale",
                                          "LPMB", "Lodgepole / Middle Bakken")
  ## merge with tbl.ndTargetFormationByWell
  ## we'll use tgtF in the VirtualWellID naming convention of
  ## DSU + tgtF + spacingDistance + spacingLane
  tbl.ndTargetFormationByWell <- dplyr::left_join(tbl.ndTargetFormationByWell,
                                                  tbl.targetFormations,
                                                  by = c("targetFormation" = "targetFormation"))
  
  
  
  
  # system.time(tbl.mb <- scrape.tableData(FormationName = tbl.selectFormation$FormationName[2],
  #                                        listOption    = tbl.selectFormation$listOption[2]))
  # system.time(tbl.tf <- scrape.tableData(FormationName = tbl.selectFormation$FormationName[3],
  #                                        listOption    = tbl.selectFormation$listOption[3]))
  # system.time(tbl.mbtf <- scrape.tableData(FormationName = tbl.selectFormation$FormationName[4],
  #                                          listOption    = tbl.selectFormation$listOption[4]))
  # system.time(tbl.lp <- scrape.tableData(FormationName = tbl.selectFormation$FormationName[5],
  #                                        listOption    = tbl.selectFormation$listOption[5]))
  # system.time(tbl.ubs <- scrape.tableData(FormationName = tbl.selectFormation$FormationName[6],
  #                                         listOption    = tbl.selectFormation$listOption[6]))
  # system.time(tbl.lbmb <- scrape.tableData(FormationName = tbl.selectFormation$FormationName[7],
  #                                          listOption    = tbl.selectFormation$listOption[7]))
  cat("+++ save tbl.ndTargetFormationByWell +++ \n")
  saveRDS(tbl.ndTargetFormationByWell, file = paste0(rdsFilePath,"/tbl.ndTargetFormationByWell.rds"))
  
  ## tbl.ndTargetFormationByWell <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.ndTargetFormationByWell.rds')
  
  ## Cleanup by closing the Selenium Chrome server
  rD$client$closeall() # Closes the Chrome window
  rD$server$stop() # Asks Selenium to stop nicely
  rm(rD) # Removes the R connection to Selenium
  gc() # Asks R to remove all memory associated with Selenium
  
  
}





## https://www.dmr.nd.gov/oilgas/bakkenwells.as
## Must install docker
#' and execute commands in terminal
#'
#' KnightAwesome:~ doucetteemail$ docker pull selenium/standalone-chrome
#' Using default tag: latest
#' latest: Pulling from selenium/standalone-chrome
#' Digest: sha256:c478febf26729b7cc27e2fac7d10fa79cf2519b5ab83f9cf82705248258b6495
#' Status: Image is up to date for selenium/standalone-chrome:latest
#' KnightAwesome:~ doucetteemail$ docker run -d -p 4445:4444 selenium/standalone-chrome
#'
#' I used chrome, make sure these commands are executed BEFORE running
#'

system.time(scrape.bakkenwells.asp(rdsFilePath = '~/Documents/Business/AGEO Energy Consult/HuntOil/Development/Hunter/RDSFiles'))

system.time(MPZAnalyticsR::scrape.bakkenwells.asp(rdsFilePath = '~/Downloads/MPZAnalyticsRrdsFiles'))
# system.time(scrape.bakkenwells.asp(rdsFilePath = '~/Downloads/MPZAnalyticsRrdsFiles'))
## Read in ndTarget Formatin by Well (state's recording of which target formation)
tbl.ndTargetFormationByWell <- readRDS(file = '~/Documents/Business/AGEO Energy Consult/HuntOil/Development/Hunter/RDSFiles/tbl.ndTargetFormationByWell.rds')


## Step 4a Update Well Index With Formation Targets
tbl.wellIndexTgtF <- dplyr::left_join( tbl.wellIndex, 
                                       tbl.ndTargetFormationByWell %>% 
                                         dplyr::select(FileNo, LastProdRptDate, targetFormation, tgtF), 
                                       by = c("FileNo" = "FileNo")) %>% 
  dplyr::relocate(tgtF, .before = CurrentOperator)


## Step 4a Create OGD_superHorizontals.shape --------------------------

## MPZAnalyticsR source file is DSUShapeFunctions.R
source(file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsR/R/DSUShapeFunctions.R')

## these data sets carve up everything
## Directionals.shape, Horizontals.shape & Wells.shape all have to be created prior to running this function
system.time(create.OGD_superHorizontals.shapeRDS(rdsFilePath = '~/Downloads/MPZAnalyticsRrdsFiles' ))

## Read in superHorizontals.shape
OGD_superHorizontals.shape <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/OGD_superHorizontals.shape.rds')


## Step 5 Create DSUWells List --------------------------------------

create.DSUWellsList <- function(OGD_DrillingSpacingUnits.shape,
                                OGD_Horizontals.shape,
                                scaleFactor = 1.0,
                                rdsFilePath = '~/Downloads/MPZAnalyticsRrdsFiles',
                                listName    = '/ls.DSUWells.rds',
                                ...) {
  
  require(sf)
  ## this is an ungly way to do it but it works
  DSUIndex        <- OGD_DrillingSpacingUnits.shape$DSUIndex
  names(DSUIndex) <- DSUIndex
  ## now with endpoints we build the ugly ls.DSUWells list
  ## the goal here was to take every DSU and divide it vertically or horizontally in 1/2 to A&B pieces
  ## Here's the theory, sections are 1 mile wide, by dividing in 1/2 we create areas for well pads
  system.time(ls.DSUWells <- lapply(DSUIndex,function(.){get.HzLinesEndPointsInDSUaAndbScaled(DSUMPZBoxes1280.shape      = OGD_DrillingSpacingUnits.shape,
                                                                                              HorizontalsEndPoints.shape = OGD_HorizontalsEndPoints.shape,
                                                                                              DSUIndex                   = .,
                                                                                              scaleFactor                = scaleFactor)}))
  
  
  # saveRDS(ls.DSUWells, file = '~/Downloads/MPZAnalyticsRrdsFiles/ls.DSUWellsEndPoints.rds')
  #
  #
  # ls.DSUWells <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/ls.DSUWellsEndPoints.rds')
  
  
  cat("<<<<<<<<<< save ls.DSUWells >>>>>>>>>>>>>\n")
  saveRDS(ls.DSUWells, file = paste0(rdsFilePath, listName))
  
  # ls.DSUWells <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/ls.DSUWells.rds')
  
}


get.HzLinesEndPointsInDSUaAndbScaled <- function(DSUMPZBoxes1280.shape, HorizontalsEndPoints.shape, DSUIndex, scaleFactor = 1.00){
  # require(tidyverse)
  # require(rgeos, quietly = TRUE)
  # require(raster, quietly = TRUE)
  #
  cat('==== MPZBox = ', DSUIndex, ' ====')
  ## Filter with [] to get the correct DSU
  DSU.shape <- DSUMPZBoxes1280.shape[DSUMPZBoxes1280.shape$DSUIndex == DSUIndex,]
  
  ## make a new bounding box by drilling spacing unit shape and find the sections it encompases and name the box accordingly
  ## then save it for future use
  myBoundingBox <- sf::st_bbox(DSU.shape)
  
  ## first check if its a vertical box enclosing 2 sections
  
  longDistance <- myBoundingBox['xmax'] - myBoundingBox['xmin']
  
  latDistance  <- myBoundingBox['ymax'] - myBoundingBox['ymin']
  
  
  ## Is the 1280 DSU Vertical?  Determine this by taking the difference in the lat and long distance
  DSUType <- dplyr::if_else(longDistance < latDistance, 'Vertical', 'Horizontal')
  
  ## lets tell the world if its Vertical or Horizontal
  cat(' ', DSUType, ' DSU; ')
  
  ## this whole function is about determining empty space and opportunities in DSUs
  ## To do this we will start with a data structure ls.DSU
  
  ls.DSU <- list(DSUIndex            = DSUIndex,
                 DSUType             = DSUType,
                 horizontalWellCount = 0,
                 tbl.data            = dplyr::tibble(wl_permit = integer(),
                                                     DSUIndex  = as.integer(DSUIndex),
                                                     api       = ' '),
                 ls.DSUa             = list(horizontalWellCount = 0,
                                            tbl.data            = dplyr::tibble(wl_permit = integer(),
                                                                                DSUIndex  = as.integer(DSUIndex),
                                                                                api       = ' ')),
                 ls.DSUb             = list(horizontalWellCount = 0,
                                            tbl.data            = dplyr::tibble(wl_permit = integer(),
                                                                                DSUIndex  = as.integer(DSUIndex),
                                                                                api       = ' ')))
  cat("+++ Call get.HzLinesEndPointsDataTableScaled +++\n")
  
  ## get the horizontal tbl.data from the Horizontal_Lines.shape@data
  ls.DSU$tbl.data <- get.HzLinesEndPointsDataTableScaled(HorizontalsEndPoints.shape = HorizontalsEndPoints.shape,
                                                         DSU.shape                  = DSU.shape,
                                                         DSUIndex                   = DSUIndex,
                                                         scaleFactor                = scaleFactor)
  
  ## use dplyr summarize() with n() to get the total rows and use $n to return the vector
  ## an empty tibble should be 0
  ls.DSU$horizontalWellCount = dplyr::summarize(ls.DSU$tbl.data, n = dplyr::n())$n
  
  
  
  ## tell the world how many horizontals are in the DSU
  cat(' Horizontal Well Total Count = ', ls.DSU$horizontalWellCount,'; ')
  
  ## divide the bbox into parts a and b seperated by 1/2 the distance of the x min & x max
  
  myBoundingBoxA <- myBoundingBox
  myBoundingBoxB <- myBoundingBox
  
  ## if its Vertical A is the Western 1/2
  ## if its Horizontal A is the Souther 1/2
  if(longDistance < latDistance){
    ## change A's x,max to 1/2 xDistance + x,min otherwise
    myBoundingBoxA['xmax'] <- longDistance/2 + myBoundingBoxA['xmin']
    ## change B's x,min to A's new x,max
    myBoundingBoxB['xmin'] <- myBoundingBoxA['xmax']
  } else{
    ## change A's y,max to 1/2 latDistance + y,min otherwise
    myBoundingBoxA['ymax'] <- latDistance/2 + myBoundingBoxA['ymin']
    ## change B's y,min to A's new y,max
    myBoundingBoxB['ymin'] <- myBoundingBoxA['ymax']
  }
  
  ## use st_as_sfc which will turn the bbox into a polygon, the st_bbox method includes the CRS so no need to create one
  myBoundingBoxA <- st_as_sfc(myBoundingBoxA)
  myBoundingBoxB <- st_as_sfc(myBoundingBoxB)
  
  MPZBoxA.shape <- sf::st_intersection(x = myBoundingBoxA, y = DSU.shape)
  MPZBoxB.shape <- sf::st_intersection(x = myBoundingBoxB, y = DSU.shape)
  
  ## try gintersect from rgeos to get a shape that is truly half the DSU
  # MPZBoxA.shape <- gIntersection(spgeom1 = myBoundingBoxA, spgeom2 = DSU.shape)
  #
  # ## try gintersect from rgeos to get a shape that is truly half the DSU
  # MPZBoxB.shape <- gIntersection(spgeom1 = myBoundingBoxB, spgeom2 = DSU.shape)
  #
  ## get the horizontal tbl.data from the Horizontal_Lines.shape@data
  ls.DSU$ls.DSUa$tbl.data <- get.HzLinesEndPointsDataTableScaled(HorizontalsEndPoints.shape = HorizontalsEndPoints.shape,
                                                                 DSU.shape                  = MPZBoxA.shape,
                                                                 DSUIndex                   = DSUIndex,
                                                                 scaleFactor                = scaleFactor)
  
  ## use dplyr summarize() with n() to get the total rows and use $n to return the vector
  ## an empty tibble should be 0
  ls.DSU$ls.DSUa$horizontalWellCount = dplyr::summarize(ls.DSU$ls.DSUa$tbl.data, n = dplyr::n())$n
  ## tell the world how many horizontals in DSUa
  cat(' HW A Count = ', ls.DSU$ls.DSUa$horizontalWellCount,';')
  
  ## get the horizontal tbl.data from the Horizontal_Lines.shape
  ## keep scaleFactor 1.0 for A & B
  ls.DSU$ls.DSUb$tbl.data <- get.HzLinesEndPointsDataTableScaled(HorizontalsEndPoints.shape = HorizontalsEndPoints.shape,
                                                                 DSU.shape                  = MPZBoxB.shape,
                                                                 DSUIndex                   = DSUIndex,
                                                                 scaleFactor                = scaleFactor)
  
  ## use dplyr summarize() with n() to get the total rows and use $n to return the vector
  ## an empty tibble should be 0
  ls.DSU$ls.DSUb$horizontalWellCount = dplyr::summarize(ls.DSU$ls.DSUb$tbl.data, n = dplyr::n())$n
  ## tell the world how many horizontals in DSUa
  cat(' HW B Count = ', ls.DSU$ls.DSUb$horizontalWellCount, '\n')
  
  return(ls.DSU)
  
}

#' get.HzLinesEndPointsDataTableScaled
#'
#' function called by create.DSUWellsList
#'
#' @param HorizontalsEndPoints.shape
#' @param DSU.shape
#' @param DSUIndex
#' @param scaleFactor
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
#' 

HorizontalsEndPoints.shape <- OGD_HorizontalsEndPoints.shape
DSU.shape <- OGD_Directionals.shape
get.HzLinesEndPointsDataTableScaled <- function(HorizontalsEndPoints.shape, 
                                                DSU.shape, 
                                                DSUIndex, 
                                                scaleFactor = 1.0){
  if(scaleFactor > 0){
    ## first scale the DSU.shape
    
    myDSU.geometry      <- sf::st_geometry(DSU.shape)
    myCentroid.geometry <- sf::st_centroid(myDSU.geometry)
    
    ## we'll scale the shape by 1/2 in order to only intersect the sections and account for errors in projections from the sf package
    ## formula for reducing is AX + b
    ## (polygon - centroidPoint) * scaling factor + centroidPoint
    ## (myDSU.geometry - myCentroid.geometry) * .50 + myCentroid.geometry
    myScaledDSU.geometry <- (myDSU.geometry - myCentroid.geometry) * scaleFactor + myCentroid.geometry
    
    ## add back the CRS
    sf::st_crs(myScaledDSU.geometry) <- sf::st_crs(myDSU.geometry)
    ## use sf::st_intersection()
    
    ## subset the shapefile with the driilingSpacingUnit
    ## use st_intersection to find all of the lines inside the DSU
    horizontalLinesInDSU.shape <- sf::st_intersection(HorizontalsEndPoints.shape, myScaledDSU.geometry)
    ## if no rows in the shape file
    if ( NROW(horizontalLinesInDSU.shape) == 0 ){
      ## return empty tibble
      ## use FileNo = integer() to create a 0 row tibble
      ## make sure DSUIndex is as.integer
      tbl.data <- dplyr::tibble(wl_permit    = integer(),
                                DSUIndex     = as.integer(DSUIndex),
                                api          = ' ',
                                measdpth     = numeric(),
                                tvd          = numeric(),
                                endPointLong = numeric(),
                                endPointLat  = numeric(),
                                scaleFactor  = as.numeric(scaleFactor))
      return(tbl.data)
      
    } else{
      ## Horizontal_Lines has multiple lines per fileNo _Vert, _LAT1 are the most common
      ## get unique FileNo
      horizontalLinesInDSU.shape$wl_permit[which(!duplicated(horizontalLinesInDSU.shape$api))]
      
      ## use Wl_Permit in Horizontals as FileNo
      tbl.data <- dplyr::tibble(wl_permit    = as.integer(horizontalLinesInDSU.shape$wl_permit[which(!duplicated(horizontalLinesInDSU.shape$api))]),
                                api          =            horizontalLinesInDSU.shape$api[which(!duplicated(horizontalLinesInDSU.shape$api))],
                                measdpth     =            horizontalLinesInDSU.shape$measdpth[which(!duplicated(horizontalLinesInDSU.shape$api))],
                                tvd          =            horizontalLinesInDSU.shape$tvd[which(!duplicated(horizontalLinesInDSU.shape$api))],
                                endPointLong =            horizontalLinesInDSU.shape$bh_long[which(!duplicated(horizontalLinesInDSU.shape$api))],
                                endPointLat  =            horizontalLinesInDSU.shape$bh_lat[which(!duplicated(horizontalLinesInDSU.shape$api))])
      
      ## add back the DSUIndex and return tbl.data
      tbl.data$DSUIndex    <- as.integer(DSUIndex)
      tbl.data$scaleFactor <- scaleFactor
      return(tbl.data)
    }
    
    
  }else{
    cat('scaleFactor must be > 0 \n')
    tbl.data <- dplyr::tibble(wl_permit    = integer(),
                              DSUIndex     = as.integer(DSUIndex),
                              api          = ' ',
                              measdpth     = numeric(),
                              tvd          = numeric(),
                              endPointLong = numeric(),
                              endPointLat  = numeric(),
                              scaleFactor  = as.numeric(scaleFactor))
    return(tbl.data)
    
  }
  
  
}


system.time(create.DSUWellsList(OGD_DrillingSpacingUnits.shape = OGD_DrillingSpacingUnits.shape[1:3,],
                                OGD_HorizontalsEndPoints.shape = OGD_HorizontalsEndPoints.shape,
                                scaleFactor                    = 1.0,
                                rdsFilePath                    = '~/Downloads/MPZAnalyticsRrdsFiles',
                                listName                       = '/ls.DSUWells.rds'))



system.time(create.DSUWellsList(OGD_DrillingSpacingUnits.shape = OGD_DrillingSpacingUnits.shape,
                                OGD_HorizontalsEndPoints.shape = OGD_HorizontalsEndPoints.shape,
                                scaleFactor                = 1.0,
                                rdsFilePath                = '~/Downloads/MPZAnalyticsRrdsFiles',
                                listName                   = '/ls.DSUWells.rds'))

## Read in DSUWells List
ls.DSUWells <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/ls.DSUWells.rds')




## Step 5a extract.DSUSummaryTables ---------------------------------

system.time(MPZAnalyticsR::extract.DSUSummaryTables(ls.DSUWells = ls.DSUWells, rdsFilePath = '~/Downloads/MPZAnalyticsRrdsFiles'))

## read in ls.DSUwells summary
tbl.AllDSUs           <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.AllDSUs.rds')
tbl.ADSUs             <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.ADSUs.rds')
tbl.BDSUs             <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.BDSUs.rds')
tbl.DSUIndexhzWells   <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUIndexhzWells.rds')
tbl.DSUIndex_AhzWells <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUIndex_AhzWells.rds')
tbl.DSUIndex_BhzWells <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUIndex_BhzWells.rds')


## Step 5b create.DSUSectionNamesTable --------------------------
## 
system.time(MPZAnalyticsR::create.DSUSectionNamesTable(DrillingSpacingUnits.shape = DrillingSpacingUnits.shape,
                                                       Sections.shape             = Sections.shape,
                                                       rdsFilePath                = '~/Downloads/MPZAnalyticsRrdsFiles',
                                                       tableName                  = '/tbl.DSUSectionNames.rds'))
## read in tbl.DSUSectionNames
tbl.DSUSectionNames <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUSectionNames.rds')


## Step 5c create.DSUFullIndexes --------------------------------
## This function constructs tbl.DSUFullIndexbyFileNo & tbl.DSUFullIndexbyFileNoWI
## Which Identify what Wells are in what DSUs


system.time(MPZAnalyticsR::create.DSUFullIndexes())
## Read in tbl.DSUFullIndexbyFileNo, tbl.DSUFullIndexbyFileNoWI
tbl.DSUFullIndexbyFileNo   <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUFullIndexbyFileNo.rds' )
tbl.DSUFullIndexbyFileNoWI <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUFullIndexbyFileNoWI.rds' )

## Step 5d create.DSUbyOperator -------------------------
## 
system.time(MPZAnalyticsR::create.DSUbyOperator())

tbl.DSUOperator <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUOperator.rds')

## Step 6 create 250' spacingLanes ------------------------------
get.HzLinesEndPointsDataTableScaled <- function(HorizontalsEndPoints.shape, DSU.shape, DSUIndex, scaleFactor = 1.0){
  if(scaleFactor > 0){
    ## first scale the DSU.shape
    
    myDSU.geometry      <- sf::st_geometry(DSU.shape)
    myCentroid.geometry <- sf::st_centroid(myDSU.geometry)
    
    ## we'll scale the shape by 1/2 in order to only intersect the sections and account for errors in projections from the sf package
    ## formula for reducing is AX + b
    ## (polygon - centroidPoint) * scaling factor + centroidPoint
    ## (myDSU.geometry - myCentroid.geometry) * .50 + myCentroid.geometry
    myScaledDSU.geometry <- (myDSU.geometry - myCentroid.geometry) * scaleFactor + myCentroid.geometry
    
    ## add back the CRS
    sf::st_crs(myScaledDSU.geometry) <- sf::st_crs(myDSU.geometry)
    ## use sf::st_intersection()
    
    
    
    
    ## subset the shapefile with the driilingSpacingUnit
    ## use st_intersection to find all of the lines inside the DSU
    horizontalLinesInDSU.shape <- sf::st_intersection(HorizontalsEndPoints.shape, myScaledDSU.geometry)
    ## if no rows in the shape file
    if ( NROW(horizontalLinesInDSU.shape) == 0 ){
      ## return empty tibble
      ## use FileNo = integer() to create a 0 row tibble
      ## make sure DSUIndex is as.integer
      tbl.data <- dplyr::tibble(FileNo       = integer(),
                                DSUIndex     = as.integer(DSUIndex),
                                api          = ' ',
                                MeasDpth     = numeric(),
                                TVD          = numeric(),
                                endPointLong = numeric(),
                                endPointLat  = numeric(),
                                scaleFactor  = as.numeric(scaleFactor))
      return(tbl.data)
      
    } else{
      ## Horizontal_Lines has multiple lines per fileNo _Vert, _LAT1 are the most common
      ## get unique FileNo
      horizontalLinesInDSU.shape$FileNo[which(!duplicated(horizontalLinesInDSU.shape$api))]
      
      ## use Wl_Permit in Horizontals as FileNo
      tbl.data <- dplyr::tibble(FileNo       = as.integer(horizontalLinesInDSU.shape$Wl_Permit[which(!duplicated(horizontalLinesInDSU.shape$api))]),
                                api          = horizontalLinesInDSU.shape$api[which(!duplicated(horizontalLinesInDSU.shape$api))],
                                MeasDpth     = horizontalLinesInDSU.shape$MeasDpth[which(!duplicated(horizontalLinesInDSU.shape$api))],
                                TVD          = horizontalLinesInDSU.shape$TVD[which(!duplicated(horizontalLinesInDSU.shape$api))],
                                endPointLong = horizontalLinesInDSU.shape$Long[which(!duplicated(horizontalLinesInDSU.shape$api))],
                                endPointLat  = horizontalLinesInDSU.shape$Lat[which(!duplicated(horizontalLinesInDSU.shape$api))])
      
      ## add back the DSUIndex and return tbl.data
      tbl.data$DSUIndex    <- as.integer(DSUIndex)
      tbl.data$scaleFactor <- scaleFactor
      return(tbl.data)
    }
    
    
  }else{
    cat('scaleFactor must be > 0 \n')
    tbl.data <- dplyr::tibble(FileNo       = integer(),
                              DSUIndex     = as.integer(DSUIndex),
                              api          = ' ',
                              MeasDpth     = numeric(),
                              TVD          = numeric(),
                              endPointLong = numeric(),
                              endPointLat  = numeric(),
                              scaleFactor  = as.numeric(scaleFactor))
    return(tbl.data)
    
  }
  
  
}

## Create 250' Spacing Lanes Table

get.HzLinesEndPointsIn250ftSpacingLanes <- function(DSUMPZBoxes1280.shape, HorizontalsEndPoints.shape, DSUIndex, scaleFactor = 1.00,
                                                    tbl.ndTargetFormationByWell,
                                                    spacingDistance = 250,
                                                    spacingDistanceMeasure = "ft",
                                                    DSUWidth = 5280,
                                                    DSUMeansure = "ft"){
  # require(tidyverse)
  # require(rgeos, quietly = TRUE)
  # require(raster, quietly = TRUE)
  #
  cat('==== DSUIndex = ', DSUIndex, ' ====')
  ## Filter with [] to get the correct DSU
  DSU.shape <- DSUMPZBoxes1280.shape[DSUMPZBoxes1280.shape$DSUIndex == DSUIndex,]
  
  ## make a new bounding box by drilling spacing unit shape and find the sections it encompases and name the box accordingly
  ## then save it for future use
  myBoundingBox <- sf::st_bbox(DSU.shape)
  
  ## first check if its a vertical box enclosing 2 sections
  
  longDistance <- myBoundingBox['xmax'] - myBoundingBox['xmin']
  
  latDistance  <- myBoundingBox['ymax'] - myBoundingBox['ymin']
  
  
  ## Is the 1280 DSU Vertical?  Determine this by taking the difference in the lat and long distance
  DSUType <- dplyr::if_else(longDistance < latDistance, 'Vertical', 'Horizontal')
  
  ## lets tell the world if its Vertical or Horizontal
  cat(' ', DSUType, ' DSU; ')
  
  
  ## add a non scaled HorizontalsEndPoints.shape so we don't have to do the intersection on the whole table
  ## just the subset
  
  ## subset the shapefile with the driilingSpacingUnit
  ## use st_intersection to find all of the lines inside the DSU
  subsetHorizontalEndPointsInDSU.shape <- sf::st_intersection(HorizontalsEndPoints.shape, DSU.shape)
  
  
  
  if(DSUType == "Horizontal"){
    ## divide up into 250' sections by dividing longDistance by 21
    ## or divide up into 300' sections by dividing by 18?
    # spacingDistance <- 250
    # ## in ft or m for meters
    # spacingDistanceMeasure <-  'ft'
    #
    # ## DSU width, assumed to be 1 mile = 5280 ft.
    # DSUWidth    <-  5280
    # DSUMeansure <- 'ft'
    # ## get numberOfSpacingLanes  assume 5280 section / footage
    if(DSUMeansure == 'ft'){
      ## use round to avoid fraction size lanes
      
      numberOfSpacingLanes <- round(DSUWidth/spacingDistance)
      ## this will be in terms of longitude
      spacingGridDistance <- latDistance/numberOfSpacingLanes
      
    }
    
    ## i hate for loops
    ## get all of the xmins by spacingGridDistance
    ymins <- unlist(lapply(0:numberOfSpacingLanes, function(.){
      
      unname(myBoundingBox['ymin'] + . * spacingGridDistance)
    }))
    ## make table of xmins and xmaxs
    tbl.longMinMax <- dplyr::tibble(ymin = ymins, ymax = dplyr::lead(ymins)) %>% dplyr::filter(!is.na(ymax))
    
    ## make 21 spacingLaneShapeBoxes
    
    tbl.spacingLaneData <- dplyr::bind_rows(lapply(1:NROW(tbl.longMinMax), function(.){
      myMiniBox <- myBoundingBox
      
      myMiniBox['ymin'] <- tbl.longMinMax$ymin[.]
      myMiniBox['ymax'] <- tbl.longMinMax$ymax[.]
      
      
      ## use st_as_sfc which will turn the bbox into a polygon, the st_bbox method includes the CRS so no need to create one
      myMiniBox <- sf::st_as_sfc(myMiniBox)
      
      myMiniBox.shape <- sf::st_intersection(x = myMiniBox, y = DSU.shape)
      
      ## get the horizontal tbl.data from the Horizontal_Lines.shape@data
      tbl.myMiniBoxData <-     get.HzLinesEndPointsDataTableScaled(HorizontalsEndPoints.shape = subsetHorizontalEndPointsInDSU.shape,
                                                                   DSU.shape                  = myMiniBox.shape,
                                                                   DSUIndex                   = DSUIndex,
                                                                   scaleFactor                = scaleFactor)
      
      ## create data summary table for the box with a spacingLaneData field for the tbl.myMiniBoxData
      
      tbl.spacingLaneData <- dplyr::tibble(DSUId                  = DSUIndex,
                                           spacingDistance        = spacingDistance,
                                           spacingDistanceMeasure = spacingDistanceMeasure,
                                           spacingLane            = .,
                                           numberOfSpacingLanes   = numberOfSpacingLanes,
                                           xmin                   = myBoundingBox['xmin'],
                                           xmax                   = myBoundingBox['xmax'],
                                           ymin                   = tbl.longMinMax$ymin[.],
                                           ymax                   = tbl.longMinMax$ymax[.])
      ## create a spacingLaneWellCount field
      tbl.spacingLaneData$spacingLaneWellCount <- NROW(tbl.myMiniBoxData)
      
      ## build out the table if it has data
      if(NROW(tbl.myMiniBoxData) > 0){
        tbl.myMiniBoxData$DSUId                  <- DSUIndex
        tbl.myMiniBoxData$spacingDistance        <- spacingDistance
        tbl.myMiniBoxData$spacingDistanceMeasure <- spacingDistanceMeasure
        tbl.myMiniBoxData$spacingLane            <- .
        tbl.myMiniBoxData$numberOfSpacingLanes   <- numberOfSpacingLanes
        tbl.myMiniBoxData$xmin                   <- myBoundingBox['xmin']
        tbl.myMiniBoxData$xmax                   <- myBoundingBox['xmax']
        tbl.myMiniBoxData$ymin                   <- tbl.longMinMax$ymin[.]
        tbl.myMiniBoxData$ymax                   <- tbl.longMinMax$ymax[.]
      }
      
      ## put the tbl.myMiniBoxData into a list element, in this way we can have an empty table
      tbl.spacingLaneData$spacingLaneTable <- list(tbl.myMiniBoxData)
      
      return(tbl.spacingLaneData)
    }))
    
    ## create master DSU spacingLaneData Grid  for now assume MB and TF for each DSU
    tbl.masterSpacingLaneData <- dplyr::bind_rows(
      tbl.spacingLaneData %>% dplyr::select(-spacingLaneTable, - spacingLaneWellCount) %>% mutate(tgtF = "MB"),
      tbl.spacingLaneData %>% dplyr::select(-spacingLaneTable, - spacingLaneWellCount) %>% mutate(tgtF = "TF")
    )
    
    ## create virtualID spacingLaneID
    tbl.masterSpacingLaneData <- tbl.masterSpacingLaneData %>%
      dplyr::mutate(spacingLaneID = dplyr::if_else(spacingLane < 10,  paste0(DSUId,tgtF,"0",spacingLane), paste0(DSUId,tgtF,spacingLane)))
    
    
    ## which lanes are filled?
    tbl.filledSpacingLanes <- tbl.spacingLaneData %>% dplyr::filter(spacingLaneWellCount > 0)
    
    ## if no lanes are filled just return masterspacing
    if(NROW(tbl.filledSpacingLanes) == 0){
      ## add back in DSUType
      tbl.masterSpacingLaneData$DSUType <- DSUType
      return(tbl.masterSpacingLaneData)
    }else{
      ##which lanes are filled?
      tbl.filledSpacingLanes <- tbl.spacingLaneData %>% dplyr::filter(spacingLaneWellCount > 0)
      
      tbl.filledSpacingLanes <-  bind_rows(tbl.filledSpacingLanes$spacingLaneTable)
      
      ## merge in target formation data
      tbl.filledSpacingLanes <- dplyr:: left_join(tbl.filledSpacingLanes,
                                                  tbl.ndTargetFormationByWell %>%
                                                    dplyr::select(FileNo, tgtF, targetFormation),
                                                  by = c("FileNo" = "FileNo"))
      
      
      ## change NAs to NoData
      ##
      
      tbl.filledSpacingLanes <- tbl.filledSpacingLanes %>% dplyr::mutate(tgtF = dplyr::if_else(is.na(tgtF), "NoData", tgtF))
      
      
      ## create virtualID spacingLaneID
      tbl.filledSpacingLanes <- tbl.filledSpacingLanes %>%
        dplyr::mutate(spacingLaneID = dplyr::if_else(spacingLane < 10,  paste0(DSUIndex,tgtF,"0",spacingLane), paste0(DSUIndex,tgtF,spacingLane)))
      
      
      
      ## merge in actuals to match FileNo with spacingLaneID
      tbl.masterSpacingLaneData <- dplyr::left_join(tbl.masterSpacingLaneData,
                                                    tbl.filledSpacingLanes %>%
                                                      dplyr::select(FileNo, api, MeasDpth, TVD, endPointLong, endPointLat, DSUIndex, scaleFactor, targetFormation, spacingLaneID),
                                                    by = ("spacingLaneID" = "spacingLaneID"))
      
      ## bind_rows for any NoData tgtFormation
      tbl.masterSpacingLaneData <- dplyr::bind_rows(tbl.masterSpacingLaneData,
                                                    tbl.filledSpacingLanes %>%
                                                      dplyr::filter(tgtF == "NoData") %>%
                                                      dplyr::select(colnames(tbl.masterSpacingLaneData)))
      
      ## add back in DSUType
      tbl.masterSpacingLaneData$DSUType <- DSUType
      return(tbl.masterSpacingLaneData)
    }
    
  }else{
    ## divide up into 250' sections by dividing longDistance by 21
    ## or divide up into 300' sections by dividing by 18?
    # spacingDistance <- 250
    # ## in ft or m for meters
    # spacingDistanceMeasure <-  'ft'
    #
    # ## DSU width, assumed to be 1 mile = 5280 ft.
    # DSUWidth    <-  5280
    # DSUMeansure <- 'ft'
    # ## get numberOfSpacingLanes  assume 5280 section / footage
    if(DSUMeansure == 'ft'){
      ## use round to avoid fraction size lanes
      
      numberOfSpacingLanes <- round(DSUWidth/spacingDistance)
      ## this will be in terms of longitude
      spacingGridDistance <- longDistance/numberOfSpacingLanes
      
    }
    ## i hate for loops
    ## get all of the xmins by spacingGridDistance
    xmins <- unlist(lapply(0:numberOfSpacingLanes, function(.){
      
      unname(myBoundingBox['xmin'] + . * spacingGridDistance)
    }))
    ## make table of xmins and xmaxs
    tbl.longMinMax <- dplyr::tibble(xmin = xmins, xmax = lead(xmins)) %>% dplyr::filter(!is.na(xmax))
    
    ## make 21 spacingLaneShapeBoxes
    
    tbl.spacingLaneData <- dplyr::bind_rows(lapply(1:NROW(tbl.longMinMax), function(.){
      myMiniBox <- myBoundingBox
      
      myMiniBox['xmin'] <- tbl.longMinMax$xmin[.]
      myMiniBox['xmax'] <- tbl.longMinMax$xmax[.]
      
      
      ## use st_as_sfc which will turn the bbox into a polygon, the st_bbox method includes the CRS so no need to create one
      myMiniBox <- sf::st_as_sfc(myMiniBox)
      
      myMiniBox.shape <- sf::st_intersection(x = myMiniBox, y = DSU.shape)
      
      ## get the horizontal tbl.data from the Horizontal_Lines.shape@data
      tbl.myMiniBoxData <-     get.HzLinesEndPointsDataTableScaled(HorizontalsEndPoints.shape = subsetHorizontalEndPointsInDSU.shape,
                                                                   DSU.shape                  = myMiniBox.shape,
                                                                   DSUIndex                   = DSUIndex,
                                                                   scaleFactor                = scaleFactor)
      
      ## create data summary table for the box with a spacingLaneData field for the tbl.myMiniBoxData
      
      tbl.spacingLaneData <- dplyr::tibble(DSUId                  = DSUIndex,
                                           spacingDistance        = spacingDistance,
                                           spacingDistanceMeasure = spacingDistanceMeasure,
                                           spacingLane            = .,
                                           numberOfSpacingLanes   = numberOfSpacingLanes,
                                           xmin                   = tbl.longMinMax$xmin[.],
                                           xmax                   = tbl.longMinMax$xmax[.],
                                           ymin                   = myBoundingBox['ymin'],
                                           ymax                   = myBoundingBox['ymax'])
      ## create a spacingLaneWellCount field
      tbl.spacingLaneData$spacingLaneWellCount <- NROW(tbl.myMiniBoxData)
      
      ## build out the table if it has data
      if(NROW(tbl.myMiniBoxData) > 0){
        tbl.myMiniBoxData$DSUId                  <- DSUIndex
        tbl.myMiniBoxData$spacingDistance        <- spacingDistance
        tbl.myMiniBoxData$spacingDistanceMeasure <- spacingDistanceMeasure
        tbl.myMiniBoxData$spacingLane            <- .
        tbl.myMiniBoxData$numberOfSpacingLanes   <- numberOfSpacingLanes
        tbl.myMiniBoxData$xmin                   <- tbl.longMinMax$xmin[.]
        tbl.myMiniBoxData$xmax                   <- tbl.longMinMax$xmax[.]
        tbl.myMiniBoxData$ymin                   <- myBoundingBox['ymin']
        tbl.myMiniBoxData$ymax                   <- myBoundingBox['ymax']
      }
      
      ## put the tbl.myMiniBoxData into a list element, in this way we can have an empty table
      tbl.spacingLaneData$spacingLaneTable <- list(tbl.myMiniBoxData)
      
      return(tbl.spacingLaneData)
    }))
    
    
    
    ## create master DSU spacingLaneData Grid  for now assume MB and TF for each DSU
    tbl.masterSpacingLaneData <- dplyr::bind_rows(
      tbl.spacingLaneData %>% dplyr::select(-spacingLaneTable, - spacingLaneWellCount) %>% dplyr::mutate(tgtF = "MB"),
      tbl.spacingLaneData %>% dplyr::select(-spacingLaneTable, - spacingLaneWellCount) %>% dplyr::mutate(tgtF = "TF")
    )
    
    ## create virtualID spacingLaneID
    tbl.masterSpacingLaneData <- tbl.masterSpacingLaneData %>%
      dplyr::mutate(spacingLaneID = dplyr::if_else(spacingLane < 10,  paste0(DSUId,tgtF,"0",spacingLane), paste0(DSUId,tgtF,spacingLane)))
    
    ## which lanes are filled?
    tbl.filledSpacingLanes <- tbl.spacingLaneData %>% dplyr::filter(spacingLaneWellCount > 0)
    ## if no lanes are filled just return masterspacing
    if(NROW(tbl.filledSpacingLanes) == 0){
      ## add back in DSUType
      tbl.masterSpacingLaneData$DSUType <- DSUType
      return(tbl.masterSpacingLaneData)
    }else{
      ##which lanes are filled?
      tbl.filledSpacingLanes <- tbl.spacingLaneData %>% dplyr::filter(spacingLaneWellCount > 0)
      
      tbl.filledSpacingLanes <-  dplyr::bind_rows(tbl.filledSpacingLanes$spacingLaneTable)
      
      ## merge in target formation data
      tbl.filledSpacingLanes <- dplyr:: left_join(tbl.filledSpacingLanes,
                                                  tbl.ndTargetFormationByWell %>%
                                                    dplyr::select(FileNo, tgtF, targetFormation),
                                                  by = c("FileNo" = "FileNo"))
      
      
      ## change NAs to NoData
      ##
      
      tbl.filledSpacingLanes <- tbl.filledSpacingLanes %>% dplyr::mutate(tgtF = dplyr::if_else(is.na(tgtF), "NoData", tgtF))
      
      
      ## create virtualID spacingLaneID
      tbl.filledSpacingLanes <- tbl.filledSpacingLanes %>%
        dplyr::mutate(spacingLaneID = dplyr::if_else(spacingLane < 10,  paste0(DSUIndex,tgtF,"0",spacingLane), paste0(DSUIndex,tgtF,spacingLane)))
      
      
      
      ## merge in actuals to match FileNo with spacingLaneID
      tbl.masterSpacingLaneData <- dplyr::left_join(tbl.masterSpacingLaneData,
                                                    tbl.filledSpacingLanes %>%
                                                      dplyr::select(FileNo, api, MeasDpth, TVD, endPointLong, endPointLat, DSUIndex, scaleFactor, targetFormation, spacingLaneID),
                                                    by = ("spacingLaneID" = "spacingLaneID"))
      
      ## bind_rows for any NoData tgtFormation
      tbl.masterSpacingLaneData <- dplyr::bind_rows(tbl.masterSpacingLaneData,
                                                    tbl.filledSpacingLanes %>%
                                                      dplyr::filter(tgtF == "NoData") %>%
                                                      dplyr::select(colnames(tbl.masterSpacingLaneData)))
      
      ## add back in DSUType
      tbl.masterSpacingLaneData$DSUType <- DSUType
      return(tbl.masterSpacingLaneData)
    }
    
    
    
    ##
  }
}

DrillingSpacingUnits.shape <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/DrillingSpacingUnits.shape.rds')

## Read in the HorizontalsEndPoints.shape
HorizontalsEndPoints.shape <- readRDS(file = "~/Downloads/MPZAnalyticsRrdsFiles/HorizontalsEndPoints.shape.rds")

tbl.ndTargetFormationByWell <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.ndTargetFormationByWell.rds')


system.time(create.nd250ftDSUSpacingLanes(DrillingSpacingUnits.shape  = DrillingSpacingUnits.shape,
                                                         HorizontalsEndPoints.shape  = HorizontalsEndPoints.shape,
                                                         scaleFactor                 = 0.995,
                                                         tbl.ndTargetFormationByWell = tbl.ndTargetFormationByWell,
                                                         spacingDistance             = 250,
                                                         spacingDistanceMeasure      = "ft",
                                                         DSUWidth                    = 5280,
                                                         DSUMeansure                 = "ft",
                                                         rdsFilePath                 = '~/Downloads/MPZAnalyticsRrdsFiles',
                                                         tableName                   = '/tbl.nd250ftSpacingLanes.rds'))

system.time(create.nd250ftDSUSpacingLanes(DrillingSpacingUnits.shape  = DrillingSpacingUnits.shape[1:5,],
                                          HorizontalsEndPoints.shape  = HorizontalsEndPoints.shape,
                                          scaleFactor                 = 0.995,
                                          tbl.ndTargetFormationByWell = tbl.ndTargetFormationByWell,
                                          spacingDistance             = 250,
                                          spacingDistanceMeasure      = "ft",
                                          DSUWidth                    = 5280,
                                          DSUMeansure                 = "ft",
                                          rdsFilePath                 = '~/Downloads/MPZAnalyticsRrdsFiles',
                                          tableName                   = '/tbl.nd250ftSpacingLanes.rds'))


system.time(MPZAnalyticsR::create.nd250ftDSUSpacingLanes(DrillingSpacingUnits.shape  = DrillingSpacingUnits.shape,
                                                         HorizontalsEndPoints.shape  = HorizontalsEndPoints.shape,
                                                         scaleFactor                 = 0.995,
                                                         tbl.ndTargetFormationByWell = tbl.ndTargetFormationByWell,
                                                         spacingDistance             = 250,
                                                         spacingDistanceMeasure      = "ft",
                                                         DSUWidth                    = 5280,
                                                         DSUMeansure                 = "ft",
                                                         rdsFilePath                 = '~/Downloads/MPZAnalyticsRrdsFiles',
                                                         tableName                   = '/tbl.nd250ftSpacingLanes.rds'))
## read in tbl.nd250ftSpacingLanes
tbl.nd250ftSpacingLanes <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.nd250ftSpacingLanes.rds')



## Step 6a create500' spacingLanes ----------------

tbl.nd250ftSpacingLanes <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.nd250ftSpacingLanes.rds')

tbl.DSUOperator <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUOperator.rds' )

system.time(MPZAnalyticsR::create.nd500ftSpacingLanes(tbl.nd250ftSpacingLanes =  tbl.nd250ftSpacingLanes,
                                                      tbl.DSUOperator         = tbl.DSUOperator,
                                                      rdsFilePath             = '~/Downloads/MPZAnalyticsRrdsFiles'))
## Read in 500' SpacingLanes and Grid Files
tbl.nd500ftSpacingLanesFilled <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.nd500ftSpacingLanesFilled.rds' )
tbl.grid500Select             <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.grid500Select.rds' )



##  Step 7 Read In All Summary Files ---------------------------------------------------

## read in tbl.WellIndex
tbl.wellIndex                 <- readRDS(file = "~/Downloads/MPZAnalyticsRrdsFiles/tbl.wellIndex.rds")
## read in all sf shape files
Horizontals_Lines.shape       <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/Horizontals_Lines.shape.rds')
Wells.shape                   <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/Wells.shape.rds')
Sections.shape                <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/Sections.shape.rds')
OilFields.shape               <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/OilFields.shape.rds')
DrillingSpacingUnits.shape    <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/DrillingSpacingUnits.shape.rds')
Seismic2DPreplot.shape        <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/Seismic2DPreplot.shape.rds')
Seismic3DPreplot.shape        <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/Seismic3DPreplot.shape.rds')
Rigs.shape                    <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/Rigs.shape.rds')
Townships.shape               <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/Townships.shape.rds')
Directionals_Lines.shape      <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/Directionals_Lines.shape.rds')
Directionals.shape            <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/Directionals.shape.rds')
Horizontals.shape             <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/Horizontals.shape.rds')
CountyBoundaries.shape        <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/CountyBoundaries.shape.rds')
## Read in superHorizontals.shape
superHorizontals.shape <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/superHorizontals.shape.rds')
## Read in the HorizontalsEndPoints.shape
HorizontalsEndPoints.shape    <- readRDS(file = "~/Downloads/MPZAnalyticsRrdsFiles/HorizontalsEndPoints.shape.rds")
## read in tbl.LogTops
tbl.LogTops                   <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.LogTops.rds')
# Read in ndTarget Formatin by Well (state's recording of which target formation)
tbl.ndTargetFormationByWell   <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.ndTargetFormationByWell.rds')
## Read in DSUWells List
ls.DSUWells                   <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/ls.DSUWells.rds')
## read in ls.DSUwells summary
tbl.AllDSUs                   <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.AllDSUs.rds')
tbl.ADSUs                     <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.ADSUs.rds')
tbl.BDSUs                     <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.BDSUs.rds')
tbl.DSUIndexhzWells           <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUIndexhzWells.rds')
tbl.DSUIndex_AhzWells         <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUIndex_AhzWells.rds')
tbl.DSUIndex_BhzWells         <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUIndex_BhzWells.rds')
## read in tbl.DSUSectionNames
tbl.DSUSectionNames           <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUSectionNames.rds')
## Read in tbl.DSUFullIndexbyFileNo, tbl.DSUFullIndexbyFileNoWI
tbl.DSUFullIndexbyFileNo      <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUFullIndexbyFileNo.rds' )
tbl.DSUFullIndexbyFileNoWI    <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUFullIndexbyFileNoWI.rds' )
## Read in DSUOperator   
tbl.DSUOperator               <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUOperator.rds')



## Read in tbl.nd250ftSpacingLanes
# tbl.nd250ftSpacingLanes <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.nd250ftSpacingLanes.rds')
## Read in 500' SpacingLanes and Grid Files
tbl.nd500ftSpacingLanesFilled <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.nd500ftSpacingLanesFilled.rds' )
tbl.grid500Select             <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.grid500Select.rds' )

## get DSU by County
DSUbyCounty <- dplyr::left_join(tbl.nd500ftSpacingLanesFilled, tbl.wellIndex %>% dplyr::select(FileNo, CountyName, CurrentOperator), by = c("FileNo" = "FileNo")) %>% dplyr::select(DSUId, CountyName) %>% dplyr::filter(!is.na(CountyName)) %>% dplyr::filter(!duplicated(.))


tbl.DSUOperator  <- dplyr::left_join(tbl.DSUOperator, DSUbyCounty, by = c("DSUIndex" = "DSUId"))

tbl.DSUOperator <- dplyr::left_join(tbl.DSUOperator, tbl.DSUFullIndexbyFileNo  %>% dplyr::select(DSUIndex, DSUType, DSUSectionsNames, dssize) %>% dplyr::filter(!duplicated(.)), by = (c("DSUIndex" = "DSUIndex")))

## next we need to get the total number of available spacing lanes by DSU including by TGT Formation and County

nd500ftSpacingLanes <- dplyr::left_join(tbl.nd500ftSpacingLanesFilled, tbl.DSUOperator %>% dplyr::select(-DSUType), by = c("DSUId" = "DSUIndex"))


## 08-03-2020 How do I assign production curve shifts to spacingLaneID_500 unique identifier?
##  
View(nd500ftSpacingLanes %>% dplyr::filter(FileNo == 31088))


DrillingSpacingUnits.shape    <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/DrillingSpacingUnits.shape.rds')

## get bbox and overwrite with study #2 boundaries
require(sp)
require(sf)


studyAreaBbox <- st_bbox(DrillingSpacingUnits.shape)

studyAreaBbox['xmin'] <- -103.51603
studyAreaBbox['xmax'] <- -103.1049
studyAreaBbox['ymin'] <-   48.02523
studyAreaBbox['ymax'] <-   48.2548


DSUsInBox <- sf::st_intersection( DrillingSpacingUnits.shape, st_as_sfc(studyAreaBbox))

## get list of just dssize == 1280

dsuIndexes <-   DSUsInBox %>% dplyr::filter(dssize == 1280) %>% .$DSUIndex


## list of 
tbl.DSUOperator[tbl.DSUOperator$DSUIndex %in% dsuIndexes,] %>% dplyr::group_by(CurrentOperator) %>% dplyr::summarise(NoOfDSUs = dplyr::n()) %>% dplyr::arrange(dplyr::desc(NoOfDSUs)) %>% dplyr::mutate(MBtotal500ftSpacingCount = 10 * NoOfDSUs,TFtotal500ftSpacingCount = 10 * NoOfDSUs)

## number of MB wells filled in
nd500ftSpacingLanes[nd500ftSpacingLanes$DSUId %in% dsuIndexes,] %>% dplyr::filter(tgtF     == 'MB',
                                                                                  filledIn == 1) %>% dplyr::group_by(CurrentOperator, tgtF) %>% dplyr::summarise(MBWellcount = dplyr::n()) %>% dplyr::arrange(dplyr::desc(MBWellcount))
nd500ftSpacingLanes[nd500ftSpacingLanes$DSUId %in% dsuIndexes,] %>% dplyr::filter(tgtF     == 'TF',
                                                                                  filledIn == 1) %>% dplyr::group_by(CurrentOperator, tgtF) %>% dplyr::summarise(TFWellcount = dplyr::n()) %>% dplyr::arrange(dplyr::desc(TFWellcount))


nd500ftSpacingLanes[nd500ftSpacingLanes$DSUId %in% dsuIndexes,] %>% dplyr::filter(filledIn == 0) %>% dplyr::group_by(CurrentOperator, tgtF) %>% dplyr::summarise(wellcount = dplyr::n()) %>% dplyr::arrange(dplyr::desc(wellcount))

## DSUs with more than 3 filledIn == 0
list1 <- nd500ftSpacingLanes[nd500ftSpacingLanes$DSUId %in% dsuIndexes,] %>% dplyr::filter(filledIn == 0) %>% 
  dplyr::group_by(CurrentOperator, tgtF, DSUId) %>% 
  dplyr::summarise(wellcount = dplyr::n()) %>% 
  dplyr::arrange(dplyr::desc(wellcount)) %>% 
  dplyr::filter(tgtF == 'MB',
                wellcount > 3)

## copy to Numbers for manual data entry --------------------------
write.table(list1, file=pipe("pbcopy"), quote = FALSE, sep="\t", row.names = FALSE)

## get total number of DSUs and available fill ins

nd500ftSpacingLanes[nd500ftSpacingLanes$DSUId %in% dsuIndexes,] %>% dplyr::filter(filledIn == 0) %>% 
  dplyr::group_by(CurrentOperator, tgtF, DSUId) %>% 
  dplyr::summarise(wellcount = dplyr::n()) %>% 
  dplyr::arrange(dplyr::desc(wellcount)) %>% 
  dplyr::filter(tgtF == 'MB',
                wellcount > 3) %>% 
  dplyr::group_by(CurrentOperator) %>% 
  dplyr::summarize(wellcount = sum(wellcount), DSUCount = dplyr::n()) %>% 
  dplyr::arrange(dplyr::desc(DSUCount))


# map bounderies for study area #2 

48.2548
48.02523
-103.51603
-103.1049



## make a shape file out of spacing lanes
## Then use study #2 box to select target curve shift areas
## code for the study #2 box is as follows
## ## this is the study area box for the EC wells
# myBox <- bbox(Horizontals_Lines.shape)
# mylat <- 48.110268
# #mylat <- 48.210268
# mylong <- -103.305468
# 
# ## new box
# myBox[1,1] <- mylong + .25
# myBox[1,2] <- mylong - .25
# myBox[2,1] <- mylat - .1
# myBox[2,2] <- mylat + .13
# 
# myPolyBox <- matrix(c(myBox[1,1],myBox[2,1],myBox[1,2],myBox[2,1],myBox[1,2],myBox[2,2],myBox[1,1],myBox[2,2]), nrow =4, ncol = 2, byrow = T)
# #plot(myPolyBox)
# #library(sp)
# p = Polygon(myPolyBox)
# ps = Polygons(list(p),1)
# sps = SpatialPolygons(list(ps))
# #plot(sps)
# ## make the projection the same for the new bounding shape
# proj4string(sps) <- proj4string(Horizontals_Lines.shape)
# 
# ## subset Horizontals_Lines by the polybox sps
# studyArea.Horizontals_Lines.shape <- Horizontals_Lines.shape[sps,]


## easier way, just use DSU shape then backtrack





## Create Operator List Of DSUs By Size ------------------------------


tbl.DSUFullIndexbyFileNo      <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUFullIndexbyFileNo.rds' )

## Read in DSUOperator   
tbl.DSUOperator               <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUOperator.rds')

## keep only 1280 dssize for 2 section

tbl.DSUFullIndexbyFileNo %>% dplyr::filter(dssize == 1280) %>% dplyr::select(DSUIndex, DSUType, DSUSectionsNames) %>% dplyr::filter(!duplicated(.))







%>% dplyr::filter(dssize == 1280)

## who owns the most DSUs?
tbl.total1280DSUsCounts <- tbl.DSUOperator %>% dplyr::filter(dssize == 1280) %>% 
  dplyr::group_by(CurrentOperator) %>% 
  dplyr::summarise(total1280DSUs = n()) %>% 
  dplyr::arrange(desc(total1280DSUs))

## how many single well DSUs by Operator?
tbl.singleWellDSUsCounts <- tbl.DSUOperator %>% dplyr::filter(dssize == 1280) %>%
  dplyr::group_by(CurrentOperator) %>% 
  dplyr::filter(totalHzWellCount == 1) %>% 
  dplyr::summarise(singleWellDSUs = n()) %>% dplyr::arrange(desc(singleWellDSUs)) %>% View()

dplyr::left_join(tbl.total1280DSUsCounts, tbl.singleWellDSUsCounts, by = "CurrentOperator")



## copy to Numbers for manual data entry --------------------------
write.table(dplyr::left_join(tbl.total1280DSUsCounts, tbl.singleWellDSUsCounts, by = "CurrentOperator"), file=pipe("pbcopy"), quote = FALSE, sep="\t", row.names = FALSE)



##                                                                                              ##
##                                                                                              ##
##                                                                                              ##
##                                                                                              ##
##                                                                                              ##
##                                                                                              ##
##                                                                                              ##
##                                                                                              ##
##                                                                                              ##
##                                                                                              ##
##                                                                                              ##
##                                                                                              ##
##                                                                                              ##

tbl.DSUFullIndexbyFileNo %>% dplyr::filter(FileNo == 32312)

View(tbl.wellIndex %>% dplyr::filter(OriginalWellName %like% 'MOOSE'))

## MPZ Study files
## read in study wellsIds
tbl.ECWellIndex <- readRDS(file = "~/Documents/Business/IDDGroup/OMPADG/tbl.ECWellIndex.rds")

tbl.whiting175 <- readRDS(file = '~/Documents/Business/IDDGroup/OMPADG/whiting175.rds')

tbl.CPEProductionData <- readRDS(file = "~/Downloads/MPZAnalyticsRrdsFiles/tbl.CrescentProductionData.rds")
tbl.CPEProductionData <- tbl.CPEProductionData %>% dplyr::ungroup()
tbl.CPEStudyData <- tbl.CPEProductionData %>% dplyr::group_by(FileNo) %>%  dplyr::slice(1)
## get final CPEStudyData fields get rid of production stuff
tbl.CPEStudyData <- tbl.CPEStudyData %>% dplyr::select("FileNo",
                                                       "APINo","CurrentOperator","CurrentWellName","LeaseName","LeaseNumber","OriginalOperator","OriginalWellName","SpudDate",
                                                       "TD","CountyName","Township","Range","Section","QQ","Footages","FieldName","ProducedPools","OilWaterGasCums","IPTDateOilWaterGas",
                                                       "Wellbore","Latitude","Longitude","WellType","WellStatus","CTB","WellStatusDate","IpDate","IpOil","IpWater","IpGas",
                                                       "cumOil","cumWater","cumGas","cumDate",
                                                       "hFormation",
                                                       "dateStimulated",
                                                       "stimulatedFormation","topFt","bottomFt","stimulatedStages","volume","volumeUnits","typeTreatment","percentAcid","lbsProppant",
                                                       "maximumTreatmentPressurePSI","maximumTreatementRateBBLsPerMinute","Details",
                                                       "page3DataResult","onsightGeologist","exposureToTargetRaw","casingProforma")
## save file
saveRDS(tbl.CPEStudyData, file = "~/Downloads/MPZAnalyticsRrdsFiles/tbl.CPEStudyData.rds")
tbl.CPEStudyData <- readRDS(file = "~/Downloads/MPZAnalyticsRrdsFiles/tbl.CPEStudyData.rds")


colnames(tbl.CPEStudyData)
c("FileNo","indexBy30","Month","DayOfProduction","BOE","BBLsOil","BBLsWater","MCFProd","MCFSold","MCFVentFlare","BBLSSold",
  "cumBOE","cumDayOfProduction","cumBBLsOil","cumBBLsWater","cumMCFProd","cumMCFSold","cumMCFVentFlare","cumBBLsSold",
  "APINo","CurrentOperator","CurrentWellName","LeaseName","LeaseNumber","OriginalOperator","OriginalWellName","SpudDate",
  "TD","CountyName","Township","Range","Section","QQ","Footages","FieldName","ProducedPools","OilWaterGasCums","IPTDateOilWaterGas",
  "Wellbore","Latitude","Longitude","WellType","WellStatus","CTB","WellStatusDate","IpDate","IpOil","IpWater","IpGas",
  "cumOil","cumWater","cumGas","cumDate",
  "hFormation",
  "dateStimulated",
  "stimulatedFormation","topFt","bottomFt","stimulatedStages","volume","volumeUnits","typeTreatment","percentAcid","lbsProppant",
  "maximumTreatmentPressurePSI","maximumTreatementRateBBLsPerMinute","Details",
  "page3DataResult","onsightGeologist","exposureToTargetRaw","casingProforma")
colnames(tbl.whiting175)
c("fileNo","FileNo",
  "stimulatedFormation","topFT","bottomFT","stages","volume","volumeUnits","typeTreatment","percentAcid","lbsProppant",
  "maximumTreatmentPressurePSI","maximumTreatementRateBBLsPerMinute",
  "cumulative90BOE","cumDayOfProduction",
  "studyName","lon","lat","zoneAft","zoneBft","zoneCft","zoneAStages","zoneBStages","zoneCStages","lateralLength",
  "APINo","CurrentOperator","CurrentWellName","LeaseName","LeaseNumber","OriginalOperator","OriginalWellName","SpudDate","TD","CountyName","Township","Range","Section","QQ","Footages","FieldName","ProducedPools","OilWaterGasCums","IPTDateOilWaterGas","Wellbore","Latitude","Longitude","WellType","WellStatus","CTB","WellStatusDate","IpDate","IpOil","IpWater","IpGas","cumOil","cumWater","cumGas","cumDate")

## page3 data fields


## XTO page3 data
## from XTOCompletionData.numbers
tbl.XTOCompletionDataStudy <- XTOCompletionDataStudy

tbl.XTOCompletionDataStudy <- as_tibble(tbl.XTOCompletionDataStudy)

saveRDS(tbl.XTOCompletionDataStudy,file =  '~/Downloads/MPZAnalyticsRrdsFiles/tbl.XTOCompletionDataStudy.rds' )

### Whting Study #2 Enhanced Completions 85-100MPZA Output ----------------
### 
tbl.MPZAnalyticsECStudyOriginal <- dplyr::as_tibble(productionRangeOriginal600v2)
saveRDS(tbl.MPZAnalyticsECStudyOriginal,file =  '~/Downloads/MPZAnalyticsRrdsFiles/tbl.MPZAnalyticsECStudyOriginal.rds' )


## quick compare with tbl.CPEStudyData

tbl.ndTargetFormationByWell <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.ndTargetFormationByWell.rds')

tbl.CPEStudyData$FileNo %in% tbl.ndTargetFormationByWell$FileNo

tbl.ndTargetFormationByWell %>% 
  dplyr::filter(FileNo %in% tbl.CPEStudyData$FileNo)

tbl.compare <- dplyr::left_join(tbl.CPEStudyData, tbl.ndTargetFormationByWell, by = "FileNo")

View(tbl.compare %>% dplyr::select(FileNo, CurrentOperator, Operator, CurrentWellName, WellName, hFormation, targetFormation))

get.NDWellFilePdf <- function(FileNo, ndUserName = "iddgroupllc", ndPassword = 'Snow2Day', tbl.wellIndex, pdfFolder  = "myNDDownloads"){
  require(httr)
  require(XML)
  
  cat('========= ',FileNo,' ========================',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),'\n')
  
  
  ## check if directory exists if it doesn't create it
  if (!dir.exists(file.path("~/Downloads",pdfFolder))){
      ## create a new directory off of downloads
      
      dir.create(file.path("~/Downloads",pdfFolder))  
    }
  
  getWebData <- httr::GET(paste0('https://www.dmr.nd.gov/oilgas/FeeServices/wfiles/',substr(FileNo, start = 1, stop = 2),'/W',FileNo,'.pdf'),
                          httr::authenticate(ndUserName, ndPassword, type = "basic"),
                          httr::write_disk(paste0('~/Downloads/', pdfFolder, '/W', FileNo, ' - ',
                                                  tbl.wellIndex$CurrentWellName[which(tbl.wellIndex$FileNo == FileNo)], '.pdf'), 
                                           overwrite = TRUE))
  
  return(getWebData)
  
}

require(tidyverse)
## Download PDF Files in a New Directory off of Downloads
# system.time(ls.productionData <- tbl.NDwellIndex$FileNo %>% purrr:::map(scrape.NDwellProductionData))
## use purrr::walk to return
as.list(tbl.myStudyWellsMPZData$FileNo[1:2]) %>% purrr::pwalk(get.NDWellFilePdf, tbl.wellIndex = tbl.wellIndex, pdfFolder  = "myECDownloads") 
  

system.time(lapply(tbl.myStudyWellsMPZData$FileNo, function(.){
  cat(paste0('=== FileNo: ', ., '\n'))
  get.NDWellFilePdf(FileNo = ., tbl.wellIndex = tbl.wellIndex, pdfFolder = "myECDownloads")}))

purrr::pwalk(as.list(tbl.myStudyWellsMPZData$FileNo[1:2]), get.NDWellFilePdf, tbl.wellIndex = tbl.wellIndex, pdfFolder  = "myECDownloads") 

tbl.wellIndex

  lapply(tbl.myStudyWellsMPZData$FileNo[1:2], function(.){print(.)})
  purrr::wwalk(get.NDWellFilePdf)

as.list(tbl.myStudyWellsMPZData$FileNo[1:2])
purrr::pwalk()
get.NDWellFilePdf(FileNo = 31797, ndUserName = "iddgroupllc", ndPassword = 'Snow2Day', tbl.wellIndex, pdfFolder  = "myECDownloads")
  
  
source(file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsR/R/DSUShapeFunctions.R')
  

#### Find Wells in DSUs ----------------------------
tbl.DSUFullIndexbyFileNo      <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUFullIndexbyFileNo.rds' )

tbl.DSUFullIndexbyFileNo %>% dplyr::filter(FileNo == 21634)

tbl.DSUFullIndexbyFileNo %>% dplyr::filter(FileNo == 24062)

tbl.DSUFullIndexbyFileNo %>% dplyr::filter(FileNo == 22323)  
  
tbl.DSUFullIndexbyFileNo %>% dplyr::filter(FileNo == 25055)    

tbl.DSUFullIndexbyFileNo %>% dplyr::filter(FileNo == 33182)

tbl.DSUFullIndexbyFileNo %>% dplyr::filter(FileNo == 19119)
## Folvag
tbl.DSUFullIndexbyFileNo %>% dplyr::filter(FileNo %in% c(24181,29318,30142,30143))
