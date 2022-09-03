## Step 1 data collection ---------------------------------------
require(dplyr)
require(data.table)
## read in tbl.WellIndex
tbl.wellIndex              <- readRDS(file = "~/Downloads/MPZAnalyticsRrdsFiles/tbl.NDwellIndex.rds")

## Find HUNT OIL Wells -----------------------

tbl.HuntWellsIndex <- tbl.wellIndex %>% filter(CurrentOperator %like% "HUNT OIL")
## 277 Wells

# filter after 1/1/2007
# Use IpDate as all wells that have production have an IpDate
tbl.HuntWellsIndex_Production <- tbl.HuntWellsIndex %>% filter(IpDate >= as.POSIXct('2007-01-01'))
 ## 145 wells with an IpDate > 2007-01-01



# ## FindXTO wells ----------------------------------------------------------------------------------
# ## 
# tbl.XTOWellsIndex <- tbl.wellIndex %>% rowwise() %>% filter(CurrentOperator %like% "XTO")
# ## 1946 wells
# 
# ## filter after 1/1/2015
# tbl.XTOWellsIndex <- tbl.XTOWellsIndex %>% filter(IpDate >= as.POSIXct('2015-01-01'))
# ## 244 wells with an IpDate > 2015-01-01
# 
# ## restric to 5 counties
# tbl.XTOWellsIndex <- tbl.XTOWellsIndex %>% filter(CountyName %in% c('BILLINGS','DUNN','MCKENZIE','STARK','WILLIAMS'))
# ## 242 wells
# 
# ## restrict to williams county near sonya federal lease
# tbl.XTOWellsIndex <- tbl.XTOWellsIndex %>% filter(CountyName %in% c('WILLIAMS'))


## with at least +265 days since IpDate
## hmmm not accurte we don't know how many days on still
# tbl.XTOWellsIndex <- tbl.XTOWellsIndex[which((as.POSIXct('2017-11-30') - tbl.XTOWellsIndex$IpDate) >265),]
## 218 wells
## 

## WILLIAMS COUNTY WELLS
## 
# tbl.XTOWellsIndex <- tbl.XTOWellsIndex %>% filter(CountyName %in% c('WILLIAMS'))
## 52 wells
## fileNo <- 30372


## Find all middle bakken wells using XTO's A-D for Middle Bakken wells and E-Z For Three Forks Wells
# tbl.XTOWellsIndex <- tbl.XTOWellsIndex[ substr(tbl.XTOWellsIndex$CurrentWellName, nchar(tbl.XTOWellsIndex$CurrentWellName), nchar(tbl.XTOWellsIndex$CurrentWellName)  ) %in% c('A', 'B', 'C', 'D'),]
  
## use regex to find last character in current wellname
## 

get.wellProdData <- function(fileNo) {
## build webscraper for ND oil and gas by file no

require(httr)
require(XML)
cat('========= ',fileNo,' ====================================\n')
## https://www.dmr.nd.gov/oilgas/feeservices/getwellprod.asp?filenumber=29727
getWebData <- httr::GET(paste0('https://www.dmr.nd.gov/oilgas/feeservices/getwellprod.asp?filenumber=', fileNo),
                        httr::authenticate('ageoenergy', 'Wood4Lumber', type = "basic"),
                        httr::add_headers("Content-Type" = "application/json")
)


content <- rawToChar( getWebData$content)

# html <- "https://www.dmr.nd.gov/oilgas/feeservices/getwellprod.asp?filenumber=29727"

webDataTables <- readHTMLTable(content, header = 'Monthly Production Data', as.data.frame = T, stringsAsFactors=FALSE )
# print(webDataTables)
if(length(webDataTables) >2 ){
  names(webDataTables) = c( 'wellSummary', 'pTable1', 'pTable2')
} else{names(webDataTables)=c( 'wellSummary', 'pTable1')}
# str(webDataTables)

## pTable1$V1 will contain the summary data
# webDataTables$pTable1$V1[1]
## pTable2 contains the well production data in table format
# webDataTables$pTable2

## V1 - V9 are Pool	Date	Days	BBLS Oil	Runs	BBLS Water	MCF Prod	MCF Sold	Vent/Flare
colnames(webDataTables$pTable2) <- c('Pool',	'Date',	'Days',	'BBLS Oil',	'Runs',	'BBLS Water',	'MCF Prod',	'MCF Sold',	'Vent/Flare')


##add file number to pTable2
webDataTables$pTable2$fileNo <- fileNo

return(webDataTables)
}

## Lets get their production data
ls.HuntProductionData <-  lapply(tbl.HuntWellsIndex_Production$FileNo, get.wellProdData)

# ls.productionData <- ls.WhitingWellProductionData
get.NDProductionDataTransformedTable <- function(ls.productionData){
  require(dplyr)
  ## take pTable2, the production by month and file number
  tbl.NDProductionData <-  dplyr::bind_rows(lapply(ls.productionData, function(x) {x$pTable2}))
  
  ## take well list and add a 1- to the date
  tbl.NDProductionData <-  tbl.NDProductionData %>% 
    mutate(Date         = as.POSIXct(strptime(paste0('1-',Date), "%d-%m-%Y")),
           `BBLS Oil`   = as.numeric(`BBLS Oil`),
           Runs         = as.numeric(Runs),
           `BBLS Water` = as.numeric(`BBLS Water`),
           `MCF Prod`   = as.numeric(`MCF Prod`),
           `MCF Sold`   = as.numeric(`MCF Sold`),
           Days         = as.numeric(Days),
           `Vent/Flare` = as.numeric(`Vent/Flare`))
  ## make BOE/Days column
  tbl.NDProductionData <-  tbl.NDProductionData %>% 
    mutate(boe                   = `BBLS Oil` + `MCF Prod`/5.8,
           boePerDay             = boe/Days,
           bblsOilPerDay         = `BBLS Oil`/Days,
           bblsWaterPerDay       = `BBLS Water`/Days,
           mcfProducedPerDay     = `MCF Prod`/Days,
           mcfSoldPerDay         = `MCF Sold`/Days,
           mcfVentedFlaredPerDay = `Vent/Flare`/Days,
           bblsSoldPerDay        = Runs/Days)
  
  glimpse(tbl.NDProductionData)
  ## get rid of all 0 days and NaN BOE/Days
  tbl.NDProductionData <- tbl.NDProductionData %>% filter(Days > 0)
  
  ## sort by fileNo and sort dates ascending
  tbl.NDProductionData <- tbl.NDProductionData %>% arrange(fileNo, Date)
  
  
  
  ## expand the table if 7 days at 1000 make 7 rows of 1000
  ## rep bbls oil, water, mcf, mcf sold, vent/flare
  repBOE          <- unlist(mapply(rep, times = tbl.NDProductionData$Days, x = tbl.NDProductionData$boePerDay))
  repBBLsOil      <- unlist(mapply(rep, times = tbl.NDProductionData$Days, x = tbl.NDProductionData$bblsOilPerDay))
  repBBLsWater    <- unlist(mapply(rep, times = tbl.NDProductionData$Days, x = tbl.NDProductionData$bblsWaterPerDay))
  repMCFProd      <- unlist(mapply(rep, times = tbl.NDProductionData$Days, x = tbl.NDProductionData$mcfProducedPerDay))
  repMCFSold      <- unlist(mapply(rep, times = tbl.NDProductionData$Days, x = tbl.NDProductionData$mcfSoldPerDay))
  repMCFVentFlare <- unlist(mapply(rep, times = tbl.NDProductionData$Days, x = tbl.NDProductionData$mcfVentedFlaredPerDay))
  repBBLSSold     <- unlist(mapply(rep, times = tbl.NDProductionData$Days, x = tbl.NDProductionData$bblsSoldPerDay))
  ## make day of 1
  repSingleDays <- unlist(mapply(rep, times = tbl.NDProductionData$Days, x = 1))
  ## repeat file number by days
  repFileNo <- unlist(mapply(rep, times = tbl.NDProductionData$Days, x = tbl.NDProductionData$fileNo))
  
  tbl.EX <- data_frame(FileNo          = repFileNo, 
                       DayOfProduction = repSingleDays, 
                       BOE             = repBOE,
                       BBLsOil         = repBBLsOil,
                       BBLsWater       = repBBLsWater,
                       MCFProd         = repMCFProd,
                       MCFSold         = repMCFSold,
                       MCFVentFlare    = repMCFVentFlare,
                       BBLSSold        = repBBLSSold)
  
  tbl.testGrouping <- tbl.EX %>% group_by(FileNo) %>% mutate(nFile     = n(),
                                                             indexBy30 = gl(ceiling(n()/30), 30, n())) 

  tbl.testGrouping$DayOfProduction
    
  tbl.testGrouping <- tbl.testGrouping %>% 
    group_by(FileNo,indexBy30) %>% 
    summarise_at( c('DayOfProduction', 'BOE', 'BBLsOil', 'BBLsWater', 'MCFProd', 'MCFSold', 'MCFVentFlare', 'BBLSSold'),sum)
  #summarise_each(funs(sum), DayOfProduction, BOE, BBLsOil, BBLsWater, MCFProd, MCFSold, MCFVentFlare, BBLSSold)
  
  ## fix the indexBy30 bug with seq(n()) and remove indexBy30
  tbl.testGrouping <- tbl.testGrouping %>% group_by(FileNo) %>% mutate( Month              = seq(n()),
                                                                        cumBOE             = cumsum(BOE),
                                                                        cumDayOfProduction = cumsum(DayOfProduction),
                                                                        cumBBLsOil         = cumsum(BBLsOil),
                                                                        cumBBLsWater       = cumsum(BBLsWater),
                                                                        cumMCFProd         = cumsum(MCFProd),
                                                                        cumMCFSold         = cumsum(MCFSold),
                                                                        cumMCFVentFlare    = cumsum(MCFVentFlare),
                                                                        cumBBLsSold        = cumsum(BBLSSold))
  # fix short 30s
  tbl.testGrouping <- tbl.testGrouping %>% filter(cumDayOfProduction %% 30 == 0) %>% 
    group_by(FileNo)
  
  return(tbl.testGrouping)
}

tbl.HuntProductionData <- get.NDProductionDataTransformedTable(ls.HuntProductionData)

## save Updated data
saveRDS(tbl.HuntProductionData, file = "~/Documents/Business/AGEO Energy Consult/HuntOil/Development/Hunter/RDSFiles/tbl.HuntProductionData.rds")

## check HuntProductionData against other Premium service data
## namely wellIndex Cum totals
## 


unique(tbl.HuntProductionData$FileNo)
tbl.HuntProductionData %>% dplyr::filter(indexBy30 == 1)
tbl.HuntProductionData %>% dplyr::filter(indexBy30 == 12)
tbl.HuntProductionData %>% dplyr::filter(indexBy30 == 24)

tbl.HuntProductionData %>% dplyr::filter(indexBy30 == 36)
tbl.HuntProductionData %>% dplyr::filter(indexBy30 == 48)
tbl.HuntProductionData %>% dplyr::filter(indexBy30 == 60) %>% tail()


require(plotly)
require(RColorBrewer)

plotly::plot_ly() %>% 
  plotly::add_lines(x = ~tbl.HuntProductionData$Month, y = ~tbl.HuntProductionData$BOE, color = ~tbl.HuntProductionData$catFileNo, showlegend = FALSE,colors=colorRampPalette(brewer.pal(11,"Spectral"))(112), line = list(width=1)) %>% 
  plotly::add_trace(data = tbl.bakkenProductionDataMean, x=~Month, y = ~BOE, type="scatter", mode = "lines",name = 'Avg', line = list(color = 'rgb(22, 96, 167)', width = 4)) %>% 
  plotly::add_trace(data = tbl.bakkenProductionDataP10P90, x=~Month, y = ~p10, type="scatter", mode = "lines",name = 'p10', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash')) %>% 
  plotly::add_trace(data = tbl.bakkenProductionDataP10P90, x=~Month, y = ~p90, type="scatter", mode = "lines",name = 'p90', line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash')) %>% 
  layout(title = "112 Whiting Wells Bakken Production Data By Month",
         xaxis = list(title = "Month",
                      autotick = FALSE,
                      range = c(1,18),
                      ticks = "outside",
                      tick0 = 0,
                      dtick = 1,
                      ticklen = 5,
                      tickwidth = 2,
                      tickcolor = toRGB("blue")),
         yaxis = list(title = "BOE",
                      type = 'log', 
                      range = c(2.5, 5))) 

## recreate this using whatever operator WellsIndex file you are using
## when I have time make the file an arguement to pass throubh
get.HuntNDWellFilePdf <- function(fileNo){
  cat('========= ',fileNo,' ========================',strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"),'\n')
  
  getWebData <- httr::GET(paste0('https://www.dmr.nd.gov/oilgas/FeeServices/wfiles/',substr(fileNo, start = 1, stop = 2),'/W',fileNo,'.pdf'),
                          httr::authenticate('ageoenergy', 'Wood4Lumber', type = "basic"),
                          httr::write_disk(paste0('~/Documents/Business/AGEO Energy Consult/HuntOil/Development/Hunter/PDFWellFiles/W', 
                                                  fileNo,' - ',tbl.HuntWellsIndex$CurrentWellName[which(tbl.HuntWellsIndex$FileNo == fileNo)],'.pdf'), overwrite=TRUE))
}
## wellFile.pdf Download ------------------------------------------

## get all wellfiles including non production wells for Hunt
## 
## lets go with a FileNo >= 16776 which is the 2007 well start
tbl.HuntWellsIndexRecent <- tbl.HuntWellsIndex %>% filter(FileNo >= 16776)
## 217 wells
## get all 217 wellfiles
lapply(tbl.HuntWellsIndexRecent$FileNo, get.HuntNDWellFilePdf)

## use this because we already downloaded 145 of the 217
lapply(tbl.HuntWellsIndexRecent$FileNo[!(tbl.HuntWellsIndexRecent$FileNo %in% tbl.HuntWellsIndex_Production$FileNo)], get.HuntNDWellFilePdf)



fileNo <- 16776
## lasfile download -----------------------------------------------
## modify path to XTOWellFiles/XTOlasFiles
get.digitalAndImageLogsHunt <- function(fileNo) {
  ## build webscraper for ND oil and gas by file no
  # library(rvest)
  # library(stringr)
  # library(tidyr)
  # require(httr)
  # require(XML)
  # library(xml2)
  cat('========= ',fileNo,' ====================================\n')
  ## https://www.dmr.nd.gov/oilgas/feeservices/getwellprod.asp?filenumber=29727
  getWebData <- httr::GET(paste0('https://www.dmr.nd.gov/oilgas/feeservices/getscoutticket.asp?filenumber=', fileNo),
                          httr::authenticate('ageoenergy', 'Wood4Lumber', type = "basic"),
                          httr::add_headers("Content-Type" = "application/json")
  )
  
  require(rvest)
  
  content <- rawToChar( getWebData$content)
  
  ## us rvest and the xml2 package to read and find all of the href tags
  digitalImageLogList <- rvest::html_attr(rvest::html_nodes(rvest::read_html(content), "a"), "href")
  ## keep only the tags referenceing digital and image logs
  digitalImageLogList <- digitalImageLogList[grep('dlogs', digitalImageLogList)]
  
  ## keep only dtsm.las files
  #digitalImageLogList <- digitalImageLogList[grep('DTSM', digitalImageLogList)]
  digitalImageLogList <- digitalImageLogList[grep('.las', digitalImageLogList)]
  
  ## basename turns "/oilgas/feeservices/dlogs/29/29726/29726-DTSM1a.las" into "29726-DTSM1b.las"
  ## so will gsub('.*/','',digitalImageLogList)
  print(digitalImageLogList)
  
  lapply(digitalImageLogList, function(x){
    cat('========= ', basename(x),'  ==\n')
    getWebData <- httr::GET(paste0('https://www.dmr.nd.gov', x),
                            httr::authenticate('ageoenergy', 'Wood4Lumber', type = "basic"),
                            httr::write_disk(paste0('~/Documents/Business/AGEO Energy Consult/HuntOil/Development/Hunter/LASFiles/W',basename(x)), overwrite=TRUE))
  })
  
  
}

## download lasfiles
lapply(tbl.HuntWellsIndex$FileNo[1], get.digitalAndImageLogsHunt)

lapply(tbl.HuntWellsIndex$FileNo, get.digitalAndImageLogsHunt)

# <a href="javascript: launchDirSurveys('2383');">view directional survey data for 2383</a>
# var dirSurveysURL = "https://www.dmr.nd.gov/oilgas/feeservices/getsurveydata.asp";
# var dirSurveysURL = "./getsurveydata.asp";
# function launchDirSurveys(theIDNumber) {
#   if (theIDNumber.length == 0) {
#     theIDNumber = '';
#   }
#   var theURL = dirSurveysURL+"?ID="+theIDNumber;
#   w = window.open(theURL,"dirSurveyDataWindow",scrollWindowAttributeSettings);
#   w.focus();
# }

tbl.xtoWellsIndex

## download Survey Data --------------------  
##"javascript:launchDirSurveys('3D157BCB31086');"
get.directionalSurveyData <- function(fileNo) {
  ## build webscraper for ND oil and gas by file no
  # library(rvest)
  # library(stringr)
  # library(tidyr)
  # require(httr)
  # require(XML)
  # library(xml2)
  cat('========= ',fileNo,' ====================================\n')
  ## https://www.dmr.nd.gov/oilgas/feeservices/getwellprod.asp?filenumber=29727
  getWebData <- httr::GET(paste0('https://www.dmr.nd.gov/oilgas/feeservices/getscoutticket.asp?filenumber=', fileNo),
                          httr::authenticate('ageoenergy', 'Wood4Lumber', type = "basic"),
                          httr::add_headers("Content-Type" = "application/json")
  )
  
  
  content <- rawToChar( getWebData$content)
  
  ## us rvest and the xml2 package to read and find all of the href tags
  digitalImageLogList <- rvest::html_attr(rvest::html_nodes(rvest::read_html(content), "a"), "href")
  
  
  ## we want the value between the quotes here "javascript:launchDirSurveys('3D157BCB31086');"
  digitalImageLogList <- digitalImageLogList[grep('launchDirSurveys', digitalImageLogList)]
  ## now remove javascript:launchDirSurveys('
  theIDNumber <- unique(gsub("'\\);",'',gsub("javascript:launchDirSurveys\\('", '', digitalImageLogList)))
  
  
  "https://www.dmr.nd.gov/oilgas/feeservices/getsurveydata.asp"
  getWebData <- httr::GET(paste0('https://www.dmr.nd.gov/oilgas/feeservices/getsurveydata.asp?ID=', theIDNumber),
                          httr::authenticate('ageoenergy', 'Wood4Lumber', type = "basic"),
                          httr::add_headers("Content-Type" = "application/json"))
  
  
  content <- rawToChar( getWebData$content)
  
  ls.directionalSurvey <- XML::readHTMLTable(content, header = 'largeTableOutput', as.data.frame = T, stringsAsFactors=FALSE )
 
  tbl.directionalSurvey <- dplyr::tbl_df(ls.directionalSurvey$largeTableOutput)
  ## assign column names
  colnames(tbl.directionalSurvey) <-  c("APINo",	"FileNo","Leg",	"MD",	"Inc",	"Azi",	"TVD",	"FtNS",	"NS",	"FtEW","EW",	"Lat",	"Long")
  
  return(tbl.directionalSurvey)
  
  # ## keep only the tags referenceing digital and image logs
  # digitalImageLogList <- digitalImageLogList[grep('dlogs', digitalImageLogList)]
  # 
  # ## keep only dtsm.las files
  # digitalImageLogList <- digitalImageLogList[grep('DTSM', digitalImageLogList)]
  # digitalImageLogList <- digitalImageLogList[grep('.las', digitalImageLogList)]
  # 
  # ## basename turns "/oilgas/feeservices/dlogs/29/29726/29726-DTSM1a.las" into "29726-DTSM1b.las"
  # ## so will gsub('.*/','',digitalImageLogList)
  # 
  # 
  # lapply(digitalImageLogList, function(x){
  #   cat('========= ', basename(x),'  ==\n')
  #   getWebData <- GET(paste0('https://www.dmr.nd.gov', x),
  #                     authenticate('iddgroupllc', 'Snow2Day', type = "basic"),
  #                     write_disk(paste0('~/Documents/Business/IDDGroup/OMPADG/XTOWellFiles/XTOlasFiles/W',basename(x)), overwrite=TRUE))
  # })
  # 
  
}

## get all survey data for wells
## download surveyData -----------------------------------------------
tbl.directionalSurveyDataHunt <- dplyr::bind_rows(lapply(tbl.HuntWellsIndex$FileNo, get.directionalSurveyData))



## get 90 day cumulative production from  production data -----------
tbl.90DayXTO <- tbl.XTOProductionData %>% filter(Month == 3)


## merge in well data from index

tbl.90DayXTO <- dplyr::left_join(tbl.90DayXTO, tbl.XTOWellsIndex, by = "FileNo")

tbl.90DayXTOSample <- tbl.90DayXTO %>% arrange(desc(cumBOE)) %>% select( FileNo,IpDate, CurrentWellName, cumBOE, FieldName,CountyName, LeaseName, Latitude, Longitude)

## keep only key fields for id
## copy to Numbers for manual data entry --------------------------
write.table(tbl.90DayXTOSample, file=pipe("pbcopy"), quote = FALSE, sep="\t", row.names = FALSE)

tbl.fixXTO90day <- paste_from_clipboard()

tbl.fixXTO90day <- dplyr::left_join(tbl.fixXTO90day, tbl.xtoWellsIndex, by = "FileNo")

write.table(tbl.fixXTO90day, file=pipe("pbcopy"), quote = FALSE, sep="\t", row.names = FALSE)


## get survey data
tbl.fixDirectionalSurveyDataXTO <- bind_rows(lapply(tbl.fixXTO90day$FileNo, get.directionalSurveyDataXTO))

## filter Hoffland Wellpad
# 30372m 29116, 29117

tbl.fixDirectionalSurveyDataXTO[which(tbl.fixDirectionalSurveyDataXTO$FileNo %in% c(30327,29116,29117)),]
write.table(tbl.fixDirectionalSurveyDataXTO[which(tbl.fixDirectionalSurveyDataXTO$FileNo %in% c(30327,29116,29117)),], file=pipe("pbcopy"), quote = FALSE, sep="\t", row.names = FALSE)

## keep only fields needed for data entry of completion data
tbl.90DayXTO <- tbl.90DayXTO %>% select(CountyName, LeaseName, FileNo, CurrentWellName, cumBOE, stimulatedFormation, topFT, bottomFT, stages, volume, volumeUnits,
                                        typeTreatment, percentAcid, lbsProppant, maximumTreatmentPressurePSI, maximumTreatementRateBBLsPerMinute, CurrentOperator)


FileNo <-  29726
library(roxygen2)
### Download ND Survey Data BEGINNING -------------------------
#' get.ND.directionalSurveyData
#'
#' Web scraper of directional survey data from ND Premium services website
#'
#' @param FileNo ND fileno from WellIndex.csv
#' @param ndUserName purchased at ND.gov
#' @param ndPassword purchased at ND.gov
#'
#' @return tibble of directional survey data
#' @export
#'
#' @examples
#' \dontrun{tbl.myStudyDirectionalSurveys <- tbl.myStudyWellsMPZData$FileNo %>% purrr::map_dfr(get.ND.directionalSurveyData)}
get.ND.directionalSurveyData <- function(FileNo, 
                                         ndUserName = "iddgroupllc",
                                         ndPassword = 'Snow2Day') {
  ## build webscraper for ND oil and gas by file no
  # require(rvest)
  # require(tidyverse)
  # require(httr)
  # require(XML)
  # require(xml2)
  
  cat('========= ',FileNo,' ====================================\n')
  ## https://www.dmr.nd.gov/oilgas/feeservices/getwellprod.asp?filenumber=29727
  getWebData <- httr::GET(paste0('https://www.dmr.nd.gov/oilgas/feeservices/getscoutticket.asp?filenumber=', FileNo),
                          httr::authenticate(ndUserName, ndPassword, type = "basic"),
                          httr::add_headers("Content-Type" = "application/json"))
  
  ## convert to characthers
  content <- rawToChar( getWebData$content)
  
  # xml2::read_html() reads in the base content as an html page
  # rvest::html_nodes takes the html page and looks for the css "a" identifier as in  <a href="javascript:launchDirSurveys('D001F8F829726');">
  # rvest::html_attr(x,name = "href") makes a list of all of the hrefs 
  
  read_hrefs <- rvest::html_attr(x = rvest::html_nodes(x = xml2::read_html(content), css = "a"), name = "href")
  ## we want the id value between the quotes here "javascript:launchDirSurveys('3D157BCB31086');" 
  ## which we will use to call up the page with the directional surves
  ## we'll use grep this will most likely find 2 IDs so we'll use unique to keep only one
  theIDValue <- read_hrefs[grep('launchDirSurveys', read_hrefs)]
  
  theIDValue <- unique(gsub("'\\);", '', gsub("javascript:launchDirSurveys\\('", '', theIDValue)))
  
  ## now use httr::GET to retreive 'https://www.dmr.nd.gov/oilgas/feeservices/getsurveydata.asp?ID='
  getWebData <- httr::GET(paste0('https://www.dmr.nd.gov/oilgas/feeservices/getsurveydata.asp?ID=', theIDValue),
                          httr::authenticate(ndUserName, ndPassword, type = "basic"),
                          httr::add_headers("Content-Type" = "application/json"))
  
  ## convert from raw to charachter
  content <- rawToChar(getWebData$content)
  
  ## use XML::readHTMLTable to read the content from header = 'largeTableOutput;
  ls.directionalSurvey <- XML::readHTMLTable(content, header = 'largeTableOutput', as.data.frame = T, stringsAsFactors = FALSE )
  
  tbl.directionalSurvey <- tibble::as_tibble(ls.directionalSurvey$largeTableOutput)
  ## assign column names
  colnames(tbl.directionalSurvey) <-  c("APINo",	"FileNo","Leg",	"MD",	"Inc",	"Azi",	"TVD",	"FtNS",	"NS",	"FtEW","EW",	"Lat",	"Long")
  
  ## delete duplicate MD
  
  tbl.directionalSurvey <- tbl.directionalSurvey[!duplicated(tbl.directionalSurvey$MD),]
  
  return(tbl.directionalSurvey)
}



get.ND.directionalSurveyData(FileNo = 29726)


## cleanup and remove TFT wells
## Get all Whiting wells data including original study
library(data.table)
tbl.whiting175 <- readRDS(file = '~/Documents/Business/IDDGroup/OMPADG/whiting175.rds')

tbl.myStudyWells <- tbl.whiting175 %>% 
  dplyr::filter(!OriginalWellName %like% c('TF'))

                
## whiting 158 after TFT removal
## keep only the 158

## use purrr::map_dfr to get one big table
system.time(tbl.myStudyDirectionalSurveys <- tbl.myStudyWells %>% dplyr::arrange(FileNo) %>% .$FileNo %>% purrr::map_dfr(get.ND.directionalSurveyData))

## use mutate to change the variable types
tbl.myStudyDirectionalSurveys <- tbl.myStudyDirectionalSurveys %>% 
  dplyr::mutate(FileNo = as.integer(FileNo),
                MD     = as.numeric(MD),
                Inc    = as.numeric(Inc),
                Azi    = as.numeric(Azi),
                TVD    = as.numeric(TVD),
                FtNS   = dplyr::if_else(NS == "S", -as.numeric(FtNS), as.numeric(FtNS)),
                FtEW   = dplyr::if_else(EW == "W", -as.numeric(FtEW), as.numeric(FtEW)),
                Lat    = as.numeric(Lat),
                Long   = as.numeric(Long)) %>% 
  dplyr::select(-NS, - EW)

view(tbl.myStudyDirectionalSurveys %>% dplyr::filter(FileNo == 28715))
## final step, FileNo 31795 has multiple casing point entries one for VERT, LAT1, STK1 etc.
## This needs to be cleaned up and flagged at this point.  Already we get rid of the duplicate MD due to VERT, LAT1

cleanSurveySidetracks <- function(surveys,...){
  # view(ECWhitingDirectionalSurveys %>% dplyr::group_by(FileNo, Leg) %>% dplyr::summarise(minDepth = min(MD),
  #                                                                                        maxDepth = max(MD)) %>% dplyr::group_by(FileNo) %>% dplyr::filter(n()>2) %>% dplyr::filter(Leg != "VERT"))
  # 
  
  ## surveys <- ECWhitingDirectionalSurveys %>% dplyr::filter(FileNo == 29227)
  myFilterList <- surveys %>% 
    dplyr::group_by(FileNo, Leg) %>% 
    dplyr::summarise(minDepth = min(MD),
                     maxDepth = max(MD)) %>% 
    dplyr::group_by(FileNo) %>% 
    dplyr::arrange(FileNo,minDepth)
  ## ND specific, sometimes they screw up based on their format file No 29227 is one such case
  # myFilterList <- filterList %>% dplyr::filter(FileNo == 29224)
  #  myFilterList
  # A tibble: 3 x 4
  # Groups:   FileNo [1]
  # FileNo Leg   minDepth maxDepth
  # <int> <chr>    <dbl>    <dbl>
  # 1  29227 VERT      1917     9837
  # 2  29227 STK1      3506    21308
  # 3  29227 LAT1      9837    11292
  ##
  ## Therefore we'll seperate VERT out
  ## ordere the rest by maxDepth, then add VERT back in first
  myFilterListVERT  <- myFilterList %>% dplyr::filter(Leg == 'VERT')
  myFilterListOther <- myFilterList %>% dplyr::filter(Leg != 'VERT') %>% dplyr::arrange(maxDepth)
  
  myFilterList      <- dplyr::bind_rows(myFilterListVERT, myFilterListOther)
  myFilterListOrder <- myFilterList %>% dplyr::mutate(maxDepth = dplyr::if_else(maxDepth > dplyr::lead(minDepth) & row_number() < n(), dplyr::lead(minDepth), maxDepth))
  ## now if one Leg maxDepth is <= the minDepth we delete the leg and reorder
  ## flag if sidetrack
  if(nrow(myFilterListOrder) > 2){
    cat('xxxxxxxx SideTrack Alert FileNo = ',surveys$FileNo[1],' nrow = ',nrow(myFilterListOrder),' xxxxxxxx\n')
    print(myFilterListOrder)
  } 
  
  myFilterListOrder <- myFilterListOrder %>% dplyr::filter(minDepth < maxDepth) 
  if(nrow(myFilterListOrder >1)){
    myFilterListOrder <- myFilterListOrder %>% 
      dplyr::mutate(maxDepth = dplyr::if_else(maxDepth > dplyr::lead(minDepth) & row_number() < n(), dplyr::lead(minDepth), maxDepth))
  }
  
  ## flag if sidetrack
    print(myFilterListOrder)
  
 
  ## use map_dfr trick with nrow to bind_rows all legs in order
  mySurveyOrdered <- purrr::map_dfr(seq(1,nrow(myFilterListOrder)), function(i){
    ## we want to keep the last row as it is TD
    if(i == nrow(myFilterListOrder)){
      cat('=== last row ',myFilterListOrder$maxDepth[i],'\n' )
      surveyLeg <- dplyr::filter(surveys, Leg == myFilterListOrder$Leg[i]) %>% dplyr::filter(MD <= myFilterListOrder$maxDepth[i])
      print(tail(surveyLeg))
    } else{
      surveyLeg <- dplyr::filter(surveys, Leg == myFilterListOrder$Leg[i]) %>% dplyr::filter(MD < myFilterListOrder$maxDepth[i])
    }
    
    return(surveyLeg)
  })
  
  return(mySurveyOrdered)
}

## clean ND survey files, VERT and LAT will share the same end/start MD, we'll delete VERT End to get rid of duplicates
## the same follows if there is a side track

tbl.cleanSurveys <- tbl.myStudyDirectionalSurveys %>% 
  split(.$FileNo) %>% purrr::map_dfr(cleanSurveySidetracks)

view(tbl.cleanSurveys %>% dplyr::filter(FileNo == 28715))
## remove any duplicate MD often from Leg VERT to Leg LAT1 the first LAT1 will be a duplicate
unique(tbl.cleanSurveys$FileNo)
## save survey data for EC Study
saveRDS(tbl.cleanSurveys,              file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/PCdataEC/ECWhitingDirectionalSurveys.rds')
## read in ECWhiting Directional Surveys
ECWhitingDirectionalSurveys <- readRDS(file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/PCdataEC/ECWhitingDirectionalSurveys.rds')

view(tbl.cleanSurveys %>% dplyr::filter(FileNo == 28715))
# tbl.OriginalWhitingDirectionalSurveys <- readRDS( file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/tbl.OriginalWhitingDirectionalSurveys.rds')






### Download ND Survey Data END -------------------------

## 3D Survey creation BEGINNING -----------------------

#' get.3dSurveyData
#' 
#' This function takes the ND table of surveys (that include Lat/Long)
#' Identifies which utm section they are in and creates Easting/Northing fieldnames
#'
#' @param svy 
#' 
#' @import tidyverse
#' @import sf
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{tbl.3dSurveys <- tbl.3dSurveys %>%
#'                       split(.$FileNo) %>%
#'                       purrr::map_dfr(get.3dSurveyData)
#'                       saveRDS(tbl.3dSurveys, file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/tbl.OriginalWhitingDirectionalSurveys.rds')
#'                       tbl.OriginalWhitingDirectionalSurveys <- readRDS( file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/tbl.OriginalWhitingDirectionalSurveys.rds')}
get.3dSurveyData <- function(svy){
  # require(tidyverse)
  # require(sf)
  # 
  
  
  ## 
  ## S12 is course length
  ## alpha calculation Eq-9 pg 26 Compendium 1
  ## dls comes from page 26 Compendium
  ## 
  ## aziLR transforms 180 - 360 into -0 -180 so we can determine if in the azi change whether we went to the left or to the right
  ## atfiRad is the equation from Eq 48 pg 31 Compendium 1 T1 was used since what we want is "what was the calculated TF between these 2 survey stations?"
  ## or put another way "What was the TF I slid at the start of the last survey to get to this survey point?"  Whether we slid or not is irrelevant, this is what happened.
  ## When no sliding occurred it is interpreted that this is the way the car drives if formation did not change.
  ## atf1Rad   = atan((sin(incRad)*sin(aziRad - dplyr::lag(aziRad))) / ((sin(incRad)*cos(dplyr::lag(incRad))*cos(aziRad - dplyr::lag(aziRad))) - sin(dplyr::lag(incRad))*cos(incRad))),
  ## atf1   = atan((sin(incRad)*sin(aziRad - dplyr::lag(aziRad))) / ((sin(incRad)*cos(dplyr::lag(incRad))*cos(aziRad - dplyr::lag(aziRad))) - sin(dplyr::lag(incRad))*cos(incRad)))*180/pi,
  ## myTF = dplyr::if_else(az2Maz1 < 0 & atf1 > 0, atf1 - 180, dplyr::if_else(az2Maz1 > 0 & atf1 < 0, atf1+180, atf1))) This works but I don't know why.
  ## It appears that when atf1Rad is < 0 we are heading down in inclination, left and right can't be determined in any way other than looking at the Azi change from a -/+ view around HS
  ## 
  ## 
  ## falpha  = tan(alpha/2) / (alpha/2) this is used in equation 11 pg 26 Compendium 1
  ## p2.vector = p1.vector + ((S12 * falpha)/2) *(t1.vector + t2.vector)
  ## if alpha < 0.02 radians we use falphHorner eq 12 pg 26 Compendium 1
  ## f.alphaHorner <- 1 + ((alpha^2)/12) * (1 + ((alpha^2)/10) * (1 + ((alpha^2)/168) * (1 + ((31 * alpha^2)/18))))
  ##  
  
  ## 
  svy <- svy  %>% 
    dplyr::mutate(S12     = MD - dplyr::lag(MD),
                  incRad  = Inc*(pi/180),
                  aziRad  = Azi*(pi/180),
                  alpha   = 2 * asin(sqrt((sin((incRad - dplyr::lag(incRad))/2)^2) + sin(dplyr::lag(incRad))*sin(incRad) * (sin((aziRad - dplyr::lag(aziRad))/2)^2))),
                  myBeta = acos(cos(incRad - dplyr::lag(incRad)) - sin(dplyr::lag(incRad))*sin(incRad)*(1 - cos(aziRad - dplyr::lag(aziRad)))),
                  falpha  = tan(alpha/2) / (alpha/2),
                  # falphaHorner  = get.f.alphaHorner(alpha),
                  dls     = ((18000*alpha) / pi) / S12,
                  aziLR   = dplyr::if_else(Azi > 180, Azi - 360, Azi),
                  az2Maz1 = aziLR - dplyr::lag(aziLR)) %>% 
    dplyr::mutate(atf1Rad  = atan((sin(incRad)*sin(aziRad - dplyr::lag(aziRad))) / ((sin(incRad)*cos(dplyr::lag(incRad))*cos(aziRad - dplyr::lag(aziRad))) - sin(dplyr::lag(incRad))*cos(incRad))),
                  atf1     = atan((sin(incRad)*sin(aziRad - dplyr::lag(aziRad))) / ((sin(incRad)*cos(dplyr::lag(incRad))*cos(aziRad - dplyr::lag(aziRad))) - sin(dplyr::lag(incRad))*cos(incRad)))*180/pi,
                  toolFace = dplyr::if_else(az2Maz1 < 0 & atf1 > 0, atf1 - 180, dplyr::if_else(az2Maz1 > 0 & atf1 < 0, atf1 + 180, atf1)))
  # 
  # 
  # ## transform from lat/long to Northing Easting utm
  ## the us is divided up into 6deg long zones
  ## ND -108 to -102 is Zone 13 crs = 32613 crs = "+proj=utm +zone=13 +north +datum=WGS84 +units=ft +no_defs" to get ft
  ##  or use st_crs("+init=epsg:32613 +units=ft")
  ## ND -102 to -96 is Zone 14 crs = 32614
  ## source https://epsg.io/map#srs=32613&x=653324.019928&y=5337144.970185&z=2&layer=streets
  
  
  
  ## find utmZone 
  
  get.UTMZonefromLatLong <- function(Latitude, Longitude) {
    utmZone <-  (floor((Longitude + 180) / 6) %% 60) + 1
    
    if(Latitude > 0) {
      utmZone <- utmZone + 32600} 
    else{
      utmZone <- utmZone + 32700
    }
    return(utmZone)
  }
  
  
    
    
    cat(paste0("===================== ", svy$FileNo[1],"*** ", nrow(svy), " ===============\n"))
    print(svy)
  
  ## use get.UTMZonefromLatLong to get the crs setting off of UTM
  svy.crs <- unique(get.UTMZonefromLatLong(Latitude = svy$Lat[1], Longitude = svy$Long[1]))
  
  
  ## we're assuming that the survey lat long from the Horizontals shape files ND Coordinate Reference System:
  ## EPSG: 4267 
  ## proj4string: "+proj=longlat +datum=NAD27 +no_defs"
  svy.shape <- sf::st_as_sf(svy, coords = c("Long", "Lat"), crs = 4267)
  ## transform the lat long into UTM northing and easting
  svy.shape <- svy.shape %>% sf::st_transform(crs = paste0("+proj=utm +zone=", dplyr::if_else(svy.crs < 32700, svy.crs -32600, svy.crs - 32700),
                                                           " +",dplyr::if_else(svy.crs < 32700, "north", "south"),
                                                           " +datum=WGS84 +units=ft +no_defs"))
  
  ## now extract new northing and easting coordintates from svy.shape and add back to svy
  svy.utm <- tibble::as_tibble(st_coordinates(svy.shape))
  
  ## create Northing and Easting Columns
  svy$Easting  <- svy.utm$X
  svy$Northing <- svy.utm$Y
  
  ## calculate DLS
  
  return(svy)
}

## Use split to map_dfr ------------------------------------
# tbl.3dSurveys %>%
#   split(.$FileNo) %>%
#   purrr::map_dfr(get.3dSurveyData, .id = "FileNo")  


#Read in study data directional surveys
# tbl.OriginalWhitingDirectionalSurveys <- readRDS( file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/tbl.OriginalWhitingDirectionalSurveys.rds')
## read in ECWhiting Directional Surveys
ECWhitingDirectionalSurveys <- readRDS(file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/PCdataEC/ECWhitingDirectionalSurveys.rds')

tbl.3dSurveys <- ECWhitingDirectionalSurveys %>%
  split(.$FileNo) %>%
  purrr::map_dfr(get.3dSurveyData)

# saveRDS(tbl.3dSurveys,                            file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/tbl.OriginalWhitingDirectionalSurveys.rds')
saveRDS(tbl.3dSurveys,                  file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/PCdataEC/ECWhitingDirectionalSurveys.rds')
ECWhitingDirectionalSurveys <- readRDS( file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/PCdataEC/ECWhitingDirectionalSurveys.rds')

unique(tbl.3dSurveys$FileNo)

## 3D Survey creation END -----------------------


### Download ND LAS files BEGINNING ------------
FileNo <- 25982

FileNo <- 26428
FileNo <- 29777


#' get.ND.digitalAndImageLogs
#'
#' Web scraper of DTSM las files from ND Premium services website
#' This function chooses the largest file of the DTSM list
#' and downloads it to pdfFolder which defaults to Downloads on mac
#'
#' @param FileNo ND fileno from WellIndex.csv
#' @param ndUserName purchased at ND.gov
#' @param ndPassword purchased at ND.gov
#'
#' @return tibble of directional survey data
#' @import tidyverse
#' @import rvest
#' @import httr
#' @import XML
#' @import xml2
#' @import purrr
#'
#' @return returns nothing
#' @export
#'
#' @examples
#' \dontrun{get.ND.digitalAndImageLogs(FileNo = 29777,pdfFolder = "myNDDownloads/OriginalStudyLASFiles")
#' download all las files
#'system.time(tbl.myStudyWellsMPZData$FileNo %>% purrr::walk(get.ND.digitalAndImageLogs, pdfFolder = "myNDDownloads/OriginalStudyLASFiles" ))}
get.ND.digitalAndImageLogs2 <- function(FileNo, 
                                       ndUserName = "iddgroupllc",
                                       ndPassword = 'Snow2Day',
                                       pdfFolder  = "myNDDownloads") {
  ## build webscraper for ND oil and gas by file no
  library(rvest)
  library(tidyr)
  require(httr)
  require(XML)
  library(xml2)
  cat('========= ',FileNo,' ====================================\n')
  ## https://www.dmr.nd.gov/oilgas/feeservices/getwellprod.asp?filenumber=29727
  getWebData <- httr::GET(paste0('https://www.dmr.nd.gov/oilgas/feeservices/getscoutticket.asp?filenumber=', FileNo),
                          httr::authenticate(ndUserName, ndPassword, type = "basic"),
                          httr::add_headers("Content-Type" = "application/json"))
  
  content <- rawToChar(getWebData$content)
  
  ## us rvest and the xml2 package to read and find all of the href tags
  # read_hrefs <- rvest::html_attr(x = rvest::html_nodes(x = xml2::read_html(content), css = "a"), name = "href")
  # 
  # ## keep only the tags referenceing digital and image logs "/oilgas/feeservices/dlogs/29/29726/29726-DTSM1.tif"
  # digitalImageLogList <- read_hrefs[grep('dlogs', read_hrefs)]
  # 
  # ## keep only dtsm.las files
  # digitalImageLogList <- digitalImageLogList[grep('DTSM', digitalImageLogList)]
  # digitalImageLogList <- digitalImageLogList[grep('.las', digitalImageLogList)]
  
  ## basename turns "/oilgas/feeservices/dlogs/29/29726/29726-DTSM1a.las" into "29726-DTSM1b.las"
  ## so will gsub('.*/','',digitalImageLogList)
  ## check if directory exists if it doesn't create it
  if (!dir.exists(file.path("~/Downloads",pdfFolder))){
    ## create a new directory off of downloads
    
    dir.create(file.path("~/Downloads",pdfFolder))  
  }
  
  ## find the digital or image log line and scrape the files off 
  ## find out which of the DTSM*.las files is the largest and download only that one.
  ##  pattern = 'Digital or Image Log\\(s\\) available\\: ')[[1]][2], 
  ## end    = stringr::str_locate_all(string  = content, pattern = "B<br>")[[1]][1])  the end line will either be Casing String or Formation Tops, 
  
  # digitalDownloadList <- stringr::str_split(string  = stringr::str_sub(string = content, 
  #                                                                      start  = stringr::str_locate_all(string  = content, pattern = 'Digital or Image Log\\(s\\) available\\: ')[[1]][2], 
  #                                                                      end    = stringr::str_locate_all(string  = content, pattern = "<br>Casing String\\(s\\)\\:")[[1]][1] - 1), 
  #                                           pattern = ",")[[1]]
  
  #purrr::map_dfr(stringr::str_locate_all(string = content, pattern = "<br>Casing String\\(s\\)\\:"), tibble::as_tibble)
  
  
  digitalDownloadList <- stringr::str_split(string  = stringr::str_sub(string = content, 
                                                                       start  = stringr::str_locate_all(string  = content, pattern = 'Digital or Image Log\\(s\\) available\\: ')[[1]][2], 
                                                                       end    = stringr::str_locate_all(string  = content, pattern = "B<br>")[[1]][1]), 
                                            pattern = ",")[[1]]
  
  ## get the DTSM file list and get rid of the .tiff files
  digitalDownloadList <- digitalDownloadList[stringr::str_which(string = digitalDownloadList,pattern = "DTSM")]
  digitalDownloadList <- digitalDownloadList[stringr::str_which(string = digitalDownloadList,pattern = ".las")]
  
  #3 use purrr::map_dfr to map str_locate_all into a table of start and end then pick end for the start = and start for the end = 
  tbl.downloadFileList <- tibble::tibble(myFileList = stringr::str_sub(string = digitalDownloadList, 
                                                                       start  = purrr::map_dfr(stringr::str_locate_all(string  = digitalDownloadList, pattern = '<a href=\\/'), tibble::as_tibble)$end, 
                                                                       end    = purrr::map_dfr(stringr::str_locate_all(string  = digitalDownloadList, pattern = '><b><span title=\\\"Drill Time - Samples'), tibble::as_tibble)$start - 1),
                                         myFileSize = stringr::str_sub(digitalDownloadList, -5)) 
  # %>% 
  #   dplyr::mutate(size2 = dplyr::if_else(stringr::str_sub(myFileSize, -2) == "KB", as.numeric(stringr::str_sub(myFileSize, 1, 3))*1000, as.numeric(stringr::str_sub(myFileSize, 1, 3)) * 1000000)) %>% 
  #   dplyr::filter(size2 == max(size2))
  # 
  
 
  # getWebData <- httr::GET(paste0('https://www.dmr.nd.gov', tbl.downloadFileList$myFileList),
  #                         httr::authenticate(ndUserName, ndPassword, type = "basic"),
  #                         httr::write_disk(paste0('~/Downloads/', pdfFolder, '/W', basename(tbl.downloadFileList$myFileList)), overwrite = TRUE))
  # 
  lapply(tbl.downloadFileList$myFileList, function(x){
    cat('========= ', basename(x),'  ==\n')
    getWebData <- GET(paste0('https://www.dmr.nd.gov', x),
                      httr::authenticate(ndUserName, ndPassword, type = "basic"),
                      write_disk(paste0('~/Downloads/', pdfFolder, '/W', basename(x)), overwrite=TRUE))
  })
  
}


# get.ND.digitalAndImageLogs2(FileNo = 29777,pdfFolder = "myNDDownloads/OriginalStudyLASFiles")
## download all las files
# system.time(tbl.myStudyWellsMPZData$FileNo %>% purrr::walk(get.ND.digitalAndImageLogs2, pdfFolder = "myNDDownloads/OriginalStudyLASFiles" ))

## cleanup and remove TFT wells
## Get all Whiting wells data including original study
library(data.table)
tbl.whiting175 <- readRDS(file = '~/Documents/Business/IDDGroup/OMPADG/whiting175.rds')

tbl.myStudyWells <- tbl.whiting175 %>% 
  dplyr::filter(!OriginalWellName %like% c('TF'))

get.ND.digitalAndImageLogs2(FileNo = 31797,pdfFolder = "myNDDownloads/ECWhitingStudyLASFiles")
## download all las files
system.time(tbl.myStudyWells %>% dplyr::arrange(FileNo) %>% .$FileNo %>% purrr::walk(get.ND.digitalAndImageLogs2, pdfFolder = "myNDDownloads/ECWhitingStudyLASFiles" ))

tail(tbl.myStudyWells %>% dplyr::arrange(FileNo))
### Download ND LAS files END ------------




## read in LAS files BEGINNING --------------

## USE reticulate and python
library(reticulate)

use_python("/users/doucetteemail/opt/anaconda3")
reticulate::use_condaenv("/users/doucetteemail/opt/anaconda3")

## import lasio
lasio <- reticulate::import("lasio")
pandas <- reticulate::import("pandas")






## first find which las has the horizontal gamma data



## read in each file using python.lasio and combine into one big tibble

get.lasFileHeaderSummary <- function(lasFile){
  require(tidyverse)
  ## https://lasio.readthedocs.io/en/latest/basic-example.html 
  ## first read in the file using read() from the python read package
  ## 
  myLasFile <- lasio$read(lasFile)
  
  ## We just want Gamma curves 
  ## we'll use lasio$curves$keys() to get the curve names from the ~CURVE section of the LAS file
  fileKeys <- myLasFile$curves$keys() 
  ## Get STRT and STOP values
  startDepth <- myLasFile$header$Well$dictview()$STRT
  endDepth   <- myLasFile$header$Well$dictview()$STOP
  
  ## make a tiblle with the File.No using basename and str_sub  all files were saved with W29725 format
  tbl.headerDetails <- tibble::tibble(File.No     = stringr::str_sub(basename(lasFile), start = 2, end = 6),
                                      lasFileName = lasFile, 
                                      fileKeys    = fileKeys, 
                                      startDepth  = startDepth, 
                                      endDepth    = endDepth)
  
  cat(paste0("================== ", basename(lasFile), " =================\n"))
  return(tbl.headerDetails)
}

tbl.myStudyWells %>% dplyr::filter(studyName == 'Enhanced Completions')

## get the study files downloaded from get.ND.digitalANDImageLogs2

# myFileList      <- list.files(path = file.path('~/Downloads/myNDDownloads/OriginalStudyLASFiles'))
# 
# myFileListPath  <- paste0(file.path('/users/doucetteemail/Downloads/myNDDownloads/OriginalStudyLASFiles'), '/', myFileList)
myFileList      <- list.files(path = file.path('~/Downloads/myNDDownloads/ECWhitingStudyLASFiles'))

myFileListPath  <- paste0(file.path('/users/doucetteemail/Downloads/myNDDownloads/ECWhitingStudyLASFiles'), '/', myFileList)

tbl.lasFileList <- tibble::tibble(myFileList = myFileList, myFileListPath = myFileListPath)

## test it
# get.lasFileHeaderSummary(lasFile = tbl.lasFileList$myFileListPath[1])


system.time(tbl.fileHeaderSummary <- tbl.lasFileList %>% .$myFileListPath %>% purrr::map_dfr(get.lasFileHeaderSummary))

require(data.table)
## do we have gamma data?  check for %like% for "GAMMA" or "GR" maybe "GAMA"?
## then check for max(endDepth) the assumption here is the one with the greatest endDepth will be the horizontal.
tbl.fileHeaderSummary <- tbl.fileHeaderSummary %>% 
  dplyr::filter(fileKeys %like% c("GAMMA") | fileKeys %like% c("GR")) %>% 
  dplyr::group_by(File.No) %>% 
  dplyr::mutate(maxDepth = max(endDepth)) %>% 
  dplyr::filter(endDepth == maxDepth)


## are we missing any las files with GAMMA?
tbl.myStudyWells$FileNo %in% tbl.fileHeaderSummary$File.No

## Now we'll use this file list as the master list for GAMMA in the horizontals
# lasFile <- tbl.fileHeaderSummary$lasFileName[1]
get.lasFileTable <- function(lasFile){
  require(tidyverse)
  
  ## first read in the file using read() from the python read package
  ## 
  myLasFile <- lasio$read(lasFile)
  df        <- lasio$LASFile$df(myLasFile)
  
  ## use as_tibble with rownames = "Depth.ft" for panda index of MD from panda data_frame
  tbl.lasData         <- tibble::as_tibble(df, rownames = "Depth.ft") %>% dplyr::mutate(Depth.ft = as.numeric(Depth.ft))
  tbl.lasData$File.No <- as.integer(stringr::str_sub(basename(lasFile), start = 2, end = 6))
  
  cat('read in las for File.No: ',tbl.lasData$File.No[1],'\n')
  
  
  return(tbl.lasData)
  
  
}
get.lasFileTable(lasFile = tbl.fileHeaderSummary$lasFileName[1])
 
ECWhitingLasData <- tbl.fileHeaderSummary %>% .$lasFileName %>%  purrr::map_dfr(get.lasFileTable)

ECWhitingLasData %>% dplyr::filter(is.na(GAMMA)) %>% .$File.No %>% unique()


# saveRDS(tbl.OriginalWhitingLasData, file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/tbl.OriginalWhitingLasData.rds')
# tbl.OriginalWhitingLasData <- readRDS( file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/tbl.OriginalWhitingLasData.rds')

saveRDS(ECWhitingLasData, file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/PCdataEC/ECWhitingLasData.rds')
## read in las data
ECWhitingLasData <- readRDS( file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/PCdataEC/ECWhitingLasData.rds')


## read in LAS files END --------------




#' get.lasFileTable
#' 
#' This reads in an LAS vs 2.0 file using stringr::read_table
#' Curve names are taken from the ~Curve header. Unfortunately,
#' most of the column headers are not standard, fortunately, most LAS
#' files have Depth.ft and Gamma.API
#' 
#' @import tidyverse
#' @param lasFile
#'
#' @return tbl.lasData
#' @export
#'
#' @examples
#' \dontrun{
#' myFileList <- list.files(path = file.path('~/Downloads/myNDDownloads/OriginalStudyLASFiles'))
#' myFileList <- paste0(file.path('~/Downloads/myNDDownloads/OriginalStudyLASFiles'), '/', myFileList)
#' get.lasFileTable(lasFile = lasFile)
#' tbl.OriginalWhitingLasData <- myFileList %>% purrr::map_dfr(get.lasFileTable)
#' saveRDS(tbl.OriginalWhitingLasData, file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/tbl.OriginalWhitingLasData.rds')
#' tbl.OriginalWhitingLasData <- readRDS( file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/tbl.OriginalWhitingLasData.rds')
#' }
get.lasFileTable <- function(lasFile){
  ## use readLines to read in the text file
  txt.lasFile <- readLines(lasFile)
  # head(txt.lasFile)
  # tail(txt.lasFile)
  
  ## below this line is the data table
  dataline  <- as.numeric(grep("~A",txt.lasFile) + 1)
  ## the curve data starts one line after ~CURVE
  curveLine <- as.numeric(grep("~CURVE", txt.lasFile))
  
  ## get curve names
  ## 
  ## get rid of extra spaces
  ## Turns this: "Depth     .ft                                        : Depth channel"
  ## Into This: "Depth .ft : Depth channel"
  ## replace " \\." with "\\."  remember . is escaped 
  ##  "Depth.ft : Depth channel"
  ## Then keep everyting before " :'
  ## and drop any Hashtag # elements
  txt.curveNames <- gsub('(*.)(\\s:.*)','\\1',gsub(' \\.','\\.',gsub("\\s+"," ",txt.lasFile[(curveLine + 1):(dataline - 3)])))
  ## replace / with _ 
  txt.curveNames <- stringr::str_replace_all(txt.curveNames, '/', '_')
  ## start of data is ~ASCII
  dataStart <- stringr::str_which(txt.lasFile,'~ASCII')
  
  
  ## use readr::read_table to read in the columns of data wihtout column names
  
  tbl.lasData           <- readr::read_table(lasFile, col_names = FALSE, skip = dataStart + 1)
  ## add column names
  colnames(tbl.lasData) <- txt.curveNames
  ## assign the file number from the file name utilize basename() to get the filename then grab the 5 digit number 
  ## this is an ND only
  tbl.lasData$File.No <- as.integer(gsub('W(\\d{5})(-DTS.*)','\\1', basename(lasFile)))
  
  ## get api number from LAS header which will either be in LIC or API or both or neither
  # str_extract(txt.lasFile[ stringr::str_which(txt.lasFile, '^API')], "[:digit:]")
  # str_extract(txt.lasFile[ stringr::str_which(txt.lasFile, '^LIC')], "[:digit:]")
  # 
  
  cat('read in las for File.No: ',tbl.lasData$File.No[1],'\n')
  
  return(tbl.lasData)
}

get.lasFileTable(lasFile = lasFile)

tbl.OriginalWhitingLasData <- myFileList %>% purrr::map_dfr(get.lasFileTable)

saveRDS(tbl.OriginalWhitingLasData, file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/tbl.OriginalWhitingLasData.rds')

tbl.OriginalWhitingLasData <- readRDS( file = '~/Documents/Business/IDDGroup/OMPADG/Development/MPZAnalyticsPlaygrounds/rdsFiles/tbl.OriginalWhitingLasData.rds')


## read in LAS files END --------------

paste_from_clipboard <-  function(sep="\t",header=T,stringsAsFactors = FALSE,...) {       
  read.table(pipe("pbpaste")
             ,sep=sep
             ,header=header
             ,stringsAsFactors = FALSE,...) 
}

## paste in numbers manually entered data
tbl.XTOfromNumbers <- tbl_df(paste_from_clipboard())

## convert dateStimulated to date
## create lateralLength and avgStageLength
tbl.XTOfromNumbers <- tbl.XTOfromNumbers %>% mutate(dateStimulated = as.Date(dateStimulated),
                                                    lateralLength  = bottomFT - topFT)
tbl.XTOfromNumbers <- tbl.XTOfromNumbers %>% mutate(avgStageLength = lateralLength / stages)

## save XTOCompletionData -----------------------------------
## save completionData part 1 to rds file
saveRDS(tbl.XTOfromNumbers, file = '~/Documents/Business/IDDGroup/OMPADG/tbl.XTOfromNumbers.rds')
glimpse(tbl.XTOfromNumbers)





### look for unknowns in scraped data ---------------------------

tbl.ndTargetFormationByWell <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.ndTargetFormationByWell.rds')

tbl.ndTargetFormationByWell %>% dplyr::group_by(targetFormation) %>% dplyr::summarise(count = n())

tbl.nd500ftSpacingLanesFilled <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.nd500ftSpacingLanesFilled.rds' )


tbl.nd500ftSpacingLanesFilled %>% dplyr::group_by(tgtF) %>% dplyr::summarise(count = n())

tbl.DSUOperator               <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUOperator.rds')

tbl.DSUSectionNames           <- readRDS(file = '~/Downloads/MPZAnalyticsRrdsFiles/tbl.DSUSectionNames.rds')


UnkownTargetFormations <- dplyr::left_join(tbl.nd500ftSpacingLanesFilled %>% dplyr::filter(tgtF == 'NoData') %>% dplyr::select(FileNo, tgtF, DSUId, MeasDpth, TVD) %>% dplyr::arrange(desc(MeasDpth)), tbl.DSUSectionNames, by = c("DSUId" = "DSUIndex")) %>% dplyr::mutate(sectionRatio = dssizeCount/sectionCount) %>% dplyr::filter(sectionRatio >= 1) %>% 
  dplyr::filter(dssize <= 1920)

## copy to Numbers for manual data entry --------------------------
write.table(UnkownTargetFormations, file=pipe("pbcopy"), quote = FALSE, sep="\t", row.names = FALSE)


### Find The Highland 4-9H --------------------
require(data.table)
tbl.wellIndex %>% dplyr::filter(LeaseName %like% 'HIGHLAND')

# A tibble: 7 x 35
# APINo FileNo CurrentOperator CurrentWellName LeaseName LeaseNumber OriginalOperator OriginalWellName SpudDate               TD CountyName Township Range Section QQ    Footages FieldName ProducedPools OilWaterGasCums
# <dbl>  <int> <chr>           <chr>           <chr>     <chr>       <chr>            <chr>            <dttm>              <int> <chr>      <chr>    <chr>   <int> <chr> <chr>    <chr>     <chr>         <chr>          
#   1 3.31e13   8905 PATRICK PETROL… HIGHLAND FEDER… HIGHLAND… 1-25        PATRICK PETROLE… HIGHLAND FEDERA… 1981-10-01 00:00:00 13470 MCKENZIE   149 N    103 W      25 SWSW  1118 FS… WILDCAT   ""            ""             
# 2 3.31e13  18377 SINCLAIR OIL &… HIGHLAND  1-9H  HIGHLAND  1-9H        SINCLAIR OIL & … HIGHLAND  1-9H   2009-11-14 00:00:00 15650 MOUNTRAIL  153 N    93 W        9 NENW  250 FNL… SANISH    "BAKKEN"      "147609|72642|…
# 3 3.31e13  21087 SINCLAIR OIL &… HIGHLAND  2-9H  HIGHLAND  2-9H        SINCLAIR OIL & … HIGHLAND  2-9H   2011-09-01 00:00:00 15300 MOUNTRAIL  153 N    93 W        9 NWNW  250 FNL… SANISH    "BAKKEN"      "227341|104097…
# 4 3.31e13  28921 SINCLAIR OIL &… HIGHLAND  4-9H  HIGHLAND  4-9H        SINCLAIR OIL & … HIGHLAND  4-9TFH 2015-04-20 00:00:00 15780 MOUNTRAIL  153 N    93 W        9 NWNW  190 FNL… SANISH    "BAKKEN"      "226732|129955…
# 5 3.31e13  30081 SINCLAIR OIL &… HIGHLAND 3-9TFH HIGHLAND  3-9TFH      SINCLAIR OIL & … HIGHLAND  3-9TFH 2015-05-11 00:00:00 15520 MOUNTRAIL  153 N    93 W        9 NWNW  190 FNL… SANISH    "BAKKEN"      "152145|120411…
# 6 3.31e13  30087 SINCLAIR OIL &… HIGHLAND  6-9T… HIGHLAND  6-9TFH      SINCLAIR OIL & … HIGHLAND  6-9TFH 2019-05-04 00:00:00 15420 MOUNTRAIL  153 N    93 W        9 NENE  380 FNL… SANISH    "BAKKEN"      "55142|47738|7…
# 7 3.31e13  30088 SINCLAIR OIL &… HIGHLAND  5-9H  HIGHLAND  5-9H        SINCLAIR OIL & … HIGHLAND  5-9H   2015-06-01 00:00:00 15315 MOUNTRAIL  153 N    93 W        9 NENE  325 FNL… SANISH    "BAKKEN"      "162803|99315|…
# … with 16 more variables: IPTDateOilWaterGas <chr>, Wellbore <chr>, Latitude <dbl>, Longitude <dbl>, WellType <chr>, WellStatus <chr>, CTB <int>, WellStatusDate <dttm>, IpDate <dttm>, IpOil <int>, IpWater <int>, IpGas <int>,
#   cumOil <int>, cumWater <int>, cumGas <int>, cumDate <chr>










