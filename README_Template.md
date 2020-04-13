# WhiteSalmon

## Summary

This repository looks at hydrologic trends on the White Salmon River in White Salmon, Washington in order to appraise the impacts of dam removal on flow regimes. On October 26, 2011, Condit Dam was removed from a lower stretch of the river, approximately xx miles from the river's confluence with the Columbia River. The removal of this dam should have removed a barrier to salmon migration and returned natural flows to the river. Here, I investigate peak and low flow conditions pre and post dam removal and seasonal trends. The 100 year flood return interval and 7Q10 rate is calculated based on historic data and then modeled based on post dam removal conditions. Seasonal Mann Kendalls were run on pre and post dam removal monthly discharge values. A need for more years of data post dam removal is needed to more accurately model the post dam removal conditions. 

## Investigators

Kristine Swann, Duke Univeristy Nicholas School of the Environment, kristine.swann@duke.edu

## Keywords

White Salmon River, Condit Dam, Dam Removal, Salmon, Whitewater, Kayaking, Hydro, Washington, PacifiCorp

## Database Information
USGS stream gage data for Station #14123500 (also known as the Underwood gage) was accessed via dataRetrieval package for dates through the 2019 water year. 

Map data includes:
- Stream gage station location, sourced from USGS;
- Stream layer, sourced from National Hydrography Dataset (also USGS);

## Folder structure, file formats, and naming conventions 

Folder structure:
There are 5 main folders: Script, Data, GIS, Plots, Misc. Of these, only GIS has subfolders based on the raw and processed spatial data. Raw GIS data is housed in folders named after the sources of the data. Processed GIS data is simply in a 'processed' folder. 

File formats: 
Script: rmd files 
Data: csv files
GIS: shapefiles and/or geodatabases
Plots: pdfs and/or jpegs
Misc: jpegs, etc (ex: photos of Condit Dam). 

File name convention: 
Script: rmd files are named 'WhiteSalmon_[analysis type]'
Data: csv file of gage station data is titled 'WhiteSalmon'
GIS: raw files from external sources retain the names from the sources; processed spatial data have explicit names, for example 'WhiteSalmonBasin' is a shapefile of the White Salmon River basin created using USGS StreamStats, or 'WhiteSalmonRiver' is a shapefile of the river pulled from the National Hydrography Dataset. 
Plots: files are named after script where they originated followed by title of the plot (i.e.: WhiteSalmon_[analysis type]_[title]); some titles may be abbreviated. 
Misc: photos are named after sources

## Metadata

There are many permutations of the stream gage data that occur through scripts. The raw stream gage data that is the basis for each script file have the following columns:
agency_cd: who created the data (USGS)
site_no: this is the station id (14123500)
Date: %Y-%m-%d
Flow: discharge rate in cubic feet per second 
Flow_cd: QA/QC code for discharge data created by USGS
Year: Year pulled from Date
Month: Month pulled from Date
WaterYear: Based on a year of October - September (more detail can be found in WhiteSalmon_writeup.rmd)

## Scripts and code

Files to look through include:

WhiteSalmon_Data_Exploration.rmd
- Shows raw data timeseries broken down by data confidence codes (A, Ae - described below) and monthly discharge averages broken down by time periods (1941-2019, 2004-2011, 2012-2019)

WhiteSalmon_Flood.rmd
- Calculates the 100 year flood return interval based on the 1940 - present data and then models the expected probabilities based on conditions specific to pre (2004-2011) and post (2012-2019) dam removal. 

WhiteSalmon_7Q10.rmd
-Similarly calculates the 7Q10 interval based on the 1940 - present data and then calls out occurrences. 

WhiteSalmon_timeseries.rmd
- Looks through the data to statistically answer: is there a significant difference in pre and post dam removal conditions? And, are seasonal monotonic trends in discharge rates more significant after dam removal?

WhiteSalmon_writeup.rmd 
-Pulls everything above together into a nicely explained pacakge. 

## Quality assurance/quality control
Data was originally screened from 1917 to present day via a scatterplot. Because there was missing data in the 1930s, historic data considered here began in 1940. There were no NAs in the data post 1940. USGS implements its own QA/QC procedures, which are summarized in the column Flow_cd. 'A' is approved data. 'Ae' is estimated approved data. Data with the 'Ae' designation occurred around the time of the dam removal and in 2016, with shorter incidences occurring from 2006-2017. These were left in the dataset because this station also has gage height data to correlate discharge to, making the 'Ae' estimates fairly reliable, especially considering the 100 years of stream flow data. 

