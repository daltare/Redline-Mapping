This draft tool (available at: https://daltare.shinyapps.io/Redline-Mapping/) displays California's Redlined communites, and helps to assess potential correlations between those policies and indicators of enviornmental and public health (e.g., 303d impaired water bodies, CalEnviroScreen scores), as well as facilities regulated by the CalEPA. More layers will be added in the future (current data sources are described in the script file). 

NOTE: the raw datasets used to develop inputs to this application are in the `data_raw.7z` folder, and the Prepare-Data.R script shows how to access and transform the data to prepare it for use in this application

For more information on the history around Redline mapping see:
- [Mapping Inequality: Redlining in New Deal America (University of Richmond)](https://dsl.richmond.edu/panorama/redlining/#text=intro)
- [How Redlining’s Racist Effects Lasted for Decades (New York Times)](https://www.nytimes.com/2017/08/24/upshot/how-redlinings-racist-effects-lasted-for-decades.html)
- [The Effects of the 1930s HOLC “Redlining” Maps (Federal Reserve Bank of Chicago)](https://www.chicagofed.org/publications/working-papers/2017/wp2017-12)

### Data Sources
- [Mapping Inequality: Redlining in New Deal America (University of Richmond)](http://dsl.richmond.edu/panorama/redlining/#text=downloads)
- [CalEPA Regulated Site Portal](https://siteportal.calepa.ca.gov/nsite/map)
- [California EPA Geoserver](https://services.calepa.ca.gov/geoserver/)
- [CalEnviroScreen 3.0](https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30)
- [2014/2016 Clean Water Act Section 303(d) Listed Waterbodies](https://www.waterboards.ca.gov/water_issues/programs/tmdl/integrated2014_2016.shtml)
- [California Drinking Water Service Areas](https://gispublic.waterboards.ca.gov/portal/home/item.html?id=bb21fcee16ea4af2a8d57aa39447aa9c)