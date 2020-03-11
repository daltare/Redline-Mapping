This draft tool (available at: https://cawaterdatadive.shinyapps.io/Redline-Mapping/) displays California's Redlined communites, and helps to assess potential correlations between those policies and indicators of enviornmental and public health (e.g., 303d impaired water bodies, CalEnviroScreen scores), as well as facilities regulated by the CalEPA. More layers will be added in the future (current data sources are described in the script file). 

NOTE: the raw datasets used to develop inputs to this application are in the `data_raw.7z` folder, and the `Prepare-Data.R` script shows how to access and transform the data to prepare it for use in this application

For more information on the history around Redline mapping and other studies of its effects, see:
- [Mapping Inequality: Redlining in New Deal America (University of Richmond)](https://dsl.richmond.edu/panorama/redlining/#text=intro)
- [How Redlining’s Racist Effects Lasted for Decades (New York Times)](https://www.nytimes.com/2017/08/24/upshot/how-redlinings-racist-effects-lasted-for-decades.html)
- [The Effects of the 1930s HOLC “Redlining” Maps (Federal Reserve Bank of Chicago)](https://www.chicagofed.org/publications/working-papers/2017/wp2017-12)
- [Redlining was banned 50 years ago. It’s still hurting minorities today. (Washington Post)](https://www.washingtonpost.com/news/wonk/wp/2018/03/28/redlining-was-banned-50-years-ago-its-still-hurting-minorities-today/)
- [Racist Housing Practices From The 1930s Linked To Hotter Neighborhoods Today (NPR)](https://www.npr.org/2020/01/14/795961381/racist-housing-practices-from-the-1930s-linked-to-hotter-neighborhoods-today)

### Data Sources
- [Redline Maps (University of Richmond)](http://dsl.richmond.edu/panorama/redlining/#text=downloads)
- [CalEnviroScreen 3.0 (California Office of Environmental Health Hazard Assessment)](https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30) | Web-service available [here](https://services.calepa.ca.gov/geoserver/) (click on the "Layer Preview" link, layer name: "calepa:CES3June2018Update")
- [2014/2016 Clean Water Act Section 303(d) Listed Waterbodies (California State Water Resources Control Board)](https://www.waterboards.ca.gov/water_issues/programs/tmdl/integrated2014_2016.shtml)
- [CalEPA Regulated Sites (California EPA Geoserver)](https://services.calepa.ca.gov/geoserver/) | Web-service (click on the "Layer Preview" link, layer names: "calepa:mv_fac_from_ciwqs", "calepa:mv_fac_from_smarts", "calepa:mv_fac_from_geotracker")
- [CalEPA Regulated Sites Violations and Enforcement Actions (CalEPA Regulated Site Portal)](https://siteportal.calepa.ca.gov/nsite/map/export)
- [California Drinking Water Provider Service Areas (California State Water Resources Control Board Map Services)](https://gispublic.waterboards.ca.gov/portal/home/item.html?id=bb21fcee16ea4af2a8d57aa39447aa9c) | For web-service, use the "Service URL" link
- [California Water Board Regional Office Boundaries (California State Water Resources Control Board Map Services)](http://gispublic.waterboards.ca.gov/arcgis/rest/services/Administrative/RB_OfficeAreas/MapServer/0)
