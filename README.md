## --- NOTE: NEW REPOSITORY ---

This repository is no longer in active development, and has been
superseded by the updated version available here:
<https://github.com/daltare/pollution-and-prejudice>. That new
repository has been updated to include data from CalEnviroScreen 4.0,
and has been reorganized with a new directory structure and tools /
processes to improve reproducibility. Please use that new repository
going forward.

------------------------------------------------------------------------

This draft tool (available at:
<https://cawaterdatadive.shinyapps.io/Redline-Mapping/>) displays
California's Redlined communites, and helps to assess potential
correlations between those policies and indicators of enviornmental and
public health (e.g., 303d impaired water bodies, CalEnviroScreen
scores), as well as facilities regulated by the CalEPA. More layers will
be added in the future (current data sources are described in the script
file).

NOTE: the `Process-Data.R` script shows how to access and transform some
of the data to prepare it for use in this application

For more information on the history around Redline mapping and other
studies of its effects, see: - [Mapping Inequality: Redlining in New
Deal America (University of
Richmond)](https://dsl.richmond.edu/panorama/redlining/#text=intro) -
[Interactive Redlining Map Zooms In On America's History Of
Discrimination
(NPR)](https://www.npr.org/sections/thetwo-way/2016/10/19/498536077/interactive-redlining-map-zooms-in-on-americas-history-of-discrimination) -
[How Redlining’s Racist Effects Lasted for Decades (New York
Times)](https://www.nytimes.com/2017/08/24/upshot/how-redlinings-racist-effects-lasted-for-decades.html) -
[The Effects of the 1930s HOLC “Redlining” Maps (Federal Reserve Bank of
Chicago)](https://www.chicagofed.org/publications/working-papers/2017/wp2017-12) -
[Redlining was banned 50 years ago. It’s still hurting minorities today.
(Washington
Post)](https://www.washingtonpost.com/news/wonk/wp/2018/03/28/redlining-was-banned-50-years-ago-its-still-hurting-minorities-today/) -
[Racist Housing Practices From The 1930s Linked To Hotter Neighborhoods
Today
(NPR)](https://www.npr.org/2020/01/14/795961381/racist-housing-practices-from-the-1930s-linked-to-hotter-neighborhoods-today)

### Data Sources

-   [Redline Maps (University of
    Richmond)](http://dsl.richmond.edu/panorama/redlining/#text=downloads)
-   [CalEnviroScreen 3.0 (California Office of Environmental Health
    Hazard
    Assessment)](https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30)
    \| Web-service available
    [here](https://services.calepa.ca.gov/geoserver/) (click on the
    "Layer Preview" link, layer name: "calepa:CES3June2018Update")
-   [2014/2016 Clean Water Act Section 303(d) Listed Waterbodies
    (California State Water Resources Control
    Board)](https://www.waterboards.ca.gov/water_issues/programs/tmdl/integrated2014_2016.shtml)
-   [CalEPA Regulated Sites (California EPA
    Geoserver)](https://services.calepa.ca.gov/geoserver/) \|
    Web-service (click on the "Layer Preview" link, layer names:
    "calepa:mv_fac_from_ciwqs", "calepa:mv_fac_from_smarts",
    "calepa:mv_fac_from_geotracker") \| Also available from the (CalEPA
    Regulated Site
    Portal)](<https://siteportal.calepa.ca.gov/nsite/map/export>)
-   [CalEPA Regulated Sites, Site Program Types, Inspections,
    Violations, and Enforcement Actions (CalEPA Regulated Site
    Portal)](https://siteportal.calepa.ca.gov/nsite/map/export)
-   [California Drinking Water Provider Service Areas (California State
    Water Resources Control Board Map
    Services)](https://gispublic.waterboards.ca.gov/portal/home/item.html?id=fbba842bf134497c9d611ad506ec48cc#overview)
    \| For web-service, use the "Service URL" link \| Also available
    from the [California State
    Geoportal](https://gis.data.ca.gov/datasets/waterboards::california-drinking-water-system-area-boundaries)
-   [California Water Board Regional Office Boundaries (California State
    Water Resources Control Board Map
    Services)](http://gispublic.waterboards.ca.gov/arcgis/rest/services/Administrative/RB_OfficeAreas/MapServer/0)
    \| Also available from the [California State
    Geoportal](https://gis.data.ca.gov/datasets/waterboards::regional-board-boundaries)

### Slides

(NOTE: press the "f" key to view in full screen mode, press "?"" to see
other viewing options)

Slides from March 18, 2021 presentation available at:
<https://daltare.github.io/Redline-Mapping/2021_03_presentation/slides_2021-03-18.html>

Slides from November 17, 2021 presentation available at:
<https://daltare.github.io/Redline-Mapping/2021_11_presentation/slides_2021-11-17.html>
