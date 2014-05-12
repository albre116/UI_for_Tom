###########################################################################
###Data Tables Examples
###########################################################################
require(rCharts)

dt <- dTable(
  iris,
  sPaginationType= "full_numbers"
)
dt

dt <- dTable(
  iris,
  bScrollInfinite = T,
  bScrollCollapse = T,
  sScrollY = "200px",
  width = "500px"
)
dt


#not really a use case but a test to check for errors
data(Orange)
dt <- dTable(
  Orange,
  sScrollY = "200px",
  bScrollCollapse = T,
  bPaginate = F,
  bJQueryUI = T,
  aoColumnDefs = list(
    sWidth = "5%", aTargets = list(-1)
  )
)
dt

########################################################################
#########Dimple Plotting Library #####
########################################################################
#get data used by dimple for all of its examples as a first test
data <- read.delim(
  "http://pmsi-alignalytics.github.io/dimple/data/example_data.tsv"
)

#eliminate . to avoid confusion in javascript
colnames(data) <- gsub("[.]","",colnames(data))

#example 1 vt bar
d1 <- dPlot(
  x ="Month" ,
  y = "UnitSales",
  data = data,
  type = "bar"
)
d1$xAxis(orderRule = "Date")
d1

#example 2 vt stacked bar
d1 <- dPlot(
  x ="Month" ,
  y = "UnitSales",
  groups = "Channel",
  data = data,
  type = "bar"
)
d1$xAxis(orderRule = "Date")
d1$legend(
  x = 60,
  y = 10,
  width = 700,
  height = 20,
  horizontalAlign = "right"
)
d1

#example 3 vertical 100% bar
#use from above and just change y axis type
d1$yAxis(type = "addPctAxis")
d1

#example 4 vertical grouped bar
d1 <- dPlot(
  x = c("PriceTier","Channel"),
  y = "UnitSales",
  groups = "Channel",
  data = data,
  type = "bar"
)
d1$legend(
  x = 60,
  y = 10,
  width = 700,
  height = 20,
  horizontalAlign = "right"
)
d1

#example 5 vertical stack grouped bar
d1 <- dPlot(
  x = c("PriceTier","Channel"),
  y = "UnitSales",
  groups = "Owner",
  data = data,
  type = "bar"
)
d1$legend(
  x = 200,
  y = 10,
  width = 400,
  height = 20,
  horizontalAlign = "right"
)
d1


#example 6 vertical 100% Grouped Bar
#just change y Axis
d1$yAxis(type = "addPctAxis")
d1

#example 7 horizontal bar
d1 <- dPlot(
  Month ~ UnitSales,
  data = data,
  type = "bar"
)
d1$xAxis(type = "addMeasureAxis")
#good test of orderRule on y instead of x
d1$yAxis(type = "addCategoryAxis", orderRule = "Date")
d1


#example 8 horizontal stacked bar
d1 <- dPlot(
  Month ~ UnitSales,
  groups = "Channel",
  data = data,
  type = "bar"
)
d1$xAxis(type = "addMeasureAxis")
#good test of orderRule on y instead of x
d1$yAxis(type = "addCategoryAxis", orderRule = "Date")
d1$legend(
  x = 200,
  y = 10,
  width = 400,
  height = 20,
  horizontalAlign = "right"
)
d1

#example 9 horizontal 100% bar
d1$xAxis(type = "addPctAxis")
d1


#example 10 horizontal stacked bar
d1 <- dPlot(
  x = "UnitSales",
  y = c("PriceTier","Channel"),
  groups = "Channel",
  data = data,
  type = "bar"
)
d1$xAxis(type = "addMeasureAxis")
#good test of orderRule on y instead of x
d1$yAxis(type = "addCategoryAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 400,
  height = 20,
  horizontalAlign = "right"
)
d1


#example 11 horizontal stacked grouped bar
d1 <- dPlot(
  x = "UnitSales",
  y = c("PriceTier","Channel"),
  groups = "Owner",
  data = data,
  type = "bar"
)
d1$xAxis(type = "addMeasureAxis")
#good test of orderRule on y instead of x
d1$yAxis(type = "addCategoryAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 400,
  height = 20,
  horizontalAlign = "right"
)
d1

#example 12 horizontal 100% grouped bar
d1$xAxis(type = "addPctAxis")
d1


#example 13 vertical marimekko
d1 <- dPlot(
  UnitSales ~ Channel,
  groups = "Owner",
  data = data,
  type = "bar"
)
d1$xAxis(type = "addAxis", measure = "UnitSales", showPercent = TRUE)
d1$yAxis(type = "addPctAxis")
d1d1$legend(
  x = 200,
  y = 10,
  width = 400,
  height = 20,
  horizontalAlign = "right"
)
d1
#test with storyboard
d1$set(storyboard = "Date")
d1

#example 14 horizontal marimekko
d1 <- dPlot(
  Channel ~ UnitSales,
  groups = "Owner",
  data = data,
  type = "bar"
)
d1$yAxis(type = "addAxis", measure = "UnitSales", showPercent = TRUE)
d1$xAxis(type = "addPctAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 400,
  height = 20,
  horizontalAlign = "right"
)
d1


#example 15 block matrix
d1 <- dPlot(
  x = c("Channel","PriceTier"),
  y = "Owner",
  groups = "PriceTier",
  data = data,
  type = "bar"
)
d1$yAxis(type = "addCategoryAxis")
d1$xAxis(type = "addCategoryAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 400,
  height = 20,
  horizontalAlign = "right"
)
d1


#example 16 Scatter
d1 <- dPlot(
  OperatingProfit~UnitSales,
  groups = c("SKU","Channel"),
  data = subset(data, Date == "01/12/2012"),
  type = "bubble"
)
d1$xAxis( type = "addMeasureAxis" )
d1$yAxis( type = "addMeasureAxis" )
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


#example 17 Vertical Lollipop
d1 <- dPlot(
  UnitSales ~ Month,
  groups = "Channel",
  data = data,
  type = "bubble"
)
#defaults to yAxis (Measure) and xAxis (Category)
d1$xAxis( orderRule = "Date")
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


#example 18 Vertical Grouped Lollipop
d1 <- dPlot(
  y = "UnitSales",
  x = c("PriceTier","Channel"),
  groups = "Channel",
  data = data,
  type = "bubble"
)
#defaults to yAxis (Measure) and xAxis (Category)
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


#example 19 Horizontal Lollipop
d1 <- dPlot(
  x = "UnitSales",
  y = "Month",
  groups = "Channel",
  data = data,
  type = "bubble"
)
d1$xAxis( type = "addMeasureAxis" )
d1$yAxis( type = "addCategoryAxis", orderRule = "Date")
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


#example 20 Horizontal Grouped Lollipop
d1 <- dPlot(
  x = "UnitSales",
  y = c("PriceTier","Channel"),
  groups = "Channel",
  data = data,
  type = "bubble"
)
d1$xAxis( type = "addMeasureAxis" )
d1$yAxis( type = "addCategoryAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1



#example 21 Dot Matrix
d1 <- dPlot(
  y = "Owner",
  x = c("Channel","PriceTier"),
  groups = "PriceTier",
  data = data,
  type = "bubble"
)
d1$xAxis( type = "addCategoryAxis" )
d1$yAxis( type = "addCategoryAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


#example 22 Bubble
d1 <- dPlot(
  x = "UnitSalesMonthlyChange",
  y = "PriceMonthlyChange",
  z = "OperatingProfit",
  groups = c("SKU","Channel"),
  data = subset(data, Date == "01/12/2012"),
  type = "bubble"
)
d1$xAxis( type = "addMeasureAxis" )
d1$yAxis( type = "addMeasureAxis" )
d1$zAxis( type = "addMeasureAxis" )
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


#example 23 Vertical Bubble Lollipop
d1 <- dPlot(
  x = "Month",
  y = "UnitSales",
  z = "OperatingProfit",
  groups = "Channel",
  data = subset(
    data,
    Date %in% c(
      "01/07/2012",
      "01/08/2012",
      "01/09/2012",
      "01/10/2012",
      "01/11/2012",
      "01/12/2012"
    )
  ),
  type = "bubble"
)
d1$xAxis( type = "addCategoryAxis", orderRule = "Date" )
d1$yAxis( type = "addMeasureAxis" )
d1$zAxis( type = "addMeasureAxis" )
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


##example 24 Vertical Grouped Bubble Lollipop
d1 <- dPlot(
  x = c("PriceTier","Channel"),
  y = "UnitSales",
  z = "OperatingProfit",
  groups = "Channel",
  data = subset(
    data,
    Date %in% c(
      "01/07/2012",
      "01/08/2012",
      "01/09/2012",
      "01/10/2012",
      "01/11/2012",
      "01/12/2012"
    )
  ),
  type = "bubble"
)
d1$xAxis( type = "addCategoryAxis" )
d1$yAxis( type = "addMeasureAxis" )
d1$zAxis( type = "addMeasureAxis" )
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


#example 25 Horizontal Bubble Lollipop
d1 <- dPlot(
  y = "Month",
  x = "UnitSales",
  z = "OperatingProfit",
  groups = "Channel",
  data = subset(
    data,
    Date %in% c(
      "01/07/2012",
      "01/08/2012",
      "01/09/2012",
      "01/10/2012",
      "01/11/2012",
      "01/12/2012"
    )
  ),
  type = "bubble"
)
d1$yAxis( type = "addCategoryAxis", orderRule = "Date" )
d1$xAxis( type = "addMeasureAxis" )
d1$zAxis( type = "addMeasureAxis" )
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


##example 26 Horizontal Grouped Bubble Lollipop
d1 <- dPlot(
  y = c("PriceTier","Channel"),
  x = "UnitSales",
  z = "OperatingProfit",
  groups = "Channel",
  data = subset(
    data,
    Date %in% c(
      "01/07/2012",
      "01/08/2012",
      "01/09/2012",
      "01/10/2012",
      "01/11/2012",
      "01/12/2012"
    )
  ),
  type = "bubble"
)
d1$yAxis( type = "addCategoryAxis" )
d1$xAxis( type = "addMeasureAxis" )
d1$zAxis( type = "addMeasureAxis" )
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


#example 27 Bubble Matrix
d1 <- dPlot(
  x = c( "Channel", "PriceTier"),
  y = "Owner",
  z = "Distribution",
  groups = "PriceTier",
  data = data,
  type = "bubble",
  aggregate = "dimple.aggregateMethod.max"
)
d1$xAxis( type = "addCategoryAxis" )
d1$yAxis( type = "addCategoryAxis" )
d1$zAxis( type = "addMeasureAxis", overrideMax = 200 )
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


#example 28 Area
d1 <- dPlot(
  UnitSales ~ Month,
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area"
)
d1$xAxis(type = "addCategoryAxis", orderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1


#example 29 Stacked Area
d1 <- dPlot(
  UnitSales ~ Month,
  groups = "Channel",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area"
)
d1$xAxis(type = "addCategoryAxis", orderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


#example 30 100% Stacked Area
#just change type for y axis
d1$yAxis( type = "addPctAxis" )
d1



#example 31 Grouped Area
d1 <- dPlot(
  y = "UnitSales",
  x = c("Owner","Month"),
  groups = "Owner",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area"
)
d1$xAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1



#example 32 Grouped Stacked Area
d1 <- dPlot(
  y = "UnitSales",
  x = c("Owner","Month"),
  groups = "SKU",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area",
  bounds = list(x=70,y=30,height=340,width=330),
  barGap = 0.05,
  lineWeight = 1,
  height = 400,
  width = 590
)
d1$xAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1$legend(
  x = 430,
  y = 20,
  width = 100,
  height = 300,
  horizontalAlign = "left"
)
d1


#example 33 Grouped 100% Area
d1$yAxis( type = "addPctAxis" )
d1


#example 34 Vertical Area
d1 <- dPlot(
  x = "UnitSales",
  y = "Month",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area",
  bounds = list(x=80,y=30,height=480,width=330),
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", orderRule = "Date")
d1



#example 35 Vertical Stacked Area
d1 <- dPlot(
  x = "UnitSales",
  y = "Month",
  groups = "Channel",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area",
  bounds = list(x=80,y=30,height=480,width=330),
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1$legend(
  x = 60,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1


#example 36 Vertical 100% Stacked Area
d1$xAxis(type = "addPctAxis")
d1


#example 37 Vertical Grouped Area
d1 <- dPlot(
  x = "UnitSales",
  y = c("Owner","Month"),
  groups = "Owner",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area",
  bounds = list(x=90,y=30,height=470,width=330),
  lineWeight = 1,
  barGap = 0.05,
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1

#example 38 Vertical Grouped Stacked Area
d1 <- dPlot(
  x = "UnitSales",
  y = c("Owner","Month"),
  groups = "SKU",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "area",
  bounds = list(x=90,y=30,height=320,width=330),
  lineWeight = 1,
  barGap = 0.05,
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1$legend(
  x = 430,
  y = 20,
  width = 100,
  height = 300,
  horizontalAlign = "left"
)
d1



#example 39 Vertical Group 100% Area
d1$xAxis( type = "addPctAxis" )
d1





#example 40 Line
d1 <- dPlot(
  UnitSales ~ Month,
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line"
)
d1$xAxis(type = "addCategoryAxis", orderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1


#example 41 Multiple Line
d1 <- dPlot(
  UnitSales ~ Month,
  groups = "Channel",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line"
)
d1$xAxis(type = "addCategoryAxis", orderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1$legend(
  x = 200,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1

#example 42 Grouped Single Line
d1 <- dPlot(
  y = "UnitSales",
  x = c("Owner","Month"),
  groups = "Owner",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line",
  barGap = 0.05
)
d1$xAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1



#example 43 Grouped Multiple line
d1 <- dPlot(
  y = "UnitSales",
  x = c("Owner","Month"),
  groups = "Brand",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line",
  bounds = list(x=70,y=30,height=420,width=330),
  barGap = 0.05,
  height = 400,
  width = 590
)
d1$xAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1$yAxis(type = "addMeasureAxis")
d1$legend(
  x = 510,
  y = 20,
  width = 100,
  height = 300,
  horizontalAlign = "left"
)
d1



#example 44 Vertical Line
d1 <- dPlot(
  x = "UnitSales",
  y = "Month",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line",
  bounds = list(x=80,y=30,height=480,width=330),
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", orderRule = "Date")
d1



#example 45 Vertical Multiple Line
d1 <- dPlot(
  x = "UnitSales",
  y = "Month",
  groups = "Channel",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line",
  bounds = list(x=80,y=30,height=480,width=330),
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", orderRule = "Date")
d1$legend(
  x = 60,
  y = 10,
  width = 500,
  height = 20,
  horizontalAlign = "right"
)
d1



#example 46 Vertical Grouped Line
d1 <- dPlot(
  x = "UnitSales",
  y = c("Owner","Month"),
  groups = "Owner",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line",
  bounds = list(x=90,y=30,height=470,width=330),
  barGap = 0.05,
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1



#example 47 Vertical Grouped Multi Line
d1 <- dPlot(
  x = "UnitSales",
  y = c("Owner","Month"),
  groups = "Brand",
  data = subset(data, Owner %in% c("Aperture","Black Mesa")),
  type = "line",
  bounds = list(x=90,y=30,height=320,width=330),
  barGap = 0.05,
  height = 400,
  width = 590
)
d1$xAxis(type = "addMeasureAxis")
d1$yAxis(type = "addCategoryAxis", grouporderRule = "Date")
d1$legend(
  x = 430,
  y = 20,
  width = 100,
  height = 300,
  horizontalAlign = "left"
)
d1


#example 48 timeAxis
data( economics, package = "ggplot2" )
economics$date = format(economics$date, "%Y-%m-%d")
d1 <- dPlot(
x = "date",
y = "uempmed",
data = economics,
type = "line",
height = 400,
width = 700,
bounds = list(x=50,y=20,width=650,height=300)
)
d1$xAxis(
type = "addTimeAxis",
inputFormat = "%Y-%m-%d",
outputFormat = "%b %Y"
)
d1

#########################################################################
#####Highcharts
#########################################################################
library(rCharts)

# Example 1
hPlot(Pulse ~ Height, data = MASS::survey, type = "scatter", group = "Exer")

## Example 2
a <- hPlot(Pulse ~ Height, data = MASS::survey, type = "bubble", title = "Zoom demo", subtitle = "bubble chart", size = "Age", group = "Exer")
a$chart(zoomType = "xy")
a$exporting(enabled = T)
a

## Example 4
x <- data.frame(key = c("a", "b", "c"), value = c(1, 2, 3))
hPlot(x = "key", y = "value", data = x, type = "pie")

## Example 5
a <- hPlot(Pulse ~ Height, data = MASS::survey, type = 'scatter', group = 'Sex', radius = 6, group.na = "Not Available")
a$colors('rgba(223, 83, 83, .5)', 'rgba(119, 152, 191, .5)', 'rgba(60, 179, 113, .5)')
a$legend(align = 'right', verticalAlign = 'top', layout = 'vertical')
a$plotOptions(scatter = list(marker = list(symbol = 'circle')))
a$tooltip(formatter = "#! function() { return this.x + ', ' + this.y; } !#")
a

## Example 6
hPlot(freq ~ Exer, data = plyr::count(MASS::survey, c('Sex', 'Exer')), type = c('column', 'line'), group = 'Sex', radius = 6)

## Example 7
hPlot(freq ~ Exer, data = plyr::count(MASS::survey, c('Sex', 'Exer')), type = 'bar', group = 'Sex', group.na = 'NA\'s')

## Example 8
a <- hPlot(freq ~ Exer, data = plyr::count(MASS::survey, c('Sex', 'Exer')), type = 'column', group = 'Sex', group.na = 'NA\'s')
a$plotOptions(column = list(dataLabels = list(enabled = T, rotation = -90, align = 'right', color = '#FFFFFF', x = 4, y = 10, style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif'))))
a$xAxis(labels = list(rotation = -45, align = 'right', style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif')), replace = F)
a

## Example 9 (not working properly)
drill_function <- "#! function() {
var drilldown = this.drilldown;
function setChart(name, categories, data, color) {
chart.xAxis[0].setCategories(categories, false);
chart.series[0].remove(false);
chart.addSeries({
name: name,
data: data,
color: color || 'black'
}, false);
chart.redraw();
};
if (drilldown) { // drill down
setChart(drilldown.name, drilldown.categories, drilldown.data, drilldown.color);
} else { // restore
setChart(name, categories, data);
}
} !#"

a <- rCharts::Highcharts$new()
a$chart(type = "column")
a$series(data = list(
    list(y = 15, drilldown = list(data = c(1, 2, 3))),
    list(y = 20, drilldown = list(data = c(1, 2, 3)))), name = "test")
a$xAxis(categories = c("Brand A", "Brand B"))
a$plotOptions(column = list(cursor = 'pointer', point = list(events = list(click = drill_function))))
a

## Example 10
a <- hPlot(freq ~ Exer, data = plyr::count(MASS::survey, c('Sex', 'Exer')), type = 'bar', group = 'Sex', group.na = 'NA\'s')
a$plotOptions(bar = list(cursor = 'pointer', point = list(events = list(click = "#! function() { alert ('Category: '+ this.category +', value: '+ this.y); } !#"))))
a

## Example 11
a <- rCharts::Highcharts$new()
a$series(data = list(
    list(y = 8, url = "https://github.com/metagraf/rHighcharts", color = "lightblue"),
    list(y = 14, url = "https://github.com/metagraf/rVega", color = "lightpink"),
    list(y = 71, url = "https://github.com/ramnathv/rCharts", color = "lightgreen")
), type = "column", name = "Number of Stars")
a$plotOptions(column = list(cursor = 'pointer', point = list(events = list(click = "#! function() { location.href = this.options.url; } !#"))))
a$xAxis(categories = c("rHighcharts", "rVega", "rCharts"), title = list(text = ""))
a$yAxis(title = list(text = ""))
a$legend(enabled = F)
a

########################################################################
##### Morris
########################################################################

require(rCharts)
haireye = as.data.frame(HairEyeColor)

## {title: Simple Bar Chart}
dat = subset(haireye, Sex == "Female" & Eye == "Blue")
p1 <- mPlot(x = 'Hair', y = list('Freq'), data = dat, type = 'Bar', labels = list("Count"))
p1

### {title: Simple Bar Chart, tag: "No Hover"}
p1$set(hideHover = "auto")
p1

## {title: Multi Bar Chart}
dat = subset(haireye, Sex == "Female")
p2 <- mPlot(Freq ~ Eye, group = "Hair", data = dat, type = "Bar", labels = 1:4)
p2

## {title: Line Chart}
data(economics, package = 'ggplot2')
dat = transform(economics, date = as.character(date))
p3 <- mPlot(x = "date", y = list("psavert", "uempmed"), data = dat, type = 'Line',
 pointSize = 0, lineWidth = 1)
p3$set(xLabelFormat = "#! function (x) {
return x.toString(); }
!#")
p3


## {title: Area Chart}
p3$set(type = 'Area')
p3


##################################################################
####NVD3
##################################################################
## {title: Scatter Chart}
p1 <- nPlot(mpg ~ wt, group = 'cyl', data = mtcars, type = 'scatterChart')
p1$xAxis(axisLabel = 'Weight')
p1


## {title: MultiBar Chart}
hair_eye = as.data.frame(HairEyeColor)
p2 <- nPlot(Freq ~ Hair, group = 'Eye', data = subset(hair_eye, Sex == "Female"), type = 'multiBarChart')
p2$chart(color = c('brown', 'blue', '#594c26', 'green'))
p2

## {title: MultiBar Horizontal Chart}
p3 <- nPlot(~ cyl, group = 'gear', data = mtcars, type = 'multiBarHorizontalChart')
p3$chart(showControls = F)
p3

## {title: Pie Chart}
p4 <- nPlot(~ cyl, data = mtcars, type = 'pieChart')
p4

## {title: Donut Chart}
p5 <- nPlot(~ cyl, data = mtcars, type = 'pieChart')
p5$chart(donut = TRUE)
p5

## {title: Line Chart}
data(economics, package = 'ggplot2')
p6 <- nPlot(uempmed ~ date, data = economics, type = 'lineChart')
p6

## {title: Line with Focus Chart }
ecm <- reshape2::melt(economics[,c('date', 'uempmed', 'psavert')], id = 'date')
p7 <- nPlot(value ~ date, group = 'variable', data = ecm, type = 'lineWithFocusChart')
#test format dates on the xAxis
#also good test of javascript functions as parameters
#dates from R to JSON will come over as number of days since 1970-01-01
#so convert to milliseconds 86400000 in a day and then format with d3
#on lineWithFocusChart type xAxis will also set x2Axis unless it is specified
p7$xAxis( tickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#" )
#test xAxis also sets x2Axis
p7
#now test setting x2Axis to something different
#test format dates on the x2Axis
#test to show %Y format which is different than xAxis
p7$x2Axis( tickFormat="#!function(d) {return d3.time.format('%Y')(new Date( d * 86400000 ));}!#" )
p7
#test set xAxis again to make sure it does not override set x2Axis
p7$xAxis( NULL, replace = T)
p7

## {title: Stacked Area Chart}
dat <- data.frame(t=rep(0:23,each=4),var=rep(LETTERS[1:4],4),val=round(runif(4*24,0,50)))
p8 <- nPlot(val ~ t, group = 'var', data = dat, type = 'stackedAreaChart', id = 'chart')
p8


## {title: InteractiveGuidline(Multi-Tooltips) on Line}
p9 <- nPlot(value ~ date, group = 'variable', data = ecm, type = 'lineChart')
p9$xAxis( tickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#" )
#try new interactive guidelines feature
p9$chart(useInteractiveGuideline=TRUE)
p9


## {title: InteractiveGuidline(Multi-Tooltips) on Stack}
p10 <- p8
p10$chart(useInteractiveGuideline=TRUE)
p10

## {title: showDistX and showDistY}
p11 <- p1
p11$chart(showDistX = TRUE, showDistY = TRUE)
p11

## {title: multiChart}
p12 <- nPlot(value ~ date, group = 'variable', data = ecm, type = 'multiChart')
p12$params$multi = list(
  uempmed = list(type="area",yAxis=1),
  psavert = list(type="line",yAxis=2)
)
p12$setTemplate(script = system.file(
  "/libraries/nvd3/layouts/multiChart.html",
  package = "rCharts"
))
p12

## {title: Facets}
p13 <- nPlot(mpg ~ wt, data = mtcars, group = "gear", type = "scatterChart")
p13$params$facet = "cyl"
p13$templates$script = system.file(
  "/libraries/nvd3/layouts/nvd3FacetPlot.html",
  package = "rCharts"
)
p13

hair_eye = as.data.frame(HairEyeColor)
p14 <- nPlot(Freq ~ Hair, group = 'Sex', data = hair_eye, type = 'multiBarChart')
p14$params$facet="Eye"
p14$templates$script = system.file(
  "/libraries/nvd3/layouts/nvd3FacetPlot.html",
  package = "rCharts"
)
p14

p15 <- nPlot(Freq ~ Hair, group = 'Eye', data = hair_eye, type = 'multiBarChart')
p15$params$facet="Sex"
p15$templates$script = system.file(
  "/libraries/nvd3/layouts/nvd3FacetPlot.html",
  package = "rCharts"
)
p15


## {title: Sparklines}
p16 <- nPlot(uempmed ~ date, data = economics, type = 'sparklinePlus',height=100,width=500)
p16$chart(xTickFormat="#!function(d) {return d3.time.format('%b %Y')(new Date( d * 86400000 ));}!#")
p16



## semi replicate sparkline with a full nvd3 model by setting short height and turning off lots of things
require(quantmod)

spy <- getSymbols("SPY",auto.assign=FALSE,from="2013-01-01")
colnames(spy) <- c("open","high","low","close","volume","adjusted")

spy.df <- data.frame(index(spy),spy)
colnames(spy.df)[1] <- "date"

p17 <- nPlot(
  x = "date",
  y = "volume",
  data = spy.df,
  type = "multiBarChart",
  height = 200)
p17$chart(showControls = FALSE, showLegend = FALSE, showXAxis = FALSE, showYAxis = FALSE)
p17$xAxis(tickFormat =
  "#!function(d) {return d3.time.format('%Y-%m-%d')(new Date(d * 24 * 60 * 60 * 1000));}!#"
)
p17


## {title: ohlcBar}
## ohlcBar not fully implemented on nvd3 side, so no axes or interactive controls
## note do not melt if using ohlcBar


p18 <- nPlot(
  x = "date",
  y = "close",
  data = spy.df,
  type = "ohlcBar"
)
p18


##################################################################
####polycharts
##################################################################
## Example 1 Facetted Scatterplot
names(iris) = gsub("\\.", "", names(iris))
p1 <- rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = 'Species', type = 'point')
p1
p1$show(cdn = T)
# p1$show(static = F)


## Example 2 Facetted Barplot
hair_eye = as.data.frame(HairEyeColor)
p2 <- rPlot(Freq ~ Hair, color = 'Eye', data = hair_eye, type = 'bar')
p2$facet(var = 'Eye', type = 'wrap', rows = 2)
p2

## Example 3 Boxplot
data(tips, package = 'reshape2')
p3 <- rPlot(x = 'day', y = 'box(tip)', data = tips, type = 'box')
p3

## Example 4
require(plyr)
dat = count(mtcars, .(gear, am))
p4 <- rPlot(x = 'bin(gear, 1)', y = 'freq', data = dat, type = 'bar',
  list(var = 'am', type = 'wrap'))
p4

## Example 5 (Heat Map)
dat = expand.grid(x = 1:5, y = 1:5)
dat = transform(dat, value = sample(1:5, 25, replace = T))
p5 <- rPlot(x = 'bin(x, 1)', y = 'bin(y, 1)', color = 'value', data = dat, type = 'tile')
p5


# Example 6 (NBA Heat Map)
require(reshape2); require(scales); require(plyr)
nba <- read.csv('http://datasets.flowingdata.com/ppg2008.csv')
nba.m <- ddply(melt(nba), .(variable), transform, rescale = rescale(value))
p6 <- rPlot(Name ~ variable, color = 'rescale', data = nba.m, type = 'tile', height = 600)
p6$guides("{color: {scale: {type: gradient, lower: white, upper: steelblue}}}")
p6

#####################################################################
######Rickshaw
#####################################################################
# Example 1
p1 <- Rickshaw$new()
p1$layer(~ cyl, group = 'am', data = mtcars, type = 'bar')
p1
# Example 2
require(rCharts)
options(RCHART_TEMPLATE = 'Rickshaw.html')
require(RColorBrewer)
data(economics, package = 'ggplot2')
datm = reshape2::melt(
  economics[,c('date', 'psavert', 'uempmed')],
  id = 'date'
)
datm <- transform(datm, date = to_jsdate(date))
p2 <- Rickshaw$new()
p2$layer(value ~ date, group = 'variable', data = datm, type = 'line',
  colors = c("darkred", "darkslategrey"))


to_jsdate <- function(date_){
  val = as.POSIXct(as.Date(date_),origin="1970-01-01")
  as.numeric(val)
}

p2

p3 <- Rickshaw$new()
p3$layer(Employed ~ Year, data = longley, type = 'line', colors = c('darkred'))
p3

usp = reshape2::melt(USPersonalExpenditure)
p4 <- Rickshaw$new()
p4$layer(value ~ Var2, group = 'Var1', data = usp, type = 'area')
p4$show(T)

dat <- yaml::yaml.load('[ { x: 0, y: 40 }, { x: 1, y: 49 }, { x: 2, y: 17 }, { x: 3, y: 42 } ]')

dat <- data.frame(
  x = c(0, 1, 2, 3),
  y = c(40, 49, 2, 17)
)


dat <- data.frame(
  x = seq(1910, 2010, 10),
  y = c(92228531, 106021568, 123202660, 132165129, 151325798, 179323175,
    203211926, 226545805, 248709873, 281421906, 308745538)
)

data(USPop, package = 'car')
dat <- USPop
dat <- transform(dat, year = to_jsdate(as.Date(paste(year, '01', '01', sep = '-'))))
p4 <- Rickshaw$new()
p4$layer(population ~ year, data = dat, type = 'area', colors = 'steelblue')
p4$yAxis(orientation = 'right')
p4$set(width = 540, height = 240)
options(RCHART_TEMPLATE = 'Rickshaw.html')
p4$save('inst/libraries/rickshaw/test-rickshaw2.html')


uspexp <- reshape2::melt(USPersonalExpenditure)
names(uspexp) <- c('category', 'year', 'expenditure')
uspexp <- transform(uspexp, year = to_jsdate(as.Date(paste(year, '01', '01', sep = '-'))))
p4 <- Rickshaw$new()
p4$layer(expenditure ~ year, group = 'category', data = uspexp, type = 'area')
p4$yAxis(orientation = 'left')
p4$xAxis(type = 'Time')
p4$set(width = 540, height = 240)
options(RCHART_TEMPLATE = 'Rickshaw.html')


require(quantmod)
require(plyr)
tickers = c('AAPL', 'GOOG', 'MSFT')
quotes = llply(tickers, getSymbols, auto.assign = F)

to_jsdate <- function(date_){
  val = as.POSIXct(as.Date(date_), origin="1970-01-01")
  as.numeric(val)
}

getSymbols('AAPL')
AAPL <- transform(AAPL, date = to_jsdate(index(AAPL)))
names(AAPL) = gsub(".", "_", names(AAPL), fixed = TRUE)

require(rCharts)
options(RCHART_TEMPLATE = 'Rickshaw.html')
r1 <- Rickshaw$new()
r1$layer(x = 'date', y = 'AAPL_Open', data = AAPL, type = 'line', colors = 'steelblue')
r1$xAxis(type = 'Time')
r1$yAxis(orientation = 'left')

riPlot <- function(x, y, data, type, ..., xAxis = list(type = 'Time'),
    yAxis = list(orientation = 'left')){
  options(RCHART_TEMPLATE = 'Rickshaw.html')
  r1 <- Rickshaw$new()
  r1$layer(x = x, y = y, data = data, type = type, ...)
  do.call(r1$xAxis, xAxis)
  do.call(r1$yAxis, yAxis)
  return(r1)
}


####################################################################
#####Timeline
####################################################################
h = Timeline$new()
h$main(
  headline = 'Sh*t People Say',
  type = "default",
  text = "People Say Stuff",
  startDate = "2012,1,26"
)
h$event(
  startDate = "2011,12,12",
  endDate = "2012,1,27",
  headline = "Vine",
  text = "Vine Text",
  asset = list(
    media = "https://vine.co/v/b55LOA1dgJU",
    credit = "",
    caption = ""
  )
)

h$event(
  startDate = "2011,12,12",
  headline = "Sh*t Girls Say",
  text = "Vine Text",
  asset = list(
    media = "http://youtu.be/u-yLGIH7W9Y",
    credit = "",
    caption = ""
  )
)

dat <- read.csv('~/Downloads/timeline.csv', stringsAsFactors = F)
events_ <- toJSONArray(dat, json = F)
lapply(events_, function(event_){
  event_$text = markdown::renderMarkdown(text = event_$text)
  event_
})

dat <- read.csv('~/Downloads/timeline.csv', stringsAsFactors = F)
h = Timeline$new()
h$main(
  headline = 'Financial Time Series',
  type = "default",
  text = "People Say Stuff",
  startDate = "2012,1,26"
)
h$config(
  font = "Merriweather-NewsCycle"
)
h$events(toJSONArray(dat, json = F))

makeTimeLine <- function(mdFile){
  require(rCharts); require(yaml)
  md_ = paste(readLines(mdFile, warn = F), collapse = '\n')
  dat = lapply(strsplit(md_, '\n---|^---')[[1]][-1], yaml.load)
  tml_ = Timeline$new()
  main = dat[[1]]
  do.call(tml_$main, main[names(main) != "config"])
  do.call(tml_$config, main$config)
  for (i in 2:(length(dat))){
    do.call(tml_$event, dat[[i]])
  }
  return(tml_)
}

#####################################################################
########UV Charts
#####################################################################
d1 <- rCharts$new()
d1$setLib('uvcharts')
d1$set(categories = names(dat), type = 'Bar')
d1$set(dataset = dat)
d1$show(cdn = T)


d2 <- dTable(iris, iDisplayLength = 4)
d2$set()





library(plyr)
library(rCharts)

make_dataset <- function(x, y, dat, group = NULL){
  require(plyr)
  dat <- rename(dat, setNames(c('name', 'value'), c(x, y)))
  if (!is.null(group)){
    dlply(dat, group, toJSONArray, json = F)
  } else {
    list(main = toJSONArray(dat, json = F))
  }
}

hair_eye <- subset(as.data.frame(HairEyeColor),Sex == "Male")
dat <- rename(dat, c('Eye' = 'name', 'Freq' = 'value'))
dat3 <- dlply(dat, "Hair", toJSONArray, json = F)

dat4 <- make_dataset("Hair", "Freq", dat = hair_eye, group = "Eye")

d1 <- rCharts$new()
d1$setLib('uvcharts')
d1$set(
  categories = names(dat4),
  type = 'Bar',
  dataset = dat4,
  dom = 'ch1'
)
d1$show(cdn = T)


## {title: Line Chart}
data(economics, package = 'ggplot2')
dat = transform(economics, date = as.character(date))
datm = reshape2::melt(dat, id = 'date')
uPlot("date", "value", data = datm, group = 'variable', type = 'LineChart')

######################################################################
######xcharts
######################################################################
require(rCharts)
options(RCHART_WIDTH = 800, RCHART_HEIGHT = 400)

### {title: Bar Chart, working: FALSE}
p1 <- xCharts$new()
p1$set(xScale = 'ordinal', yScale = 'linear')
p1$layer(~ cyl, data = mtcars, type = 'bar')
p1

### {title: Multi Bar Chart, working: TRUE}
haireye = subset(as.data.frame(HairEyeColor), Sex == "Male")
p2 <- xCharts$new()
p2$set(xScale = 'ordinal', yScale = 'linear', width = 600)
p2$layer(Freq ~ Hair, group = 'Eye', data = haireye, type = 'bar')
p2

### {title: Line Chart, working: TRUE}
uspexp = reshape2::melt(USPersonalExpenditure)
names(uspexp)[1:2] = c('Category', 'Year')
p3 <- xCharts$new()
p3$layer(value ~ Year, group = 'Category', data = uspexp)
p3$set(xScale = 'linear', yScale = 'linear', type = 'line-dotted', xMin = 1935)
p3



