# Automatic Data Visualization with Shiny Modules.

### Install package
To install the package, you can use 

```
devtools::install_github(Davide-bll/Shinymod)
```


## Main modules
### Plot Module
Modules for building basic automatic datavisualization dashboard. It includes BoxPlots, Histograms, 
and Scatter Plot.

A Dataframe is expected in input, wide and long format.
The default value of the panel type plot is *aggregate*, which shows the variable selected without differentiating the groups the observations belong to.
You can deal with one "group" at a time, by selcting the criteria of differentiating.


As an exmaple, consider the CO2 dataframe, which has this structure:


|   obs      | conc          | uptake        | treatment     |
|------------| ------------- | ------------- |---------------|
|   1        | Value         | Value         | treat1        |
|   2        | Value         | Value         | treat2        |    

you can visualize the scatter plot *conc* vs *treatment* without differentiating the treatment values of differenciating them.
A basic dashboard would be:


```
require(shiny)
require(shinymod)

if(interactive()) {

ui <- fluidPage(

  typeplotUi("type")


)

server <- function(input, output, session) {

  callModule(typeplot, "type", data = CO2)

}


shinyApp(ui, server)

}
```
or just type:
```
help(typeplot)
```
If you want to observe the scatter plot, choose *plot_2var* from the type of visualization panel,
and you'll get something like:



![](img_readme/agg_plot.JPG?raw=true)

Since *aggregate* is the default value of typeplot, you'll see all the observation plotted. 
If you want to differentiate the treatments, just select the column treatments from the *typeplot* panel:

![](img_readme/grp_plot.jpg?raw=true)

To see a more concrete of a data exploration app, see this [data exploarion app](https://github.com/Davide-bll/Shinymod/blob/master/examples/data_exploration_app.R), 
which load a local file, (csv, txt, or feather), and do some basic filtering and automatic Data visualization

