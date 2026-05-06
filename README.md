# Short Description

https://laurabrune.github.io/datavisualization-fuel-efficiency/

This project presents a data visualization report exploring which car
characteristics influence fuel efficiency in the American market between
1984 and 2014. It was original created as an Assignment in the course 
"Data Management & Visualization" taught by Stefan Rose at the University of 
Cologne. 

The analysis is built using Quarto and is accessible in two formats:

-   as a Quarto document for reproducible analysis

-   as a published website via GitHub Pages: https://laurabrune.github.io/datavisualization-fuel-efficiency/

The report investigates how the following variables relate to fuel
efficiency:

-   Type of gear (automatic vs. manual)

-   Number of gears

-   Number of cylinders

-   Car brand

A key aspect of this project is that the research question is addressed
primarily through data visualization rather than formal statistical
modeling. Instead of building regression models or testing hypotheses,
the analysis relies on carefully designed plots with `ggplot2` to reveal
patterns, trends, and relationships in the data.

This approach enables an intuitive understanding of the data, while
requiring careful interpretation, as visual patterns do not necessarily
imply causation.

# Key findings

Some of the key findings are:

-   Fuel efficiency improved significantly after the late 2000s

-   Automatic gearboxes are associated with higher fuel efficiency

-   More gears generally correlate with better efficiency

-   Fewer cylinders tend to improve fuel consumption Project Structure

# Project structure

The report can be rendered using Quarto. All data preparation and
visualization steps are organized in R/main.R, which calls
01_transform_data.R and 02_data_visualization.R.

# Data Source

Fuel economy data is provided by the U.S. Department of Energy:

<https://www.fueleconomy.gov/feg/download.shtml>

# Author

Laura Brune
