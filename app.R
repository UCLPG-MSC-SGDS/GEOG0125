
# load packages
library(htmlwidgets)
library(shiny)
library(shinythemes)
library(leaflet)
library(tmap)
library(sf)
library(DT)
library(dplyr)

# set your main directory
setwd("/Volumes/Anwar-HHD/GEOG0125/Practicals/Shiny")

# open RDS file
# save file as RDS format for Shiny
la.spatial.df <- readRDS("Road Accidents Output Data.RDS")

# load shape files 
laboundaries <- read_sf("North_West_LAs.shp")

# Create new Shiny web application. You can run the application by clicking
# the 'Run App' button above.

#Part 1: Define User Interface for application
ui <- fluidPage(
	# set the theme (see https://rstudio.github.io/shinythemes/)
	theme = shinytheme("cerulean"),
	# Title for web application
	titlePanel("North West Road Accident Risk Maps (2015-2020)"),
	# Make application dynamic by including a sidebar - use sidebarLayout() function
	sidebarLayout(
		sidebarPanel(
		# use selectInput() to capture the year values 2015, 2016, ..., 2020
		selectInput(inputId = "Years", label = "Select a year:", choices = c(2015, 2016, 2017, 2018, 2019, 2020))
	),
	# Plots of distributions of the risk estimates from Bayesian model
	mainPanel(
		tabsetPanel(
			tabPanel("Posterior Risk Estimates", leafletOutput("CompactLayeredMapOutput", height = "90vh")),
			tabPanel("Casualties (Plots)", plotOutput("HistogramOutput", height = "90vh")),
			tabPanel("Data Viewer", dataTableOutput("DataViewerOutput", height = "90vh"))
		)
	)
	)
)

# Part 2: Define server logic
server <- function(input, output) {
	# create a reactive function 
	year_react <- reactive({
		yr <- la.spatial.df[la.spatial.df$year == input$Years,]
		return(yr)
	})
	# year_react() apply this function in place of la.spatial.df
	output$CompactLayeredMapOutput <- renderLeaflet({
		# Create risk categories for labels
		RiskCategorylist <- c("0.01 to 0.10", "0.11 to 0.25", "0.26 to 0.50", "0.51 to 0.75", "0.76 to 0.99", "1.00 (null value)", ">1.00 to 1.10", "1.11 to 1.25", "1.26 to 1.50", "1.51 to 1.75", "1.76 to 2.00", "2.01 to 3.00", "Above 3.00")
		# Create the colours for the above categories - from extreme blues to extreme reds
		RiskPalette <- c("#33a6fe","#65bafe","#98cffe","#cbe6fe","#dfeffe","#fef9f9","#fed5d5","#feb1b1","#fe8e8e","#fe6a6a","#fe4646","#fe2424","#fe0000")
		
		# Significance label categories and colour scheme
		SigCategorylist <- c("Significantly low", "Not Significant", "Significantly high")
		SigPalette <- c("#33a6fe", "white", "#fe0000")
		
		# create one table containing the year, casualties, RelativeRiskCat, Signficance & ProbCat
		`Relative Risk` <- year_react()[, c(1,4,10)]
		`Significance` <- year_react()[, c(1,4,11)]
		`Local Authority Areas` <- laboundaries
		
		# apply filters here to make dynamic
		InteractiveMap <- tm_shape(`Significance`) +
			tm_fill("Significance", style = "cat", title = "Significance Categories", palette = SigPalette, labels = SigCategorylist) +
			tm_shape(`Relative Risk`) + # map of relative risk
			tm_polygons("RelativeRiskCat", style = "cat", title = "Relavtive Risk", palette = RiskPalette, labels = RiskCategorylist, border.alpha = 0) +
			tm_shape(`Local Authority Areas`) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black")
		
		InteractiveMap <- tmap_leaflet(InteractiveMap) %>%
			addProviderTiles(providers$OpenStreetMap, group="Street") %>% 
			addProviderTiles(providers$CartoDB.DarkMatter, group="Dark") %>%
			addLayersControl(baseGroups = c("Street", "Dark"), overlayGroups=c("Local Authority Areas", "Relative Risk", "Significance"), position="topleft", options = layersControlOptions(collapsed = FALSE))
		
		InteractiveMap
		
	})
	output$HistogramOutput <- renderPlot({
		hist(year_react()$casualties, 
				 xlab = paste("Number of recorded casualties in ", input$Years, sep= ""),
				 ylab = "Frequency", main=paste("Distribution of Road Accidents in ", input$Years, sep = ""), breaks = 50)
	})
	output$DataViewerOutput <- renderDataTable({
			DT::datatable(year_react()[, c("LAD21CD", "LAD21NM", "year", "casualties", "Risks")], rownames = FALSE, options = list(sScrollY = '75vh', scrollCollapse = TRUE), extensions = list("Scroller"))
	})
}

#Part 3: Run shiny app
shinyApp(ui = ui, server = server)
