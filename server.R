suppressPackageStartupMessages(library('rvest'))
suppressPackageStartupMessages(library('plyr'))
suppressPackageStartupMessages(library('XML'))
suppressPackageStartupMessages(library('shiny'))
suppressPackageStartupMessages(library('RCurl'))
suppressPackageStartupMessages(library('googleVis'))
suppressPackageStartupMessages(library(fastmatch))

source('2.functions.r')


shinyServer(function(input, output) {
	locations <- reactive({
		#get_locations (term = input$term, continents = input$continents)
		indeed_api(term = input$term, 
			#title = input$title, 
			#location = input$location, 
			fromage = input$added,
			countries =  codes$country_code[codes$continent %in% input$continents])

	})
output$view <- renderGvis({
	map_jobs2(locations())
	})
}) 