# ui.R
codes = read.csv('code.csv',  stringsAsFactors =FALSE)
shinyUI(fluidPage(theme = "bootstrap.css",

	tags$head(
		tags$title('International jobs search | Map'),
		tags$meta(name="description", content="Find a job anywhere in the world. Search for a job and view thousands of jobs all over the world on an interactive map. Jobs by Indeed. Site by Tom Liptrot")
	),

		fluidRow(
			column(4, 
	
					textInput("term", label = h3("Search for international jobs"), value = "enter job details"),
					#textInput("title", label = h6("Job title"), value = ""),
					
					submitButton("Search jobs")
					),
			column(4, 
				#textInput("location", label = h6("Location"), value = ""),
				checkboxGroupInput("continents", label = h3("Select continents"), 
					choices = as.list( unique(codes$continent)),
					selected = unique(codes$continent), inline = TRUE)
				
				)	

				),
		htmlOutput("view"),
		
					
				p('Site made by Tom Liptrot in R, using the 
				Shiny, rvest plyr, XML, RCurl, googleVis and fastmatch packages'),
				HTML('<a href="https://uk.linkedin.com/pub/tom-liptrot/66/1b2/bb9">
      
          <img src="https://static.licdn.com/scds/common/u/img/webpromo/btn_viewmy_160x33.png" width="160" height="33" border="0" alt="View Tom Liptrot profile on LinkedIn"> </a>'),
					tags$div(
					  HTML('<span id=indeed_at><a href="http://www.indeed.com/">jobs</a> by <a
					href="http://www.indeed.com/" title="Job Search"><img
					src="http://www.indeed.com/p/jobsearch.gif" style="border: 0;
					vertical-align: middle;" alt="Indeed job search"></a></span>'
													)
						)
	)
	)
	
	continents = as.list(c('All', unique(codes$continent)))
		
	