codes = read.csv('code.csv',  stringsAsFactors =FALSE)
#term = 'data'
get_locations = function(term, limit = 100, fromage = 14, continents = 'All'){

	if(continents[1] == 'All')	countries = codes$country_code
	else countries = codes$country_code[codes$continent %in% continents]
	
	if(term =='enter job details') return(NULL)

	xml_names = c("jobtitle", "company", "city", "state", "country", "formattedLocation", 
	"source", "date", "snippet", "url", "onmousedown", "latitude", 
	"longitude", "jobkey", "sponsored", "expired", "indeedApply", 
	"formattedLocationFull", "formattedRelativeTime")
	
	cat('Search :', term, ':', continents, '\n')		

	#add more search options here
	term = gsub(' ', '+', term)
	urls = paste('http://api.indeed.com/ads/apisearch?',
			'publisher=9391027427234359',
			paste0('q=', term),
			'userip=1.2.3.4',
			paste0('fromage=', fromage),
			paste0('limit=', limit),
			paste('co=', countries, sep =''),
			'latlong=1',
			'useragent=Mozilla/%2F4.0%28Firefox%29',
			'v=2', sep = '&')
	indeed_feed = getURIAsynchronous(urls)
	
	# job_list = alply(indeed_feed , 1, xmlToDF,
		# xpath = "/response/results/result", isXML = FALSE, verbose = FALSE
		# )
	# job_df <- rbind.fill(job_list)
	# try this approach as well: http://stackoverflow.com/questions/22888949/r-fast-xml-parsing
	#job_df
	
	xml_matrix_list = alply(indeed_feed , 1, xml_to_matrix, xpath = '/response/results/result',names = xml_names )
	xml_matrix = do.call('rbind', xml_matrix_list)
	xml_df = as.data.frame(xml_matrix , stringsAsFactors = FALSE)
	names(xml_df ) = xml_names
	xml_df
		
	}
	
indeed_api = function(term = '', 
	title = '', 
	location = '', 
	fromage = 30, 
	limit = 50, 
	countries = "us"){
	#add start stop to paramaters
	
	title_query = function(title){
		if(is.null(title)| title == '') return(NULL)
		else
		return(paste0('+title%3A(', title, ')'))
		}
		
	if(term =='enter job details') return(NULL)
		
	cat('Search :', term, '\n')
	
	urls = paste('http://api.indeed.com/ads/apisearch?',
			'publisher=9391027427234359',
			'userip=1.2.3.4',
			'latlong=1',
			'useragent=Mozilla/%2F4.0%28Firefox%29',
			paste0('q=', term ,title_query(title)),
			paste0('l=', location),
			paste0('fromage=', fromage),
			paste0('limit=', limit),
			paste('co=', countries, sep =''),
			'v=2', sep = '&')
			
	urls = gsub(' ', '+', urls)
	indeed_feed = getURIAsynchronous(urls)
	
	xml_names = c("jobtitle", "company", "city", "state", "country", "formattedLocation", 
		"source", "date", "snippet", "url", "onmousedown", "latitude", 
		"longitude", "jobkey", "sponsored", "expired", "indeedApply", 
		"formattedLocationFull", "formattedRelativeTime")
	
	
	xml_matrix_list = alply(indeed_feed , 1, xml_to_matrix, xpath = '/response/results/result',names = xml_names )
	xml_matrix = do.call('rbind', xml_matrix_list)
	xml_df = as.data.frame(xml_matrix , stringsAsFactors = FALSE)
	names(xml_df ) = xml_names
	xml_df
	}
	
map_jobs2 <- function(locations){
	library('googleVis')
	
	if(is.null(locations)){
		return(gvisMap(	data.frame(locationvar = 'Manchester', tipvar= ''), locationvar = "locationvar" , tipvar ='tipvar',
								options=list(showTip=TRUE, 
									zoomLevel = 2,
									  showLine=TRUE, 
									  enableScrollWheel=TRUE,
									  mapType='normal', 
									  useMapTypeControl=TRUE,
									  width="100%", height="75vh")))
									}
	if(nrow(locations) == 0) return(list(html = list(chart = '<h1>No jobs found</h1>')))
	
	locations$latitude <- as.numeric(locations$latitude)
	locations$longitude<- as.numeric(locations$longitude)

	locations$lat_long <- paste(locations$latitude, locations$longitude, sep = ':')
	locations_complete = locations[!is.na(locations$latitude),]
	
	if(nrow(locations_complete) > 400){
		locations_complete  = ddply(locations_complete ,.(country), function(.x){
						.x$order = 1:nrow(.x)
						.x
					})
		locations_complete = locations_complete[order(locations_complete$order)[1:400],]	
		
		}
	 
	locations_complete $tip =  with(locations_complete, paste('<a href="', url, '"target="_blank"',  '">', jobtitle,'</a>',
		'<br>',  company, '-', city, '<br>', snippet ))
		
	ds_map <- gvisMap(locations_complete, locationvar = "lat_long" , tipvar = 'tip',
						 options=list(showTip=TRUE, 
									  showLine=TRUE, 
									  enableScrollWheel=TRUE,
									  mapType='normal', 
									  useMapTypeControl=TRUE,
									  width="100%", height="75vh", margin= "0",border="none" ))

	#plot(ds_map)
	ds_map
	}
							  

get_and_map = function(term){
	locations = get_locations (term = term)
	map_jobs2(locations )
	}
	
xmlToDF = function(doc, xpath, isXML = TRUE, usewhich = TRUE, verbose = TRUE) {
 
    if (!isXML)
        doc = xmlParse(doc)
    #### get the records for that form
    nodeset <- getNodeSet(doc, xpath)
	if(length(nodeset) == 0) return(NULL)
 
    ## get the field names
    var.names <- lapply(nodeset, names)
 
    ## get the total fields that are in any record
    fields = unique(unlist(var.names))
 
    ## extract the values from all fields
    dl = lapply(fields, function(x) {
        if (verbose)
            print(paste0("  ", x))
        xpathSApply(doc, paste0(xpath, "/", x), xmlValue)
    })
 
    ## make logical matrix whether each record had that field
    name.mat = t(sapply(var.names, function(x) fields %in% x))
    df = data.frame(matrix(NA, nrow = nrow(name.mat), ncol = ncol(name.mat)))
    names(df) = fields
 
    ## fill in that data.frame
    for (icol in 1:ncol(name.mat)) {
        rep.rows = name.mat[, icol]
        if (usewhich)
            rep.rows = which(rep.rows)
        df[rep.rows, icol] = dl[[icol]]
    }
 
    return(df)
}

xml_to_matrix = function(doc, xpath, names){
	doc = xmlParse(doc)
	nodeset <- getNodeSet(doc, path = xpath)
	size = xmlSize(nodeset )

	out = matrix(NA, ncol = length(names), nrow = size)
	if(size == 0) return(out)
	for(i in 1:size){
		v = getChildrenStrings(nodeset [[i]])
		out[i,] <- v[fmatch(names, names(v))]
		}
	#t(out)
	out
	}
	
map_jobs = function(locations, location = 'Europe', zoom = 4, ...){
	library(ggmap)
	library(mapproj)
	map <- get_map(location = location, zoom = zoom, ...)
	ggmap(map, extent = 'device') +
	  geom_point(aes(x = jitter(longitude), y = latitude), data = locations, alpha = .8, colour = 2, size=2)
	 }

get_logs = function (appDir = getwd(), appName = NULL, account = NULL, entries = 50, 
    streaming = FALSE) 
{
	library(shinyapps)
    target <- shinyapps:::deploymentTarget(appDir, appName, account)
    accountInfo <- shinyapps:::accountInfo(target$account)
    lucid <- shinyapps:::lucidClient(accountInfo)
    application <- shinyapps:::getAppByName(lucid, accountInfo, target$appName)
    logs <- lucid$getLogs(application$id, entries, FALSE, 
            NULL)
	return(logs)

}

get_search_logs = function(entries = 1000,...){
	logs = get_logs(..., entries = entries)
	logs_split = strsplit(logs, '\\n')[[1]]
	search_logs = logs_split[grep('Search', logs_split)]
	search_logs  = data.frame(do.call('rbind', strsplit(search_logs , ': ')))
	search_logs
	}
	
