  
#-------------------------------------------------------------------------------

## Set path ixmp folder in message_ix working copy
message_ix_path = Sys.getenv("MESSAGE_IX_PATH")

# load packages and launch the IX modeling platform
source(file.path(Sys.getenv("IXMP_R_PATH"), "ixmp.R"))

# launch the IX modeling platform using the default central ORCALE database
ix_platform = ixPlatform()

# Load other packages
require(RCurl)
require(magrittr)
require(dplyr)
require(xlsx)
memory.limit(size=1e9)

#-------------------------------------------------------------------------------
# functions to upload file to MESSAGEix_WorkDB

database_upload = function(filename, path, user = "parkinso", password = "goodpassword", verbose = FALSE) {
  
  # set file location
  aFile = paste(path,filename,sep='/')
  
  # setup curl options
  curl = getCurlHandle()
  curlSetOpt(cookiejar="",  useragent = "Mozilla/5.0", verbose = verbose, followlocation = TRUE, curl = curl)
  
  # login
  loginUrl = paste("db1.ene.iiasa.ac.at/MESSAGEix_WorkDB/dsd?Action=loginform&usr=", user, "&pwd=", password, sep = '')
  html.response1 = getURLContent(loginUrl, curl = curl)
  if (verbose) write(rawToChar(html.response1), file = 'html_response1.html')
  
  # file upload
  uploadUrl = "db1.ene.iiasa.ac.at/MESSAGEix_WorkDB/dsd?Action=uploadFile"
  html.reponse2 = postForm(uploadUrl, curl = curl, "name" = "file1", "filedata" = fileUpload(aFile))
  if (verbose) write( html.response2, file = 'html_response2.html')

  }

#-------------------------------------------------------------------------------

# existing model and scenario names to start with
modelName = "MESSAGE-GLOBIOM CD-LINKS R2.3.1"
scenarioName = "baseline"

# new model and scenario name for ix platform
newmodelName = "MESSAGE-GLOBIOM CD-LINKS R2.3.1"
newscenarioName = paste("SSP2_INDC","baseline-water-v3",sep='_')

# model and scenario names for database
dbmodelName = "SSP_INDC_CD-Links_R2.3.1"
dbscenarioName =  paste("SSP2_INDC","baseline-water-v3",sep='_')

comment = paste( "MESSAGE-GLOBIOM SSP2 for CD-LINKS with water consumption - ", scenarioName, sep = '' )

#-------------------------------------------------------------------------------
# initialize a new ixDatastructure
ixDSoriginal = ix_platform$datastructure(model=modelName, scen=scenarioName)

# clone data structure with new scenario name
ixDS = ixDSoriginal$clone(new_model = newmodelName, new_scen = newscenarioName, annotation = comment, keep_sol = FALSE)

# check out
ixDS$check_out()

# ----- modifications

## Add consumption as an emission
ret = ixDS$add_set( "emission", "fresh_consumption" )
ret = ixDS$add_set( "emission", "saline_consumption" )
ret = ixDS$add_set( "emission", "instream_consumption" )
consumption_type = data.frame( freshwater_supply = "fresh_consumption", saline_supply = "saline_consumption" )
res = lapply( c( ixDS$set('node') ), function(nn){
	
	withdrawal_intensities.df = data.frame( tryCatch( ixDS$par( 'input', list(commodity=c('freshwater_supply','saline_supply'),node_loc=nn) ), error = function(e){} ) )
	
	if(length(withdrawal_intensities.df)>0)
		{
		return_intensities.df = data.frame( tryCatch( ixDS$par( 'emission_factor', list(emission=c('fresh_wastewater','saline_wastewater'),node_loc=nn) ), error = function(e){} ) )
		
		res1 = lapply( 1:nrow(withdrawal_intensities.df), function(i){ 
			ixDS$add_par( 	"emission_factor", 
							paste( nn, withdrawal_intensities.df$technology[ i ], as.character( withdrawal_intensities.df$year_vtg[ i ] ), as.character( withdrawal_intensities.df$year_act[ i ] ), as.character( withdrawal_intensities.df$mode[ i ] ), as.character( unlist( consumption_type[ as.character( withdrawal_intensities.df$commodity[ i ] ) ] ) ), sep = '.' ), # set key
							round( ( withdrawal_intensities.df$value[ i ] - return_intensities.df$value[ which( return_intensities.df$technology == withdrawal_intensities.df$technology[ i ] & return_intensities.df$year_vtg == withdrawal_intensities.df$year_vtg[ i ] & return_intensities.df$year_act == withdrawal_intensities.df$year_act[ i ] & return_intensities.df$mode == withdrawal_intensities.df$mode[ i ] ) ]  ), digits = 6 ), # parameter value
							'-' )				
			} )
		
		}
	} )

# Hydropower consumption	
withdrawal_intensities.df = data.frame( tryCatch( ixDS$par( 'input', list(commodity=c('freshwater_instream')) ), error = function(e){} ) )
res = lapply( 1:nrow(withdrawal_intensities.df), function(i){ 
			ixDS$add_par( 	"emission_factor", 
							paste( as.character( withdrawal_intensities.df$node_loc[ i ] ), withdrawal_intensities.df$technology[ i ], as.character( withdrawal_intensities.df$year_vtg[ i ] ), as.character( withdrawal_intensities.df$year_act[ i ] ), as.character( withdrawal_intensities.df$mode[ i ] ), "instream_consumption", sep = '.' ), # set key
							round( ( withdrawal_intensities.df$value[ i ] ), digits = 6 ), # parameter value
							'-' )				
			} )	
	
# commit scenario to platform and set as default case
ixDS$commit(comment)
ixDS$set_as_default()

# run MESSAGE scenario in GAMS and import results in ix platform
ixDS$solve(model = "MESSAGE-MACRO", case = dbscenarioName)
gc()

#-------------------------------------------------------------------------------

# # start Python-based reporting script
# system( paste( "C:/Users/parkinso/AppData/Local/Continuum/Anaconda2/python.exe C:/Users/parkinso/git/message_ix/ixmp/reporting/iamc_report.py --scenario ",'"', newscenarioName,'"', " --scenario_out ",'"', dbscenarioName,'"', " --model ",'"', newmodelName,'"', " --model_out ",'"', dbmodelName,'"', "", sep = ''))

# # # read reporting files with "historical" periods
# path.history = "C:/Users/parkinso/Documents"
# data.nopolicy = data.frame(read.csv(paste(path.history,"CD_Links_R2.3.1_20170906_upload__NoPolicy_V3.csv",sep='/'), stringsAsFactors = FALSE))
# data.npi = data.frame(read.csv(paste(path.history,"CD_Links_R2.3.1_20170906_upload__NPi_V3.csv",sep='/'), stringsAsFactors = FALSE))

# # read solution file
# path.solution = "C:/Users/parkinso/git/message_ix/data_utils/upload"
# file.solution = paste(dbmodelName, "_", dbscenarioName, ".xlsx", sep = "")
# data.solution = read.xlsx(paste(path.solution,file.solution,sep='/'), as.data.frame = TRUE, sheetName = 'data')

# # merge reporting xlsx file with other files with "historical" periods
# data.merged = inner_join(inner_join(select(data.nopolicy, Region, Variable, X2000, X2005, X2010), select(data.npi, Region, Variable, X2020)), select(data.solution, Model, Scenario, Region, Variable, Unit, X2030, X2040, X2050, X2060, X2070, X2080, X2090, X2100, X2110)) %>% select(Model, Scenario, Region, Variable, Unit, X2000, X2005, X2010, X2020, X2030, X2040, X2050, X2060, X2070, X2080, X2090, X2100, X2110)
# names(data.merged) =  c( "Model", "Scenario", "Region", "Variable", "Unit", as.character( c(2000, 2005, seq( 2010, 2110, by = 10 ) )) )
# file.merged = paste( dbmodelName, "_", dbscenarioName, "_merged.xlsx", sep = "" )
# write.xlsx(data.merged, paste( path.solution, file.merged, sep = '/' ), sheetName = "data", row.names = FALSE)
	
#-------------------------------------------------------------------------------

# upload file to MESSAGEix_WorkDB scenario database
#database_upload( file.merged, path.solution, user = "parkinso", password = "goodpassword" )

# rm(data.solution,data.npi,data.nopolicy,data.merged,ixDSoriginal,ixDS)
# gc()

#-------------------------------------------------------------------------------
# Water constraint scenarios

ixDSbase = ix_platform$datastructure(model=newmodelName, scen=newscenarioName)

scenarios = c('NPi2020_1000-con-prim-dir-ncr')

for( scen in scenarios )
	{
		
	# existing model and scenario names to start with
	modelName = "MESSAGE-GLOBIOM CD-LINKS R2.3.1"
	scenarioName = scen

	# new model and scenario name for ix platform
	newmodelName = "MESSAGE-GLOBIOM CD-LINKS R2.3.1"
	newscenarioName = paste( paste("SSP2_INDC",scen,sep="_"), 'water', 'v3', sep = '-' )
	dbscenarioName = paste( paste("SSP2_INDC",scen,sep="_"), 'water', 'v3', sep = '-' )

	comment = paste( "MESSAGE-GLOBIOM SSP2 for CD-LINKS with water consumption - ", scenarioName, sep = '' )

	#-------------------------------------------------------------------------------
	# initialize a new ixDatastructure
	ixDSoriginal = ix_platform$datastructure(model=modelName, scen=scenarioName)
	
	region = as.character( ixDSoriginal$set('cat_node')$node )
	firstmodelyear = as.numeric( as.character( ixDSoriginal$set('cat_year')[which(as.character(unlist(ixDSoriginal$set('cat_year')[,'type_year']))=='firstmodelyear'),'year'] ) )
	lastmodelyear = as.numeric( as.character( ixDSoriginal$set('cat_year')[which(as.character(unlist(ixDSoriginal$set('cat_year')[,'type_year']))=='lastmodelyear'),'year'] ) )
	modelyears = as.numeric( unlist(ixDSoriginal$set('year')) )[ which( as.numeric( unlist(ixDSoriginal$set('year')) ) >= firstmodelyear & as.numeric( unlist(ixDSoriginal$set('year')) ) <= lastmodelyear ) ]
	
	# clone data structure with new scenario name
	ixDS = ixDSoriginal$clone(new_model = newmodelName, new_scen = newscenarioName, annotation = comment, keep_sol = FALSE)

	# check out
	ixDS$check_out()

	# ----- modifications

	## Add consumption as an emission
	ret = ixDS$add_set( "emission", "fresh_consumption" )
	ret = ixDS$add_set( "emission", "saline_consumption" )
	ret = ixDS$add_set( "emission", "instream_consumption" )
	ret = ixDS$add_set( 'type_emission', 'water_consumption' ) # Add to technology types
	ret = ixDS$add_set( 'type_emission', 'fresh_consumption' ) # Add to technology types
	ret = ixDS$add_set( 'cat_emission', paste( 'water_consumption', 'fresh_consumption', sep='.' ) )
	ret = ixDS$add_set( 'cat_emission', paste( 'fresh_consumption', 'fresh_consumption', sep='.' ) )	
	consumption_type = data.frame( freshwater_supply = "fresh_consumption", saline_supply = "saline_consumption" )
	res = lapply( c( ixDS$set('node') ), function(nn){
		
		withdrawal_intensities.df = data.frame( tryCatch( ixDS$par( 'input', list(commodity=c('freshwater_supply','saline_supply'),node_loc=nn) ), error = function(e){} ) )
		
		if(length(withdrawal_intensities.df)>0)
			{
			return_intensities.df = data.frame( tryCatch( ixDS$par( 'emission_factor', list(emission=c('fresh_wastewater','saline_wastewater'),node_loc=nn) ), error = function(e){} ) )
			
			res1 = lapply( 1:nrow(withdrawal_intensities.df), function(i){ 
				ixDS$add_par( 	"emission_factor", 
								paste( nn, withdrawal_intensities.df$technology[ i ], as.character( withdrawal_intensities.df$year_vtg[ i ] ), as.character( withdrawal_intensities.df$year_act[ i ] ), as.character( withdrawal_intensities.df$mode[ i ] ), as.character( unlist( consumption_type[ as.character( withdrawal_intensities.df$commodity[ i ] ) ] ) ), sep = '.' ), # set key
								round( ( withdrawal_intensities.df$value[ i ] - return_intensities.df$value[ which( return_intensities.df$technology == withdrawal_intensities.df$technology[ i ] & return_intensities.df$year_vtg == withdrawal_intensities.df$year_vtg[ i ] & return_intensities.df$year_act == withdrawal_intensities.df$year_act[ i ] & return_intensities.df$mode == withdrawal_intensities.df$mode[ i ] ) ]  ), digits = 6 ), # parameter value
								'-' )				
				} )
			
			}
		} )

	# Hydropower consumption	
	withdrawal_intensities.df = data.frame( tryCatch( ixDS$par( 'input', list(commodity=c('freshwater_instream')) ), error = function(e){} ) )
	res = lapply( 1:nrow(withdrawal_intensities.df), function(i){ 
				ixDS$add_par( 	"emission_factor", 
								paste( as.character( withdrawal_intensities.df$node_loc[ i ] ), withdrawal_intensities.df$technology[ i ], as.character( withdrawal_intensities.df$year_vtg[ i ] ), as.character( withdrawal_intensities.df$year_act[ i ] ), as.character( withdrawal_intensities.df$mode[ i ] ), "instream_consumption", sep = '.' ), # set key
								round( ( withdrawal_intensities.df$value[ i ] ), digits = 6 ), # parameter value
								'-' )				
				} )	
		
	# commit scenario to platform and set as default case
	ixDS$commit(comment)
	ixDS$set_as_default()

	# run MESSAGE scenario in GAMS and import results in ix platform
	ixDS$solve(model = "MESSAGE-MACRO", case = dbscenarioName)

	#-------------------------------------------------------------------------------
	
	# # start Python-based reporting script
	# system( paste( "C:/Users/parkinso/AppData/Local/Continuum/Anaconda2/python.exe C:/Users/parkinso/git/message_ix/ixmp/reporting/iamc_report.py --scenario ",'"', newscenarioName,'"', " --scenario_out ",'"', dbscenarioName,'"', " --model ",'"', newmodelName,'"', " --model_out ",'"', dbmodelName,'"', "", sep = ''))
	
	# # read reporting files with "historical" periods
	# path.history = "C:/Users/parkinso/Documents"
	# data.nopolicy = data.frame(read.csv(paste(path.history,"CD_Links_R2.3.1_20170906_upload__NoPolicy_V3.csv",sep='/'), stringsAsFactors = FALSE))
	# data.npi = data.frame(read.csv(paste(path.history,"CD_Links_R2.3.1_20170906_upload__NPi_V3.csv",sep='/'), stringsAsFactors = FALSE))

	# # read solution file
	# path.solution = "C:/Users/parkinso/git/message_ix/data_utils/upload"
	# file.solution = paste(dbmodelName, "_", dbscenarioName, ".xlsx", sep = "")
	# data.solution = read.xlsx(paste(path.solution,file.solution,sep='/'), as.data.frame = TRUE, sheetName = 'data')

	# # merge reporting xlsx file with other files with "historical" periods
	# data.merged = inner_join(inner_join(select(data.nopolicy, Region, Variable, X2000, X2005, X2010), select(data.npi, Region, Variable, X2020)), select(data.solution, Model, Scenario, Region, Variable, Unit, X2030, X2040, X2050, X2060, X2070, X2080, X2090, X2100, X2110)) %>% select(Model, Scenario, Region, Variable, Unit, X2000, X2005, X2010, X2020, X2030, X2040, X2050, X2060, X2070, X2080, X2090, X2100, X2110)
	# names(data.merged) =  c( "Model", "Scenario", "Region", "Variable", "Unit", as.character( c(2000, 2005, seq( 2010, 2110, by = 10 ) )) )
	# file.merged = paste( dbmodelName, "_", dbscenarioName, "_merged.xlsx", sep = "" )
	# write.xlsx(data.merged, paste( path.solution, file.merged, sep = '/' ), sheetName = "data", row.names = FALSE)

	#-------------------------------------------------------------------------------

	# upload file to MESSAGEix_WorkDB scenario database
	#database_upload( file.merged, path.solution, user = "parkinso", password = "goodpassword" )
	
	# rm(data.solution,data.npi,data.nopolicy,data.merged)
	gc()
	
	if( scen %in% c('NPi2020_400-con-prim-dir-ncr','NPi2020_1000-con-prim-dir-ncr') ) # additional scenario including water constraints
		{
		newscenarioName0 = newscenarioName
		dbscenarioName0 = dbscenarioName
		
		for( jj in c( 1 ) )
			{
				
			newscenarioName = paste( newscenarioName0, paste('cn',jj,sep=''), sep = '-' )
			dbscenarioName = paste( dbscenarioName0, paste('cn',jj,sep=''), sep = '-' )

			# clone data structure with new scenario name
			ixDS2 = ixDS$clone( new_model = newmodelName, new_scen = newscenarioName, annotation = paste( comment, ' - constrained to not exceed NPiREF-con-prim-dir-ncr', sep = '' ), keep_sol = FALSE )

			# check out
			ixDS2$check_out()
			
			# Bound consumption
			if( jj == 1 ){ rp = 1}else{ rp = 0.75 }
			res = lapply( region, function(rr){ lapply( modelyears, function(yy){ ixDS2$add_par( 'bound_emission', paste(rr,'water_consumption','all',yy,sep='.'), unlist( rp * baseline_consumption.df[ as.character(yy) , rr ] ) , '-' ) } ) } )
			
			# commit scenario to platform and set as default case
			ixDS2$commit(comment)
			ixDS2$set_as_default()

			# run MESSAGE scenario in GAMS and import results in ix platform
			ixDS2$solve(model = "MESSAGE-MACRO", case = dbscenarioName)
			
			ixDS3 = ix_platform$datastructure(model="MESSAGE-GLOBIOM CD-LINKS R2.3.1", scen='SSP2_INDC_NPi2020_400-con-prim-dir-ncr-water-v3-cn1')
			ixDS4 = ixDS3$clone( new_model = "MESSAGE-GLOBIOM CD-LINKS R2.3.1", new_scen = 'SSP2_INDC_NPi2020_400-con-prim-dir-ncr-water-v3-cn2', annotation = '', keep_sol = FALSE )
			ixDS4$check_out()
			ret = ixDS4$add_set( 'type_emission', 'fresh_consumption' ) 
			ret = ixDS4$add_set( 'type_emission', 'water_consumption' ) 
			ixDS4$commit('')
			ixDS4$solve(model = "MESSAGE-MACRO", case =  'SSP2_INDC_NPi2020_400-con-prim-dir-ncr-water-v3-cn2')
			ixDS4$set_as_default()
			
			#-------------------------------------------------------------------------------
			
			# # start Python-based reporting script
			# system( paste( "C:/Users/parkinso/AppData/Local/Continuum/Anaconda2/python.exe C:/Users/parkinso/git/message_ix/ixmp/reporting/iamc_report.py --scenario ",'"', newscenarioName,'"', " --scenario_out ",'"', dbscenarioName,'"', " --model ",'"', newmodelName,'"', " --model_out ",'"', dbmodelName,'"', "", sep = ''))

			# # read reporting files with "historical" periods
			# path.history = "C:/Users/parkinso/Documents"
			# data.nopolicy = data.frame(read.csv(paste(path.history,"CD_Links_R2.3.1_20170906_upload__NoPolicy_V3.csv",sep='/'), stringsAsFactors = FALSE))
			# data.npi = data.frame(read.csv(paste(path.history,"CD_Links_R2.3.1_20170906_upload__NPi_V3.csv",sep='/'), stringsAsFactors = FALSE))

			# # read solution file
			# path.solution = "C:/Users/parkinso/git/message_ix/data_utils/upload"
			# file.solution = paste(dbmodelName, "_", dbscenarioName, ".xlsx", sep = "")
			# data.solution = read.xlsx(paste(path.solution,file.solution,sep='/'), as.data.frame = TRUE, sheetName = 'data')

			# # merge reporting xlsx file with other files with "historical" periods
			# data.merged = inner_join(inner_join(select(data.nopolicy, Region, Variable, X2000, X2005, X2010), select(data.npi, Region, Variable, X2020)), select(data.solution, Model, Scenario, Region, Variable, Unit, X2030, X2040, X2050, X2060, X2070, X2080, X2090, X2100, X2110)) %>% select(Model, Scenario, Region, Variable, Unit, X2000, X2005, X2010, X2020, X2030, X2040, X2050, X2060, X2070, X2080, X2090, X2100, X2110)
			# names(data.merged) = c( "Model", "Scenario", "Region", "Variable", "Unit", as.character( c(2000, 2005, seq( 2010, 2110, by = 10 ) )) )
			# file.merged = paste( dbmodelName, "_", dbscenarioName, "_merged.xlsx", sep = "" )
			# write.xlsx(data.merged, paste( path.solution, file.merged, sep = '/' ), sheetName = "data", row.names = FALSE)

			# #-------------------------------------------------------------------------------

			# # upload file to MESSAGEix_WorkDB scenario database
			# database_upload( file.merged, path.solution, user = "parkinso", password = "goodpassword" )
			
			# rm(data.solution,data.npi,data.nopolicy,data.merged, ixDS, ixDS2, ixDSoriginal)
			# gc()
			
			}
			
		}else # get the baseline consumption in each region for the No Policy scenario
		{
		
		baseline_consumption.df = do.call( cbind, lapply( region, function(rr){ return( data.frame( sapply( unique( c( 2030, modelyears ) ), function(yy){ 
			if( yy %in% modelyears )
				{
				return( unlist( ixDS$var('EMISS',list( emission = 'fresh_consumption', node = rr, year = yy, type_tec = 'all' ))['level'] ) )
				}else
				{ # 2030 is a historical year for the no policy scenario 
				wci = ixDSbase$par( 'emission_factor', list( emission = 'fresh_consumption', node_loc = rr, year_act = yy ) )
				hist_act = ixDS$par( 'historical_activity', list( node_loc = rr, technology = as.character(unlist(unique(wci['technology']))), year_act = yy ))
				hist_act = hist_act[ which( hist_act$value > 0 ) , ]
				return( sum( sapply( 1:nrow( hist_act ), function(ii){ return( unlist( hist_act$value[ii] ) * unlist( mean( wci$value[ which( wci$technology == hist_act$technology[ii] ) ] ) ) ) } ) ) )
				}
			} ) ) ) } ) )
		names(baseline_consumption.df) = region
		row.names(baseline_consumption.df) = unique( c( 2030, modelyears ) )
		write.csv(baseline_consumption.df,"C:/Users/parkinso/Documents/baseline_consumption.csv", row.names=TRUE)
		
		rm( ixDS, ixDSoriginal )
		gc()
		
		}
	
	}

## Upoad to the db1

## Set path ixmp folder in message_ix working copy
message_ix_path = Sys.getenv("MESSAGE_IX_PATH")

# load packages and launch the IX modeling platform
source(file.path(Sys.getenv("IXMP_R_PATH"), "ixmp.R"))

# launch the IX modeling platform using the default central ORCALE database
ix_platform = ixPlatform()

# Load other packages
require(RCurl)
require(magrittr)
require(dplyr)
require(xlsx)
memory.limit(size=1e9)

#-------------------------------------------------------------------------------
# functions to upload file to MESSAGEix_WorkDB

database_upload = function(filename, path, user = "parkinso", password = "goodpassword", verbose = FALSE) {
  
  # set file location
  aFile = paste(path,filename,sep='/')
  
  # setup curl options
  curl = getCurlHandle()
  curlSetOpt(cookiejar="",  useragent = "Mozilla/5.0", verbose = verbose, followlocation = TRUE, curl = curl)
  
  # login
  loginUrl = paste("db1.ene.iiasa.ac.at/MESSAGEix_WorkDB/dsd?Action=loginform&usr=", user, "&pwd=", password, sep = '')
  html.response1 = getURLContent(loginUrl, curl = curl)
  if (verbose) write(rawToChar(html.response1), file = 'html_response1.html')
  
  # file upload
  uploadUrl = "db1.ene.iiasa.ac.at/MESSAGEix_WorkDB/dsd?Action=uploadFile"
  html.reponse2 = postForm(uploadUrl, curl = curl, "name" = "file1", "filedata" = fileUpload(aFile))
  if (verbose) write( html.response2, file = 'html_response2.html')

  }

#-------------------------------------------------------------------------------

# existing model and scenario names to start with
modelName = "MESSAGE-GLOBIOM CD-LINKS R2.3.1"
scenarioName = "baseline"

# new model and scenario name for ix platform
newmodelName = "MESSAGE-GLOBIOM CD-LINKS R2.3.1"
newscenarioName = paste("SSP2_INDC","baseline-water-v3",sep='_')

# model and scenario names for database
dbmodelName = "SSP_INDC_CD-Links_R2.3.1"
upath = "C:/Users/parkinso/git/message_ix/model/output/"
scen = c('SSP2_INDC_NPiREF-con-prim-dir-ncr-water-v3','SSP2_INDC_NPi2020_400-con-prim-dir-ncr-water-v3','SSP2_INDC_NPi2020_400-con-prim-dir-ncr-water-v3-cn1','SSP2_INDC_NPi2020_1000-con-prim-dir-ncr-water-v3','SSP2_INDC_NPi2020_1000-con-prim-dir-ncr-water-v3-cn1')

ret = lapply( scen, function(sc){
	
	print(sc)
	
	# start Python-based reporting script
	system( paste( "C:/Users/parkinso/AppData/Local/Continuum/Anaconda2/python.exe C:/Users/parkinso/git/message_ix/ixmp/reporting/iamc_report.py --scenario ",'"', sc,'"', " --scenario_out ",'"', sc,'"', " --model ",'"', newmodelName,'"', " --model_out ",'"', dbmodelName,'"', "", sep = ''))

	# read reporting files with "historical" periods
	path.history = "C:/Users/parkinso/Documents"
	data.nopolicy = data.frame(read.csv(paste(path.history,"CD_Links_R2.3.1_20170906_upload__NoPolicy_V3.csv",sep='/'), stringsAsFactors = FALSE))
	data.npi = data.frame(read.csv(paste(path.history,"CD_Links_R2.3.1_20170906_upload__NPi_V3.csv",sep='/'), stringsAsFactors = FALSE))
	
	unique(data.nopolicy$Variable)[ 'ACT' ]
	
	# read solution file
	path.solution = "C:/Users/parkinso/git/message_ix/data_utils/upload"
	file.solution = paste(dbmodelName, "_", sc, ".xlsx", sep = "")
	data.solution = read.xlsx(paste(path.solution,file.solution,sep='/'), as.data.frame = TRUE, sheetName = 'data')
	
	data.nopolicy2 = data.nopolicy[ -1*which( grepl('Water',data.nopolicy$Variable) ),  ]
	data.npi2 = data.npi[ -1*which( grepl('Water',data.npi$Variable) ),  ]
	data.solution2 = data.solution[ -1*which( grepl('Water',data.solution$Variable) ),  ]
	y2c = c( names(data.nopolicy2)[grepl('X2',names(data.nopolicy2))], names(data.solution2)[grepl('X2',names(data.solution2))] )
	y2c = unique( as.numeric( unlist(strsplit(y2c,'X'))[seq(2,length(unlist(strsplit(y2c,'X'))),by=2)] ) )
		
	# Scrape the data from the gdx files
	require(gdxrrw)
	igdx( 'C:/GAMS/win64/24.8' )
	baseline_consumption.df = data.frame( read.csv('C:/Users/parkinso/Documents/baseline_consumption.csv',stringsAsFactors=FALSE,row.names=1) )
	upath = "C:/Users/parkinso/git/message_ix/model/output/"
	res.list = lapply( sc, function(fpath){
		vars = c( 'ACT', 'historical_activity', 'EMISS', 'emission_factor') 
		gdx_res = lapply( vars, function( vv ){
			tmp = rgdx( paste( upath, paste('MSGoutput',fpath,sep='_'), sep = '' ), list( name = vv, form = "sparse" ) )
			names(tmp$uels) = tmp$domains
			rs = data.frame( tmp$val )
			names(rs) = c( unlist( tmp$domains ), 'val' )
			rs[ , which( names(rs) != 'val' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'val' ) ], function( cc ){ sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) } ) )
			return(rs)
			} )
		names(gdx_res) = vars	
		return(gdx_res)
		})
	names(res.list) = sc	
	
	regional_consumption.list = lapply( sc, function( sc ){ 
		dat.df = data.frame( res.list[[ sc ]][[ 'EMISS' ]][ which( as.character( unlist( res.list[[ sc ]][[ 'EMISS' ]][ 'emission' ] ) ) == 'fresh_consumption' & as.character( unlist( res.list[[ sc ]][[ 'EMISS' ]][ 'type_tec' ] ) ) == 'all' ), ] )
		res.df = do.call( cbind, lapply( names( baseline_consumption.df ), function( region ){ data.frame( sapply( as.character( y2c ) , function( year ){ 
			if( year %in% dat.df$year_all )
				{
				return( unlist( dat.df[ which( as.character( dat.df$year_all ) == year & as.character( dat.df$node ) == region ), 'val' ] ) )
				}else
				{
				emf = res.list[[ sc ]][[ 'emission_factor' ]][ which( as.character( unlist( res.list[[ sc ]][[ 'emission_factor' ]][ 'emission' ] ) ) == 'fresh_consumption' & as.character( unlist( res.list[[ sc ]][[ 'emission_factor' ]][ 'year_all' ] ) ) == year & as.character( unlist( res.list[[ sc ]][[ 'emission_factor' ]][ 'node' ] ) ) == region ) , ]
				act = res.list[[ sc ]][[ 'historical_activity' ]][ which( as.character( unlist( res.list[[ sc ]][[ 'historical_activity' ]][ 'tec' ] ) ) %in% as.character(unique(emf$tec)) & as.character( unlist( res.list[[ sc ]][[ 'historical_activity' ]][ 'year_all' ] ) ) == year & as.character( unlist( res.list[[ sc ]][[ 'historical_activity' ]][ 'node' ] ) ) == region ) , ]
				con = sum( sapply( 1:nrow(act), function(ii){ return( act$val[ ii ] * mean(emf$val[ which( as.character(emf$tec) == as.character( act$tec[ ii ] ) ) ]) ) } ) )
				return( con )
				}
			} ) ) } ) ) 
		names(res.df) = names( baseline_consumption.df )
		res.df$World = rowSums( as.matrix( res.df ) )
		row.names( res.df ) = as.character( y2c )
		return(res.df)
		} )
	names(regional_consumption.list) = sc	
	
	regional_withdrawal.list = lapply( sc, function( sc ){ 
		dat.df = data.frame( res.list[[ sc ]][[ 'ACT' ]][ which( as.character( unlist( res.list[[ sc ]][[ 'ACT' ]][ 'tec' ] ) ) == 'extract__freshwater_supply' ), ] )
		dat_hist.df = data.frame( res.list[[ sc ]][[ 'historical_activity' ]][ which( as.character( unlist( res.list[[ sc ]][[ 'historical_activity' ]][ 'tec' ] ) ) == 'extract__freshwater_supply' ), ] )
		res.df =  do.call( cbind, lapply( names( baseline_consumption.df ), function( region ){ data.frame( sapply( as.character( y2c ) , function( year ){ 
			if( year %in% dat.df$year_all ){return(dat.df[ which( dat.df$node == region & dat.df$year_all == year ) , 'val'])}else{return(dat_hist.df[ which( dat_hist.df$node == region & dat_hist.df$year_all == year ) , 'val'])}
			} ) ) } ) )
		names(res.df) = names( baseline_consumption.df )
		res.df$World = rowSums(as.matrix(res.df))
		row.names( res.df ) = as.character( y2c )
		return(res.df)
		} )	
	names(regional_withdrawal.list) = sc
	
	# # merge reporting xlsx file with other files with "historical" periods
	data.merged = inner_join(inner_join(select(data.nopolicy2, Region, Variable, X2000, X2005, X2010), select(data.npi2, Region, Variable, X2020)), select(data.solution2, Model, Scenario, Region, Variable, Unit, X2030, X2040, X2050, X2060, X2070, X2080, X2090, X2100, X2110)) %>% select(Model, Scenario, Region, Variable, Unit, X2000, X2005, X2010, X2020, X2030, X2040, X2050, X2060, X2070, X2080, X2090, X2100, X2110)
	names(data.merged) = c( "Model", "Scenario", "Region", "Variable", "Unit", as.character( c(2000, 2005, seq( 2010, 2110, by = 10 ) )) )
	
	wwdat.df = do.call( rbind, lapply( c(names( baseline_consumption.df ),'World'), function(rrr){
		if( rrr == 'World' ){rfsa='World'}else{rfsa=unlist(strsplit(rrr,'_'))[2]}
		df = cbind( data.frame(  Model = unique(data.merged$Model), Scenario = unique(data.merged$Scenario), Region = rfsa, Variable = 'Water Withdrawal', Unit = 'km3/yr'), data.frame( as.matrix( t( regional_withdrawal.list[[ sc ]][ as.character(y2c), rrr ] ) ) ) )
		names(df) = c( "Model", "Scenario", "Region", "Variable", "Unit", as.character( c(2000, 2005, seq( 2010, 2110, by = 10 ) )) )
		return(df)
		} ) )
	wcdat.df = do.call( rbind, lapply( c(names( baseline_consumption.df ),'World'), function(rrr){
		if( rrr == 'World' ){rfsa='World'}else{rfsa=unlist(strsplit(rrr,'_'))[2]}
		df = cbind( data.frame(  Model = unique(data.merged$Model), Scenario = unique(data.merged$Scenario), Region = rfsa, Variable = 'Water Consumption', Unit = 'km3/yr'), data.frame( as.matrix( t( regional_consumption.list[[ sc ]][ as.character(y2c), rrr ] ) ) ) )
		names(df) = c( "Model", "Scenario", "Region", "Variable", "Unit", as.character( c(2000, 2005, seq( 2010, 2110, by = 10 ) )) )
		return(df)
		} ) )	
	
	data.merged2 = rbind( data.merged, rbind( wwdat.df, wcdat.df ) )
	
	file.merged = paste( dbmodelName, "_", sc, "_merged.xlsx", sep = "" )
	
	write.xlsx(data.merged2, paste( path.solution, file.merged, sep = '/' ), sheetName = "data", row.names = FALSE)

	#-------------------------------------------------------------------------------

	# upload file to MESSAGEix_WorkDB scenario database
	database_upload( file.merged, path.solution, user = "parkinso", password = "goodpassword" )
	
	rm(data.merged, data.merged2)
	
	gc()
	
	} )

	
## Diagnostics

# Scrape the data from the gdx files
require(gdxrrw)
igdx( 'C:/GAMS/win64/24.8' )
baseline_consumption.df = data.frame( read.csv('C:/Users/parkinso/Documents/baseline_consumption.csv',stringsAsFactors=FALSE,row.names=1) )
upath = "C:/Users/parkinso/git/message_ix/model/output/"
scen = c('MSGoutput_SSP2_INDC_NPiREF-con-prim-dir-ncr-water-v3','MSGoutput_SSP2_INDC_NPi2020_400-con-prim-dir-ncr-water-v3','MSGoutput_SSP2_INDC_NPi2020_400-con-prim-dir-ncr-water-v3-cn1','MSGoutput_SSP2_INDC_NPi2020_1000-con-prim-dir-ncr-water-v3','MSGoutput_SSP2_INDC_NPi2020_1000-con-prim-dir-ncr-water-v3-cn1')
res.list = lapply( scen, function(fpath){
	vars = c( 'ACT', 'historical_activity', 'CAP', 'CAP_NEW', 'EMISS', 'emission_factor', 'PRICE_EMISSION' ) 
	gdx_res = lapply( vars, function( vv ){
		tmp = rgdx( paste( upath, fpath, sep = '' ), list( name = vv, form = "sparse" ) )
		names(tmp$uels) = tmp$domains
		rs = data.frame( tmp$val )
		names(rs) = c( unlist( tmp$domains ), 'val' )
		rs[ , which( names(rs) != 'val' ) ] = do.call( cbind, lapply( names( rs )[ which( names(rs) != 'val' ) ], function( cc ){ sapply( unlist( rs[ , cc ] ) , function(ii){ return( tmp$uels[[ cc ]][ ii ] ) } ) } ) )
		return(rs)
		} )
	names(gdx_res) = vars	
	return(gdx_res)
	})
names(res.list) = scen	

cat_tec.list = lapply( scen, function(fpath){ return( data.frame( rgdx.set( paste( upath, fpath, sep = '' ), 'cat_tec' ) ) ) } )
names(cat_tec.list) = scen

### Regional consumption pathways

	regional_consumption.list = lapply( scen, function( sc ){ 
		dat.df = data.frame( res.list[[ sc ]][[ 'EMISS' ]][ which( as.character( unlist( res.list[[ sc ]][[ 'EMISS' ]][ 'emission' ] ) ) == 'fresh_consumption' & as.character( unlist( res.list[[ sc ]][[ 'EMISS' ]][ 'type_tec' ] ) ) == 'all' ), ] )
		res.df = do.call( cbind, lapply( names( baseline_consumption.df ), function( region ){ data.frame( sapply( as.character( seq(2010,2100,by=10) ) , function( year ){ 
			if( year %in% dat.df$year_all )
				{
				return( unlist( dat.df[ which( as.character( dat.df$year_all ) == year & as.character( dat.df$node ) == region ), 'val' ] ) )
				}else
				{
				emf = res.list[[ sc ]][[ 'emission_factor' ]][ which( as.character( unlist( res.list[[ sc ]][[ 'emission_factor' ]][ 'emission' ] ) ) == 'fresh_consumption' & as.character( unlist( res.list[[ sc ]][[ 'emission_factor' ]][ 'year_all' ] ) ) == year & as.character( unlist( res.list[[ sc ]][[ 'emission_factor' ]][ 'node' ] ) ) == region ) , ]
				act = res.list[[ sc ]][[ 'historical_activity' ]][ which( as.character( unlist( res.list[[ sc ]][[ 'historical_activity' ]][ 'tec' ] ) ) %in% as.character(unique(emf$tec)) & as.character( unlist( res.list[[ sc ]][[ 'historical_activity' ]][ 'year_all' ] ) ) == year & as.character( unlist( res.list[[ sc ]][[ 'historical_activity' ]][ 'node' ] ) ) == region ) , ]
				con = sum( sapply( 1:nrow(act), function(ii){ return( act$val[ ii ] * mean(emf$val[ which( as.character(emf$tec) == as.character( act$tec[ ii ] ) ) ]) ) } ) )
				return( con )
				}
			} ) ) } ) ) 
		names(res.df) = names( baseline_consumption.df )
		res.df$World = rowSums( as.matrix( res.df ) )
		row.names( res.df ) = as.character( seq(2010,2100,by=10) )
		return(res.df)
		} )
	names(regional_consumption.list) = scen	
	nm = data.frame( sapply( names(regional_consumption.list[[1]]), function(n){ if( length( unlist( strsplit( n, '_' ) ) ) > 1 ){return( unlist( strsplit( n, '_' ) )[2] )}else{return( n )} } ) )
	names(nm) = 'nm'

	# Multi plot 4 rows by 3 collumns to represent each macro-region plus the global aggregate
	nsc = data.frame(nsc=c('National Policies', '1.5 degrees C', '1.5 degrees C + water constraint', '2 degrees C', '2 degrees C + water constraint' ))
	row.names(nsc) = scen
	pdf( 'C:/Users/parkinso/Documents/regional_water_consumption.pdf', width = 7, height = 10 )
	p1 = layout(rbind( c(16,13,13,13), cbind(rep(14,4),matrix(c(seq(1,12,by=1)),4,3,byrow=TRUE)), c(17,15,15,15) ),widths=c(0.05,0.28,0.31,0.31),heights=c(0.1,rep(0.21,3),0.19,0.05))
	ltys = c(1,1,2,1,4)
	lwds = c(2,2,2,2,2)
	cols = c('red','navy','navy','cyan','cyan')
	par(mar=c(2,0,2,2), oma = c(1,2,1,1))
	for( reg in names( regional_consumption.list[[1]] ) )
		{
		ii = which( names( regional_consumption.list[[1]] ) == reg )
		if( ii > 1 ){ if( ii < 10 ){ if( (ii-1)%%3 == 0 ){ par(mar=c(2,0,2,2)) }else{ par(mar=c(2,2,2,2)) } }else{ if( (ii-1)%%3 == 0 ){ par(mar=c(0,0,2,2)) }else{ par(mar=c(0,2,2,2)) } } }	
		matplot( seq(2010, 2050, by = 10), as.matrix( do.call( cbind, lapply( scen, function(sc){ return( data.frame( regional_consumption.list[[ sc ]][ c(as.character(seq(2010, 2050, by = 10))) , reg ] ) ) } ) ) ), type = 'l', col = cols, lty = ltys, lwd = lwds, main = as.character(nm[reg,]), xlab = '', ylab = '' ) 
		}
	par(mar=c(0,0,0,0))
	plot.new() # legend
	legend('left',legend = c('National Policies',  expression("1.5"~degree*"C"),  expression("1.5"~degree*"C + water constraint"), expression("2"~degree*"C"),  expression("2"~degree*"C + water constraint") ),seg.len=4,lty=ltys,lwd=lwds*1.2,ncol=2,col = cols,y.intersp=1.1,bty='n',cex=1.1,title = expression(bold("Energy System Transformation Pathway")),title.col="black",title.adj=0)
	plot.new() # y axis label
	mtext(expression('Freshwater Consumption [ '*km^3*' ]'), side = 2, line = 0)
	plot.new() # x axis label
	mtext(expression('Year'), side = 1, line = 0)
	plot.new() # fill corners
	plot.new() # fill corners
	dev.off()


### Regional water price
scen2 = c('MSGoutput_SSP2_INDC_NPi2020_400-con-prim-dir-ncr-water-v3-cn1','MSGoutput_SSP2_INDC_NPi2020_1000-con-prim-dir-ncr-water-v3-cn1')
	regional_price.list = lapply( scen2, function( sc ){ 
		dat.df = data.frame( res.list[[ sc ]][[ 'PRICE_EMISSION' ]][ which( as.character( unlist( res.list[[ sc ]][[ 'PRICE_EMISSION' ]][ 'emission' ] ) ) == 'fresh_consumption' ), ] )
		res.df = do.call( cbind, lapply( names( baseline_consumption.df ), function( region ){ data.frame( sapply( as.character( seq(2030,2100,by=10) ) , function( year ){ return( unlist( abs(min(0,dat.df[ which( as.character( dat.df$year_all ) == year & as.character( dat.df$node ) == region ), 'val' ],na.rm=TRUE))/1e3 ) ) } ) ) } ) ) 
		names(res.df) = names( baseline_consumption.df )
		row.names( res.df ) = as.character( seq(2030,2100,by=10) )
		return(res.df)
		} )
	names(regional_price.list) = scen2	
	nm2 = data.frame( sapply( names(regional_price.list[[1]]), function(n){ if( length( unlist( strsplit( n, '_' ) ) ) > 1 ){return( unlist( strsplit( n, '_' ) )[2] )}else{return( n )} } ) )
	names(nm2) = 'nm'
scen3 = c('MSGoutput_SSP2_INDC_NPi2020_400-con-prim-dir-ncr-water-v3','MSGoutput_SSP2_INDC_NPi2020_400-con-prim-dir-ncr-water-v3-cn1','MSGoutput_SSP2_INDC_NPi2020_1000-con-prim-dir-ncr-water-v3','MSGoutput_SSP2_INDC_NPi2020_1000-con-prim-dir-ncr-water-v3-cn1')	
	carbon_price.list = lapply( scen3, function( sc ){ 
		dat.df = data.frame( res.list[[ sc ]][[ 'PRICE_EMISSION' ]][ which( as.character( unlist( res.list[[ sc ]][[ 'PRICE_EMISSION' ]][ 'emission' ] ) ) == 'TCE' & as.character( unlist( res.list[[ sc ]][[ 'PRICE_EMISSION' ]][ 'node' ] ) ) == 'World' ), ] )
		res.df = do.call( cbind, lapply( 'World', function( region ){ data.frame( sapply( as.character( seq(2030,2100,by=10) ) , function( year ){ return( unlist( abs(min(0,dat.df[ which( as.character( dat.df$year_all ) == year & as.character( dat.df$node ) == region ), 'val' ],na.rm=TRUE))/1e3 ) ) } ) ) } ) ) 
		names(res.df) = 'World'
		row.names( res.df ) = as.character( seq(2030,2100,by=10) )
		return(res.df)
		} )
	names(carbon_price.list) = scen3	
	nm3 = data.frame( sapply( names(carbon_price.list[[1]]), function(n){ if( length( unlist( strsplit( n, '_' ) ) ) > 1 ){return( unlist( strsplit( n, '_' ) )[2] )}else{return( n )} } ) )
	names(nm3) = 'nm'
	
# Multi plot 4 rows by 3 collumns to represent each macro-region plus the global aggregate
	pdf( 'C:/Users/parkinso/Documents/regional_price_impact.pdf', width = 7, height = 10 )
	p1 = layout(rbind( c(16,13,13,13), cbind(rep(14,4),matrix(c(seq(1,12,by=1)),4,3,byrow=TRUE)), c(17,15,15,15) ),widths=c(0.05,0.28,0.31,0.31),heights=c(0.1,rep(0.21,3),0.19,0.05))
	ltys = c(1,1)
	lwds = c(2,2)
	cols = c('navy','cyan')
	par(mar=c(2,0,2,2), oma = c(1,2,1,1))
	for( reg in names( regional_price.list[[1]] ) )
		{
		ii = which( names( regional_price.list[[1]] ) == reg )
		if( ii > 1 ){ if( ii < 10 ){ if( (ii-1)%%3 == 0 ){ par(mar=c(2,0,2,2)) }else{ par(mar=c(2,2,2,2)) } }else{ if( (ii-1)%%3 == 0 ){ par(mar=c(0,0,2,2)) }else{ par(mar=c(0,2,2,2)) } } }	
		matplot( seq(2030, 2090, by = 10), as.matrix( do.call( cbind, lapply( scen2, function(sc){ return( data.frame( regional_price.list[[ sc ]][ c(as.character(seq(2030, 2090, by = 10))) , reg ] ) ) } ) ) ), type = 'l', col = cols, lty = ltys, lwd = lwds, main = as.character(nm2[reg,]), xlab = '', ylab = '' ) 
		}
	plot.new()
	#par(mar=c(0,5,2,2))
	#matplot( seq(2030, 2090, by = 10), as.matrix( do.call( cbind, lapply( scen3, function(sc){ return( data.frame( carbon_price.list[[ sc ]][ c(as.character(seq(2030, 2090, by = 10))) ,  ] ) ) } ) ) ), type = 'l', col = c(cols[ 1 ],cols[ 1 ],cols[ 2 ],cols[ 2 ]), lty = c(2,1,2,1), lwd = 2, main = 'World', xlab = '', ylab = 'Carbon Price' ) 
	par(mar=c(0,0,0,0))
	plot.new() # legend
	legend('left',legend = c( expression("1.5"~degree*"C + water constraint"), expression("2"~degree*"C + water constraint") ),seg.len=4,lty=c(1,1),lwd=lwds*1.2,ncol=1,col = cols,y.intersp=1.1,bty='n',cex=1.1,title = expression(bold("Energy System Transformation Pathway")),title.col="black",title.adj=0)
	plot.new() # y axis label
	mtext(expression('Freshwater Consumption Price [ USD per '*m^3*' ]'), side = 2, line = 0)
	plot.new() # x axis label
	mtext(expression('Year'), side = 1, line = 0)
	plot.new() # fill corners
	plot.new() # fill corners
	dev.off()

pdf( 'C:/Users/parkinso/Documents/carbon_price_impact.pdf', width = 6, height = 6 )
matplot( seq(2030, 2090, by = 10), (1/max(as.matrix( do.call( cbind, lapply( scen3, function(sc){ return( data.frame( carbon_price.list[[ sc ]][ c(as.character(seq(2030, 2090, by = 10))) ,  ] ) ) } ) ) )))*as.matrix( do.call( cbind, lapply( scen3, function(sc){ return( data.frame( carbon_price.list[[ sc ]][ c(as.character(seq(2030, 2090, by = 10))) ,  ] ) ) } ) ) ), type = 'l', col = c(cols[ 1 ],cols[ 1 ],cols[ 2 ],cols[ 2 ]), lty = c(1,2,1,2), lwd = 2, xlab = 'Year', ylab = 'Normalized Global CO2 Price' ) 
legend('topleft',legend = c( expression("1.5"~degree*"C"), expression("1.5"~degree*"C + water constraint"), expression("2"~degree*"C"), expression("2"~degree*"C + water constraint") ),seg.len=4,lty=c(1,2,1,2),lwd=2,ncol=1,col = c(cols[ 1 ],cols[ 1 ],cols[ 2 ],cols[ 2 ]),y.intersp=1.1,bty='n',cex=0.95)
dev.off()		
	
### Regional power generation by fuel type in 2070
	ptech = c( unique( c( do.call( rbind, lapply( scen, function(sc){ return( unique( as.character( unlist( cat_tec.list[[ sc ]][ which( as.character( unlist( cat_tec.list[[ sc ]]['type_tec'] ) ) %in% c( 'powerplant_fossil', 'powerplant_low-carbon' ) ), 'tec' ] ) ) ) ) } ) ) ) ),'igcc')
	#fuels = unique( sapply( ptech, function(pt){ unlist(strsplit(pt,'_'))[1] } ) )
	fuels = c('coal','oil','gas','bio','solar','geo','hydro','nuc','wind')
	fl2.df = data.frame( coal = 'coal', oil = 'oil', gas = 'gas', biomass = 'bio', solar = 'solar', geothermal = 'geo', hydro = 'hydro', nuclear = 'nuc', wind = 'wind')
	regional_power_generation_by_fuel_type.list = lapply( scen, function( sc ){ 
		dat.df = data.frame( res.list[[ sc ]][[ 'ACT' ]][ which( as.character( unlist( res.list[[ sc ]][[ 'ACT' ]][ 'year_all' ] ) ) == 2050 & as.character( unlist( res.list[[ sc ]][[ 'ACT' ]][ 'tec' ] ) ) %in% ptech ), ] )
		res.df = do.call( cbind, lapply( names( baseline_consumption.df ), function( region ){ data.frame( sapply( fuels, function( fl ){ 
			nms = fl
			if( fl == 'coal' ){nms = c('coal','igcc')}
			if( fl == 'solar' ){nms = c('solar','csp')}
			if( fl == 'oil' ){nms = c('oil','foil','loil')}
			ftech = ptech[ which( sapply( ptech, function(pt){ unlist(strsplit(pt,'_'))[1] } ) %in% nms & !( grepl('co2scr',sapply( ptech, function(pt){ unlist(strsplit(pt,'_'))[2] } )) ) ) ]
			return( sum( dat.df[ which( dat.df$tec %in% ftech & dat.df$node == region ) , 'val' ] )/1000 )
			} ) ) } ) ) 
		names(res.df) = names( baseline_consumption.df )
		res.df$World = rowSums( as.matrix( res.df ) )
		return(res.df)
		} )
	names(regional_power_generation_by_fuel_type.list) = scen	
	nm = data.frame( sapply( names(regional_power_generation_by_fuel_type.list[[1]]), function(n){ if( length( unlist( strsplit( n, '_' ) ) ) > 1 ){return( unlist( strsplit( n, '_' ) )[2] )}else{return( n )} } ) )
	names(nm) = 'nm'
	
	# Multi plot 4 rows by 3 collumns to represent each macro-region plus the global aggregate
	pdf( 'C:/Users/parkinso/Documents/regional_power_generation_by_fuel_type.pdf', width = 7, height = 10 )
	p1 = layout(rbind( c(16,13,13,18), cbind(rep(14,4),matrix(c(seq(1,12,by=1)),4,3,byrow=TRUE)), c(17,15,15,15) ),widths=c(0.05,0.28,0.31,0.31),heights=c(0.09,rep(0.195,3),0.165,0.05))
	colb = c('brown','blue','red','green','lightgoldenrod','purple','deepskyblue','forestgreen','lavenderblush3')
	dns = c(25,NA,NA,20,NA,30,25,NA,35)
	ang = c(110,NA,NA,110,NA,60,25,NA,160)
	par(mar=c(3,0,2,2), oma = c(2,2,1,1))
	for( reg in names( regional_power_generation_by_fuel_type.list[[1]] ) )
		{
		ii = which( names( regional_power_generation_by_fuel_type.list[[1]] ) == reg )
		if( ii > 1 ){ if( ii < 10 ){ if( (ii-1)%%3 == 0 ){ par(mar=c(3,0,2,2)) }else{ par(mar=c(3,2,2,2)) } }else{ if( (ii-1)%%3 == 0 ){ par(mar=c(0,0,2,2)) }else{ par(mar=c(0,2,2,2)) } } }	
		if( ii >= 10 ){ dfd = c('(A)','(B)','(C)','(D)','(E)') }else{ dfd = c('(A)','(B)','(C)','(D)','(E)') }
		rst = do.call( cbind, lapply( scen, function(sc){ return( data.frame( regional_power_generation_by_fuel_type.list[[ sc ]][ , reg ] ) ) } ) )
		row.names(rst) = row.names(regional_power_generation_by_fuel_type.list[[ sc ]])
		barplot( ( as.matrix( rst )* 31.536 ) , col = colb , density = dns, angle = ang, names.arg = dfd, main = as.character(nm[reg,]) )
		abline(h=0)
		}
	par(mar=c(0,0,0,0))
	plot.new() # legend
	legend('left',legend = c('(A) National Policies',  expression("(B) 1.5"~degree*"C"),  expression("(C) 1.5"~degree*"C + water constraint"), expression("(D) 2"~degree*"C"),  expression("(E) 2"~degree*"C + water constraint") ),ncol=2, seg.len=NA, y.intersp=1.25,bty='n',cex=1.05,title = expression(bold("Energy System Transformation Pathway")),title.col="black",title.adj=0)
	plot.new() # y axis label
	mtext(expression('Power Generation [ EJ ]'), side = 2, line = 0)
	par(mar=c(0,0,0,2))
	plot.new() # x axis label
	mtext(expression('Energy System Transformation Pathway'), side = 1, line = 0)
	plot.new() # fill corners
	plot.new() # fill corners
	plot.new() # legend
	legend('left',legend = as.character(sapply( fuels, function(fff){ unlist( names(fl2.df)[which( as.character( unlist(fl2.df) ) == fff )] ) } )), fill = colb, bty='n',cex=1.05,,ncol=2,angle=ang,density=dns,title = expression(bold("Fuel")),title.col="black",title.adj=0)
	dev.off()
	
### Regional power generation by cooling type in 2070
	
	sc = scen[ 1 ]
	ot_fresh = unique( as.character( unlist( cat_tec.list[[ sc ]]['tec'] ) )[ grepl('__ot_fresh',as.character( unlist( cat_tec.list[[ sc ]]['tec'] ) )) ] )
	ot_saline = unique( as.character( unlist( cat_tec.list[[ sc ]]['tec'] ) )[ grepl('__ot_saline',as.character( unlist( cat_tec.list[[ sc ]]['tec'] ) )) ] )
	cl_fresh = unique( as.character( unlist( cat_tec.list[[ sc ]]['tec'] ) )[ grepl('__cl_fresh',as.character( unlist( cat_tec.list[[ sc ]]['tec'] ) )) ] )	
	air = unique( as.character( unlist( cat_tec.list[[ sc ]]['tec'] ) )[ grepl('__air',as.character( unlist( cat_tec.list[[ sc ]]['tec'] ) )) ]	)
	regional_power_generation_by_cooling_type.list = lapply( scen, function( sc ){ 
		dat.df = data.frame( res.list[[ sc ]][[ 'ACT' ]][ which( as.character( unlist( res.list[[ sc ]][[ 'ACT' ]][ 'year_all' ] ) ) == 2050 & as.character( unlist( res.list[[ sc ]][[ 'ACT' ]][ 'tec' ] ) ) %in% c(ot_fresh,ot_saline,cl_fresh,air) ), ] )
		res.df = do.call( cbind, lapply( names( baseline_consumption.df ), function( region ){ data.frame( sapply( c('ot_fresh','ot_saline','cl_fresh','air'), function( cl ){ 
			ctech = get( cl )
			return( max(0,sum( dat.df[ which( dat.df$tec %in% ctech & dat.df$node == region ) , 'val' ] ),na.rm=TRUE )/1000 )
			} ) ) } ) ) 
		names(res.df) = names( baseline_consumption.df )
		res.df$World = rowSums( as.matrix( res.df ) )
		return(res.df)
		} )
	names(regional_power_generation_by_cooling_type.list) = scen	
	nm = data.frame( sapply( names(regional_power_generation_by_cooling_type.list[[1]]), function(n){ if( length( unlist( strsplit( n, '_' ) ) ) > 1 ){return( unlist( strsplit( n, '_' ) )[2] )}else{return( n )} } ) )
	names(nm) = 'nm'
	
	# Multi plot 4 rows by 3 collumns to represent each macro-region plus the global aggregate
	pdf( 'C:/Users/parkinso/Documents/regional_power_generation_by_cooling_type.pdf', width = 7, height = 10 )
	p1 = layout(rbind( c(16,13,13,18), cbind(rep(14,4),matrix(c(seq(1,12,by=1)),4,3,byrow=TRUE)), c(17,15,15,15) ),widths=c(0.05,0.28,0.31,0.31),heights=c(0.09,rep(0.195,3),0.165,0.05))
	colb = c('gold4','blue','mediumseagreen','tomato')
	dns = c(NA,NA,NA,35)
	ang = c(NA,NA,NA,35)
	par(mar=c(3,0,2,2), oma = c(2,2,1,1))
	for( reg in names( regional_power_generation_by_cooling_type.list[[1]] ) )
		{
		ii = which( names( regional_power_generation_by_cooling_type.list[[1]] ) == reg )
		if( ii > 1 ){ if( ii < 10 ){ if( (ii-1)%%3 == 0 ){ par(mar=c(3,0,2,2)) }else{ par(mar=c(3,2,2,2)) } }else{ if( (ii-1)%%3 == 0 ){ par(mar=c(0,0,2,2)) }else{ par(mar=c(0,2,2,2)) } } }	
		if( ii >= 10 ){ dfd = c('(A)','(B)','(C)','(D)','(E)') }else{ dfd = c('(A)','(B)','(C)','(D)','(E)') }
		rst = do.call( cbind, lapply( scen, function(sc){ return( data.frame( regional_power_generation_by_cooling_type.list[[ sc ]][ , reg ] ) ) } ) )
		row.names(rst) = row.names(regional_power_generation_by_cooling_type.list[[ sc ]])
		names(rst) = nsc[scen,]
		barplot( ( as.matrix( rst ) * 31.536 ) , col = colb , density = dns, angle = ang, names.arg = dfd, main = as.character(nm[reg,]) )
		abline(h=0)
		}
	par(mar=c(0,0,0,0))
	plot.new() # legend
	legend('left',legend =  c('(A) National Policies',  expression("(B) 1.5"~degree*"C"),  expression("(C) 1.5"~degree*"C + water constraint"), expression("(D) 2"~degree*"C"),  expression("(E) 2"~degree*"C + water constraint") ), ncol=2,seg.len=NA, y.intersp=1.25,bty='n',cex=1.05,title = expression(bold("Energy System Transformation Pathway")),title.col="black",title.adj=0)
	plot.new() # y axis label
	mtext(expression('Power Generation [ EJ ]'), side = 2, line = 0)
	plot.new() # x axis label
	par(mar=c(0,0,0,2))
	mtext(expression('Energy System Transformation Pathway'), side = 1, line = 0)
	plot.new() # fill corners
	plot.new() # fill corners
	par(mar=c(0,0,0,0))
	plot.new() # legend
	legend('left',legend = c('Once-through : Freshwater','Once-through : Seawater','Closed-loop : Freshwater','Dry Cooling'), fill = colb, bty='n',cex=1.05,,ncol=1,angle=ang,density=dns,title = expression(bold("Cooling Technology")),title.col="black",title.adj=0)
	plot.new() # y axis label
	dev.off()	

	

