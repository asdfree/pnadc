if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
this_sample_break <- Sys.getenv( "this_sample_break" )
pnadc_cat <- get_catalog( "pnadc" , output_dir = file.path( getwd() ) )
record_categories <- ceiling( seq( nrow( pnadc_cat ) ) / ceiling( nrow( pnadc_cat ) / 23 ) )
pnadc_cat <- pnadc_cat[ record_categories == this_sample_break , ]
pnadc_cat <- lodown( "pnadc" , pnadc_cat )
if( any( pnadc_cat$year == 2015 & pnadc_cat$quarter == '03' ) ){











library(survey)

options( survey.lonely.psu = "adjust" )

pnadc_df <- readRDS( file.path( getwd() , "pnadc 2015 03.rds" ) )

# add a column of all ones
pnadc_df$one <- 1

# construct a data.frame object with all state names.
uf <-
 structure(list(V1 = c(11L, 12L, 13L, 14L, 15L, 16L, 17L, 21L, 
 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 31L, 32L, 33L, 35L, 41L, 
 42L, 43L, 50L, 51L, 52L, 53L), V2 = structure(c(22L, 1L, 4L, 
 23L, 14L, 3L, 27L, 10L, 18L, 6L, 20L, 15L, 17L, 2L, 26L, 5L, 
 13L, 8L, 19L, 25L, 16L, 24L, 21L, 12L, 11L, 9L, 7L), .Label = c("Acre", 
 "Alagoas", "Amapa", "Amazonas", "Bahia", "Ceara", "Distrito Federal", 
 "Espirito Santo", "Goias", "Maranhao", "Mato Grosso", "Mato Grosso do Sul", 
 "Minas Gerais", "Para", "Paraiba", "Parana", "Pernambuco", "Piaui", 
 "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", 
 "Rondonia", "Roraima", "Santa Catarina", "Sao Paulo", "Sergipe", 
 "Tocantins"), class = "factor")), .Names = c("uf", "uf_name"), 
		class = "data.frame", row.names = c(NA, -27L))

# merge this data.frame onto the main `x` data.frame
# using `uf` as the merge field, keeping all non-matches.
pnadc_df <- merge( pnadc_df , uf , all.x = TRUE )

# confirm complete matches
stopifnot( all( !is.na( pnadc_df$uf_name ) ) )

# preliminary survey design
pre_stratified <-
	svydesign(
		ids = ~ upa , 
		strata = ~ estrato , 
		weights = ~ v1027 , 
		data = pnadc_df ,
		nest = TRUE
	)
# warning: do not use `pre_stratified` in your analyses!
# you must use the `pnadc_design` object created below.

# post-stratification targets
df_pos <- 
	data.frame( posest = unique( pnadc_df$posest ) , Freq = unique( pnadc_df$v1029 ) )

# final survey design object
pnadc_design <- postStratify( pre_stratified , ~ posest , df_pos )

# remove the `pnadc_df` data.frame object
# and the `pre_stratified` design before stratification
rm( pnadc_df , pre_stratified )
pnadc_design <- 
	update( 
		pnadc_design , 
		age_categories = factor( 1 + findInterval( v2009 , seq( 5 , 60 , 5 ) ) ) ,
		male = as.numeric( v2007 == 1 ) ,
		pia = as.numeric( v2009 >= 14 ) ,
		region = substr( uf , 1 , 1 )
	)
	
pnadc_design <- 
	update( 
		pnadc_design , 
		ocup_c = ifelse( pia == 1 , as.numeric( vd4002 %in% 1 ) , NA ) ,
		desocup30 = ifelse( pia == 1 , as.numeric( vd4002 %in% 2 ) , NA ) ,
		# calculate usual income from main job
		# (rendimento habitual do trabalho principal)
		vd4016n = ifelse( pia %in% 1 & vd4015 %in% 1 , vd4016 , NA ) ,
		# calculate effective income from main job
		# (rendimento efetivo do trabalho principal) 
		vd4017n = ifelse( pia %in% 1 & vd4015 %in% 1 , vd4017 , NA ) ,
		# calculate usual income from all jobs
		# (variavel rendimento habitual de todos os trabalhos)
		vd4019n = ifelse( pia %in% 1 & vd4015 %in% 1 , vd4019 , NA ) ,
		# calculate effective income from all jobs
		# (rendimento efetivo do todos os trabalhos) 
		vd4020n = ifelse( pia %in% 1 & vd4015 %in% 1 , vd4020 , NA ) ,
		# determine individuals who are either working or not working
		# (that is, the potential labor force)
		pea_c = as.numeric( ocup_c == 1 | desocup30 == 1 )
	)
sum( weights( pnadc_design , "sampling" ) != 0 )

svyby( ~ one , ~ uf_name , pnadc_design , unwtd.count )
svytotal( ~ one , pnadc_design )

svyby( ~ one , ~ uf_name , pnadc_design , svytotal )
svymean( ~ vd4020n , pnadc_design , na.rm = TRUE )

svyby( ~ vd4020n , ~ uf_name , pnadc_design , svymean , na.rm = TRUE )
svymean( ~ age_categories , pnadc_design )

svyby( ~ age_categories , ~ uf_name , pnadc_design , svymean )
svytotal( ~ vd4020n , pnadc_design , na.rm = TRUE )

svyby( ~ vd4020n , ~ uf_name , pnadc_design , svytotal , na.rm = TRUE )
svytotal( ~ age_categories , pnadc_design )

svyby( ~ age_categories , ~ uf_name , pnadc_design , svytotal )
svyquantile( ~ vd4020n , pnadc_design , 0.5 , na.rm = TRUE )

svyby( 
	~ vd4020n , 
	~ uf_name , 
	pnadc_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ ocup_c , 
	denominator = ~ pea_c , 
	pnadc_design ,
	na.rm = TRUE
)
sub_pnadc_design <- subset( pnadc_design , desocup30 == 1 )
svymean( ~ vd4020n , sub_pnadc_design , na.rm = TRUE )
this_result <- svymean( ~ vd4020n , pnadc_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ vd4020n , 
		~ uf_name , 
		pnadc_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( pnadc_design )
svyvar( ~ vd4020n , pnadc_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ vd4020n , pnadc_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ vd4020n , pnadc_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ male , pnadc_design ,
	method = "likelihood" )
svyttest( vd4020n ~ male , pnadc_design )
svychisq( 
	~ male + age_categories , 
	pnadc_design 
)
glm_result <- 
	svyglm( 
		vd4020n ~ male + age_categories , 
		pnadc_design 
	)

summary( glm_result )
library(convey)
pnadc_design <- convey_prep( pnadc_design )

sub_pnadc_design <- 
	subset( pnadc_design , pia == 1 )

svygini( ~ vd4020n , sub_pnadc_design , na.rm = TRUE )
nationwide_pop <- 
	svytotal( ~ pia , pnadc_design , na.rm = TRUE )
nationwide_forca <- 
	svytotal( ~ factor( vd4001 ) , pnadc_design , na.rm = TRUE )
nationwide_ocupacao <- 
	svytotal( ~ factor( vd4002 ) , pnadc_design , na.rm = TRUE )
regional_pop <- 
	svyby( ~ pia , ~ region , pnadc_design , svytotal , na.rm = TRUE )
regional_forca <- 
	svyby( ~ factor( vd4001 ) , ~ region , pnadc_design , svytotal , na.rm = TRUE )
regional_ocupacao <- 
	svyby( ~ factor( vd4002 ) , ~ region , pnadc_design , svytotal , na.rm = TRUE )
}
