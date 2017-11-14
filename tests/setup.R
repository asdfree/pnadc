if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

pnadc_cat <-
	get_catalog( "pnadc" ,
		output_dir = file.path( getwd() ) )

record_categories <- ceiling( seq( nrow( pnadc_cat ) ) / ceiling( nrow( pnadc_cat ) / 11 ) )

pnadc_cat <- unique( rbind( pnadc_cat[ record_categories == this_sample_break , ] , pnadc_cat[ pnadc_cat$year == 2015 & pnadc_cat$quarter == '03' , ] ) )

lodown( "pnadc" , pnadc_cat )
