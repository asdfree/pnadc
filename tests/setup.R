if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)

pnadc_cat <-
	get_catalog( "pnadc" ,
		output_dir = file.path( getwd() ) )

# sample 25% of the records
which_records <- sample( seq( nrow( pnadc_cat ) ) , round( nrow( pnadc_cat ) * 0.25 ) )

# always sample year == 2015
pnadc_cat <- unique( rbind( pnadc_cat[ which_records , ] , subset( pnadc_cat , year == 2015 ) ) )

lodown( "pnadc" , pnadc_cat )
