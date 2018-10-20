library(naniar)

table(trip$SAMPLE_NO, trip$PERSON_NO
      )
table(trip$HH_SA3_NAME, trip$HH_SA2_ID
      )
person_trip_dm$
nonzperson_trip <- (person_trip_dm $tot_trip > 0)
ggplot(person_trip_dm[nonzperson_trip,], aes(AGE,log(tot_trip))) + geom_point() + facet_wrap(~INCOME_UNADJUSTED)
ggplot(person_trip_dm[nonzperson_trip,], aes((AGE),log10(tot_trip))) + geom_point() + facet_wrap(~ADULT_EMP_CATEGORY_NAME)
ggplot(person_trip_dm[nonzperson_trip,], aes((AGE),log10(`Vehicle driver`))) + geom_point() + facet_wrap(~ADULT_EMP_CATEGORY_NAME)
ggplot(person_trip_dm[nonzperson_trip,], aes(AGE,tot_trip, color = ADULT_EMP_CATEGORY_NAME)) + geom_point()

readOGR("/Users/Corinna/Desktop/corinna - temp/1270055001_sa2_2016_aust_shape", layer = "SA2_2016_AUST" )