####
# ilab part C data prep
####
library(lme4)
library(magrittr)
# ======= Person table
fnl_p_review_df <- readRDS("/Users/Corinna/Desktop/corinna - temp/income_imp_review16/fnl_p_review_df.rds")
# ======= trip table
trip <- read.csv("/Users/Corinna/Desktop/corinna - temp/iLAb project/ilab data/hts_hts_trp_odmatrix.csv")

## simple poisson regression
# trips_for_model <- trip %>% dplyr::mutate(count = 1, PERS_ID = paste0(SAMPLE_NO,PERSON_NO), HH_ID = SAMPLE_NO, 
#                                           WAVE = dplyr::if_else(WAVE == 2016, 0,1), month = lubridate::month(as.Date(TRAVEL_DATE,
#                                                                                                           format = "%d/%b/%y"))) %>% 
#   dplyr::select(WAVE, month, TRAVEL_DATE_NUMBER, HH_ID, PERS_ID,  HH_SA2_NAME, CURRENT_MODE_DESC_9, O_SA2_NAME, D_SA2_NAME, count) %>% #AGE,
#   dplyr::group_by(WAVE, month, TRAVEL_DATE_NUMBER, HH_ID, PERS_ID, HH_SA2_NAME, CURRENT_MODE_DESC_9, O_SA2_NAME, D_SA2_NAME)%>% #AGE,
#   dplyr::summarise(TC = sum(count)

trips_for_model <- trip %>% dplyr::filter(WAVE ==2016) %>% dplyr::mutate(count = 1, PERS_ID = paste0(SAMPLE_NO,PERSON_NO), HH_ID = SAMPLE_NO,
                                                                         year = dplyr::if_else(lubridate::year(as.Date(TRAVEL_DATE,
                                                                                                                       format = "%d/%b/%y")) == 2016, 0,1), month = lubridate::month(as.Date(TRAVEL_DATE,
                                                                                                                                                                                             format = "%d/%b/%y"))) %>%
  dplyr::select(year,month, TRAVEL_DATE, TRAVEL_DATE_NUMBER, HH_ID, PERS_ID,  CURRENT_MODE_DESC_9,HH_SA2_NAME, count) %>% 
  dplyr::group_by(year, month,TRAVEL_DATE, TRAVEL_DATE_NUMBER, HH_ID, PERS_ID,  CURRENT_MODE_DESC_9, HH_SA2_NAME)%>% 
  dplyr::summarise(TC = sum(count)) %>% 
  dplyr::left_join(dplyr::select(dplyr::mutate(trip, 
                                               PERS_ID = paste0(SAMPLE_NO,PERSON_NO)),
                                 PERS_ID,DAYFACTOR,PPEX), by = "PERS_ID") %>% 
  dplyr::left_join(dplyr::select(dplyr::mutate(fnl_p_review_df, 
                                               PERS_ID = paste0(SAMPLE_NO,PERSON_NO)),
                                 PERS_ID,AGE, SEX_CODE), by = "PERS_ID") %>% dplyr::filter(!(is.na(AGE))) # need to update person table to include 2017 age/sex

## add on household and are factors
hhsa2 <- rgdal::readOGR("/Users/Corinna/Desktop/corinna - temp/1270055001_sa2_2016_aust_shape", layer = "SA2_2016_AUST")
hts_sa2_map <- subset(hhsa2, SA2_5DIG16 %in% trips_for_model$HH_SA2_NAME)
plot(hts_sa2_map)
# Compute adjacency objects
nbsa2 <- spdep::poly2nb(hts_sa2_map)
grasa2 <- spdep::nb2gra(hts_sa2_map)
nbsa3 <- spdep::poly2nb(hts_sa2_map)
grasa3 <- spdep::nb2gra(hts_sa2_map)
# Fit a Poisson glmer

model1poisson <- glmer(TC ~ AGE + (TRAVEL_DATE_NUMBER-1) + (1|CURRENT_MODE_DESC_9) + (1|PERS_ID) + 
                         (TRAVEL_DATE_NUMBER-1|HH_ID) + (1|HH_SA2_NAME), family = 'poisson', data = trips_for_model
                       ) # weights = (DAYFACTOR*PPEX/7)
plot(model1poisson)
trips_for_model$tripPredictHHsa <-  predict(model1poisson, trips_for_model)
ggplot() + theme_minimal() +
  geom_point(data =trips_for_model,
             aes(x = (DAYFACTOR*PPEX/7), y = TC)) + 
  geom_point(data = trips_for_model,
             aes(x = (DAYFACTOR*PPEX/7), y = tripPredictHHsa),
             color = 'blue', alpha = 0.5) 

ggplot() + theme_minimal() +
  geom_point(data =trips_for_model,
             aes(x = AGE, y = TC, alpha = (DAYFACTOR*PPEX/7))) + 
  geom_point(data = trips_for_model,
             aes(x = AGE, y = tripPredictHHsa),
             color = 'blue', alpha = 0.5) + facet_wrap(~CURRENT_MODE_DESC_9) +geom_hline(yintercept =0)
summary(model1poisson)
# extractAndPlot(model1poisson)
# Age goes before year
# modelOut <- glmer(count ~ age + year + (year|county), family = 'poisson',
#                   data = ILdata)
# summary(modelOut)
modelOut <- model1poisson
# Extract out fixed effects
plot(fixef(modelOut))
# Extract out random effects 
plot(ranef(modelOut))

# Run code to see one method for plotting the data
ggplot(data = trips_for_model, aes(x = (TRAVEL_DATE_NUMBER-1), y = TC, group = HH_SA2_NAME, alpha = (DAYFACTOR*PPEX/7))) +
  geom_point() + facet_grid((month-1) ~ . ) +
  stat_smooth( method = 'glm',
               method.args = list( family = "poisson"), se = FALSE,
               alpha = 0.5) +
  theme_minimal()

# Run the paired-test like before
t.test(y[treat == "before"], y[treat == "after"], paired = TRUE)

# Run a repeated-measures ANOVA
anova(lmer(y ~ treat + (1|x)))


### fit a separate model for each origin destination combination
modellist <- dlply(lmm.data, .(school, class), function(x) glm(extro ~ open + 
                                                                 agree + social, data = x))
display(modellist[[1]])

## lets make variables that count number of trips by mode for modelling putposes
str(trip)
table(is.na(trip$CURRENT_MODE_DESC))
unique(trip$CURRENT_MODE_DESC)
trip_dm <- trip %>% dplyr::select(WAVE, SAMPLE_NO, PERSON_NO, STOP_NO, CURRENT_MODE_DESC_9, TRAVEL_DATE) %>% 
  dplyr::mutate(count = 1) %>% dplyr::group_by(WAVE, SAMPLE_NO, PERSON_NO, TRAVEL_DATE, CURRENT_MODE_DESC_9) %>% 
  dplyr::summarise(count = sum(count)) %>% dplyr::ungroup() %>% tidyr::spread("CURRENT_MODE_DESC_9","count", fill = 0)

person_trip_dm <- merge(fnl_p_review_df, trip_dm, by = c("WAVE","SAMPLE_NO", "PERSON_NO"), all.x = T)
names(person_trip_dm)
person_trip_dm <- person_trip_dm  %>% dplyr::mutate(tot_trip = 
                                                      (Bicycle +Bus +Ferry+ Other +Taxi + 
                                                         Train + `Vehicle driver` + `Vehicle passenger` + 
                                                         Walking)) %>%  dplyr::mutate(tot_trip = dplyr::if_else(
                                                           is.na(tot_trip),0,tot_trip))

