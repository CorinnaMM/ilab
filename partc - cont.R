library(spdep)
library(lme4)
## 
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
read.OGR("/Users/Corinna/Desktop/corinna - temp/1270055001_sa2_2016_aust_shape/")

# read in both areas

trips_for_model$hhnum = rank(trips_for_model$HH_ID, ties.method = "min")
trips_for_model$ppnum = sapply(trips_for_model$PERS_ID, function(x) substr(x, nchar(x), nchar(x)))
trips_for_model$homesa2 = rank(trips_for_model$HH_SA2_NAME, ties.method = "min")
trips_for_model <- data.frame(trips_for_model)
# tmp <- with(trips_for_model,
#             by(trips_for_model, homesa2,
#                function(x) lm(TC ~ AGE, data = x)))
tmp <- with(trips_for_model,
            by(trips_for_model, homesa2,
               function(x) glm(TC ~ AGE + TRAVEL_DATE_NUMBER + month, data = x, family = "poisson")))
tmat<-t(sapply(tmp,coef))
tmat

# get mode type for each trip group
# ptmp <- with(trips_for_model,
#             by(trips_for_model, homesa2,
#                function(x) lm(CURRENT_MODE_DESC_9 ~ 1, data = x)))
# pmat<-as.matrix(sapply(ptmp,coef))

# htmp<-by(imm10, schnum, function(x) lm(math ~ homework, data=x))
# hmat<-t(sapply(tmp,coef))
# hmat

# combine public with intercept and slope
merMod_hts<-cbind(tmat,pmat)
# merMod_hts <"add all models above"

# Compute adjacency objects
nbsa2 <- poly2nb(hts_sa2_map)
grasa2 <- nb2gra(hts_sa2_map)
nbsa3 <- poly2nb(hts_sa2_map)
grasa3 <- nb2gra(hts_sa2_map)

# Fit spatial model
flu_spatial <- bayesx(
  Flu_OBS ~ HealthDeprivation + sx(i, bs = "spatial", map = borough_gra),
  offset = log(london$TOTAL_POP),
  family = "poisson", data = data.frame(london), 
  control = bayesx.control(seed = 17610407)
)

# Summarize the model
summary(flu_spatial)
# Summarise the model
summary(flu_spatial)

# Map the fitted spatial term only
london$spatial <- fitted(flu_spatial, term = "sx(i):mrf")[, "Mean"]
spplot(london, zcol = "spatial")

#  https://www.r-bloggers.com/sampling-weights-and-multilevel-modeling-in-r/
#  https://github.com/becarioprecario/SAERTutorial/blob/master/SAE02.pdf
#  https://cran.r-project.org/web/views/OfficialStatistics.html