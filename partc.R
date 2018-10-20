#########
## ilab part C
#########
# Use the spdep package
library(spdep)
# For the binomial statistics function
library(epitools)
# Load lmerTest
# library(lme4) should be read in with lmerTest
library(lmerTest)
# Use R2BayesX
library(R2BayesX)
fnl_person <- readRDS("~/income_imp_review16/fnl_p_review_df.rds")

# install.packages("spdep")
# install.packages("epitools")
# install.packages("lme4")
# install.packages("lmerTest")
# install.packages("R2BayesX")

### Spatial Autocorelation test
## lets make variables that count number of trips by mode for modelling putposes
trip <- read.csv("/Users/MittmanC/Desktop/hts_hts_trp_odmatrix.csv")
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
ggplot(person_trip_dm, aes(x = factor(INCOME_UNADJUSTED), y = tot_trip)) + geom_point()


# Make neighbor list
sa2_nb <- poly2nb(hts_ref)

# Get center points of each sa2
sa2_centers <- coordinates(hts_ref)

# Show the connections
plot(hts_ref); plot(sa2_nb, sa2_centers, add = TRUE)

# Map the total pop'n
spplot(hts_ref, zcol = "tot_sa2_pop")

# Run a Moran I test on total pop'n
moran.test(
  hts_ref$tot_sa2_pop, 
  nb2listw(sa2_nb)
)

# Map % Remain
spplot(hts_ref, zcol = "tot_trips")

# Run a Moran I MC test on % Remain
moran.mc(
  hts_ref$tot_trips, 
  nb2listw(sa2_nb), 
  nsim = 999
)

### SMR exceedence for trips

# Get CI from binomial distribution
flu_ci <- binom.exact(london$Flu_OBS, london$TOTAL_POP)

# Add borough names
flu_ci$NAME <- london$NAME

# Calculate London rate, then compute SMR
r <- sum(london$Flu_OBS) / sum(london$TOTAL_POP)
flu_ci$SMR <- flu_ci$proportion /r


### Exceedence pobabilities
# Probability of a binomial exceeding a multiple
binom.exceed <- function(observed, population, expected, e){
  1 - pbinom(e * expected, population, prob = observed / population)
}

# Compute P(rate > 2)
london$Flu_gt_2 <- binom.exceed(
  observed = london$Flu_OBS,
  population = london$TOTAL_POP,
  expected = london$Flu_EXP,
  e = 2)

# Use a 50-color palette that only starts changing at around 0.9
pal <- c(
  rep("#B0D0B0", 40),
  colorRampPalette(c("#B0D0B0", "orange"))(5), 
  colorRampPalette(c("orange", "red"))(5)
)

# Plot the P(rate > 2) map
spplot(london, "Flu_gt_2", col.regions = pal, at = seq(0, 1, len = 50))


### Poisson glm Spatial Model
# Fit a poisson GLM.
model_flu <- glm(
  Flu_OBS ~ HealthDeprivation, 
  offset = log(TOTAL_POP), 
  data = london, 
  family = poisson)

# Is HealthDeprivation significant?
summary(model_flu)

# Put residuals into the spatial data.
london$Flu_Resid <- residuals(model_flu)

borough_nb <- poly2nb(london)

# Test spatial correlation of the residuals.
moran.mc(london$Flu_Resid, listw = nb2listw(borough_nb), nsim = 999)

### BayesX glm spatial model

# Fit a GLM
model_flu <- glm(Flu_OBS ~ HealthDeprivation, offset = log(TOTAL_POP),
                 data = london, family = poisson)

# Summarize it                    
summary(model_flu)

# Calculate coeff confidence intervals
confint(model_flu)

# Compute adjacency objects
borough_nb <- poly2nb(london)
borough_gra <- nb2gra(borough_nb)

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

# Map the residuals
london$spatial_resid <- residuals(flu_spatial)[, "mu"]
spplot(london, zcol = "spatial_resid")

# Test residuals for spatial correlation
moran.mc(london$spatial_resid, nb2listw(borough_nb), 999)


### Mixed Effect Model

# Fit a Poisson glmer
summary( glmer(clicks ~ webpage + (1|group), family = 'poisson', data = userGroups))

# Age goes before year
modelOut <- glmer(count ~ age + year + (year|county), family = 'poisson',
                  data = ILdata)
summary(modelOut)

# Extract out fixed effects
fixef(modelOut)
# Extract out random effects 
ranef(modelOut)

# Run code to see one method for plotting the data
ggplot(data = ILdata2, aes(x = year, y = count, group = county)) +
  geom_line() +
  facet_grid(age ~ . ) +
  stat_smooth( method = 'glm',
               method.args = list( family = "poisson"), se = FALSE,
               alpha = 0.5) +
  theme_minimal()

# Run the paired-test like before
t.test(y[treat == "before"], y[treat == "after"], paired = TRUE)

# Run a repeated-measures ANOVA
anova(lmer(y ~ treat + (1|x)))