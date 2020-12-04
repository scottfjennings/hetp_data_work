
#devtools::install_github("ctmm-initiative/ctmm")
library(ctmm)


## start with a filtered dataframe from hetp_covariate_filter.R


#-----
short_gapper <- function(zdf, zgap) {

zdf <- zdf %>% 
  arrange(timestamp) %>% 
  mutate(tdiff.lag = timestamp - lag(timestamp),
         tdiff.lead = lead(timestamp) - timestamp)

zdf_thin <- zdf %>% 
  filter(tdiff.lag > zgap & tdiff.lead > zgap) %>% 
  select(-tdiff.lag, -tdiff.lead)
return(zdf_thin)
}
df2 <- short_gapper(df, 200)
df <- df2

#ggplot(data = df_thin, aes(x = location.long, y = location.lat)) +
#  geom_point()


#------
# supposedly you can set the projection of the telemetry object, but I can't get this to work yet
#zoof <- list(x = df_thin$location.long, y = df_thin$location.lat)
#pzoof <- project(zoof, "+proj=longlat")
#df_tele <- as.telemetry(df_thin, projection = project(pzoof, "+proj=longlat"))

df_tele <- as.telemetry(df)
plot(df_tele, error = FALSE)

##

OUT <- outlie(df_tele)
plot(OUT)
hist(OUT$speed)
hist(OUT$distance)

df_tele_out <- cbind(df_tele, OUT) %>% 
  filter(speed < 13) %>% 
  select(-speed, -distance) %>% 
  as.telemetry()

OUT2 <- outlie(df_tele_out)
plot(OUT2)
hist(OUT2$speed)
hist(OUT2$distance)

df_tele <- df_tele_out


##

SVF_df_tele <- variogram(df_tele)
zoom(SVF_df_tele)

##
# determine best guess for starting model parms
# by eyeball
#variogram.fit(SVF_df_tele)
#dayeelY_guess <- GUESS
# auto
df_tele_autoguess <- ctmm.guess(df_tele, interactive=FALSE)
summary(df_tele_autoguess)

### clean up the env before running processor-intensive stuff
rm(hetp, hetp_covs, OUT, OUT2, df_tele_out, df, df2)




##
# fit model to telemetry data, using starting parm guess from previous step
## !!! perhaps this step not needed; not used in 'telemetry error' vignette
system.time(df_tele_m.autoguess <- ctmm.fit(df_tele, df_tele_autoguess, trace = TRUE))
summary(df_tele_m.autoguess)

##
#control <- list(method='pNewton')
#system.time(df_thin_tele_FITZ <- ctmm.select(df_tele, df_tele_autoguess, control = control, trace = TRUE, cores = 2, verbose=TRUE))
#summary(df_thin_tele_FITZ)

system.time(df_tele_FITZ <- ctmm.select(df_tele, df_tele_m.autoguess, verbose=TRUE))
summary(df_tele_FITZ)
zoom(SVF_df_tele, CTMM = df_tele_FITZ, col.CTMM = c("red","purple","blue", "green"))

df_best <- df_tele_FITZ$`OUF anisotropic`
summary(df_best)
#foo <- summary(df_best)$CI %>% 
#  data.frame() %>%
#  rownames_to_column('parm') %>% 
#  mutate(df = obj,
#         run.date = Sys.Date())


##


###########################
# fit homerange by autocorrelated kernel density estimate

df_hr <- akde(df_tele, df_best)
plot(df_hr)
summary(df_hr)
summary(df_hr, level.UD = 0.50)
plot(df_tele, UD = df_hr, error = FALSE)

projection(greg1_dayeelY_hr, asText = FALSE)

zlevel.UD = 0.50

writeShapefile(df_hr, level.UD = zlevel.UD, folder = "C:/Users/scott.jennings/Documents/Projects/hetp/hetp_data_work/model_output/shapefiles", file = paste(obj, zlevel.UD, sep = "_"))

#

foo50 <- summary(df_hr, level.UD = 0.50)$CI %>% 
  data.frame() %>% 
  rownames_to_column(., "varb") %>% 
  mutate(zlevel.UD = 0.50,
         obj = obj)
foo95 <- summary(df_hr, level.UD = 0.95)$CI %>% 
  data.frame() %>% 
  rownames_to_column(., "varb") %>% 
  mutate(zlevel.UD = 0.95,
         obj = obj)

hr_ud_summaries <- rbind(foo50, foo95)

#hr_ud_summaries_all <- hr_ud_summaries
hr_ud_summaries_all <- read.csv("model_output/hr_ud_summaries_all.csv")
hr_ud_summaries_all <- rbind(hr_ud_summaries_all, hr_ud_summaries)

write.csv(hr_ud_summaries_all, "model_output/hr_ud_summaries_all.csv", row.names = F)

##############################################################

