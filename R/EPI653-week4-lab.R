pacman::p_load(tidyverse,  # general data management
               sf,         # spatial data classes
               DCluster,   # Function achisq.test for homogeneity
               spdep,      # spatial analysis package supports probability maps
               SpatialEpi, # spatial analysis package with Empirical Bayes
               tmap,
               spdep,
               here)       # For mapping

ppor <- st_read(here::here("Data","Week 4 Data","ga_ppor.gpkg"))
ppor_age <- st_read(here::here("Data","Week 4 Data", "ga_ppor_age.gpkg"))

# Define r as reference rate: sum of all events divided by sum of all at risk
r <- sum(ppor$DEATH_TOTAL) / sum(ppor$FETOINFANT_DENOM)


# Expected is just reference rate X number at risk for event
ppor$expected <- ppor$FETOINFANT_DENOM * r


achisq.test(DEATH_TOTAL ~ offset(log(expected)), 
            data = ppor,
            model = 'poisson',
            R = 499)

# Observed / Expected count
ppor$SMR <- ppor$DEATH_TOTAL / ppor$expected
hist(ppor$SMR)


# calculate the raw/observed feto-infant mortality rate
ppor$rate <- ppor$DEATH_TOTAL / ppor$FETOINFANT_DENOM * 1000

# Create map of feto-infant mortality RATE
m1<-  tm_shape(ppor) + 
  tm_fill(c('rate'),
          style = 'quantile',
          palette = 'BuPu',
          title = 'Rate per 1,000') +
  tm_borders() + 
  tm_layout(main.title = 'Feto-infant mortality rate',
            inner.margins = c(0.02, 0.02, 0.02, 0.1),
            legend.format = list(digits = 1))

# Create map of feto-infant mortality SMR
m2<-  tm_shape(ppor) + 
  tm_fill(c('SMR'),
          style = 'quantile',
          palette = '-RdYlBu',
          title = 'SMR') +
  tm_borders() + 
  tm_layout(main.title = 'Feto-infant mortality SMR',
            inner.margins = c(0.02, 0.02, 0.02, 0.1),
            legend.format = list(digits = 1))

tmap_arrange(m1, m2)

x <- probmap(n = ppor$DEATH_TOTAL, x = ppor$FETOINFANT_DENOM, 
             alternative = 'greater')

# We can assign the probability to each county like this:
ppor$pmap <- x$pmap

smr_map <- tm_shape(ppor) +
  tm_fill('SMR',
          style = 'quantile',
          palette = '-RdYlBu',
          title = 'Std. Morbidity Ratio') + 
  tm_borders() +
  tm_layout(main.title = 'Unadjusted SMR of Feto-Infant Mortality',
            inner.margins = c(0.02, 0.02,0.05,0.2))

prob <- tm_shape(ppor) + 
  tm_fill('pmap',
          style = 'cont',
          palette = 'PiYG',
          title = 'Prob SMR > 1\nby chance alone') + 
  tm_borders() + 
  tm_layout(main.title = 'Probability Map',
            inner.margins = c(0.02, 0.02,0.05,0.2))

tmap_arrange(smr_map, prob)


# Create a "p-value" layer
pv <- ppor %>%
  mutate(pmap.pv = ifelse(SMR > 1 & pmap <0.05, 1, 0)) %>%
  group_by(pmap.pv) %>%
  summarise() %>%
  filter(pmap.pv == 1)


tm_shape(ppor) +
  tm_fill('SMR',
          style = 'fixed',
          palette = '-RdYlBu',
          breaks = c(0.13, 0.67, 0.9, 1.1, 1.4, 2.3),
          title = 'Std. Morbidity Ratio') + 
  tm_borders() +
  tm_layout(main.title = 'Unadjusted SMR of Feto-Infant Mortality',
            inner.margins = c(0.1, 0.02,0.05,0.2)) +
  # Add dark borders for significant
  tm_shape(pv) +
  tm_borders(lwd = 2, col = 'black') +
  tm_credits('Counties with higher than expected risk (p<0.05) highlighted with dark borders')


# First, sort the long dataset by county and covariate strata
ppor_age <- ppor_age %>%
  arrange(GEOID, AGE)

# Second, use expected() function to produce indirectly-adjusted expected counts
ppor$expected_indirect <- SpatialEpi::expected(population = ppor_age$FI_DENOM, 
                                               cases = ppor_age$FI_DEATH,
                                               n.strata = 5)

# Finally, recalculate the county-lspecific SMR using *age adjusted* expected counts
ppor$SMR_adj <- ppor$DEATH_TOTAL / ppor$expected_indirect

plot(ppor$expected, ppor$expected_indirect)
plot(ppor$SMR, ppor$SMR_adj)


## Calculate the EB smoothed estimates
ee <- eBayes(ppor$DEATH_TOTAL,ppor$expected)


# ee$RR is the EB-smoothed relative excess risk in each county
ppor$eb2 <- ee$RR


# Calculate probability that each county *actually* exceeds the null
ppor$eb2_prob <- EBpostthresh(Y = ppor$DEATH_TOTAL, 
                              E = ppor$expected, 
                              alpha = ee$alpha, 
                              beta = ee$beta, 
                              rrthresh = 1)

# Code included for your information; this chunk is optional extension
ggplot(ppor, aes(x = pmap, y = eb2_prob)) +
  geom_point(alpha = 0.5) +
  coord_fixed() + 
  theme_bw() + 
  xlab('Probability from Observed pmap()') +
  ylab('Probability from EB-smoothed Poisson-Gamma') 


pv2 <- ppor %>%
  mutate(eb.pv = ifelse(eb2 > 1 & eb2_prob > 0.95, 1, 0)) %>%
  group_by(eb.pv) %>%
  summarise() %>%
  filter(eb.pv == 1)

tm_shape(ppor) +
  tm_fill('eb2',
          style = 'fixed',
          palette = '-RdYlBu',
          breaks = c(0.13, 0.67, 0.9, 1.1, 1.4, 2.3),
          title = 'Rel Risk') + 
  tm_borders() +
  tm_layout(main.title = 'Empirical Bayes smoothed\nRelative Risk Infant Mortality',
            inner.margins = c(0.1, 0.02,0.05,0.2)) +
  # Add dark borders for significant
  tm_shape(pv2) +
  tm_borders(lwd = 2, col = 'black') +
  tm_credits('Counties with higher than expected risk (p<0.05) highlighted with dark borders')


#####Deliverable####

#Top 5 counties by raw SMR
top_5_raw <- ppor %>%
  arrange(desc(SMR)) %>%
  slice(head(row_number(),5))
head(top_5_raw) #Hancock, Baker, Jenkins, Dougherty, Jeff Davis

#Top 5 counties by adjusted SMR
top_5_adj <- ppor %>%
  arrange(desc(SMR_adj)) %>%
  slice(head(row_number(),5))
head(top_5_adj) #Hancock, Baker, Jenkins, Clay, Meriwether

#Top 5 counties by raw SMR and frequentist p<0.05
top_5_raw_sig <- ppor %>%
  subset(pmap<0.05) %>%
  arrange(desc(SMR)) %>%
  slice(head(row_number(),5))
head(top_5_raw_sig) #Hancock, Jenkins, Dougherty, Jeff Davis, Crisp

#Top 5 counties by EB SMR
top_5_EB_smr <- ppor %>%
  arrange(desc(eb2)) %>%
  slice(head(row_number(),5))
head(top_5_EB_smr) #Dougherty, Rockdale, Crisp, Jeff Davis, Mitchell

#Map observed MR by perinatal region
ppor_regional <- ppor %>%
  group_by(PERI_REGN) %>%
  summarise(MR_REGIONAL = (sum(DEATH_TOTAL)/sum(FETOINFANT_DENOM))*1000)
tm_shape(ppor_regional)+
  tm_fill("MR_REGIONAL",title="Feto-Infant MR (per 1000)")+
  tm_borders()+
tm_scale_bar()+
tm_credits("Data Source: Georgia OASIS",
           position= c("0.03","0.03"))+
tm_layout(frame=F,
          main.title = "Georgia Feto-Infant Mortality Rate by Perinatal Region",
          main.title.size=1.2,
          legend.title.size = 1.1,
          legend.position = c("RIGHT",0.8),
          inner.margins=c(0.1,0.02,0.02,0.02))

