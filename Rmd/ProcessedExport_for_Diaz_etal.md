Processed Export for Diaz et al., 2020
================
Nicholas Baetge
6/22/2020

# Intro

This document is shows how nitrate drawdown and NCP were calculated from
the processed bottle file

# Import Data

  - estimate MLDs using the temperature criterion of de Boyer Montegut,
    2004: *“…threshold value of temperature or density from a
    near-surface value at 10 m depth (\(∆T = 0.2°C\) or
    \(∆\sigma = 0.03\) \(kg\)
\(m^{-3}\)).”*

<!-- end list -->

``` r
ctd <- read_rds("~/GITHUB/naames_export_ms/Input/deriv_naames_ctd.rds") %>% 
  select(Cruise, Station, CampCN, bin_depth, pres_db, ave_temp_c, ave_sal_psu) %>% 
  mutate(sigT = swSigmaTheta(salinity = ave_sal_psu, temperature = ave_temp_c, pressure = pres_db, referencePressure = 0)) %>% 
  select(Cruise:bin_depth, sigT) %>% 
  mutate(z_horizon = ifelse(bin_depth <= 10, "near surf", "below surf")) %>% 
  group_by(Cruise, Station, CampCN) %>% 
  mutate(max_z = max(bin_depth)) %>% 
  filter(max_z > 500) %>%
  group_by(Cruise, Station, CampCN, z_horizon) %>% 
  mutate(nearsurf_sigT = ifelse(z_horizon == "near surf", mean(sigT), NA),
         mld_sigT = nearsurf_sigT + 0.03) %>% 
  ungroup() %>% 
  arrange(Cruise, Station, CampCN, bin_depth) %>% 
  select(-c(max_z, z_horizon)) %>% 
  group_by(Cruise, Station, CampCN) %>% 
  fill(nearsurf_sigT, mld_sigT) %>% 
  ungroup()
```

To be able to estimate MLDs, we’ll need to interpolate the depth at
which the sigmaT threshold is reached.

``` r
#split the dataframe into a list, by campcn
mld.list <-  split(ctd, ctd$CampCN)
```

``` r
#create a function that adds an empty row to each campcn and also inputs the sigT threshold value int to the sigT column
addz.func <- function(a){
  #add an empty row to each list element
  a[nrow(a) + 1,] <- NA
  #add the mld_sigT value to the empty field in the temperature column
  a$sigT[is.na(a$sigT)] <- max(a$mld_sigT, na.rm = T)
  #fill the metadata for the empty row
  b <- a %>% 
    fill(., Cruise:CampCN, nearsurf_sigT, mld_sigT, .direction = c("downup")) 
  }
```

``` r
#apply function to list
added.mld.list <- lapply(mld.list, addz.func)
```

``` r
#save the list as a data frame 
added.mld.df <- plyr::ldply(added.mld.list, data.frame) %>%
  # group_by(CampCN) %>% 
  # distinct(., temperature, .keep_all = T) %>% 
  # ungroup() %>% 
  arrange(CampCN,  sigT) %>% 
  group_by(CampCN) %>% 
  mutate(interp_depth = na.approx(bin_depth)) %>% 
  filter(sigT == mld_sigT) %>% 
  rename(z_mld_sigT = interp_depth) %>% 
  select(Cruise, Station, CampCN, z_mld_sigT) %>% 
  mutate_at(vars(Station), as.character) %>% 
  mutate(Station = ifelse(Cruise == "AT38" & Station == "0", "1A", Station)) %>% 
  mutate(Cruise = ifelse(Cruise == "AT39", "AT39-6", Cruise))
```

``` r
headers <- read_sheet("https://docs.google.com/spreadsheets/d/1zw-W1k__BeuJg1oQpQQ_XT7zWLiY3dfL-dTaQ0pzN5Q/edit#gid=1446474071", sheet = "Bottle File Headers", skip = 0)

cols <- headers %>% 
  pull(col_type) %>% 
  str_c(., collapse = "")

google.df <-  read_sheet("https://docs.google.com/spreadsheets/d/1zw-W1k__BeuJg1oQpQQ_XT7zWLiY3dfL-dTaQ0pzN5Q/edit#gid=1446474071", sheet = "Bottle File", skip = 1, col_types = cols)


#load the float-station matchup with the maximum MLD data
float_maxmld.df <- read_rds("~/GITHUB/naames_export_ms/Output/max_mld.rds") %>% 
  select(bin, max_mld) %>%
  rename(degree_bin = bin) %>% 
  distinct()


mld_diaz <- read_excel("~/GITHUB/naames_export_ms/Input/MLD_Diaz_6.19.2020.xlsx") %>% 
  mutate(Date = ifelse(NAAMES == 3 & Station == 1, "20170904", Date),
         Date = ifelse(NAAMES == 4 & Station == 3, "20180330", Date)) %>% 
  mutate(Cruise = ifelse(NAAMES == 1, "AT32", NA),
         Cruise = ifelse(NAAMES == 2, "AT34", Cruise),
         Cruise = ifelse(NAAMES == 3, "AT38", Cruise),
         Cruise = ifelse(NAAMES == 4, "AT39-6", Cruise),
         Station = ifelse(Station == "5b", 5, Station),
         Station = ifelse(Station == "1a", "1A", Station),
         Date = ymd(Date)) %>% 
  select(-NAAMES) %>% 
  rename(Diaz_MLD = Z_MLD) 


  

#join the google df with the max mld data from the floats
#this depth will be used to determine the background DOC
google2.df <- google.df %>% 
  mutate(lat = round(Latitude, 1),
         degree_bin = round(lat),
         BactAbund = BactAbund * 10^8,
         BactAbund_sd = BactAbund_sd * 10^8) %>% 
  left_join(., float_maxmld.df) %>% 
  rename(Max_MLD = max_mld) %>% 
  select(-lat) %>% 
  select(Cruise:Longitude, Subregion, degree_bin, CruiseCN:CampCN, Cast_Type, Max_MLD, Z_MLD, EZD, Target_Z, DOC, DOC_sd, BactAbund, BactAbund_sd, Virusml) %>% 
   filter(!Cast_Type %in% c("Microlayer", "Flow-through")) %>% 
  select(-Time, -Type) %>% 
  mutate(Time_Stamp = gsub("T", "_", Time_Stamp),
         Time_Stamp = gsub(":", "-", Time_Stamp),
         Date = as.Date(Time_Stamp),
         datetime = ymd_hm(Time_Stamp, tz = "UTC")) %>% 
  select(Cruise:Date, datetime,  everything(), -Time_Stamp) %>% 
  rename(doc = DOC,
         sd_doc = DOC_sd,
         ba = BactAbund,
         sd_ba = BactAbund_sd,
         va = Virusml) %>% 
  mutate(va = va * 10^3) %>% 
  mutate(Subregion = gsub("Gulf_Stream_Sargasso", "GS/Sargasso", Subregion)) %>% 
  mutate(Season = Cruise, 
         Season = gsub("AT32", "Late Autumn", Season),
         Season = gsub("AT34", "Late Spring", Season),
         Season = gsub("AT38", "Early Autumn", Season),
         Season = gsub("AT39-6", "Early Spring", Season)) %>% 
  select(Cruise, Subregion, Season, Station, everything())  %>% 
  left_join(., mld_diaz) %>% 
  left_join(., added.mld.df) %>% 
  select(Cruise:Z_MLD, Diaz_MLD, z_mld_sigT, everything())
```

# Collapse Profiles

``` r
#Split data frame into a list based on campaign cast no 
cast.list <- split(google2.df, google2.df$CampCN)

#create a function to collapse the entire cast list 
#the end product: unique column values for the same depth but sampled from different niskins are combined into a single row. 
#what is happening here is:
#for every depth that has more than one niskin associated with it, a mean is reported for each variable for that depth (disregarding NA values)
#this works for this dataset as there are never two measurements of a variable taken from the same depth of the same cast, but from different bottles
collapse.func <- function(casper){
  meta.df <- casper %>% 
    select(Cruise:Target_Z) %>% 
    unique(.)
  data.df <- casper %>% 
    select(-c(Cruise:EZD)) %>% 
    aggregate(., by = list(.$Target_Z), mean, na.rm = T)
  collapsedcast.df <- meta.df %>% 
    left_join(., data.df, by = "Target_Z")
}

#run the cruise list through the function 
collapsed.list <- lapply(cast.list, collapse.func)

#convert the list into a dataframe 
collapsed.df <- data.frame(rbindlist(collapsed.list)) %>% 
  select(-Group.1) %>% 
  group_by(CampCN) %>% 
  fill(Cruise:SCN, Cast_Type:EZD) %>% 
  ungroup()
```

# Interpolate Data for Max MLD (and for nominal depths not sampled)

``` r
#extract deep casts from the collapsed profiles data frame 
add_maxMLD.df <- collapsed.df[which(!collapsed.df$Cast_Type %in% c("Biology", "Shallow")) , ] 

#split the df by CampCN  
add_maxMLD.list <- split(add_maxMLD.df, add_maxMLD.df$CampCN)

#create a function to add an empty row to each cast, then add the max MLD to the Target Z column 
add.func <- function(morty){
  morty[nrow(morty) + 1,] <- NA
  morty$Target_Z[is.na(morty$Target_Z)] <- max(morty$Max_MLD, na.rm = T)
  rick <- morty %>% 
    fill(., Cruise:EZD, .direction = c("updown")) %>% 
    arrange(CampCN, Target_Z)
  
  rick[nrow(rick) + 1,] <- NA
  rick$Target_Z[is.na(rick$Target_Z)] <- 0
   schwifty <- rick %>% 
     fill(., Cruise:EZD, .direction = c("updown")) %>%
    arrange(CampCN, Target_Z)

   
  schwifty[nrow(schwifty) + 1,] <- NA
  schwifty$Target_Z[is.na(schwifty$Target_Z)] <- max(schwifty$z_mld_sigT, na.rm = T)
   summer <- schwifty %>% 
     fill(., Cruise:EZD, .direction = c("updown")) %>%
    arrange(CampCN, Target_Z)
}


#apply function to list 
added_maxMLD.list <- lapply(add_maxMLD.list, add.func)


#save the list as a data frame 
added_maxMLD.df <- plyr::ldply(added_maxMLD.list, data.frame) %>% 
  group_by(CampCN) %>% 
  distinct(., Target_Z, .keep_all = T) %>% 
  select(-.id) %>% 
  ungroup() %>% 
  mutate(Max_MLD = ifelse(z_mld_sigT > Max_MLD, z_mld_sigT, Max_MLD)) %>% 
  filter(!Target_Z == "-Inf")


#extract deep casts from the collapsed profiles data frame 
to_interpolate.df <- added_maxMLD.df

#split the data frame into lists based on the campaign cast number
to_interpolate.list <- split(to_interpolate.df, to_interpolate.df$CampCN)

#create a function that will linearly interpolate each VOI according to the depth intervals of the casts 
interpolate.func <- function(copper) {
to_interpolate.df <- copper %>% 
  select(Target_Z:ncol(.)) %>% 
  zoo(., order.by = .$Target_Z) 

interp_doc <- as.numeric(na.approx(to_interpolate.df$doc, na.rm = F))
interp_ba <- as.numeric(na.approx(to_interpolate.df$ba, na.rm = F))
interp_va <- as.numeric(na.approx(to_interpolate.df$va, na.rm = F))
Target_Z <- to_interpolate.df$Target_Z
interpolations.df <- data.frame(Target_Z, interp_doc, interp_ba, interp_va)
}

#apply function to list 
interpolations.list <- lapply(to_interpolate.list, interpolate.func)

#save the list as a data frame 
interpolations.df <- plyr::ldply(interpolations.list, data.frame) %>% 
  rename(., CampCN = .id) %>% 
  group_by(CampCN) %>% 
  fill(interp_doc:interp_va, .direction = "up") %>% 
  ungroup()

#combine the interpolated and non-interpolated data frames
interpolations.df$CampCN <- as.numeric(interpolations.df$CampCN)
interpolated.df <- left_join(to_interpolate.df, interpolations.df) %>% 
  drop_na(z_mld_sigT) %>% 
  #omitting CampCN 98 because only measurements at 5 m and below 1500 m were taken. the interpolated values for this cast were as a result, not reliable
  filter(!CampCN == 98) %>% 
  #the multiday N2S4 drifted from the 48˚Lat bin to the 47˚Lat bin, but was a lagrangian study (followed float), so we'll denote all casts within station 4 as being in the 48˚N bin. This also occured with N1S7 - casts will be in the 40˚ bin with a max MLD of 484 m
  mutate(degree_bin = ifelse(Cruise == "AT34" & Station == "4", 48, degree_bin),
         degree_bin = ifelse(Cruise == "AT32" & Station == "7", 40, degree_bin),
         Max_MLD = ifelse(Cruise == "AT34" & Station == "4", 508, Max_MLD),
          Max_MLD = ifelse(Cruise == "AT32" & Station == "7", 484, Max_MLD)) %>% 
  select(-contains("sd")) 
```

# calc carbon and doc without bugs

``` r
calcs <- interpolated.df %>% 
  mutate(bc = interp_ba * (1.03 * 10^-9),
         bc = 0.6 * bc,
         vc = interp_va * (1.7 * 10^-11),
         doc_corr = interp_doc - (bc + vc), 
         doc_corr = ifelse(is.na(doc_corr), interp_doc - bc, doc_corr)) #viruses = 0.2 fg per particle, bact = 12.4 fg per cell
```

## Average Variable Values

We’ll use these averages to redistribute profiles

``` r
to_redis <- calcs %>%
  group_by(Cruise, Station, degree_bin, Target_Z) %>% 
  mutate(ave_doc = mean(interp_doc, na.rm = T),
         ave_doc = ifelse(ave_doc == "Nan", NA, ave_doc),
         ave_doc = ifelse(ave_doc == "NaN", NA, ave_doc),
         
         ave_doc_corr = mean(doc_corr, na.rm = T),
         ave_doc_corr = ifelse(ave_doc_corr == "Nan", NA, ave_doc_corr),
         ave_doc_corr = ifelse(ave_doc_corr == "NaN", NA, ave_doc_corr),
         ) %>% 
  ungroup() %>% 
  select(Cruise, degree_bin, Station, Max_MLD, Target_Z, ave_doc,  ave_doc_corr) %>%
  distinct() %>% 
  arrange(Cruise, Station, degree_bin, Target_Z)  %>% 
  drop_na(ave_doc_corr)
```

# Redistribute DOC Profiles

``` r
redis_DOC <- to_redis %>% 
  group_by(Cruise, Station) %>% 
  filter(Target_Z <= Max_MLD) %>% 
  mutate(redis_doc_vol = integrateTrapezoid(Target_Z, ave_doc, type = "A")/Max_MLD,
         redis_doc_corr_vol = integrateTrapezoid(Target_Z, ave_doc_corr, type = "A")/Max_MLD,
         ) %>% 
  select(Cruise, Station,  degree_bin, redis_doc_vol, redis_doc_corr_vol) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(degree_bin) %>% 
  arrange(degree_bin) %>% 
  mutate(redis_doc_vol = ifelse(Cruise %in% c( "AT34", "AT39-6") & degree_bin %in% c(44), NA, redis_doc_vol),
         redis_doc_corr_vol = ifelse(Cruise %in% c( "AT34", "AT39-6") & degree_bin %in% c(44), NA, redis_doc_corr_vol),
         ave_redis_doc = mean(redis_doc_vol, na.rm = T),
         ave_redis_doc_corr = mean(redis_doc_corr_vol, na.rm = T),
         redis_doc_vol = ifelse(is.na(redis_doc_vol) & degree_bin == 44, ave_redis_doc, redis_doc_vol),
         redis_doc_corr_vol = ifelse(is.na(redis_doc_corr_vol) & degree_bin == 44, ave_redis_doc_corr, redis_doc_corr_vol)) %>% 
  select(-c(ave_redis_doc, ave_redis_doc_corr)) %>% 
  fill(redis_doc_vol, redis_doc_corr_vol, .direction = "updown") %>% 
  ungroup() %>% 
  arrange(Cruise, Station)
```

# Calculate Mixed Profile Areas in z\_mld\_sigT

``` r
redis_areas <- calcs %>% 
  left_join(., redis_DOC) %>% 
  mutate(redis_doc_diaz_mld_area = redis_doc_vol * z_mld_sigT,
         redis_doc_corr_diaz_mld_area = redis_doc_corr_vol * z_mld_sigT ) %>% 
  drop_na(interp_doc) 
```

# Integrate DOC Profiles

``` r
int_doc <- redis_areas %>% 
  group_by(Cruise, Station, CampCN) %>% 
  filter(Target_Z <= Max_MLD) %>%
  mutate(int_doc_maxmld = integrateTrapezoid(Target_Z, interp_doc,type = "A")) %>% 
  filter(Target_Z <= z_mld_sigT) %>%
  mutate(int_doc_diaz_mld = integrateTrapezoid(Target_Z, interp_doc,type = "A"),
         int_doc_corr_diaz_mld = integrateTrapezoid(Target_Z, doc_corr,type = "A")) %>% 
  ungroup() %>% 
  select(Cruise, Station, Date, CampCN, int_doc_diaz_mld, int_doc_corr_diaz_mld) %>% 
  distinct() 
```

# Calculate ∆DOC in Diaz\_MLD

``` r
processed_export <- redis_areas %>% 
  left_join(., int_doc) %>% 
  group_by(Cruise, Station, CampCN ) %>% 
  mutate(int_delta_doc_diaz_mld = (int_doc_diaz_mld - redis_doc_diaz_mld_area)/1000,
         int_delta_doc_corr_diaz_mld = (int_doc_corr_diaz_mld - redis_doc_corr_diaz_mld_area)/1000,
         vol_delta_doc_diaz_mld = (int_delta_doc_diaz_mld/Diaz_MLD) * 1000,
         vol_delta_doc_corr_diaz_mld = (int_delta_doc_corr_diaz_mld/Diaz_MLD) * 1000 ) %>% 
  ungroup() %>% 
  distinct() %>% 
  select(Cruise:z_mld_sigT, Target_Z, doc:redis_doc_corr_vol, vol_delta_doc_diaz_mld,  vol_delta_doc_corr_diaz_mld) %>% 
  distinct() %>% 
  rename(accm_doc_vol = vol_delta_doc_diaz_mld,
         accm_doc_corr_vol = vol_delta_doc_corr_diaz_mld) %>% 
  mutate(accm_doc_vol = ifelse(Cruise == "AT32", NA, accm_doc_vol),
         accm_doc_corr_vol = ifelse(Cruise == "AT32", NA, accm_doc_corr_vol)
         )

processed_export[ is.na(processed_export) ] <- NA

summary <- processed_export %>% 
  mutate(diff_conc = doc - doc_corr,
         diff_accm = accm_doc_vol - accm_doc_corr_vol) %>% 
  select(Cruise, Station, CampCN, Target_Z, contains("diff")) %>% 
  distinct() %>% 
  drop_na(diff_conc) 
  
summary_conc <- summary %>% 
  summarise_at(vars(diff_conc), list(mean = mean, sd = sd, median = median, max = max, min = min)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename(value = V1)

summary_accm <- summary %>% 
  drop_na(diff_accm) %>% 
  summarise_at(vars(diff_accm), list(mean = mean, sd = sd, median = median, max = max, min = min)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename(value = V1)
```

# Save Data

``` r
write_csv(processed_export, "~/GITHUB/naames_export_ms/Output/processed_export_for_Diaz.3.31.21.csv")
```

# Plot

## doc v bug corr doc

    ## RMA was not requested: it will not be computed.

## Plots

    ## RMA was not requested: it will not be computed.

``` r
library(patchwork)

reg2.plot + reg1.plot +  plot_annotation(tag_levels = "a") + plot_layout(guides = "collect") &
  theme(plot.tag = element_text(size = 14))
```

    ## Warning: Removed 57 rows containing missing values (geom_point).

    ## Warning in is.na(x): is.na() applied to non-(list or vector) of type
    ## 'expression'

    ## Warning: Removed 11 rows containing missing values (geom_point).

    ## Warning in is.na(x): is.na() applied to non-(list or vector) of type
    ## 'expression'

![](ProcessedExport_for_Diaz_etal_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->
