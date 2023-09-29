# Package Installation
```{r polaR_install, eval=FALSE}
# devtools::install("path/to/package-folder")
# library(polaR)
```

# Importing Data

The package's functions refer to specific variable names. With `polaR_import()`, you can import .dta files and rename the relevant variables into their corresponding polaR versions. `polaR:::var_dict()` shows the renaming pattern and variables used for this. 
The function also re-codes number-coded missing values (99999, 99998,...) into R-friendly `NA`s.

```{r Import Data, cache = TRUE}
cses_5 <- polaR_import(source = "cses_5", path = "datasets/cses/cses5.dta", keep_all = F)
cses_imd <- polaR_import(source = "cses_imd", path = "datasets/cses/cses_imd.dta", keep_all = F)

ess1  <- polaR_import(source = "ess", path = "datasets/ess/ESS1e06_6.dta", keep_all = F)
ess2  <- polaR_import(source = "ess", path = "datasets/ess/ESS2e03_6.dta", keep_all = F)
ess3  <- polaR_import(source = "ess", path = "datasets/ess/ESS3e03_7.dta", keep_all = F)
ess4  <- polaR_import(source = "ess", path = "datasets/ess/ESS4e04_5.dta", keep_all = F)
ess5  <- polaR_import(source = "ess", path = "datasets/ess/ESS5ATe1_1.dta", keep_all = F)
ess6  <- polaR_import(source = "ess", path = "datasets/ess/ESS6.dta", keep_all = F)
ess7  <- polaR_import(source = "ess", path = "datasets/ess/ESS7e02_2.dta", keep_all = F)
ess8  <- polaR_import(source = "ess", path = "datasets/ess/ESS8e02_2.dta", keep_all = F)
ess9  <- polaR_import(source = "ess", path = "datasets/ess/ESS9e03_1.dta", keep_all = F)
ess10 <- polaR_import(source = "ess", path = "datasets/ess/ESS10.dta", keep_all = F) 
ess <-  rbind(ess1, ess2, ess3, ess4, ess5, ess6, ess7, ess8, ess9, ess10)
rm(ess1, ess2, ess3, ess4, ess5, ess6, ess7, ess8, ess9, ess10)

ches <- polaR_import(source = "ches", path = "datasets/ches/1999-2019_CHES_dataset_means(v3).dta")


# CMP: .dta file seems to be corrupt
# cmp <- polaR_import(source = "cmp", path = "datasets/CMP/CMP.dta")

eb <- polaR_import(source = "eb_selection", "/Users/felix/Nextcloud/project-polref/data/polarization/datasets/eurobarometer/eb_selection.dta", keep_all = FALSE)
```

# Measures

## Individual Level

Some measures work on the individual level, i.e., they add an additional polarisation variables for every respondent in the original dataset.

```{r}
cses_5 <- spread_likedislike(cses_5)
cses_5 <- distance(cses_5)
cses_5 <- ind_range(cses_5, "leftright")
cses_5 <- sd_parties(cses_5, "leftright")

cses_imd <- spread_likedislike(cses_imd)
cses_imd <- distance(cses_imd)
cses_imd <- ind_range(cses_imd, "leftright")
cses_imd <- sd_parties(cses_imd, "leftright")
```

### Aggregate Individual Scores

To compare these scores between countries, they need to be aggregated to their country-year means.

```{r}
spread_likedislike_wgt_cses_5 <- mean_aggregate(cses_5, polarization_var = "spread_likedislike_wgt")
spread_likedislike_cses_5 <- mean_aggregate(cses_5, polarization_var = "spread_likedislike")
distance_wgt_cses_5 <- mean_aggregate(cses_5, polarization_var = "distance_wgt")
distance_cses_5 <- mean_aggregate(cses_5, polarization_var = "distance")
ind_range_cses_5 <- mean_aggregate(cses_5, polarization_var = "ind_range")
sd_parties_cses_5 <- mean_aggregate(cses_5, polarization_var = "sd_parties")

spread_likedislike_wgt_cses_imd <- mean_aggregate(cses_imd, polarization_var = "spread_likedislike_wgt")
spread_likedislike_cses_imd <- mean_aggregate(cses_imd, polarization_var = "spread_likedislike")
distance_wgt_cses_imd <- mean_aggregate(cses_imd, polarization_var = "distance_wgt")
distance_cses_imd <- mean_aggregate(cses_imd, polarization_var = "distance")
ind_range_cses_imd <- mean_aggregate(cses_imd, polarization_var = "ind_range")
sd_parties_cses_imd <- mean_aggregate(cses_imd, polarization_var = "sd_parties")
```

## Country-Year Level measures

Other measures are computed directly on a country-year level.

```{r Standard Deviation}
# Left-Right Self-Placement
sd_lr_cses_5 <- sd_mass(cses_5, score_var =  "leftright_self")
sd_lr_cses_imd <- sd_mass(cses_imd, score_var =  "leftright_self")

sd_lr_ess <- sd_mass(ess, score_var = "leftright_self")

sd_lr_eb <- sd_mass(eb, score_var = "leftright_self")
```

```{r CSES Polarisation Index}
polarisation_index_cses_5 <- cses_polarisation_index(cses_5)
polarisation_index_cses_imd <- cses_polarisation_index(cses_imd)
```

```{r API}
api_cses_5 <- api(cses_5)
api_cses_imd <- api(cses_imd)
```

## Expert measures

```{r CSES Collaborators}
sd_lr_expert_cses_5 <- sd_expert_leftright(cses_5)
sd_lr_expert_cses_imd <- sd_expert_leftright(cses_imd)
```

### CHES

```{r}
# No specific functions for datasets with long format yet
sd_lr_ches <- ches %>% 
  group_by(country, year) %>% 
  summarize(pol_score = sd(leftright_expert_party_), .groups = "drop") %>% 
  mutate(measure = "sd_parties") %>% 
  mutate(dataset = "ches")

colnames(sd_lr_ches) <- c("country", "year", "pol_score", "measure", "dataset")
```

# Combine datasets

```{r}
# Define dfs to combine
dfs = sapply(.GlobalEnv, is.data.frame) 
dfs_keep <- c("cses_5", "cses_imd", "ess", "eb", "ches", "cmp", "var_dict", "country_dict")

# Combine all datasets
polarization_data_long <- do.call(rbind, mget(names(dfs)[dfs][names(dfs)[dfs] %in%  dfs_keep == FALSE]))

# Remove combined dfs
rm(list = names(dfs)[dfs][names(dfs)[dfs] %in% dfs_keep == FALSE])
rm(dfs_keep, dfs)

# Omit Rows with NAs
polarization_data_long <- na.omit(polarization_data_long)

# Combine CSES 1-4 & 5
polarization_data_long <- polarization_data_long %>% 
  mutate(dataset = case_when(
    dataset == "cses_5" | dataset == "cses_imd" ~ "cses",
    TRUE ~ dataset
  ))
```
