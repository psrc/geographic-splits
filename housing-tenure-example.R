library(psrccensus)
library(psrcelmer)
library(psrcplot)
library(tidyverse)

# Inputs ------------------------------------------------------------------
years <- c(2016, 2020,2021)
county_order <- c("King County","Kitsap County", "Pierce County", "Snohomish County", "Region Total")
value_order <- c("Total", "Less than $100,000","$100,000 to $199,999","$200,000 to $299,999","$300,000 to $399,999","$400,000 to $499,999",
                 "$500,000 to $749,999","$750,000 to $999,999","$1,000,000 to $1,499,999","$1,500,000 to $1,999,999","$2,000,000 or more")

# Blockgroup Splits from Elmer --------------------------------------------
blockgroup_splits <- NULL
for (y in years) {

  if (y >=2020) {ofm_vin <- 2022} else {ofm_vin <- 2020}

  q <- paste0("SELECT * FROM general.get_geography_splits('blockgroup20', 'HCT Station Areas (Vision 2050)'," , y, ", ",ofm_vin,")")
  vision <- get_query(sql = q, db_name = "Elmer")

  q <- paste0("SELECT * FROM general.get_geography_splits('blockgroup20', 'Regional Growth Center (2022 RTP)'," , y, ", ",ofm_vin,")")
  rgc <- get_query(sql = q, db_name = "Elmer")

  q <- paste0("SELECT * FROM general.get_geography_splits('blockgroup20', 'MIC (2022 RTP)'," , y, ", ",ofm_vin,")")
  mic <- get_query(sql = q, db_name = "Elmer")

  splits <- bind_rows(vision, rgc, mic)

  if (is.null(blockgroup_splits)) {blockgroup_splits <- splits} else {blockgroup_splits <- bind_rows(blockgroup_splits, splits)}
  rm(vision, rgc, mic, splits, q)

}

rm(y, ofm_vin)

# Housing Tenure by HCT Station Area ----------------------------------------------------------

# Download ACS data and clean
tenure <- get_acs_recs(geography = 'block group', table.names = c('B25003'), years = years)

tenure <- tenure %>%
  select(geoid="GEOID", geography="census_geography", "estimate", "moe", "variable", "year") %>%
  mutate(variable = factor(case_when(variable == "B25003_001" ~ "total households",
                                     variable == "B25003_002" ~ "owner",
                                     variable == "B25003_003" ~ "renter",
                                     !is.na(variable) ~"N/A"),
                           levels=c("total households","owner","renter")))

# Calculate Estimates for HCT Areas using Blockgroup splits from Elmer Parcelization Data
in_hct_split <- blockgroup_splits %>%
  filter(planning_geog=="in station area") %>%
  select(geoid="data_geog", year="ofm_estimate_year", inside_split="percent_of_occupied_housing_units")

out_hct_split <- blockgroup_splits %>%
  filter(planning_geog=="not in station area") %>%
  select(geoid="data_geog", year="ofm_estimate_year", outside_split="percent_of_occupied_housing_units")

# Add Splits to Tenure Data
tenure_by_hct <- list(tenure, in_hct_split, out_hct_split) %>% reduce(left_join, by = c("geoid","year")) %>% mutate(across(everything(), ~replace_na(.x, 0)))

# Calculate Population Inside and Outside HCT Station Areas for Each Block Group
tenure_by_hct <- tenure_by_hct %>%
  mutate(inside_estimate = round(estimate*inside_split,0), outside_estimate = round(estimate*outside_split,0)) %>%
  mutate(inside_moe = round(moe*inside_split,0), outside_moe = round(moe*outside_split,0))

# Add County Name to Data
tenure_by_hct <- tenure_by_hct %>%
  mutate(county=case_when(
    str_detect(geoid, "53033") ~ "King County",
    str_detect(geoid, "53035") ~ "Kitsap County",
    str_detect(geoid, "53053") ~ "Pierce County",
    str_detect(geoid, "53061") ~ "Snohomish County"))

# Clean to match the titles in the report
tenure_by_hct <- tenure_by_hct %>%
  select("geoid","county","variable","year",`Inside centers and HCT station areas`="inside_estimate",`Outside centers and HCT station areas`="outside_estimate") %>%
  pivot_longer(!c(geoid,county,variable,year), names_to = "metric", values_to = "estimate")

# Create Summary Chart by HCT Area
tenure_summary <- tenure_by_hct %>%
  group_by(county, year, variable, metric) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble()

region_summary <- tenure_by_hct %>%
  group_by(year, variable, metric) %>%
  summarise(estimate=sum(estimate)) %>%
  as_tibble() %>%
  mutate(county="Region Total")

tenure_summary <- bind_rows(tenure_summary, region_summary) %>%
  mutate(county = factor(county, levels=county_order))

totals <- tenure_summary %>% filter(variable=="total households") %>% rename(total=estimate) %>% select(-"variable")

tenure_summary <- left_join(tenure_summary, totals, by=c("county","year", "metric")) %>%
  mutate(share=estimate/total) %>%
  select(-"total")

tenure_chart_2016 <- static_column_chart(t = tenure_summary %>% filter(year==2016 & variable=="owner"),
                                         x = "county", y="share", fill="metric", color="pognbgy_5",
                                         title="Housing Ownership in Centers and Near Transit: 2020",
                                         source="Source: US Census Bureau 2016-2020 American Community Survey 5-year Estimates Table B25003")


tenure_chart_2020 <- static_column_chart(t = tenure_summary %>% filter(year==2020 & variable=="owner"),
                                         x = "county", y="share", fill="metric", color="pognbgy_5",
                                         title="Housing Ownership in Centers and Near Transit: 2020",
                                         source="Source: US Census Bureau 2016-2020 American Community Survey 5-year Estimates Table B25003")

tenure_chart_2021 <- static_column_chart(t = tenure_summary %>% filter(year==2021 & variable=="owner"),
                                         x = "county", y="share", fill="metric", color="pognbgy_5",
                                         title="Housing Ownership in Centers and Near Transit: 2021",
                                         source="Source: US Census Bureau 2017-2021 American Community Survey 5-year Estimates Table B25003")

rm(in_hct_split, out_hct_split, totals, region_summary, tenure_by_hct)
