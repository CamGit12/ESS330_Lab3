#prep states
states_daily_cases <- covid_raw %>%
  filter(state %in% c("New York", "Colorado", "Alabama", "Ohio")) %>%
  group_by(state, county) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(new_cases = pmax(cases - lag(cases), 0)) %>%
  ungroup() %>% group_by(state, date) %>% summarize(daily_cases = sum(new_cases, na.rm = TRUE), .groups = "drop")

#prep census (01 is AL, 08 is CO, 36 is NY, 39 is OH)
states_popdata <-read_csv('https://www2.census.gov/programs-surveys/popest/datasets/2020-2023/counties/totals/co-est2023-alldata.csv') %>%
  mutate(
    STATE = sprintf("%02d", as.numeric(STATE)),
    COUNTY = sprintf("%03d", as.numeric(COUNTY)),
    fips = paste0(STATE, COUNTY)) %>%
  select(fips, STATE, COUNTY, contains("NAME"), POPESTIMATE2021) %>%
  filter(STATE %in% c("01", "08", "36", "39"), str_sub(fips, 3, 5) != "000")

#pop to state level
states_pop <- states_popdata %>%
  group_by(STNAME) %>%
  summarize(pop_2021 = sum(POPESTIMATE2021, na.rm = TRUE), .groups = "drop")

#join the two
state_cases_with_pop <- states_daily_cases %>%
  left_join(states_pop, by = c("state" = "STNAME"))

#Compute the rolling 7day mean
state_cases_with_pop <- state_cases_with_pop %>%
  group_by(state) %>%
  mutate(cases_per_100k = (daily_cases / pop_2021) * 100000,
         rolling_mean_7day_per_100k = rollmean(cases_per_100k, k = 7, fill = NA, align = "center")) %>% ungroup()

#plot 1- new cases with rolling mean
ggplot(state_cases_with_pop, aes(x = date)) +
  geom_col(aes(y = daily_cases), fill = "blue", alpha = 0.4) +
  geom_line(aes(y = rolling_mean_7day_per_100k), color = "red", linewidth = 1.3, na.rm = TRUE) +
  facet_wrap(~ state, scales = "free_y") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title = "New Daily COVID Cases and 7-Day Rolling Mean",
    x = "Date",
    y = "Daily New Cases") + theme_minimal() +
    theme(strip.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1))

#plot 2
ggplot(state_cases_with_pop, aes(x = date, y = rolling_mean_7day_per_100k, color = state)) +
  geom_line() +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(title = "7 Day Rolling Avg of COVID Cases per 100k Citizens",
    x = "Date",
    y = "Cases per 100k",
    color = "State") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust =1))


Analysis:

Scaling by population results in a better comparison. Without this element, we are comparing small states to big states without regard to their size, which strongly impacts the number of cases they have. In this way, scaling by population keeps us from comparing "apples to oranges".

In this way, scaling by population makes New York look better than Alabama because Alabamas smaller population results in its per capita cases being higher (due to scaling by pop.)




OLD 7
#county data
county_centroids <- readr::read_csv("https://raw.githubusercontent.com/mikejohnson51/csu-ess-330/refs/heads/main/resources/county-centroids.csv")

#filter
covid_filtered <- covid_raw %>%
  select(fips, date, cases) %>%
  filter(!is.na(fips), !is.na(cases))

#join
covid_spatial <- covid_filtered %>%
  inner_join(county_centroids, by = "fips") %>%
  filter(!is.na(LAT), !is.na(LON))

#WMC calc
weighted_centers <- covid_spatial %>%
  group_by(date) %>%
  summarize(
    LON = sum(LON * cases, na.rm = TRUE) / sum(cases, na.rm = TRUE),
    LAT = sum(LAT * cases, na.rm = TRUE) / sum(cases, na.rm = TRUE),
    total_cases = sum(cases, na.rm = TRUE),
    month = format(date, "%m")
  )

#plot
ggplot() +
  borders("state", fill = "gray90", colour = "white") +
  geom_point(data = weighted_centers,
             aes(x = LON, y = LAT, color = month, size = total_cases),
             alpha = 0.7) +
  geom_path(data = weighted_centers,
            aes(x = LON, y = LAT),
            color = "darkgray", alpha = 0.5) +
  scale_color_brewer(name = "Month", palette = "Spectral") +
  scale_size_continuous(name = "Total Cases", labels = scales::comma) +
  labs(title = "COVID-19 Weighted Mean Center Movement",
       x = "Longitude", y = "Latitude") +
  theme_minimal()
```


all_dates <- expand.grid(fips = unique(statecoords$fips), date = unique(covid_raw$date))

covid_raw <- all_dates %>%
  left_join(covid_raw, by = c("fips", "date")) %>%
  mutate(cases = ifelse(is.na(cases), 0, cases)) %>%
  arrange(fips, date) %>%
  group_by(fips) %>%
  mutate(new_cases = pmax(cases - lag(cases), 0, na.rm = TRUE)) %>%
  ungroup()

#make DF, fix the filter to get correct number of daily points
Covidspatial <- covid_raw %>%
  group_by(fips, date) %>%
  summarize(daily_cases = sum(new_cases, na.rm = TRUE), .groups = "drop") %>%
  inner_join(statecoords, by = "fips")


#compute wmc
wmc_df <- Covidspatial %>%
  group_by(date) %>%
  summarise(
    total_cases = sum(daily_cases, na.rm = TRUE),
    WMC_LONG = ifelse(total_cases > 0, sum(LON * daily_cases, na.rm = TRUE) / total_cases, NA), WMC_LAT = ifelse(total_cases > 0, sum(LAT * daily_cases, na.rm = TRUE) / total_cases, NA), month = format(date, "%m"), .groups = "drop") %>% mutate(month = as.factor(month))


#plot
ggplot() +
  borders("state", fill = "gray90", colour = "white") +
  geom_point(data = wmc_df, aes(x = WMC_LONG, y = WMC_LAT, colour = month, size = total_cases), alpha = 0.5) +
  scale_size_continuous(range = c(2, 10)) +
  labs(title = "Weighted Mean Center (WMC) of Covid Cases in US",
       x = "Longitude",
       y = "Latitude",
       color = "Month",
       size = "Total Cases") + theme_minimal()


The WMC of Covid cases in the US looks to be weighted towards the East coast, likely explained due to the higher population of states in the Eastern US. As time passes, we can also see that there was pull from both the East and West coasts, with the WMC sitting around Illinois but spreading out as covid spread across the US. Additionally, we see a spread towards the later months of the year to the south, which could be explained by later hotspots in Florida, where restrictions and prevention measures were less severe.

## Question 8

```{r, fig.width=12, fig.height=6, out.width="100%"}
# Question 8

# Filter data
covid_filtered <- covid_raw %>%
  select(fips, date, cases, deaths) %>%
  filter(!is.na(fips))

# Compute daily new cases and deaths
covid_filtered <- covid_filtered %>%
  group_by(fips) %>%
  arrange(date) %>%
  mutate(
    new_cases = pmax(cases - lag(cases), 0, na.rm = TRUE),
    new_deaths = pmax(deaths - lag(deaths), 0, na.rm = TRUE)
  ) %>%
  ungroup()

# Merge covid data with state coordinates
covid_spatial2 <- covid_filtered %>%
  left_join(statecoords, by = "fips") %>%
  drop_na(LAT, LON)

# WMC for Cases
wmc_cases_df <- covid_spatial2 %>%
  group_by(date) %>%
  summarize(
    total_cases = sum(new_cases, na.rm = TRUE),
    WMC_LON_cases = ifelse(total_cases > 0, sum(LON * new_cases, na.rm = TRUE) / total_cases, NA),
    WMC_LAT_cases = ifelse(total_cases > 0, sum(LAT * new_cases, na.rm = TRUE) / total_cases, NA),
    month = format(date, "%m"),
    .groups = "drop"
  )

# WMC for Deaths
wmc_deaths_df <- covid_spatial2 %>%
  group_by(date) %>%
  summarize(
    total_deaths = sum(new_deaths, na.rm = TRUE),
    WMC_LON_deaths = ifelse(total_deaths > 0, sum(LON * new_deaths, na.rm = TRUE) / total_deaths, NA),
    WMC_LAT_deaths = ifelse(total_deaths > 0, sum(LAT * new_deaths, na.rm = TRUE) / total_deaths, NA),
    month = format(date, "%m"),
    .groups = "drop"
  )

# Cases Plot - red
P_cases <- ggplot() +
  borders("state", fill = "gray90", colour = "white") +
  geom_point(data = wmc_cases_df, aes(x = WMC_LON_cases, y = WMC_LAT_cases, colour = month, size = total_cases), alpha = 0.5) +
  scale_size_continuous(range = c(2, 10)) +
  labs(title = "Weighted Mean Center of Covid Cases", x = "Longitude", y = "Latitude", color = "Month", size = "Total Cases") +
  theme_minimal() +
  theme(plot.title = element_text(color = "darkred", size = 7, face = "bold"))

# Deaths Plot - navy
P_deaths <- ggplot() +
  borders("state", fill = "gray90", colour = "white") +
  geom_point(data = wmc_deaths_df, aes(x = WMC_LON_deaths, y = WMC_LAT_deaths, colour = month, size = total_deaths), alpha = 0.5) +
  scale_size_continuous(range = c(2, 10)) +
  labs(title = "Weighted Mean Center of Covid Deaths", x = "Longitude", y = "Latitude", color = "Month", size = "Total Deaths") +
  theme_minimal() +
  theme(plot.title = element_text(color = "navy", size = 7, face = "bold"))

# Patchwork to combine plots
final_plot <- P_cases + P_deaths + plot_layout(ncol = 2)
print(final_plot)
```

Here in this plot illustrating the WMC of both Cases and Deaths by Covid in the US, we can find that there was a very similar shape of origination of Covid. Additionally, we find that both cases and deaths were similarly spatially spread across the US, but there are sonme key spatial-temporal differences. One differences we find is the way that deaths is found spreading Northeast of the center. This movement of deaths can likely be explained by overwhelmed hospitals in the denser US East Coast states. Additionally, we find a later hotspot of deaths in Florida, likely explained by a large old-age retiree community and less restrictions which allowed for the spread we see as the cases also spread towards Florida during a similar period. Ulitmately both plots tell a similar story but allow for more information to be gained when comparing the two.
