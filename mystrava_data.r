library(rStrava)
library(ggplot2)
library(hexbin)
library(gghighlight)
library(lubridate)
library(dplyr)

# ref: https://www.andrewaage.com/post/analyzing-strava-data-using-r/

app_name <- 'rTrainingApp' # chosen by user
app_client_id  <- Sys.getenv("app_client_id") 
app_secret <- Sys.getenv("app_secret") 

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all"))
# stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])


# public test
myinfo <- get_athlete(stoken, id = '10328239')
head(myinfo)

# examples
my_acts <- get_activity_list(stoken)
df <- compile_activities(my_acts)
saveRDS(df, file = here::here("strava_df.rds"))
df <- readRDS(here::here("strava_df.rds"))

df <- df %>%
  mutate(start_date = as_date(start_date),
         year = year(start_date),
         day_of_year = yday(start_date),
         month = month(start_date),
         day = wday(start_date, label = TRUE),
         week = week(start_date))

df %>%
  group_by(year) %>%
  arrange(start_date) %>%
  mutate(cumulative_riding_distance = cumsum(distance)) %>%
  ungroup() %>%
  ggplot(aes(x = day_of_year, y = cumulative_riding_distance, color = factor(year))) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  gghighlight::gghighlight(year > 2016) +
  labs(title = "Cumulative riding distance per year",
       subtitle = "Last 3 years highlighted")

ggplot(df %>% filter(year > 2016), aes(x = week, y = factor(day))) +
  geom_tile(aes(fill = distance)) +
  scale_fill_continuous(low = "lightgreen", high = "red") +
  facet_wrap(~ year, scales = "free_x") +
  labs(x = "Week", y = "") 


df %>%
  ggplot(aes(x = round(distance,0), y = round(average_speed,0))) + 
  geom_hex() +
  scale_fill_viridis_c()



