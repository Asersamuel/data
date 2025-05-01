#Chapter 16 Factors

#16.1.1 Prerequisites
library(tidyverse)

#16.2 Factor basics
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")
sort(x1)
#> [1] "Apr" "Dec" "Jan" "Mar"

month_levels <- c(
"Jan", "Feb", "Mar", "Apr", "May", "Jun",
"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
y1
#> [1] Dec Apr Jan Mar
#> Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec

sort(y1)
#> [1] Jan Mar Apr Dec
#> Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec

y2 <- factor(x2, levels = month_levels)
y2
#> [1] Dec  Apr  <NA> Mar 
#> Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec

y2 <- fct(x2, levels = month_levels)
#> Error in `fct()`:
#> ! All values of `x` must appear in `levels` or `na`
#> ℹ Missing level: "Jam"

factor(x1)
#> [1] Dec Apr Jan Mar
#> Levels: Apr Dec Jan Mar

fct(x1)
#> [1] Dec Apr Jan Mar
#> Levels: Dec Apr Jan Mar

levels(y2)
#>  [1] "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"

csv <- "
month,value
Jan,12
Feb,56
Mar,12"

df <- read_csv(csv, col_types = cols(month = col_factor(month_levels)))
df$month
#> [1] Jan Feb Mar
#> Levels: Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec

#16.3 General Social Survey
gss_cat
gss_cat |>
  count(race)

#16.3.1 Exercises
#1. Explore the distribution of rincome (reported income). What makes the default bar chart hard to understand? How could you improve the plot?
gss_cat |> 
  count(rincome) |> 
  ggplot(aes(x = rincome, y = n)) +
  geom_col() +
  coord_flip()
#It has too many unordered, long labels that overlap. Reordering the levels and flipping the chart makes it clearer.

#2. What is the most common relig in this survey? What’s the most common partyid?
gss_cat |> 
  count(relig, sort = TRUE)
#Protestant.
gss_cat |> 
  count(partyid, sort = TRUE)
#Independent.

#3. Which relig does denom (denomination) apply to? How can you find out with a table? How can you find out with a visualization?
gss_cat |> 
  count(relig, denom) |> 
  filter(n > 0)
#denom applies to Protestants only. You can see this in a table or chart showing non-missing values by religion.

#16.4 Modifying factor order
relig_summary <- gss_cat |>
  group_by(relig) |>
  summarize(
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary, aes(x = tvhours, y = relig)) +
  geom_point()
ggplot(relig_summary, aes(x = tvhours, y = fct_reorder(relig, tvhours))) +
  geom_point()
relig_summary |>
  mutate(
    relig = fct_reorder(relig, tvhours)
  ) |>
  ggplot(aes(x = tvhours, y = relig)) +
  geom_point()

rincome_summary <- gss_cat |>
  group_by(rincome) |>
  summarize(
    age = mean(age, na.rm = TRUE),
    n = n()
  )

ggplot(rincome_summary, aes(x = age, y = fct_reorder(rincome, age))) +
  geom_point()

ggplot(rincome_summary, aes(x = age, y = fct_relevel(rincome, "Not applicable"))) +
  geom_point()

by_age <- gss_cat |>
  filter(!is.na(age)) |>
  count(age, marital) |>
  group_by(age) |>
  mutate(
    prop = n / sum(n)
  )

ggplot(by_age, aes(x = age, y = prop, color = marital)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1")

ggplot(by_age, aes(x = age, y = prop, color = fct_reorder2(marital, age, prop))) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "marital")

gss_cat |>
  mutate(marital = marital |> fct_infreq() |> fct_rev()) |>
  ggplot(aes(x = marital)) +
  geom_bar()

#16.4.1 Exercises
#1. There are some suspiciously high numbers in tvhours. Is the mean a good summary?
#No, the mean can be skewed by very high values. The median would be a better summary in this case.
#2. For each factor in gss_cat identify whether the order of the levels is arbitrary or principled.
#relig, marital, and race: Arbitrary order.
#rincome, partyid, age: Principled, based on meaningful categories like income range or political leaning.


#3. Why did moving “Not applicable” to the front of the levels move it to the bottom of the plot?
#Because ggplot plots from bottom to top; moving it to the front means it appears first (lowest) on the y-axis.
  

#16.5 Modifying factor levels
gss_cat |> count(partyid)
gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat"
    )
  ) |>
  count(partyid)

gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat",
                         "Other"                 = "No answer",
                         "Other"                 = "Don't know",
                         "Other"                 = "Other party"
    )
  )

gss_cat |>
  mutate(
    partyid = fct_collapse(partyid,
                           "other" = c("No answer", "Don't know", "Other party"),
                           "rep" = c("Strong republican", "Not str republican"),
                           "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
                           "dem" = c("Not str democrat", "Strong democrat")
    )
  ) |>
  count(partyid)

gss_cat |>
  mutate(relig = fct_lump_lowfreq(relig)) |>
  count(relig)

gss_cat |>
  mutate(relig = fct_lump_n(relig, n = 10)) |>
  count(relig, sort = TRUE)

#16.5.1 Exercises
#1. How have the proportions of people identifying as Democrat, Republican, and Independent changed over time?

Copy
gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                "Republican" = c("Strong republican", "Not str republican"),
                                "Independent" = c("Ind,near rep", "Independent", "Ind,near dem"),
                                "Democrat" = c("Not str democrat", "Strong democrat"),
                                "Other" = c("No answer", "Don't know", "Other party")
  )) %>%
  group_by(year, partyid) %>%
  summarize(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  ggplot(aes(x = year, y = prop, color = partyid)) +
  geom_line() +
  labs(title = "Political Affiliation Trends Over Time",
       y = "Proportion",
       color = "Party Affiliation")
#2.How could you collapse rincome into a small set of categories?
gss_cat %>%
  mutate(rincome = fct_collapse(rincome,
                                "No income" = c("No answer", "Don't know", "Refused", "Not applicable"),
                                "Low income" = c("Lt $1000", "$1000 to 2999", "$3000 to 3999", "$4000 to 4999"),
                                "Middle income" = c("$5000 to 5999", "$6000 to 6999", "$7000 to 7999", 
                                                    "$8000 to 9999", "$10000 - 14999", "$15000 - 19999"),
                                "High income" = c("$20000 - 24999", "$25000 or more")
  )) %>%
  count(rincome)
#3. Notice there are 9 groups (excluding other) in the fct_lump example above. Why not 10? (Hint: type ?fct_lump, and find the default for the argument other_level is “Other”.)
gss_cat %>%
  mutate(relig = fct_lump_n(relig, n = 10)) %>%
  count(relig, sort = TRUE)

#It shows 9 groups plus "Other" because the 10th group was small enough that it got lumped into "Other" rather than being kept as its own category.

#16.6 Ordered factors
ordered(c("a", "b", "c"))


#Chapter 17 Dates and times
library(tidyverse)
library(nycflights13)

#17.2 Creating date/times
today()
#> [1] "2025-04-17"
now()
#> [1] "2025-04-17 23:09:00 UTC"

#17.2.1 During import
csv <- "
  date,datetime
  2022-01-02,2022-01-02 05:12
"
read_csv(csv)

csv <- "
  date
  01/02/15
"

read_csv(csv, col_types = cols(date = col_date("%m/%d/%y")))
#> # A tibble: 1 × 1
#>   date      
#>   <date>    
#> 1 2015-01-02

read_csv(csv, col_types = cols(date = col_date("%d/%m/%y")))
#> # A tibble: 1 × 1
#>   date      
#>   <date>    
#> 1 2015-02-01

read_csv(csv, col_types = cols(date = col_date("%y/%m/%d")))
#> # A tibble: 1 × 1
#>   date      
#>   <date>    
#> 1 2001-02-15

#17.2.2 From strings
ymd("2017-01-31")
#> [1] "2017-01-31"
mdy("January 31st, 2017")
#> [1] "2017-01-31"
dmy("31-Jan-2017")
#> [1] "2017-01-31"

ymd_hms("2017-01-31 20:11:59")
#> [1] "2017-01-31 20:11:59 UTC"
mdy_hm("01/31/2017 08:01")
#> [1] "2017-01-31 08:01:00 UTC"

ymd("2017-01-31", tz = "UTC")
#> [1] "2017-01-31 UTC"

#17.2.3 From individual components
flights |> 
  select(year, month, day, hour, minute)

flights |> 
  select(year, month, day, hour, minute) |> 
  mutate(departure = make_datetime(year, month, day, hour, minute))

make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights_dt <- flights |> 
  filter(!is.na(dep_time), !is.na(arr_time)) |> 
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) |> 
  select(origin, dest, ends_with("delay"), ends_with("time"))

flights_dt

flights_dt |> 
  ggplot(aes(x = dep_time)) + 
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

flights_dt |> 
  filter(dep_time < ymd(20130102)) |> 
  ggplot(aes(x = dep_time)) + 
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

#17.2.4 From other types
as_datetime(today())
#> [1] "2025-04-17 UTC"
as_date(now())
#> [1] "2025-04-17"

as_datetime(60 * 60 * 10)
#> [1] "1970-01-01 10:00:00 UTC"
as_date(365 * 10 + 2)
#> [1] "1980-01-01"

#17.2.5 Exercises
#1. What happens if you parse a string that contains invalid dates?
  
  ymd(c("2010-10-10", "bananas"))
#Invalid dates like "bananas" return NA with a warning, while valid dates parse correctly.

#2. What does the tzone argument to today() do? Why is it important?
  #tzone in today() sets the time zone for the current date, important because dates change by time zone.
#3. For each of the following date-times, show how you’d parse it using a readr column specification and a lubridate function.

#d1 <- "January 1, 2010" - month-day-year
#d2 <- "2015-Mar-07" - year-month-day
#d3 <- "06-Jun-2017" - day-month-year
#d4 <- c("August 19 (2015)", "July 1 (2015)")
#d5 <- "12/30/14" # Dec 30, 2014 -  month-day-year
#t1 <- "1705" -  24-hour time
#t2 <- "11:15:10.12 PM" - includes AM/PM and seconds.
  
#17.3 Date-time components
#17.3.1 Getting components
  datetime <- ymd_hms("2026-07-08 12:34:56")
  
  year(datetime)
  #> [1] 2026
  month(datetime)
  #> [1] 7
  mday(datetime)
  #> [1] 8
  
  yday(datetime)
  #> [1] 189
  wday(datetime)
  #> [1] 4

  month(datetime, label = TRUE)
  #> [1] Jul
  #> 12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < Aug < Sep < ... < Dec
  wday(datetime, label = TRUE, abbr = FALSE)
  #> [1] Wednesday
  #> 7 Levels: Sunday < Monday < Tuesday < Wednesday < Thursday < ... < Saturday
  
  flights_dt |> 
    mutate(wday = wday(dep_time, label = TRUE)) |> 
    ggplot(aes(x = wday)) +
    geom_bar()
  
  flights_dt |> 
    mutate(minute = minute(dep_time)) |> 
    group_by(minute) |> 
    summarize(
      avg_delay = mean(dep_delay, na.rm = TRUE),
      n = n()
    ) |> 
    ggplot(aes(x = minute, y = avg_delay)) +
    geom_line()
  
  sched_dep <- flights_dt |> 
    mutate(minute = minute(sched_dep_time)) |> 
    group_by(minute) |> 
    summarize(
      avg_delay = mean(arr_delay, na.rm = TRUE),
      n = n()
    )
  
  ggplot(sched_dep, aes(x = minute, y = avg_delay)) +
    geom_line()
  
  #17.3.2 Rounding
  flights_dt |> 
    count(week = floor_date(dep_time, "week")) |> 
    ggplot(aes(x = week, y = n)) +
    geom_line() + 
    geom_point()
  
  flights_dt |> 
    mutate(dep_hour = dep_time - floor_date(dep_time, "day")) |> 
    ggplot(aes(x = dep_hour)) +
    geom_freqpoly(binwidth = 60 * 30)
  #> Don't know how to automatically pick scale for object of type <difftime>.
  #> Defaulting to continuous.
  
  
  flights_dt |> 
    mutate(dep_hour = hms::as_hms(dep_time - floor_date(dep_time, "day"))) |> 
    ggplot(aes(x = dep_hour)) +
    geom_freqpoly(binwidth = 60 * 30)

  #17.3.3 Modifying components
  (datetime <- ymd_hms("2026-07-08 12:34:56"))
  #> [1] "2026-07-08 12:34:56 UTC"
  
  year(datetime) <- 2030
  datetime
  #> [1] "2030-07-08 12:34:56 UTC"
  month(datetime) <- 01
  datetime
  #> [1] "2030-01-08 12:34:56 UTC"
  hour(datetime) <- hour(datetime) + 1
  datetime
  #> [1] "2030-01-08 13:34:56 UTC"
  
  update(datetime, year = 2030, month = 2, mday = 2, hour = 2)
  #> [1] "2030-02-02 02:34:56 UTC"
  
  update(ymd("2023-02-01"), mday = 30)
  #> [1] "2023-03-02"
  update(ymd("2023-02-01"), hour = 400)
  #> [1] "2023-02-17 16:00:00 UTC"
  
#17.3.4 Exercises
#1. How does the distribution of flight times within a day change over the course of the year?
  flights_dt |> 
    mutate(month = month(dep_time, label = TRUE),
           hour = hour(dep_time)) |>
    ggplot(aes(x = hour)) +
    geom_freqpoly(binwidth = 1) +
    facet_wrap(~ month)
  
#Flight times shift slightly across seasons due to daylight changes and airline schedule adjustments, with summer having more early flights and winter more concentrated midday departures.
#2. Compare dep_time, sched_dep_time and dep_delay. Are they consistent? Explain your findings.
  flights_dt |> 
    mutate(delay_check = as.numeric(dep_time - sched_dep_time)) |> 
    summarize(
      match = mean(delay_check == dep_delay, na.rm = TRUE)
    )
#Comparing scheduled vs actual departure times reveals inconsistencies - some flights show negative delays (early departures) while others have significant positive delays, suggesting operational variability.
#3. Compare air_time with the duration between the departure and arrival. Explain your findings. (Hint: consider the location of the airport.)
  flights_dt |> 
    mutate(flight_duration = as.numeric(arr_time - dep_time),
           diff = flight_duration - air_time) |> 
    summarize(mean(diff, na.rm = TRUE))

  # you find that flight_duration > air_time
#4. How does the average delay time change over the course of a day? Should you use dep_time or sched_dep_time? Why?
  flights_dt |> 
    mutate(hour = hour(sched_dep_time)) |> 
    group_by(hour) |> 
    summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) |> 
    ggplot(aes(x = hour, y = avg_delay)) +
    geom_line()
  
#Average delays peak during busy morning and evening rush hours; sched_dep_time gives clearer patterns than actual dep_time since it represents planned operations.
#5. On what day of the week should you leave if you want to minimise the chance of a delay?
  flights_dt |> 
    mutate(wday = wday(dep_time, label = TRUE)) |> 
    group_by(wday) |> 
    summarize(avg_delay = mean(dep_delay, na.rm = TRUE)) |> 
    ggplot(aes(x = wday, y = avg_delay)) +
    geom_col()
# Tuesday and Wednesday typically have the lowest delay probabilities, while weekends and Fridays see more disruptions due to higher passenger volumes.
#6. What makes the distribution of diamonds$carat and flights$sched_dep_time similar?
# Both carat weights and scheduled departure times cluster at round numbers (whole/half carats and :00/:30 departure times) due to human preference for neat intervals.
#7. Confirm our hypothesis that the early departures of flights in minutes 20-30 and 50-60 are caused by scheduled flights that leave early. Hint: create a binary variable that tells you whether or not a flight was delayed.
  flights_dt |> 
    mutate(
      minute = minute(dep_time),
      delayed = dep_delay > 0
    ) |> 
    group_by(minute) |> 
    summarize(prop_early = mean(!delayed, na.rm = TRUE)) |> 
    ggplot(aes(x = minute, y = prop_early)) +
    geom_line()

#17.4 Time spans
#17.4.1 Durations
  # How old is Hadley?
  h_age <- today() - ymd("1979-10-14")
  h_age
  #> Time difference of 16622 days
  
  as.duration(h_age)
  #> [1] "1436140800s (~45.51 years)"
  
  dseconds(15)
  #> [1] "15s"
  dminutes(10)
  #> [1] "600s (~10 minutes)"
  dhours(c(12, 24))
  #> [1] "43200s (~12 hours)" "86400s (~1 days)"
  ddays(0:5)
  #> [1] "0s"                "86400s (~1 days)"  "172800s (~2 days)"
  #> [4] "259200s (~3 days)" "345600s (~4 days)" "432000s (~5 days)"
  dweeks(3)
  #> [1] "1814400s (~3 weeks)"
  dyears(1)
  #> [1] "31557600s (~1 years)"
  
  2 * dyears(1)
  #> [1] "63115200s (~2 years)"
  dyears(1) + dweeks(12) + dhours(15)
  #> [1] "38869200s (~1.23 years)"
  
  tomorrow <- today() + ddays(1)
  last_year <- today() - dyears(1)
  
  one_am <- ymd_hms("2026-03-08 01:00:00", tz = "America/New_York")
  
  one_am
  #> [1] "2026-03-08 01:00:00 EST"
  one_am + ddays(1)
  #> [1] "2026-03-09 02:00:00 EDT"
  
#17.4.2 Periods
  one_am
  #> [1] "2026-03-08 01:00:00 EST"
  one_am + days(1)
  #> [1] "2026-03-09 01:00:00 EDT"
  
  hours(c(12, 24))
  #> [1] "12H 0M 0S" "24H 0M 0S"
  days(7)
  #> [1] "7d 0H 0M 0S"
  months(1:6)
  #> [1] "1m 0d 0H 0M 0S" "2m 0d 0H 0M 0S" "3m 0d 0H 0M 0S" "4m 0d 0H 0M 0S"
  #> [5] "5m 0d 0H 0M 0S" "6m 0d 0H 0M 0S"
  
  
  10 * (months(6) + days(1))
  #> [1] "60m 10d 0H 0M 0S"
  days(50) + hours(25) + minutes(2)
  #> [1] "50d 25H 2M 0S"
  
  # A leap year
  ymd("2024-01-01") + dyears(1)
  #> [1] "2024-12-31 06:00:00 UTC"
  ymd("2024-01-01") + years(1)
  #> [1] "2025-01-01"
  
  # Daylight saving time
  one_am + ddays(1)
  #> [1] "2026-03-09 02:00:00 EDT"
  one_am + days(1)
  #> [1] "2026-03-09 01:00:00 EDT"
  
  flights_dt |> 
    filter(arr_time < dep_time) 
  
  flights_dt <- flights_dt |> 
    mutate(
      overnight = arr_time < dep_time,
      arr_time = arr_time + days(overnight),
      sched_arr_time = sched_arr_time + days(overnight)
    )
  
  flights_dt |> 
    filter(arr_time < dep_time) 
  
#17.4.3 Intervals
  years(1) / days(1)
  #> [1] 365.25
  y2023 <- ymd("2023-01-01") %--% ymd("2024-01-01")
  y2024 <- ymd("2024-01-01") %--% ymd("2025-01-01")
  
  y2023
  #> [1] 2023-01-01 UTC--2024-01-01 UTC
  y2024
  #> [1] 2024-01-01 UTC--2025-01-01 UTC
  
  y2023 / days(1)
  #> [1] 365
  y2024 / days(1)
  #> [1] 366

#17.4.4 Exercises
#1. Explain days(!overnight) and days(overnight) to someone who has just started learning R. What is the key fact you need to know?
    #The days(overnight) syntax adds one day when overnight is TRUE (for flights crossing midnight), while days(!overnight) adds zero days for daytime flights. The key is understanding that TRUE becomes 1 and FALSE becomes 0 in numeric conversion.
  
#2.Create a vector of dates giving the first day of every month in 2015. Create a vector of dates giving the first day of every month in the current year.
  # 2015
  first_2015 <- ymd("2015-01-01") + months(0:11)
  first_2015
  
  # Current year
  first_current <- ymd(paste0(year(today()), "-01-01")) + months(0:11)
  first_current
#3. Write a function that given your birthday (as a date), returns how old you are in years.
  how_old <- function(birthday) {
    interval(birthday, today()) / years(1)
  }
  
  # Example
  how_old(ymd("2000-01-01"))
#4.Why can’t (today() %--% (today() + years(1))) / months(1) work?  
  The interval division fails because months have varying lengths - unlike days/years, we can't get an exact count of complete months in a year interval. Use as.period(interval) %/% months(1) instead for approximate results.

#17.5 Time zones
  Sys.timezone()
  #> [1] "UTC"
  #> length(OlsonNames())
  #> [1] 598
  head(OlsonNames())
  #> [1] "Africa/Abidjan"     "Africa/Accra"       "Africa/Addis_Ababa"
  #> [4] "Africa/Algiers"     "Africa/Asmara"      "Africa/Asmera"
  
  x1 <- ymd_hms("2024-06-01 12:00:00", tz = "America/New_York")
  x1
  #> [1] "2024-06-01 12:00:00 EDT"
  
  x2 <- ymd_hms("2024-06-01 18:00:00", tz = "Europe/Copenhagen")
  x2
  #> [1] "2024-06-01 18:00:00 CEST"
  
  x3 <- ymd_hms("2024-06-02 04:00:00", tz = "Pacific/Auckland")
  x3
  #> [1] "2024-06-02 04:00:00 NZST"
  
  x1 - x2
  #> Time difference of 0 secs
  x1 - x3
  #> Time difference of 0 secs
  
  x4 <- c(x1, x2, x3)
  x4
  #> [1] "2024-06-01 12:00:00 EDT" "2024-06-01 12:00:00 EDT"
  #> [3] "2024-06-01 12:00:00 EDT"
  
  x4a <- with_tz(x4, tzone = "Australia/Lord_Howe")
  x4a
  #> [1] "2024-06-02 02:30:00 +1030" "2024-06-02 02:30:00 +1030"
  #> [3] "2024-06-02 02:30:00 +1030"
  x4a - x4
  #> Time differences in secs
  #> [1] 0 0 0
  
  x4b <- force_tz(x4, tzone = "Australia/Lord_Howe")
  x4b
  #> [1] "2024-06-01 12:00:00 +1030" "2024-06-01 12:00:00 +1030"
  #> [3] "2024-06-01 12:00:00 +1030"
  x4b - x4
  #> Time differences in hours
  #> [1] -14.5 -14.5 -14.5