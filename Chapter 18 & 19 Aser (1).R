# Chapter 18 & 19.............

# Chapter 18  Missing values...........

# 18.1.1 Prerequisites.........

library(tidyverse)

# 18.2 Explicit missing values.........

# 18.2.1 Last observation carried forward......

treatment <- tribble(
  ~person,           ~treatment, ~response,
  "Derrick Whitmore", 1,         7,
  NA,                 2,         10,
  NA,                 3,         NA,
  "Katherine Burke",  1,         4
)

treatment |>
  fill(everything())

# 18.2.2 Fixed values.........

x <- c(1, 4, 5, 7, NA)
coalesce(x, 0)

x <- c(1, 4, 5, 7, -99)
na_if(x, -99)

# 18.2.3 NaN.......

x <- c(NA, NaN)
x * 10

x == 1

is.na(x)

0 / 0 

0 * Inf

Inf - Inf

sqrt(-1)

# 18.3 Implicit missing values........

stocks <- tibble(
  year  = c(2020, 2020, 2020, 2020, 2021, 2021, 2021),
  qtr   = c(   1,    2,    3,    4,    2,    3,    4),
  price = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

# 18.3.1 Pivoting........

stocks |>
  pivot_wider(
    names_from = qtr, 
    values_from = price
  )

# 18.3.2 Complete..............

stocks |>
  complete(year, qtr)

stocks |>
  complete(year = 2019:2021, qtr)

# 18.3.3 Joins...........

library(nycflights13)

flights |> 
  distinct(faa = dest) |> 
  anti_join(airports)

flights |> 
  distinct(tailnum) |> 
  anti_join(planes)

# 18.4 Factors and empty groups............

health <- tibble(
  name   = c("Ikaia", "Oletta", "Leriah", "Dashay", "Tresaun"),
  smoker = factor(c("no", "no", "no", "no", "no"), levels = c("yes", "no")),
  age    = c(34, 88, 75, 47, 56),
)

health |> count(smoker)

health |> count(smoker, .drop = FALSE)

ggplot(health, aes(x = smoker)) +
  geom_bar() +
  scale_x_discrete()

ggplot(health, aes(x = smoker)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)

health |> 
  group_by(smoker, .drop = FALSE) |> 
  summarize(
    n = n(),
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  )

x1 <- c(NA, NA)
length(x1)

x2 <- numeric()
length(x2)

health |> 
  group_by(smoker) |> 
  summarize(
    n = n(),
    mean_age = mean(age),
    min_age = min(age),
    max_age = max(age),
    sd_age = sd(age)
  ) |> 
  complete(smoker)


# Chapter 19  Joins.............

# 19.1.1 Prerequisites.............

library(tidyverse)
library(nycflights13)

# 19.2 Keys..............

# 19.2.1 Primary and foreign keys...........

airlines

airports

planes

weather

# 19.2.2 Checking primary keys............

planes |> 
  count(tailnum) |> 
  filter(n > 1)

weather |> 
  count(time_hour, origin) |> 
  filter(n > 1)

planes |> 
  filter(is.na(tailnum))

weather |> 
  filter(is.na(time_hour) | is.na(origin))

# 19.2.3 Surrogate keys.............

flights |> 
  count(time_hour, carrier, flight) |> 
  filter(n > 1)

airports |>
  count(alt, lat) |> 
  filter(n > 1)

flights2 <- flights |> 
  mutate(id = row_number(), .before = 1)
flights2

# 19.3 Basic joins..........

# 19.3.1 Mutating joins............

flights2 <- flights |> 
  select(year, time_hour, origin, dest, tailnum, carrier)
flights2

flights2 |>
  left_join(airlines)

flights2 |> 
  left_join(weather |> select(origin, time_hour, temp, wind_speed))

flights2 |> 
  left_join(planes |> select(tailnum, type, engines, seats))

flights2 |> 
  filter(tailnum == "N3ALAA") |> 
  left_join(planes |> select(tailnum, type, engines, seats))

# 19.3.2 Specifying join keys............

flights2 |> 
  left_join(planes)

flights2 |> 
  left_join(planes, join_by(tailnum))

flights2 |> 
  left_join(airports, join_by(dest == faa))

flights2 |> 
  left_join(airports, join_by(origin == faa))

# 19.3.3 Filtering joins...........

airports |> 
  semi_join(flights2, join_by(faa == origin))

airports |> 
  semi_join(flights2, join_by(faa == dest))

flights2 |> 
  anti_join(airports, join_by(dest == faa)) |> 
  distinct(dest)

flights2 |>
  anti_join(planes, join_by(tailnum)) |> 
  distinct(tailnum)

# 19.4 How do joins work?........

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)

# 19.4.1 Row matching..........

df1 <- tibble(key = c(1, 2, 2), val_x = c("x1", "x2", "x3"))
df2 <- tibble(key = c(1, 2, 2), val_y = c("y1", "y2", "y3"))

df1 |> 
  inner_join(df2, join_by(key))

# 19.5 Non-equi joins........

x |> inner_join(y, join_by(key == key), keep = TRUE)

# 19.5.1 Cross joins........

df <- tibble(name = c("John", "Simon", "Tracy", "Max"))
df |> cross_join(df)

# 19.5.2 Inequality joins...........

df <- tibble(id = 1:4, name = c("John", "Simon", "Tracy", "Max"))

df |> inner_join(df, join_by(id < id))

# 19.5.3 Rolling joins..............

parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03"))
)

set.seed(123)
employees <- tibble(
  name = sample(babynames::babynames$name, 100),
  birthday = ymd("2022-01-01") + (sample(365, 100, replace = TRUE) - 1)
)
employees

employees |> 
  left_join(parties, join_by(closest(birthday >= party)))

employees |> 
  anti_join(parties, join_by(closest(birthday >= party)))

# 19.5.4 Overlap joins............

parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03")),
  start = ymd(c("2022-01-01", "2022-04-04", "2022-07-11", "2022-10-03")),
  end = ymd(c("2022-04-03", "2022-07-11", "2022-10-02", "2022-12-31"))
)
parties

parties |> 
  inner_join(parties, join_by(overlaps(start, end, start, end), q < q)) |> 
  select(start.x, end.x, start.y, end.y)

parties <- tibble(
  q = 1:4,
  party = ymd(c("2022-01-10", "2022-04-04", "2022-07-11", "2022-10-03")),
  start = ymd(c("2022-01-01", "2022-04-04", "2022-07-11", "2022-10-03")),
  end = ymd(c("2022-04-03", "2022-07-10", "2022-10-02", "2022-12-31"))
)

employees |> 
  inner_join(parties, join_by(between(birthday, start, end)), unmatched = "error")




