
#Chapter 12 and 13.....................

#Chapter 12: Logical vectors....................

#12.1.1 Prerequisites..............

library(tidyverse)
library(nycflights13)

x <- c(1, 2, 3, 5, 7, 11, 13)
x * 2

df <- tibble(x)
df |> 
  mutate(y = x * 2)

#12.2 Comparisons..................

flights |> 
  filter(dep_time > 600 & dep_time < 2000 & abs(arr_delay) < 20)

flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
    .keep = "used"
  )

flights |> 
  mutate(
    daytime = dep_time > 600 & dep_time < 2000,
    approx_ontime = abs(arr_delay) < 20,
  ) |> 
  filter(daytime & approx_ontime)

#12.2.1 Floating point comparison...............

x <- c(1 / 49 * 49, sqrt(2) ^ 2)
x

x == c(1, 2)

print(x, digits = 16)

near(x, c(1, 2))

#12.2.2 Missing values.................

NA > 5

10 == NA

NA == NA

# We don't know how old Mary is...........
age_mary <- NA

# We don't know how old John is..........
age_john <- NA

# Are Mary and John the same age?............
age_mary == age_john
# We don't know!

flights |> 
  filter(dep_time == NA)

#12.2.3 is.na()............ 

is.na(c(TRUE, NA, FALSE))

is.na(c(1, NA, 3))

is.na(c("a", NA, "b"))

flights |> 
  filter(is.na(dep_time))

flights |> 
  filter(month == 1, day == 1) |> 
  arrange(dep_time)

flights |> 
  filter(month == 1, day == 1) |> 
  arrange(desc(is.na(dep_time)), dep_time)

#12.3.1 Missing values......................

df <- tibble(x = c(TRUE, FALSE, NA))

df |> 
  mutate(
    and = x & NA,
    or = x | NA
  )

#12.3.2 Order of operations...................

flights |> 
  filter(month == 11 | month == 12)

flights |> 
  filter(month == 11 | 12)

flights |> 
  mutate(
    nov = month == 11,
    final = nov | 12,
    .keep = "used"
  )

#12.3.3 %in%.............. 

1:12 %in% c(1, 5, 11)

letters[1:10] %in% c("a", "e", "i", "o", "u")

flights |> 
  filter(month %in% c(11, 12))

c(1, 2, NA) == NA

c(1, 2, NA) %in% NA

flights |> 
  filter(dep_time %in% c(NA, 0800))

#12.4.1 Logical summaries.................

flights |> 
  group_by(year, month, day) |> 
  summarize(
    all_delayed = all(dep_delay <= 60, na.rm = TRUE),
    any_long_delay = any(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )

#12.4.2 Numeric summaries of logical vectors................

flights |> 
  group_by(year, month, day) |> 
  summarize(
    proportion_delayed = mean(dep_delay <= 60, na.rm = TRUE),
    count_long_delay = sum(arr_delay >= 300, na.rm = TRUE),
    .groups = "drop"
  )

#12.4.3 Logical subsetting................

flights |> 
  filter(arr_delay > 0) |> 
  group_by(year, month, day) |> 
  summarize(
    behind = mean(arr_delay),
    n = n(),
    .groups = "drop"
  )

flights |> 
  group_by(year, month, day) |> 
  summarize(
    behind = mean(arr_delay[arr_delay > 0], na.rm = TRUE),
    ahead = mean(arr_delay[arr_delay < 0], na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

#12.5.1 if_else()..................

x <- c(-3:3, NA)
if_else(x > 0, "+ve", "-ve")

if_else(x > 0, "+ve", "-ve", "???")

if_else(x < 0, -x, x)

x1 <- c(NA, 1, 2, NA)
y1 <- c(3, NA, 4, 6)
if_else(is.na(x1), y1, x1)

if_else(x == 0, "0", if_else(x < 0, "-ve", "+ve"), "???")

#12.5.2 case_when()................

x <- c(-3:3, NA)
case_when(
  x == 0   ~ "0",
  x < 0    ~ "-ve", 
  x > 0    ~ "+ve",
  is.na(x) ~ "???"
)

case_when(
  x < 0 ~ "-ve",
  x > 0 ~ "+ve"
)

case_when(
  x < 0 ~ "-ve",
  x > 0 ~ "+ve",
  .default = "???"
)

case_when(
  x > 0 ~ "+ve",
  x > 2 ~ "big"
)

flights |> 
  mutate(
    status = case_when(
      is.na(arr_delay)      ~ "cancelled",
      arr_delay < -30       ~ "very early",
      arr_delay < -15       ~ "early",
      abs(arr_delay) <= 15  ~ "on time",
      arr_delay < 60        ~ "late",
      arr_delay < Inf       ~ "very late",
    ),
    .keep = "used"
  )

#12.5.3 Compatible types..................

if_else(TRUE, "a", 1)

case_when(
  x < -1 ~ TRUE,  
  x > 0  ~ now()
)

#Chapter 13  Numbers.........

#13.1.1 Prerequisites..........

library(tidyverse)
library(nycflights13)

#13.2 Making numbers........

x <- c("1.2", "5.6", "1e3")
parse_double(x)

x <- c("$1,234", "USD 3,513", "59%")
parse_number(x)

#13.3 Counts.........

flights |> count(dest)

flights |> count(dest, sort = TRUE)

flights |> 
  group_by(dest) |> 
  summarize(
    n = n(),
    delay = mean(arr_delay, na.rm = TRUE)
  )

n()

flights |> 
  group_by(dest) |> 
  summarize(carriers = n_distinct(carrier)) |> 
  arrange(desc(carriers))

flights |> 
  group_by(tailnum) |> 
  summarize(miles = sum(distance))

flights |> count(tailnum, wt = distance)

flights |> 
  group_by(dest) |> 
  summarize(n_cancelled = sum(is.na(dep_time))) 

#13.4 Numeric transformations.......

#13.4.1 Arithmetic and recycling rules........

x <- c(1, 2, 10, 20)
x / 5

x / c(5, 5, 5, 5)

x * c(1, 2)

x * c(1, 2, 3)

flights |> 
  filter(month == c(1, 2))

#13.4.2 Minimum and maximum............

df <- tribble(
  ~x, ~y,
  1,  3,
  5,  2,
  7, NA,
)

df |> 
  mutate(
    min = pmin(x, y, na.rm = TRUE),
    max = pmax(x, y, na.rm = TRUE)
  )

df |> 
  mutate(
    min = min(x, y, na.rm = TRUE),
    max = max(x, y, na.rm = TRUE)
  )

#13.4.3 Modular arithmetic.........

1:10 %/% 3

1:10 %% 3

flights |> 
  mutate(
    hour = sched_dep_time %/% 100,
    minute = sched_dep_time %% 100,
    .keep = "used"
  )

flights |> 
  group_by(hour = sched_dep_time %/% 100) |> 
  summarize(prop_cancelled = mean(is.na(dep_time)), n = n()) |> 
  filter(hour > 1) |> 
  ggplot(aes(x = hour, y = prop_cancelled)) +
  geom_line(color = "grey50") + 
  geom_point(aes(size = n))


#13.4.5 Rounding........

round(123.456)

round(123.456, 2)

round(123.456, 1)

round(123.456, -1)

round(123.456, -2)

round(c(1.5, 2.5))

x <- 123.456

floor(x)

ceiling(x)

# Round down to nearest two digits
floor(x / 0.01) * 0.01

# Round up to nearest two digits
ceiling(x / 0.01) * 0.01

# Round to nearest multiple of 4
round(x / 4) * 4

# Round to nearest 0.25
round(x / 0.25) * 0.25

#13.4.6 Cutting numbers into ranges.........

x <- c(1, 2, 5, 10, 15, 20)
cut(x, breaks = c(0, 5, 10, 15, 20))

cut(x, breaks = c(0, 5, 10, 100))

cut(x, 
    breaks = c(0, 5, 10, 15, 20), 
    labels = c("sm", "md", "lg", "xl")
)

y <- c(NA, -10, 5, 10, 30)
cut(y, breaks = c(0, 5, 10, 15, 20))


#13.4.7 Cumulative and rolling aggregates............

x <- 1:10
cumsum(x)

#13.5 General transformations............

#13.5.1 Ranks.....

x <- c(1, 2, 2, 3, 4, NA)
min_rank(x)

min_rank(desc(x))

df <- tibble(x = x)
df |> 
  mutate(
    row_number = row_number(x),
    dense_rank = dense_rank(x),
    percent_rank = percent_rank(x),
    cume_dist = cume_dist(x)
  )

df <- tibble(id = 1:10)

df |> 
  mutate(
    row0 = row_number() - 1,
    three_groups = row0 %% 3,
    three_in_each_group = row0 %/% 3
  )

#13.5.2 Offsets..........

x <- c(2, 5, 11, 11, 19, 35)
lag(x)

lead(x)

x - lag(x)

x == lag(x)


#13.5.3 Consecutive identifiers.......

events <- tibble(
  time = c(0, 1, 2, 3, 5, 10, 12, 15, 17, 19, 20, 27, 28, 30)
)

events <- events |> 
  mutate(
    diff = time - lag(time, default = first(time)),
    has_gap = diff >= 5
  )
events

events |> mutate(
  group = cumsum(has_gap)
)

df <- tibble(
  x = c("a", "a", "a", "b", "c", "c", "d", "e", "a", "a", "b", "b"),
  y = c(1, 2, 3, 2, 4, 1, 3, 9, 4, 8, 10, 199)
)

df |> 
  group_by(id = consecutive_id(x)) |> 
  slice_head(n = 1)


#13.6 Numeric summaries..........

#13.6.1 Center...........

flights |>
  group_by(year, month, day) |>
  summarize(
    mean = mean(dep_delay, na.rm = TRUE),
    median = median(dep_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = mean, y = median)) + 
  geom_abline(slope = 1, intercept = 0, color = "white", linewidth = 2) +
  geom_point()


#13.6.2 Minimum, maximum, and quantiles.............

flights |>
  group_by(year, month, day) |>
  summarize(
    max = max(dep_delay, na.rm = TRUE),
    q95 = quantile(dep_delay, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

#13.6.3 Spread..............

flights |> 
  group_by(origin, dest) |> 
  summarize(
    distance_iqr = IQR(distance), 
    n = n(),
    .groups = "drop"
  ) |> 
  filter(distance_iqr > 0)


#13.6.4 Distributions..........

flights |>
  filter(dep_delay < 120) |> 
  ggplot(aes(x = dep_delay, group = interaction(day, month))) + 
  geom_freqpoly(binwidth = 5, alpha = 1/5)


#13.6.5 Positions............

flights |> 
  group_by(year, month, day) |> 
  summarize(
    first_dep = first(dep_time, na_rm = TRUE), 
    fifth_dep = nth(dep_time, 5, na_rm = TRUE),
    last_dep = last(dep_time, na_rm = TRUE)
  )

flights |> 
  group_by(year, month, day) |> 
  mutate(r = min_rank(sched_dep_time)) |> 
  filter(r %in% c(1, max(r)))








