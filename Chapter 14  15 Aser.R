

# Chapter 14 and 15.................

#Chapter 14  Strings..............

#14.1.1 Prerequisites..............

library(tidyverse)
library(babynames)

#14.2 Creating a string.............

string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'

#14.2.1 Escapes.........

double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'"

backslash <- "\\"

x <- c(single_quote, double_quote, backslash)
x


str_view(x)


#14.2.2 Raw strings................

tricky <- "double_quote <- \"\\\"\" # or '\"'
single_quote <- '\\'' # or \"'\""
str_view(tricky)

tricky <- r"(double_quote <- "\"" # or '"'
single_quote <- '\'' # or "'")"
str_view(tricky)

#To eliminate the escaping, you can instead use a raw string

tricky <- r"(double_quote <- "\"" # or '"'

single_quote <- '\' # or "")"

#r"(  your string goes here )"
#r"[ your string if it includes ()]"
#r"{ your string if it includes []}"


#14.2.3 Other special characters........

x <- c("one\ntwo", "one\ttwo", "\u00b5", "\U0001f604")
x

#14.3 Creating many strings from data........

#14.3.1 str_c()............

str_c("x", "y")

str_c("x", "y", "z")

str_c("Hello ", c("John", "Susan"))

df <- tibble(name = c("Flora", "David", "Terra", NA))
df |> mutate(greeting = str_c("Hi ", name, "!"))

df |> 
  mutate(
    greeting1 = str_c("Hi ", coalesce(name, "you"), "!"),
    greeting2 = coalesce(str_c("Hi ", name, "!"), "Hi!")
  )

#14.3.2 str_glue()..........

df |> mutate(greeting = str_glue("Hi {name}!"))

df |> mutate(greeting = str_glue("{{Hi {name}!}}"))

#14.3.3 str_flatten()..........

str_flatten(c("x", "y", "z"))

str_flatten(c("x", "y", "z"), ", ")

str_flatten(c("x", "y", "z"), ", ", last = ", and ")

df <- tribble(
  ~ name, ~ fruit,
  "Carmen", "banana",
  "Carmen", "apple",
  "Marvin", "nectarine",
  "Terence", "cantaloupe",
  "Terence", "papaya",
  "Terence", "mandarin"
)
df |>
  group_by(name) |> 
  summarize(fruits = str_flatten(fruit, ", "))

#14.4 Extracting data from strings............

#14.4.1 Separating into rows.........

df1 <- tibble(x = c("a,b,c", "d,e", "f"))
df1 |> 
  separate_longer_delim(x, delim = ",")

df2 <- tibble(x = c("1211", "131", "21"))
df2 |> 
  separate_longer_position(x, width = 1)


#14.4.2 Separating into columns.........

df3 <- tibble(x = c("a10.1.2022", "b10.2.2011", "e15.1.2015"))
df3 |> 
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", "edition", "year")
  )

df3 |> 
  separate_wider_delim(
    x,
    delim = ".",
    names = c("code", NA, "year")
  )

df4 <- tibble(x = c("202215TX", "202122LA", "202325CA")) 
df4 |> 
  separate_wider_position(
    x,
    widths = c(year = 4, age = 2, state = 2)
  )

#14.4.3 Diagnosing widening problems...........

df <- tibble(x = c("1-1-1", "1-1-2", "1-3", "1-3-2", "1"))

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z")
  )

debug <- df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_few = "debug"
  )

debug

debug |> filter(!x_ok)

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_few = "align_start"
  )

df <- tibble(x = c("1-1-1", "1-1-2", "1-3-5-6", "1-3-2", "1-3-5-7-9"))

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z")
  )

debug <- df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "debug"
  )

debug |> filter(!x_ok)

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "drop"
  )

df |> 
  separate_wider_delim(
    x,
    delim = "-",
    names = c("x", "y", "z"),
    too_many = "merge"
  )

#14.5 Letters........

#14.5.1 Length.............

str_length(c("a", "R for data science", NA))

babynames |>
  count(length = str_length(name), wt = n)

babynames |> 
  filter(str_length(name) == 15) |> 
  count(name, wt = n, sort = TRUE)

#14.5.2 Subsetting........

x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)

str_sub(x, -3, -1)

str_sub("a", 1, 5)

babynames |> 
  mutate(
    first = str_sub(name, 1, 1),
    last = str_sub(name, -1, -1)
  )

#14.6 Non-English text...........

#14.6.1 Encoding..............

charToRaw("Hadley")

x1 <- "text\nEl Ni\xf1o was particularly bad this year"
read_csv(x1)$text

x2 <- "text\n\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"
read_csv(x2)$text

read_csv(x1, locale = locale(encoding = "Latin1"))$text

read_csv(x2, locale = locale(encoding = "Shift-JIS"))$text


#14.6.2 Letter variations.........


u <- c("\u00fc", "u\u0308")
str_view(u)

str_length(u)

str_sub(u, 1, 1)

u[[1]] == u[[2]]

str_equal(u[[1]], u[[2]])


#14.6.3 Locale-dependent functions.............

str_to_upper(c("i", "ı"))

str_to_upper(c("i", "ı"), locale = "tr")

str_sort(c("a", "c", "ch", "h", "z"))

str_sort(c("a", "c", "ch", "h", "z"), locale = "cs")


# Chapter 15  Regular expressions...............

#15.1.1 Prerequisites

library(tidyverse)
library(babynames)

#15.2 Pattern basics...........

str_view(fruit, "berry")

str_view(c("a", "ab", "ae", "bd", "ea", "eab"), "a.")

str_view(fruit, "a...e")

str_view(c("a", "ab", "abb"), "ab?")

str_view(c("a", "ab", "abb"), "ab+")

str_view(c("a", "ab", "abb"), "ab*")

str_view(words, "[aeiou]x[aeiou]")

str_view(words, "[^aeiou]y[^aeiou]")

str_view(fruit, "apple|melon|nut")

str_view(fruit, "aa|ee|ii|oo|uu")


# 15.3 Key functions................


#15.3.1 Detect matches........

str_detect(c("a", "b", "c"), "[aeiou]")

babynames |> 
  filter(str_detect(name, "x")) |> 
  count(name, wt = n, sort = TRUE)

babynames |> 
  group_by(year) |> 
  summarize(prop_x = mean(str_detect(name, "x"))) |> 
  ggplot(aes(x = year, y = prop_x)) + 
  geom_line()

# 15.3.2 Count matches..........

x <- c("apple", "banana", "pear")
str_count(x, "p")

str_count("abababa", "aba")

str_view("abababa", "aba")

babynames |> 
  count(name) |> 
  mutate(
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )

babynames |> 
  count(name) |> 
  mutate(
    name = str_to_lower(name),
    vowels = str_count(name, "[aeiou]"),
    consonants = str_count(name, "[^aeiou]")
  )

# 15.3.3 Replace values...................

x <- c("apple", "pear", "banana")
str_replace_all(x, "[aeiou]", "-")

x <- c("apple", "pear", "banana")
str_remove_all(x, "[aeiou]")


# 15.3.4 Extract variables............

df <- tribble(
  ~str,
  "<Sheryl>-F_34",
  "<Kisha>-F_45", 
  "<Brandon>-N_33",
  "<Sharon>-F_38", 
  "<Penny>-F_58",
  "<Justin>-M_41", 
  "<Patricia>-F_84", 
)

df |> 
  separate_wider_regex(
    str,
    patterns = c(
      "<", 
      name = "[A-Za-z]+", 
      ">-", 
      gender = ".",
      "_",
      age = "[0-9]+"
    )
  )

# 15.4 Pattern details..............

# 15.4.1 Escaping............

# To create the regular expression \., we need to use \\.
dot <- "\\."

# But the expression itself only contains one \
str_view(dot)
#> [1] │ \.

# And this tells R to look for an explicit .
str_view(c("abc", "a.c", "bef"), "a\\.c")
#> [2] │ <a.c>

x <- "a\\b"
str_view(x)
#> [1] │ a\b
str_view(x, "\\\\")
#> [1] │ a<\>b

str_view(x, r"{\\}")

str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")

str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")

# 15.4.2 Anchors..................

str_view(fruit, "^a")

str_view(fruit, "a$")

str_view(fruit, "apple")

str_view(fruit, "^apple$")

x <- c("summary(x)", "summarize(df)", "rowsum(x)", "sum(x)")
str_view(x, "sum")

str_view(x, "\\bsum\\b")

str_view("abc", c("$", "^", "\\b"))

str_replace_all("abc", c("$", "^", "\\b"), "--")


# 15.4.3 Character classes...............

x <- "abcd ABCD 12345 -!@#%."
str_view(x, "[abc]+")

str_view(x, "[a-z]+")

str_view(x, "[^a-z0-9]+")

str_view("a-b-c", "[a-c]")

str_view("a-b-c", "[a\\-c]")

x <- "abcd ABCD 12345 -!@#%."
str_view(x, "\\d+")

str_view(x, "\\D+")

str_view(x, "\\s+")

str_view(x, "\\S+")

str_view(x, "\\w+")

str_view(x, "\\W+")


# 15.4.6 Grouping and capturing............

str_view(fruit, "(..)\\1")

str_view(words, "^(..).*\\1$")

sentences |> 
  str_replace("(\\w+) (\\w+) (\\w+)", "\\1 \\3 \\2") |> 
  str_view()

sentences |> 
  str_match("the (\\w+) (\\w+)") |> 
  head()

sentences |> 
  str_match("the (\\w+) (\\w+)") |> 
  as_tibble(.name_repair = "minimal") |> 
  set_names("match", "word1", "word2")

x <- c("a gray cat", "a grey dog")
str_match(x, "gr(e|a)y")

str_match(x, "gr(?:e|a)y")


# 15.5 Pattern control...................

# 15.5.1 Regex flags................

bananas <- c("banana", "Banana", "BANANA")
str_view(bananas, "banana")

str_view(bananas, regex("banana", ignore_case = TRUE))

x <- "Line 1\nLine 2\nLine 3"
str_view(x, ".Line")
str_view(x, regex(".Line", dotall = TRUE))

x <- "Line 1\nLine 2\nLine 3"
str_view(x, "^Line")

str_view(x, regex("^Line", multiline = TRUE))

phone <- regex(
  r"(
    \(?     # optional opening parens
    (\d{3}) # area code
    [)\-]?  # optional closing parens or dash
    \ ?     # optional space
    (\d{3}) # another three numbers
    [\ -]?  # optional space or dash
    (\d{4}) # four more numbers
  )", 
  comments = TRUE
)

str_extract(c("514-791-8141", "(123) 456 7890", "123456"), phone)


# 15.5.2 Fixed matches...................

str_view(c("", "a", "."), fixed("."))

str_view("x X", "X")

str_view("x X", fixed("X", ignore_case = TRUE))

str_view("i İ ı I", fixed("İ", ignore_case = TRUE))

str_view("i İ ı I", coll("İ", ignore_case = TRUE, locale = "tr"))


#15.6 Practice...................

str_view(sentences, "^The")

str_view(sentences, "^The\\b")

str_view(sentences, "^She|He|It|They\\b")

str_view(sentences, "^(She|He|It|They)\\b")

pos <- c("He is a boy", "She had a good time")
neg <- c("Shells come from the sea", "Hadley said 'It's a great day'")

pattern <- "^(She|He|It|They)\\b"
str_detect(pos, pattern)

str_detect(neg, pattern)

# 15.6.2 Boolean operations.............

str_view(words, "^[^aeiou]+$")

str_view(words[!str_detect(words, "[aeiou]")])

str_view(words, "a.*b|b.*a")

words[str_detect(words, "a") & str_detect(words, "b")]

words[str_detect(words, "a.*e.*i.*o.*u")]
# ...
words[str_detect(words, "u.*o.*i.*e.*a")]

words[
  str_detect(words, "a") &
    str_detect(words, "e") &
    str_detect(words, "i") &
    str_detect(words, "o") &
    str_detect(words, "u")
]

# 15.6.3 Creating a pattern with code..........

str_view(sentences, "\\b(red|green|blue)\\b")

rgb <- c("red", "green", "blue")

str_c("\\b(", str_flatten(rgb, "|"), ")\\b")

str_view(colors())

cols <- colors()
cols <- cols[!str_detect(cols, "\\d")]
str_view(cols)

pattern <- str_c("\\b(", str_flatten(cols, "|"), ")\\b")
str_view(sentences, pattern)

# 15.7 Regular expressions in other places...........

# 15.7.2 Base R............

apropos("replace")

head(list.files(pattern = "\\.Rmd$"))

