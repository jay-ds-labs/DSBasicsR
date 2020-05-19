filename <- "murders.csv"
dir <- system.file("extdata", package = "dslabs") 
fullpath <- file.path(dir, filename)
file.copy(fullpath, "murders.csv")

library(tidyverse)
dat <- read_csv(filename)

system.file(package = "dslabs")
dir <- system.file(package = "dslabs")
list.files(path = dir)

filename %in% list.files(file.path(dir, "extdata")) 

read_lines("murders.csv", n_max = 3)
library(readxl)

url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat <- read_csv(url)
download.file(url, "murders.csv")

tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

url <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
dat2 = read_csv(url)
nrow(dat2)
ncol(dat)


library(tidyverse)
library(dslabs)
data(gapminder)

# create and inspect a tidy data frame
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

# plotting tidy data is simple
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# import and inspect example of original Gapminder data in wide format
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)

# RESHAPING DATA

# original wide data
library(tidyverse) 
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)

# tidy data from dslabs
library(dslabs)
data("gapminder")
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)

# gather wide data to make new tidy data
new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)

# gather all columns except country
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)
new_tidy_data
# gather treats column names as characters by default
class(tidy_data$year)
class(new_tidy_data$year)

# convert gathered column names to numeric
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)
new_tidy_data 
# ggplot works on new tidy data
new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# spread tidy data to generate wide data
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)


# SEPARATE & UNITE
# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

# separate on underscores
dat %>% separate(key, c("year", "variable_name"), "_")
dat %>% separate(key, c("year", "variable_name"))

# split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), 
                 fill = "right")

# split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")

# separate then spread
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value) 

# separate then unite
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_")

# full code for tidying data
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)


# Labs
View(co2)
co2
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_wide
co2_tidy <- gather(co2_wide,month,co2,-year)

co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

# lab
library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
dat

tmp <- gather(admissions, key, value, admitted:applicants)
tmp
tmp2 <- unite(tmp, column_name, c(key, gender))
tmp2


# JOINS
# import US murders data
library(tidyverse)
install.packages('ggrepel')
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)

# join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
head(tab)

# plot electoral votes versus population
tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() + 
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "lm", se = FALSE)

# make two smaller tables to demonstrate joins
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2

# experiment with different joins
left_join(tab_1, tab_2, by = "state")
tab1 %>% left_join(tab2, by = "state")
tab1 %>% right_join(tab2)
inner_join(tab1, tab2)
semi_join(tab1, tab2, by = "state")
anti_join(tab1, tab2, by = "state")

# binding
bind_cols(a = 1:3, b = 4:6)

tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1, tab2, tab3)
head(new_tab)

tab1 <- tab[1:2,]
tab2 <- tab[3:4,]
bind_rows(tab1, tab2)

# set operations
# intersect vectors or data frames
intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
intersect(tab1, tab2)

# perform a union of vectors or data frames
union(1:10, 6:15)
union(c("a","b","c"), c("b","c","d"))
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
union(tab1, tab2)

# set difference of vectors or data frames
setdiff(1:10, 6:15)
setdiff(6:15, 1:10)
tab1 <- tab[1:5,]
tab2 <- tab[3:7,]
setdiff(tab1, tab2)

# setequal determines whether sets have the same elements, regardless of order
setequal(1:5, 1:6)
setequal(1:5, 5:1)
setequal(tab1, tab2)

# lab
install.packages('Lahman')
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

Master %>% as_tibble()

data(Salaries)
head(Salaries)

data("AwardsPlayers")
head(AwardsPlayers)
top
award = AwardsPlayers %>% filter(yearID==2016) %>%pull(playerID)
award = data.frame(playerID = unique(award))
semi_join(top,award,by='playerID')
anti_join(award,top, by='playerID')
setdiff(top,award)


# WEB SCRAPING
# import a webpage into R
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab <- tab[[2]]

tab <- tab %>% html_table
class(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

# WEB SCRAPING 2
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

guacamole <- list(recipe, prep_time, ingredients)
guacamole

get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
} 

get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")


# WEB SCRAPING 3
url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Gun_violence_in_the_United_States_by_state",
              "&direction=prev&oldid=810166167")

library(tidyverse)
library(rvest)
h <- read_html(url)
class(h)
h
html_text(h)
tab <- h %>% html_nodes("table")
tab
tab[[1]]

tab <- tab[[1]] %>% html_table
class(tab)

library(jsonlite) # for more flexibility use rjson
citi_bike <- fromJSON("http://citibikenyc.com/stations/json")
citi_bike$executionTime
citi_bike$stationBeanList %>% as_tibble() 



# Lab
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
h
nodes <- html_nodes(h, "table")
nodes
length(nodes)

html_text(nodes[[8]])
html_table(nodes[[8]])

html_table(nodes[[1]])
html_table(nodes[[2]])
html_table(nodes[[3]])
html_table(nodes[[4]])

html_table(nodes[[21]])
html_table(nodes[[20]])
html_table(nodes[[19]])

tab_1 = html_table(nodes[[10]])
tab_2 = html_table(nodes[[19]])
tab_1 = tab_1 %>% setNames(tab_1[1,])
tab_1 = tab_1[-1,-1]
tab_2 = tab_2 %>% setNames(tab_2[1,])
tab_2 = tab_2[-1,]
tab_3 = tab_1 %>% full_join(tab_2, by = 'Team')

# Lab
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

h = read_html(url)
h
tab = html_nodes(h, "table")
length(tab)
head(html_table(tab[[1]], fill= T))
for(i in 1:40){
  t = html_table(tab[[i]], fill= T)
  if(names(t)[1]=='Date(s) conducted'){
    print(i)
    break
  }
}

head(html_table(tab[[5]], fill= T))
length(names(html_table(tab[[5]], fill= T)))

# STRING PROCESSING - REGEX EXAMPLE

pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_detect(s, pattern_without_groups) # generates just true or false. detection same with or without groups
str_detect(s, pattern_with_groups)
str_match(s, pattern_without_groups) # str match generates all the patterns that matched and the individual matching of the groups. it does that only with groups
str_match(s, pattern_with_groups)
str_extract(s, pattern_with_groups) # str_extract gives us the matched strings, it gives same result for with groups or without groups
str_extract(s, pattern_without_groups)


# Words to numbers
install.packages('english')
library(english)
words_to_numbers <- function(s){
  s <- str_to_lower(s)
  for(i in 0:11)
    s <- str_replace_all(s, words(i), as.character(i))
  s
}

words(1)
as.character(1)


# STRING PROCESSING - CASE 1 - MURDERS
library(tidyverse)
library(stringr) # all functions start with str_ and they take the string getting processesd as the first argument

library(rvest)
url <- paste0("https://en.wikipedia.org/w/index.php?title=",
              "Gun_violence_in_the_United_States_by_state",
              "&direction=prev&oldid=810166167")
murders_raw <- read_html(url) %>%
  html_node("table") %>%
  html_table() %>%
  setNames(c("state", "population", "total", "murder_rate"))

library(dslabs)
data(murders)

View(murders_raw)
str(murders_raw)
murders_raw$population[1:3]
commas <- function(x) any(str_detect(x, ",")) # any returns true if any value of a vector is true
murders_raw %>% summarize_all(commas)

test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)
test_2 <- parse_number(murders_raw$population) # does the same thing as removing , to convert to number
identical(test_1, test_2)

murders_new = murders_raw %>% mutate_at(2:3, parse_number)
head(murders_new)


# STRING PROCESSING - CASE 2 - HEIGHTS
data(reported_heights)
str(reported_heights)
as.numeric(reported_heights$height)
sum(is.na(reported_heights$height))
sum(is.na(as.numeric(reported_heights$height)))
head(reported_heights[which(is.na(as.numeric(reported_heights$height))),])

not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}   # returns True or False

library(english)
words_to_numbers <- function(s){
  s <- str_to_lower(s)
  for(i in 0:11)
    s <- str_replace_all(s, words(i), as.character(i))
  s
}

convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% 
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% 
    str_replace("^([56])'?$", "\\1'0") %>% 
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>%  
    str_trim() 
}

pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12 * feet + inches) %>%
  mutate(height = case_when(
    is.na(height) ~ as.numeric(NA),
    between(height, smallest, tallest) ~ height,  #inches
    between(height/2.54, smallest, tallest) ~ height/2.54, #cm
    between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    TRUE ~ as.numeric(NA))) %>%
  mutate(height = ifelse(is.na(height) & 
                           inches < 12 & between(guess, smallest, tallest),
                         guess, height)) %>%
  select(-guess)

new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

new_heights %>% arrange(height) %>% head(n=7)

# CASE STUDY 3: EXTRACTING TABLES FROM A PDF
install.packages('pdftools')
library("pdftools")
temp_file <- tempfile()
url <- paste0("https://www.pnas.org/content/suppl/2015/09/16/",
              "1510159112.DCSupplemental/pnas.201510159SI.pdf")
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

raw_data_research_funding_rates <- txt[2]
raw_data_research_funding_rates
tab <- str_split(raw_data_research_funding_rates, "\n")
tab
tab <- tab[[1]]
the_names_1 <- tab[3]
the_names_2 <- tab[4]
the_names_1
the_names_2

the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE) # simplify = true makes it matrix instead of list
the_names_1 

the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% as_tibble()

identical(research_funding_rates, new_research_funding_rates)

# RECODING LARGE NAMES IN CATEGORICAL VARS INSTEAD OF USING CASE_WHEN
library(dslabs)
data("gapminder")
gapminder %>% 
  filter(region == "Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          `Antigua and Barbuda` = "Barbuda",
                          `Dominican Republic` = "DR",
                          `St. Vincent and the Grenadines` = "St. Vincent",
                          `Trinidad and Tobago` = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()


# diff between cat & print. cat shows how it is stored in R
s <- "5'"
cat(s)
print(s)

# Labs
s <- '5\'10"'    # correct
cat(s)
data('dat')
head(dat)
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,|\\.|\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

# Labs - Brexit data
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)
head(polls)


polls <- polls %>% set_names(c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes"))
polls = polls[-1,]
head(polls)
polls %>% filter(str_detect(remain,'%')) %>% summarize(n())

# TEXT MINING CASE - TRUMP TWEETS
library(tidyverse)
library(lubridate)
library(scales)

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, 
                                      orders = "a b! d! H!:M!:S! z!* Y!",
                                      tz="EST")) 
library(dslabs)
data("trump_tweets")
head(trump_tweets)
trump_tweets$text[16413] %>% str_wrap(width = options()$width) %>% cat
trump_tweets %>% count(source) %>% arrange(desc(n)) %>% head(5)

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)", y = "% of tweets", color = "")

# Analysis of common words
install.packages('tidytext')
library(tidytext)

poem <- c("Roses are red,", "Violets are blue,", 
          "Sugar is sweet,", "And so are you.")
example <- tibble(line = c(1, 2, 3, 4),
                  text = poem)
example
example %>% unnest_tokens(word, text)

i <- 3008
campaign_tweets$text[i] %>% str_wrap(width = 65) %>% cat()
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  pull(word) 


campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "tweets") %>%
  pull(word)

links <- "https://t.co/[A-Za-z\\d]+|&amp;"
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, links, ""))  %>%
  unnest_tokens(word, text, token = "tweets") %>%
  pull(word)

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, links, ""))  %>%
  unnest_tokens(word, text, token = "tweets") 

tweet_words %>% 
  count(word) %>%
  arrange(desc(n))

stop_words

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, links, ""))  %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word ) 

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  arrange(desc(n))

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, links, ""))  %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))

android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)
android_iphone_or %>% filter(Android+iPhone > 100) %>%arrange(desc(or))
android_iphone_or %>% filter(Android+iPhone > 100) %>%arrange(or)

# Sentiment analysis
library(tidytext)
install.packages('textdata')
library(textdata)

get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("loughran")
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") 
get_sentiments("nrc") %>% count(sentiment)
nrc <- get_sentiments("nrc") %>%select(word, sentiment)
tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% 
  sample_n(5)

sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

library(broom)
log_or <- sentiment_counts %>%
  mutate(log_or = log((Android / (sum(Android) - Android)) / 
                        (iPhone / (sum(iPhone) - iPhone))),
         se = sqrt(1/Android + 1/(sum(Android) - Android) + 
                     1/iPhone + 1/(sum(iPhone) - iPhone)),
         conf.low = log_or - qnorm(0.975)*se,
         conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

log_or %>%
  mutate(sentiment = reorder(sentiment, log_or)) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 


android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))

android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 


# Labs
data(brexit_polls)
brexit_polls %>% filter(month(startdate)==4) %>% summarize(n())
brexit_polls %>% filter(round_date(enddate,unit='week')==round_date(ymd('2016-06-12'),unit='week')) %>% summarize(n())
brexit_polls %>% mutate(wkday = weekdays(enddate)) %>% count(wkday) %>% arrange(desc(n))

# Labs
data(movielens)
movielens %>% mutate(timestamp = as_datetime(timestamp), yr = year(timestamp), hr = hour(timestamp)) %>% count(yr) %>% arrange(desc(n))
movielens %>% mutate(timestamp = as_datetime(timestamp), yr = year(timestamp), hr = hour(timestamp)) %>% count(hr) %>% arrange(desc(n))

# Lab Project Gutenbergr
library(tidyverse)
install.packages('gutenbergr')
library(gutenbergr)
library(tidytext)
options(digits = 3)

gutenberg_metadata
names(gutenberg_metadata)
pp = gutenberg_metadata %>% filter(str_detect(title, 'Pride and Prejudice'))
gutenberg_works(languages = "en") %>% filter(str_detect(title, 'Pride and Prejudice'))
book = gutenberg_download(1342)
words = book %>% unnest_tokens(word, text) %>% filter(! word %in% stop_words$word) %>% filter(! str_detect(word, '\\d')) %>% count(word) %>% filter(n>100) %>% arrange(desc(n))
words

afinn <- get_sentiments("afinn")
afinn
words = book %>% unnest_tokens(word, text) %>% filter(! word %in% stop_words$word) %>% filter(! str_detect(word, '\\d'))
words %>% inner_join(afinn, by = 'word') %>% filter(value==4) %>% count()



