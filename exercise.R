#10.5
#1
#We can look at the class type 
class(mtcars)
#It shows that mtcars is a data.frame, not a tibble.

#2
library(tidyverse)
df <- data.frame(abc = 1, xyz = "a")
dft <- tibble(abc = 1, xyz = "a")

df$x
dft$xyz
#data frame does partial matching，but we have to type the complete variable name in tibble

df[,'xyz'] #returns a vector
dft[,'xyz'] #returns a data frame

df[, c("abc", "xyz")]
dft[, c("abc", "xyz")]
#returns the same thing but xyz is a factor in the data frame and a char in the tibble

#3
tibble_mtcars <- as.tibble(mtcars)
var <- 'mpg'
tibble_mtcars[var]

#4
annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)

#(1)
annoying$`1`

#(2)
annoying %>% ggplot() + geom_point(mapping = aes(x = `1`, y = `2`))

#(3)
annoying$`3` <- annoying$`2` / annoying$`1`
annoying

#(4)
annoying %>% rename(one = `1`, two = `2`, three = `3`)

#5
#it converts named atomic vectors or lists to two-column data frames. 
#For unnamed vectors, the natural sequence is used as name column.
#For example:
x <- c(Alice=165, Andy=180, Sara=160)
enframe(x,name='Name', value='Height')

#6
#To limit the number of additional column information at the footer, we can use the argument n_extra.


#12.6.1
#1
#We look at the number of years of recorded data for each country
who %>%
  group_by(country) %>%
  summarize(count = n()) %>%
  ggplot() +
  geom_point(mapping = aes(x = country, y = count), color = 'green') +
  coord_flip()
#While most countries have 34 years of recorded data, some countries have less. 
#That means that there are implicit missing values.
#To check if there are any 0 recorded cases:
sum(who %>% select(-c(1:4)) == 0, na.rm = TRUE)
#There are cases that have a recorded value of 0, which mean they are explicitly stated as no-case.
#check the number of NAs in each column:
who %>% select(-c(1:4)) %>%
  sapply(function(x){sum(is.na(x))})
#These NAs are explicitly stated as missing values.
#Setting na.rm = TRUE is reasonable because of the importance of NAs

#2
#We will fail to separate the code into the three columns new, var, and sexage.

#3
#check the number of unique values in country, iso2, and iso3
who %>% select(1:3) %>% sapply(function(x){length(unique(x))})
#check the number of unique combinations of these columns
who %>% select(1:3) %>%
  unite(combined, 1:3) %>%
  select(combined) %>%
  distinct() %>%
  nrow()
#for each country, there is only one iso2 code and one iso3 code. Therefore iso2 and iso3 are redundant.

#4
who %>%
  gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>% 
  mutate(code = stringr::str_replace(code, "newrel", "new_rel")) %>%
  separate(code, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1) %>%
  group_by(country, year, sex) %>%
  summarize(total_case = sum(value)) %>%
  unite(country_sex, country, sex, remove = FALSE) %>%
  ggplot() +
  geom_line(mapping = aes(x = year, y = total_case, color = sex,
                          group = country_sex))


#Tidy Data article
#(1)
library(tidyverse)
library(foreign)
library(stringr)
library(plyr)
library(reshape2)
source("xtable.r")

pew <- read.spss("pew.sav",header=TRUE)
pew <- as.data.frame(pew)

religion <- pew[c("q16", "reltrad", "income")]
religion$reltrad <- as.character(religion$reltrad)
religion$reltrad <- str_replace(religion$reltrad, " Churches", "")
religion$reltrad <- str_replace(religion$reltrad, " Protestant", " Prot")
religion$reltrad[religion$q16 == " Atheist (do not believe in God) "] <- "Atheist"
religion$reltrad[religion$q16 == " Agnostic (not sure if there is a God) "] <- "Agnostic"
religion$reltrad <- str_trim(religion$reltrad)
religion$reltrad <- str_replace_all(religion$reltrad, " \\(.*?\\)", "")

religion$income <- c("Less than $10,000" = "<$10k", 
                     "10 to under $20,000" = "$10-20k", 
                     "20 to under $30,000" = "$20-30k", 
                     "30 to under $40,000" = "$30-40k", 
                     "40 to under $50,000" = "$40-50k", 
                     "50 to under $75,000" = "$50-75k",
                     "75 to under $100,000" = "$75-100k", 
                     "100 to under $150,000" = "$100-150k", 
                     "$150,000 or more" = ">150k", 
                     "Don't know/Refused (VOL)" = "Don't know/refused")[religion$income]

religion$income <- factor(religion$income, levels = c("<$10k", "$10-20k", "$20-30k", "$30-40k", "$40-50k", "$50-75k", 
                                                      "$75-100k", "$100-150k", ">150k", "Don't know/refused"))

counts <- count(religion, c("reltrad", "income"))
names(counts)[1] <- "religion"

xtable(counts[1:10, ], file = "pew-clean.tex")

# Convert into the form in which I originally saw it -------------------------

raw <- dcast(counts, religion ~ income)
xtable(raw[1:10, 1:7], file = "pew-raw.tex")

pew2 <- raw %>% gather(key="income", value="freq", -religion)

#(2)
library(tidyverse)

bb <- read_csv("billboard.csv")
bb.1 <- bb %>% gather(key="week", value="rank", -year,-artist.inverted, -track, -time, -genre, -date.entered, -date.peaked)
bb.2 <- bb.1 %>% select(year, artist=artist.inverted, time, track, date=date.entered, week, rank)
bb.3 <- bb.2 %>% arrange(track)
bb.4 <- bb.3 %>% filter(!is.na(rank))
bb.5 <- bb.4 %>% separate(week, into=c("A", "B", "C"), sep=c(1, -7), convert=TRUE)
bb.6 <- bb.5 %>% select(-A, -C)
bb.7 <- bb.6 %>% dplyr::rename(week=B)
bb.8 <- bb.7 %>% arrange(artist, track)
bb.9 <- bb.8 %>% mutate(date=date + (week-1)*7)
bb.10 <- bb.9 %>% mutate(rank=as.integer(rank)) 

