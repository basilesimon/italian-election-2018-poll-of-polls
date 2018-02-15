# install.packages('rvest')
# install.packages('tidyverse')
# install.packages('tidyquant')
# install.packages('ggthemes')
library('rvest')
library('dplyr')
library('ggplot2')
library('tidyquant')
library('ggthemes')
library('reshape2')

url <- 'https://en.wikipedia.org/wiki/Opinion_polling_for_the_Italian_general_election,_2018'

table_2018 <- url %>%
          read_html() %>%
          html_node(xpath='/html/body/div[3]/div[3]/div[4]/div/table[7]') %>%
          html_table(fill = TRUE) %>%
          setNames(c('date', 'firm','centre-left','centre-right','m5s','leu','others','lead')) %>%
          tail(-1)

for(i in c(3:ncol(table_2018))) {
  table_2018[,i] <- as.numeric(as.character(table_2018[,i]))
}

data_2018 <- table_2018 %>%
  group_by(date) %>%
  mutate(cut_date = paste(tail(strsplit(date, "–")[[1]], n=1), " 2018")) %>%
  mutate(clean_date = as.Date(cut_date, format="%d %b %Y")) %>%
  ungroup() %>%
  select(-date, -firm, -lead, -cut_date) %>%
  melt(id="clean_date")

ggplot(data = data_2018, aes(clean_date, value, color=variable)) + 
    geom_point() + 
    geom_ma(ma_fun = SMA, n = 3)

one_year <- url %>%
          read_html() %>%
          html_node(xpath='/html/body/div[3]/div[3]/div[4]/div/table[8]') %>%
          html_table(fill = TRUE) %>%
          setNames(c('date', 'firm','centre-left','centre-right','m5s','leu','others','lead')) %>%
          tail(-1)

for(i in c(3:ncol(one_year))) {
  one_year[,i] <- as.numeric(as.character(one_year[,i]))
}

data_2017 <- one_year %>% group_by(date) %>%
  mutate(cut_date = paste(tail(strsplit(date, "–")[[1]], n=1), " 2017")) %>%
  mutate(clean_date = as.Date(cut_date, format="%d %b  %Y")) %>%
  ungroup() %>%
  select(-date, -firm, -lead, -cut_date) %>%
  melt(id="clean_date")

ggplot(data = data_2017, aes(clean_date, value, color=variable)) + 
  geom_point(aes(shape="21",alpha=1/100)) + 
  geom_ma(ma_fun = SMA, n = 10)

two_years<- url %>%
          read_html() %>%
          html_node(xpath='/html/body/div[3]/div[3]/div[4]/div/table[6]') %>%
          html_table(fill = TRUE) %>%
          setNames(c('date', 'firm', 'lead')) %>%
          tail(-2)

for(i in c(3:13:ncol(two_years))) {
  two_years[,i] <- as.numeric(as.character(two_years[,i]))
}


#########################
# merge data together now

data <- merge(data_2018, data_2017, all=TRUE)


# rename
data <- data %>% rename(date = clean_date, coalition = variable) %>%
  group_by(coalition) %>%
  mutate(mean20_missing = rollapply(value, width = 20, fill = NA, partial = TRUE, 
                                    FUN=function(x) mean(x, na.rm=TRUE), align = "right"))

ggplot(data, 
       aes(date, color=coalition)) + 
  geom_point(aes(y=value, shape="21", alpha=1/100)) + 
  geom_line(aes(y=mean20_missing, color=coalition))

write_csv(data, "data/coalition.csv")

