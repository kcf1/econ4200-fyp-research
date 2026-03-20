# Load the package.
library(datasets)
library(dslabs)
library(tidyr)
library(dplyr)
library(rvest)
library(stringr)
library(lubridate)
library(janeaustenr)
library(tidytext)
library(textdata)
library(ggplot2)

df <- read.csv('data/job.csv',na.strings = c('N.A.','[-§32]','[*1]'))
head(df)

colnames(df) <- paste0(df[2,],' - ',df[3,]) |> str_replace('^\\s{2}-\\s$','') |> paste0(df[4,])
df |> str()

df <- df[5:2270,-c(5,7,9)]
df$Year <- df$Year |> as.numeric()
df[2:3] <- df[2:3] |> lapply(as.factor)
df[4:7] <- df[4:7] |> lapply(as.numeric)
head(df)

df <- df |>
  mutate(
    Date = mdy(paste0(Month,'1, ',Year)),
    `Vacancy rate (1) - (%)` = round(`Number of vacancies - No.`/(`Number of vacancies - No.`+`Number of persons engaged - No.`)*100,2)
  )
df <- df |>
  arrange(Date)

df |> str()


all_ind <- df |>
  filter(`Industry section` == 'All industry sections covered')
agg_ind <- df |>
  filter(
    `Industry section` %in% 
      c('B, D & E: Mining and quarrying; and electricity and gas supply, and waste management',
        'G: Import/export, wholesale and retail trades',
        'M & N: Professional and business services',
        'P - S: Social and personal services'
        )
  )
gran_ind <- df |>
  filter(
    `Industry section` != 'All industry sections covered' &
    ! `Industry section` %in% 
      c('B, D & E: Mining and quarrying; and electricity and gas supply, and waste management',
        'G: Import/export, wholesale and retail trades',
        'M & N: Professional and business services',
        'P - S: Social and personal services'
        )
  )

gran_ind |>
  filter(`Industry section` %in% c('B: Mining and quarrying','D & E: Electricity and gas supply, and waste management')) |>
  group_by(Date) |>
  mutate(
    ratio = `B: Mining and quarrying` / `D & E: Electricity and gas supply, and waste management`
  )

all_eng <- gran_ind |>
  group_by(Date) |>
  summarise(
    tot_eng = sum(`Number of persons engaged - No.`),
    tot_vac = sum(`Number of vacancies - No.`)
  ) |>
  mutate(
    tot_per = tot_eng + tot_vac
  )
all

shares <- gran_ind |> 
  group_by(Date) |>
  mutate(
    ind_share = `Number of establishments - No.` / sum(`Number of establishments - No.`)
  )

unique(shares$`Industry section`)

shares |> 
  ggplot(aes(x=Date, y=`ind_share`, fill=`Industry section`)) +
  geom_area(alpha=0.8, size=0.5, color='white')

shares_chg <- shares |> 
  group_by(`Industry section`) |>
  summarize(
    change = last(ind_share) - first(ind_share)
  ) |>
  arrange(-change)
shares_chg |>
  ggplot(aes(x=reorder(`Industry section`,-change),y=change,fill = `Industry section`)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

gran_ind |> 
  ggplot(aes(x=Date,y=`Vacancy rate (1) - (%)`,fill=`Industry section`)) +
  #geom_bar(stat = 'identity', position = 'fill') +
  geom_area(alpha=0.8, size=0.5, color='white')

rel <- gran_ind |>
  group_by(Date) |>
  mutate(
    rel_vac = (`Vacancy rate (1) - (%)`-mean(`Vacancy rate (1) - (%)`,na.rm))
  ) #|> 
  ggplot(aes(x=Date,y=rel_vac,fill=`Industry section`)) +
  geom_bar(stat = 'identity', position = 'stack')
  #geom_area(alpha=0.8, size=0.5, color='white')
?mean

