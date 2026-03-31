# Load the package.
library(datasets)
library(dslabs)
library(tidyr)
library(stringr)
library(dplyr)
library(rvest)
library(stringr)
library(lubridate)
library(janeaustenr)
library(tidytext)
library(textdata)
library(ggplot2)

df <- read.csv('data/job.csv',sep = ',',na.strings = c('N.A.','[-§32]','[*1]'))
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
      c('B: Mining and quarrying',
        'D & E: Electricity and gas supply, and waste management',
        #'B, D & E: Mining and quarrying; and electricity and gas supply, and waste management',
        'G: Import/export, wholesale and retail trades',
        'M & N: Professional and business services',
        'P - S: Social and personal services'
        )
  )

all_eng <- gran_ind |>
  group_by(Date) |>
  summarise(
    tot_eng = sum(`Number of persons engaged - No.`),
    tot_vac = sum(`Number of vacancies - No.`)
  ) #|>
  mutate(
    tot_per = tot_eng + tot_vac
  )
all_eng_norm <- all_eng |>
  mutate(
    tot_eng = (tot_eng - first(tot_eng)) / sd(tot_eng),
    tot_vac = (tot_vac - first(tot_vac)) / sd(tot_vac),
    #tot_per = (tot_per - first(tot_per)) / sd(tot_per),
    period = Date |> floor_date(unit = '10years') |> as.factor()
  )

all_eng_chg <- all_eng |>
  mutate(
    tot_eng = (tot_eng/lag(tot_eng))-1,
    tot_vac = (tot_vac/lag(tot_vac))-1,
    #tot_per = (tot_per/lag(tot_per))-1,
    period = Date |> floor_date(unit = '10years') |> as.factor()
  ) |>
  filter(!is.na(tot_eng)) |>
  mutate(
    tot_eng = tot_eng/sd(tot_eng),
    tot_vac = tot_vac/sd(tot_vac),
    #tot_per = tot_per/sd(tot_per)
  )
all_eng_chg |>
  pivot_longer(-c(Date,period), names_to = 'index', values_to = 'pct') |>
  ggplot(aes(x=Date,y=pct,fill=index)) +
  geom_bar(stat='identity',position='dodge')

all_eng_norm |>
  pivot_longer(-c(Date,period), names_to = 'index', values_to = 'level') |>
  ggplot(aes(x=Date,y=level,colour=index)) +
  geom_line()

all_eng_norm |>
  ggplot(aes(x=tot_eng,y=tot_vac,colour=period)) +
  geom_point() +
  geom_smooth(method='lm')


shares <- gran_ind |> 
  group_by(Date) |>
  mutate(
    est_share = `Number of establishments - No.` / sum(`Number of establishments - No.`),
    eng_share = `Number of persons engaged - No.` / sum(`Number of persons engaged - No.`),
    vac_share = `Number of vacancies - No.` / sum(`Number of vacancies - No.`),
  )

shares |> 
  ggplot(aes(x=Date, y=`vac_share`, fill=`Industry section`)) +
  geom_area(alpha=0.8, size=0.5, color='white') +
  ggtitle('Vacancy Share')

shares_chg <- shares |> 
  group_by(`Industry section`) |>
  summarize(
    est_chg = last(est_share) - first(est_share),
    eng_chg = last(eng_share) - first(eng_share),
    vac_chg = last(vac_share) - first(vac_share)
  )
shares_chg |>
  ggplot(aes(x=reorder(`Industry section`,-vac_chg),y=vac_chg,fill = `Industry section`)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1))

gran_ind |> 
  ggplot(aes(x=Date,y=`Vacancy rate (1) - (%)`,colour=`Industry section`)) +
  geom_line()
  #geom_bar(stat = 'identity', position = 'fill') +
  #geom_area(alpha=0.8, size=0.5, color='white')

rel <- gran_ind |>
  group_by(Date) |>
  mutate(
    rel_vac = (`Vacancy rate (1) - (%)`-mean(`Vacancy rate (1) - (%)`,na.rm))
  ) #|> 
  ggplot(aes(x=Date,y=rel_vac,fill=`Industry section`)) +
  geom_bar(stat = 'identity', position = 'stack')
  #geom_area(alpha=0.8, size=0.5, color='white')
?mean
  
rel

df |> slice(7) #|> pull(`X.8`)
