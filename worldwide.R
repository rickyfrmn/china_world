library(readr)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(lubridate)

# covid <- read.csv("~/FGA/csv/confirmed_cases_china_vs_world.csv")
covid <- read.csv("~/FGA/csv/confirmed_cases_worldwide.csv", header = TRUE)
# cov_id_raw <- content(covid, as = "parsed", simplifyVector = TRUE) 
length(covid)

names(covid)

covid_pekanan <- covid %>% 
  count(
    tahun = year(date),
    pekan_ke = day(date),
    wt = cum_cases,
    name = "jumlah"
  )

glimpse(covid_pekanan)


covid_pekanan <-
  covid_pekanan %>% 
  mutate(
    jumlah_pekanlalu = dplyr::lag(jumlah, 1),
    jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
    lebih_baik = jumlah < jumlah_pekanlalu
  )
glimpse(covid_pekanan)

ggplot(covid_pekanan, aes(pekan_ke, jumlah, fill=lebih_baik))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(breaks=1:31, expand=c(0,0))+
  scale_fill_manual(values=c("TRUE"="seagreen3", "FALSE"="salmon"))+
  labs(
    x=NULL,
    y="Jumlah kasus",
    title="Kasus Pekanan Positif COVID-19",
    subtitle="Kolom hijau menunjukan penambahan kasus baru lebih sedikit dibandingkan satu pekan sebelumnya",
    caption="Sumber data: "
  )+
  theme_ipsum(
    base_size=9,
    plot_title_size=15,
    grid="Y",
    ticks=TRUE
  )+
  theme(plot.title.position="plot")
