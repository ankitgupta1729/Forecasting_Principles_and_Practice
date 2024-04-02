library(fpp3)

# creating a tsibble object
mydata <- tsibble(year=2015:2019,y=c(1,3,5,7,9),index = year)

# converting a dataset to tsibble object

#prison <- read.csv("C:/Users/ankit19.gupta/ankit/ankit/ML_Code/Forecasting_Principles_and_Practice/data/prison.csv")
#write.table(PBS , file = "C:/Users/ankit19.gupta/ankit/ankit/ML_Code/Forecasting_Principles_and_Practice/pbs.csv")

PBS |> filter(ATC2=="A10") |> select(Month,Concession,Type,Cost) |> summarise(TotalC=sum(Cost)) |> mutate(Cost=TotalC/1e6) ->a10
a10 |> autoplot(Cost) +
  labs(y="$ (millions)",title="Australian Antidiabetic drug sales")

print(a10)
print(PBS$Month)
a10 |> autoplot()

# printing points

a10 |> ggplot(aes(x=Month,y=Cost))+
  geom_point()

# printing line between points
a10 |> ggplot(aes(x=Month,y=Cost))+
  geom_line()


# both lines and points

a10 |> autoplot(Cost) + geom_point()

ansett |> autoplot(Passengers)

ansett |> distinct(Class)

ansett |> distinct(Airports)

ansett |> filter(Class=="Economy") |> autoplot()


## seasonal plot
a10 |> gg_season(Cost, labels = "both")+
  labs(y="$ million",title = "Seasonal Plot: Antidiabetic drug sales")

vic_elec |> gg_season(Demand,period = "day")

## subseries plot

a10 |> gg_subseries(Cost) +
  labs(y="$ million", title="Subseries plot: antidiabetic drug sales")

## scatter plots

# to identify relationship between multiple time series

vic_elec_day_type <- vic_elec |>
  filter(year(Time) == 2014) |>
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))
print(vic_elec_day_type)

# if holiday column has true value then day_type value="holiday" otherwise when date
# is 2 or 6 then day_type="weekday" and otherwise "weekend"

vic_elec_day_type |>
  ggplot(aes(x=Temperature,y=Demand))+
  geom_point()+
  labs(x="Temperature (Degrees Celcius)", y=" Electricity Demand (GW)")

vic_elec_day_type |>
  ggplot(aes(x=Temperature,y=Demand,colour=Day_Type))+
  geom_point()+
  labs(x="Temperature (Degrees Celcius)", y=" Electricity Demand (GW)")

## Scatter Plot for more than 2 variables.

library(GGally)
us_change |> GGally :: ggpairs(columns=2:6)

## Scatterplot matrices

visitors <- tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips))
visitors |> ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Australian domestic tourism",
       y= "Overnight trips ('000)")

visitors |>
  pivot_wider(values_from=Trips, names_from=State) |>
  GGally::ggpairs(columns = 2:9)

## Lag plots

new_production <- aus_production |>
  filter(year(Quarter) >= 1992)
new_production


new_production |> gg_lag(Beer,geom = "point")

## Autocorrelation

new_production |> gg_lag(Beer,geom="point")

new_production |> ACF(Beer,lag_max = 9)

new_production |> ACF(Beer,lag_max = 9) |>autoplot()

new_production |> ACF(Beer) |> autoplot()

print_retail <- aus_retail |>
  filter(Industry == 'Newspaper and book retailing') |>
  group_by(Industry) |>
  index_by(Year = year(Month)) |>
  summarise(Turnover=sum(Turnover))
aus_economy <- global_economy |>
  filter(Code == 'AUS')
print_retail |>
  left_join(aus_economy, by= 'Year') |>
  mutate(Adjusted_turnover = Turnover/CPI*100) |>
  pivot_longer(c(Turnover,Adjusted_turnover),values_to = 'Turnover') |>
  mutate(name=factor(name,levels=c('Turnover','Adjusted_Turnover'))) |>
  ggplot(aes(x=Year, y= Turnover))+
  geom_line()+
  facet_grid(name ~ ., scales= "free_y")+
  labs(title = "Turnover: Australian print media industry",y="$AU")

