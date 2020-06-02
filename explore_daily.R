#Globals
require(data.table)
require(ggplot2)

setwd('C:\\Users\\hottm\\Desktop\\covid_19_analysis\\Data')

#Download latest daily data from GitHub
github_top_dir = 'raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'

cur_date = Sys.Date() - 1
cur_file = paste0(strftime(cur_date, format = '%m-%d-%Y'), '.csv')
while (!(cur_file %in% dir('.'))) {
  download.file(paste0(github_top_dir, cur_file), cur_file, method = 'libcurl')
  cur_date = cur_date - 1
  cur_file = paste0(strftime(cur_date, format = '%m-%d-%Y'), '.csv')
}

#Read data
daily_files = grep('^([0-9]{2}-){2}[0-9]{4}\\.csv', dir('.'), value = TRUE)
data = rbindlist(lapply(daily_files, function(file) cbind(file, fread(file))), fill = TRUE)

#Clean
colnames(data) = make.names(colnames(data))

setnames(data, 'Admin2', 'County')
colnames(data)[1] = 'Day'
data[, Day := as.Date(gsub('\\.csv$', '', Day), format = '%m-%d-%Y')]

data[is.na(Country_Region), Country_Region := Country.Region]
data[grepl('China',   Country_Region), Country_Region := 'China']
data[grepl('Korea',   Country_Region), Country_Region := 'South Korea']
data[grepl('Gambia',  Country_Region), Country_Region := 'Gambia']
data[grepl('Bahamas', Country_Region), Country_Region := 'Bahamas']

data[is.na(Province_State), Province_State := Province.State]

data[, c('Province.State', 'Country.Region', 'Last.Update', 'Latitude',
         'Longitude', 'FIPS', 'Last_Update', 'Lat', 'Long_', 'Combined_Key')
     := NULL]

#Reshape
data = melt(data, measure.vars = c('Confirmed', 'Deaths', 'Recovered', 'Active'),
            variable.name = 'Metric', value.name = 'Value')
data[is.na(Value), Value := 0]

#Aggregate numbers by US state, and for the whole US
data = data[, .(Value = sum(Value)), by = .(Day, Province_State, Country_Region, Metric)]
us_data = data[Country_Region == 'US', .(Province_State = '', Country_Region = 'US', Value=sum(Value)), by = .(Day, Metric)]
data = rbind(data, us_data)

#Define and filter down to areas of interest
aoi = as.data.table(t(data.table(
  #c('Hubei', 'China'),
  c('', 'US'),
  #c('', 'Italy'),
  #c('', 'Iran'),
  #c('', 'South Korea'),
  #c('', 'Brazil'),
  #c('', 'Germany'),
  #c('', 'Spain'),
  #c('', 'Sweden')
  c('Washington', 'US'),
  c('New York', 'US'),
  c('New Jersey', 'US'),
  c('Oregon', 'US')
  #c('Florida', 'US'),
  #c('Louisiana', 'US'),
  #c('Michigan', 'US'),
  #c('Illinois', 'US'),
  #c('California', 'US')
  #c('Massachusetts', 'US')
)))
colnames(aoi) = colnames(data)[2:3]

aoi_data = merge(aoi, data, by = c('Province_State', 'Country_Region'))
aoi_data[, Area := paste0(Province_State, ifelse(Province_State == '', '', ', '), Country_Region)]

#Plot
cur_metric = 'Deaths'
cur_metric = 'Confirmed'
p = ggplot(aoi_data[Day >= as.Date('2020-4-10')][Metric == cur_metric], aes(x = Day, y = Value, color = Area))
#p = ggplot(aoi_data[Metric == cur_metric], aes(x = Day, y = Value, color = Area))
p = p + geom_point() + geom_line()
p = p + scale_x_date(date_breaks = '1 day', minor_breaks = NULL, date_labels = '%b %d')
#p = p + scale_y_log10(breaks = 10^(1:5), labels = c('10', '100', '1k', '10k', '100k'))
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab(cur_metric)
p

p = ggplot(aoi_data[Metric %in% c('Confirmed', 'Deaths')], aes(x = Day, y = Value, color = Metric))
p = p + facet_wrap(~ Area, scale='free') + geom_point() + geom_line()
p = p + scale_x_date(date_breaks = '1 week', minor_breaks = NULL, date_labels = '%b %d')
p = p + scale_y_log10(breaks = 10^(1:5), labels = c('10', '100', '1k', '10k', '100k'))
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab(cur_metric)
p


cur_data=aoi_data[Country_Region=='US' & Province_State=='' & Metric=='Deaths', Value]
day_over_day_incr=round((cur_data[2:length(cur_data)]/cur_data[1:(length(cur_data)-1)]-1)*100, digits = 1)
smoothed_dodi=round(sapply(1:(length(day_over_day_incr)-6), function(i) mean(day_over_day_incr[i:(i+6)])), digits=1)
plot(smoothed_dodi)
