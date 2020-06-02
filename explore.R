require(data.table)
require(ggplot2)

setwd('C:\\Users\\hottm\\Desktop\\covid_19_analysis\\Data')

#Download latest data from GitHub
github_top_dir   = 'raw.githubusercontent.com/CSSEGISandData/COVID-19/master/'

csse_dirpath     = 'csse_covid_19_data/csse_covid_19_time_series/'
conf_file        = 'time_series_covid19_confirmed_global.csv'
deaths_file      = 'time_series_covid19_deaths_global.csv'
rec_file         = 'time_series_covid19_recovered_global.csv'

who_dir          = 'who_covid_19_situation_reports/who_covid_19_sit_rep_time_series/'
who_file         = 'who_covid_19_sit_rep_time_series.csv'

cur_date = Sys.Date()
cur_date = paste0(substr(cur_date, 1, 2), substr(cur_date, 6, 7), substr(cur_date, 9, 10))
insert.date = function(x) gsub('.csv$', paste0('_', cur_date, '.csv'), x)

my.download.file = function(path, filename)
  download.file(paste0(github_top_dir, path, filename), insert.date(filename), method = 'libcurl')
my.download.file(csse_dirpath, conf_file)
my.download.file(csse_dirpath, deaths_file)
my.download.file(csse_dirpath, rec_file)
my.download.file(who_dir, who_file)

label = 'Confirmed Cases'
label = 'Deaths'
file = conf_file
file = deaths_file

#Read data
data = fread(insert.date(file))
colnames(data) = gsub('^X', '', make.names(colnames(data)))

#Reshape
data = melt(data, id.vars = colnames(data)[1:4], variable.name = 'Date', value.name = 'Value')
data[, Date := as.Date(data[, Date], format = '%m.%d.%y')]

#Make all-up numbers for US
#us_data = data[Country.Region == 'US' & !grepl(',', Province.State) & !is.na(Value),
#               .(Province.State = '', Country.Region = 'US', Lat = NA, Long = NA, Value=sum(Value)),
#               by = Date]
#data = rbind(data, us_data)

#Areas of interest
aoi = data.table()
aoi = rbind(aoi, data.table('Hubei', 'China'))
aoi = rbind(aoi, data.table('', 'Italy'))
aoi = rbind(aoi, data.table('Washington', 'US'))
aoi = rbind(aoi, data.table('New York', 'US'))
aoi = rbind(aoi, data.table('Oregon', 'US'))
aoi = rbind(aoi, data.table('', 'US'))
aoi = rbind(aoi, data.table('', 'Germany'))
aoi = rbind(aoi, data.table('', 'Spain'))
aoi = rbind(aoi, data.table('', 'Iran'))
colnames(aoi) = colnames(data)[1:2]

aoi_data = merge(aoi, data, by = c('Province.State', 'Country.Region'))
aoi_data[, Area := paste0(Province.State, ifelse(Province.State == '', '', ', '), Country.Region)]

#Plot
p = ggplot(aoi_data[Date >= as.Date('2020-03-01')], aes(x = Date, y = Value, color = Area)) + geom_point()
p = p + scale_y_log10(breaks = 10^(1:5), labels = c('10', '100', '1k', '10k', '100k'))
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p = p + ylab(label)
p
