# Initial required package 

if (!require("dplyr")) install.packages("dplyr"); library(dplyr)

# Setting working directory
setwd("C:\\Saul\\UCLA\\Week 10\\Capstone Project\\Covid")


#List all required packages - dplyr
packages = c("DT", "RJDBC", "ROCR", "rJava","summarytools","ExPanDaR","DataExplorer","DescTools","RSQLite","DBI","rpivotTable","dataPreparation")

# Install packages not yet installed
installed_packages = packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
lapply(packages, library, character.only = TRUE) %>%  invisible()


# Load the data

f_file = function (l_date) {
  l_today =  format(l_date, "%m-%d-%Y")
  l_prefix = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'
  l_file = paste0(l_prefix,l_today,'.csv')
  return(l_file)
}
f_file(Today()-1) 

if (file.exists( f_file(Today()))) {
  l_last_day_file = f_file(Today()) 
  l_a_day_before_file = f_file(Today()-1) 
} else {
  l_last_day_file = f_file(Today()-1) 
  l_a_day_before_file = f_file(Today()-2) 
}



covid_csv = readr::read_csv(l_last_day_file  , locale = readr::locale(encoding = "latin1") )
covid_adb_csv = readr::read_csv(l_a_day_before_file  , locale = readr::locale(encoding = "latin1") )

l_countries = list('US','Australia','Austria','Belgium','Canada','France','Germany','Italy','Japan', 
                   'SouthKorea','Netherlands','Spain','Sweden','Switzerland','United Kingdom','China')

covid_csv = covid_csv %>% dplyr::filter(Country_Region %in% l_countries  ) %>%
  dplyr::select(c('Country_Region','Lat','Long_','Confirmed','Deaths','Recovered','Active','Last_Update'))


covid_csv = covid_csv %>% dplyr::group_by(Country_Region,Last_Update) %>% 
  dplyr::summarise(Confirmed=sum(Confirmed), Deaths=sum(Deaths) ,Recovered=sum(Recovered), 
                   Active=sum(Active), Lat = mean(Lat,  na.rm = TRUE),  Long_ = mean(Long_,  na.rm = TRUE)) %>%
  dplyr::select(-c('Last_Update'))

covid_adb_csv = covid_adb_csv %>% dplyr::filter(Country_Region %in% l_countries  ) %>%
  dplyr::select(c('Country_Region','Lat','Long_','Confirmed','Deaths','Recovered','Active','Last_Update'))


covid_adb_csv = covid_adb_csv %>% dplyr::group_by(Country_Region,Last_Update) %>% 
  dplyr::summarise(Confirmed=sum(Confirmed), Deaths=sum(Deaths) ,Recovered=sum(Recovered), 
                   Active=sum(Active), Lat = mean(Lat,  na.rm = TRUE),  Long_ = mean(Long_,  na.rm = TRUE)) %>%
  dplyr::select(-c('Last_Update'))

covid_adb_csv$Lat = NULL
covid_adb_csv$Long_  = NULL
covid_adb_csv =covid_adb_csv %>% dplyr::rename(Confirmed_adb ='Confirmed' , Deaths_adb = 'Deaths',
                                               Recovered_adb ='Recovered' , Active_adb = 'Active' )



covid_join = covid_csv %>% 
  dplyr::inner_join(covid_adb_csv , by = c('Country_Region'='Country_Region') )

datatable(covid_join)


# get world map
wmap <- getMap(resolution="high")
# get centroids
centroids <- gCentroid(wmap, byid=TRUE)
# get a data.frame with centroids
df_long_lat <- as.data.frame(centroids)
names(df_long_lat) = c("long","lat")
df_long_lat <- tibble::rownames_to_column(df_long_lat, "COUNTRY_NAME")

covid_join$Country_Region[covid_join$Country_Region == "US"] = "United States of America"

covid_join <- merge(covid_join, df_long_lat, by.x = "Country_Region", by.y = "COUNTRY_NAME")

covid_join$Lat = NULL
covid_join$Long_  = NULL

covid_join =covid_join %>% dplyr::rename(Lat ='lat' , Long_ = 'long' )

covid_join$Mortality = covid_join$Deaths/covid_join$Confirmed
covid_join$New_Cases = ifelse(covid_join$Confirmed - covid_join$Confirmed_adb>0,covid_join$Confirmed - covid_join$Confirmed_adb,0)
covid_join$Change = (covid_join$Confirmed - covid_join$Confirmed_adb)/covid_join$Confirmed_adb


datatable(covid_join)




covid_deaths_csv = readr::read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv', locale = readr::locale(encoding = "latin1") )


l_countries = list('US','Australia','Austria','Belgium','Canada','France','Germany','Italy','Japan', 
                   'SouthKorea','Netherlands','Spain','Sweden','Switzerland','United Kingdom','China')

covid_deaths_csv = covid_deaths_csv %>% dplyr::filter(`Country/Region` %in% l_countries  ) %>%
  dplyr::select(-c('Province/State','Lat','Long'))


covid_deaths_csv =covid_deaths_csv %>% dplyr::rename(Country = `Country/Region`)

covid_deaths_csv = covid_deaths_csv %>% dplyr::group_by(Country) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::ungroup()

covid_deaths_csv = melt(covid_deaths_csv, id.vars="Country")

covid_deaths_csv =covid_deaths_csv %>% dplyr::rename(Date = 'variable', Deaths = 'value')


datatable(covid_deaths_csv)


plot_ly(covid_deaths_csv, x = ~Date, y = ~Deaths) %>%
  add_lines(linetype = ~Country)










rpivotTable(data = covid_csv)



prepare_descriptive_table(covid_csv)

conn = dbConnect(RSQLite::SQLite(), "CovidDB.db")

dbWriteTable(conn, "covid_table", covid_csv)

dbListTables(conn)

df_covid = dbGetQuery(conn, "SELECT * FROM covid_table")

rpivotTable(data = df_covid)

dbDisconnect(conn)

#dataPreparation
#https://cran.r-project.org/web/packages/dataPreparation/dataPreparation.pdf















library(plotly)
library(gapminder)
?gapminder

df = gapminder
str(df)
str(covid_deaths_csv)



covid_deaths_csv = readr::read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv', locale = readr::locale(encoding = "latin1") )
covid_confirmed_csv = readr::read_csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv', locale = readr::locale(encoding = "latin1") )

l_countries = list('US','France','Germany','Italy', 
                   'Spain','United Kingdom','China')


covid_deaths_csv = covid_deaths_csv %>% dplyr::filter(`Country/Region` %in% l_countries  ) %>%
  dplyr::select(-c('Province/State','Lat','Long'))
covid_deaths_csv =covid_deaths_csv %>% dplyr::rename(Country = `Country/Region`)
covid_deaths_csv = covid_deaths_csv %>% dplyr::group_by(Country) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::ungroup()
covid_deaths_csv = melt(covid_deaths_csv, id.vars="Country")
covid_deaths_csv =covid_deaths_csv %>% dplyr::rename(Date = 'variable', Deaths = 'value')


covid_confirmed_csv = covid_confirmed_csv %>% dplyr::filter(`Country/Region` %in% l_countries  ) %>%
  dplyr::select(-c('Province/State','Lat','Long'))
covid_confirmed_csv =covid_confirmed_csv %>% dplyr::rename(Country = `Country/Region`)
covid_confirmed_csv = covid_confirmed_csv %>% dplyr::group_by(Country) %>%
  dplyr::summarise_all(sum) %>%
  dplyr::ungroup()
covid_confirmed_csv = melt(covid_confirmed_csv, id.vars="Country")
covid_confirmed_csv =covid_confirmed_csv %>% dplyr::rename(Date = 'variable', Confirmed = 'value')


df_joined =  covid_deaths_csv %>%
  dplyr::inner_join(covid_confirmed_csv , by = c('Country'='Country','Date'='Date'))


df_joined$Date = as.Date(df_joined$Date, format = "%m/%d/%y")

l_min_date = min(df_joined$Date)

df_joined$Days = df_joined$Date - l_min_date + 1




base <- df_joined %>%
  plot_ly(x = ~Confirmed, y = ~Deaths, size = ~Deaths, 
          text = ~Country, hoverinfo = "text") 


base %>%
  add_markers(
    color = ~Country, showlegend = F,
    alpha = 0.1, alpha_stroke = 0.7
  ) %>%
  add_markers(color = ~Country, frame = ~Days, ids = ~Country) %>%
  animation_opts(100, redraw = FALSE)








gg <- ggplot(df_joined, aes(Confirmed, Deaths, color = Country)) +
  geom_point(aes(size = Deaths*50000, frame = Days))
ggplotly(gg)


datatable(covid_deaths_csv)

base <- df_joined %>%
  plot_ly(x = ~Confirmed, y = ~Deaths, size = ~Deaths, 
          text = ~Country, hoverinfo = "text") %>%
  layout(xaxis = list(type = "log"))

base %>%
  add_markers(color = ~Country, frame = ~Days, ids = ~Country) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "Date ", font = list(color="red"))
  )

base











data <- df_joined  

# Most basic bubble plot
base = data %>%
  arrange(desc(Deaths)) %>%
  plot_ly(aes(x=Confirmed, y=Deaths, size=Deaths*500, fill=Country)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 24), name="Population (M)") +
  theme(legend.position="bottom") +
  ylab("Life Expectancy") +
  xlab("Gdp per Capita") +
  theme(legend.position = "none")



base %>%
  add_markers(color = ~Country, frame = ~Days, ids = ~Country) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "Date ", font = list(color="red"))
  )

data


########################## P R O P H E T ############################################
#install.packages("prophet")
library(prophet)
library(ggthemes)

df = df_joined %>% dplyr::filter(df_joined$Country == "US")

#visualize time series            
qplot(Date, Deaths, data = df, main = 'Covid Deaths')

# prep new df for prophet input/fitting. need 'ds' and 'y' columns
df = data.frame(ds = df$Date, y = df$Deaths)

df$floor <- 1000 
df$cap <- 80000 
# fit prophet model
#mod.1 = prophet::prophet(df, seasonality.mode = 'multiplicative') 
mod.1 = prophet::prophet(df, growth='logistic' ) 

future <- make_future_dataframe(mod.1, periods = 240, freq = 'day')
future$floor <- 1000 
future$cap <- 80000 
future

# predictions
forecast_df <- tbl_df(predict(mod.1, future))

#create a forecast data frame, including upper and lower limits
#forecast_df <- data.frame(forecast_df[c('ds','yhat', 'yhat_lower', 'yhat_upper','trend')])

tail(forecast_df)

forecast_df$ds <- as.Date(forecast_df$ds, "%Y-%m-%d")

ggplot(forecast_df,aes(x=ds)) + geom_point(data=forecast_df,aes(y=trend,color="Trend"))+ 
  geom_point(data=df,aes(y=y,color="Actual")) + theme_economist()


str(mod.1)
str(forecast_df)

# plot forecast
plot(mod.1, forecast_df, main = 'Covid Forecast', 
     xlab = 'Daily Time Series',
     ylab = 'HPI - Nonseasonally Adj.')    

# plot trend components
prophet_plot_components(mod.1, predict(mod.1, future))

#install.packages("forecast")
library(forecast)

N = nrow(df)
n = 0.8*N
train = df[1:n, ]
test  = df[(n+1):N,  ]

dataprediction <- data.frame(forecast_df$ds,forecast_df$yhat)
dataprediction <- dataprediction[c((n+1):N),]

print(dataprediction$forecast_df.yhat)
print(test$y)

#Creating cross validation 
accuracy(dataprediction$forecast_df.yhat ,test$y)



##########################  XGBOOST   ####################
# xgboost
#install.packages("xgboost")
library(xgboost)
#install.packages("caret")
library(caret)
#install.packages("doParallel")
library(doParallel)
#install.packages("foreach")
library(foreach)
library(lubridate)

df_xgb = df_joined %>% dplyr::filter(df_joined$Country == "US")

#lmax = max(df_xgb$Date)
df_xgb$Day = as.integer(mday(df_xgb$Date))
df_xgb$Weekday =  as.integer(wday(df_xgb$Date))
df_xgb$Month =  as.integer(month(df_xgb$Date))

df_xgb = df_xgb %>% dplyr::select(c('Day','Weekday','Month','Deaths'))

str(df_xgb)




fit_xgboost <- function(X_Train, Y_Train){
  X_Train <- X_Train %>% as.matrix()
  Y_Train <- Y_Train %>% as.matrix()
  # parameter settings
  set.seed(17)
  param <- list(objective = "reg:linear",nround=1000,
                eval_metric = "rmse",
                eta = 0.05,
                max_depth = 10,
                min_child_weight = 3,
                colsample_bytree = 1,
                gamma = 0.9,
                alpha = 1.0,
                subsample = 0.7
  )
  
  # parallel calculation
  N_cpu = detectCores()
  
  # find nrounds with cross-validation
  xgbcv <- xgb.cv(param = param, data = X_Train, label = Y_Train,
                  nrounds = 100,
                  nfold = 3,
                  nthread = N_cpu
  )
  
  # modling
  set.seed(17)
  model_xgb <- xgboost(param = param, data = X_Train, label = Y_Train,
                       nrounds = which.min(xgbcv$evaluation_log$test_rmse_mean),
                       nthread = N_cpu, importance = TRUE)
  
  return(model_xgb)
}

N = nrow(df_xgb)
n = 0.8*N
train = df_xgb[1:n, ]
test  = df_xgb[(n+1):N,  ]



x_train_train <- train %>%
  dplyr::select(-c('Deaths'))
y_train_train <- train %>%
  dplyr::select(Deaths)

str(x_train_train)

x_valid_train  <- test %>%
  dplyr::select(-c('Deaths'))
y_valid_train  <- test %>%
  dplyr::select(Deaths)


# calculate modeling
xgb1 <- fit_xgboost(x_train_train, y_train_train)

xgb1
# output importance
imp1 <- xgb.importance(names(x_train_train), model = xgb1)
imp1 %>%
  ggplot()+
  geom_bar(mapping = aes(x = reorder(Feature, Gain), y =Gain), stat = "identity")+
  coord_flip()


# train RMSE
train_train.pred <- train %>%
  dplyr::bind_cols(pred = predict(xgb1, newdata = x_train_train %>% as.matrix(), type = "response")) %>%
  dplyr::mutate(error = Deaths - pred)

train_train.pred %>%
  dplyr::summarise(
    RMSE = sqrt(sum(abs(error^2))/n())
  )

# valid RMSE
valid_train.pred <- test %>%
  dplyr::bind_cols(pred = predict(xgb1, newdata = x_valid_train %>% as.matrix(), type = "response")) %>%
  dplyr::mutate(error = Deaths - pred)

valid_train.pred %>%
  dplyr::summarise(
    RMSE = sqrt(sum(abs(error^2))/n())
  )

