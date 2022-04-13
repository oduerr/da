library(tidyr)
library(dplyr)

# See also https://towardsdatascience.com/create-a-coronavirus-app-using-r-shiny-and-plotly-6a6abf66091d
baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

minutesSinceLastUpdate = function(fileName) {
  (as.numeric(as.POSIXlt(Sys.time())) -  
     as.numeric(file.info(fileName)$ctime)) / 60
}

loadData = function(fileName, columnName) {
  if(!file.exists(fileName) || 
     minutesSinceLastUpdate(fileName) > 10) {
    data = read.csv(file.path(baseURL, fileName), 
                    check.names=FALSE, stringsAsFactors=FALSE) %>%
      select(-Lat, -Long) %>% 
      pivot_longer(-(1:2), names_to="date", values_to=columnName)%>%
      mutate(
        date=as.Date(date, format="%m/%d/%y"),
        `Province/State`=
          if_else(`Province/State` == "", "<all>", `Province/State`)
      )
    save(data, file=fileName)  
  } else {
    load(file=fileName)
  }
  return(data)
}
library(lubridate)
allData = 
  loadData("time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
  inner_join(loadData("time_series_covid19_deaths_global.csv", "CumDeaths")) #%>%
#inner_join(
#  loadData("time_series_19-covid-Recovered.csv","CumRecovered"))

last_date = max(allData$date)
print(paste0('Data from ', last_date))

