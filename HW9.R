#load(url("weather2011.rda"))

makePlotRegion = function(xlim, ylim, bgcolor, ylabels,
                          margins, cityName, xtop = TRUE) {
  # This function is to produce a blank plot that has 
  # the proper axes labels, background color, etc.
  # It is to be used for both the top and bottom plot.
  
  # The parameters are
  # xlim is a two element numeric vector used for the two
  #   end points of the x axis
  # ylim is the same as xlim, but for the y axis
  # ylabels is a numeric vector of labels for "tick marks"
  #   on the y axis
  # We don't need to x labels because they are Month names
  # margins specifies the size of the plot margins (see mar parameter in par)
  # cityName is a character string to use in the title
  # xtop indicates whether the month names are to appear
  # at the top of the plot or the bottom of the plot
  # 
  # See the assignment for a pdf image of the plot that is
  # produced as a result of calling this function.
  par()
  par(bg = bgcolor)
  par(mar = margins)
  plot(0, type = "n", xlim = xlim, ylim = ylim, xaxt='n', yaxt='n', ann=FALSE)
  axis(2, at = ylabels, lty = 1)
  
}


drawTempRegion = function(day, high, low, col){
  # This plot will produce 365 rectangles, one for each day
  # It will be used for the record temps, normal temps, and 
  # observed temps
  
  # day - a numeric vector of 365 dates
  # high - a numeric vector of 365 high temperatures
  # low - a numeric vector of 365 low temperatures
  # col - color to fill the rectangles
  
  for (i in day){
    rect(day-1, low, day, high, col = col, border = col)
  }
  
  
}

addGrid = function(location, col, ltype, vertical = TRUE) {
  # This function adds a set of parallel grid lines
  # It will be used to place vertical and horizontal lines
  # on both temp and precip plots
  
  # location is a numeric vector of locations for the lines
  # col - the color to make the lines
  # ltype - the type of line to make
  # vertical - indicates whether the lines are vertical or horizontal
  
  for (i in location){
    abline(v = i, col = col, lty = ltype)
  }
  
}

monthPrecip = function(day, dailyprecip, normal, lin_col, fill_col, normal_col){
  # This function adds one month's precipitation to the 
  #   precipitation plot.
  # It will be called 12 times, once for each month
  # It creates the cumulative precipitation curve,
  # fills the area below with color, add the total
  # precipitation for the month, and adds a reference
  # line and text for the normal value for the month
  
  # day a numeric vector of dates for the month
  # dailyprecip a numeric vector of precipitation recorded
  # for the month (any NAs can be set to 0)
  # normal a single value, which is the normal total precip
  #  for the month
  
  total_precip = 0
  precip = c()
  dailyprecip[is.na(dailyprecip)] <- 0
  for (i in day){
    total_precip = total_precip + dailyprecip[i]
    precip <- c(precip, total_precip)
  }
  lines(day-1, precip, col = lin_col)
  norm_vec = c(rep(normal, each = length(day)))
  lines(day, norm_vec, col = normal_col)
  #abline(h = normal, col = normal_col)
  
}

finalPlot = function(temp, precip){
  # The purpose of this function is to create the whole plot
  # Include here all of the set up that you need for
  # calling each of the above functions.
  # temp is the data frame sfoWeather or laxWeather
  # precip is the data frame sfoMonthlyPrecip or laxMonthlyPrecip
  
  
  # Here are some vectors that you might find handy
  
  monthNames = c("January", "February", "March", "April",
                 "May", "June", "July", "August", "September",
                 "October", "November", "December")
  daysInMonth = c(31, 28, 31, 30, 31, 30, 31, 
                  31, 30, 31, 30, 31)
  cumDays = cumsum(c(1, daysInMonth))
  
  normPrecip = as.numeric(as.character(precip$normal))
  ### Fill in the various stages with your code
  
  # DAY
  
  days_temp = c(1:365)
  precip_normal = c(2.98, 3.11, 2.40, 0.63, 0.24, 0.08, 0.03, 0.05, 0.21, 0.56, 1.11, 2.05)
  
  # marker position
  
  temp[is.na(temp)] <- 0
  
  high_marker = days_temp[temp$High >= temp$RecordHigh]
  low_marker = days_temp[temp$Low <= temp$RecordLow]
  
  ### Add any additional variables that you will need here
  
  # COLORS
  
  bg_col = "navajowhite"
  
  obs_temp_col = "purple"
  norm_temp_col = "navajowhite4"
  record_temp_col = "tan"
  
  grid_col = "grey40"
  grid_col2 = "grey83"
  
  precip_col = "royalblue"
  precip_fill_col = "tan"
  precip_normal_col = "turquoise"
  
  record_low_col = "steelblue1"
  record_high_col = "orangered1"
  
  # AXES SETUP
  
  month_pos = c()
  for (i in 1:12){
    pos = (cumDays[i] + cumDays[i+1])/2
    month_pos = c(month_pos, pos)
  }
  
  ### Set up the graphics device to plot to pdf and layout
  ### the two plots on one canvas
  ### pdf("", width = , height = )
  ### layout(  )
  
  par(mfrow = c(2,1))
  
  ### Call makePlotRegion to create the plotting region
  ### for the temperature plot
  
  makePlotRegion(xlim = c(1, 366), ylim = c(0, 120), bgcolor = bg_col, ylabels = seq(from = 0, to = 120, by = 10), margins = c(2,4,4,2), cityName = "LAX")
  
  ### Call drawTempRegion 3 times to add the rectangles for
  ### the record, normal, and observed temps
  
  drawTempRegion(days_temp, temp$RecordHigh, temp$RecordLow, record_temp_col)
  drawTempRegion(days_temp, temp$NormalHigh, temp$NormalLow, norm_temp_col)
  drawTempRegion(days_temp, temp$High, temp$Low, obs_temp_col)
  
  drawTempRegion(high_marker, temp$High[high_marker], temp$Low[high_marker], record_high_col)
  drawTempRegion(low_marker, temp$High[low_marker], temp$Low[low_marker], record_low_col)
  
  ### Call addGrid to add the grid lines to the plot
  
  addGrid(location = (cumDays-1), col = grid_col, ltype = 1)
  
  for (i in seq(from = 0, to = 120, by = 10)){
    abline(h = i, col = grid_col2, lty = 1)
  }
  
  ### Add the markers for the record breaking days
  
  #title(xlab = "Record Breaking Days are Represented as Blue for Low and Orange for High")
  
  #text(10, 20, "Record Breakings are Represented as Blue for Low and Orange for High", 3)
  
  # for (i in high_marker){
  #   text(i, 60, "Record High Temperature", 3)
  # }
  # for (i in low_marker){
  #   text(i, 60, "Record Low Temperature", 1)
  # }
  
  
  ### Add the titles 
  title(main="Los Angeles Weather", ylab="Temperature (degress Fahrenheit)", xlab = monthNames)
  axis(3, at = month_pos, labels = monthNames)
  axis(1, at = 70, labels = "Record Breaking Days are Represented as Blue for Low and Orange for High", cex = 0.50)
  
  legend(150, 45, c("Record Temperatures", "Normal Temperatures", "Observed Temperatures"), lty=c(1,1,1), lwd=c(10, 10, 10), col = c(record_temp_col, norm_temp_col, obs_temp_col), cex = 0.55, y.intersp = 0.5)
  
  ### Call makePlotRegion to create the plotting region
  ### for the precipitation plot
  
  makePlotRegion(xlim = c(0, 365), ylim = c(0, 5), bgcolor = bg_col, ylabels = seq(from = 0, to = 10, by = 2), margins = c(4,4,2,2), cityName = "LAX")
  title(ylab="Precipitation (inches)")
  
  for (i in seq(from = 0, to = 10, by = 1)){
    abline(h = i, col = grid_col2, lty = 1)
  }
  
  ### Call monthPrecip 12 times to create each months
  ### cumulative precipitation plot. To do this use
  ### sapply(1:12, function(m) {
  ###             code
  ###             monthPrecip(XXXX)
  ###             })
  ### the anonymous function calls monthPrecip with the
  ### appropriate arguments
  for (i in 1:12){
    days_precip = c(cumDays[i] : (cumDays[i+1]-1))
    #precip_lvl = temp$Precip[days_precip]
    norm = precip_normal[i]
    monthPrecip(day = days_precip, dailyprecip = temp$Precip, normal = norm, lin_col = precip_col, fill_col = precip_fill_col, normal_col = precip_normal_col)
    #text(month_pos[i], norm, paste("Normal: ", norm, sep = " "), 3)
  }
  
  for (i in 1:12){
    text(month_pos[i] + 59, precip_normal[i] + 0.3, paste("Normal: ", precip_normal[i], sep = " "), 3, cex = 0.80)
  }
  
  ### Call addGrid to add the grid lines to the plot
  
  addGrid(location = (cumDays-1), col = grid_col, ltype = 1)
  
  ### Add the titles
  axis(1, at = month_pos, labels = monthNames)
  legend(200, 4.5, c("Normal Precipitation", "Observed Temperatures"), lty=c(1,1), lwd=c(10, 10), col = c(precip_normal_col, precip_col), cex = 0.55, y.intersp = 0.5)
  ### Close the pdf device dev.off()
  
}

### Call: 
finalPlot(temp = laxWeather, precip = laxMonthlyPrecip)


