
makePlotRegion = function(xlim, ylim, bgcolor, ylabels,
                          margins, xtop = TRUE) {
  #print(xlim)
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
  #axis(2, at = ylabels, lty = 1)
  
}

minToPlot = function(time){ # x in format of aa:bb --> aa.bb --> aabb.0 for graphing
  
  dec = sapply(strsplit(time,":"),
         function(x) {
           x <- as.numeric(x)
           x[1]+x[2]/60
         }
  )
  #print(dec)
  return(dec*100)
}

sortPrimCol = function(Theme){
  # main colors (for instruments)
  col = "grey40"
  
  for (char in character_color$Characters){
    if (!is.integer0(grep(tolower(char), tolower(Theme)))){
      #print(char)
      col = character_color$Primary[character_color$Characters == char]
    }
  }
  
  return(col)
  
}

sortSecCol = function(Theme){
  #secondary colors (backgroud for sections)

  for (char in character_color$Characters){
    if (!is.integer0(grep(tolower(char), tolower(Theme)))){
      #print(char)
      return(character_color$Secondary[character_color$Characters == char])
    }
  }
  
}

sortInstr = function(instrument){
  pos = 0
  for (instr in instrument_position$Instrument){
    if (!is.integer0(grep(tolower(instr), tolower(instrument)))){
      pos = instrument_position$Position[instrument_position$Instrument == instr]
    }
  }
  return(pos)
  
}

addSectionLine = function(start_time, end_time, y_pos, color){
  rect(start_time, y_pos-5, end_time, y_pos+5, col = color, border = color)
}

addInstrumentLine = function(start_time, end_time, y_pos, color){
  x_vec = c(start_time:end_time)
  y_vec = rep(y_pos, length(x_vec))
  
  lines(x_vec, y_vec, col = color, lwd = 4)
}

plotCue = function(characters, instruments, start, end){
  length = length(characters[[1]])
  
  plot_col = c()
  plot_pos = c()
  
  for (row in c(1:length)){
    char_set = characters[[1]][row]
    instr_set = instruments[[1]][row]
    
    chars = strsplit(char_set, split = "_")
    instrs = strsplit(instr_set, split = "_")
    
    chars = lapply(chars, function(x){x[!x ==""]})
    instrs = lapply(instrs, function(x){x[!x ==""]})
    
    
    for (char in chars[[1]]){
      for (instr in instrs[[1]]){
        primCol = sortPrimCol(char)
        pos = sortInstr(instr)
        
        plot_col = c(plot_col, primCol)
        plot_pos = c(plot_pos, pos)
      }
    }
    
  }
  
  # separate overlaps
  while (length(which(duplicated(plot_pos)))){
    plot_pos[duplicated(plot_pos)] = plot_pos[duplicated(plot_pos)] + 1
  }
  
  #print("new plot")
  #print(plot_col)
  #print(plot_pos)
  for (row in c(1:length)){
    #print(row)
    #print(plot_pos[row])
    addInstrumentLine(start, end, plot_pos[row], plot_col[row])
  }
}

plotSong = function(data, song){
  # The purpose of this function is to create the whole plot
  # Include here all of the set up that you need for
  # calling each of the above functions.
  # temp is the data frame sfoWeather or laxWeather
  # precip is the data frame sfoMonthlyPrecip or laxMonthlyPrecip
  
  duration = data$durString[data$Title == song][1]
  startTimes = data$start[data$Title == song]
  endTimes = data$end[data$Title == song]
  characters = data$char[data$Title == song]
  instruments = data$instr[data$Title == song]
  
  startTimes_String = startTimes
  
  length = length(characters)
  rows = c(1:length)
  
  #print(startTimes)
  
  for (row in rows){
    startTimes[row] = minToPlot(startTimes[row])
    endTimes[row] = minToPlot(endTimes[row])
  }
  
  startTimes = as.numeric(startTimes)
  endTimes = as.numeric(endTimes)
  
  duration = minToPlot(duration)
  
  par(mfrow = c(1,1))
  bg_col = "navajowhite"
  bs_temp_col = "purple"
  x_min = 0
  x_max = duration #as.numeric(max(song$`End Time`, na.rm = TRUE))
  # 
  # 
  makePlotRegion(xlim = c(0, x_max), ylim = c(0, 80), bgcolor = bg_col, margins = c(2,4,4,2))
  
  
  for (row in rows){
    plotCue(characters[row], instruments[row], startTimes[row], endTimes[row])
  }
  
  title(main = song)
  legend(20, 80, character_color$Characters, col = character_color$Primary, cex = 0.55, y.intersp = 0.5, lty = 10, lwd = 3)
  axis(1, at = startTimes, labels = startTimes_String)
  axis(2, at = as.numeric(instrument_position$Position), labels = instrument_position$Instrument)
}

plotAlbum = function(data){
  
  songs = unique(data$Title)
  
  for (song in songs){
    plotSong(data, song)
  }
}

#plotSong(Han_and_Leia)
#plotAlbum(tfa_info)
plotSong(tfa_info, "Farewell and the Trip"   )
