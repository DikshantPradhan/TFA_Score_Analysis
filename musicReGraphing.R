
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

plotToMin = function(time){
  time = as.numeric(time)/100
  #print(time)
  
  dec_sec = time%%1
  seconds = dec_sec*60
  seconds = round(seconds, digits = 2)
  minutes = time - dec_sec
  
  string_time = paste(toString(minutes), toString(seconds), sep = ":")
  return(string_time)
}

minToPlot2 = function(time){ # x in format of aa:bb --> aa.bb --> aabb.0 for graphing
  
  time = time/(60*60)
  #print(time)
  #print(dec)
  return(time)
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
  rect(start_time, y_pos-2, end_time, y_pos+2, col = color, border = color)
}

addInstrumentLine = function(start_time, end_time, y_pos, color){
  print(start_time)
  print(end_time)
  print(y_pos)
  print(color)
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

plotSong = function(song_info){
  # The purpose of this function is to create the whole plot
  # Include here all of the set up that you need for
  # calling each of the above functions.
  # temp is the data frame sfoWeather or laxWeather
  # precip is the data frame sfoMonthlyPrecip or laxMonthlyPrecip
  
  #duration = data$durString[data$Title == song][1]
  startTimes = song_info$start
  endTimes = song_info$end
  characters = song_info$characters
  instruments = song_info$instruments
  
  length = length(characters)
  
  duration = endTimes[length]
  rows = c(1:length)
  
  #print(startTimes)
  
  for (row in rows){
    startTimes[row] = minToPlot2(startTimes[row])
    endTimes[row] = minToPlot2(endTimes[row])
  }
  
  startTimes = as.numeric(startTimes)
  endTimes = as.numeric(endTimes)
  
  duration = minToPlot2(duration)
  
  par(mfrow = c(1,1))
  bg_col = "navajowhite"
  bs_temp_col = "purple"
  x_min = 0
  x_max = duration #as.numeric(max(song$`End Time`, na.rm = TRUE))
  # 
  # 
  makePlotRegion(xlim = c(0, x_max), ylim = c(0, 80), bgcolor = bg_col, margins = c(2,4,4,2))
  
  #print(startTimes)
  #print(endTimes)
  #print(characters)
  #print(instruments)
  
  for (row in rows){
    print(row)
    color = sortPrimCol(characters[row])
    pos = sortInstr(instruments[row])
    #plotCue(characters[row], instruments[row], startTimes[row], endTimes[row])
    addSectionLine(startTimes[row], endTimes[row], pos, color)
  }
  
  startTime_string = c()
  for (time in startTimes){
    #time = as.numeric(time)
    #print(typeof(time))
    startTime_string = c(startTime_string, plotToMin(time*100))
  }
  
  title(main = unique(song_info$title))
  legend(20, 80, character_color$Characters, col = character_color$Primary, cex = 0.55, y.intersp = 0.5, lty = 10, lwd = 3)
  axis(1, at = startTimes, labels = startTime_string)
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
plotSong(torn_apart_info)
