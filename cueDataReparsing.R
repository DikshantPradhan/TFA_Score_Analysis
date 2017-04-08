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

plotToMin = function(time){
  time = time/100
  #print(time)
  
  dec_sec = time%%1
  seconds = dec_sec*60
  seconds = round(seconds, digits = 2)
  minutes = time - dec_sec
  
  string_time = paste(toString(minutes), toString(seconds), sep = ":")
  return(string_time)
}

plotCue = function(characters, instruments, start, end){
  length = length(characters[[1]])
  
  character_set = c()
  instrument_set = c()
  
  for (row in c(1:length)){
    char_set = characters[[1]][row]
    instr_set = instruments[[1]][row]
    
    chars = strsplit(char_set, split = "_")
    instrs = strsplit(instr_set, split = "_")
    
    chars = lapply(chars, function(x){x[!x ==""]})
    instrs = lapply(instrs, function(x){x[!x ==""]})
    
    
    for (char in chars[[1]]){
      for (instr in instrs[[1]]){
        #primCol = sortPrimCol(char)
        #pos = sortInstr(instr)
        
        character_set = c(character_set, char)
        instrument_set = c(instrument_set, instr)
      }
    }
    
  }
  
  data_set = data.frame(characters = character_set, instruments = instrument_set)
  #print(data_set)
  return(data_set)
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
  x_min = 0
  x_max = duration #as.numeric(max(song$`End Time`, na.rm = TRUE))
  # 
  # 
  
  
  #tfa_new_info = data.frame(title = character(), )
  new_title = c()
  new_start = c()
  new_end = c()
  new_char = c()
  new_instr = c()
  
  for (row in rows){
    cue_data = plotCue(characters[row], instruments[row], startTimes[row], endTimes[row])
    cue_data <- data.frame(lapply(cue_data, as.character), stringsAsFactors=FALSE)
    #print(cue_data)
    #print(cue_data$characters)
    #print(cue_data$instruments[2])
    length = dim(cue_data)[1]
    title = song #rep(song, length)
    start = startTimes[row]#rep(startTimes[row], length)
    end = endTimes[row]#rep(endTimes[row], length)
    for (row_ in c(1:length)){
      new_title = c(new_title, title)
      new_start = c(new_start, plotToMin(start))
      #print(start)
      #plotToMin(start)
      new_end = c(new_end, plotToMin(end))
      #print(cue_data$characters[row])
      #print(cue_data$instruments[row])
      new_char = c(new_char, cue_data$characters[row_])
      new_instr = c(new_instr, cue_data$instruments[row_])
    }
  }
  
  song_new_info = data.frame(title = new_title, start = new_start, end = new_end, characters = new_char, instruments = new_instr)
  #print(song_new_info)
  return(song_new_info)
}

song_info = plotSong(tfa_info, "Torn Apart" )
write.csv(song_info, "torn_apart_info.txt")