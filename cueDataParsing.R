is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

fillInTitle = function(data){
  for (row_num in c(1:nrow(data))){
    rowTitle = data$Title[row_num]
    if (is.na(rowTitle) | rowTitle == "(Concert Suite)"){
      data$Title[row_num] = data$Title[row_num-1]
    }
  }
  return(data)
}

durParse = function(duration){
  time = as.character.Date(duration)
  #typeof(duration)
  timeParts = strsplit(time, split = " ")
  timeParts2 = strsplit(timeParts[[1]][2], ":")
  minDur = paste(timeParts2[[1]][1], timeParts2[[1]][2], sep=":")
  return(minDur)
}

fillInDuration = function(data){
  durString = c()
  for (row_num in c(1:nrow(data))){
    rowDur = data$Dur[row_num]
    if (!is.na(rowDur)){
      durString[row_num] = durParse(rowDur)
    }
    else {
      durString[row_num] = durString[row_num-1]
    }
  }
  data$durString = durString
  return(data)
}

getStart = function(data){
  start = c()
  times = data$`Film Sequence and Musical Themes`
  for (row_num in c(1:nrow(data))){
    parts = strsplit(times[row_num], split = ":")
    startTime = paste(parts[[1]][1], parts[[1]][2], sep=":")
    start[row_num] = startTime
  }
  data$start = start
  return(data)
}

getEnd = function(data){
  end = c()
  start_times = data$start
  duration_times = data$durString
  
  for (row_num in c(1:nrow(data))){
    if (row_num == 107){
      end[row_num] = duration_times[row_num]
      break
    }
    if (start_times[row_num+1] == "0:00"){
      end[row_num] = duration_times[row_num]
    }
    else {
      end[row_num] = start_times[row_num+1]
    }
  }
  data$end = end
  return(data)
}

getCues = function(data){
  cues = data$`Film Sequence and Musical Themes`
  cues_info = c()
  for (row_num in c(1:nrow(data))){
    cue = tolower(cues[row_num])
    parts = strsplit(cue, split = ":")
    info = parts[[1]][3]
    info_parts = strsplit(info, split = ",")
    char_instr = getCharInstr(info_parts)
    data$char[row_num] = list(char_instr$char)
    data$instr[row_num] = list(char_instr$instr)
    
  }
  
  return(data)
}

getCharInstr = function(cue){
  characters = c()
  instruments = c()
  for (cue_frag in cue[[1]]){
    new_char = "NA"
    new_instr = "NA"
    
    for (char in character_instrument$Characters){
      if (!is.integer0(grep(tolower(char), cue_frag))){
        new_char = gsub(pattern = "NA", replacement = "", new_char)
        new_char = paste(new_char, tolower(char), sep="_")
      }
    }
    for (instr in character_instrument$Instruments){
      if (!is.integer0(grep(tolower(instr), cue_frag))){
        new_instr = gsub(pattern = "NA", replacement = "", new_instr)
        new_instr = paste(new_instr, tolower(instr), sep="_")
      }
    }  
    characters = c(characters, new_char)
    instruments = c(instruments, new_instr)
  }
  
  char_instr = list(char = characters, instr = instruments)
  return(char_instr)
}

cueParse = function(data){
  data = fillInTitle(data)
  data = fillInDuration(data)
  data = getStart(data)
  data = getEnd(data)
  data = getCues(data)
  #print(data)
  #write.table(data, "mydata.txt", sep = "\t")
  return(data)
}

tfa_info = cueParse(Cue_Data)
