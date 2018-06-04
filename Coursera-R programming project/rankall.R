rankall <- function(outcome, num = "best"){
  if(num == "best"){num = 1}
  file_name <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states_name = unique(file_name[,7])
  data = file_name[ , c(1:11,17,23)]
  data_matrix = matrix(NA, nrow = 54, ncol = 2)
  if(outcome == "heart attack"){
    i = 1
    for(state in states_name){
      data = file_name[ , c(1:11,17,23)]
      state_subset <- data[which(data[,7] == state),]
      x = as.data.frame(as.numeric(state_subset[,11]))
      if(num == 'worst'){num = length(sort(x[,1][which(x[,1] != 'NA')])) }
      num_value = sort(x[,1][which(x[,1] != 'NA')])[num]
      rank_upto_num = sort(x[,1][which(x[,1] != 'NA')])[1:num]
      total_assigned_rank = num - length(which(rank_upto_num %in% num_value))
      selected_rank = num - total_assigned_rank
      hospital.name = sort(state_subset[ which(as.numeric(state_subset[,11]) ==  num_value ),2])[selected_rank]
      data_matrix[i, 1] = hospital.name
      data_matrix[i, 2] = state
      i = i+ 1
    }   
    data_matrix = as.data.frame(data_matrix)
    names(data_matrix)= c("hospital", "state")
    return(data_matrix)
  }
  
  ###### Heart failure #########
  else if(outcome == "heart failure"){
    i = 1
    for(state in states_name){
      data = file_name[ , c(1:11,17,23)]
      state_subset <- data[which(data[,7] == state),]
      x = as.data.frame(as.numeric(state_subset[,12]))
      if(num == 'worst'){num = length(sort(x[,1][which(x[,1] != 'NA')])) }
      num_value = sort(x[,1][which(x[,1] != 'NA')])[num]
      rank_upto_num = sort(x[,1][which(x[,1] != 'NA')])[1:num]
      total_assigned_rank = num - length(which(rank_upto_num %in% num_value))
      selected_rank = num - total_assigned_rank
      hospital.name = sort(state_subset[ which(as.numeric(state_subset[,12]) ==  num_value ),2])[selected_rank]
      data_matrix[i, 1] = hospital.name
      data_matrix[i, 2] = state
      i = i+ 1
    }   
    data_matrix = as.data.frame(data_matrix)
    names(data_matrix)= c("hospital", "state")
    return(data_matrix)
  }
  
  ###### Pneumonia #######
  else if(outcome == "pneumonia"){
    i = 1
    for(state in states_name){
      data = file_name[ , c(1:11,17,23)]
      state_subset <- data[which(data[,7] == state),]
      x = as.data.frame(as.numeric(state_subset[,13]))
      if(num == 'worst'){num = length(sort(x[,1][which(x[,1] != 'NA')])) }
      num_value = sort(x[,1][which(x[,1] != 'NA')])[num]
      rank_upto_num = sort(x[,1][which(x[,1] != 'NA')])[1:num]
      total_assigned_rank = num - length(which(rank_upto_num %in% num_value))
      selected_rank = num - total_assigned_rank
      hospital.name = sort(state_subset[ which(as.numeric(state_subset[,13]) ==  num_value ),2])[selected_rank]
      data_matrix[i, 1] = hospital.name
      data_matrix[i, 2] = state
      i = i+ 1
    }   
    data_matrix = as.data.frame(data_matrix)
    names(data_matrix)= c("hospital", "state")
    return(data_matrix)
  }    
  ######## Stop if the outcome is invalid ######## 
  else{
    return(stop("invalid outcome"))
  } 
  
}

rankall("heart attack", 4)
rankall("pneumonia", "worst")
rankall("heart failure", 10)

