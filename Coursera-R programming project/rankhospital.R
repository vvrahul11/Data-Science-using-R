rankhospital <- function( state, outcome, num = "best"){
  if(num == "best"){num = 1}
  file_name <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data = file_name[ , c(1:11,17,23)]
  if(any(data[,7] == state)){
    state_subset <- data[which(data[,7] == state),]
    if(outcome == "heart attack"){
      x = as.data.frame(as.numeric(state_subset[,11]))
      if(num == 'worst'){num = length(sort(x[,1][which(x[,1] != 'NA')])) }
      num_value = sort(x[,1][which(x[,1] != 'NA')])[num]
      rank_upto_num = sort(x[,1][which(x[,1] != 'NA')])[1:num]
      total_assigned_rank = num - length(which(rank_upto_num %in% num_value))
      selected_rank = num - total_assigned_rank
      hospital.name = sort(state_subset[ which(as.numeric(state_subset[,11]) ==  num_value ),2])[selected_rank]
      hospital.name
    }
    else if(outcome == "heart failure"){
      x = as.data.frame(as.numeric(state_subset[,12]))
      if(num == 'worst'){num = length(sort(x[,1][which(x[,1] != 'NA')])) }
      num_value = sort(x[,1][which(x[,1] != 'NA')])[num]
      rank_upto_num = sort(x[,1][which(x[,1] != 'NA')])[1:num]
      total_assigned_rank = num - length(which(rank_upto_num %in% num_value))
      selected_rank = num - total_assigned_rank
      hospital.name = sort(state_subset[ which(as.numeric(state_subset[,12]) ==  num_value ),2])[selected_rank]
      hospital.name
    }
    else if(outcome == "pneumonia"){
      x = as.data.frame(as.numeric(state_subset[,13]))
      if(num == 'worst'){num = length(sort(x[,1][which(x[,1] != 'NA')])) }
      num_value = sort(x[,1][which(x[,1] != 'NA')])[num]
      rank_upto_num = sort(x[,1][which(x[,1] != 'NA')])[1:num]
      total_assigned_rank = num - length(which(rank_upto_num %in% num_value))
      selected_rank = num - total_assigned_rank
      hospital.name = sort(state_subset[ which(as.numeric(state_subset[,13]) ==  num_value ),2])[selected_rank]
      hospital.name
     
    }  
    else{
      return(stop("invalid outcome"))
    }
  }
  else{
    return(stop("invalid state"))
  }
  
  
}




