rm(list=ls(all=TRUE))
training_data <- read.csv("Training Data.csv")
test_data <- read.csv("Test Data.csv")
drop <-c("X")
training_data=training_data[,!(names(training_data) %in% drop)]
test_data=test_data[,!(names(test_data) %in% drop)]



no_of_users <- nrow(training_data)
no_of_items <- ncol(training_data)

library("Matrix")
#------------------------------------------------------------------------#
total_ratings_in_dataset<-nnzero(training_data, na.counted = NA)
#------------------------------------------------------------------------#
total_rating_received_per_item= colSums(training_data != 0)
total_rating_received_per_item=as.matrix(total_rating_received_per_item)
#------------------------------------------------------------------------#
total_count_per_rating<-matrix(0, nrow=1, ncol=5)
for(i in 1:ncol(total_count_per_rating))
{
  total_count_per_rating[1,i]=length(which(training_data==i))
}
#------------------------------------------------------------------------#
training_data<-as.matrix(training_data)
Specific_rating_received_by_each_item = matrix(0, nrow = no_of_items, ncol = 5)
for (i in 1:no_of_items)
{
  for(j in 1:ncol(Specific_rating_received_by_each_item))
  {
    Specific_rating_received_by_each_item[i,j]=nrow(as.matrix(which(training_data[,i]==j)))
  }
}
#------------------------------------------------------------------------#
total_items_rated_by_each_user<-rowSums(training_data != 0)
total_items_rated_by_each_user=as.matrix(total_items_rated_by_each_user)
#------------------------------------------------------------------------#

Specific_rating_provided_by_each_user<-matrix(0,nrow = no_of_users, ncol=5)
for (i in 1:no_of_users)
{
  for(j in 1:ncol(Specific_rating_provided_by_each_user))
  {
    Specific_rating_provided_by_each_user[i,j]=nrow(as.matrix(which(training_data[i,]==j)))
  }
}
#------------------------------------------------------------------------#
Prob_item_is_rated=as.matrix(total_rating_received_per_item/total_ratings_in_dataset)
#------------------------------------------------------------------------#
Prob_of_each_rating=as.matrix(total_count_per_rating/total_ratings_in_dataset)
#------------------------------------------------------------------------#
Prob_each_item_gets_specific_rating<-matrix(0, nrow = no_of_items, ncol = 5)
for (i in 1:no_of_items)
{
  Prob_each_item_gets_specific_rating[i,]=as.matrix(Specific_rating_received_by_each_item[i,]/total_rating_received_per_item[i,1])
}
#some items have no ratings, have to ignore such items
#------------------------------------------------------------------------#
Prob_each_user_provides_specific_rating<-matrix(0, nrow = no_of_users, ncol = 5)
for (i in 1:no_of_users)
{
  Prob_each_user_provides_specific_rating[i,]=as.matrix(Specific_rating_provided_by_each_user[i,]/total_items_rated_by_each_user[i,1])
}
#many users gave no ratings, have to ignore such users
#------------------------------------------------------------------------#

Prob_of_item_given_rating<-matrix(0, nrow = no_of_items, ncol=5)
{
  for(i in 1:nrow(Prob_of_item_given_rating))
  {
    for(j in 1:ncol(Prob_of_item_given_rating))
    {
      Prob_of_item_given_rating[i,j]=(Specific_rating_received_by_each_item[i,j]/total_count_per_rating[1,j])
    }
  }
}

#------------------------------------------------------------------------#

Prob_that_a_user_rates_a_specific_item<-matrix(NA, nrow = no_of_items, ncol = no_of_users)
for(i in 1:no_of_items)
{
  for(j in 1:no_of_users)
  {
    Sum=0
    if(is.nan(Prob_each_user_provides_specific_rating[j,])==FALSE)
    {
      for(k in 1:5)
      {
        Sum= Sum+(Prob_of_item_given_rating[i,k]*Prob_each_user_provides_specific_rating[j,k])
      }
      Prob_that_a_user_rates_a_specific_item[i,j]=Sum
    }
    
  }
}
Prob_that_a_user_rates_a_specific_item<-t(Prob_that_a_user_rates_a_specific_item)
Prob_that_a_user_rates_a_specific_item<-as.matrix(Prob_that_a_user_rates_a_specific_item)
#------------------------------------------------------------------------#
log_operation=matrix( 0,nrow = no_of_users, ncol = no_of_items)
for(i in 1:no_of_users)
{
  for(j in 1:no_of_items)
  {
    if(is.na(Prob_that_a_user_rates_a_specific_item[i,j])==FALSE)
    {
      if(Prob_that_a_user_rates_a_specific_item[i,j] > 0)
        
        log_operation[i,j]=log(Prob_that_a_user_rates_a_specific_item[i,j],base=2)
      
    }
  }
  
  
}

#------------------------------------------------------------------------#
modified_matrix=matrix( NA,nrow = no_of_users, ncol = no_of_items)
for(i in 1:no_of_users)
{
  if(is.na(Prob_that_a_user_rates_a_specific_item[i,])==FALSE)
  {
    for(j in 1:no_of_items)
    {
      modified_matrix[i,j]=(Prob_that_a_user_rates_a_specific_item[i,j]/Prob_item_is_rated[j,1])
    }
  }
  
}
modified_matrix[which(is.nan(modified_matrix))]=NA
#------------------------------------------------------------------------#

clarity_log_operation=matrix( 0,nrow = no_of_users, ncol = no_of_items)
for(i in 1:no_of_users)
{
  if(is.na(Prob_that_a_user_rates_a_specific_item[i,])==FALSE)
  {
    for(j in 1:no_of_items)
    {
      if(modified_matrix[i,j] > 0)
      {
        clarity_log_operation[i,j]=log(modified_matrix[i,j],base=2)
      }
      
    }
    
  }
  
}

#------------------------------------------------------------------------#

Entropy=matrix(0,nrow = no_of_users, ncol = 1)
for(i in 1:no_of_users)
{
  for(j in 1:no_of_items)
  {
    if(is.na(log_operation[i,j])==FALSE && is.nan(Prob_that_a_user_rates_a_specific_item[i,j])==FALSE)
    {
      A=(Prob_that_a_user_rates_a_specific_item[i,j]*log_operation[i,j])
      if(is.nan(A)==FALSE)
      {
        Entropy[i,1]=Entropy[i,1]-A
      }
      
    }
    else{Entropy[i,1]=NA}
  }
}

#-------------------------------------------------------------------------#
Clarity=matrix(0,nrow = no_of_users, ncol = 1)
for(i in 1:no_of_users)
{
  for(j in 1:no_of_items)
  {
    if(is.na(clarity_log_operation[i,j])==FALSE && is.nan(Prob_that_a_user_rates_a_specific_item[i,j])==FALSE)
    {
      A=(Prob_that_a_user_rates_a_specific_item[i,j]*clarity_log_operation[i,j])
      if(is.nan(A)==FALSE)
      {
        Clarity[i,1]=Clarity[i,1]+A
      }
      
    }
    else{Clarity[i,1]=NA}
  }
}
#--------------------------------------------------------------------------#


write.csv(Entropy,'EntropyValues.csv')
write.csv(Clarity,'ClarityValues.csv')
