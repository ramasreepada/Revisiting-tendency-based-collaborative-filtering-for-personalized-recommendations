#Creating User-Item Matrix

rm(list=ls(all=TRUE))
training_data <- read.csv("Training Data.csv")
test_data <- read.csv("Test Data.csv")
# Validation_Matrix = read.csv("Validation_Set.csv")
drop <- c("X")
Train_Matrix = training_data[,!(names(training_data) %in% drop)]
Test_Matrix = test_data[,!(names(test_data) %in% drop)]
# Validation_Matrix = Validation_Matrix[,-1]

################
# Test_Matrix = Validation_Matrix

uniq_user <- nrow(Train_Matrix)
uniq_item <- ncol(Train_Matrix)

clarity_values = as.matrix(read.csv('ClarityValues.csv', sep = ','))
clarity_values = clarity_values[,-1]

theta = 0.005
#Mean Ratings of each Item

Mean_rating_Item<-matrix(0,nrow=1,ncol=uniq_item)
# for(j in 1:uniq_item)
# {
#   Sum=0
#   n=0
#   for (i in 1:uniq_user)
#   {
#     if(Train_Matrix[i,j]!=0)
#     {
#       Sum=Sum+Train_Matrix[i,j]
#       n=n+1
#     }
#     Mean_rating_Item[1,j]=round((Sum/n),2)
#   }
# }
stable_user_instances = as.matrix(which(clarity_values > theta))
for(j in 1:uniq_item)
{
  train_nonZero_instances = as.matrix(which(Train_Matrix[,j] > 0))
  
  stable_user_indexes = intersect(train_nonZero_instances, stable_user_instances)
  train_ratings_non_zero = as.matrix(Train_Matrix[stable_user_indexes,j])
  
  if(as.numeric(nrow(as.matrix(stable_user_indexes))) > 0)
  {
    Mean_rating_Item[1,j] = mean(train_ratings_non_zero)
  }
 
}
#Mean Ratings of each User


# for(i in 1:uniq_user)
# {
#   sum=0
#   n=0
#   for (j in 1:uniq_item)
#   {
#     if(Train_Matrix[i,j]!=0)
#     {
#       sum=sum+Train_Matrix[i,j]
#       n=n+1
#     }
#     Mean_rating_User[i,1]=round((sum/n),2)
#   }
# }

Mean_rating_User<-matrix(0,nrow=uniq_user,ncol=1)
Train_Matrix = t(Train_Matrix)

for(i in 1:uniq_user)
{
  train_ratings_non_zero = as.matrix(Train_Matrix[which(Train_Matrix[,i]>0),i])
  Mean_rating_User[i,1] = mean(train_ratings_non_zero)
}
Train_Matrix = t(Train_Matrix)
#Tendancy Based Collaborative filtering

#Tendancy of User

Tendancy_User=matrix(0,nrow=nrow(Train_Matrix),ncol=1)

for(i in 1:nrow(Train_Matrix))
{
  instances_rated_by_i = as.matrix(Train_Matrix[i,which(Train_Matrix[i,] > 0)])
  count_instances = as.numeric(nrow(instances_rated_by_i))
  
  instance_indexes = as.matrix(which(Train_Matrix[i,] > 0))
  
  mean_ratings_of_these_items = as.matrix(Mean_rating_Item[1,instance_indexes])
  
  if(count_instances > 0){
  Tendancy_User[i,1] = (sum(instances_rated_by_i - mean_ratings_of_these_items))/count_instances
  }
}



#Tendancy of Item




# for(j in 1:ncol(Train_Matrix))
# {
#   Count_2=0
#   Sum_2=0
#   for(i in 1:nrow(Train_Matrix))
#   {
#     if(Train_Matrix[i,j]!=0)
#     {
#       Count_2=Count_2+1
#       Sum_2=Sum_2 + (Train_Matrix[i,j]-Mean_rating_User[i,1])
#     }
#   }
#   Tendancy_Item[1,j]=Sum_2/Count_2
# }

Tendancy_Item=matrix(0,nrow=1,ncol=ncol(Train_Matrix))
for(j in 1:ncol(Train_Matrix))
{
  users_who_rated_j = as.matrix(which(Train_Matrix[,j]> 0))
  
  stable_users_who_rated_j = intersect(stable_user_instances,users_who_rated_j)
  
  ratings_of_users_who_rated_j = Train_Matrix[stable_users_who_rated_j,j]
  
  Mean_ratings_of_these_users = Mean_rating_User[stable_users_who_rated_j,]
  
  count_these_users = as.numeric(nrow(as.matrix(stable_users_who_rated_j)))
  
  if(count_these_users > 0 )
  {
    Tendancy_Item[1,j] = (sum(ratings_of_users_who_rated_j - Mean_ratings_of_these_users))/count_these_users
  }
}
#Predicting the values of entries in Testset, i.e Recommendation


Recomm=matrix(0,nrow = nrow(Test_Matrix),ncol=ncol(Test_Matrix))
for(i in 1:nrow(Test_Matrix))
{
  if(as.numeric(nrow(as.matrix(which(Test_Matrix[i,] > 0)))) > 0)
  {
    for(j in 1:ncol(Test_Matrix))
    {
      if(Test_Matrix[i,j]!=0)
      {
        if(is.nan(Tendancy_Item[1,j])==TRUE)
        {
          Test_Matrix[i,j]=0
          Recomm[i,j]=0
        }
        else
        {
          a=Tendancy_Item[1,j]+Mean_rating_User[i,1]
          b=Tendancy_User[i,1]+Mean_rating_Item[1,j]
          Beta=0.55
          if(Tendancy_User[i,1]>0 && Tendancy_Item[1,j]>0)
          {
            Recomm[i,j]=max(a,b)
          }
          else if(Tendancy_User[i,1]<0 && Tendancy_Item[1,j]<0)
          {
            Recomm[i,j]=min(a,b)
          }
          else if((Tendancy_User[i,1]<0 && Tendancy_Item[1,j]>0) && (Mean_rating_User[i,1] < 2.5 && Mean_rating_Item[1,j] < 2.5))
          {
            Recomm[i,j]=min(max(Mean_rating_User[i,1],((b*Beta)+(a*(1-Beta)))),Mean_rating_Item[1,j])
          }
          else
          {
            Recomm[i,j]=((Mean_rating_Item[1,j]*Beta)+(Mean_rating_User[i,1]*(1-Beta)))
          }
        }
        
      }
    }
  }
}

write.csv(Recomm,"Tendency Results Clarity.csv",row.names = F,col.names = F)


