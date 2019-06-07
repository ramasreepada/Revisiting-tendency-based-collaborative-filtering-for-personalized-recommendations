#Creating User-Item Matrix

rm(list=ls(all=TRUE))
training_data <- read.csv("Training Data.csv")
test_data <- read.csv("Test Data.csv")
# Validation_Matrix = read.csv("Validation_Set.csv")
drop <- c("X")
Train_Matrix = training_data[,!(names(training_data) %in% drop)]
Test_Matrix = test_data[,!(names(test_data) %in% drop)]
# Validation_Matrix = Validation_Matrix[,-1]
similarity_scores = as.matrix(read.csv('Pearson_Values.csv', sep=','))

################
# Test_Matrix = Validation_Matrix

uniq_user <- nrow(Train_Matrix)
uniq_item <- ncol(Train_Matrix)

clarity_values = as.matrix(read.csv('EntropyValues.csv', sep = ','))
clarity_values = clarity_values[,-1]
##############################


k_dash = 40
theta =10.95
#Mean Ratings of each Item
stable_user_instances = as.matrix(which(clarity_values < theta))

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

# Tendancy_Item=matrix(0,nrow=1,ncol=ncol(Train_Matrix))
# for(j in 1:ncol(Train_Matrix))
# {
#   users_who_rated_j = as.matrix(which(Train_Matrix[,j]> 0))
#   
#   stable_users_who_rated_j = intersect(stable_user_instances,users_who_rated_j)
#   
#   ratings_of_users_who_rated_j = Train_Matrix[stable_users_who_rated_j,j]
#   
#   Mean_ratings_of_these_users = Mean_rating_User[stable_users_who_rated_j,]
#   
#   count_these_users = as.numeric(nrow(as.matrix(stable_users_who_rated_j)))
#   
#   if(count_these_users > 0 )
#   {
#     Tendancy_Item[1,j] = (sum(ratings_of_users_who_rated_j - Mean_ratings_of_these_users))/count_these_users
#   }
# }
#Predicting the values of entries in Testset, i.e Recommendation


Recomm=matrix(0,nrow = nrow(Test_Matrix),ncol=ncol(Test_Matrix))
for(i in 1:nrow(Test_Matrix))
{
  if(as.numeric(nrow(as.matrix(which(Test_Matrix[i,] > 0)))) > 0)
  {
    for(j in 1:ncol(Test_Matrix))
    {
      if(Test_Matrix[i,j] == 0)
      {
        # Test_Matrix[i,j]=0
        Recomm[i,j]=0
      }
      else
      {
        # for each active user compute item mean and item tendency
        
        # compute item tendency
        
        
        
        # find people who rated this item first
        
        users_who_rated_currentItem = as.matrix(which(Train_Matrix[,j] > 0))
        
        
        similar_users_in_above_Set = as.matrix(which(similarity_scores[i,users_who_rated_currentItem] > 0))
        
        similar_users_who_rated_current_Item = as.matrix(users_who_rated_currentItem[similar_users_in_above_Set])
        
        similar_users = similar_users_who_rated_current_Item[order(similarity_scores[i,similar_users_who_rated_current_Item], decreasing = TRUE),]
        
        
        # find if all the similar users are stable or not
        # select only the K stable users
        stable_neighbors = 0
        count =0
        
        for(k in 1:as.numeric(nrow(as.matrix(similar_users))))
        {
          current_user = similar_users[k]
          
          if(as.numeric(nrow(as.matrix(which(stable_user_instances == current_user)))) > 0)
          {
            count = count + 1
            if(count == 1)
            {
              stable_neighbors = current_user
            }
            
            else
            {
              stable_neighbors = rbind(stable_neighbors, current_user)
            }
          }
          
          if(count == k_dash)
          {
            break;
          }
          
        }
        
        # find the rating of these stable neighbors on target item
        stable_neighbors = as.matrix(stable_neighbors)
        
        if(as.numeric(nrow(stable_neighbors)) >= 10)
        {
          currentItem_ratings_by_stable_neighbors = Train_Matrix[stable_neighbors,j]
          
          # find the mean rating of all these users
          similar_users_mean_Rating = Mean_rating_User[stable_neighbors]
          
          Tendancy_Item = sum(currentItem_ratings_by_stable_neighbors - similar_users_mean_Rating)/as.numeric(nrow(stable_neighbors))
          current_item_personalized_mean = mean(currentItem_ratings_by_stable_neighbors)
          
          if(is.nan(Tendancy_Item) == TRUE)
            
          {
            Tendancy_Item = 0
            current_item_personalized_mean
          }
        }
        
        else
        {
          similar_users = as.matrix(similar_users)
          
          if(as.numeric(nrow(similar_users)) >= k_dash)
          {
            k_similar_users = as.matrix(similar_users[1:k_dash])
          }
          
          else if(as.numeric(nrow(similar_users)) >= 10)
          {
            k_similar_users = as.matrix(similar_users)
          }
          
          else
          {
            k_similar_users = as.matrix(which(Train_Matrix[,j] > 0))
          }
          
          # user_indexes_who_rated_current_item = as.matrix(which(Train_Matrix[,j] > 0))
          mean_ratings_of_these_users = as.matrix(Mean_rating_User[k_similar_users])
          
          all_ratings_of_users_who_Rated_current_item = as.matrix(Train_Matrix[k_similar_users,j])
          
          Tendancy_Item = sum(all_ratings_of_users_who_Rated_current_item - mean_ratings_of_these_users )/as.numeric(nrow(k_similar_users))
          
          current_item_personalized_mean = mean(all_ratings_of_users_who_Rated_current_item)
          
          if(is.nan(Tendancy_Item) == TRUE)
          {
            Tendancy_Item = 0
            current_item_personalized_mean = 0
          }
        }
        
        
        ### compute tendency of user
        
        
        a=Tendancy_Item + Mean_rating_User[i,1]
        b=Tendancy_User[i,1] + current_item_personalized_mean
        Beta=0.55
        if(Tendancy_User[i,1]>0 && Tendancy_Item>0)
        {
          Recomm[i,j]=max(a,b)
          
        }
        else if(Tendancy_User[i,1]<0 && Tendancy_Item<0)
        {
          Recomm[i,j]=min(a,b)
        }
        else if(Tendancy_User[i,1]<0 && Tendancy_Item >0 && Mean_rating_User[i,1] < 2.5 && current_item_personalized_mean < 2.5)
        {
          Recomm[i,j]=min(max(Mean_rating_User[i,1],((b*Beta)+(a*(1-Beta)))),current_item_personalized_mean)
        }
        else
        {
          Recomm[i,j]=((current_item_personalized_mean*Beta)+(Mean_rating_User[i,1]*(1-Beta)))
        }
        
        
        if((Recomm[i,j]) < 1)
        {
          Recomm[i,j] = 1
        }
        
        if((Recomm[i,j]) > 5)
        {
          Recomm[i,j] = 5
        }
      }
      
      
    }
  }
}
write.csv(Recomm,"Tendency Hybrid Results Entropy.csv",row.names = F,col.names = F)

MAE=0
Count=0
results=Recomm
X<-0
Y<-0
numerator<-0
for(i in 1:uniq_item)
{
  temp=length(which(results[,i]!=0))
  if(temp >0)
  {
    instances=which(results[,i]!=0)
    A=results[instances,i]
    B=Test_Matrix[instances,i]
    MAE=MAE+sum(abs(A-B))
    Count=Count+length(instances)
    for(j in 1:length(A)){
      if(B[j] >= 4 && A[j] >= 4){
        numerator <- numerator + 1
      }
      if(B[j] >= 4){
        X <- X + 1
      }
      if(A[j] >= 4){
        Y <- Y + 1
      }
    }
  }
}
MAE<-MAE/Count
precision <- numerator/Y
recall <- numerator/X
f_score <- 2*precision*recall/(precision + recall)
cat("MAE=", MAE, "Precision=",precision,"Recall=",recall,"F1=",f_score,"\n")

