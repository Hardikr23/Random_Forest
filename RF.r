# Creating CLass for our nodes
setClass("node", slots=list(is_leaf = "logical", l_node = "node", r_node = "node", feature = "character", optimal_val = "numeric", prediction = "numeric"))

# Function to select the best feature and it value to create the split
optimal_split = function(random_feature_list, new_train_data){
  
  min_rss = Inf
  # min_rss_feature = ""
  # optimal_value = 1
  for (feature in random_feature_list){
    unique_feature_data = unique(new_train_data[feature])
    for (data in unique_feature_data[,1]){
      
      dataset1 = new_train_data[new_train_data[, feature]<=data,]
      dataset2 = new_train_data[new_train_data[, feature]>data,]
      
      rss1 = sum((dataset1[["medv"]] - mean(dataset1[["medv"]]))^2)
      rss2 = sum((dataset2[["medv"]] - mean(dataset2[["medv"]]))^2)
      
      if ((rss1+rss2) < min_rss){
        min_rss = rss1 + rss2
        min_rss_feature = feature
        optimal_value = data
      }
    }
  }
  
  return (list(min_rss = min_rss, min_rss_feature = min_rss_feature, optimal_value = optimal_value))
  }

make_tree = function(train_data, h){
  new_node = new("node")
# check to see if we have reached the desired height
  if (h == 0){
    new_node@is_leaf = TRUE
    new_node@prediction = if(nrow(train_data)==0) 0 else mean(train_data[["medv"]])
  }
  else{
    new_node@is_leaf <- FALSE
    # check to see if there is only one column in the provided dataset. Coz then we don't need to split
    if (length(unique(colnames(train_data))) == 1){
      new_node@is_leaf = TRUE
      new_node@prediction = mean(train_data[["medv"]])
    }
    else{
      new_node@is_leaf = FALSE
      
      # removing the medv col from label list
      medv_index = which(colnames(train_data) == "medv")
      feature_col_list = colnames(train_data)[-medv_index]
      # creating a set of p/3 random features
      random_feature_list = sample(feature_col_list, ceiling(ncol(train_data)/3), replace = FALSE)
      
      # Calling the optimal_split function to decide on the feature and its value to create the split on
      feature_split = optimal_split(random_feature_list, train_data)
      new_node@feature = feature_split$min_rss_feature
      new_node@optimal_val = feature_split$optimal_val
      # print(new_node)
      
      # Now we split the data into 2 parts using the feature and its split value
      left_index = c()
      right_index = c()
      for (index in 1:nrow(train_data)){
        if (train_data[index,new_node@feature] <= new_node@optimal_val){
          left_index = append(left_index, index)
        }
        else{
          right_index = append(right_index, index)
        }
      }

      left_data = train_data[left_index,]
      right_data = train_data[right_index,]
      
      # print(new_node)
      # Creating the children (left and the right) nodes
      new_node@l_node = make_tree(left_data, h-1)
      new_node@r_node = make_tree(right_data, h-1)
    }
  }
  return(new_node)
}

get_pred = function(row, tree){
  if (tree@is_leaf == TRUE){
    return (tree@prediction)
  }else{
    if (row[,tree@feature] <= tree@optimal_val){
      get_pred(row, tree@l_node)
    }
    else{
      get_pred(row, tree@r_node)
    }
  }
}


library(MASS)
boston_data = data.frame(Boston)
# boston_data = boston_data[1:5,]
# Splitting the dataset
set.seed(2110)
train_indexs = sample(1:nrow(boston_data),nrow(boston_data)/2)
train_data = boston_data[train_indexs,]
test_data = boston_data[-train_indexs,]

# PART A
B_list = c(100) #list of no of trees to be generated
h_list = c(5) #list of height of the trees
training_mse_list = c()
test_mse_list = c()

# PART D
for (n in 1:length(B_list)){
  B = B_list[n]
  h = h_list[n]

  all_dtrees = list()
  for (i in 1:B){
    # creating a bts
    bts_index = sample(1:nrow(train_data),replace = TRUE)
    new_train_data = train_data[bts_index,]
    # PART B
    # Creating a tree for each bts
    root = make_tree(new_train_data, h)
    all_dtrees = append(all_dtrees, root)
    cat(i, "trees created out of ", B,"\n")
  }
  
  # PART C
  # Getting predictions for all training and test samples
  training_preds =c()
  test_preds = c()
  for (row in 1:nrow(train_data)){
    train_row_preds = c()
    test_row_preds = c()
    for (tree in all_dtrees){
      train_row_preds = append(train_row_preds, get_pred(train_data[row,], tree))
      test_row_preds = append(test_row_preds, get_pred(test_data[row,], tree))
    }
    training_preds = append(training_preds, mean(train_row_preds))
    test_preds = append(test_preds, mean(test_row_preds))
    
  }
  
  # Calculating the test and train MSE
  training_mse = sum((training_preds - train_data[["medv"]])^2)/nrow(train_data)
  training_mse_list = append(training_mse_list, training_mse)
  print(training_mse_list)
  test_mse = sum((test_preds - test_data[["medv"]])^2)/nrow(test_data)
  test_mse_list = append(test_mse_list, test_mse)
  print(test_mse_list)
  
}

plot(h_list, training_mse_list, type="l",col="red", main="Plot with B=50", ylab = "MSE", xlab="Height", lty=1, ylim=c(0,45))
lines(h_list, test_mse_list,col="green", type="l", lty=1)
legend(2.5, 40, legend=c("Train MSE", "Test MSE"),col=c("red", "green"), lty=1, cex=0.8)
