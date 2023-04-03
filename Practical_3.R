#Reading the csv file with our model
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(pracma)
library(cluster)
library(factoextra)


#Read the data set and convert it to a data frame
emnist_balanced <- read.csv('emnist-balanced-train.csv')

#Renaming the columns in our the label column in the data frame
emnist_balanced <- emnist_balanced%>%rename('label' = X45)

#Checking for the unique labels in the emnist data frame
unique_values <- unique(emnist_balanced['label'])


#remove the small letter alphabets since there are not complete on the balanced data set
emnist_balanced_new <- select(emnist_balanced, everything()) %>% filter(label <= 9)

set.seed(123)
#Randomly selecting 10,000 of the original image
emnist_shuffled <- sample(nrow(emnist_balanced_new), 10000)
emnist_shuffled <- emnist_balanced_new[emnist_shuffled, ]
emnist_labels <- as.factor(emnist_shuffled$label)


# Making Enhancement to the Images 7
add_white_slash <- function(images) {
  labels <- images$label
  
  for (i in 1:nrow(images)) {
    if (labels[i] == 7) {
      # Add a white horizontal stroke across the image
      image_matrix <- images[i, -1] # removing the first column (label) from the row
      image_dim <- sqrt(length(image_matrix))
      image_matrix <- matrix(image_matrix, nrow = image_dim, ncol = image_dim) # Reshape to original dimensions
      
      # Determine the row for the horizontal stroke
      stroke_row <- floor(image_dim / 2)
      
      # Make the row white
      image_matrix[stroke_row, ] <- 255
      
      # Update the images data.frame
      images[i, -1] <- as.vector(image_matrix)
    }
  }
  
  return(images)
}


# Making Enhancement to the Images 4
feet_added_number_four <- function(images) {
  labels <- images$label
  
  for (i in 1:nrow(images)) {
    if (labels[i] == 4) {
      # Add a white horizontal stroke across the image
      image_matrix <- images[i, -1] # removing the first column (label) from the row
      image_dim <- sqrt(length(image_matrix))
      image_matrix <- matrix(image_matrix, nrow = image_dim, ncol = image_dim) # Reshape to original dimensions
      
      
      # Make the row white
      image_matrix[25:25.5, ] <- 255
      
      # Update the images data.frame
      images[i, -1] <- as.vector(image_matrix)
    }
  }
  
  return(images)
}


# Making Enhancement to the Images 0
zero_added_dot <- function(images) {
  labels <- images$label
  
  for (i in 1:nrow(images)) {
    if (labels[i] == 0) {
      # Add a white horizontal stroke across the image
      image_matrix <- images[i, -1] # removing the first column (label) from the row
      image_dim <- sqrt(length(image_matrix))
      image_matrix <- matrix(image_matrix, nrow = image_dim, ncol = image_dim) # Reshape to original dimensions
      
      #Calculate the centre of mass for our images
      center_x = (ncol(image_matrix) + 1)/2
      center_y = (nrow(image_matrix) + 1)/2
      
      #Drawing a dot on the image
      image_matrix[center_x, center_y] <- 255
      
      # Update the images data.frame
      images[i, -1] <- as.vector(image_matrix)
    }
  }
  
  return(images)
}



#Adding a horizontal slash across the Number 7
enhanced_emnist_balanced <- add_white_slash(emnist_shuffled)

#Adding a feet across the Number 4
enhanced_emnist_balanced<- feet_added_number_four(enhanced_emnist_balanced)

# Adding a dot across the Number Zero
enhanced_emnist_balanced<- zero_added_dot(enhanced_emnist_balanced)


#Visualize random random samples of our emnist data set
#Plot an EMNIST image
plot_emnist <- function(data){
  
  #Create a sample of images to be plotted
  sample_img <- sample(nrow(data), 8)
  
  #create a 2x2 plot
  par(mfrow =c(2, 4))
  
  #create a for loop to loop through our sample of images
  for(i in 1:length(sample_img)){
    #choosing a random sample of an image
    img <- data[sample_img[i], 2:785]
    label <- data[sample_img[i], 1]
  
    #Reshape the image into a 28 by 28 matrix 
    image_matrix <- matrix(img, nrow =28, ncol = 28)
  
    #convert the images into matrix of numbers since it is in character
    image_matrix_numbers <- apply(image_matrix, 2, as.numeric)
  
    #plot the image
    image(1:28, 1:28, image_matrix_numbers,  col = grey((0:255)/255))
    title(paste('label:', label))}
}

#Viewing the original Image 
plot_emnist(emnist_balanced_new)

#Viewing the enhanced Images for Number 7
plot_emnist(enhanced_emnist_balanced)



#Dropping the label column for the original data set
emnist_columns <- select(emnist_shuffled, 2:785)

#Dropping the label column for the original data set
enhanced_columns <- select(enhanced_emnist_balanced, 2:785)


#performing PCA on the original image to reduce dimensions of the data set.
emnist_pca <- prcomp(emnist_columns)

enhanced_pca <- prcomp(enhanced_columns)

#creating a plot to visualize PCA
OG_data_pca_plot <- ggplot(as.data.frame(emnist_pca$x), aes(x=PC1, y=PC2, colour = emnist_labels, label = emnist_label))+
                    geom_text(aes(label=emnist_labels))

OG_data_pca_plot


# Compute variance explained by each principal component
pca_variance_explained <- emnist_pca$sdev^2 / sum(emnist_pca$sdev^2)

# Compute cumulative variance explained
cumm_variance <- cumsum (pca_variance_explained)

# Create a scree plot for the first 10 principal components
scree_plot <- ggplot(data = data.frame(PC = 1:55,
                                       Variance_Explained = pca_variance_explained[1:55],
                                       Cumulative_Variance_Explained = cumm_variance[1:55]),
                     aes(x = PC, y = Variance_Explained)) +
  geom_point(size = 3, aes(color = "pca_variance")) +
  geom_line(aes(color = "pca_variance")) +
  geom_point(aes(y = Cumulative_Variance_Explained, color = "cum_variance"), size = 3) +
  geom_line(aes(y = Cumulative_Variance_Explained, color = "cum_variance")) +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot of PCA on EMNIST Dataset (PC 1-55)") +
  scale_x_continuous(breaks = seq(1, 55, by = 1)) +  # Set x-axis range to 1-30
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "black") + # Add abline at 90%
  scale_color_manual(name='PC component',
                     breaks=c('pca_variance', 'cum_variance'),
                     values=c('pca_variance'='blue', 'cum_variance'='red'))

scree_plot

#Creating a data frame to store the score (Eigen vector) of the reduced data till the 55th PC component
reduced_data <- data.frame(emnist_pca$x[, 1:55])
reduced_data_with_labels <- cbind(emnist_labels, reduced_data)

#Fitting a k-means to identify wrongly classified values
#The K-means did not converge in 10 iterations so iterations is increased to 15 maximum
k_means <- kmeans(reduced_data, centers = 10, iter.max = 15)

#Plotting a Scatter plot for the K-means clustering
# Criteria to select number of clusters using within cluster sum of squares
within_cluster_plot <- fviz_nbclust(x = reduced_data, FUNcluster = kmeans, iter.max = 40, 
                                    method = "wss", k.max = 20)
within_cluster_plot

#Plot to visualize K-means Clustering 
cluster <- k_means$cluster
reduced_data_with_labels <- cbind(cluster, reduced_data_with_labels)

# Plot to visualize K-means Clustering
cluster_plot <- ggplot(data = reduced_data_with_labels, aes(y = cluster)) +
  geom_bar(aes(fill = emnist_labels)) +
  ggtitle('Visualization of Classified Numbers with K-means') +
  theme(plot.title = element_text(hjust = 0.5))

cluster_plot

#Between Cluster sum of Square for the original data set
wcss_original <- sum(k_means$withinss)
tss_original <- sum(var(reduced_data) * (nrow(reduced_data) - 1))

bcss_original <- tss_original - wcss_original
bcss_original


# Calculating the Proportion of Misclassified text.
cluster_one <- select(reduced_data_with_labels, everything()) %>% filter(cluster == 1)
table(cluster_one$emnist_labels)
prop_one <- max(table(cluster_one$emnist_labels))/sum(table(cluster_one$emnist_labels))

cluster_two <- select(reduced_data_with_labels, everything()) %>% filter(cluster == 2)
table(cluster_two$emnist_labels)
prop_two <- max(table(cluster_two$emnist_labels))/sum(table(cluster_two$emnist_labels))


cluster_three <- select(reduced_data_with_labels, everything()) %>% filter(cluster == 3)
table(cluster_three$emnist_labels)
prop_three <- max(table(cluster_three$emnist_labels))/sum(table(cluster_three$emnist_labels))


cluster_four <- select(reduced_data_with_labels, everything()) %>% filter(cluster == 4)
table(cluster_four$emnist_labels)
prop_four <- max(table(cluster_four$emnist_labels))/sum(table(cluster_four$emnist_labels))

cluster_five <- select(reduced_data_with_labels, everything()) %>% filter(cluster == 5)
table(cluster_five$emnist_labels)
prop_five <- max(table(cluster_five$emnist_labels))/sum(table(cluster_five$emnist_labels))

cluster_six <- select(reduced_data_with_labels, everything()) %>% filter(cluster == 6)
table(cluster_six$emnist_labels)
prop_six <- max(table(cluster_six$emnist_labels))/sum(table(cluster_six$emnist_labels))


cluster_seven <- select(reduced_data_with_labels, everything()) %>% filter(cluster == 7)
table(cluster_seven$emnist_labels)
prop_seven <- max(table(cluster_seven$emnist_labels))/sum(table(cluster_seven$emnist_labels))


cluster_eight <- select(reduced_data_with_labels, everything()) %>% filter(cluster == 8)
table(cluster_eight$emnist_labels)
prop_eight <- max(table(cluster_eight$emnist_labels))/sum(table(cluster_eight$emnist_labels))

cluster_nine <- select(reduced_data_with_labels, everything()) %>% filter(cluster == 9)
table(cluster_nine$emnist_labels)
prop_nine <- max(table(cluster_nine$emnist_labels))/sum(table(cluster_nine$emnist_labels))

cluster_ten <- select(reduced_data_with_labels, everything()) %>% filter(cluster == 10)
table(cluster_ten$emnist_labels)
prop_ten <- max(table(cluster_ten$emnist_labels))/sum(table(cluster_ten$emnist_labels))


#calculating the weighted average of the KNN Score.
KNN_score <- sum(prop_one + prop_two + prop_three + prop_four + prop_five + 
                  prop_six + prop_seven + prop_eight + prop_nine + prop_ten)/10

KNN_score


#Creating a data frame to store the score (eigen vector) of the reduced data till the 55th PC component
reduced_data_enhanced <- data.frame(enhanced_pca$x[, 1:55])
k_means_enhanced <- kmeans(reduced_data_enhanced, centers = 10, iter.max = 15)

cluster_enhanced <- k_means_enhanced$cluster

reduced_data_enhanced_label <- cbind(emnist_labels, reduced_data_enhanced)
reduced_data_enhanced_label <- cbind(cluster_enhanced, reduced_data_enhanced_label)

# Plot to visualize K-means Clustering on the enhanced data set
cluster_plot_enhanced <- ggplot(data = reduced_data_enhanced_label, aes(y = cluster_enhanced)) +
  geom_bar(aes(fill = emnist_labels)) +
  ggtitle('Visualization of the Enhanced Classified Numbers with K-means') +
  theme(plot.title = element_text(hjust = 0.5))

cluster_plot_enhanced

# Calculating the Proportion of Misclassified text.
cluster_one_enhanced <- select(reduced_data_enhanced_label, everything()) %>% filter(cluster_enhanced == 1)
table(cluster_one_enhanced$emnist_labels)
prop_one_enhanced <- max(table(cluster_one_enhanced$emnist_labels))/sum(table(cluster_one_enhanced$emnist_labels))


cluster_two_enhanced <- select(reduced_data_enhanced_label, everything()) %>% filter(cluster_enhanced == 2)
table(cluster_two_enhanced$emnist_labels)
prop_two_enhanced <- max(table(cluster_two_enhanced$emnist_labels))/sum(table(cluster_two_enhanced$emnist_labels))


cluster_three_enhanced <- select(reduced_data_enhanced_label, everything()) %>% filter(cluster_enhanced  == 3)
table(cluster_three_enhanced$emnist_labels)
prop_three_enhanced <- max(table(cluster_three_enhanced$emnist_labels))/sum(table(cluster_three_enhanced$emnist_labels))


cluster_four_enhanced <- select(reduced_data_enhanced_label, everything()) %>% filter(cluster_enhanced == 4)
table(cluster_four_enhanced$emnist_labels)
prop_four_enhanced <- max(table(cluster_four_enhanced$emnist_labels))/sum(table(cluster_four_enhanced$emnist_labels))

cluster_five_enhanced <- select(reduced_data_enhanced_label, everything()) %>% filter(cluster_enhanced == 5)
table(cluster_five_enhanced$emnist_labels)
prop_five_enhanced <- max(table(cluster_five_enhanced$emnist_labels))/sum(table(cluster_five_enhanced$emnist_labels))

cluster_six_enhanced <- select(reduced_data_enhanced_label, everything()) %>% filter(cluster_enhanced == 6)
table(cluster_six_enhanced$emnist_labels)
prop_six_enhanced <- max(table(cluster_six_enhanced$emnist_labels))/sum(table(cluster_six_enhanced$emnist_labels))


cluster_seven_enhanced  <- select(reduced_data_enhanced_label, everything()) %>% filter(cluster_enhanced == 7)
table(cluster_seven_enhanced$emnist_labels)
prop_seven_enhanced <- max(table(cluster_seven_enhanced$emnist_labels))/sum(table(cluster_seven_enhanced$emnist_labels))


cluster_eight_enhanced <- select(reduced_data_enhanced_label, everything()) %>% filter(cluster_enhanced == 8)
table(cluster_eight_enhanced$emnist_labels)
prop_eight_enhanced <- max(table(cluster_eight_enhanced$emnist_labels))/sum(table(cluster_eight_enhanced$emnist_labels))

cluster_nine_enhanced <- select(reduced_data_enhanced_label, everything()) %>% filter(cluster_enhanced == 9)
table(cluster_nine_enhanced$emnist_labels)
prop_nine_enhanced <- max(table(cluster_nine_enhanced$emnist_labels))/sum(table(cluster_nine_enhanced$emnist_labels))

cluster_ten_enhanced <- select(reduced_data_enhanced_label, everything()) %>% filter(cluster_enhanced == 10)
table(cluster_ten_enhanced$emnist_labels)
prop_ten_enhanced <- max(table(cluster_ten_enhanced$emnist_labels))/sum(table(cluster_ten_enhanced$emnist_labels))



#calculating the weighted average of the KNN Score.
KNN_score_enhanced <- sum(prop_one_enhanced + prop_two_enhanced + prop_three_enhanced + prop_four_enhanced +
                            prop_five_enhanced + prop_six_enhanced + prop_seven_enhanced +
                            prop_eight_enhanced + prop_nine_enhanced + prop_ten_enhanced)/10

KNN_score_enhanced

#Between Cluster sum of Square for the enhanced data set
wcss_enhanced <- sum(k_means_enhanced$withinss)
tss_enhanced <- sum(var(reduced_data_enhanced) * (nrow(reduced_data_enhanced) - 1))

bcss_enhanced <- tss_enhanced - wcss_enhanced
bcss_enhanced