library(dplyr)

# Load Dataset into R

        # Load the subject dataset
        subj_test <- read.table("test\\subject_test.txt")
        subj_train <- read.table("train\\subject_train.txt" )
        
        # Load X datatset
        X_test <- read.table("test\\X_test.txt" )
        X_train <- read.table("train\\X_train.txt" )
        
        #Load Y dataset
        Y_test <- read.table("test\\Y_test.txt" )
        Y_train <- read.table("train\\Y_train.txt" )
        
        # Load features for dataset X
        features <- read.table("features.txt" )
        
        #Load Activities
        activities <- read.table("activity_labels.txt" )
        
        
        #Inertial Signals Test Dataset Loading
        
        bodyaccx_test <- read.table("test\\Inertial Signals\\body_acc_x_test.txt")
        bodyaccy_test <- read.table("test\\Inertial Signals\\body_acc_y_test.txt")
        bodyaccz_test <- read.table("test\\Inertial Signals\\body_acc_z_test.txt")
        
        bodygyrox_test <- read.table("test\\Inertial Signals\\body_gyro_x_test.txt")
        bodygyroy_test <- read.table("test\\Inertial Signals\\body_gyro_y_test.txt")
        bodygyroz_test <- read.table("test\\Inertial Signals\\body_gyro_z_test.txt")
        
        
        totalaccx_test <- read.table("test\\Inertial Signals\\total_acc_x_test.txt")
        totalaccy_test <- read.table("test\\Inertial Signals\\total_acc_y_test.txt")
        totalaccz_test <- read.table("test\\Inertial Signals\\total_acc_z_test.txt")
        
        
        
        #Inertial Signals Train Dataset Loading
        
        bodyaccx_train <- read.table("train\\Inertial Signals\\body_acc_x_train.txt")
        bodyaccy_train <- read.table("train\\Inertial Signals\\body_acc_y_train.txt")
        bodyaccz_train <- read.table("train\\Inertial Signals\\body_acc_z_train.txt")
        
        bodygyrox_train <- read.table("train\\Inertial Signals\\body_gyro_x_train.txt")
        bodygyroy_train <- read.table("train\\Inertial Signals\\body_gyro_y_train.txt")
        bodygyroz_train <- read.table("train\\Inertial Signals\\body_gyro_z_train.txt")
        
        
        totalaccx_train <- read.table("train\\Inertial Signals\\total_acc_x_train.txt")
        totalaccy_train <- read.table("train\\Inertial Signals\\total_acc_y_train.txt")
        totalaccz_train <- read.table("train\\Inertial Signals\\total_acc_z_train.txt")
        
        
        #Replace the labels 
        getActivity <- function(currLabel, newLabel){
                #currLabel takes in a table with one column for which the replacement needs to be done
                #newLabel takes in the dataframe for mapping where first column is the key
                for (i in 1:nrow(currLabel)) {
                        currLabel[i,1] <- newLabel[currLabel[i,1],2]
                        #print(newLabel[currLabel[i,1],2])
                        #print(currLabel[i,1])
                }
                currLabel
        }
   
# Merge Datasets
        
        #Merge X,Y,Subject
        subject <- rbind(subj_test,subj_train)
        X_ds <- rbind(X_test,X_train)
        Y_ds <- rbind(Y_test, Y_train )
        colnames(X_ds) <- features[,2]
        colnames(Y_ds) <- "Activity"
        colnames(subject) <- "Subject"
        
        #Merge Inertial Signals Dataset
        bodyaccx = rbind(bodyaccx_test,bodyaccx_train)
        bodyaccy = rbind(bodyaccy_test,bodyaccy_train)
        bodyaccz = rbind(bodyaccz_test,bodyaccz_train)
        
        bodygyrox = rbind(bodygyrox_test,bodygyrox_train)
        bodygyroy = rbind(bodygyroy_test,bodygyroy_train)
        bodygyroz = rbind(bodygyroz_test,bodygyroz_train)
        
        totalaccx = rbind(totalaccx_test,totalaccx_train)
        totalaccy = rbind(totalaccy_test,totalaccy_train)
        totalaccz = rbind(totalaccz_test,totalaccz_train)
        
        # Create combined dataset
        Activity <- merge(Y_ds ,  activities, by.x = "Activity", by.y = "V1" , sort = FALSE)[,2]
        X_ds <- cbind(subject, Activity,X_ds)
        
        
        # Create means subset 
        subset <- X_ds[,1:2]
        subset2 <- X_ds[,which(grepl("mean()|std()",names(X_ds)))]
        Means_ds <- cbind(subset,subset2)
        
# Create Tidy DS
        write.table(X_ds,"Activity_Recog_Tidy_DS.txt",row.names = FALSE, col.names = TRUE)
        write.table(Means_ds,"Means_and_std_Tidy_DS.txt",row.names = FALSE, col.names = TRUE)
        
        
        finalDs <-aggregate(X_ds, list(X_ds$Subject, X_ds$Activity), mean, simplify = TRUE)
        write.table(finalDs,"Mean_by_Activity_subject_DS.txt",row.names = FALSE, col.names = TRUE)
        
        
        