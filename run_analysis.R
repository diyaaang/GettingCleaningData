run_analysis <- function()
{
	#1. Merges the training and the test sets to create one data set.
	#2. Extracts only the measurements on the mean 
		# and standard deviation for each measurement. 
	#3. Uses descriptive activity names to name the activities in the data set
	#4. Appropriately labels the data set with descriptive variable names. 
	#5. From the data set in step 4, 
		# creates a second, independent tidy data set 
		# with the average of each variable 
		# for each activity and each subject.
	
	# convert all .txt files into data frames
	subtest = read.table("subject_test.txt")
	subtrain = read.table("subject_train.txt")
	xtest = read.table("X_test.txt")
	ytest = read.table("y_test.txt")
	xtrain = read.table("X_train.txt")
	ytrain = read.table("y_train.txt")

	features = read.table("features.txt")
	activity = read.table("activity_labels.txt")
		names(activity)[1] <- "code"
		names(activity)[2] <- "description"

	# row bind ytest and ytrain
	yinfo <- rbind(ytest,ytrain)
		# rename column name for activity (satisfies step 4)
		names(yinfo) <- "activity"

	# row bind xtest and xtrain
	xinfo <- rbind(xtest,xtrain)
		# apply column names from features.txt (satisfies step 4)
		colnames(xinfo) <- features[,2]

	#row bind subtest and subtrain
	sub <- rbind(subtest,subtrain)
		# rename column name for subject (satisfies step 4)
		names(sub) <- "subject"

	# column bind xinfo and yinfo
	xy <- cbind(xinfo,yinfo)

	# column bind sub and xy (satisfies step 1)
	all <- cbind(sub,xy)

	# extract subject and activity columns
	# extract all columns with names containing "mean" or "std" 
		#(satisfies step 2)
	# assign to data frame
	meanstdv <- all[,grepl("-mean|-std|subject|activity",colnames(all))]

	# create new column called action, made of factors with values from activity column
	meanstdv$action <- factor(meanstdv$activity)

	# assign values from activity$description to factor values of meanstdv$action
	# (satisfies step 3)
	levels(meanstdv$action) <- activity$description

	# aggregate data frame with average of each variable for each activity and each subject
	sub <- aggregate(meanstdv,list(meanstdv$subject,meanstdv$activity), mean)

	# write.table() using row.name=FALSE
	data <- write.table(sub,row.name=FALSE)

}
