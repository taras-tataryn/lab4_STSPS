# 1. Об'єднувати дані про тренування і тестування для утворення одного набору даних.
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
x <- rbind(x_train, x_test)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
y <- rbind(y_train, y_test)
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
subject <- rbind(subject_train, subject_test)

# 2. Витягувати тільки вимірювання математичного очікування та дисперсії для кожного вимірювання.
features <- read.table("UCI HAR Dataset/features.txt")
indices <- grep("mean()|std()", features[, 2])
x <- x[, indices]

# 3. Використовувати назви активностей для назв діяльності в наборі даних.
activity <- read.table("UCI HAR Dataset/activity_labels.txt")
y[,1] <- activity[y[, 1], 2]

# 4. Надавати назви даним іменами, які описують суть змінних.
names(x) <- features[indices, 2]
names(y) <- "activity"
names(subject) <- "subject"
result <- cbind(subject, y, x)

# 5. З даних пункту 4 створити інший незалежний набір даних 
# з середнім значенням по кожній змінній за кожним видом діяльності 
# і по кожному предмету.
subject_len <- length(table(subject))
activity_len <- length(activity[, 1])
cols <- dim(result)[2]
result_avg <- result[1:(subject_len*activity_len), ]
row <- 1
for(i in 1:subject_len) {
  for(j in 1:activity_len) {
    result_avg[row, 1] <- unique(subject)[, 1][i]
    result_avg[row, 2] <- activity[j, 2]
    tmp <- result[result$subject == i & result$activity == activity[j, 2], ]
    result_avg[row, 3:cols] <- colMeans(tmp[, 3:cols])
    row <- row + 1
  }
}
