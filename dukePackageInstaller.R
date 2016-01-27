# Adam's package installer

wants <- c("dplyr", "ggplot2", "RColorBrewer", "glmnet", "caret", "pROC", "permute", "gbm",
           "klaR", "plyr", "foreach", "doMC")
has   <- wants %in% rownames(installed.packages())

if(any(!has)) install.packages(wants[!has])


check   <- wants %in% rownames(installed.packages())


if(all(check == TRUE)){
    print("This probably worked as expected")
} else {
    print("This might not have worked as expected")
}


# library(c("dplyr", "ggplot2", "RColorBrewer", "glmnet", "caret", "pROC", "permute", "gbm",
"klaR", "plyr", "foreach", "doMC"))