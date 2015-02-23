#' Function to grade 560R:Intro to R for Epi Homework 2
#'
#' This function will provide general feedback for homework 2
#' @param file Name of file with extension (e.g. "Krall_560R_hw2.R")
#' @export 
grade_homework2 <- function(file) {
	ls1 <- ls()[-which(ls() == "file")]
	rm(list = ls1)
	age_kid <- 1
	try1 <- try(source(file, local = T))
	if (class(try1) == "try-error") {
		out <- "ERROR: the code does not run without errors"
	} else {
		
		p1 <- hw2_1_sleep(file)
		p2 <- hw2_2_diab(file)
		out <- cat("Part 1:", p1, "\n Part 2:", p2)
	}
	cat(out)
}


#' Function to Part 1 in Homework 2
#'
#' This function will grade Homework 2 part 1
#' @param file Name of file with extension (e.g. "Krall_560R_hw2.R")
hw2_1_sleep <- function(file) {
	#Remove all but file
	ls1 <- ls()[-which(ls() == "file")]
	rm(list = ls1)
	
	
	# Define ages
	ages <- c(0, 1, 2, 2.5, 3, 5, 6, 7, 13, 15)
	res <- vector()
	
	# For each age
	for(jk in 1 : length(ages)) {
		# Set age value
		age_kid <- ages[jk]
		# Source file
		source(file, local = T, echo = F)
		
		# Save sleep duration
		res[jk] <- sleep_duration
	}
	
	values <- c("11-14 hours", "10-13 hours", "9-11 hours", "not defined")
	values1 <- values[c(4, 1, 1, 1, 2, 2, 3, 3, 3, 4)]
	l1 <- length(which(res == values1))
	
	if(l1 >= 9) {
		out <- "Looks good"
	}else if(l1 <= 8 & l1 >= 6) {
		out <- "Some errors: may want to check your code"
	}else if(l1 <= 5) {
		out <- "Multiple errors: may want to check your code"
	}
	out
}



#' Function to Part 2 in Homework 2
#'
#' This function will grade Homework 2 part 2
#' @param file Name of file with extension (e.g. "Krall_560R_hw2.R")
hw2_2_diab <- function(file) {
	ls1 <- ls()[-which(ls() == "file")]
	rm(list = ls1)
	age_kid <- 1
	source(file, local = T, echo = F)
	
	if("tab_diab" %in% ls())  {
		p1 <- 1
		p2 <- 1 * (sum(dim(tab_diab)) == 15)
	
		cn <- c('Variable', 'Mean', 'N_diabetic', 'Mean_diabetic', 'N_not_diabetic', 'Mean_not_diabetic', 'Meandiff', 'Meandiff_LB', 'Meandiff_UB', 'Meandiff_Pval')
		m1 <- match(cn, colnames(tab_diab))
		if(length(which(is.na(m1))) > 0) {
			p3 <- 0
		} else {
			p3 <- sum(1 * (seq(1, 10) == m1))/10	
		}

		vars <- c('chol', 'hdl', 'age', 'height', 'weight')
		m1 <- match(vars, tab_diab[, 1])
		if(length(which(is.na(m1))) > 0) {
			p4 <- 0
		} else {
			p4 <- sum(1 * (seq(1, 5) == m1))/5
		}
		
		
		grade_partial <- p1 + p2 + p3 + p4
		if(grade_partial == 4) {
			out <- "Your final table is properly formatted"
		} else if (p3 != 1 | p4 != 1) {
			out <- "ERROR: column names or variable names incorrect"
		} else {
			out <- "ERROR: final table is not formatted correctly and cannot be graded"
		}

	
	} else{
		
		out <- "ERROR: no table found"
	}
	out
	
}


