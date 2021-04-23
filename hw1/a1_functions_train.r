
check.dim <- function(data) {
	if (is.na(dim(data)[1])) {
		data <- matrix(data, length(data), 1)
	}
	return(data)
}

#impute using the mean (assuming it's all numeric)
mean.imp <- function(data) {
	data <- check.dim(data)
	for (i in 1:dim(data)[2]) {
		data[which(is.na(data[,i])),i] <- mean(data[,i], na.rm=T)
	}
	return(data)
}

#add missingness to a matrix or dataframe
miss.set.df <- function(dat, prop, type.miss='mcar') {
	dat <- check.dim(dat)
	for (i in 1:dim(dat)[2]) {
		dat[,i] <- miss.set.vec(dat[,i], prop, type.miss)
	}
	return(dat)
}

#add misingness to a vector; mcar = at random; mnar = not at random (remove higher values)
miss.set.vec <- function(x, prop, type.miss='mcar') {
	n.miss <- rbinom(1, length(x), prop)
	if (type.miss == 'mcar') {
		miss.idx <- sample(1:length(x), n.miss, replace=F)
	} else if (type.miss == 'mnar') {
		miss.idx <- miss.high.val(x, n.miss)
	}
	x[miss.idx] <- NA
	return(x)
}

#Remove higher values
miss.high.val <- function(x, n.miss) {
	miss.idx <- order(x, decreasing=T)[1:n.miss]
	return(miss.idx)
}


