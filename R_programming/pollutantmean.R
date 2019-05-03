pollutantmean = function(directory, pollutant, id = 1:332){
    result  = c();
    for (i in id){
        num = keep_format(i);
        result = append_result(directory, num, result, pollutant);
    }
    mean(result, na.rm = T)
}
append_result = function(dir, num, r, pollutant){
    temp = read.csv(paste(dir, "/", num, ".csv", sep = ""), header = T);
    append(r, temp[[pollutant]])
}

complete = function(directory, id = 1:332){
    nobs = c()
    for (i in id){
        num = keep_format(i)
        nobs = append_complete(directory, num, nobs)
    }
    tb = data.frame(id, nobs)
    tb
}
append_complete = function(dir, num, r){
    temp = read.csv(paste(dir, "/", num, ".csv", sep = ""), header = T);
    append(r, nrow(na.omit(temp)))
}

keep_format = function(i){
    if (any(i == 1:9)){
        num = paste("00", as.character(i), sep = "");
    }
    else if(any(i == 10:99)){
        num = paste("0", as.character(i), sep = "")
    }
    else{
        num = i
    }
    num
}

corr = function(directory, threhold = 0){
    result = c()
    tb = complete(directory)
    tb = tb[tb[,2] > threhold, 1]
    for(i in tb){
        num = keep_format(i)
        result = append_corr(directory, num, result)
    }
    result
}

append_corr = function(dir, num, r){
    temp = read.csv(paste(dir, "/", num, ".csv", sep = ""), header = T);
    temp = na.omit(temp)
    append(r, cor(temp$sulfate, temp$nitrate))
}