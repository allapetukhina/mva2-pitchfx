[<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/banner.png" alt="Visit QuantNet">](http://quantlet.de/index.php?p=info)

## [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **spl-enRgenics** [<img src="https://github.com/QuantLet/Styleguide-and-Validation-procedure/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/d3/ia)


```yaml

Name of Quantlet: ENRgenics_ImportEIA

Published in: [Github]

Description: Loads and cleans data from EIA excel file

Keywords: data, cleaning, loading, EIA, energy, excel, csv

See also: ENRgenics_AddOther

Author: David Pollack, Reethesh Jayakumar, Brian Rooney

Submitted:  07.2016

Datafile: sales_revenue.csv.0

```

```r

# Load Libraries and Data
# download data from https://www.eia.gov/electricity/data/eia826/
# options(repos= c("http://cloud.r-project.org/"))
# Install packages if not installed
libraries = c("zoo", "reshape2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

# Load packages
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

check4data = function(file, reverse = FALSE) {
    # this function is a bit of a hack.  The as.integer function creates an NA,
    # which is a relatively slow process so we've used a for loop limited to 100
    # entries
    numlines = 100
    s        = readLines(file)
    if(reverse == FALSE) {
        # from beginning of file
        checkLines = c(1:numlines)
    } else {
        # from end of file
        tLines     = length(s)
        checkLines = rev(c((tLines - numlines):tLines))
    }
    for (i in checkLines) {
        lineVec = unlist(strsplit(s[i], split=","))
        if (is.na(suppressWarnings(as.integer(lineVec[1]))) == F) {
            return(i)
        }
    }
    return(0)
}

# This function fills in the empty cells created by merged cells in Excel
revandcollapse = function(x) {
    x = rev(x)
    x[which(x == "")] = NA
    x = na.locf(x)
    return(x)
}

# This function collapses and merges the header rows
extractheaderinfo = function(file, lines) {
    raw = readLines(file, n = lines)
    for (i in rev(c(1:lines))) {
        lineVec = unlist(strsplit(raw[i], split=","))
        # the above misses the last element if it's blank.
        if(i != lines) {
            if(length(lineVec) < length(prevline)) {
                lineVec = append(lineVec,"", after = length(lineVec))
            }
        }

        lineVec[which(lineVec == "")] = NA
        
        t                  = na.locf(lineVec, na.rm = FALSE)
        t[which(is.na(t))] = ""
        t                  = gsub("\\s", "", t)
        t                  = gsub('"', '', t)
        
        if(i == lines) {
            prevline = t
        } else {
            prevline = paste(prevline, t, sep = "|")
        }
    }
    r = sapply(strsplit(as.character(prevline), "\\|"), function(x) {revandcollapse(x)})
    return(r)
}

# This function converts columns to numeric and fixes formatting
c2num = function(data, headers) {
    els               = sapply(headers, function(x) {length(x)})
    chidxs            = which(els <= 1)
    uhidxs            = setdiff(c(1:length(els)), chidxs)
    data[uhidxs]      = lapply(data[uhidxs], function(x) {suppressWarnings(as.numeric(gsub(",", "",as.character(x))))})
    data[is.na(data)] = 0
    return(data)
}

# datefields
adddate = function(data, fields) {
    names = c("year", "month", "day")
    for (i in 1:length(names)) {
        if(i <= length(fields)) {
            assign(names[i], data[fields[i]])
        } else {
            assign(names[i], NULL)
        }
    }
    if(is.null(year)) {
        year = 1920
    }
    if(is.null(month)) {
        print(month)
        month = 1
    }
    if(is.null(day)) {
        day = 1
    }
    d = ISOdate(year, month, day)
    return(d)
}

splitdata = function(data, headers, catlen = 4) {
    els            = sapply(headers, function(x) {length(x)})
    chidxs         = which(els <= 1)
    uhidxs         = setdiff(c(1:length(els)), chidxs)
    cnames         = c(c(sapply(headers[chidxs],function(x) {x[1]})), c(sapply(headers[uhidxs],function(x) {paste(x[2],x[1], sep="_")})))
    colnames(data) = cnames
    ymd            = which(cnames == "Year" | cnames == "Month" | cnames == "Day")
    Date           = apply(data[chidxs], 1, function(x){adddate(x,ymd)})
    data           = data.frame(data, Date = Date)
    r              = reshape(data, varying=uhidxs, direction="long",idvar="ID",timevar = "Cat", sep="_")
    return(r)
}

load_eia_data = function(file) {
    firstline = check4data(file) - 1
    # [1] 4
    lastline  = check4data(file, reverse = TRUE) - firstline
    # [1] 16014

    rawdata = read.csv(file, header = FALSE, skip = firstline, nrows = lastline, stringsAsFactors = FALSE)
    headers = extractheaderinfo(file,firstline)
    alldata = c2num(rawdata, headers)
    eiadata = splitdata(alldata, headers)
    return(eiadata)
}


# sample usage
# see ENRgenics_AddOther to load 
#file = "../data/sales_revenue.csv.0"
#eiadat = load_eia_data(file)
#head(eiadat)
#write.csv2(reformeddata, paste(file,".out.csv",sep=""))

```

![Picture1](./data/david.jpg)

![Picture2](./data/reethesh.jpg)

![Picture3](./data/brian.jpg)


