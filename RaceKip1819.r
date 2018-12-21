library(data.table)
library(readxl)
res <- data.table(read_xlsx("./data/season1819/Overall.xlsx", 1))

## res[,Last:=gsub("Vandyke", "VanDyke", Last)]
## res[,Last:=gsub("peerson", "Peerson", Last)]

setnames(res, c("CAT-POS"), c("Place"))

res[,PlaceNum:=as.character(Place)]
res[grep("[[:alpha:]]", PlaceNum), PlaceNum:="0"]
res[,PlaceNum:=as.numeric(PlaceNum)]

poi <- fread("./data/points.csv")
setnames(poi, "Place", "PlaceNum")
res <- merge(res, poi, by="PlaceNum", all.x = T)

resComp <- res[grepl("^[0-9]+$", Place),]
resComp[,RaceCount:=.N, by=c("ATHLETE", "CATEGORY")]
resSer <- resComp[RaceCount >= 2,]

overall <- dcast(resComp, ATHLETE~CATEGORY,
                 value.var = "Points",
                 fun.aggregate = sum, na.rm = T)

topRace <- function(x, topCount = 4){
    if(length(x) > topCount){
        return(sum(sort(x, decreasing = T)[1:topCount], na.rm = T))
    }
    return(sum(x, na.rm=T))
}

overallTopCount <- dcast(resComp, ATHLETE~CATEGORY,
                     value.var = "Points",
                     fun.aggregate = topRace)





### SCRATCH
## resNames <- unique(res[,.(First, Last)])
## View(unique(resNames[order(First),]))

## points <- data.table(Place=1:10,
##                      Points=c(seq(35,20,-5), seq(18,8,-2)))

## resUpdate <- merge(reg, res, by=c("First", "Last"), all.x=T)

## res$Points[is.na(res$Points)] <- 0

## res[First=="Jesse",]

## uniReg <- unique(reg[, .(.N, paste(CategoryEnteredMerchandiseOrdered, collapse = ",")),
##                          by=c("First", "Last")])[N >1,]

## View(reg[order(First),])

