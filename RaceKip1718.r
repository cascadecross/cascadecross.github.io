library(data.table)
library(xlsx)
res <- data.table(read.xlsx("./data/season1718/RaceResults.xlsx", 1))

res[,Last:=gsub("Vandyke", "VanDyke", Last)]
res[,Last:=gsub("peerson", "Peerson", Last)]

res[,PlaceNum:=as.character(res$Place)]
res[grep("[[:alpha:]]", res$PlaceNum), PlaceNum:="0"]
res[,PlaceNum:=as.numeric(res$PlaceNum)]

poi <- fread("./data/points.csv")
setnames(poi, "Place", "PlaceNum")
res <- merge(res, poi, by="PlaceNum", all.x = T)

resComp <- res[grepl("^[0-9]+$", Place),]
resComp[,RaceCount:=.N, by=c("First","Last","Cat")]
resSer <- resComp[RaceCount >= 2,]

overall <- dcast(resComp, First+Last~Cat,
                 value.var = "Points",
                 fun.aggregate = sum, na.rm = T)

top5 <- function(x){
    if(length(x) > 5){
        return(sum(sort(x, decreasing = T)[1:5], na.rm = T))
    }
    return(sum(x, na.rm=T))
}

overallTop5 <- dcast(resComp, First+Last~Cat,
                     value.var = "Points",
                     fun.aggregate = top5)





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

