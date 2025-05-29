
library(RSQLite)

## connect to the relational database
con <- dbConnect(dbDriver("SQLite"),
                 dbname='prairie_saved/prairie.db')


dir.create("prairie_saved/data/relational/traditional",
           showWarnings = FALSE, recursive = TRUE)


## **************************************************
## make a table containing everything
sql <- paste('SELECT * FROM tblSpecimens',
             'JOIN tblInsect',
             'ON tblSpecimens.InsectFK = tblInsect.InsectPK',
             'JOIN tblPlant',
             'ON tblSpecimens.PlantFK = tblPlant.PlantPK',
             'JOIN tblConditions',
             'ON tblSpecimens.ConditionsFK = tblConditions.ConditionsPK',
             'JOIN tblGeography',
             'ON tblSpecimens.GeographyFK = tblGeography.GeographyPK')

res.complete <- dbGetQuery(con, sql)

## drop unwanted columns
drop <- c('InsectPK', 'InsectFK',
          'GeographyPK', 'GeographyFK',
          'PlantPK', 'PlantFK',
          'ConditionsPK', 'ConditionsFK')

res.complete <- res.complete[-match(drop, names(res.complete))]

to.drop <- grep("GeographyFK", colnames(res.complete))
res.complete <- res.complete[-to.drop]

print(paste("after traditional, dim=", nrow(res.complete)))

## set NA to blanks
res.complete[is.na(res.complete)] <- ''

## write full table to csv
write.csv(res.complete, file='prairie_saved/data/relational/traditional/specimens-complete.csv',
          row.names=FALSE)

save(res.complete, file='prairie_saved/data/relational/traditional/specimens-complete.Rdata')

## **************************************************
## veg
## **************************************************

## make a table containing everything
sqlveg <- paste('SELECT * FROM tblVeg',
                'JOIN tblPlant',
                'ON tblVeg.PlantFK = tblPlant.PlantPK',
                'JOIN tblConditions',
                'ON tblVeg.ConditionsFK = tblConditions.ConditionsPK'
                ## 'JOIN tblGeography',
                ## 'ON tblVeg.GeographyFK = tblGeography.GeographyPK'
)
veg.complete <- dbGetQuery(con, sqlveg)

## drop unwanted columns
drop <- c('PlantPK', 'PlantFK',
          'ConditionsPK', 'ConditionsFK')

veg.complete <- veg.complete[-match(drop, names(veg.complete))]

veg.complete <- veg.complete[,!grepl("\\..", colnames(veg.complete))]

print(paste("after veg traditional, dim=", nrow(veg.complete)))

## set NA to blanks
veg.complete[is.na(veg.complete)] <- ''

## write full table to csv
write.csv(veg.complete, file='prairie_saved/data/relational/traditional/veg-complete.csv',
          row.names=FALSE)
save(veg.complete, file='prairie_saved/data/relational/traditional/veg-complete.Rdata')
## **************************************************
## close connection to database
dbDisconnect(con)
