add.to.data <- function(sp.ids, spec.dat,
                        output_dir = "prairie_saved/data/cleaning") {

    if (!dir.exists(output_dir) &&
        !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
      stop("Could not create ID diagnostic directory: ", output_dir)
    }


    lengths <- sapply(sp.ids, function(x) length(x$temp.id))
    cats <- names(sp.ids[[1]])[names(sp.ids[[1]]) != "temp.id"]
    ## Confirm that each of the ids have all the categories in cat
    ##Make temporary id list and remove all temp_id from it
    sp <- sp.ids
    for (i in 1:length(sp)) { 
      sp[[i]] <- sp[[i]][which(names(sp[[i]]) != "temp.id")]
      }

    non_matching_sublists <- list()
    # Check each sublist in sp
    for (i in 1:length(sp)) {
      if (length(sp[[i]]) == length(cats)) {
        ## print("All sp.ids match in length with cats")
      } else {
        non_matching_sublists <- append(non_matching_sublists, list(sp[[i]]))
      }
    }
    print(non_matching_sublists)

    ## make into data-frame
    dd <- sapply(cats, function(cat)
        rep(sapply(sp.ids, function(x) x[[cat]]), lengths))
    TempID <- unlist(sapply(sp.ids, function(x) x$temp.id))

    ## check for duplicates
    if(sum(table(TempID) > 1) > 0) {
      cat('!!!Duplicate TempID found!!!\n')
      tab.dup <- table(TempID)[table(TempID) > 1]
      print(tab.dup )
      names.dup <- names(TempID)[duplicated(TempID)]
      dup.id <- TempID[duplicated(TempID)]
      names(dup.id) <- names.dup 
      write.csv(dup.id, file=file.path(output_dir, "dup_IDs_names.csv"))
    }

    ## check that no IDs are already present in main data-set
    if(any(TempID %in% spec.dat$UniqueID[!is.na(spec.dat$Species)])) {
        cat('!!!TempID already present!!!\n')
        dup.temp <- TempID[TempID %in%
                     spec.dat$UniqueID[!is.na(spec.dat$Species)]]
        print(dup.temp)
        write.csv(dup.temp, file=file.path(output_dir, "dup_IDs_2.csv"))
    }
    ind <- match(TempID, spec.dat$UniqueID)

    if(any(is.na(ind))){
      bad.ids <- TempID[is.na(ind)]
      print("bad temp ID")
      print(bad.ids)
        write.csv(bad.ids, file=file.path(output_dir, "ids_not_in_specimens.csv"))
    }

    good.ids <- !is.na(ind)

    ind <- ind[good.ids]

    spec.dat[ind, cats] <- ""

    spec.dat[ind, cats] <- dd[good.ids, cats]

    return(spec.dat)
}



