# Prepare input data for ALFAM2mod()

prepDat <- function(dat, app.mthd.name = 'app.mthd', incorp.name = 'incorp', source.name = 'man.source',
                    app.mthd.levels = list(ts = 'ts', 
                                           bc = c('broadcast', 'bc', 'broadspread', 'bredspredning', 'bredspredt'),
                                           os = c('open slot injection', 'os', 'open-slot injection', 'shallow injection', 'nedf\u00E6ldning i gr\u00E6s'), 
                                           cs = c('closed slot injection', 'cs', 'closed-slot injection', 'deep injection', 'nedf\u00E6ldning p\u00E5 sort jord')),
                    incorp.levels = list(shallow = c('shallow', 'harrow'), deep = c('deep', 'plough', 'plow', 'nedbringning')),
                    source.levels = list(pig = c('pig', 'swine', 'svin', 'svinegylle')),
                    keep.all = FALSE
                    ) {
  # get number of initial columns
  nca <- ncol(dat)

  # Application method
  if (app.mthd.name %in% names(dat)) {
    # create application method dummy variables
    dat <- .add_dummy_vars(dat, app.mthd.name, app.mthd.levels)
  }

  # Incorporation
  if (incorp.name %in% names(dat)) {
    # create incorporation dummy variables
    dat <- .add_dummy_vars(dat, incorp.name, incorp.levels)
  }

  # Source
  if (source.name %in% names(dat)) {
    # create incorporation dummy variables
    dat <- .add_dummy_vars(dat, source.name, source.levels)
  }

  # get new ncol
  nco <- ncol(dat)

  # return new data.frame
  if (keep.all) {
    dat
  } else if (nco > nca) {
    dat[, nca:nco, drop = FALSE]
  } else {
    NULL
  }

}

#dat <- data.frame(ct = 168, app.mthd = c('open slot injection', 'cs', 'bsth'), t.incorp = 12, incorp = c('none', 'shallow', 'deep'), man.source = 'pig')
#dat
#prepDat(dat)
#prepDat(dat, keep.all = TRUE)

.add_dummy_vars <- function(x, col, levels, unknown = c('ignore', 'error', 'keep')) {

    # prepare named vector with known application method levels
    known_levels <- rep(names(levels), lengths(levels))
    names(known_levels) <- unlist(levels)

    # convert column entries to character
    column_levels <- as.character(x[, col])

    # check application levels
    match_levels <- known_levels[tolower(column_levels)]
    
    # TODO: add argument to decide what to do with non standard values.
    #       1) give error (default?) 2) keep  3) remove(?) 4) ignore(?)
    if (anyNA(unique_levels <- unique(match_levels))) {
      # what to do with unknown values?
      switch(unknown[1]
        , 'ignore' = {
          unique_levels <- na.exclude(unique_levels)
        }
        , 'error' = {
          na_levels <- unique(column_levels[is.na(match_levels)])
          stop(paste0('Unknown levels in column ', col,':\n - ',
            paste(na_levels, sep = '\n - ')))
        }
        , 'keep' = {
          # TODO: what should be done with 'incompatible' names?
          # so far fix with make.names. Should code in model be able to handle such spaces?
          match_levels[is.na(match_levels)] <- make.names(column_levels[is.na(match_levels)])
          unique_levels <- unique(match_levels)
        }
        , stop('Argument "unknown" should be one of "ignore", "error" or "keep"')
      )
    }

    # add application method dummy variables
    for (lvl in unique_levels) {
      x[, paste(col, lvl, sep = '.')] <- as.integer(match_levels == lvl & !is.na(match_levels))
    }

    return(x)
}
