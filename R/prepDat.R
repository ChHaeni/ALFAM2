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

  # get column names
  nms <- names(dat)

  # TODO:
  # guess col names if missing
  # check if columns have correct entries?!
  # add argument keep.names (default.names?): provided vs. default names
  # default names should be default :)

  # column name dictionaries
  # TODO: should we also allow app(lication) only?
  dict <- list(
    app.mthd = '^app(l|lic(ation)?)?[ ._-](me?tho?d?|type?)$',
    incorp = '^inc(o?r?p?(oration)?)?([ ._-](me?tho?d?|type?))?$',
    man.source = '^(man(ure)?|slur?(ry)?|slry?)([ ._-](s(ou)?r(ce)?|type?))?$'
  )

  # check names
  app.mthd.name <- .check_names(missing(app.mthd.name), app.mthd.name, nms, dict$app.mthd)
  incorp.name <- .check_names(missing(incorp.name), incorp.name, nms, dict$incorp)
  source.name <- .check_names(missing(source.name), source.name, nms, dict$man.source)

  # Application method
  if (length(app.mthd.name) == 1L) {
    # create application method dummy variables
    dat <- .add_dummy_vars(dat, app.mthd.name, app.mthd.levels)
  }

  # Incorporation
  if (length(incorp.name) == 1L) {
    # create incorporation dummy variables
    dat <- .add_dummy_vars(dat, incorp.name, incorp.levels)
  }

  # Source
  if (length(source.name) == 1L) {
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

.add_dummy_vars <- function(x, col, levels) {

    # TODO: 
    #       check column entries
    #       only if col length > 1?
    #       maybe create function for lines below
    #       and call it on all columns, then check
    #       if one has a match -> prefect,
    #       else -> error

    # prepare named vector with known application method levels
    known_levels <- rep(names(levels), lengths(levels))
    names(known_levels) <- unlist(levels)

    # convert column entries to character
    column_levels <- as.character(x[, col])

    # check application levels
    match_levels <- known_levels[tolower(column_levels)]
    
    # TODO: add argument to decide what to do with non standard values.
    #       1) give error (default?) 2) keep  3) remove(?) 4) ignore(?)
    unique_levels <- na.exclude(unique_levels)

    # add application method dummy variables
    for (lvl in unique_levels) {
      x[, paste(col, lvl, sep = '.')] <- as.integer(match_levels == lvl & !is.na(match_levels))
    }

    return(x)
}

# check column names helper function
.check_names <- function(miss, name, nms, rex) {
  if (miss) {
      # TODO: 
      #     check if dummy columns already exist (only if argument is missing)
      #     ^^^^ we don't need to do this, only issue: column might be replaced...
      #     check if more than one column match
      #     if more than one, check entries?
      #     if more than one have valid entries throw error

    # check if directly matches, otherwise use dict
    if (!any(nm_index <- tolower(nms) %in% name)) {
      nm_index <- grep(rex, tolower(nms))
    }

    # return match
    nms[nm_index]
  } else {
    name
  }
}
