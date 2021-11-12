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

  # TODO:
  # guess col names if missing
  # add argument keep.names (default.names?): provided vs. default names
  # default names should be default :)

  # column name dictionaries
  dict <- list(
    app.mthd = '^app(l|lic(ation)?)?[ ._-]me?tho?d?$',
    incorp = '^inc(o?r?p?(oration)?)?$',
    man.source = '^(man(ure)?|slur?(ry)?|slry?)([ ._-](s(ou)?r(ce)?|ty?pe?))?$'
  )

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

.add_dummy_vars <- function(x, col, levels) {

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
