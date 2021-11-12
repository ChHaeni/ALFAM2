calcEmis <- function(ct, a0, u0, r1, r2, r3, f4, drop.rows) {
  # t is interval length (hr)

  if(is.unsorted(ct)) {
    stop('In calcEmis, ct is not sorted.')
  }

  # Time step
  ddt <- diff(c(0, ct))

  # run 'core model'
  cm <- core_model(ddt, r1, r2, r3, f4, a0, u0)

  # Now combine and drop rows that were added for incorporation in afMod()
  out <- data.frame(ct = ct, dt = NA, 
                    f0 = a0 / (u0 + a0), r1 = r1, r2 = r2, r3 = r3, f4 = f4, 
                    f = cm$a, s = cm$u, 
                    j = NA, e = cm$e, e.int = NA, er = cm$e/(a0 + u0))

  out <- out[!drop.rows, ]

  # Recalculate ddt, e.int, and j.ave.int in case there were dropped rows
  out$dt <- diff(c(0, out$ct))
  out$e.int <- diff(c(0, out$e))
  out$j <- out$e.int/out$dt

  return(out)
}

.wrap_calcEmis <- function(exp.list.list){
  out <- lapply(exp.list.list, function(exp.list){
    data.frame(
      orig.order = exp.list[["orig.order"]],
      calcEmis(
        ct = exp.list[[3]],
        # Calculate a0 and u0 (f4 transfers done in calcEmis())
        a0 = exp.list[1, "__f0"]*exp.list[1, 4],
        u0 = (1 - exp.list[1, "__f0"])*exp.list[1, 4],
        r1 = exp.list[["__r1"]],
        r2 = exp.list[["__r2"]],
        r3 = exp.list[["__r3"]],
        f4 = exp.list[["__f4"]],
        drop.rows = exp.list[["__drop.row"]]), 
      row.names = NULL, check.names = FALSE)
    })
  do.call(rbind, out)
}
