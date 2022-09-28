gower_topn_debug <- function(x, y, pair_x=NULL, pair_y = NULL, n=5, eps=1e-8, weights = NULL,
                       ignore_case=FALSE, nthread=getOption("gd_num_thread")){
  gower_work_debug(x=x,y=y,pair_x=pair_x,pair_y=pair_y
             , n=n, eps=eps, weights=weights, ignore_case=ignore_case, nthread=nthread)
}



gower_work_debug <- function(x, y, pair_x, pair_y, n, eps, weights, ignore_case, nthread){
  stopifnot(is.numeric(eps), eps>0)

  if (max(nrow(x), nrow(y)) <= 1000) nthread <- 1L

  if (is.null(pair_x) & is.null(pair_y)){
    xnames <- if(ignore_case) toupper(names(x)) else names(x)
    ynames <- if(ignore_case) toupper(names(y)) else names(y)
    pair <- match(xnames, ynames, nomatch = 0L)
  } else if (is.null(pair_x)){
    pair <- pair_y
  } else {
    if (is.character(pair_x) & is.character(pair_y)){
      m <- match(names(x),pair_x,nomatch=0)
      pair_x <- pair_x[m]
      pair_y <- pair_y[m]
    }
    pair <- numeric(ncol(x))
    pair[pair_x] <- pair_y
  }
  if ( !any(pair > 0) ){
    message("Nothing to compare")
    return( if (is.null(n)){ # gower_dist
      invisible(numeric(0))
    } else { # gower_topn
      invisible(list(distance=matrix(0)[0,0],index=matrix(0L)[0,0]))
    }
    )
  }

  if ( !is.null(weights) && ( any( weights < 0 ) || !all(is.finite(weights)) ) ){
    stop("At least one element of 'weights' is not a finite nonnegative number"
         , call. = FALSE)
  }
  if ( !is.null(weights) && length(weights) < length(pair) ){
    msg <- sprintf("%d weights specified, expected %d"
                   , length(weights), length(pair))
    stop(msg, call. = FALSE)
  }
  # If the user didn't pass any weights, then weight all components of the
  # distance equally.
  if (is.null(weights))
    weights <- rep(1, ncol(x))

  # check column classes

  nthread <- as.integer(nthread)
  ranges <- numeric(length(pair))
  for ( i in seq_along(pair)){
    if (pair[i] == 0 ) next
    ranges[i] <- .Call("R_get_xy_range",x[[i]],y[[pair[i]]],nthread)
  }

  factor_x <- sapply(x,is.factor)
  factor_y <- sapply(y,is.factor)

  for ( i in seq_along(pair) ){
    if ( pair[i] == 0 ) next
    iy <- pair[i]
    if (!factor_x[i] & !factor_y[iy]) next

    if ( factor_x[i] && !factor_y[iy] ){
      stop("Column ", i, " of x is of class factor while matching column "
           , pair[i]," of y is of class ", class(y[[iy]]) )
    }
    if ( !factor_x[i] && factor_y[iy] ){
      stop("Column ", i, " of x is of class ", class(x[[i]]), " while matching column "
           , pair[i]," of y is of class factor" )
    }
    if (factor_x[i] && factor_y[iy]){
      if ( !isTRUE( all.equal( levels(x[[i]]), levels(y[[iy]]) ) ) ){
        stop("Levels in column ", i, " of x do  not match those of column ",
             pair[i], " in y.")

      }
    }
  }

  factor_pair <- as.integer(factor_x)

  eps <- as.double(eps)
  # translate to C-indices (base-0).
  print("made it to the C calls")
  pair <- as.integer(pair-1L)
  if (is.null(n)){
    print('n is null i guess')
    .Call("R_gower", x, y , ranges, pair, factor_pair, eps, weights, nthread)

  } else {
    print('n is not null i guess')
    browser()
    L <- .Call("R_gower_topn", x, y, ranges, pair, factor_pair, as.integer(n), eps, weights, nthread)
    print('passed C call')
    names(L) <- c("index","distance")
    dim(L$index) <- c(n,nrow(x))
    dim(L$distance) <- dim(L$index)
    dimnames(L$index) <- list(topn=NULL,row=NULL)
    dimnames(L$distance) <- dimnames(L$index)
    L
  }

}

RECYCLEWARNING <- tryCatch(1:3+1:2,warning=function(e) e$message)

check_recycling <- function(nx,ny){
  mx <- max(nx,ny)
  mn <- min(nx,ny)
  if ((mx %% mn) != 0) warning(RECYCLEWARNING, call.=FALSE)
}
