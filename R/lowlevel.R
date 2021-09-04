#' @title
#' Low Level Cache
#' @rdname lowLevelCache
#' @keywords internal
#' @export

lowLevelCache <-
  function(data, query) {
    R.cache::saveCache(
      object = data, key = list(query),
      dirs = "chariot"
    )
  }

#' @title
#' Low Level Load Cache
#' @rdname lowLevelLoadCache
#' @keywords internal
#' @export

lowLevelLoadCache <-
  function(query) {
    R.cache::loadCache(key = list(query), dirs = "chariot")
  }
