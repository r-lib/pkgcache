
async <- function(fun) {
  fun <- as_function(fun)
  if (is_async(fun)) return(fun)

  async_fun <- fun
  body(async_fun) <- bquote({
    mget(ls(environment(), all.names = TRUE), environment())
    fun2 <- function() {
      evalq(
      .(body(fun)),
      envir = parent.env(environment())
      )
    }

    deferred$new(
      type = "async",
      action = function(resolve) resolve(fun2())
    )
  })

  # This is needed, otherwise async_fun might not find 'deferred'
  async_env <- new.env(parent = environment(async_fun))
  async_env$deferred <- deferred
  environment(async_fun) <- async_env

  mark_as_async(async_fun)
}

mark_as_async <- function(fun) {
  attr(body(fun), "async")$async <- TRUE

  ## These are not valid any more, anyway
  attr(fun, "srcref") <- NULL
  attr(body(fun), "srcref") <- NULL

  fun
}

is_async <- function(fun) {
  assert_that(is.function(fun))
  is.list(a <- attr(body(fun), "async")) && identical(a$async, TRUE)
}

#' @importFrom assertthat assert_that on_failure<-
NULL

is_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_string) <- function(call, env) {
  paste0(deparse(call$x), " is not a string (length 1 character)")
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_flag) <- function(call, env) {
  paste0(deparse(call$x), " is not a flag (length 1 logical)")
}

is_action_function <- function(x) {
  is.function(x) && length(formals(x)) %in% 1:2
}

on_failure(is_action_function) <- function(call, env) {
  paste0(deparse(call$x), " is not a function with two arguments")
}

is_time_interval <- function(x) {
  inherits(x, "difftime") ||
    (is.numeric(x) && length(x) == 1 && !is.na(x) && x >= 0)
}

on_failure(is_time_interval) <- function(call, env) {
  paste0(deparse(call$x), " is not a valid time interval")
}

is_count <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && as.integer(x) == x
}

on_failure(is_count) <- function(call, env) {
  paste0(deparse(call$x), " is not a count (non-negative integer)")
}

is_flag <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}

on_failure(is_flag) <- function(call, env) {
  paste0(deparse(call$x), " must be a flag (length 1 logical)")
}

call_function <- function(func, args = list()) {
  func; args

  id <- NULL

  deferred$new(
    type = "pool-task", call = sys.call(),
    action = function(resolve) {
      resolve
      reject <- environment(resolve)$private$reject
      id <<- get_default_event_loop()$add_pool_task(
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        list(func = func, args = args))
    },
    on_cancel = function(reason) {
      if (!is.null(id)) {
        get_default_event_loop()$cancel(id)
      }
    }
  )
}

call_function <- mark_as_async(call_function)

async_constant <- function(value = NULL) {
  force(value)
  deferred$new(
    type = "constant", call = sys.call(),
    function(resolve) resolve(value))
}

async_constant <- mark_as_async(async_constant)

async_env <- new.env(parent = emptyenv())
async_env$loops <- list()

get_default_event_loop <- function() {
  num_loops <- length(async_env$loops)
  if (num_loops == 0) {
    err <- make_error(
      "You can only call async functions from an async context",
      class = "async_synchronization_barrier_error"
    )
    stop(err)
  }

  async_env$loops[[num_loops]]
}

push_event_loop <- function() {
  num_loops <- length(async_env$loops)
  if (num_loops > 0) async_env$loops[[num_loops]]$suspend()
  new_el <- event_loop$new()
  async_env$loops <- c(async_env$loops, list(new_el))
  new_el
}

pop_event_loop <- function() {
  num_loops <- length(async_env$loops)
  async_env$loops[[num_loops]] <- NULL
  if (num_loops > 1) async_env$loops[[num_loops - 1]]$wakeup()
}

async_next <- function(el = NULL) {
  el <- el %||% find_sync_frame()$new_el
  if (is.null(el)) stop("No async context")
  ## TODO: some visual indication that something has happened?
  if (! el$run("once")) message("[ASYNC] async phase complete")
}

# nocov start

async_step <- function() {
  el <- find_sync_frame()$new_el
  if (is.null(el)) stop("No async context")
  ## TODO: some visual indication that something has happened?
  old <- options(async_debug_steps = TRUE)
  on.exit(options(old))
  if (! el$run("once")) {
    message("[ASYNC] async phase complete")
  }
}

async_step_back <- function() {
  options(async_debug_steps = FALSE)
  message("[ASYNC] step back, you still need to 'c'ontinue")
}

# nocov end

async_list <- function(def = NULL) {
  def <- def %||% find_sync_frame()$res
  if (is.null(def)) stop("No async context")
  info <- list()
  find_parents <- function(def) {
    info <<- c(info, list(get_private(def)$get_info()))
    prn <- get_private(def)$parents
    lapply(prn, find_parents)
  }
  find_parents(def)

  do.call(rbind, info)
}

async_tree <- function(def = NULL) {
  def <- def %||% find_sync_frame()$res
  data <- async_list(def)
  root <- as.character(def$get_id())
  cli::tree(data, root = root)
}

async_debug <- function(id, action = TRUE, parent = TRUE) {
  def <- find_deferred(id)
  if (is.null(def)) stop("Cannot find deferred `", id, "`")
  prv <- get_private(def)

  if (prv$state != "pending") {
    message("[ASYNC] ", id, " already resolved")
    return(invisible())
  }

  what <- character()
  if (action) {
    if (prv$running) {
      message("[ASYNC] ", id, " action already running")
    } else if (is.null(prv$action)) {
      message("[ASYNC] ", id, " has no action")
    } else {
      ## TODO: make a copy? Or should the deferred make a copy?
      debug1(prv$action)
      what <- "action"
    }
  }

  if (parent) {
    ## TODO: make copies?
    debug_all(prv$parent_resolve)
    debug_all(prv$parent_reject)
    what <- c(what, "parent callbacks")
  }

  if (length(what) == 1) {
    message("[ASYNC] ", id, " debugging ", what)
  }
  if (length(what) == 2) {
    message("[ASYNC] ", id, " debugging ", what[1], " and ", what[2])
  }

  invisible(def)
}

async_wait_for <- function(id) {
  el <- find_sync_frame()$new_el
  if (is.null(el)) stop("No async context")
  def <- find_deferred(id)
  if (is.null(def)) stop("Cannot find deferred `", id, "`")
  priv <- get_private(def)
  while (priv$state == "pending") el$run("once")
  message("[ASYNC] ", id, "  resolved")
}

async_where <- function(calls = sys.calls(), parents = sys.parents(),
                        frm = get_async_frames()) {
  afrm <- viapply(frm, "[[", "frame")
  num <- seq_along(calls)

  src <- lapply(calls, get_source_position)

  res <- data.frame(
    stringsAsFactors = FALSE,
    call = I(calls),
    parent = parents,
    filename = vcapply(src, "[[", "filename"),
    position = vcapply(src, "[[", "position"),
    async = num %in% afrm
  )

  res$def_id <- NA_integer_
  res$def_id[afrm] <- viapply(frm, function(x) x$deferred)
  res$def_cb_type <- NA_character_
  res$def_cb_type[afrm] <- vcapply(frm, function(x) x$type)
  res$def_call <- I(list(NULL))
  res$def_call[afrm] <- lapply(frm, "[[", "call")

  def_src <- lapply(res$def_call[afrm], get_source_position)
  res$def_filename <- NA_character_
  res$def_filename[afrm] <- vcapply(def_src, "[[", "filename")
  res$def_position <- NA_character_
  res$def_position[afrm] <- vcapply(def_src, "[[", "position")

  class(res) <- c("async_where", class(res))
  res
}

# nocov start

print.async_where <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}

# nocov end

format.async_where <- function(x, ...) {
  paste0(paste(
    formatC(seq_len(nrow(x)), width = 3),
    vcapply(x$call, expr_name),
    paste0(" ", x$filename, ":", x$position),
    ifelse (! x$async, "",
            paste0("\n    ", x$def_id, " ", x$def_cb_type, " ",
                   x$def_call, " ", x$def_filename, ":", x$def_position)),
    collapse = "\n"
  ), "\n")
}

get_async_frames <- function() {
  drop_nulls(lapply(seq_along(sys.frames()), function(i) {
    if (! is.null(data <- sys.frame(i)$`__async_data__`)) {
      list(frame = i + data$skip %||% 1L, deferred = data[[1]], type = data[[2]],
           call = get_private(data[[3]])$mycall)
    }
  }))
}

find_sync_frame <- function() {
  for (i in seq_along(sys.frames())) {
    cand  <- sys.frame(-i)
    if (isTRUE(cand$`__async_synchronise_frame__`)) return(cand)
  }
}

find_async_data_frame <- function() {
  frames <- sys.frames()
  for (i in seq_along(frames)) {
    cand  <- sys.frame(-i)
    if (!is.null(data <- cand$`__async_data__`)) {
      return(list(frame = length(frames) - i + 1L, data = data))
    }
  }
}

find_deferred <- function(id, def = NULL) {
  def <- def %||% find_sync_frame()$res
  if (is.null(def)) stop("No async context")
  search_parents <- function(def) {
    if (def$get_id() == id) return(def)
    prn <- get_private(def)$parents
    for (p in lapply(prn, search_parents)) {
      if (!is.null(p)) return(p)
    }
  }
  search_parents(def)
}

# nocov start

debug1 <- function(fun) {
  debugonce(fun)
}

async_debug_shortcuts <- function() {
  as <- function(name, fun) {
    makeActiveBinding(name, fun, .GlobalEnv)
  }
  as(".an", async_next)
  as(".as", async_step)
  as(".asb", async_step_back)
  as(".al", async_list)
  as(".at", async_tree)
  as(".aw", async_where)
}

async_debug_remove_shortcuts <- function() {
  tryCatch(
    rm(list = c(".an", ".as", ".asb", ".al", ".at", ".aw"),
       envir = .GlobalEnv),
    error = function(x) x)
}

# nocov end

debug_all <- function(fun) {
  debug(fun)
}

#' @importFrom R6 R6Class

deferred <- R6Class(
  "deferred",
  public = list(
    initialize = function(action = NULL, on_progress = NULL, on_cancel = NULL,
                          parents = NULL, parent_resolve = NULL,
                          parent_reject = NULL, type = NULL,
                          call = sys.call(-1))
      async_def_init(self, private, action, on_progress, on_cancel,
                     parents, parent_resolve, parent_reject, type, call),
    then = function(on_fulfilled)
      def_then(self, private, on_fulfilled),
    catch = function(...)
      def_catch(self, private, ...),
    finally = function(on_finally)
      def_finally(self, private, on_finally),
    cancel = function(reason = "Cancelled")
      def_cancel(self, private, reason),
    share = function() { private$shared <<- TRUE; invisible(self) },
    get_id = function() private$id
  ),

  private = list(
    action = NULL,
    running = FALSE,
    id = NULL,
    type = NULL,
    state = c("pending", "fulfilled", "rejected")[1],
    event_loop = NULL,
    value = NULL,
    children = list(),
    progress_callback = NULL,
    cancel_callback = NULL,
    cancelled = FALSE,
    dead_end = FALSE,
    parents = NULL,
    parent_resolve = NULL,
    parent_reject = NULL,
    shared = FALSE,
    mycall = NULL,

    run_action = function()
      def__run_action(self, private),

    null = function()
      def__null(self, private),

    resolve = function(value)
      def__resolve(self, private, value),
    reject = function(reason)
      def__reject(self, private, reason),
    progress = function(data)
      def__progress(self, private, data),

    make_error_object = function(err)
      def__make_error_object(self, private, err),

    maybe_cancel_parents = function(reason)
      def__maybe_cancel_parents(self, private, reason),
    add_as_parent = function(child)
      def__add_as_parent(self, private, child),

    get_info = function()
      def__get_info(self, private)
  )
)

async_def_init <- function(self, private, action, on_progress,
                           on_cancel, parents, parent_resolve,
                           parent_reject, type, call) {

  private$type <- type
  private$id <- get_id()
  private$event_loop <- get_default_event_loop()
  private$parents <- parents
  private$action <- action
  private$mycall <- call

  "!DEBUG NEW `private$id` (`type`)"

  assert_that(is.null(on_progress) || is.function(on_progress))
  private$progress_callback <- on_progress
  assert_that(is.null(on_cancel) || is.function(on_cancel))
  private$cancel_callback <- on_cancel

  ## Handle the parents

  private$parent_resolve <- def__make_parent_resolve(parent_resolve)
  private$parent_reject <- def__make_parent_reject(parent_reject)

  for (prt in parents) {
    prt_pvt <- get_private(prt)
    prt_pvt$add_as_parent(self)
  }

  invisible(self)
}

def__run_action <- function(self, private) {
  if (private$running) return()
  action <- private$action
  private$running <- TRUE
  private$action <- NULL
  "!DEBUG ACTION `private$type` `private$id`"

  if (!is.null(action)) {
    if (!is.function(action)) {
      action <- as_function(action)
      formals(action) <- alist(resolve = NULL, progress = NULL)
    }
    assert_that(is_action_function(action))

    action_args <- names(formals(action))
    args <- list(private$resolve)
    if (!is.na(pr_arg <- match("progress", action_args))) {
      args$progress <- private$progress
    }

    private$event_loop$add_next_tick(
      function() {
        if (isTRUE(getOption("async_debug_steps", FALSE))) debug1(action)
        `__async_data__` <- list(private$id, "action", self, skip = 2L)
        do.call(action, args) },
      function(err, res) if (!is.null(err)) private$reject(err))
  }

  ## If some parents are done, we want them to notify us.
  ## We also start the ones that are not running yet
  for (prt in private$parents) {
    prt_priv <- get_private(prt)
    if (prt_priv$state != "pending") {
      def__call_then(
        if (prt_priv$state == "fulfilled") "parent_resolve" else "parent_reject",
        self, prt_priv$value, prt_priv$id)
    }
    prt_priv$run_action()
  }
}

def_then <- function(self, private, on_fulfilled = NULL,
                     on_rejected = NULL) {
  force(self)
  force(private)

  if (! identical(private$event_loop, get_default_event_loop())) {
    err <- make_error(
      "Cannot create deferred chain across synchronization barrier",
      class = "async_synchronization_barrier_error")
    stop(err)
  }

  if (!is_deferred(on_fulfilled)) {
    parent_resolve <- def__make_parent_resolve(on_fulfilled)
    parent_reject <- def__make_parent_reject(on_rejected)

    deferred$new(parents = list(self),
                 type = paste0("then-", private$id),
                 parent_resolve = parent_resolve,
                 parent_reject = parent_reject,
                 call = sys.call(-1))

  } else {
    private$add_as_parent(on_fulfilled)
    child_private <- get_private(on_fulfilled)
    child_private$parents <- c(child_private$parents, self)
    self
  }
}

def_catch <- function(self, private, ...) {
  def_then(self, private, on_rejected = list(...))
}

def_finally <- function(self, private, on_finally) {
  force(on_finally)
  def_then(
    self,
    private,
    on_fulfilled = function(value) {
      on_finally()
      value
    },
    on_rejected = function(reason) {
      on_finally()
      stop(reason)
    }
  )
}

def_cancel <- function(self, private, reason) {
  if (private$state != "pending") return()
  cancel_cond <- structure(
    list(message = reason %||% "Deferred computation cancelled", call = NULL),
    class = c("async_cancelled", "error", "condition")
  )
  private$reject(cancel_cond)
  invisible(self)
}

def__null <- function(self, private) {
  self$.__enclos_env__$private$dead_end <- TRUE
  invisible(self)
}

def__resolve <- function(self, private, value) {
  if (private$cancelled) return()
  if (private$state != "pending") return()

  if (is_deferred(value)) {
    private$parent_resolve <- def__make_parent_resolve(NULL)
    private$parent_reject <- def__make_parent_reject(NULL)
    value$then(self)

  } else {
    if (!private$dead_end && !length(private$children) &&
        !private$shared) {
      ## This cannot happen currently
      "!DEBUG ??? DEAD END `self$get_id()`"   # nocov
      warning("Computation going nowhere...")   # nocov
    }

    "!DEBUG +++ RESOLVE `self$get_id()`"
    private$state <- "fulfilled"
    private$value <- value
    for (child in private$children) {
      def__call_then("parent_resolve", child, value, self$get_id())
    }
    private$maybe_cancel_parents(private$value)
    private$parents <- NULL
  }
}

def__make_error_object <- function(self, private, err) {
  class(err) <- unique(c("async_rejected", class(err)))
  err
}

def__make_parent_resolve <- function(fun) {
  if (is.null(fun)) {
    function(value, resolve, id) resolve(value)
  } else if (!is.function(fun)) {
    fun <- as_function(fun)
    function(value, resolve, id) resolve(fun(value))
  } else if (num_args(fun) == 0) {
    function(value, resolve, id) resolve(fun())
  } else if (num_args(fun) == 1) {
    function(value, resolve, id) resolve(fun(value))
  } else if (identical(names(formals(fun)),
                       c("value", "resolve"))) {
    function(value, resolve, id) fun(value, resolve)
  } else if (identical(names(formals(fun)),
                       c("value", "resolve", "id"))) {
    fun
  } else {
    stop("Invalid parent_resolve callback")
  }
}

def__make_parent_reject <- function(fun) {
  if (is.null(fun)) {
    function(value, resolve, id) stop(value)
  } else if (is.list(fun)) {
    def__make_parent_reject_catch(fun)
  } else if (!is.function(fun)) {
    fun <- as_function(fun)
    function(value, resolve, id) resolve(fun(value))
  } else if (num_args(fun) == 0) {
    function(value, resolve, id) resolve(fun())
  } else if (num_args(fun) == 1) {
    function(value, resolve, id) resolve(fun(value))
  } else if (identical(names(formals(fun)),
                       c("value", "resolve"))) {
    function(value, resolve, id) fun(value, resolve)
  } else if (identical(names(formals(fun)),
                       c("value", "resolve", "id"))) {
    fun
  } else {
    stop("Invalid parent_reject callback")
  }
}

def__make_parent_reject_catch <- function(handlers) {
  handlers <- lapply(handlers, as_function)
  function(value, resolve, id) {
    ok <- FALSE
    ret <- tryCatch({
      quo <- quo(tryCatch(stop(value), !!!handlers))
      ret <- eval_tidy(quo)
      ok <- TRUE
      ret
    }, error = function(x) x)

    if (ok) resolve(ret) else stop(ret)
  }
}

def__reject <- function(self, private, reason) {
  if (private$cancelled) return()
  if (private$state != "pending") return()

  ## 'reason' cannot be a deferred here

  "!DEBUG !!! REJECT `self$get_id()`"
  private$state <- "rejected"
  private$value <- private$make_error_object(reason)
  if (inherits(private$value, "async_cancelled")) {
    private$cancelled <- TRUE
  }
  if (!is.null(private$cancel_callback)) {
    private$cancel_callback(conditionMessage(private$value))
  }
  for (child in private$children) {
    def__call_then("parent_reject", child, private$value, self$get_id())
  }
  private$maybe_cancel_parents(private$value)
  private$parents <- NULL
}

def__maybe_cancel_parents <- function(self, private, reason) {
  for (parent in private$parents) {
    if (is.null(parent)) next

    parent_priv <- get_private(parent)
    if (parent_priv$state != "pending") next
    if (parent_priv$shared) next
    parent$cancel(reason)
  }
}

def__call_then <- function(which, x, value, id)  {
  force(value); force(id)
  private <- get_private(x)
  if (!private$running) return()
  if (private$state != "pending") return()

  cb <- private[[which]]
  private$event_loop$add_next_tick(
    function() {
      if (isTRUE(getOption("async_debug_steps", FALSE))) {
        debug1(private[[which]])        # nocov
      }
      `__async_data__` <- list(private$id, "parent", x)
      private[[which]](value, private$resolve, id)
    },
    function(err, res) if (!is.null(err)) private$reject(err))
}

def__add_as_parent <- function(self, private, child) {
  "!DEBUG EDGE [`private$id` -> `child$get_id()`]"

  if (! identical(private$event_loop, get_private(child)$event_loop)) {
    err <- make_error(
      "Cannot create deferred chain across synchronization barrier",
      class = "async_synchronization_barrier_error")
    stop(err)
  }
  if (length(private$children) && !private$shared) {
    stop("Deferred value is already owned")
  }

  private$children <- c(private$children, child)

  if (get_private(child)$running) private$run_action()
  if (private$state == "pending") {
    ## Nothing to do

  } else if (private$state == "fulfilled") {
    def__call_then("parent_resolve", child, private$value, self$get_id())

  } else {
    def__call_then("parent_reject", child, private$value, self$get_id())
  }
}

def__progress <- function(self, private, data) {
  if (private$state != "pending") return()
  if (is.null(private$progress_callback)) return()
  private$progress_callback(data)
}

def__get_info <- function(self, private) {
  res <- data.frame(
    stringsAsFactors = FALSE,
    id = private$id,
    parents = I(list(viapply(private$parents, function(x) x$get_id()))),
    label = as.character(private$id),
    call = I(list(private$mycall)),
    children = I(list(viapply(private$children, function(x) x$get_id()))),
    type = private$type %||%  "unknown",
    running = private$running,
    state = private$state,
    cancelled = private$cancelled,
    shared = private$shared
  )
  src <- get_source_position(private$mycall)
  res$filename <- src$filename
  res$position <- src$position
  res$label <- paste0(
    res$id, " ",
    if (private$state == "fulfilled") paste0(cli::symbol$tick, " "),
    if (private$state == "rejected")  paste0(cli::symbol$cross, "  "),
    deparse(private$mycall)[1], " @ ",
    res$filename, ":", res$position)

  res
}

is_deferred <- function(x) {
  inherits(x, "deferred")
}

delay <- function(delay) {
  force(delay)
  id <- NULL
  deferred$new(
    type = "delay", call = sys.call(),
    action = function(resolve) {
      assert_that(is_time_interval(delay))
      force(resolve)
      id <<- get_default_event_loop()$add_delayed(
        delay,
        function() TRUE,
        function(err, res) resolve(TRUE)
      )
    },
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    }
  )
}

delay <- mark_as_async(delay)

async_detect <- function(.x, .p, ..., .limit = Inf) {
  if (.limit < length(.x)) {
    async_detect_limit(.x, .p, ..., .limit = .limit)
  } else {
    async_detect_nolimit(.x, .p, ...)
  }
}

async_detect <- mark_as_async(async_detect)

async_detect_nolimit <- function(.x, .p, ...) {
  defs <- lapply(.x, async(.p), ...)
  nx <- length(defs)
  done <- FALSE
  ids <- NULL

  deferred$new(
    type = "async_detect", call = sys.call(),
    parents = defs,
    action = function(resolve) {
      ids <<- viapply(defs, function(x) x$get_id())
      if (nx == 0) resolve(NULL)
    },
    parent_resolve = function(value, resolve, id) {
      if (!done && isTRUE(value)) {
        done <<- TRUE
        resolve(.x[[match(id, ids)]])
      } else if (!done) {
        nx <<- nx - 1L
        if (nx == 0) resolve(NULL)
      }
    }
  )
}

async_detect_limit <- function(.x, .p, ..., .limit = .limit) {
  len <- length(.x)
  nx <- len
  .p <- async(.p)
  args <- list(...)

  done <- FALSE
  nextone <- .limit + 1L
  firsts <- lapply(.x[seq_len(.limit)], .p, ...)
  ids <- viapply(firsts, function(x) x$get_id())

  self <- deferred$new(
    type = "async_detect (limit)", call = sys.call(),
    parents = firsts,
    action = function(resolve) if (nx == 0) resolve(NULL),
    parent_resolve = function(value, resolve, id) {
      if (!done && isTRUE(value)) {
        done <<- TRUE
        resolve(.x[[match(id, ids)]])
      } else if (!done) {
        nx <<- nx - 1L
        if (nx == 0) {
          resolve(NULL)
        } else if (nextone <= len) {
          dx <- .p(.x[[nextone]], ...)
          ids <<- c(ids, dx$get_id())
          dx$then(self)
          nextone <<- nextone + 1L
        }
      }
    }
  )

  self
}

#' @importFrom R6 R6Class

event_loop <- R6Class(
  "event_loop",
  public = list(
    initialize = function()
      el_init(self, private),

    add_http = function(handle, callback, file = NULL, progress = NULL,
                        data = NULL)
      el_add_http(self, private, handle, callback, file, progress, data),
    add_process = function(conns, callback, data)
      el_add_process(self, private, conns, callback, data),
    add_r_process = function(conns, callback, data)
      el_add_r_process(self, private, conns, callback, data),
    add_pool_task = function(callback, data)
      el_add_pool_task(self, private, callback, data),
    add_delayed = function(delay, func, callback, rep = FALSE)
      el_add_delayed(self, private, delay, func, callback, rep),
    add_next_tick = function(func, callback, data = NULL)
      el_add_next_tick(self, private, func, callback, data),

    cancel = function(id)
      el_cancel(self, private, id),
    cancel_all = function()
      el_cancel_all(self, private),

    run = function(mode = c("default", "nowait", "once"))
      el_run(self, private, mode = match.arg(mode)),

    suspend = function()
      el_suspend(self, private),
    wakeup = function()
      el_wakeup(self, private)
  ),

  private = list(
    create_task = function(callback, ..., id =  NULL, type = "foobar")
      el__create_task(self, private, callback, ..., id = id, type = type),
    ensure_pool = function(...)
      el__ensure_pool(self, private, ...),
    get_poll_timeout = function()
      el__get_poll_timeout(self, private),
    run_pending = function()
      el__run_pending(self, private),
    run_timers = function()
      el__run_timers(self, private),
    is_alive = function()
      el__is_alive(self, private),
    update_time = function()
      el__update_time(self, private),
    io_poll = function(timeout)
      el__io_poll(self, private, timeout),
    update_curl_data = function()
      el__update_curl_data(self, private),

    id = NULL,
    time = Sys.time(),
    stop_flag = FALSE,
    tasks = list(),
    timers = Sys.time()[numeric()],
    pool = NULL,
    curl_fdset = NULL,                 # return value of multi_fdset()
    curl_poll = TRUE,                  # should we poll for curl sockets?
    curl_timer = NULL,                 # call multi_run() before this
    next_ticks = character(),
    worker_pool = NULL
  )
)

el_init <- function(self, private) {
  private$id <- new_event_loop_id()
  invisible(self)
}

#' @importFrom curl multi_add parse_headers_list handle_data

el_add_http <- function(self, private, handle, callback, progress, file,
                        data) {
  self; private; handle; callback; progress; outfile <- file; data

  id  <- private$create_task(callback, list(handle = handle, data = data),
                             type = "http")
  private$ensure_pool()
  if (!is.null(outfile)) cat("", file = outfile)

  content <- NULL

  multi_add(
    handle = handle,
    pool = private$pool,
    done = function(response) {
      task <- private$tasks[[id]]
      private$tasks[[id]] <- NULL
      response$content <- do.call(c, as.list(content))
      response$file <- outfile
      task$callback(NULL, response)
    },
    data = function(bytes, ...) {
      if (!is.null(outfile)) {
        ## R runs out of connections very quickly, especially because they
        ## are not removed until a gc(). However, calling gc() is
        ## expensive, so we only do it if we have to. This is a temporary
        ## solution until we can use our own connections, that are not
        ## so limited in their numbers.
        con <- tryCatch(
          file(outfile, open = "ab"),
          error = function(e) { gc(); file(outfile, open = "ab") } # nocov
        )
        writeBin(bytes, con)
        close(con)
      } else {
        content <<- c(content, list(bytes))
      }
    },
    fail = function(error) {
      task <- private$tasks[[id]]
      private$tasks[[id]] <- NULL
      error <- make_error(message = error)
      class(error) <- unique(c("async_rejected", "async_http_error",
                               class(error)))
      task$callback(error, NULL)
    }
  )
  id
}

el_add_process <- function(self, private, conns, callback, data) {
  self; private; conns; callback; data
  data$conns <- conns
  private$create_task(callback, data, type = "process")
}

el_add_r_process <- function(self, private, conns, callback, data) {
  self; private; conns; callback; data
  data$conns <- conns
  private$create_task(callback, data, type = "r-process")
}

el_add_pool_task <- function(self, private, callback, data) {
  self; private; callback; data
  id <- private$create_task(callback, data, type = "pool-task")
  if (is.null(async_env$worker_pool)) {
    async_env$worker_pool <- worker_pool$new()
  }
  async_env$worker_pool$add_task(data$func, data$args, id, private$id)
  id
}

el_add_delayed <- function(self, private, delay, func, callback, rep) {
  force(self); force(private); force(delay); force(func); force(callback)
  force(rep)
  id <- private$create_task(
    callback,
    data = list(delay = delay, func = func, rep = rep),
    type = "delayed"
  )
  # This has to be real time, because our event loop time might
  # be very much in the past when his is called.
  private$timers[id] <- Sys.time() + as.difftime(delay, units = "secs")
  id
}

el_add_next_tick <- function(self, private, func, callback, data) {
  force(self) ; force(private) ; force(callback); force(data)
  data$func <- func
  id <- private$create_task(callback, data = data, type = "nexttick")
  private$next_ticks <- c(private$next_ticks, id)
}

#' @importFrom curl multi_cancel

el_cancel <- function(self, private, id) {
  private$next_ticks <- setdiff(private$next_ticks, id)
  private$timers  <- private$timers[setdiff(names(private$timers), id)]
  if (id %in% names(private$tasks) && private$tasks[[id]]$type == "http") {
    multi_cancel(private$tasks[[id]]$data$handle)
  } else if (id %in% names(private$tasks) &&
             private$tasks[[id]]$type %in% c("process", "r-process")) {
    private$tasks[[id]]$data$process$kill()
  } else if (id %in% names(private$tasks) &&
             private$tasks[[id]]$type == "pool-task") {
    async_env$worker_pool$cancel_task(id)
  }
  private$tasks[[id]] <- NULL
  invisible(self)
}

#' @importFrom curl multi_cancel multi_list

el_cancel_all <- function(self, private) {
  http <- multi_list(pool = private$pool)
  lapply(http, multi_cancel)
  private$next_ticks <- character()
  private$timers <- Sys.time()[numeric()]

  ## Need to cancel pool tasks, these are interrupts for the workers
  types <- vcapply(private$tasks, "[[", "type")
  ids <- vcapply(private$tasks, "[[", "id")
  for (id in ids[types == "pool-task"]) {
    self$cancel(id)
  }

  private$tasks <-  list()
  invisible(self)
}

el_run <- function(self, private, mode) {

  ## This is closely modeled after the libuv event loop, on purpose,
  ## because some time we might switch to that.

  alive <- private$is_alive()
  if (! alive) private$update_time()

  while (alive && !private$stop_flag) {
    private$update_time()
    private$update_curl_data()
    private$run_timers()
    ran_pending <- private$run_pending()
    ## private$run_idle()
    ## private$run_prepare()

    timeout <- 0
    if ((mode == "once" && !ran_pending) || mode == "default") {
      timeout <- private$get_poll_timeout()
    }

    private$io_poll(timeout)
    ## private$run_check()
    ## private$run_closing_handles()

    if (mode == "once") {
      ## If io_poll returned without doing anything, that means that
      ## we have some timers that are due, so run those.
      ## At this point we have surely made progress
      private$update_time()
      private$run_timers()
    }

    alive <- private$is_alive()
    if (mode == "once" || mode == "nowait") break
  }

  private$stop_flag <- FALSE

  alive
}

el_suspend <- function(self, private) {
  ## TODO
}

el_wakeup <- function(self, private) {
  ## TODO
}

el__run_pending <- function(self, private) {
  next_ticks <- private$next_ticks
  private$next_ticks <- character()
  for (id in next_ticks) {
    task <- private$tasks[[id]]
    private$tasks[[id]] <- NULL
    call_with_callback(task$data$func, task$callback,
                       info = task$data$error_info)
  }

  ## Check for workers from the pool finished before, while another
  ## event loop was active
  finished_pool <- FALSE
  pool <- async_env$worker_pool
  if (!is.null(pool)) {
    done_pool <- pool$list_tasks(event_loop = private$id, status = "done")
    finished_pool <- nrow(done_pool) > 0
    for (tid in done_pool$id) {
      task <- private$tasks[[tid]]
      private$tasks[[tid]] <- NULL
      res <- pool$get_result(tid)
      err <- res$error
      res <- res[c("result", "stdout", "stderr")]
      task$callback(err, res)
    }
  }

  length(next_ticks) > 0 || finished_pool
}

#' @importFrom curl multi_run multi_fdset

el__io_poll <- function(self, private, timeout) {

  types <- vcapply(private$tasks, "[[", "type")

  ## The things we need to poll, and their types
  ## We put the result here as well
  pollables <- data.frame(
    stringsAsFactors = FALSE,
    id = character(),
    pollable = I(list()),
    type = character(),
    ready = character()
  )

  ## HTTP.
  if (private$curl_poll) {
    curl_pollables <- data.frame(
      stringsAsFactors = FALSE,
      id = "curl",
      pollable = I(list(processx::curl_fds(private$curl_fdset))),
      type = "curl",
      ready = "silent")
    pollables <- rbind(pollables, curl_pollables)
  }

  ## Processes
  proc <- types %in% c("process", "r-process")
  if (sum(proc)) {
    conns <- unlist(lapply(
      private$tasks[proc], function(t) t$data$conns),
      recursive = FALSE)
    proc_pollables <- data.frame(
      stringsAsFactors = FALSE,
      id = names(private$tasks)[proc],
      pollable = I(conns),
      type = types[proc],
      ready = rep("silent", sum(proc)))
    pollables <- rbind(pollables, proc_pollables)
  }

  ## Pool
  px_pool <- if (!is.null(async_env$worker_pool)) {
    async_env$worker_pool$get_poll_connections()
  }
  if (length(px_pool)) {
    pool_pollables <- data.frame(
      stringsAsFactors = FALSE,
      id = names(px_pool),
      pollable = I(px_pool),
      type = rep("pool", length(px_pool)),
      ready = rep("silent", length(px_pool)))
    pollables <- rbind(pollables, pool_pollables)
  }

  if (!is.null(private$curl_timer) && private$curl_timer <= private$time) {
    multi_run(timeout = 0L, poll = TRUE, pool = private$pool)
    private$curl_timer <- NULL
  }

  if (nrow(pollables)) {

    ## OK, ready to poll
    pollables$ready <- unlist(processx::poll(pollables$pollable, timeout))

    ## Any HTTP?
    if (private$curl_poll &&
        pollables$ready[match("curl", pollables$type)] == "event") {
      multi_run(timeout = 0L, poll = TRUE, pool = private$pool)
    }

    ## Any processes
    proc_ready <- pollables$type %in% c("process", "r-process") &
      pollables$ready == "ready"
    for (id in pollables$id[proc_ready]) {
      p <- private$tasks[[id]]
      private$tasks[[id]] <- NULL
      ## TODO: this should be async
      p$data$process$wait(1000)
      p$data$process$kill()
      res <- list(
        status = p$data$process$get_exit_status(),
        stdout = read_all(p$data$stdout, p$data$encoding),
        stderr = read_all(p$data$stderr, p$data$encoding),
        timeout = FALSE
      )

      if (p$type == "r-process") {
        res$result = p$data$process$get_result()
      }

      unlink(c(p$data$stdout, p$data$stderr))

      if (p$data$error_on_status && res$status != 0) {
        err <- make_error("process exited with non-zero status")
        err$data <- res
        res <- NULL
      } else {
        err <- NULL
      }
      p$callback(err, res)
    }

    ## Worker pool
    pool_ready <- pollables$type == "pool" & pollables$ready == "ready"
    if (sum(pool_ready)) {
      pool <- async_env$worker_pool
      done <- pool$notify_event(as.integer(pollables$id[pool_ready]),
                                event_loop = private$id)
      mine <- intersect(done, names(private$tasks))
      for (tid in mine) {
        task <- private$tasks[[tid]]
        private$tasks[[tid]] <- NULL
        res <- pool$get_result(tid)
        err <- res$error
        res <- res[c("result", "stdout", "stderr")]
        task$callback(err, res)
      }
    }

  } else if (length(private$timers) || !is.null(private$curl_timer)) {
    Sys.sleep(timeout / 1000)
  }
}

#' @importFrom uuid UUIDgenerate

el__create_task <- function(self, private, callback, data, ..., id, type) {
  id <- id %||% UUIDgenerate()
  private$tasks[[id]] <- list(
    type = type,
    id = id,
    callback = callback,
    data = data,
    error = NULL,
    result = NULL
  )
  id
}

#' @importFrom curl new_pool

el__ensure_pool <- function(self, private, ...) {
  if (is.null(private$pool)) private$pool <- new_pool(...)
}

el__get_poll_timeout <- function(self, private) {
  t <- if (length(private$next_ticks)) {
    ## TODO: can this happen at all? Probably not, but it does not hurt...
    0 # nocov
  } else {
    max(0, min(Inf, private$timers - private$time))
  }

  if (!is.null(private$curl_timer)) {
    t <- min(t, private$curl_timer - private$time)
  }

  t <- max(t, 0)

  if (is.finite(t)) as.integer(t * 1000) else -1L
}

el__run_timers <- function(self, private) {

  expired <- names(private$timers)[private$timers <= private$time]
  expired <- expired[order(private$timers[expired])]
  for (id in expired) {
    task <- private$tasks[[id]]
    if (private$tasks[[id]]$data$rep) {
      ## If it is repeated, then re-init
      private$timers[id] <-
        private$time + as.difftime(task$data$delay, units = "secs")
    } else {
      ## Otherwise remove
      private$tasks[[id]] <- NULL
      private$timers <- private$timers[setdiff(names(private$timers), id)]
    }
    call_with_callback(task$data$func, task$callback)
  }
}

el__is_alive <- function(self, private) {
  length(private$tasks) > 0 ||
    length(private$timers) > 0 ||
    length(private$next_ticks) > 0
}

el__update_time <- function(self, private) {
  private$time <- Sys.time()
}

#' @importFrom curl multi_fdset

el__update_curl_data <- function(self, private) {
  private$curl_fdset <- multi_fdset(private$pool)
  num_fds <- length(unique(unlist(private$curl_fdset[1:3])))
  private$curl_poll <- num_fds > 0
  private$curl_timer <- if ((t <- private$curl_fdset$timeout) != -1) {
    private$time + as.difftime(t / 1000.0, units = "secs")
  }
}

#' @importFrom R6 R6Class

event_emitter <- R6Class(
  "event_emitter",
  public = list(
    initialize = function(async = TRUE)
      ee_init(self, private, async),

    listen_on = function(event, callback)
      ee_listen_on(self, private, event, callback),

    listen_off = function(event, callback)
      ee_listen_off(self, private, event, callback),

    listen_once = function(event, callback)
      ee_listen_once(self, private, event, callback),

    emit = function(event, ...)
      ee_emit(self, private, event, ...),

    get_event_names = function()
      ee_get_event_names(self, private),

    get_listener_count = function(event)
      ee_get_listener_count(self, private, event),

    remove_all_listeners = function(event)
      ee_remove_all_listeners(self, private, event)
  ),

  private = list(
    lsts = NULL,
    async = NULL,

    cleanup_events = function()
      ee__cleanup_events(self, private),
    error_callback = function(err, res)
      ee__error_callback(self, private, err, res)
  )
)

ee_init <- function(self, private, async) {
  assert_that(is_flag(async))
  private$lsts <- structure(list(), names = character())
  private$async <- async
  invisible(self)
}

ee_listen_on <- function(self, private, event, callback) {
  assert_that(is_string(event), is.function(callback))
  private$lsts[[event]] <-
    c(private$lsts[[event]], list(list(cb = callback, once = FALSE)))
  invisible(self)
}

ee_listen_off <- function(self, private, event, callback) {
  assert_that(is_string(event), is.function(callback))
  for (idx in seq_along(private$lsts[[event]])) {
    if (identical(private$lsts[[event]][[idx]]$cb, callback)) {
      private$lsts[[event]] <- private$lsts[[event]][-idx]
      break
    }
  }
  invisible(self)
}

ee_listen_once <- function(self, private, event, callback) {
  assert_that(is_string(event), is.function(callback))
  private$lsts[[event]] <-
    c(private$lsts[[event]], list(list(cb = callback, once = TRUE)))
  invisible(self)
}

ee_emit <- function(self, private, event, ...) {
  assert_that(is_string(event))
  list(...)
  tocall <- private$lsts[[event]]
  once <- vlapply(tocall, "[[", "once")
  if (any(once)) private$lsts[[event]] <- tocall[!once]

  ## a for loop is not good here, because it does not create
  ## a closure for lst
  lapply(tocall, function(lst) {
    lst
    if (private$async) {
      get_default_event_loop()$add_next_tick(
        function() lst$cb(...),
        private$error_callback,
        data = list(error_info = list(event = event)))

    } else {
      call_with_callback(
        function() lst$cb(...),
        private$error_callback,
        info = list(event = event))
    }
  })

  invisible(self)
}

ee_get_event_names <- function(self, private) {
  private$cleanup_events()
  names(private$lsts)
}

ee_get_listener_count <- function(self, private, event) {
  assert_that(is_string(event))
  length(private$lsts[[event]])
}

ee_remove_all_listeners <- function(self, private, event) {
  assert_that(is_string(event))
  private$lsts[[event]] <- NULL
  invisible(self)
}

ee__cleanup_events <- function(self, private) {
  len <- viapply(private$lsts, length)
  private$lsts <- private$lsts[len > 0]
}

ee__error_callback <- function(self, private, err, res) {
  if (is.null(err)) return()
  tocall <- private$lsts[["error"]]
  once <- vlapply(tocall, "[[", "once")
  if (any(once)) private$lsts[["error"]] <- tocall[!once]

  if (length(tocall)) {
    for (lst in tocall) lst$cb(err)
  } else {
    stop(err)
  }
}

async_every <- function(.x, .p, ...) {
  defs <- lapply(.x, async(.p), ...)
  nx <- length(defs)
  done <- FALSE

  deferred$new(
    type = "async_every", call = sys.call(),
    parents = defs,
    action = function(resolve) if (nx == 0) resolve(TRUE),
    parent_resolve = function(value, resolve) {
      if (!done && !isTRUE(value)) {
        done <<- TRUE
        resolve(FALSE)
      } else if (!done) {
        nx <<- nx - 1L
        if (nx == 0) resolve(TRUE)
      }
    }
  )
}

async_every <- mark_as_async(async_every)

async_filter <- function(.x, .p, ...) {
  when_all(.list = lapply(.x, async(.p), ...))$
    then(function(res) .x[vlapply(res, isTRUE)])
}

async_filter <- mark_as_async(async_filter)

async_reject <- function(.x, .p, ...) {
  when_all(.list = lapply(.x, async(.p), ...))$
    then(function(res) .x[! vlapply(res, isTRUE)])
}

async_reject <- mark_as_async(async_reject)

#' @importFrom curl new_handle handle_setheaders

http_get <- function(url, headers = character(), file = NULL,
                     options = list(), on_progress = NULL) {

  url; headers; file; options; on_progress
  options <- get_default_curl_options(options)

  make_deferred_http(
    function() {
      assert_that(is_string(url))
      handle <- new_handle(url = url)
      handle_setheaders(handle, .list = headers)

      if (!is.null(on_progress)) {
        options$noprogress <- FALSE
        fun <- options$progressfunction <- function(down, up) {
          on_progress(list(
            url = url,
            handle = handle,
            file = file,
            total = down[[1]],
            current = down[[2]]
          ))
          TRUE
        }
        ## This is a workaround for curl not PROTECT-ing the progress
        ## callback function
        reg.finalizer(handle, function(...) fun, onexit = TRUE)
      }

      handle_setopt(handle, .list = options)
      list(handle = handle, options = options)
    },
    file
  )
}

http_get <- mark_as_async(http_get)

#' @importFrom curl handle_setopt

http_head <- function(url, headers = character(), file = NULL,
                      options = list(), on_progress = NULL) {

  url; headers; file; options; on_progress
  options <- get_default_curl_options(options)

  make_deferred_http(
    function() {
      assert_that(is_string(url))
      handle <- new_handle(url = url)
      handle_setheaders(handle, .list = headers)
      handle_setopt(handle, customrequest = "HEAD", nobody = TRUE,
                    .list = options)
      list(handle = handle, options = options)
    },
    file
  )
}

http_head <- mark_as_async(http_head)

http_post <- function(url, data, headers = character(), file = NULL,
                      options = list(), on_progress = NULL) {

  url; data; headers; file; options; on_progress
  if (!is.raw(data)) data <- charToRaw(data)
  options <- get_default_curl_options(options)

  make_deferred_http(
    function() {
      assert_that(is_string(url))
      handle <- new_handle(url = url)
      handle_setheaders(handle, .list = headers)
      handle_setopt(handle, customrequest = "POST",
                    postfieldsize = length(data), postfields = data,
                    .list = options)
      list(handle = handle, options = options)
    },
    file
  )
}

http_post <- mark_as_async(http_post)

#' @importFrom utils modifyList

get_default_curl_options <- function(options) {
  getopt <- function(nm) {
    if (!is.null(v <- options[[nm]])) return(v)
    anm <- paste0("async_http_", nm)
    if (!is.null(v <- getOption(anm))) return(v)
    if (!is.na(v <- Sys.getenv(toupper(anm), NA_character_))) return (v)
  }
  modifyList(
    options,
    list(
      timeout = as.integer(getopt("timeout") %||% 0),
      connecttimeout = as.integer(getopt("connecttimeout") %||% 300),
      low_speed_time = as.integer(getopt("low_speed_time") %||% 0),
      low_speed_limit = as.integer(getopt("low_speed_limit") %||% 0)
    )
  )
}

make_deferred_http <- function(cb, file) {
  cb; file
  id <- NULL
  deferred$new(
    type = "http", call = sys.call(),
    action = function(resolve, progress) {
      resolve; progress
      ## This is a temporary hack until we have proper pollables
      ## Then the deferred will have a "work" callback, which will
      ## be able to throw.
      reject <- environment(resolve)$private$reject
      ho <- cb()
      id <<- get_default_event_loop()$add_http(
        ho$handle,
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        progress,
        file,
        data = ho$options)
    },
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    }
  )
}

http_stop_for_status <- function(resp) {
  if (!is.integer(resp$status_code)) stop("Not an HTTP response")
  if (resp$status_code < 400) return(invisible(resp))
  stop(http_error(resp))
}

http_error <- function(resp, call = sys.call(-1)) {
  status <- resp$status_code
  reason <- http_status(status)$reason
  message <- sprintf("%s (HTTP %d).", reason, status)
  status_type <- (status %/% 100) * 100
  if (length(resp[["content"]]) == 0 && !is.null(resp$file) &&
              file.exists(resp$file)) {
    tryCatch({
      n <- file.info(resp$file, extra_cols = FALSE)$size
      resp$content <- readBin(resp$file, what = raw(), n = n)
    }, error = identity)
  }
  http_class <- paste0("async_http_", unique(c(status, status_type, "error")))
  structure(
    list(message = message, call = call, response = resp),
    class = c(http_class, "error", "condition")
  )
}

http_status <- function(status) {
  status_desc <- http_statuses[as.character(status)]
  if (is.na(status_desc)) {
    stop("Unknown http status code: ", status, call. = FALSE)
  }

  status_types <- c("Information", "Success", "Redirection", "Client error",
    "Server error")
  status_type <- status_types[[status %/% 100]]

  # create the final information message
  message <- paste(status_type, ": (", status, ") ", status_desc, sep = "")

  list(
    category = status_type,
    reason = status_desc,
    message = message
  )
}

http_statuses <- c(
  "100" = "Continue",
  "101" = "Switching Protocols",
  "102" = "Processing (WebDAV; RFC 2518)",
  "200" = "OK",
  "201" = "Created",
  "202" = "Accepted",
  "203" = "Non-Authoritative Information",
  "204" = "No Content",
  "205" = "Reset Content",
  "206" = "Partial Content",
  "207" = "Multi-Status (WebDAV; RFC 4918)",
  "208" = "Already Reported (WebDAV; RFC 5842)",
  "226" = "IM Used (RFC 3229)",
  "300" = "Multiple Choices",
  "301" = "Moved Permanently",
  "302" = "Found",
  "303" = "See Other",
  "304" = "Not Modified",
  "305" = "Use Proxy",
  "306" = "Switch Proxy",
  "307" = "Temporary Redirect",
  "308" = "Permanent Redirect (experimental Internet-Draft)",
  "400" = "Bad Request",
  "401" = "Unauthorized",
  "402" = "Payment Required",
  "403" = "Forbidden",
  "404" = "Not Found",
  "405" = "Method Not Allowed",
  "406" = "Not Acceptable",
  "407" = "Proxy Authentication Required",
  "408" = "Request Timeout",
  "409" = "Conflict",
  "410" = "Gone",
  "411" = "Length Required",
  "412" = "Precondition Failed",
  "413" = "Request Entity Too Large",
  "414" = "Request-URI Too Long",
  "415" = "Unsupported Media Type",
  "416" = "Requested Range Not Satisfiable",
  "417" = "Expectation Failed",
  "418" = "I'm a teapot (RFC 2324)",
  "420" = "Enhance Your Calm (Twitter)",
  "422" = "Unprocessable Entity (WebDAV; RFC 4918)",
  "423" = "Locked (WebDAV; RFC 4918)",
  "424" = "Failed Dependency (WebDAV; RFC 4918)",
  "424" = "Method Failure (WebDAV)",
  "425" = "Unordered Collection (Internet draft)",
  "426" = "Upgrade Required (RFC 2817)",
  "428" = "Precondition Required (RFC 6585)",
  "429" = "Too Many Requests (RFC 6585)",
  "431" = "Request Header Fields Too Large (RFC 6585)",
  "444" = "No Response (Nginx)",
  "449" = "Retry With (Microsoft)",
  "450" = "Blocked by Windows Parental Controls (Microsoft)",
  "451" = "Unavailable For Legal Reasons (Internet draft)",
  "499" = "Client Closed Request (Nginx)",
  "500" = "Internal Server Error",
  "501" = "Not Implemented",
  "502" = "Bad Gateway",
  "503" = "Service Unavailable",
  "504" = "Gateway Timeout",
  "505" = "HTTP Version Not Supported",
  "506" = "Variant Also Negotiates (RFC 2295)",
  "507" = "Insufficient Storage (WebDAV; RFC 4918)",
  "508" = "Loop Detected (WebDAV; RFC 5842)",
  "509" = "Bandwidth Limit Exceeded (Apache bw/limited extension)",
  "510" = "Not Extended (RFC 2774)",
  "511" = "Network Authentication Required (RFC 6585)",
  "598" = "Network read timeout error (Unknown)",
  "599" = "Network connect timeout error (Unknown)"
)

async_map <- function(.x, .f, ..., .args = list(), .limit = Inf) {
  if (.limit < length(.x))  {
    async_map_limit(.x, .f, ..., .args = .args, .limit = .limit)
  } else {
    defs <- do.call(lapply, c(list(.x, async(.f), ...), .args))
    when_all(.list = defs)
  }
}

async_map <- mark_as_async(async_map)

async_map_limit <- function(.x, .f, ..., .args = list(), .limit = Inf) {
  len <- length(.x)
  nx <- len
  .f <- async(.f)
  args <- c(list(...), .args)

  nextone <- .limit + 1L
  firsts <- lapply_args(.x[seq_len(.limit)], .f, .args = args)
  ids <- viapply(firsts, function(x) x$get_id())

  result <- structure(
    vector(mode = "list", length = len),
    names = names(.x)
  )

  self <- deferred$new(
    type = "async_map (limit)", call = sys.call(),
    parents = firsts,
    action = function(resolve) if (nx == 0) resolve(result),
    parent_resolve = function(value, resolve, id) {
      nx <<- nx - 1L
      result[[match(id, ids)]] <<- value
      if (nx == 0) {
        resolve(result)
      } else if (nextone <= len) {
        dx <- do.call(".f", c(list(.x[[nextone]]), args))
        ids <<- c(ids, dx$get_id())
        dx$then(self)
        nextone <<- nextone + 1L
      }
    }
  )

  self
}

#' @import rlang
NULL

#' @importFrom processx process

run_process <- function(command = NULL, args = character(),
  error_on_status = TRUE, wd = NULL, env = NULL,
  windows_verbatim_args = FALSE, windows_hide_window = FALSE,
  encoding = "", ...) {

  command; args; error_on_status; wd; env; windows_verbatim_args;
  windows_hide_window; encoding; list(...)

  id <- NULL

  deferred$new(
    type = "process", call = sys.call(),
    action = function(resolve) {
      resolve
      reject <- environment(resolve)$private$reject
      stdout <- tempfile()
      stderr <- tempfile()
      px <- process$new(command, args = args,
        stdout = stdout, stderr = stderr, poll_connection = TRUE,
        env = env, cleanup = TRUE, wd = wd, encoding = encoding, ...)
      pipe <- px$get_poll_connection()
      id <<- get_default_event_loop()$add_process(
        list(pipe),
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        list(process = px, stdout = stdout, stderr = stderr,
             error_on_status = error_on_status, encoding = encoding))
    },
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    }
  )
}

run_process <- mark_as_async(run_process)

#' @importFrom callr r_process_options r_process rcmd_safe_env

run_r_process <- function(func, args = list(), libpath = .libPaths(),
  repos = c(getOption("repos"), c(CRAN = "https://cloud.r-project.org")),
  cmdargs = c("--no-site-file", "-s", "--no-save", "--no-restore"),
  system_profile = FALSE, user_profile = FALSE, env = rcmd_safe_env()) {

  func; args; libpath; repos; cmdargs; system_profile; user_profile; env

  id <- NULL

  deferred$new(
    type = "r-process", call = sys.calls(),
    action = function(resolve) {
      resolve
      reject <- environment(resolve)$private$reject
      stdout <- tempfile()
      stderr <- tempfile()
      opts <- r_process_options(
        func = func, args = args, libpath = libpath, repos = repos,
        cmdargs = cmdargs, system_profile = system_profile,
        user_profile = user_profile, env = env, stdout = stdout,
        stderr = stderr)
      rx <- r_process$new(opts)
      pipe <- rx$get_poll_connection()
      id <<- get_default_event_loop()$add_r_process(
        list(pipe),
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        list(process = rx, stdout = stdout, stderr = stderr,
             error_on_status = TRUE, encoding = ""))
    },
    on_cancel = function(reason) {
      if (!is.null(id)) get_default_event_loop()$cancel(id)
    }
  )
}

run_r_process <- mark_as_async(run_r_process)

async_reflect <- function(task) {
  task <- async(task)
  function(...) {
    task(...)$
      then(function(value)  list(error = NULL, result = value))$
      catch(error = function(reason) list(error = reason, result = NULL))
  }
}

async_reflect <- mark_as_async(async_reflect)

async_replicate <- function(n, task, ...,  .limit = Inf) {
  assert_that(
    is_count(n),
    .limit == Inf || is_count(.limit), .limit >= 1L)

  force(list(...))
  task <- async(task)

  if (n == 0) {
    async_constant(list())
  } else if (n <= .limit) {
    async_replicate_nolimit(n, task, ...)
  } else {
    async_replicate_limit(n, task, ..., .limit = .limit)
  }
}

async_replicate_nolimit <- function(n, task, ...) {
  defs <- lapply(seq_len(n), function(i) task(...))
  when_all(.list = defs)
}

async_replicate_limit  <- function(n, task, ..., .limit = .limit) {
  n; .limit

  defs <- ids <- nextone <- result <- NULL

  self <- deferred$new(
    type = "async_replicate", call = sys.call(),
    action = function(resolve) {
      defs <<- lapply(seq_len(n), function(i) task(...))
      ids <<- viapply(defs, function(x) x$get_id())
      result <<- vector(n, mode = "list")
      for (i in 1:.limit) defs[[i]]$then(self)
      nextone <<- .limit + 1L
    },
    parent_resolve = function(value, resolve, id) {
      result[[match(id, ids)]] <<- value
      if (nextone > n) {
        resolve(result)
      } else {
        defs[[nextone]]$then(self)
        nextone <<- nextone + 1L
      }
    }
  )

  self
}

async_retry <- function(task, times, ...) {
  task <- async(task)
  times <- times
  force(list(...))

  self <- deferred$new(
    type = "retry", call = sys.call(),
    parents = list(task(...)),
    parent_reject = function(value, resolve) {
      times <<- times - 1L
      if (times > 0) {
        task(...)$then(self)
      } else {
        stop(value)
      }
    }
  )
}

async_retry <- mark_as_async(async_retry)

async_retryable <- function(task, times) {
  task <- async(task)
  force(times)
  function(...) {
    async_retry(task, times, ...)
  }
}

async_sequence <- function(..., .list = NULL) {
  funcs <- c(list(...), .list)
  if (length(funcs) == 0) stop("Function list empty in `async_sequence`")

  function(...) {
    dx <- async(funcs[[1]])(...)
    for (i in seq_along(funcs)[-1]) dx <- dx$then(funcs[[i]])
    dx
  }
}

async_sequence <- mark_as_async(async_sequence)

async_some <- function(.x, .p, ...) {
  defs <- lapply(.x, async(.p), ...)
  nx <- length(defs)
  done <- FALSE

  deferred$new(
    type = "async_some", call = sys.call(),
    parents = defs,
    action = function(resolve) if (nx == 0) resolve(FALSE),
    parent_resolve = function(value, resolve) {
      if (!done && isTRUE(value)) {
        done <<- TRUE
        resolve(TRUE)
      } else if (!done) {
        nx <<- nx - 1L
        if (nx == 0) resolve(FALSE)
      }
    }
  )
}

async_some <- mark_as_async(async_some)

synchronise <- function(expr) {
  new_el <- push_event_loop()
  on.exit({ new_el$cancel_all(); pop_event_loop() }, add = TRUE)

  ## Mark this frame as a synchronization point, for debugging
  `__async_synchronise_frame__` <- TRUE

  res <- expr

  if (!is_deferred(res)) return(res)

  priv <- get_private(res)
  if (! identical(priv$event_loop, new_el)) {
    err <- make_error(
      "Cannot create deferred chain across synchronization barrier",
      class = "async_synchronization_barrier_error")
    stop(err)
  }

  priv$null()
  priv$run_action()

  if (isTRUE(getOption("async_debug"))) start_browser()
  while (priv$state == "pending") new_el$run("once")

  if (priv$state == "fulfilled") priv$value else stop(priv$value)
}

start_browser <- function() {
  async_debug_shortcuts()
  on.exit(async_debug_remove_shortcuts(), add = TRUE)
  cat("This is a standard `browser()` call, but you can also use the\n")
  cat("following extra commands:\n")
  cat("- .an / async_next(): next event loop iteration.\n")
  cat("- .as / async_step(): next event loop, debug next action or parent callback.\n")
  cat("- .asb / async_step_back(): stop debugging of callbacks.\n")
  cat("- .al / async_list(): deferred values in the current async phase.\n")
  cat("- .at / async_tree(): DAG of the deferred values.\n")
  cat("- .aw / async_where(): print call stack, mark async callback.\n")
  cat("- async_wait_for(): run until deferred is resolved.\n")
  cat("- async_debug(): debug action and/or parent callbacks of deferred.\n")
  cat("\n")
  browser(skipCalls = 1)
}

run_event_loop <- function(expr) {
  new_el <- push_event_loop()
  on.exit({ new_el$cancel_all(); pop_event_loop() }, add = TRUE)

  ## Mark this frame as a synchronization point, for debugging
  `__async_synchronise_frame__` <- TRUE

  expr
  new_el$run()

  invisible()
}

distill_error <- function(err) {
  if (is.null(err$aframe)) return(err)
  err$aframe <- list(
    frame = err$aframe$frame,
    deferred = err$aframe$data[[1]],
    type = err$aframe$data[[2]],
    call = get_private(err$aframe$data[[3]])$mycall
  )
  err
}

# nocov start

print.async_rejected <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}

# nocov end

format.async_rejected <- function(x, ...) {
  x <- distill_error(x)
  src <- get_source_position(x$aframe$call)
  paste0(
    "<async error: ", x$message, "\n",
    " in *", x$aframe$type, "* callback of `",
    expr_name(x$aframe$call %||% ""),
    "` at ", src$filename, ":", src$position, ">"
  )
}

summary.async_rejected <- function(object, ...) {
  x <- distill_error(object)
  fmt_out <- format(object, ...)
  stack <- async_where(calls = x$calls, parents = x$parents,
                       frm = list(x$aframe))
  stack_out <- format(stack)
  structure(
    paste0(fmt_out, "\n\n", stack_out),
    class = "async_rejected_summary")
}

# nocov start

print.async_rejected_summary <- function(x, ...) {
  cat(x)
  invisible(x)
}

# nocov end

async_timeout <- function(task, timeout, ...) {
  task <- async(task)
  force(timeout)
  done <- FALSE

  deferred$new(
    type = "timeout", call = sys.call(),
    parents = list(d1 <- task(...), d2 <- delay(timeout)),
    parent_resolve = function(value, resolve, id) {
      if (!done) {
        done <<- TRUE
        if (id == d1$get_id()) {
          resolve(value)
        } else {
          stop("Timed out")
        }
      }
    }
  )
}

async_timeout <- mark_as_async(async_timeout)

#' @importFrom R6 R6Class

async_timer <- R6Class(
  "async_timer",
  inherit = event_emitter,
  public = list(
    initialize = function(delay, callback)
      async_timer_init(self, private, super, delay, callback),
    cancel = function()
      async_timer_cancel(self, private)
  ),

  private = list(
    id = NULL
  )
)

async_timer_init <- function(self, private, super, delay, callback) {
  assert_that(
    is_time_interval(delay),
    is.function(callback) && length(formals(callback)) == 0)

  ## event emitter
  super$initialize()

  private$id <- get_default_event_loop()$add_delayed(
    delay,
    function() self$emit("timeout"),
    function(err, res) {
      if (!is.null(err)) self$emit("error", err)              # nocov
    },
    rep = TRUE)

  self$listen_on("timeout", callback)

  invisible(self)
}

async_timer_cancel  <- function(self, private) {
  self; private
  self$remove_all_listeners("timeout")
  get_default_event_loop()$cancel(private$id)
  invisible(self)
}

async_try_each <- function(..., .list = list()) {
  defs <- c(list(...), .list)
  wh <- nx <- NULL
  errors <- list()

  self <- deferred$new(
    type = "async_try_each", call = sys.call(),
    action = function(resolve) {
      nx <<- length(defs)
      if (nx == 0) resolve(NULL)
      wh <<- 1L
      defs[[wh]]$then(self)
    },
    parent_resolve = function(value, resolve, id) {
      resolve(value)
    },
    parent_reject = function(value, resolve, id) {
      errors <<- c(errors, list(value))
      if (wh == nx) {
        err <- structure(
          list(errors = errors, message = "async_try_each failed"),
          class = c("async_rejected", "error", "condition"))
        stop(err)
      } else {
        wh <<- wh + 1
        defs[[wh]]$then(self)
      }
    }
  )

  self
}

async_try_each <- mark_as_async(async_try_each)

async_until <- function(test, task, ...) {
  force(test)
  task <- async(task)

  self <- deferred$new(
    type = "async_until", call = sys.call(),
    parents = list(task(...)),
    parent_resolve = function(value, resolve) {
      if (test()) {
        resolve(value)
      } else {
        task(...)$then(self)
      }
    }
  )

  self
}

async_until <- mark_as_async(async_until)

`%||%` <- function(l, r) if (is.null(l)) r else l

vlapply <- function(X, FUN, ..., FUN.VALUE = logical(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

viapply <- function(X, FUN, ..., FUN.VALUE = integer(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

vcapply <- function(X, FUN, ..., FUN.VALUE = character(1)) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ...)
}

make_error <- function(message, class = "simpleError", call = NULL) {
  class <- c(class, "error", "condition")
  structure(
    list(message = as.character(message), call = call),
    class = class
  )
}

num_args <- function(fun) {
  length(formals(fun))
}

get_private <- function(x) {
  x$.__enclos_env__$private
}

call_with_callback <- function(func, callback, info = NULL) {
  recerror <- NULL
  result <- NULL
  tryCatch(
    withCallingHandlers(
      result <- func(),
      error = function(e) {
        recerror <<- e
        recerror$aframe <<- recerror$aframe %||% find_async_data_frame()
        recerror$calls <<- recerror$calls %||% sys.calls()
        recerror$parents <<- recerror$parents %||% sys.parents()
        recerror[names(info)] <<- info
        handler <- getOption("async.error")
        if (is.function(handler)) handler()
      }
    ),
    error = identity
  )
  callback(recerror, result)
}

get_id <- local({
  id <- 0L
  function() {
    id <<- id + 1L
    id
  }
})

new_event_loop_id <- local({
  id <- 0L
  function() {
    id <<- id + 1L
    id
  }
})

lapply_args <- function(X, FUN, ..., .args = list()) {
  do.call("lapply", c(list(X = X, FUN = FUN), list(...), .args))
}

drop_nulls <- function(x) {
  x[!vlapply(x, is.null)]
}

#' @importFrom utils getSrcDirectory getSrcFilename getSrcLocation

get_source_position <- function(call) {
  list(
    filename = file.path(
      c(getSrcDirectory(call), "?")[1],
      c(getSrcFilename(call), "?")[1]),
    position = paste0(
      getSrcLocation(call, "line", TRUE) %||% "?", ":",
      getSrcLocation(call, "column", TRUE) %||% "?")
  )
}

file_size <- function(...) {
  file.info(..., extra_cols = FALSE)$size
}

read_all <- function(filename, encoding) {
  r <- readBin(filename, what = raw(0), n = file_size(filename))
  s <- rawToChar(r)
  Encoding(s) <- encoding
  s
}

crash <- function () {
  get("attach")(structure(list(), class = "UserDefinedDatabase"))
}

str_trim <- function(x) {
  sub("\\s+$", "", sub("^\\s+", "", x))
}

when_all <- function(..., .list = list()) {

  defs <- c(list(...), .list)
  isdef <- vlapply(defs, is_deferred)
  nx <- sum(isdef)

  deferred$new(
    type = "when_all", call = sys.call(),
    parents = defs[isdef],
    action = function(resolve) if (nx == 0) resolve(defs),
    parent_resolve = function(value, resolve) {
      nx <<- nx - 1L
      if (nx == 0L) resolve(lapply(defs, get_value_x))
    }
  )
}

when_all <- mark_as_async(when_all)

get_value_x <- function(x) {
  if (is_deferred(x)) get_private(x)$value else x
}

when_some <- function(count, ..., .list = list()) {
  force(count)
  defs <- c(list(...), .list)
  num_defs <- length(defs)
  num_failed <- 0L
  ifdef <- vlapply(defs, is_deferred)
  resolved <- defs[!ifdef]
  errors <- list()

  cancel_all <- function() lapply(defs[ifdef], function(x) x$cancel())

  deferred$new(
    type = "when_some", call = sys.call(),
    parents = defs[ifdef],
    action = function(resolve) {
      if (num_defs < count) {
        stop("Cannot resolve enough deferred values")
      } else if (length(resolved) >= count) {
        resolve(resolved[seq_len(count)])
      }
    },
    parent_resolve = function(value, resolve) {
      resolved <<- c(resolved, list(value))
      if (length(resolved) == count) {
        resolve(resolved)
      }
    },
    parent_reject = function(value, resolve) {
      num_failed <<- num_failed + 1L
      errors <<- c(errors, list(value))
      if (num_failed + count == num_defs + 1L) {
        err <- structure(
          list(errors = errors, message = "when_some / when_any failed"),
          class = c("async_rejected", "error", "condition"))
        stop(err)
      }
    }
  )
}

when_some <- mark_as_async(when_some)

when_any <- function(..., .list = list()) {
  when_some(1, ..., .list = .list)$then(function(x) x[[1]])
}

when_any <- mark_as_async(when_any)

async_whilst <- function(test, task, ...) {
  force(test)
  task <- async(task)

  self <- deferred$new(
    type = "async_whilst", call = sys.call(),
    action = function(resolve)  {
      if (!test()) {
        resolve(NULL)
      } else {
        task(...)$then(self)
      }
    },
    parent_resolve = function(value, resolve) {
      if  (!test()) {
        resolve(value)
      } else {
        task(...)$then(self)
      }
    }
  )

  self
}

async_whilst <- mark_as_async(async_whilst)

#' @importFrom R6 R6Class

worker_pool <- R6Class(
  public = list(
    initialize = function()
      wp_init(self, private),
    add_task = function(func, args, id, event_loop)
      wp_add_task(self, private, func, args, id, event_loop),
    get_fds = function()
      wp_get_fds(self, private),
    get_pids = function()
      wp_get_pids(self, private),
    get_poll_connections = function()
      wp_get_poll_connections(self, private),
    notify_event = function(pids, event_loop)
      wp_notify_event(self, private, pids, event_loop),
    start_workers = function()
      wp_start_workers(self, private),
    kill_workers = function()
      wp_kill_workers(self, private),
    cancel_task = function(id)
      wp_cancel_task(self, private, id),
    cancel_all_tasks = function()
      wp_cancel_all_tasks(self, private),
    get_result = function(id)
      wp_get_result(self, private, id),
    list_workers = function()
      wp_list_workers(self, private),
    list_tasks = function(event_loop = NULL, status = NULL)
      wp_list_tasks(self, private, event_loop, status),
    finalize = function() self$kill_workers()
  ),

  private = list(
    workers = list(),
    tasks = list(),

    try_start = function()
      wp__try_start(self, private),
    interrupt_worker = function(pid)
      wp__interrupt_worker(self, private, pid)
  )
)

wp_init <- function(self, private) {
  self$start_workers()
  invisible(self)
}

#' @importFrom callr r_session
#' @importFrom processx conn_get_fileno

wp_start_workers <- function(self, private) {
  num <- worker_pool_size()

  ## See if we need to start more
  if (NROW(private$workers) >= num) return(invisible())

  ## Yeah, start some more
  to_start <- num - NROW(private$workers)
  sess <- lapply(1:to_start, function(x) r_session$new(wait = FALSE))
  fd <- viapply(sess, function(x) conn_get_fileno(x$get_poll_connection()))
  new_workers <- data.frame(
    stringsAsFactors = FALSE,
    session = I(sess),
    task = NA_character_,
    pid = viapply(sess, function(x) x$get_pid()),
    fd = fd,
    event_loop = NA_integer_
  )

  private$workers <- rbind(private$workers, new_workers)
  invisible()
}

wp_add_task <- function(self, private, func, args, id, event_loop) {
  private$tasks <- rbind(
    private$tasks,
    data.frame(
      stringsAsFactors = FALSE,
      event_loop = event_loop, id = id, func = I(list(func)),
      args = I(list(args)), status = "waiting", result = I(list(NULL)))
  )

  private$try_start()
  invisible()
}

## We only need to poll the sessions that actually do something...

wp_get_fds <- function(self, private) {
  sts <- vcapply(private$workers$session, function(x) x$get_state())
  private$workers$fd[sts %in% c("starting", "busy")]
}

wp_get_pids <- function(self, private) {
  sts <- vcapply(private$workers$session, function(x) x$get_state())
  private$workers$pid[sts %in% c("starting", "busy")]
}

wp_get_poll_connections <- function(self, private) {
  sts <- vcapply(private$workers$session, function(x) x$get_state())
  busy <- sts %in% c("starting", "busy")
  structure(
    lapply(private$workers$session[busy],
           function(x) x$get_poll_connection()),
    names = private$workers$pid[busy])
}

wp_notify_event <- function(self, private, pids, event_loop) {
  done <- NULL
  dead <- integer()
  which <- match(pids, private$workers$pid)
  for (w in which) {
    msg <- private$workers$session[[w]]$read()
    if (is.null(msg)) next
    if (msg$code == 200 || (msg$code >= 500 && msg$code < 600)) {
      if (msg$code >= 500 && msg$code < 600) dead <- c(dead, w)
      wt <- match(private$workers$task[[w]], private$tasks$id)
      if (is.na(wt)) stop("Internal error, no such task")
      private$tasks$result[[wt]] <- msg
      private$tasks$status[[wt]] <- "done"
      private$workers$task[[w]] <- NA_character_
      done <- c(done, private$tasks$id[[wt]])
    }
  }
  if (length(dead)) {
    private$workers <- private$workers[-dead,]
    self$start_workers()
  }

  private$try_start()

  done
}

worker_pool_size <- function() {
  getOption("async.worker_pool_size") %||%
    as.integer(Sys.getenv("ASYNC_WORKER_POOL_SIZE", 4))
}

wp_kill_workers <- function(self, private) {
  lapply(private$workers$session, function(x) x$kill())
  private$workers <- NULL
  invisible()
}

wp_cancel_task <- function(self, private, id) {
  wt <- match(id, private$tasks$id)
  if (is.na(wt)) stop("Unknown task")

  if (private$tasks$status[[wt]] == "running") {
    wk <- match(id, private$workers$task)
    if (!is.na(wk)) private$interrupt_worker(private$workers$pid[wk])
  }
  private$tasks <- private$tasks[-wt, ]
  invisible()
}

wp_cancel_all_tasks <- function(self, private) {
  stop("`cancel_all_tasks` method is not implemented yet")
}

wp_get_result <- function(self, private, id) {
  wt <- match(id, private$tasks$id)
  if (is.na(wt)) stop("Unknown task")

  if (private$tasks$status[[wt]] != "done") stop("Task not done yet")
  result <- private$tasks$result[[wt]]
  private$tasks <- private$tasks[-wt, ]
  result
}

wp_list_workers <- function(self, private) {
  private$workers[, setdiff(colnames(private$workers), "session")]
}

wp_list_tasks <- function(self, private, event_loop, status) {
  dont_show <- c("func", "args", "result")
  ret <- private$tasks
  if (!is.null(event_loop)) ret <- ret[ret$event_loop %in% event_loop, ]
  if (!is.null(status)) ret <- ret[ret$status %in% status, ]
  ret[, setdiff(colnames(private$tasks), dont_show)]
}

## Internals -------------------------------------------------------------

#' @importFrom utils head

wp__try_start <- function(self, private) {
  sts <- vcapply(private$workers$session, function(x) x$get_state())
  if (all(sts != "idle")) return()
  can_work <- sts == "idle"

  can_run <- private$tasks$status == "waiting"
  num_start <- min(sum(can_work), sum(can_run))
  will_run <- head(which(can_run), num_start)
  will_work <- head(which(can_work), num_start)

  for (i in seq_along(will_run)) {
    wt <- will_run[[i]]
    ww <- will_work[[i]]
    func <- private$tasks$func[[wt]]
    args <- private$tasks$args[[wt]]
    private$workers$session[[ww]]$call(func, args)
    private$tasks$status[[wt]] <- "running"
    private$workers$task[[ww]] <- private$tasks$id[[wt]]
  }

  invisible()
}

wp__interrupt_worker <- function(self, private, pid) {
  ww <- match(pid, private$workers$pid)
  if (is.na(ww)) stop("Unknown task in interrupt_worker() method")

  kill <- FALSE
  sess <- private$workers$session[[ww]]
  int <- sess$interrupt()
  pr <- sess$poll_io(100)["process"]

  if (pr == "ready") {
    msg <- sess$read()
    if (! inherits(msg, "interrupt")) {
      tryCatch({
        sess$write_input("base::Sys.sleep(0)\n")
        sess$read_output()
        sess$read_error()
      }, error = function(e) kill <<- TRUE)
    }
    private$workers$task[[ww]] <- NA_character_
  } else {
    kill <- TRUE
  }

  if (kill) {
    sess$close()
    private$workers <- private$workers[-ww, ]
    ## Make sure that we have enough workers running
    self$start_workers()
  }

  invisible()
}
