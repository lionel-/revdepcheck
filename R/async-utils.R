
# {coro} operations for the {async} package

.__coro_async_ops__. <- coro::async_ops(
  package = "async",
  then = function(x, callback) {
    x$
      then(callback)$
      catch(error = function(err) callback(stop(err)))
  },
  as_promise = function(x) as_deferred(x)
)
as_deferred <- function(x) {
  if (async::is_deferred(x)) {
    x
  } else {
    async::async_constant(x)
  }
}

# Takes a processx object and returns a `deferred`
async_px <- function(px) {
  id <- NULL
  async::deferred$new(
    type = "process",
    call = sys.calls(),
    action = function(resolve) {
      resolve
      reject <- environment(resolve)$private$reject

      pipe <- px$get_poll_connection()

      # {async} currently needs actual output files
      stdout <- tempfile()
      stderr <- tempfile()
      file.create(stdout)
      file.create(stderr)

      id <<- async:::get_default_event_loop()$add_process(
        list(pipe),
        function(err, res) if (is.null(err)) resolve(res) else reject(err),
        list(
          process = px,
          stdout = stdout,
          stderr = stderr,
          error_on_status = TRUE,
          encoding = ""
        ))
    },
    on_cancel = function(reason) {
      if (!is.null(id)) async:::get_default_event_loop()$cancel(id)
    }
  )
}

`%<~%` <- function(lhs, rhs, env = caller_env()) {
  env_bind_lazy(env, !!substitute(lhs) := !!substitute(rhs), .eval_env = env)
}

on_load <- function(expr, env = topenv(caller_env())) {
  callback <- function() eval_bare(expr, env)

  hook <- env$.__rlang_hook__.
  env$.__rlang_hook__. <- list2(!!!hook, callback)
}

run_on_load <- function(env = topenv(caller_env())) {
  hook <- env$.__rlang_hook__.
  env_unbind(env, ".__rlang_hook__.")

  for (callback in hook) {
    callback()
  }

  NULL
}
