

open_browser <- function(client){
  er <- suppressMessages(suppressWarnings(try(client$getSession(), silent = T)))
  store <- T

  if(inherits(er,"try-error")){
    # only place for starting a session
    # this won't prevent situations where singular_pid_sid is false
    slock <- get_selenium_storm_session_lock()
    slocked <- slock$lock(wait = 10)
    if(slocked){
      res <- tryCatch(client$open(silent = TRUE), error = function(e) e)
      if (inherits(res, "error")) {
        message("Could not open browser.")
        message("Client error message:\n", res$message)
        message("Check server log for further details.")
        store <- F
      }
    }
  }

  # after open check
  er <- suppressMessages(suppressWarnings(try(client$getSession(), silent = T)))

  if(inherits(er,"try-error")){
    store <- F
  }

  if(store){
    st <- get_selenium_storm_storr()
    e <- environment(client$open)
    st$set(key = e$sessionid, value = Sys.getpid(), namespace = "sessions")
  }
}


get_active_sessions <- function(client, close_inactive = T){

  all_sessions <- client$getSessions()
  e <- environment(client$open)
  all_sessions_id <- all_sessions %>% lapply("[[","id")
  test_id <- function(id){
    qpath <- sprintf("%s/session/%s/url", e$serverURL, id)
    e$queryRD(qpath)
    !("message" %in% names(e$.self$value))
  }

  all_sessions_info <- all_sessions_id %>% lapply(test_id) %>% unlist()
  active_sessions <- all_sessions[all_sessions_info]
  if(close_inactive & length(all_sessions_info)){
    close_loc <- function(id){
      qpath <- sprintf("%s/session/%s", e$serverURL, id)
      e$queryRD(qpath, "DELETE")
    }
    all_sessions_id[!all_sessions_info] %>% lapply(close_loc)
  }
  return(active_sessions)
}

lock_to_sid <- function(sid){
  sidlock <- get_selenium_storm_session_lock(sid = sid)
  sidlock$lock(wait = 10)
}

attach_to_active_session <- function(client, session_id){
  aok <- F
  active_sessions <- get_active_sessions(get_control_client())
  active_sessions_ids <- active_sessions %>% purrr::map_chr("id")
  e <- environment(client$open)
  if(session_id %in% active_sessions_ids){
    sid_locked <- lock_to_sid()
    if(sid_locked){
      tar <- which(active_sessions_ids == session_id)
      active_session <- active_sessions[[tar]]
      e$sessionInfo <- active_session
      e$sessionInfo$id <- session_id
      e$sessionid <- session_id
      aok <- T
    }
  }
  aok
}
