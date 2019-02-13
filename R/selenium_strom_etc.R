
selenium_storm_init <- function(port, check, clean_start = F, singular_pid_sid = T,
                                Browser, headless, type = c("server", "client")){

  st <- get_selenium_storm_storr()
  type <- match.arg(type)

  if(type=="server"){

    if(clean_start){
      st$destroy()
      st <- get_selenium_storm_storr()
    }

    create_if_not_exists <- function(key, default, ns = "config", force = F){
      if(!st$exists(key, namespace = ns)){
        st$set(key, default, namespace = ns)
      }
      if(force){
        st$set(key, default, namespace = ns)
      }
      st$get(key, ns)
    }

    last_pid <- create_if_not_exists("last_pid", Sys.getpid())

    if(last_pid!=Sys.getpid()){
      if(is_pid_active(last_pid)){
        stop(paste0("selenium_storm is running already with pid:", last_pid))
      }else{
        st$set("last_pid", Sys.getpid(), "config")
      }
    }

    # clean locks folder
    unlink(get_selenium_storm_lock_path(""), recursive = T)

    create_if_not_exists("port", port, force = T)
    create_if_not_exists("check", check, force = T)
    create_if_not_exists("singular_pid_sid", singular_pid_sid, force = T)

    create_if_not_exists("num_cores", parallel::detectCores(), force = T)

    version <- create_if_not_exists("version", get_version(), force = T)
    if(version!=get_version()){
      warning("Version mismatch. (if not running consider clean_start = T)")
    }

  }

  if(type=="client"){
    # need to develop
  }

}




client_new <- function(Browser = c("chrome", "firefox", "phantomjs", "internet explorer"), headless = F, start_browser = T, ...){

  Browser <- match.arg(Browser)

  if (identical(Browser, "internet explorer") &&
      !identical(.Platform[["OS.type"]], "windows")) {
    stop("Internet Explorer is only available on Windows.")
  }

  if(!(Browser %in% c("chrome","phantomjs") ) & headless){
    stop("Yet not implemented headless browsing with this Browser")
  }

  if(!check_selenium_strom()){
    stop("Can't create remoteDriver instance without selenium_strom properly running")
  }

  st <- get_selenium_storm_storr()

  exnames <- names(list(...))

  if(("extraCapabilities" %in% exnames) & Browser=="chrome" & headless ){
    warning("Browser = 'chrome', headless = TRUE and additionally extraCapabilities supplied. \nHence headless will be disabled (possibly it has to be taken care by extraCapabilities)")
    headless <- F
  }

  if(Browser=="chrome" & headless){
    eCaps <- list(chromeOptions = list(
      args = c('--headless', '--disable-gpu', '--window-size=1280,800')
    ))

    remDr <- RSelenium::remoteDriver(browserName = Browser, port = as.integer(st$get("port", "config")), extraCapabilities = eCaps, ...)
  }else{
    remDr <- RSelenium::remoteDriver(browserName = Browser, port = as.integer(st$get("port", "config")), ...)
  }

  count <- 0
  while (
    inherits(res <- tryCatch(remDr$getStatus(), error = function(e) e), "error")
  ) {
    Sys.sleep(1)
    count <- count + 1
    if (count > 10) {
      warning("Could not determine server status. (after 10 secs)")
      break
    }
  }

  if(start_browser){
    open_browser(remDr)
  }

  invisible(remDr)

}


get_control_client <- function(){

  sst <- get_selenium_storm_storr(session = T)
  if(sst$exists("control_client", "handles")){
    cc <- sst$get("control_client", "handles")
  }else{
    cc <- client_new(start_browser = F)
    sst$set("control_client", cc,"handles")
  }
  invisible(cc)
}


sessions_cleanup.sessions <- function(){

  st <- get_selenium_storm_storr()

  stored_sessions <- st$list("sessions")
  active_sessions <- get_active_sessions(get_control_client()) %>% purrr::map_chr("id")

  delinked <- setdiff(stored_sessions, active_sessions)

  if(length(delinked)){
    delinked %>% purrr::map(~st$del(.x, namespace = "sessions"))
  }

  invisible(0)
}


sid_pid_map <- function(){
  st <- get_selenium_storm_storr()

  dsi <- data.frame(si = st$list("sessions"), stringsAsFactors = F)
  safeget <- function(x){
    gt <- try({st$get(x,namespace = "sessions")}, silent = T)
    if(inherits(gt,"try-error")) return(NA)
    gt
  }
  dsi$pid <- dsi$si %>% purrr::map_chr(safeget) %>%  as.integer()
  dsi$is_dead_pid <- !is_pid_active(dsi$pid)
  dsi
}

sessions_cleanup.pids <- function(){

  st <- get_selenium_storm_storr()

  dsi <- sid_pid_map()

  if(any(dsi$is_dead_pid)){
    dsi$si[dsi$is_dead_pid] %>% purrr::map(~st$del(.x, namespace = "sessions"))
  }

  invisible(dsi$si[dsi$is_dead_pid])
}

sessions_cleanup <- function(){
  sessions_cleanup.sessions()
  sessions_cleanup.pids()
}

free_sessions <- function(){

  sessions_cleanup()

  st <- get_selenium_storm_storr()

  stored_sessions <- st$list("sessions")
  active_sessions <- get_active_sessions(get_control_client()) %>% purrr::map_chr("id")

  setdiff(active_sessions, stored_sessions)

}

client_instant <- function(Browser = c("chrome", "firefox", "phantomjs", "internet explorer"), headless = F, ...){
  active_sessions <- get_active_sessions(get_control_client())
  st <- get_selenium_storm_storr()
  num_cores <- st$get("num_cores","config")
  singular_pid_sid <- st$get("singular_pid_sid","config")
  fss <- free_sessions()

  dummy_client <- client_new(Browser = Browser, headless = headless, start_browser = F, ...)

  if(singular_pid_sid){
    dsi <- sid_pid_map()
    any_sid_for_this_pid <- dsi$si[dsi$pid==Sys.getpid()]
    if(length(fss)==0){
      fss <- any_sid_for_this_pid
    }
  }

  if(length(fss)){
    attach_ok <- attach_to_active_session(dummy_client, fss[1])
    if(attach_ok){
      open_browser(dummy_client)
      return(dummy_client)
    }
  }else{
    if(length(active_sessions) < num_cores){
      open_browser(dummy_client)
      return(dummy_client)
    }
  }

  return(NULL)
}
