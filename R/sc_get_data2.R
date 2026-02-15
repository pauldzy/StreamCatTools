sc_get_data2 <- function(
   request_body = NULL
  ,chunker      = NULL
  ,endpoint     = NULL
) {

  # Base API URL.
  if (is.null(endpoint)) {
    base_url <- 'https://api.epa.gov';
    base_end <- '/StreamCat/streams2/metrics';
    request  <- 
      httr2::request(base_url) |>
      httr2::req_url_path(base_end);
  
  } else {
    request <- httr2::request(endpoint);
    
  }
  
  # Force old and odd naming convention to behave correctly
  if ("aoi" %in% names(request_body)) {
    request_body[["aoi"]] <- unlist(lapply(request_body[["aoi"]],function(x) {
      if (tolower(x) == "catchment") {
        x <- "cat";
      }
      if (tolower(x) == "watershed") {
        x <- "ws";
      }
      if (tolower(x) == "riparian_catchment") {
        x <- "catrp100";
      }
      if (tolower(x) == "riparian_watershed") {
        x <- "wsrp100";
      }
      return(x);
    }));
  }

  # if user supplies their own offset, limit or after, disable chunking
  if (!is.null(chunker)) {
    if ("offset" %in% names(request_body) && !is.null(request_body[["offset"]])) {
      chunker <- NULL;
      message("suppressing chunker value when offset is provided in request body");
    }
    if ("limit" %in% names(request_body) && !is.null(request_body[["limit"]])) {
      chunker <- NULL;
      message("suppressing chunker value when limit is provided in request body");
    }
    if ("after" %in% names(request_body) && !is.null(request_body[["after"]])) {
      chunker <- NULL;
      message("suppressing chunker value when after is provided in request body");
    }
    
  }

  if ("name" %in% names(request_body) && request_body[["name"]][1] == "all") {
    if ("conus" %in% names(request_body) && !is.null(request_body[["conus"]][0])) {
      stop('If you are requesting all metrics please request for regions, states or counties rather than all of conus')

    } else {
      message("Using metric='all' with a large aoi may take a considerable amount of time to return results - request may timeout if multiple AOIs are requested")

    }

  }

  params <- sc_get_params(param='metric_names');
  if ("name" %in% names(request_body) && request_body[["name"]][1] != "all") {
    if (!all(request_body[["name"]] %in% params)){
      message("One or more of the provided metric names do not match the expected metric names in StreamCat.  Use sc_get_params(param='metric_names') to list valid metric names for StreamCat");

    }

  }

  if (is.null(chunker)) {
    req <-
      request |>
      httr2::req_retry(backoff = ~ 5, max_tries = 6) |>
      httr2::req_body_json(request_body);
    
    resp <- httr2::req_perform(req);

    resp_list <-
      resp |>
      httr2::resp_body_json(simplifyVector = TRUE);
    
    df <- as.data.frame(resp_list[["results"]]);

    if (exists("df") && !is.null(df)) {
      if ("count" %in% colnames(df)) {
        return(df$items);

      } else {
        df %>% dplyr::select(comid,dplyr::everything());
        return(df);

      }

    }
    stop("unable to convert service response into valid data frame");
    
  } else {
    rowcnt <- NULL;
    aft    <- 0;
    df     <- NULL;
    
    while (is.null(rowcnt) || rowcnt > 0) {
      message(paste(". requesting",chunker,"comids")); 
      
      rb <- c(
         request_body
        ,limit = chunker
        ,after = aft
      );
      
      req <-
        request |>
        httr2::req_retry(
           backoff = ~ 15
          ,max_tries = 6
          ,retry_on_failure = TRUE
        ) |>
        httr2::req_timeout(seconds = 60) |> 
        httr2::req_body_json(rb) |>
        httr2::req_verbose();
      
      resp <- tryCatch(
         httr2::req_perform(req)
        ,httr2_http_502 = function(cnd) {
          message(". got 502, waiting to try again");
          Sys.sleep(10);
          req |> httr2::req_perform(req)
         }
        ,httr2_http_503 = function(cnd) {
          message(". got 503, waiting to try again");
          Sys.sleep(30);
          req |> httr2::req_perform(req)
         }
        ,httr2_http_504 = function(cnd) {
          message(". got 504, waiting to try again");
          Sys.sleep(15);
          req |> httr2::req_perform(req)
         }
      );

      resp_list <-
        resp |>
        httr2::resp_body_json(simplifyVector = TRUE);
      
      if (is.null(df)) {
        df <- as.data.frame(resp_list[["results"]]);
        
        if (exists("df") && !is.null(df)) {
          if ("count" %in% colnames(df)) {
            return(df$items);

          }
          
        }
        
        colcnt = length(df);
        rowcnt = nrow(df);
        if (rowcnt == 0) {
          df %>% dplyr::select(comid,dplyr::everything());
          return(df);
          
        }
        
        message(paste(". got",rowcnt,"records of",colcnt,"columns at last comid",aft));
      
      } else {
        tmp <- as.data.frame(resp_list[["results"]]);
        
        colcnt = length(tmp);
        rowcnt = nrow(tmp);
        message(paste(". got",rowcnt,"records of",colcnt,"columns at last comid",aft));
        
        if (rowcnt == 0) {
          df %>% dplyr::select(comid,dplyr::everything());
          return(df);
        
        } else {
          df <- rbind(
             df
            ,tmp
          );
          
        }
      
      }
      
      aft <- resp_list[["last"]]; 
       
    }
  
  }

}

NULL;
