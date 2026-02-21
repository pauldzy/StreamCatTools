sc_get_data2 <- function(
   request_body = NULL
  ,chunker      = NULL
  ,endpoint     = NULL
  ,tmpfile      = NULL
  ,checkparms   = TRUE
  ,verbose      = FALSE
  ,showrequest  = FALSE
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
  
  if (isTRUE(verbose)) {
    message(paste(". querying",httr2::req_get_url(request)));
  }
  
  if (is.null(tmpfile)) {
    tmpfile = paste0(tempfile(),'.csv');
  }
  if (isTRUE(verbose)) {
    message(paste(". staging results at",tmpfile));
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
    
  } else {
    if (isTRUE(verbose)) {
      message(paste(". chunk value of",request));
      
    }
    
  }

  if ("name" %in% names(request_body) && request_body[["name"]][1] == "all") {
    if ("conus" %in% names(request_body) && !is.null(request_body[["conus"]][0])) {
      stop('If you are requesting all metrics please request for regions, states or counties rather than all of conus')

    } else {
      message("Using metric='all' with a large aoi may take a considerable amount of time to return results - request may timeout if multiple AOIs are requested")

    }

  }

  if (isTRUE(checkparms)) {
    params <- sc_get_params(param='metric_names');
    
    if ("name" %in% names(request_body) && request_body[["name"]][1] != "all") {
      if (!all(request_body[["name"]] %in% params)){
        message("One or more of the provided metric names do not match the expected metric names in StreamCat.  Use sc_get_params(param='metric_names') to list valid metric names for StreamCat");

      }
      
    }

  }
  
  rb2prm <- function(rb,key,scalar = FALSE) {
    if (key %in% names(rb)) {
      if (is.null(rb[[key]])) {
        return(NULL);
      
      } else {
        if (isTRUE(scalar)) {
          return(rb[[key]][1]);
        
        } else {
          return(paste(rb[[key]],collapse = ","));
        }
        
      }
      
    } else {
      return(NULL);
      
    } 
  
  }
  
  # be careful using static tempfile names if multiple requests are made similtaneously
  if (file.exists(tmpfile)) {
    file.remove(tmpfile)
  }
  # Open output csv for append
  con <- file(tmpfile,"a");

  colnames <- NULL;  
  
  # when chunker is null, do a straightforward CSV extraction into a data frame
  if (is.null(chunker)) {
    req <-
      request |>
      httr2::req_timeout(seconds = 180) |>
      httr2::req_retry(
         backoff          = ~ 15
        ,max_tries        = 10
        ,retry_on_failure = TRUE
      ) |>
      httr2::req_body_form(
          comid        = rb2prm(request_body,'comid')
         ,name         = rb2prm(request_body,'name')
         ,aoi          = rb2prm(request_body,'aoi')
         ,conus        = rb2prm(request_body,'conus',TRUE)
         ,countonly    = rb2prm(request_body,'countonly',TRUE)
         ,region       = rb2prm(request_body,'region')
         ,state        = rb2prm(request_body,'state')
         ,county       = rb2prm(request_body,'county')
         ,showpctfull  = rb2prm(request_body,'showpctfull',TRUE)
         ,showareasqkm = rb2prm(request_body,'showareasqkm',TRUE)
         ,showshape    = rb2prm(request_body,'showshape',TRUE)
         ,csv_header   = TRUE
         ,csv_last     = FALSE
         ,offset       = rb2prm(request_body,'offset',TRUE)
         ,limit        = rb2prm(request_body,'limit',TRUE)
         ,after        = rb2prm(request_body,'after',TRUE)
         ,debug        = rb2prm(request_body,'debug',TRUE)
      ) |>
      httr2::req_headers(Accept = "text/csv");
      
    if (isTRUE(showrequest)) {
      req |> httr2::req_dry_run();
      
    }

    resp <- tryCatch(
       httr2::req_perform_connection(req)
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
    
    while (!httr2::resp_stream_is_complete(resp)) {
      line <- httr2::resp_stream_lines(resp);
      
      if (substring(line,1,2) == '//') {
        # filter away any possible metadata
        {}
        
      } else {
        if (is.null(colnames)) {
          colnames <- line;
        }
        
        writeLines(line,con=con,sep="");
      
      }
    
    }
    
    # Make sure to close as R only provides 128 connections
    close(resp);
    
  # when chunker is provided, set limit to chunker size and capture last value of request using csv_after flag
  } else {
    hdr <- TRUE;
    aft <- 0;
    
    while (!is.null(aft) && aft != '') {
      if (isTRUE(verbose)) {
        message(paste(". requesting",chunker,"comids with after value",aft)); 
      }
      
      req <-
        request |>
        httr2::req_timeout(seconds = 180) |> 
        httr2::req_retry(
           backoff          = ~ 15
          ,max_tries        = 10
          ,retry_on_failure = TRUE
        ) |>
        httr2::req_body_form(
          comid        = rb2prm(request_body,'comid')
         ,name         = rb2prm(request_body,'name')
         ,aoi          = rb2prm(request_body,'aoi')
         ,conus        = rb2prm(request_body,'conus',TRUE)
         ,countonly    = rb2prm(request_body,'countonly',TRUE)
         ,region       = rb2prm(request_body,'region')
         ,state        = rb2prm(request_body,'state')
         ,county       = rb2prm(request_body,'county')
         ,showpctfull  = rb2prm(request_body,'showpctfull',TRUE)
         ,showareasqkm = rb2prm(request_body,'showareasqkm',TRUE)
         ,showshape    = rb2prm(request_body,'showshape',TRUE)
         ,csv_header   = hdr
         ,csv_last     = TRUE
         ,offset       = rb2prm(request_body,'offset',TRUE)
         ,limit        = chunker
         ,after        = aft
         ,debug        = rb2prm(request_body,'debug',TRUE)
      ) |>
      httr2::req_headers(Accept = "text/csv");
      
      if (isTRUE(showrequest)) {
        req |> httr2::req_dry_run();
        
      }
      
      resp <- tryCatch(
         httr2::req_perform_connection(req)
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
      
      while (!httr2::resp_stream_is_complete(resp)) {
        line <- httr2::resp_stream_lines(resp);
        
        if (substring(line,1,2) == '//') {
          if (substring(line,1,7) == '//last=') {
            # When a request returns no further records, aft will be NULL and exit the loop
            aft <- substring(line,8);
            
          }
          
        } else {
          if (is.null(colnames)) {
            colnames <- line;
          }
          
          writeLines(line,con=con,sep="");
        
        }
      
      }
      
      # Make sure to close as R only provides 128 connections
      close(resp);
      
      # Remove header from further iterations
      hdr <- FALSE;
      
    }
    
    cols = read.csv(text = colnames,header = FALSE);
    if (isTRUE(verbose)) {
      message(paste(". results have",length(cols),"columns"));
    }
     
    if (isTRUE(verbose)) {
      message(". loading results into data frame");
    }
    
    # This assumes all StreamCat results are numeric doubles
    df <- fread(
       tmpfile
      ,colClasses = list(
         integer64 = c(1)
        ,numeric   = c(2,length(cols))
       )      
    );

    if (isTRUE(verbose)) {
      message(". passing back dataframe");
    }
    if (exists("df") && !is.null(df)) {
      if ("count" %in% colnames(df)) {
        return(df$items);

      } else {
        return(df);

      }

    }
  
  }
  
  # Close the CSV file
  close(con);

}
NULL;
