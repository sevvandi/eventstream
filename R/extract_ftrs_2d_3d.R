extract_event_ftrs_3d <- function(stream, supervised=FALSE, details=NULL, win_size=2, step_size=1, thres=0.98, tt, epsilon, miniPts){

  # The first dimension is always time

  supervised=FALSE
  details=NULL
  # save=FALSE
  # folder="None"
  # vis=FALSE


  if(supervised){
    if( is.null(details) ){
      stop("Please specify details for supervised event extraction.")
    }
  }

  num_times <- floor((dim(stream)[1]-win_size)/step_size)+1
  win_st <- 1
  win_en <- win_size

  for(jj in 1:num_times){
    ## Chop window first
    win_dat <- stream[win_st:win_en,,]
    # if(save){
    #   file_name <- paste(folder, "Events_", 100000+jj, ".jpg", sep="")
    #   dlm_feat <- extract_events_3d(win_dat, "Y", file_name, thres, vis, tt, epsilon, miniPts)
    # }else{
      dlm_feat <- extract_events_3d(win_dat,  thres, tt, epsilon, miniPts)
    #}

    if(!is.null(dlm_feat)){
      if(dim(dlm_feat)[1]>0){
        # if(supervised){
        #   dlm_feat <-   get_class_labels(dlm_feat, win_st, win_en, details)
        # }
        if((jj==1)|(!exists('all_train_features'))){
          all_train_features <- dlm_feat
        }else{
          all_train_features <- abind::abind(all_train_features,dlm_feat, along=1)
        }
      }
    }
    
    ## Update start and end indices
    win_st <- win_st + step_size
    win_en <- min(win_en + step_size, dim(stream)[1])
  }
  if(!exists("all_train_features")){
    # if(!supervised){
    #   class_col <- dim(all_train_features)[2]
    #   all_train_features <- all_train_features[,-class_col,]
    # }
    all_train_features <- NULL
  }
  return(all_train_features)

}





















######################################################################################

extract_event_ftrs_2d <- function(stream, supervised, details, win_size, step_size, thres, save, folder, vis, tt, epsilon, miniPts, rolling=TRUE){

  if(supervised){
    if( is.null(details) ){
      stop("Please specify details for supervised event extraction.")
    }
  }

  num_times <- floor((dim(stream)[1]-win_size)/step_size)+1
  win_st <- 1
  win_en <- win_size

  for(jj in 1:num_times){
    ## Chop window first
    win_dat <- stream[win_st:win_en,]
    if(save){
      file_name <- paste(folder, "Events_", 100000+jj, ".jpg", sep="")
      #file_name <- paste(folder, "Events_", 100000+jj, ".pdf", sep="")
      dlm_feat <- extract_events(win_dat, "Y", file_name, thres, vis, tt, epsilon, miniPts, rolling=rolling)
    }else{
      dlm_feat <- extract_events(win_dat, "N", file_name, thres, vis, tt, epsilon, miniPts, rolling=rolling)
    }

    if(!is.null(dlm_feat)){
      if(dim(dlm_feat)[1]>0){
        if(supervised){
          dlm_feat <-   get_class_labels(dlm_feat, win_st, win_en, details)
        }
        if((jj==1)|(!exists('all_train_features'))){
          all_train_features <- dlm_feat
        }else{
          all_train_features <- abind::abind(all_train_features,dlm_feat, along=1)
        }
      }
    }
 
    ## Update start and end indices
    win_st <- win_st + step_size
    win_en <- min(win_en + step_size, dim(stream)[1])
  }

  if(exists("all_train_features")){
    if(!supervised){
      class_col <- dim(all_train_features)[2]
      all_train_features <- all_train_features[,-class_col,]
    }
  }else{
    all_train_features <- NULL
  }

  return(all_train_features)

}

