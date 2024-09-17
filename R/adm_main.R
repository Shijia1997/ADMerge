#' Alzheimer’s Disease Biomarker Collection
#'
#' This function merges data from various sources to create a longitudinal dataset for Alzheimer's disease biomarker collection.
#' The input data can be in various formats, such as Excel or CSV files. The function takes the path to the input data,
#' the type of date used in the data, and various other parameters as inputs, and returns a merged dataset with all the relevant information.
#'
#' @param path The path to the directory containing the input data.
#' @param DATE_type The type of date used in the data, either "Date" or "Number".
#' @param ... Additional parameters for the function.
#' @param dict_src A dataframe containing information about the input data files.
#' @param dict_src_path The path to the dictionary source file.
#' @param timeline_file The name of the file containing the timeline for the data.
#' @param timeline_path The path to the timeline file.
#' @param FILE_pattern A regular expression pattern for the input file names.
#' @param ID_pattern A regular expression pattern for the ID variable in the data.
#' @param DATE_pattern A regular expression pattern for the date variable in the data.
#' @param IS_overlap_list A list of boolean values indicating whether the data for each file is overlapping or not.
#' @param WINDOW_list A list of window lengths for each file.
#' @param ID_usr_list A list of user-specified ID variables for each file.
#' @param DATE_usr_list A list of user-specified date variables for each file.
#'
#' @return A merged dataset with all the relevant information.
#' @export
#'
#' @examples
#' \dontrun{
#' ad_merge("data", DATE_type = "Date")
#' }
#'
#' @import tidyverse
#'
#' @import lubridate
#' 
#' @import stringr
#' 
#' @import stringi
#' 
ad_merge = function(path,
                    DATE_type,
                    ...,
                    dict_src = NULL,
                    dict_src_path = NULL,
                    timeline_file = NULL,
                    timeline_path = NULL,
                    FILE_pattern = ".xlsx|.xls|.csv",
                    ID_pattern = "ID",
                    DATE_pattern = "DATE|VISITNO",
                    IS_overlap_list = NULL,
                    WINDOW_list = NULL,
                    ID_usr_list = NULL,
                    DATE_usr_list = NULL) {
  files_list = list.files(path, pattern = FILE_pattern)
  if(missing(DATE_type)) {
    stop("Argument 'DATE_type' is missing. You must specify 'DATE_type' as either 'Date' or 'Number'.")
  }
  
  if(!is.character(DATE_type) || length(DATE_type) != 1 || !DATE_type %in% c("Date", "Number")) {
    stop("Argument 'DATE_type' must be a single character string, either 'Date' or 'Number'.")
  }
  for (file_name in files_list) {
    dat = suppressWarnings(read_by_type(file_name, path))
    f_name = gsub(FILE_pattern, "", file_name) # file name
    assign(f_name, dat, envir = .GlobalEnv)
  }
  cat("Finished loading data. \n")
  # Get summary of source data
  if (is.null(dict_src)) {
    if (is.null(dict_src_path)) {
      dict_src = get_src_table(path,
                               FILE_pattern,
                               ID_pattern,
                               DATE_pattern,
                               IS_overlap_list,
                               WINDOW_list,
                               ID_usr_list,
                               DATE_usr_list)
    }
    else {
      dict_src = read.csv(dict_src_path, header = T)
    }
  }
  cat("dict_src generated. \n")
  # Generate key ID data
  key_ID = get_key_IDs(dict_src)

  # Select DATE data (there are two types of DATE, strings like visit number or actual date)
  if (!is.null(timeline_path)) {
    key_DATE = read.csv(timeline_path, header = T)
  }
  else {
    if (is.null(timeline_file)) {
      timeline_list = dict_src %>%
        filter(!is.na(DATE_for_merge))
      timeline_file = timeline_list$file[1]
    }

  }
  
  key_DATE = get_key_DATEs(dict_src,
                           timeline_file,
                           DATE_type)
  

  
  cat("Keys generated. \n")
  # Merge
  ID_DATE = dict_src$ID_for_merge[grep(timeline_file, dict_src$file)]
  name_DATE = dict_src$DATE_for_merge[grep(timeline_file, dict_src$file)]
  
  timeline_index <- match(timeline_file, dict_src$file)
  
  new_order <- c(timeline_index, setdiff(1:nrow(dict_src), timeline_index))
  
  dict_src <- dict_src[new_order, ]
  
  dat_list = dict_src$file
  if (DATE_type == "Date") {
    dat_all = key_ID %>%
      left_join(key_DATE,
                by = c("ID_merged" = ID_DATE),
                multiple = "all",
                suffix = c("", ".dup")) %>%
      select(-ends_with(".dup")) 
    base_names = names(dat_all)
    for (dat in dat_list) {
      idx = grep(dat, dict_src$file)
      ID = dict_src$ID_for_merge[idx]
      DATE = dict_src$DATE_for_merge[idx]
      OVLP = dict_src$IS_overlap[idx]
      WIN = dict_src$WINDOW[idx]
      if (is.na(DATE)|is.na(name_DATE)) { # demo
        dat_tem = eval(as.name(dat)) %>%
          mutate(!!as.name(ID) := as.character(!!as.name(ID)))
        dat_all = dat_all %>%
          left_join(dat_tem,
                    by = c("ID_merged" = ID),
                    suffix = c("", ".dup"),
                    multiple = "all") %>%
          select(-ends_with(".dup")) %>%
          distinct()
        
      }
      else { # longitudinal
        dat_tem = eval(as.name(dat)) %>%
          mutate(!!as.name(ID) := as.character(!!as.name(ID)),
                 across(any_of(DATE), function(x) {
                   case_when(
                     stringr::str_detect(x, "\\d{1,2}/\\d{1,2}/\\d{4}") ~ dmy(x, quiet = TRUE),
                     stringr::str_detect(x, "\\d{4}-\\d{1,2}-\\d{1,2}") ~ ymd(x, quiet = TRUE),
                     TRUE ~ as.Date(NA)
                   )
                 }))
        

        
        if (DATE != name_DATE){dat_add = dat_all %>%
          group_by(ID_merged) %>% 
          mutate(tem_date_left = get_window_bound(!!as.name(name_DATE),
                                                  date_left,
                                                  is_left = 1,
                                                  ovlp = OVLP,
                                                  window_len = WIN),
                 tem_date_right = get_window_bound(!!as.name(name_DATE),
                                                   date_right,
                                                   is_left = -1,
                                                   ovlp = OVLP,
                                                   window_len = WIN))%>%
          left_join(dat_tem,
                    by = c("ID_merged" = ID),
                    suffix = c("", ".dup"),
                    multiple = "all")  %>%
          select(-ends_with(".dup")) %>%
          distinct() %>%
          filter(!!as.name(DATE) >= tem_date_left &
                   !!as.name(DATE) < tem_date_right) %>%
          mutate(diff = abs(as.Date(!!as.name(DATE)) - as.Date(!!as.name(name_DATE)))) %>%
          filter(!is.na(diff)) %>% 
          group_by(ID_merged, !!as.name(name_DATE)) %>%
          arrange(diff, .by_group = T) %>%
          filter(row_number() == 1) %>%
          ungroup() %>%
          select(-c("diff", "tem_date_left", "tem_date_right"))%>%
          filter(!is.na(!!as.name(name_DATE))) %>% 
          distinct() %>% 
          select(-!!as.name(DATE))
        
        
        } else if (DATE == name_DATE){
          dat_add = dat_all %>%
          group_by(ID_merged) %>% 
          mutate(tem_date_left = get_window_bound(!!as.name(name_DATE),
                                                  date_left,
                                                  is_left = 1,
                                                  ovlp = OVLP,
                                                  window_len = WIN),
                 tem_date_right = get_window_bound(!!as.name(name_DATE),
                                                   date_right,
                                                   is_left = -1,
                                                   ovlp = OVLP,
                                                   window_len = WIN)) %>% 
        
          left_join(dat_tem,
                    by = c("ID_merged" = ID),
                    suffix = c("", ".dup"),
                    multiple = "all")  %>%
          distinct() %>%
          filter(!!as.name(paste0(DATE,".dup")) >= tem_date_left &
                   !!as.name(paste0(DATE,".dup")) < tem_date_right) %>%
          mutate(diff = abs(as.Date(!!as.name(name_DATE)) - as.Date(!!as.name(paste0(DATE,".dup"))))) %>%
          filter(!is.na(diff)) %>% 
          group_by(ID_merged, !!as.name(name_DATE)) %>%
          arrange(diff, .by_group = T) %>%
          filter(row_number() == 1) %>%
          ungroup() %>%
          select(-c("diff", "tem_date_left", "tem_date_right"))%>%
          select(-ends_with(".dup")) %>%
          filter(!is.na(!!as.name(name_DATE))) %>% 
          distinct() 
       
    
        
        }
        dat_all = dat_all %>%
          left_join(dat_add,
                    by = base_names,
                    suffix = c("", ".dup")) 
        
        hold_data = dat_all
        
        dup_columns <- names(dat_all)[grepl("\\.dup$", names(dat_all))]
        orig_columns <- sub("\\.dup$", "", dup_columns)
        
        
        for (col in orig_columns) {
          dup_col <- paste0(col, ".dup")
          dat_all[[col]] <- coalesce(dat_all[[col]], dat_all[[dup_col]])
        }
        
        
        dat_all <- dat_all %>% select(-ends_with(".dup")) %>% distinct()
      }
      
      
    }#
    if (!is.na(name_DATE)){
      dat_all = dat_all %>%
        select(-c("date_left", "date_right")) %>% 
        mutate(!!as.name("Date_timeline") := as.Date(!!as.name(name_DATE))) %>%
        filter(!is.na(!!as.name(name_DATE))) %>% 
        select(1, Date_timeline,everything()) %>% 
        select(-!!as.name(name_DATE))
        
    }
    
  }
  else if (DATE_type == "Number") {
    dat_all = key_ID %>%
      left_join(key_DATE,
                by = c("ID_merged" = ID_DATE),
                multiple = "all",
                suffix = c("", ".dup")) %>%
      select(-ends_with(".dup")) %>%
      mutate(!!as.name("Date_timeline") := as.character(!!as.name(name_DATE))) %>% 
      select(-!!as.name(name_DATE)) %>% 
      na.omit() 
    
    for (dat in dat_list) {
      idx = grep(dat, dict_src$file)
      ID = dict_src$ID_for_merge[idx]
      DATE = dict_src$DATE_for_merge[idx]
      if (is.na(DATE)) { # demo
        dat_tem = eval(as.name(dat)) %>%
          mutate(!!as.name(ID) := as.character(!!as.name(ID)))
        dat_all = dat_all %>%
          left_join(dat_tem,
                    by = c("ID_merged" = ID),
                    suffix = c("", ".dup"),
                    multiple = "all") %>%
          select(-ends_with(".dup")) %>%
          distinct()
      }
      else { # longitudinal
        dat_tem = eval(as.name(dat)) %>%
          mutate(!!as.name(ID) := as.character(!!as.name(ID)),
                 !!as.name(DATE) := as.character(!!as.name(DATE))) 
        
        
          dat_all = dat_all %>%
            left_join(dat_tem,
                      by = c("ID_merged" = ID,
                             "Date_timeline" = DATE),
                      suffix = c("", ".dup"),
                      multiple = "all")
          
          dup_columns <- names(dat_all)[grepl("\\.dup$", names(dat_all))]
          orig_columns <- sub("\\.dup$", "", dup_columns)
          
          
          for (col in orig_columns) {
            dup_col <- paste0(col, ".dup")
            dat_all[[col]] <- coalesce(dat_all[[col]], dat_all[[dup_col]])
          }
          
          
          dat_all <- dat_all %>% select(-ends_with(".dup")) %>% distinct()
          
  
      }
    }
  }
  else {
    stop("Readed 'DATE_type' must be either 'Date' or 'Number'.")
  }
  cat("Merge done! \n")
  out_res = list(analysis_data = dat_all %>% distinct(),
                 dict_src = dict_src,hold_data)
  class(out_res) = "ADMerge_res"
  return(out_res)
}


