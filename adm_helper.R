#' Read files by type.
#'
#' This function detects the file type and reads it. Currently supported file types are .xls, .xlsx, and .csv.
#'
#' @param file_name The name of the file to be read.
#' @param path The path of the file to be read.
#'
#' @return The data from the specified file.
#' @export
#'
#' @examples
#' \dontrun{
#' dat = read_by_type("example_file.xlsx", "example_path/")
#' }
#'
#' @importFrom readxl read_excel
#'
read_by_type = function(file_name, path) {
  file_type = gsub(".*\\.", "", file_name) # file type detection while reading
  if (file_type == "xlsx" | file_type == "xls") {
    dat = read_excel(paste0(path, "/", file_name))
    return(dat)
  }
  else if (file_type == "csv") {
    dat = read.csv(paste0(path, "/", file_name), header = T)
    return(dat)
  }
  else {
    stop("Fil type '", file_type, "' has not yet supported for ", file_name,
         ". Please convert it into .xlsx, .xls, or .csv")
  }
}


#' Match the first occurring item.
#'
#' This function finds the first variable in usr_list that matches one of the variables in file_list.
#' If none of the variables in the usr_list match any of the variables in the file_list,
#' the function returns the first variable in the default_list
#'
#' @param usr_list A list of variables that the user is interested in finding a match for.
#' @param file_list A list of variables that are available in a file.
#' @param default_list A character vector of default variables to use if no matching variables are found.
#'
#' @return The matched variable.
#' @export
#'
#' @examples
#' ordered_match(c("A", "B"), "C; D; A", c("X", "Y", "Z"))
ordered_match = function(usr_list, file_list, default_list) {
  if (file_list[1] == "") {
    return(NA)
  }
  file_list = unlist(strsplit(file_list, "; "))
  for (i in usr_list) {
    if (i %in% file_list) {
      return(i)
    }
  }
  return(default_list[1])
}


#' Get Source Table
#'
#' This function reads in data files from a specified directory and generates a source table
#' that summarizes the variables available in each file.
#' The source table includes information on the file name, variables in the file,
#' potential ID variables, potential DATE variables, and the ID and DATE variables to be used for merging.
#' The function also allows users to input merge options,
#' such as whether or not the DATE variables overlap and the time window for matching.
#' Users can also specify the names of the ID and DATE variables they wish to use for merging.
#'
#' @param path The path to the directory containing the data files.
#' @param FILE_pattern A regular expression pattern specifying the types of files to be read in. Default is ".xlsx|.xls|.csv".
#' @param ID_pattern A regular expression pattern specifying the potential ID variables in the data. Default is "ID".
#' @param DATE_pattern A regular expression pattern specifying the potential DATE variables in the data. Default is "DATE|VISITNO".
#' @param IS_overlap_list A list of Boolean values specifying whether or not the DATE variables overlap for each file. Default is NULL.
#' @param WINDOW_list A list of numric time windows for matching the DATE variables for each file. Default is NULL.
#' @param ID_usr_list A list of user-specified ID variable names for each file.
#' @param DATE_usr_list A list of user-specified DATE variable names for each file.
#' @param file A path to a file where the source table will be saved as a CSV file. If NULL, it will not be saved locally.
#'
#' @return A data frame summarizing the variables available in each file, with information on potential ID and DATE variables and the ID and DATE variables to be used for merging.
#' @export
#'
#' @examples
#' \dontrun{
#' get_src_table("path/to/data/directory")
#' }
get_src_table = function(path,
                         FILE_pattern = ".xlsx|.xls|.csv",
                         ID_pattern = "ID",
                         DATE_pattern = "DATE|VISITNO",
                         IS_overlap_list = NULL,
                         WINDOW_list = NULL,
                         ID_usr_list = NULL,
                         DATE_usr_list = NULL,
                         file = NULL) { # outputfile
  # get the source table
  files_list = list.files(path, pattern = FILE_pattern)
  dict_src = data.frame()
  for (file_name in files_list) {
    dat = suppressWarnings(read_by_type(file_name, path))
    var_names = names(dat) # all variables in the data
    f_name = gsub(FILE_pattern, "", file_name) # file name
    assign(f_name, dat)
    f_id = grep(ID_pattern, names(dat), ignore.case = T,  value = T) # select potential ID variables
    f_date = grep(DATE_pattern, names(dat), ignore.case = T,  value = T) # select potential DATE variables
    # building source table
    dict_tem = data.frame(file = f_name,
                          VARS_in_file = paste(var_names, collapse = "; "),
                          ID_in_file = paste(f_id, collapse = "; "),
                          DATE_in_file = paste(f_date, collapse = "; "),
                          ID_for_merge = f_id[1], # default to select the first one
                          DATE_for_merge = f_date[1]) %>% # default to select the first one
      mutate(IS_overlap = if_else(is.na(DATE_for_merge), NA, FALSE), # default FALSE
             WINDOW = if_else(is.na(DATE_for_merge), NA, 366)) # defailt 366
    dict_src = rbind(dict_src, dict_tem)
  }
  if (nrow(dict_src) == 0) {
    stop("No data detected, please double check the 'path' and file types.")
  }
  # exclude data without IDs
  dict_src = dict_src %>%
    filter(!is.na(ID_for_merge))
  # User input merge options (the length of input list must equal to the number of rows for dict_src, use NA for none DATE data)
  if (!is.null(IS_overlap_list)) {
    dict_src = dict_src %>%
      mutate(IS_overlap = IS_overlap_list)
  }
  if (!is.null(WINDOW_list)) {
    dict_src = dict_src %>%
      mutate(WINDOW = WINDOW_list)
  }
  # Select based on user inputs ID and DATE names
  if (!is.null(ID_usr_list)) {
    dict_src = dict_src %>%
      rowwise() %>%
      mutate(ID_for_merge = ordered_match(ID_usr_list, VARS_in_file, ID_for_merge)) %>%
      ungroup()
  }
  if (!is.null(DATE_usr_list)) {
    dict_src = dict_src %>%
      rowwise() %>%
      mutate(DATE_for_merge = ordered_match(DATE_usr_list, VARS_in_file, DATE_for_merge)) %>%
      ungroup()
  }
  if (!is.null(file)) {
    write.csv(dict_src, file = paste0(file, "/dict_src.csv"), row.names = F, quote = F)
  }
  return(dict_src)
}


#' Get full list of IDs
#'
#' This function creates a data frame containing all possible IDs across multiple data files.
#' It uses the source table generated by the 'get_src_table' function to identify potential ID variables in each data file
#' and merge them into a single data frame.
#'
#' @param dict_src A data frame containing information on the data files and their variables, potential ID and DATE variables, and merge options.
#'
#' @return A data frame containing all possible IDs across multiple data files.
#' @export
#'
#' @examples
#' \dontrun{
#' get_key_IDs(dict_src)
#' }
#'
#' @import dplyr
#'
get_key_IDs = function(dict_src) {
  name_ID = na.omit(unique(unlist(strsplit(dict_src$ID_for_merge, ", ")))) # get all possible names
  key_ID = data.frame(matrix("NA", nrow = 0, ncol = length(name_ID)))
  colnames(key_ID) = name_ID
  #key_DATE
  dat_list = dict_src$file
  for (dat in dat_list) {
    dat_ID = eval(as.name(dat)) %>%
      select(any_of(name_ID)) %>%
      mutate(across(everything(), as.character)) # regard all IDs as characters
    if (ncol(dat_ID) == 0) {
      next
    }
    merge_ID = names(dat_ID)
    key_ID = key_ID %>%
      full_join(dat_ID, by = merge_ID, multiple = "all") %>%
      distinct() %>%
      na.omit()
  }
  if (nrow(key_ID) == 0) {
    stop("Errors occur when generating all IDs. Please double check that the 'ID_for_merge' listed in 'dict_src' for each file is mergeable. 'dict_src' can be obtained by function 'get_src_table(path)'")
  }
  return(key_ID)
}


#' Get full list of DATEs
#'
#' This function creates a data frame containing all DATEs for merging.
#'
#' @param dict_src A data frame containing information on the data files and their variables, potential ID and DATE variables, and merge options.
#' @param timeline_file The name of the file containing the timeline.
#' @param DATE_type A character string specifying the type of DATE variable to read in. Must be either "Date" or "Number".
#'
#' @return A data frame containing all DATEs for merging.
#' @export
#'
#' @examples
#' \dontrun{
#' get_key_DATEs(dict_src, timeline_file = "timeline.xlsx", DATE_type = "Date")
#' }
#'

get_key_DATEs = function(dict_src,
                         timeline_file,
                         DATE_type = c("Date", "Number")) {
  name_ID = na.omit(unique(unlist(strsplit(dict_src$ID_for_merge, ", "))))
  name_DATE = dict_src$DATE_for_merge[grep(timeline_file, dict_src$file)]
  if (DATE_type == "Date") {
    key_DATE = eval(as.name(timeline_file)) %>%
      select(any_of(c(name_ID, name_DATE))) %>%
      mutate(across(any_of(name_ID), as.character),
             across(any_of(name_DATE), as.Date)) %>%
      na.omit() %>%
      group_by(across(any_of(name_ID))) %>%
      arrange(name_DATE, .by_group = T) %>%
      mutate(date_left  = (!!as.name(name_DATE) - lag(!!as.name(name_DATE))) / 2,
             date_right = lead(date_left))
  }
  else if (DATE_type == "Number") {
    key_DATE = eval(as.name(timeline_file)) %>%
      select(any_of(c(name_ID, name_DATE))) %>%
      mutate(across(everything(), as.character)) %>%
      na.omit()
  }
  else {
    stop("Readed 'DATE_type' must be either 'Date' or 'Number'.")
  }
  return(key_DATE)
}


#' Get the bounds for the window timeline.
#'
#' This function calculates the window bounds for a given date.
#' The window bounds are used to define the time range for longitudinal data.
#' The function takes the original date, the bound date, the overlap flag,
#' and the window length as inputs, and returns the left or right window bound.
#'
#' @param original_date The original date for which to calculate the window bound.
#' @param bound_date The bound date for the window, which can be specified as a specific date or a number of days.
#' @param is_left A logical value indicating whether the left or right bound should be returned.
#' @param ovlp A logical value indicating whether the data is overlapping or not. If the data is overlapping, the window bounds will be calculated using only the window length. If the data is not overlapping, the minimum of the bound date and today's date minus half the window length will be used.
#' @param window_len The length of the window in days.
#'
#' @return The left or right window bound for the given date.
#' @export
#'
#' @examples
#' \dontrun{
#' get_window_bound("2021-01-01", "2022-01-01", is_left = TRUE, ovlp = TRUE, window_len = 365)
#' }
#'
get_window_bound = function(original_date,
                            bound_date,
                            is_left,
                            ovlp,
                            window_len) {
  bound_date[is.na(bound_date)] = Sys.Date()-(Sys.Date()-window_len/2)
  if(ovlp) {
    return(original_date - is_left * window_len / 2)
  }
  else {
    return(original_date - is_left * min(bound_date,
                                         Sys.Date()-(Sys.Date()-window_len/2)))
  }
}



