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
#' @param IS_overlap_list A list of logical values that specifies, when merging, whether overlapping between time windows is allowed (TRUE) or not (FALSE). The length of the list must be equal to the number of files being read. Default is NULL.
#' @param WINDOW_list A list of numeric time windows for matching the DATE variables for each file. The length of the list must be equal to the number of files being read. Default is NULL.
#' @param ID_usr_list A list of user-specified ID variable names. If provided, the function will try to match the variable names to the potential ID variables in the data files. The default is NULL.
#' @param DATE_usr_list A list of user-specified DATE variable names. If provided, the function will try to match the variable names to the potential DATE variables in the data files. The default is NULL.
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
                         DATE_pattern = "VIS|DATE",
                         IS_overlap_list = NULL,
                         WINDOW_list = NULL,
                         ID_usr_list = NULL,
                         DATE_usr_list = NULL,
                         non_longitudinal_list = NULL,
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
    longitudinal_value = !((f_name %in% non_longitudinal_list)| (length(f_date) == 1 && "update_stamp" %in% f_date))

    
    
    # building source table
    dict_tem = data.frame(file = f_name,
                          VARS_in_file = paste(var_names, collapse = "; "),
                          ID_in_file = paste(f_id, collapse = "; "),
                          DATE_in_file = paste(f_date, collapse = "; "),
                          ID_for_merge = f_id[1], # default to select the first one
                          DATE_for_merge = f_date[1],# default to select the first one
                          longitudinal = longitudinal_value) %>% 
      mutate(IS_overlap = if_else(is.na(DATE_for_merge), NA, FALSE), # default FALSE
             WINDOW = if_else(is.na(DATE_for_merge), NA_real_, 366)) # defailt 366
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
  
  if (!is.null(non_longitudinal_list) | (FALSE %in% dict_src$longitudinal)) {
    dict_src$DATE_for_merge = ifelse(dict_src$longitudinal, dict_src$DATE_for_merge, NA)
  } else {
    dict_src$longitudinal = TRUE
  }
  
  dict_src$longitudinal = ifelse(!is.na(dict_src$DATE_for_merge), TRUE, FALSE)
  
  
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
#' @import tidyverse
#'


first_non_na <- function(row) {
  non_na_values <- row[!is.na(row)]
  if (length(non_na_values) > 0) {
    return(non_na_values[1])
  } else {
    return(NA)
  }
}

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
      distinct()
  }
  if (nrow(key_ID) == 0) {
    stop("Errors occur when generating all IDs. Please double check that the 'ID_for_merge' listed in 'dict_src' for each file is mergeable. 'dict_src' can be obtained by function 'get_src_table(path)'")
  }
  
  key_ID$ID_merged <- apply(key_ID, 1, first_non_na)
  key_ID <- key_ID %>% select(ID_merged)
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

get_key_DATEs = function(dict_src, timeline_file, DATE_type = c("Date", "Number")) {
  name_ID = na.omit(unique(unlist(strsplit(dict_src$ID_for_merge, ", "))))
  name_DATE = dict_src$DATE_for_merge[grep(timeline_file, dict_src$file)]
  name_DATE = ifelse(name_DATE == "", NA, name_DATE)
  
  if (DATE_type == "Date") {
    key_DATE = eval(as.name(timeline_file)) 
    
    if (!is.na(name_DATE)) {
      key_DATE = key_DATE %>%
        select(any_of(c(name_ID, name_DATE))) %>%
        mutate(across(any_of(name_ID), as.character),across(any_of(name_DATE), function(x) {
          case_when(
            str_detect(x, "\\d{1,2}/\\d{1,2}/\\d{4}") ~ dmy(x, quiet = TRUE),
            str_detect(x, "\\d{4}-\\d{1,2}-\\d{1,2}") ~ ymd(x, quiet = TRUE),
            TRUE ~ as.Date(NA)
          )
        })) %>%
        na.omit() %>% 
        group_by(across(any_of(name_ID))) %>%
        arrange(!!as.name(name_DATE), .by_group = T) %>%
        mutate(date_left = (!!as.name(name_DATE) - lag(!!as.name(name_DATE))) / 2,
               date_right = lead(date_left))
    }
    
    else if(is.na(name_DATE)) {
      key_DATE = key_DATE %>%
        select(any_of(c(name_ID))) %>%
        mutate(across(any_of(name_ID), as.character)) %>% 
        na.omit()
      
      
    }
  } else if (DATE_type == "Number") {
    key_DATE = eval(as.name(timeline_file)) %>%
      mutate(across(any_of(name_ID), as.character)) %>% 
      distinct()
    
    
    if (!is.na(name_DATE)) {
      key_DATE = key_DATE %>%
        select(any_of(c(name_ID, name_DATE))) %>%
        mutate(across(everything(), as.character)) %>% 
        distinct()
    }
  } else {
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
get_window_bound = suppressWarnings(function(original_date,
                            bound_date,
                            is_left,
                            ovlp,
                            window_len) {
  bound_date <- as.numeric(bound_date)
  
  
  bound_date[is.na(bound_date)] = as.numeric(window_len / 2)
  if(ovlp) {
    return(original_date - is_left * window_len / 2)
  }
  else {
    
    result = ifelse(as.numeric(bound_date) >= as.numeric(window_len / 2),
           original_date - is_left * as.numeric(window_len / 2),
           original_date - is_left * as.numeric(bound_date))
    
    return (result)
    
    
  }
})





plot_files <- function(path, FILE_pattern = "\\.xlsx$|\\.xls$|\\.csv$", dict_src,study_type,date_type) {
  
  if(!study_type %in% c("BIOCARD", "ADNI")) {
    stop("Invalid study_type. Please enter 'BIOCARD' or 'ADNI'.")
  }
  
  # Ensure that date_type is either "Date" or "Number"
  if(!date_type %in% c("Date", "Number")) {
    stop("Invalid date_type. Please enter 'Date' or 'Number'.")
  }
  
  
  files_list <- list.files(path, pattern = FILE_pattern)
  
  # Load each file into the global environment
  for (file_name in files_list) {
    dat <- suppressWarnings(read_by_type(file_name, path))
    f_name <- gsub(FILE_pattern, "", file_name) # Clean file name
    assign(f_name, dat, envir = .GlobalEnv)
  }
  
  # Initialize an empty dataframe for the results
  combined_data <- data.frame(ID = character(), DATE = character(), FILE = character())
  
  dat_list <- dict_src$file
  
  for (data in dat_list) {
    idx <- grep(data, dict_src$file)
    if (length(idx) > 0) {
      ID_col <- dict_src$ID_for_merge[idx]
      DATE_col <- dict_src$DATE_for_merge[idx]
      
      dat_name <- gsub(FILE_pattern, "", data)
      dat_tem <- get(dat_name, envir = .GlobalEnv)
      
      if (ID_col %in% names(dat_tem) && DATE_col %in% names(dat_tem) && date_type == "Number" && study_type == "ADNI"){
        
        dat_tem <- suppressWarnings(dat_tem %>% 
          mutate(!!as.name(ID_col) := as.character(.[[ID_col]]),
                 !!as.name(DATE_col) := factor(.[[DATE_col]],levels = c("bl", "sc","m06", "m12", "uns1", "m36", "m18", "m24", "m48", "m60", "m72",
                                                                           "m84", "m96", "m108", "m120", "m132", "m54", "m66", "m78", "m126",
                                                                          "m42", "m138", "m144", "m90", "m150", "m102", "m156", "m162", 
                                                                          "m30", "m168", "m114", "m174", "m180", "m186", "m192", "m198", "m204"),
                                                  ordered = TRUE),
                 FILE = as.character(data)) %>% 
          select(ID = !!as.name(ID_col), DATE = !!as.name(DATE_col), FILE))
        combined_data <- rbind(combined_data, dat_tem)
        combined_data$DATE <- as.character(combined_data$DATE)
        
        
      }
      
      if (ID_col %in% names(dat_tem) && DATE_col %in% names(dat_tem) && date_type == "Number" && study_type == "BIOCARD"){
        
        dat_tem <- suppressWarnings(dat_tem %>% 
                                      mutate(!!as.name(ID_col) := as.character(.[[ID_col]]),
                                             !!as.name(DATE_col) := factor(.[[DATE_col]],levels = c("1", "2", "3", "4", "5", "6", "7","8", "9", "10", "101", "102", "103", "104", "105", "106", "107", "108", "109", "110", "111", "112"),
                                                                           ordered = TRUE),
                                             FILE = as.character(data)) %>% 
                                      select(ID = !!as.name(ID_col), DATE = !!as.name(DATE_col), FILE))
        combined_data <- rbind(combined_data, dat_tem)
        combined_data$DATE <- as.character(combined_data$DATE)
        
        
      }
      
      
      else if (ID_col %in% names(dat_tem) && DATE_col %in% names(dat_tem) && date_type == "Date") {
        dat_tem <- dat_tem %>%
          mutate(!!as.name(ID_col) := as.character(.[[ID_col]]),
                 !!as.name(DATE_col) := case_when(
                   stringr::str_detect(.[[DATE_col]], "\\d{1,2}/\\d{1,2}/\\d{4}") ~ dmy(.[[DATE_col]], quiet = TRUE),
                   stringr::str_detect(.[[DATE_col]], "\\d{4}-\\d{1,2}-\\d{1,2}") ~ ymd(.[[DATE_col]], quiet = TRUE),
                   TRUE ~ as.Date(NA)
                 ),
                 FILE = as.character(data)) %>%
          select(ID = !!as.name(ID_col), DATE = !!as.name(DATE_col), FILE)
        
        # Combine with the previously collected data
        combined_data <- rbind(combined_data, dat_tem)
        combined_data$DATE <- as.Date(combined_data$DATE)
        
      }
    }
    
  }
  
  # Start plotting
  
 
  
  combined_data <- combined_data  %>%  distinct() %>% drop_na()
    
  
  combined_data <- combined_data %>% mutate(color = as.factor(FILE))
  
  color_palette <- RColorBrewer::brewer.pal(length(unique(combined_data$color)), "Set1")
  
  color_map <- setNames(color_palette, levels(combined_data$color))
  
  combined_data <- combined_data %>% mutate(color_value = color_map[color])
  
  combined_data <- combined_data %>% mutate(hover_text = paste("File:", FILE))
  
  combined_data$ID <- as.numeric(combined_data$ID)
  
  combined_data <- combined_data[order(combined_data$ID), ]
  
  setDT(combined_data)
  
  



  # p <- plot_ly(data = combined_data, x = ~DATE, y = ~ID, type = 'scatter', mode = 'markers',
  #              hoverinfo = 'text', # Display the hover text when hovering
  #              text = ~hover_text, # Set the text that appears on hover
  #              marker = list(size = 10, opacity = 0.6, color = ~color_value))

  unique_types <- unique(combined_data$FILE)

  fig <- plot_ly()

if (date_type == "Date" && study_type == "ADNI"){
  
  for (type in unique_types) {
    fig <- fig %>% add_trace(
      data = combined_data[combined_data$FILE == type, ],
      x = ~DATE,
      y = ~ID,
      hoverinfo ="text",
      name = type,
      type = 'scattergl',
      text = ~hover_text,
      mode = 'markers',
      visible = TRUE,
      marker = list(size = 5, opacity = 0.6, color = ~color_value)# Set visible to TRUE
    )
  }
  
  adni_phases <- list(
    list(name = "ADNI 1", start = "2004-10-01", end = "2009-09-30", color = 'yellow'),
    list(name = "ADNI GO", start = "2009-10-01", end = "2011-09-30", color = 'purple'),
    list(name = "ADNI 2", start = "2011-10-01", end = "2016-09-30", color = 'green'),
    list(name = "ADNI 3", start = "2016-10-01", end = "2022-12-31", color = 'red')
  )
  
  
  fig <- layout(fig, title = 'Highlighting with Rectangles',
                shapes = )
  
  for(phase in adni_phases) {
    fig <- fig %>% layout(
      shapes = list(
        list(type = "rect",
             fillcolor = "blue", line = list(color = "blue"), opacity = 0.3,
             x0 = "1980-01-01", x1 = "1985-01-01", xref = "x",
             y0 = 4, y1 = 12.5, yref = "y"),
        list(type = "rect",
             fillcolor = "blue", line = list(color = "blue"), opacity = 0.2,
             x0 = "2000-01-01", x1 = "2005-01-01", xref = "x",
             y0 = 4, y1 = 12.5, yref = "y"))
    )
  }
  
  
  # # Adding annotations for ADNI phases
  # for(phase in adni_phases) {
  #   fig <- fig %>% layout(
  #     annotations = list(
  #       list(
  #         x = mean(c(as.numeric(as.Date(phase$start)), as.numeric(as.Date(phase$end)))),
  #         y = 1.05,
  #         xref = 'x',
  #         yref = 'paper',
  #         text = phase$name,
  #         showarrow = FALSE,
  #         font = list(
  #           family = "Arial, sans-serif",
  #           size = 16,
  #           color = "black"
  #         )
  #       )
  #     )
  #   )
  # }
  
  
  
}
  
  
  
  if (date_type == "Date" && study_type == "BIOCARD"){
    
    for (type in unique_types) {
      fig <- fig %>% add_trace(
        data = combined_data[combined_data$FILE == type, ],
        x = ~DATE,
        y = ~ID,
        hoverinfo ="text",
        name = type,
        type = 'scattergl',
        text = ~hover_text,
        mode = 'markers',
        visible = TRUE,
        marker = list(size = 5, opacity = 0.6, color = ~color_value)# Set visible to TRUE
      )
    }
    
    biocard_phases <- list(
      list(name = "NIH Phase", start = "1995-01-01", end = "2005-12-31", color = 'blue'),
      list(name = "Interruption", start = "2006-01-01", end = "2008-12-31", color = 'grey'),
      list(name = "JHU Phase", start = "2009-01-01", end = "2014-12-31", color = 'purple') 
    )
    
    
    
    for(phase in biocard_phases) {
      fig <- fig %>% layout(
        shapes = list(
          list(
            type = 'rect',
            # Convert to numeric Date for x-axis compatibility
            x0 = as.numeric(as.Date(phase$start)),
            x1 = as.numeric(as.Date(phase$end)),
            y0 = 0,
            y1 = 1,
            xref = 'x',
            yref = 'paper',
            fillcolor = phase$color,
            opacity = 0.3,
            line = list(
              width = 0
            ),
            layer = "below"
          )
        )
      )
    }
    
    
    # Adding annotations for ADNI phases
    for(phase in biocard_phases) {
      fig <- fig %>% layout(
        annotations = list(
          list(
            x = mean(c(as.numeric(as.Date(phase$start)), as.numeric(as.Date(phase$end)))),
            y = 1.05,
            xref = 'x',
            yref = 'paper',
            text = phase$name,
            showarrow = FALSE,
            font = list(
              family = "Arial, sans-serif",
              size = 16,
              color = "black"
            )
          )
        )
      )
    }
    
    
    
  }
  
  
  else if (date_type == "Number"){
    
    for (type in unique_types) {
      fig <- fig %>% add_trace(
        data = combined_data[combined_data$FILE == type, ],
        x = ~DATE,
        y = ~ID,
        hoverinfo ="text",
        name = type,
        type = 'scattergl',
        text = ~hover_text,
        mode = 'markers',
        visible = TRUE,
        marker = list(size = 5, opacity = 0.6, color = ~color_value)# Set visible to TRUE
      )
    }
    
  }


  # Generate dropdown items based on unique types
  buttons <- lapply(seq_along(unique_types), function(i) {
    list(
      method = "restyle",
      args = list("visible", lapply(seq_along(unique_types), function(j) i == j)),
      label = unique_types[i]
    )
  })

  # Add an "All" button to the dropdown
  all_button <- list(
    method = "restyle",
    args = list("visible", rep(TRUE, length(unique_types))),
    label = "All"
  )

  # Ensure "All" is the first button, making it the default selection
  buttons <- c(list(all_button), buttons)

  fig <- fig %>% layout(
    title = NULL,
    xaxis = list(title = "TIME"),
    yaxis = list(title = "ID"),
    showlegend = FALSE,
    updatemenus = list(
      list(
        buttons = buttons,
        direction = "down",
        showlegend = FALSE,
        pad = list(r = 10, t = 10),
        showactive = TRUE,
        x = 0.1,
        xanchor = "left",
        y = 1.1,
        yanchor = "top"
      )
    )
  )

  # Add a custom hover event to highlight the group (FILE)
  js_code <-  "
function(el) {
  var plotlyGraph = document.getElementById(el.id);

  plotlyGraph.on('plotly_hover', function(data) {
    // Find the index of the hovered data point
    var hoverIndex = data.points[0].pointIndex;
    console.log('Hover index data:', hoverIndex);

    // Get the trace and group information for the hovered data point
    var traceIndex = data.points[0].curveNumber;
    var groupValue = data.points[0].fullData.marker.color[hoverIndex];
    console.log('groupValue:', groupValue);

    // Create an array to set opacities
    var opacities = new Array(data.points[0].fullData.x.length).fill(0.1); // Start with all opacities at 0.1

    // Set the opacity of the points in the same group as the hovered point to 1 (highlight)
    data.points[0].fullData.x.forEach(function(_, i) {
      if(data.points[0].fullData.marker.color[i] === groupValue) {
        opacities[i] = 1.5;
      }
    });

    // Restyle the plot with the updated opacities
    Plotly.restyle(el.id, {'marker.opacity': [opacities]}, [traceIndex]);
  });

  plotlyGraph.on('plotly_unhover', function(data) {
    // Reset the opacity for all points when not hovering
    var resetOpacities = new Array(data.points[0].fullData.x.length).fill(0.1); // Reset all opacities to 0.1
    Plotly.restyle(el.id, {'marker.opacity': [resetOpacities]});
  });
}
"

# Customize the layout if desired


fig <- fig %>% onRender(js_code)



return(fig)
}

