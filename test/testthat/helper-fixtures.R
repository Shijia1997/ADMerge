make_test_dir_basic <- function() {
  td <- tempfile("admerge_test_")
  dir.create(td, recursive = TRUE)
  
  # Reference timeline: DXSUM
  dxsum <- data.frame(
    RID = c("1", "1", "2", "2", "3"),
    EXAMDATE = c("2020-01-01", "2021-01-01",
                 "2020-06-01", "2021-06-01",
                 "2020-03-01"),
    DX = c("CN", "MCI", "CN", "MCI", "CN"),
    stringsAsFactors = FALSE
  )
  
  # MRI longitudinal file
  mri <- data.frame(
    RID = c("1", "1", "2", "2", "3"),
    EXAMDATE = c("2020-01-08", "2021-01-10",
                 "2020-05-20", "2021-05-20",
                 "2020-03-12"),
    ST29SV = c(100, 95, 110, 100, 120),
    ST88SV = c(101, 96, 111, 101, 121),
    stringsAsFactors = FALSE
  )
  
  # Cognitive longitudinal file using VISDATE
  neurobat <- data.frame(
    RID = c("1", "1", "2", "2"),
    VISDATE = c("2020-01-03", "2021-01-05",
                "2020-06-05", "2021-06-05"),
    LIMMTOTAL = c(12, 11, 13, 10),
    stringsAsFactors = FALSE
  )
  
  # Cross-sectional demographics
  ptdemog <- data.frame(
    RID = c("1", "2", "3"),
    AGE = c(70, 72, 69),
    SEX = c("F", "M", "F"),
    stringsAsFactors = FALSE
  )
  
  write.csv(dxsum,    file.path(td, "DXSUM.csv"), row.names = FALSE)
  write.csv(mri,      file.path(td, "UCSFFSX43_1_go_2.csv"), row.names = FALSE)
  write.csv(neurobat, file.path(td, "NEUROBAT.csv"), row.names = FALSE)
  write.csv(ptdemog,  file.path(td, "PTDEMOG.csv"), row.names = FALSE)
  
  td
}

make_test_dir_overlap <- function() {
  td <- tempfile("admerge_overlap_")
  dir.create(td, recursive = TRUE)
  
  dxsum <- data.frame(
    RID = c("1", "1"),
    EXAMDATE = c("2020-01-01", "2020-07-01"),
    stringsAsFactors = FALSE
  )
  
  # One MRI scan sits between the two visits; with overlap it may be reused
  mri <- data.frame(
    RID = c("1"),
    EXAMDATE = c("2020-04-01"),
    ST29SV = 100,
    ST88SV = 101,
    stringsAsFactors = FALSE
  )
  
  write.csv(dxsum, file.path(td, "DXSUM.csv"), row.names = FALSE)
  write.csv(mri,   file.path(td, "UCSFFSX43_1_go_2.csv"), row.names = FALSE)
  
  td
}

make_test_dir_tie <- function() {
  td <- tempfile("admerge_tie_")
  dir.create(td, recursive = TRUE)
  
  dxsum <- data.frame(
    RID = c("1"),
    EXAMDATE = c("2020-01-11"),
    stringsAsFactors = FALSE
  )
  
  # Two MRI records equally far from timeline date: 2020-01-01 and 2020-01-21
  mri <- data.frame(
    RID = c("1", "1"),
    EXAMDATE = c("2020-01-01", "2020-01-21"),
    ST29SV = c(100, 200),
    ST88SV = c(101, 201),
    stringsAsFactors = FALSE
  )
  
  write.csv(dxsum, file.path(td, "DXSUM.csv"), row.names = FALSE)
  write.csv(mri,   file.path(td, "UCSFFSX43_1_go_2.csv"), row.names = FALSE)
  
  td
}