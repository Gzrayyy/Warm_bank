###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 1:                             ###
###                          TRANSFORM TO GTFS                          ###
###                                                                     ###
###########################################################################
###########################################################################

# This section mainly talked about how to convert ATOC to GTFS

# Load packages -----------------------------------------------------------

  remotes::install_github("ITSleeds/UK2GTFS")
  library(UK2GTFS)
  library(tidyverse)
  library(sf)
  library(mapview)


# ATOC files to GTFS - (rail) ---------------------------------------------

  # 调用了atoc2gtfs函数，将ATOC格式的数据转换为GTFS格式。path_in = path_in定义了输入文件的路径
  # shapes = TRUE表示生成GTFS数据时包括地形数据，ncores = 14定义了计算过程中使用的核心数量。
  # ncores = 0 use max cores
  # Rail data(national rail timetable)
  path_in <- "data/atoc/ttis739.zip"
  # Convert atoc to gtfs
  # GTFS(General Transit Feed Specification) produced by Google
  # It contains agency.txt: 描述了运营公共交通系统的机构的信息。
  # stops.txt: 描述了公交站点的信息，如ID、名称、地理坐标等。
  # routes.txt: 描述了运营的路线，如路线ID、路线名称、路线类型（公交、地铁、火车等）。
  # trips.txt: 描述了路线的具体行程，如行程ID、行程名称、行程所属的路线、行程所使用的日历等。
  # stop_times.txt: 描述了每个行程在每个站点的到达和离开时间。
  # calendar.txt 或 calendar_dates.txt: 描述了行程的运行时间表，比如哪些行程在周几运行，或特定日期的行程运行情况。
  # shapes.txt（可选）: 描述了行程的具体路线轨迹，如经过的地理坐标点。
  ttis739 <- atoc2gtfs(path_in = path_in, shapes = TRUE, ncores = 14)
  
  ## Inspect output
  # Calendar
  # View preview of ttis739$calendar
  glimpse(ttis739$calendar)
  # convert a date from character format to date format and summeraise
  summary(parse_date(ttis739$calendar$start_date, format = "%Y%m%d"))
  summary(parse_date(ttis739$calendar$end_date, format = "%Y%m%d"))
  # plot
  barplot(table(ttis739$calendar$end_date))
  
  # Stops
  ttis739$stops %>%
    st_as_sf(coords = c('stop_lon', 'stop_lat'), crs = 4326) %>%
    mapview()
  
  # Check internal validity
  UK2GTFS::gtfs_validate_internal(ttis739)
  # Warning messages:
  # 1: In UK2GTFS::gtfs_validate_internal(ttis194) : NA values in stops
  # 2: In UK2GTFS::gtfs_validate_internal(ttis194) :
  #   Unknown stop_id in stop_times
  
  ## Force valid. This function does not fix problems it just removes them
  ttis739_gtfs <- UK2GTFS::gtfs_force_valid(ttis739)
  ## Compare original and valid
  # Find difference
  map2(ttis739, ttis739_gtfs, identical)
  # Stop times not included in GTFS version
  anti_join(ttis739$stop_times, ttis739_gtfs$stop_times)
  # trip_id arrival_time departure_time stop_id stop_sequence pickup_type drop_off_type
  # 1   12400     21:38:00       21:40:00 RESTSTN            12           0             0
  # 2   66575     16:45:00       16:49:00 SOHA491             4           0             0
  # 3   63914     12:45:00       12:49:00 SOHA491             4           0             0
  # 4   52802     31:45:00       32:37:00 WMBYEFR            17           0             0

  ## Write as GTFS
  UK2GTFS::gtfs_write(ttis739_gtfs, folder = 'data/atoc/', name = 'ttis739.gtfs')
  
  # Clean env.
  rm(list = ls())
  gc(reset = TRUE)

# Check GTFS ------------------------------------------------------------
  
  library(tidytransit)
  library(lubridate)
  
  # Read ATOC GTFS
  train_gtfs <- read_gtfs('data/atoc/ttis739.gtfs.zip')
  summary(train_gtfs)
  
  # Validation status
  atoc_valid <- attr(train_gtfs, "validation_result")
  write_csv(atoc_valid, 'data/atoc/ttis739_validation.csv')
  
  # Clean
  rm(train_gtfs)
  gc()
  
  
  # Read transxchange GTFS
  bus_gtfs <- read_gtfs('data/transxchange/itm_all.gtfs.zip', quiet = FALSE)
  summary(bus_gtfs)
  
  # Validation status
  bus_valid <- attr(bus_gtfs, "validation_result")
  write_csv(bus_valid, 'data/transxchange/itm_validation.csv')
  
  # Clean env.
  rm(list = ls())
  gc(reset = TRUE)
  
  
