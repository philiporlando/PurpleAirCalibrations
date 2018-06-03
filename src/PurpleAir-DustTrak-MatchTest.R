# test message
# created by Philip Orlando @ Sustainable Atmopsheres Research Lab
# PI Dr. Linda George
# 2018-05-21
# Match Test in lab chamber (beer fermenter) of PurpleAirSD, and DustTrak

# load the necessary packages
if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}

# load necessary packages
p_load(readr
       ,ggplot2
       ,plyr
       ,dplyr
       ,broom
       ,reshape2
       ,tidyr
       ,stringr
       ,magrittr
       ,rlang
)


# create unanimous time resolution for all data (5 min needs clean nexts in order to work, debug later!)
time_resolution <- "1 min"

# creating a custom not-in function
'%!in%' <- function(x,y)!('%in%'(x,y))

# create read functions for each sensor type (SD vs non-SD)
read_purpleair <- function(file_path) {
  data <- read.csv(file_path)
  data <- select(data, -c(X, entry_id))
  file_name <- basename(file_path)
  name <- gsub("\\(.*$","",file_name)
  name <- trimws(name, which = c("right"))
  name <- str_replace_all(name," ","_")
  data <- data %>% 
    group_by(created_at = cut(as.POSIXct(created_at
                                         ,format = "%Y-%m-%d %H:%M:%S"
                                         ,tz = "GMT"
    )
    ,breaks = time_resolution)) %>%
    summarize_all(funs(mean))
  
  data$created_at <- as.POSIXct(data$created_at, tz = "GMT")
  data$created_at <- format(data$created_at, tz = "US/Pacific") %>%
    as.POSIXct(tz = "US/Pacific")
  data$name <- name
  return(data)
}

# file_path <- "./data/CorruptFiles/20180518_EC_FA_BC_B_B1_9D.csv"
read_purpleairSD <- function(file_path) {
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  data$UTCDateTime <- as.POSIXct(data$UTCDateTime
                                 ,format = "%Y/%m/%dT%H:%M:%S"
                                 ,tz = "GMT"
  ) 
  
  # filter out error-logs in data (may need to update this later)
  data <- data %>% na.omit()
  
  # this is needed for later!
  mac_address <- data$mac_address[1]
  data <- data %>% 
    group_by(UTCDateTime = cut(as.POSIXct(UTCDateTime
                                          ,format = "%Y-%m-%d %H:%M:%S"
                                          , tz = "GMT"
    )
    ,breaks = time_resolution)) %>%
    summarize_all(funs(mean))
  
  # group by can't average a character string
  data$mac_address <- str_replace_all(mac_address,":","_")
  
  ## holy time formatting batman!
  data$UTCDateTime <- as.POSIXct(data$UTCDateTime, tz = "GMT")
  data$UTCDateTime <- format(data$UTCDateTime, tz="US/Pacific") %>%
    as.POSIXct(tz = "US/Pacific")
  
  return(data)
}

## reads in one TSI DustTrak DRX 8533 file:
read_dtrak<-function(fpath){
  sdate<-read.csv(fpath, header=FALSE, nrow=1, skip=7)
  stime <-read.csv(fpath, header = FALSE, nrow=1, skip=6)  
  startDate<-strptime(paste(sdate$V2, stime$V2), "%m/%d/%Y %H:%M:%S", tz="US/Pacific")
  x<-read.csv(fpath, skip=37, stringsAsFactors = FALSE, header = FALSE)
  names(x)<-c("elapsedtime","pm1","pm2.5","pm4","pm10","total","alarms","errors")
  x$datetime<-x$elapsedtime+startDate
  x$datetime <- as.POSIXct(x$datetime, format = "%m/%d/%Y %H:%M:%S", tz = "US/Pacific")
  x<-x[,-c(1,7,8)]
  x<-x[,c(6,1,2,3,4,5)]
  x$pm1 <- x$pm1 * 1000
  x$pm2.5 <- x$pm2.5 * 1000
  x$pm4 <- x$pm4 * 1000
  x$pm10 <- x$pm10 * 1000
  x$total <- x$total * 1000
  x <- x %>% 
    group_by(datetime = cut(datetime, breaks = time_resolution)) %>%
    dplyr::summarize(pm1 = mean(pm1), pm2.5 = mean(pm2.5), pm4 = mean(pm4), 
                     pm10 = mean(pm10), total = mean(total))
  ## manage time zones (necessary when using PurpleAir sensors!)
  x$datetime <- as.POSIXct(x$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "US/Pacific")
  return(x)
  
}



# setting filepaths for each sensor type
#purpleair_path <- "./data/PurpleAir/"
purpleairSD_path <- "./data/PurpleAirSD/"
#purpleairSD_path <- "./data/AllFiles/" # for testing corrupt data with normal data
dtrak_path <- "./data/DustTrak/"


# pulling in a list of files within each directory
# purpleair_files <- list.files(path = purpleair_path, pattern = "\\.csv$",
#                               all.files=FALSE, full.names = TRUE,
#                               ignore.case = FALSE)


purpleairSD_files <- list.files(path = purpleairSD_path, pattern = "\\.csv$",
                                all.files=FALSE, full.names = TRUE,
                                ignore.case = FALSE)

dtrak_files <- list.files(path = dtrak_path, pattern = "\\.csv$",
                          all.files=FALSE, full.names = TRUE,
                          ignore.case = FALSE)

# pulling in the DustTrak data
dtrak <- ldply(dtrak_files, read_dtrak)


# creating separate file lists based on primary and secondary files
# primary_files <- grep(" Primary ", purpleair_files, value = TRUE) 
# secondary_files <- grep(" Secondary ", purpleair_files, value = TRUE)
# 
# # creating separate file lists based on sensor A versus sensor B
# primary_files_a <- grep(" B ", primary_files, value = TRUE, invert = TRUE) 
# primary_files_b <- grep(" B ", primary_files, value = TRUE) 
# 
# secondary_files_a <- grep(" B ", secondary_files, value = TRUE, invert = TRUE)
# secondary_files_b <- grep(" B ", secondary_files, value = TRUE)


# applying our generic read function to the SD files
purpleairSD <- ldply(purpleairSD_files, read_purpleairSD) # warning message is being handled within the new read function

sd_sub <- select(purpleairSD, -c(
                                 firmware_ver
                                 ,hardware
                                 ,current_temp_f
                                 ,current_humidity
                                 ,current_dewpoint_f
                                 ,pressure
                                 ,adc
                                 ,mem
                                 ,rssi
                                 ,uptime
))

# convert to long format
sd_tidy <- sd_sub %>%
  gather(pollutant, value, -c(UTCDateTime, mac_address)) ## fix this accordingly!


split <- str_split(sd_tidy$pollutant, pattern = "_b", simplify = TRUE)
tail(split)

# include mac_address in sensor name in future:
sd_tidy$sensor <- ifelse(grepl("_b$", sd_tidy$pollutant), "SD_B", "SD_A")

# replace mac_address with Sensor Name, or Sensor ID via lookup table in the future?
sd_tidy$sensor_id <- ifelse(grepl("_b$", sd_tidy$pollutant), paste0(sd_tidy$mac_address, "_SD_B"), paste0(sd_tidy$mac_address, "_SD_A"))
sd_tidy$pollutant <- split[,1] # is this doing anything?

# rename date column
colnames(sd_tidy)[colnames(sd_tidy) == "UTCDateTime"] <- "datetime"
head(sd_tidy$datetime)
sd_tidy <- select(sd_tidy, -c(mac_address, sensor))

# match the column order of the other 
sd_tidy <- sd_tidy %>% 
  select(datetime, sensor_id, pollutant, value)


# # applying our generic read functions to each list of files separately! (tricky)
# primary_a <- ldply(primary_files_a, read_purpleair)
# primary_b <- ldply(primary_files_b, read_purpleair)
# 
# secondary_a <- ldply(secondary_files_a, read_purpleair)
# secondary_b <- ldply(secondary_files_b, read_purpleair)
# 
# # assign better column names for number density data
# colnames(secondary_a) <- c("created_at"
#                            ,"PNC0.3um.dl"
#                            ,"PNC0.5um.dl"
#                            ,"PNC1.0um.dl"
#                            ,"PNC2.5um.dl"
#                            ,"PNC5.0um.dl"
#                            ,"PNC10.0um.dl"
#                            ,"PM1.0_CF_1_ug.m3"
#                            ,"PM10_CF_1_ug.m3"
#                            ,"name")
# 
# colnames(secondary_b) <- c("created_at"
#                            ,"PNC0.3um.dl"
#                            ,"PNC0.5um.dl"
#                            ,"PNC1.0um.dl"
#                            ,"PNC2.5um.dl"
#                            ,"PNC5.0um.dl"
#                            ,"PNC10.0um.dl"
#                            ,"PM1.0_CF_1_ug.m3"
#                            ,"PM10_CF_1_ug.m3"
#                            ,"name")
# 
# 
# # joining primary and secondary data for each sensor (A vs B)
# purpleair_a <- inner_join(primary_a, secondary_a, by = c("created_at", "name"))
# purpleair_b <- inner_join(primary_b, secondary_b, by = c("created_at", "name"))
# 
# 
# # creating a "sensor" vector
# purpleair_a$sensor <- "A"
# purpleair_b$sensor <- "B"
# #purpleair_a$sensor_id <- paste0(purpleair$name, "_", purpleair$sensor)
# 
# # creating long data for our non-SD purpleair data
# purpleair <- bind_rows(purpleair_a, purpleair_b)
# 
# # 'B' already included in sensor name, therefore we shouldn't be pasting it together twice!
# purpleair$sensor_id <- ifelse(purpleair$sensor == "A", paste0(purpleair$name, "_", purpleair$sensor), purpleair$name)
# 
# head(purpleair$created_at)
# 
# 
# # fix this, NEED sensor ID or mac_address appended as a new vector somehow!!!
# purple_sub <- select(purpleair, -c(UptimeMinutes
#                                    ,RSSI_dbm
#                                    ,Temperature_F
#                                    ,Humidity_.
# ))
# 
# ## fix this to match DustTrak fields?
# colnames(purple_sub) <- c("datetime"
#                           ,"pm1_0_atm"
#                           ,"pm2_5_atm"
#                           ,"pm10_0_atm"
#                           ,"pm2_5_cf_1"
#                           ,"name"
#                           ,"p_0_3_um"
#                           ,"p_0_5_um"
#                           ,"p_1_0_um"
#                           ,"p_2_5_um"
#                           ,"p_5_0_um"
#                           ,"p_10_0_um"
#                           ,"pm1_0_cf_1"
#                           ,"pm10_0_cf_1"
#                           ,"sensor"
#                           ,"sensor_id")


colnames(dtrak) <- c("datetime"
                     ,"pm1_0_atm"
                     ,"pm2_5_atm"
                     ,"pm4_0_atm"
                     ,"pm10_0_atm"
                     ,"pm_total"
)


# # fix this, NEED sensor ID or mac_address appended as a new vector somehow!!!
# purple_sub <- select(purple_sub, -c(sensor, name))


# # create duplicate pm reference fields to match purpleair format
dtrak$pm1_0_cf_1 <- dtrak$pm1_0_atm
dtrak$pm2_5_cf_1 <- dtrak$pm2_5_atm
dtrak$pm10_0_cf_1 <- dtrak$pm10_0_atm
dtrak$sensor <- "DustTrak"
dtrak$sensor_id <- "DustTrak"
# 
# remove non-matching fields from dusttrak data
dtrak <- select(dtrak, -c(pm4_0_atm, pm_total, sensor))
# 
dtrak_tidy <- dtrak %>%
  gather(pollutant, value, -c(datetime, sensor_id))
# 
# 
# pa_tidy <- purple_sub %>%
#   gather(pollutant, value, -c(datetime, sensor_id))


#pm_tidy <- bind_rows(pa_tidy, sd_tidy, dtrak_tidy)
pm_tidy <- bind_rows(sd_tidy, dtrak_tidy)
#pm_tidy$datetime <- as.POSIXct(pm_tidy$datetime)


write.csv(pm_tidy, paste0("./data/Tidy/", format(Sys.time(), "%Y-%m-%d"), "_pm_tidy.csv"), row.names = FALSE)

dtrak_ref <- select(dtrak_tidy, c(datetime, pollutant, value))
names(dtrak_ref) <- c("datetime", "pollutant", "DustTrak")
head(dtrak_ref)

df <- inner_join(pm_tidy
                 ,dtrak_ref
                 ,by = c("datetime", "pollutant"))

head(df)
str(df)
summary(df)

head(unique(df$datetime))
tail(unique(df$datetime))

##################################### set start and end time!!! ###############################################################
# df <- df %>%
#   filter(as.POSIXct(datetime) >= as.POSIXct("2018-05-18 16:30") & as.POSIXct(datetime) <= as.POSIXct("2018-05-18 20:30"))
############################################################### ###############################################################


## EDA time series to determine cut points for start and end time

# ggplot(df) +
#   geom_point(aes(x = datetime, y = value, colour = sensor_id), alpha = 0.1) + 
#   facet_wrap(~pollutant) +
#   geom_point(aes(x = datetime, y = DustTrak), alpha = 0.1) + 
#   theme_bw()
# 
# ggplot(filter(df, sensor_id == "ec_fa_bc_b_b1_9d_SD_A")) +
#   geom_point(aes(x = datetime, y = value, colour = sensor_id), alpha = 0.1) + 
#   facet_wrap(~pollutant) +
#   geom_point(aes(x = datetime, y = DustTrak), alpha = 0.1) + 
#   theme_bw()
# 
# 
# ggplot(filter(df, DustTrak < 200 & DustTrak >= 0)) +
#   geom_point(aes(x=DustTrak, y=value, colour=sensor_id)) +
#   #geom_smooth(aes(DustTrak, value, colour=sensor_id), method=lm, se=FALSE) +
#   facet_wrap(~pollutant, scales="free_y") + 
#   theme_bw()
# 
# ggplot(filter(df, pollutant %!in% c("p_0_3_um", "p_0_5_um", "p_1_0_um", "p_2_5_um", "p_5_0_um", "p_10_0_um"))) +
#   geom_point(aes(x=DustTrak, y=value, colour=sensor_id)) +
#   geom_smooth(aes(DustTrak, value, colour=sensor_id), method=lm, se=FALSE) +
#   facet_wrap(~pollutant, scales="free_y") + 
#   theme_bw()



# ggplot(df_sensor, aes(datetime, value, color = pollutant)) + 
#   geom_point()
# 
# 
# ggplot(df_pollutant, aes(datetime, value, color = pollutant)) + 
#   geom_point()

# ggregression <- function (fit) {
#   
#   ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
#     geom_point() +
#     geom_smooth(method = "lm", col = "red", level = 0.95) + ## "lm" or "loess" fit!
#     geom_abline(intercept = 0, slope = 1, linetype = 2, color = "firebrick") +
#     theme_bw() + 
#     xlab(names(fit$model)[2]) + 
#     ylab(names(fit$model)[1]) +
#     theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
#           plot.subtitle = element_text(hjust = 0.5, size=12, face = "bold"), legend.position = "none",
#           axis.text = element_text(size=rel(1.0), face = "bold", colour = "black"),
#           axis.title = element_text(size=15, face = "bold")) +  
#     labs(title = paste0(pollutant, " ", sensor, " ", names(fit$model[1]), " ~ ", names(fit$model)[2]),
#          subtitle = paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 4),
#                           "Intercept =",signif(fit$coef[[1]], 2), 
#                           " Slope =",signif(fit$coef[[2]], 2), 
#                           " P =",signif(summary(fit)$coef[2,4], 3)))
# }


# for testing purposes:
# sensor <- "68_c6_3a_8e_5b_a9_SD_A"
# sensor <- "DustTrak"
# pollutant <- "pm1_0_cf_1"
# start_time <- "2018-05-21 16:24"
# end_time <- "2018-05-21 19:45"


# list of start times from log books
start_times <- c("2018-05-18 14:00"
                 ,"2018-05-21 16:24"
                 ,"2018-05-22 11:16"
                 ,"2018-05-24 10:48"
                 ,"2018-05-28 13:01"
                 ,"2018-05-30 15:15"
                 ,"2018-05-30 19:20"
                 ,"2018-05-31 10:30"
                 ,"2018-05-31 14:00"
                 ,"2018-05-31 17:45"
                 ,"2018-06-02 11:00"
                 ,"2018-06-02 17:30"
                 ,"2018-06-02 23:05"
                 )
# list of end times from log books
end_times <- c("2018-05-18 19:45"
               ,"2018-05-21 19:45"
               ,"2018-05-22 17:45"
               ,"2018-05-24 16:00"
               ,"2018-05-28 18:00"
               ,"2018-05-30 19:15"
               ,"2018-05-30 23:00"
               ,"2018-05-31 13:45"
               ,"2018-05-31 17:30"
               ,"2018-06-01 03:30"
               ,"2018-06-02 17:30"
               ,"2018-06-02 17:50"
               ,"2018-06-03 06:05" # check these end times for 06/02 & 06/03
               )

# concat start and end times into single dataframe for looping
sample_period <- data.frame(start_times, end_times)

# create output df to capture our results
output_names <- c("start_time"
                  ,"end_time"
                  ,"sensor"
                  ,"pollutant"
                  ,"n_relative"
                  ,"n_observation"
                  ,"r_squared"
                  ,"slope"
                  ,"intercept"
                  ,"p_value"
                  )

output_df <- data.frame(matrix(ncol = length(output_names), nrow = 0))
colnames(output_df) <- output_names

# create list of sensors and pollutants to iterate through
sensors <- unique(df$sensor_id)
pollutants <- unique(df$pollutant)



## remove existing output file for today if exists
old_file <- paste0("./data/Output/", format(Sys.time(), "%Y-%m-%d"), "-PurpleAirSummaryTable.txt")

if(file.exists(old_file)) {
  print(paste0("Deleting file: ", basename(old_file)))
  file.remove(old_file)
} 

## filter dataframe for specific test period
for (time in 1:nrow(sample_period)) {
  
  # extract start and end times from our sample_period df
  start_time <- sample_period[time, "start_times"]
  end_time <- sample_period[time, "end_times"]
  #print(paste(start_time, end_time)) ## for testing
  
  # subset our test interval
  test <- df %>% 
    filter(as.POSIXct(datetime) >= as.POSIXct(start_time) & as.POSIXct(datetime) <= as.POSIXct(end_time))
  
  if (nrow(test) <= 0) {
    print(paste("Obj. 'test' is null"))
    next
  }
  
  
  # iterate through each sensor 
  for(sensor in unique(df$sensor_id)) {
    
    if (is.null(sensor)) {
      print(paste("Obj. 'sensor' is null", sensor))
      next
    }
    
    test_sensor <- subset(test, sensor_id == sensor)
    
    if (nrow(test_sensor) <= 0) {
      print(paste("Obj. 'test_sensor' is null"))
      next
    }
    
    # iterate through each pollutant
    for(species in unique(test_sensor$pollutant)) {
      
      if (is.null(species)) {
        print(paste("Obj. 'species' is null", species))
        next
      }
      
      
      # change this as needed!
      upper_limit <- 150 # apply conditionals for specific pm categories!
      lower_limit <- 0 # omit negative values
      
      test_pollutant <- subset(test_sensor, pollutant == species)
      
      if (nrow(test_pollutant) <= 0) {
        print(paste("Obj. 'test_pollutant' is null"))
        next
      }
      
      df_mod <- subset(test_pollutant, DustTrak <= upper_limit & DustTrak >= lower_limit)
      
      if (nrow(df_mod) <= 0) {
        print(paste("Obj. 'df_mod' is null"))
        next
      }
      
      # determine number of observations, and relative_n
      n_value <- df_mod$value %>% na.omit() %>% length() 
      n_dtrak <- df_mod$DustTrak %>% na.omit() %>% length()
      n_relative <- n_value/n_dtrak
      
      # define our regression plotting function
      ggregression <- function (fit) {
        
        ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
          geom_point() +
          geom_smooth(method = "lm", col = "red", level = 0.95, se = FALSE) + ## "lm" or "loess" fit!
          geom_abline(intercept = 0, slope = 1, linetype = 2, color = "firebrick") +
          theme_bw() + 
          xlab(names(fit$model)[2]) + 
          scale_x_continuous(limits = c(lower_limit, upper_limit)) +
          scale_y_continuous(limits = c(lower_limit, upper_limit)) +
          #ylab(expression(df_mod$pollutant~mu*g*m^-3)) +
          #ylab(as.character(unique(df_mod$pollutant))) + 
          xlab(expression(~DustTrak~mu*g*m^-3)) + 
          ylab(substitute(paste(foo, " ", mu, "", g, "", m^-3), list(foo = species))) + 
          #xlab(substitute(paste(foo, " ", mu, "", g, "", m^-3), "DustTrak")) + 
          theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
                plot.subtitle = element_text(hjust = 0.5, size=12, face = "bold"), legend.position = "none",
                axis.text = element_text(size=rel(1.0), face = "bold", colour = "black"),
                axis.title = element_text(size=15, face = "bold")) +  
          labs(title = paste0(sensor, " & ", names(fit$model)[2], " ", as.Date(start_time)),
               subtitle = paste("Adj R2 = ", signif(summary(fit)$adj.r.squared, 4),
                                "Intercept =",signif(fit$coef[[1]], 2), 
                                " Slope =",signif(fit$coef[[2]], 2), 
                                " P =",signif(summary(fit)$coef[2,4], 3)))
      }
      
      
    # capturing our regression model output and variables of interest
    ## try({
      
      print(paste("trying", start_time, end_time, sensor, species))
      mod <- lm(value~DustTrak, data = df_mod)
      r_squared <- signif(summary(mod)$r.squared, 4)
      slope <- signif(mod$coefficients[[2]], 2)
      intercept <- signif(mod$coefficients[[1]], 2)
      p_value <- signif(summary(mod)$coef[2,4], 3)
      
      # create new row for our output df
      new_row <- list(start_time, end_time, sensor, species, r_squared, slope, intercept, p_value)
      
      # append new row to output_df 
      output_df <- rbind(output_df
                         ,data.frame(
                           start_time = start_time
                           ,end_time = end_time
                           ,sensor = sensor
                           ,pollutant = species
                           ,n_relative = n_relative
                           ,n_observation = n_value
                           ,r_squared = r_squared
                           ,slope = slope
                           ,intercept = intercept
                           ,p_value = p_value
                         ))
      
      ## check if our output file already exists for today's date
      txt_path <- paste0("./data/Output/", format(Sys.time(), "%Y-%m-%d"), "-PurpleAirSummaryTable.txt")
      if(!file.exists(txt_path)) {
        
        
        print(paste0("Creating file: ", basename(txt_path)))
        write.table(output_df
                    ,txt_path
                    ,row.names = FALSE
                    ,col.names = TRUE
                    ,quote = FALSE
                    ,sep = ",")
        
      } else {
        
        print(paste0("Appending file: ", basename(txt_path)))
        write.table(output_df
                    ,txt_path
                    ,row.names = FALSE
                    ,append = TRUE # append if already exists
                    ,col.names = FALSE
                    ,quote = FALSE # makes reading data a challenge if TRUE...
                    ,sep =  ",")
        
      }
      
      
      # plotting our regression results
      mod_plot <- ggregression(mod)
      plot(mod_plot)
      
      Sys.sleep(1) # catch a glimpse of each plot
      
      # only save the nice looking figures
      if (r_squared >= 0.90 & slope >= 0.7 & slope <= 1.3) {
        
        print(paste("Saving plot for", start_time, end_time, sensor, species))
        
        file_name <- paste0("./figures/", start_time, "-", end_time, "_",  sensor, "_", species, "_UDL", upper_limit, ".png")
        file_name <- str_replace_all(file_name, " ", "_")
                            
        # ggsave is really slow at this DPI
        try(ggsave(filename = file_name,
               plot = mod_plot,
               scale = 1,
               width = 16,
               height = 10,
               units = "in",
               dpi = 400))
        Sys.sleep(1) # is R tripping over itself?
        
        
        
      }
      
      

      
      
    ##}) ## end of try block

      
    }
    
  }
  
  
} 

# read in output table
results <- read.csv(txt_path)

# filter atm values, and strong correlations with good slopes
results %>%
  filter(pollutant %in% c("pm1_0_atm", "pm2_5_atm", "pm10_0_atm")) %>%
  filter(r_squared > 0.9) %>%
  filter(slope <= 1.3 & slope >= 0.7) %>%
  filter(sensor != "DustTrak") %>%
  unique() %>% # why are there duplicate rows in my df here?
  arrange(sensor, pollutant, desc(r_squared)) -> top

sort_name <- tools::file_path_sans_ext(basename(txt_path))
write.csv(top, paste0(dirname(txt_path), "/", sort_name, "Sorted.txt"), row.names = FALSE)

# write separate files for each sensor's results
for (id in unique(top$sensor)) {
  
  out <- filter(top, sensor == id)
  out %>% arrange(start_time, sensor, pollutant, desc(r_squared), desc(slope)) -> out
  write.csv(out
            ,file = paste0("./data/Output/", id, ".csv")
            ,row.names = FALSE
            )
  
}


## group by sensor ID

top %>% 
  select(-c(start_time, end_time, n_relative)) %>%
  group_by(sensor, pollutant) %>%
  summarise_all(funs(mean)) %>%
  arrange(sensor, pollutant) -> top_sensors

# round before saving (for printing on 1-page)
top_sensors$n_observation <- round(top_sensors$n_observation, digits = 2)
top_sensors$r_squared <- round(top_sensors$r_squared, digits = 4)
top_sensors$slope <- round(top_sensors$slope, digits = 2)
top_sensors$intercept <- round(top_sensors$intercept, digits = 2)
top_sensors$p_value <- round(top_sensors$p_value, digits = 4)



write.csv(top_sensors, "./data/Output/top_sensors.csv", row.names = FALSE)

 
top_unique_sensors <- as.data.frame(unique(top_sensors$sensor))
colnames(top_unique_sensors) <- c("sensor_id")

write.csv(top_unique_sensors, "./data/Output/top_unique_sensors.csv", row.names = FALSE)


## post EDA

results %>%
  filter(pollutant %in% c("pm1_0_atm", "pm2_5_atm", "pm10_0_atm")) %>%
  filter(as.POSIXct(start_times, format = "%Y-%m-%d") >= as.POSIXct("2018-05-30", format = "%Y-%m-%d"))


