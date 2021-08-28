# # Only Current Results file
# Rscript pass_fail_check_2.R app_config.csv transaction_config.csv 3pt.csv NA FALSE NA
# # Current Results file compared to n baseline Files
# Rscript pass_fail_check_2.R app_config.csv transaction_config.csv 3pt.csv 443ExitRegression.csv,444ExitRegression.csv FALSE NA
# # Combined n Current Results file compared to n baseline Files
# Rscript pass_fail_check_2.R app_config.csv transaction_config.csv 3pt.csv 443ExitRegression.csv,444ExitRegression.csv TRUE C:\dev\workspace\R\comparision\temp2\combined.html

#Install required Packages
list.of.packages <- c("dplyr","moments", "dbscan","fpc","readr","logger","Rcpp","rmarkdown","influxdbr","psych","devtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='https://cran.rstudio.com')
# Install latest reactable package from GIT
# devtools::install_github("glin/reactable")

suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(moments)))
suppressWarnings(suppressMessages(library(dbscan)))
suppressWarnings(suppressMessages(library(fpc)))
suppressWarnings(suppressMessages(library(psych)))
suppressWarnings(suppressMessages(library(logger)))
suppressWarnings(suppressMessages(library(readr)))
# suppressWarnings(suppressMessages(library(factoextra)))
# suppressWarnings(suppressMessages(library(dygraphs)))
suppressWarnings(suppressMessages(library(reactable)))
suppressWarnings(suppressMessages(library(rmarkdown)))
suppressWarnings(suppressMessages(library(influxdbr)))



log_threshold(DEBUG)

log_debug("Starting Program...")

# Command line arguments check
args <- commandArgs(trailingOnly=TRUE)
# if (length(args)!=7) {
#   stop("In sufficient args, required args: \n\t app_config.csv transaction_config.csv (current_result_file).(csv|jtl) (base_result_files).(csv|jtl) weighted_result weighted_result_File_path <fistNmin,LastNmin> \n\n Example:-\n\tRscript comparision_v5.R app_config.csv transaction_config.csv /abs_path/current.csv /abs_path/base.csv,/abs_path/base1.csv,/abs_path/base2.csv TRUE path/weighted_combined.html 10,5\n\n" , call.=FALSE)
# }

# base <- "444ExitRegression.csv"
# current <- "443ExitRegression.csv"
base <- "C:\\dev\\work\\rscripts\\JMeter\\pass_fail\\results_file\\3PTv2\\Aks1199_445-23_Aug19\\2021-08-07_00-52-31\\summary.csv,C:\\dev\\work\\rscripts\\JMeter\\pass_fail\\results_file\\3PTv2\\Aks1199_445-23_Aug19\\2021-08-07_10-03-03\\summary.csv,C:\\dev\\work\\rscripts\\JMeter\\pass_fail\\results_file\\3PTv2\\Aks1199_445-23_Aug19\\2021-08-07_14-43-08\\summary.csv"
current <- "C:\\dev\\work\\rscripts\\JMeter\\pass_fail\\results_file\\3PTv2\\PRF06ExitReg445-21Run1\\2021-07-10_17-35-12\\summary.csv"
config_file_app <- "app_config.csv"
config_file_transaction <- "transaction_config.csv"
algorithm <- "boxplot"
standalone_max_txn_error_pct <- 10
compare_max_txn_error_pct <- 10
req_successful_comparisons <- 3
throw_error_on_standalone_failure <- TRUE
throw_error_on_comparison_failure <- TRUE
continue_on_standalone_failure <- TRUE
enable_dbscan <- FALSE
temp_results_store <- "aggregated_results_store.csv"
temp_errors_store <- "error_results_store.csv"
is_archive_current_test <- FALSE
show_base_result_issues_summary <- TRUE
use_filename_as_identifier <- FALSE
weighted_comparision <- FALSE
weighted_comparision_file <- "combined.html"
removeFirstNMinutes <- 10
removeLastNMinutes <- 5
generate_vs_comparision_weighted <- FALSE
get_best_of_all_results_for_weighted_summary <- FALSE
number_of_result_file_to_be_merged_for_weighted_summary <- 4
copy_data_files_to_local_temp <- FALSE

# Create empty data frame to store final comparison and standalone result
final_results <- data.frame(
                    baseFilename=character(),
                    currentFilename=character(),
                    comparision_status=logical(),
                    err_pct=double(),
                    type=character()
)
#Create empty data frame for all results
test_results_markdown <- base::data.frame()
test_errors_markdown <- base::data.frame()
comparision_results_markdown <- base::data.frame()

#temporary data frames used when checking for best result of n weighted average
temp_curr_aggregated <- base::data.frame()
temp_base_aggregated <- base::data.frame()

#temp file name store
temp_result_filemap <- data.frame(
  filename=character(),
  temp_filename=character(),
  type=character(),
  result_no=character(),
  actual_file=character()
)

temp_results_store_best_result <- data.frame(
  label=character(),
  pass=double(),
  fail=double(),
  avg=double(),
  ci_95=double(),
  sd=double(),
  kr=double(),
  discard_pct=double(),
  throughput=double(),
  connect=double(),
  receivedBytesPerSec=double(),
  sentBytesPerSec=double(),
  combination=character()
)

base_result_no <- 1
current_result_no <- 1

#####################  Common Functions ########################


#load transaction config
load_config <- function(config_file_transaction,config_file_app){
  
  config <<-
    read.csv(config_file_transaction,blank.lines.skip=TRUE,header = TRUE)
  # config <<- read_delim(config_file_transaction, ",", col_names = TRUE, progress= FALSE, skip_empty_rows=TRUE)
  
  config[is.na(config)] <<- 0
  
  app_config <<-
    read.csv(config_file_app,blank.lines.skip=TRUE,header = TRUE)
  # app_config <<- read_delim(config_file_app, ",", col_names = TRUE, progress= FALSE, skip_empty_rows=TRUE)
  
  app_config[is.na(app_config)] <<- 0
}

load_commandLine_args <- function(){

  if(length(args)>0){
    
      config_file_app <<- as.character(args[1])
      config_file_transaction <<- as.character(args[2])
      
      removeMinute<-strsplit(as.character(args[3]), ",")[[1]]
      removeFirstNMinutes <<- as.double(removeMinute[1])
      removeLastNMinutes <<- as.double(removeMinute[2])
      
      current <<- as.character(args[4])
      
      if(length(args)>4) {
        base <<- as.character(args[5])  
      }else{
        base <<- "NA"
      }
      
      if(length(args)==7) {
        weighted_comparision <<- as.character(args[6])=="TRUE"
        weighted_comparision_file <<- as.character(args[7])  
      }
  }

  log_debug("\n")
  log_debug("\n")
  log_debug(paste("config_file_app: ",config_file_app),"\n")
  log_debug(paste("config_file_transaction: ",config_file_transaction),"\n")
  log_debug(paste("current: ",current),"\n")
  log_debug(paste("base: ",base),"\n")
  log_debug("\n")
  log_debug("\n")
}

#Load Old data
load_aggregated_result <- function() {
  
  #load Aggregated results
  old_results <- data.frame()
  #Load aggregated data temporary results file and check
  if (file.exists(temp_results_store)){
    old_results <- read_delim(temp_results_store, ",", col_types = "cdddddddddddiiiidddddcllllllllcccdd", col_names = TRUE, progress= FALSE, skip_empty_rows=TRUE)
  }
  old_results$details[is.na(old_results$details)] <- ""
  return(old_results)
}

#Load Old data
load_errors_store <- function() {
  #load Aggregated results
  errors_store <- data.frame()
  #Load aggregated data temporary results file and check
  if (file.exists(temp_errors_store)){
    errors_store <- read_delim(temp_errors_store, ",", col_types = "cccicc", col_names = TRUE, progress= FALSE, skip_empty_rows=TRUE)
  }
  return(errors_store)
}

#initialize configuration from property file
initialize_config <- function(){
  algorithm <<- subset(app_config,property=="algorithm")[,c("value")]
  standalone_max_txn_error_pct <<- as.double(subset(app_config,property=="standalone_max_txn_error_pct")[,c("value")])
  compare_max_txn_error_pct <<- as.double(subset(app_config,property=="compare_max_txn_error_pct")[,c("value")])
  req_successful_comparisons <<- as.double(subset(app_config,property=="required_successful_comparisons")[,c("value")])
  throw_error_on_standalone_failure <<- subset(app_config,property=="throw_error_on_standalone_failure")[,c("value")]=="TRUE"
  throw_error_on_comparison_failure <<- subset(app_config,property=="throw_error_on_comparison_failure")[,c("value")]=="TRUE"
  continue_on_standalone_failure <<- subset(app_config,property=="continue_on_standalone_failure")[,c("value")]=="TRUE"
  enable_dbscan <<- subset(app_config,property=="enable_dbscan_alogrithm")[,c("value")]=="TRUE"
  temp_results_store <<- subset(app_config,property=="aggregated_results_store")[,c("value")]
  is_archive_current_test <<- subset(app_config,property=="enable_archive_current_test")[,c("value")]=="TRUE"
  is_archive_current_test <<- subset(app_config,property=="enable_archive_current_test")[,c("value")]=="TRUE"
  show_base_result_issues_summary <<- subset(app_config,property=="show_base_result_issues_summary")[,c("value")]=="TRUE"
  html_report_output_directory <<- subset(app_config,property=="html_report_output_directory")[,c("value")]
  use_filename_as_identifier<<-subset(app_config,property=="use_filename_as_identifier")[,c("value")]=="TRUE"
  enable_html_report<<-subset(app_config,property=="enable_html_report")[,c("value")]=="TRUE"
  temp_errors_store <<-subset(app_config,property=="error_results_store")[,c("value")]
  generate_vs_comparision_weighted<<-subset(app_config,property=="generate_compare_current_vs_base_weighted")[,c("value")]=="TRUE"
  get_best_of_all_results_for_weighted_summary<<-subset(app_config,property=="get_best_of_all_results_for_weighted_summary")[,c("value")]=="TRUE"
  number_of_result_file_to_be_merged_for_weighted_summary<<-subset(app_config,property=="number_of_result_file_to_be_merged_for_weighted_summary")[,c("value")]=="TRUE"
}

#Calculate Percent difference between two numbers
percent_difference <- function(base,current){
  
  if(is.na(current) && is.na(base)){
    return(NA)
  }
  # return(((current-base)/base)*100)
  #Updated as per calculations used by team
  return(((base-current)/base)*100)
}

#Check if value is between two thresholds + and -
threshold_criteria_check <- function(percent_diff,positive_thresh,negative_thresh){
  
  bool=FALSE
  if(percent_diff>=negative_thresh && percent_diff<=positive_thresh) {
    return(TRUE)
  }
  return(FALSE)
}

#Calculate error % for standalone and comparison summary
get_txn_summary_error_pct <- function(data){
  z=data[,c("check")]
  error_pct=length(z[z==FALSE])/ (length(z[z==FALSE])+length(z[z==TRUE]))
  return(error_pct*100)
}

#Print errors from comparison and standalone summary results
print_error_from_summary <- function(data){
  
  if(nrow(data)>=1){
    for(i in 1:nrow(data)){
      if(data[i,c("details")]!="" & data[i,c("skip")]==FALSE){
        log_debug(paste0(data[i,c("label")]," = ",data[i,c("details")]))
      }
    }
  }else{
    log_debug("No errors found\n")
  }
  # log_debug("***************************************")
}

#Throw error if minimum comparisons results are not satisfied
check_and_throw_error <- function(overall_summary) {
  
  current_test_chk <- TRUE
  comparision_chk <- TRUE
  
  # Current test Check
  if(nrow(subset(final_results, (is.na(baseFilename) & !is.na(currentFilename) & status==TRUE)))==0){
    current_test_chk <- FALSE
  }
  
  #Check if current to base comparisons meet minimum required success criteria
  if(nrow(subset(final_results, (!is.na(baseFilename) & !is.na(currentFilename)  & status==TRUE)))<req_successful_comparisons) {
    comparision_chk <- FALSE
  }
  
  #throw error  Standalone
  if(!current_test_chk) {
    if(throw_error_on_standalone_failure){
      stop("Test Failed, check log above for more deails")
    }else{
      log_debug("Test Failed, check log above for more deails, throw error ignored beacause of config\n\n")
    }
  }
  
  #throw error Comparison
  if(!comparision_chk & base!="NA") {
    if(throw_error_on_comparison_failure){
      stop("Comparision Failed, check log above for more deails")
    }else{
      log_debug("Comparision Failed, check log above for more deails, throw error ignored beacause of config\n\n")
    }
  }
}


##Since file name is not unique adding custom method to generate name based on path - Specfic to 3PT
get_filename <- function(file){
  
  #Replace \ to / only applicable for windows
  file <- gsub("\\\\", "/", file)
  
  if(use_filename_as_identifier){
    new_file_name <- sub('\\..*$', '', basename(file))
  }else{
    file_name_parts <- strsplit(file, '/')[[1]]

    p3 <- length(file_name_parts)-1
    p2 <- length(file_name_parts)-2
    p1 <- length(file_name_parts)-3
    
    # new_file_name <- paste0(file_name_parts[p1],"_",file_name_parts[p2],"_",file_name_parts[p3])
    
    if(length(file_name_parts)>4){
      new_file_name <- paste0(file_name_parts[p1],"_",file_name_parts[p2],"_",file_name_parts[p3])
    }else if(length(file_name_parts)==4) {
      new_file_name <- paste0(file_name_parts[p2],"_",file_name_parts[p3])
    }else if(length(file_name_parts)==3) {
        new_file_name <- paste0(file_name_parts[p3])
    }else if(length(file_name_parts)==1) {
      new_file_name <- sub('\\..*$', '', basename(file_name_parts[1]))
    }
  }
  
  return(new_file_name)
}

stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}


#####################---Common Functions----########################

#####################  Standalone Functions ########################

#DBScan outlier detection, algo follow the pattern and leaves only ones that are odd man out.
dbscan_subset <- function(data,transaction_filter){
  tran_data <- data %>%
    subset(grepl(transaction_filter, label))
  
  filtered <- tran_data %>%
    select("timeStamp","elapsed")
  
  #Calculate k nearest mean for data and find eps
  knn <- dbscan::kNNdist(filtered, k=5)
  eps <- quantile(knn, probs = c(0.98))
  
  # # KNN Algorithm to find eps visual
  # #set size of graph
  # par(mar=c(3,3,3,3))
  # dbscan::kNNdistplot(filtered, k =  5)
  # abline(h = eps, lty = 3)
  
  #analyze data with dbscan algorithm
  db <- fpc::dbscan(filtered, eps = eps, MinPts = 5)
  
  #Generate graph with dbscan - required factoextra library
  # fviz_cluster(db, data = filtered, stand = FALSE,
  #              ellipse = FALSE, show.clust.cent = FALSE,
  #              geom = "point",palette = "jco", ggtheme = theme_classic())
  # str(db)
  
  #create data frame with observations from dbscan algorithm
  dbscan_assessed_data=do.call(rbind, Map(data.frame, A=db$cluster, B=db$isseed))
  
  # merge data dbscan with actual data
  filtered <- cbind(tran_data, dbscan_assessed_data)
  colnames(filtered)[5] <- "cluster_no"
  colnames(filtered)[6] <- "valid"
  
  #get outliers and data to be processed
  processed <- filtered %>%
    subset(valid==TRUE)
  outlier <- filtered %>%
    subset(valid==FALSE)
  
  dbscan_data <- list("processed" = processed, "outlier" = outlier)
  
  return(dbscan_data)
}

boxplot_subset <- function(data,transaction_filter) {
  
  data <- data %>%
    subset(grepl(transaction_filter, label))
  
  iqr <- IQR(data$elapsed)
  q1 <- (quantile(data$elapsed, probs =0.25, names = FALSE))- 1.5*iqr
  q3 <- (quantile(data$elapsed, probs =0.75, names = FALSE))+ 1.5*iqr
  
  processed <- data %>%
    subset(elapsed>=q1 & elapsed<=q3)
  outlier <- data %>%
    subset(elapsed<q1 | elapsed>q3)
  boxplot_data <- list("processed" = processed, "outlier" = outlier)
  return(boxplot_data)
}

#WIP - needed for Parallel processing...
get_boxplot_filtered_data <- function(i){
    log_debug(paste0("Processing data with Boxplot alogorithm:  ",as.character(i),"\n"))
    #filter name is Regex check if it matches
    boxplot_tran_data <- boxplot_subset(success_samples_data,paste0("^",i))[["processed"]]
    boxplot_data <<- rbind(boxplot_data,boxplot_tran_data)
    tran_scope <<- rbind(tran_scope,data.frame(label=i,inscope=nrow(boxplot_tran_data), stringsAsFactors=FALSE))
}

get_results_data_from_file <- function(filename){
  
  log_debug(paste0("loading data from:  ",filename))
  # data <- read_delim(filename, ",", col_names = TRUE, progress= FALSE, skip_empty_rows=TRUE)
  data <- readr::read_delim_chunked(filename,  DataFrameCallback$new(function(x, i) {x}), delim= ",",  chunk_size = 100, progress = TRUE)
  log_debug(paste0("loaded data from:  ",filename))
  
  ###remove first n seconds of data
  start_time=as.double(head(data, n = 1)["timeStamp"])
  end_time=as.double(tail(data, n = 1)["timeStamp"])
  
  nstart_time=(as.double(start_time)+(removeFirstNMinutes*60*1000))
  nend_time=(as.double(end_time)-(removeLastNMinutes*60*1000))
  data <- subset(data, timeStamp>nstart_time & timeStamp<nend_time)

  ###Filter
  log_debug(paste0("filtering Data started with include pattern"))

  included_data=data.frame()
  includes <- subset(app_config,property=="include")[,c("value")]
  for(i in includes){
    included_data <- rbind(included_data,subset(data, (grepl(i, label))), stringsAsFactors=FALSE)
  }
  log_debug("Removing duplicates if existis")
  included_data <- distinct(included_data, timeStamp,label, .keep_all= TRUE)

  log_debug(paste0("Removing Exclude pattern Data"))
  excludes <- subset(app_config,property=="exclude")[,c("value")]
  for(i in excludes){
    data_e <- subset(data, (grepl(i, label)))
    included_data <- anti_join(included_data,data_e,by="label")
  }

  log_debug(paste0("Data processing complete"))
  log_debug(paste0("Filtered Data: ",nrow((included_data))))
  return(included_data)
}
 
#Get average and 90% for unique labels from individual results JTL files
#error results are skipped during data processing
get_results <- function(filename,algorithm,isCurrent,isAddToFinalResult) {
  
  result_temp <- data.frame()
  data_full <- data.frame()
  if(nrow(old_results)>0){
    if(is_archive_current_test | !isCurrent){
      data_full <- old_results %>%
        subset(file==get_filename(filename) & algorithm==algorithm)
    }
  }
  
  if(nrow(old_errors)>0){
    if(is_archive_current_test | !isCurrent){
      old_errors <- old_errors %>% subset(file==get_filename(filename) & algorithm==algorithm)
    }
  }

  if(nrow(data_full)==0 ){
    
    #If 'none', comparison is expecting string so initializing string
    if(is.na(algorithm)){
      algorithm <- "none"
    } 
    
    #Get results from file
    data <- get_results_data_from_file(filename)
    
    if(nrow(data)>0){
      start_time=as.double(head(data, n = 1)["timeStamp"])
      end_time=as.double(tail(data, n = 1)["timeStamp"])
      
      # start_time=format(as.POSIXct(as.double(start_time) / 1000, origin = "1970-01-01", tz = "EST"), "%Y-%m-%d %H:%M:%OS3")
      # end_time=format(as.POSIXct(as.double(end_time) / 1000, origin = "1970-01-01", tz = "EST"), "%Y-%m-%d %H:%M:%OS3")
      
      # Get Failures
      failure_tran_count <- data %>%
        subset(responseCode < 200 | responseCode >= 400) %>%
        group_by(label) %>%
        summarise(fail=n())
      
      #Get Success Samples  
      success_samples_data <- data %>%
        select(timeStamp,elapsed, label, responseCode,bytes,sentBytes,Connect) %>%
        # select(timeStamp,elapsed, label, responseCode) %>%
        subset(responseCode >= 200 & responseCode < 400)
      
      cdata <- success_samples_data
      
      # Get Success Transaction_count
      success_tran_count <- success_samples_data %>%
        group_by(label) %>%
        summarise(pass=n())
      
      #Data filter algorithm
      if(tolower(algorithm) == "boxplot") {
        boxplot_data <- data.frame()
        tran_scope <- data.frame(label=character(),inscope=double())
        transactions <- unique(success_samples_data[,c("label")])$label
        
        # lapply(transactions, FUN=get_boxplot_filtered_data)
        # mcapply(transactions, FUN=get_boxplot_filtered_data)
        # mcmapply(transactions, FUN=get_boxplot_filtered_data, SIMPLIFY = TRUE)
        # 
        # foreach(i = 1:length(transactions), .inorder = FALSE, .export = c("boxplot_subset","success_samples_data","boxplot_data","tran_scope"), .packages=c("dplyr","moments","dbscan","fpc","logger","readr")) %dopar% {
        #     log_debug(paste0("Processing data with Boxplot alogorithm:  ",transactions[i],"\n"))
        #     #filter name is Regex check if it matches
        #     boxplot_tran_data <- boxplot_subset(success_samples_data,paste0("^",transactions[i]))[["processed"]]
        #     boxplot_data <<- rbind(boxplot_data,boxplot_tran_data)
        #     tran_scope <<- rbind(tran_scope,data.frame(label=i,inscope=nrow(boxplot_tran_data), stringsAsFactors=FALSE))
        # }
        
        for(i in transactions) {
          log_debug(paste0("Processing data with Boxplot alogorithm:  ",as.character(i),"\n"))
          #filter name is Regex check if it matches
          boxplot_tran_data <- boxplot_subset(success_samples_data,paste0("^",i))[["processed"]]
          boxplot_data <- rbind(boxplot_data,boxplot_tran_data)
          tran_scope <- rbind(tran_scope,data.frame(label=i,inscope=nrow(boxplot_tran_data), stringsAsFactors=FALSE))
        }
        
        success_data <- boxplot_data
      } else{
        success_data <- success_samples_data
      }
      
      data_summary <- success_data %>%
        group_by(label) %>%
        summarise(
          min=min(elapsed),
          avg=mean(elapsed),
          max=max(elapsed),
          pc_90 =quantile(elapsed, probs =0.90, names = FALSE),
          pc_95 =quantile(elapsed, probs =0.95, names = FALSE),
          sd=sd(elapsed),
          ci_95=qnorm(0.975)*sd(elapsed)/sqrt(n()),
          kr=psych::kurtosi(elapsed, na.rm = TRUE),
          # kr=kurtosis(elapsed, na.rm = TRUE),
          receivedBytes_sum=sum(bytes),
          sentBytes_sum=sum(sentBytes),
          connect=mean(Connect)
        )
      
      rm(success_data)
      
      #merge all the data
      data_full <- merge(data_summary,failure_tran_count,by="label",all = TRUE)
      data_full <- merge(data_full,success_tran_count,by="label",all = TRUE)
      if(tolower(algorithm) == "boxplot") {
        data_full <- merge(data_full,tran_scope,by="label",all = TRUE)
        
        #add discard column
        discard_col=ncol(data_full)+1
        for(i in 1:nrow(data_full)){
          data_full[i,discard_col] <- data_full[i,c("pass")]-data_full[i,c("inscope")]
        }
        colnames(data_full)[discard_col] <- "discard"
      }else{
        #add discard & in scope  if not boxplot algorithm is not used
        discard_col=ncol(data_full)+1
        for(i in 1:nrow(data_full)){
          data_full[i,discard_col] <- 0
          data_full[i,(discard_col+1)] <- data_full[i,c("pass")]
        }
        colnames(data_full)[discard_col] <- "discard"
        colnames(data_full)[discard_col+1] <- "inscope"
      }
      
      #Set 0 for all NA
      data_full[is.na(data_full)] <- 0
      
      #discard%
      discardpct_col=ncol(data_full)+1
      for(i in 1:nrow(data_full)){
        data_full[i,discardpct_col] <- (data_full[i,c("discard")]/(data_full[i,c("pass")]+data_full[i,c("fail")]))*100
      }
      colnames(data_full)[discardpct_col] <- "discard_pct"
      
      #error%
      error_pct_col=ncol(data_full)+1
      for(i in 1:nrow(data_full)){
        data_full[i,error_pct_col] <- (data_full[i,c("fail")]/data_full[i,c("pass")])*100
      }
      colnames(data_full)[error_pct_col] <- "error_pct"
      
      #throughput
      throughput_col=ncol(data_full)+1
      for(i in 1:nrow(data_full)){
        data_full[i,throughput_col] <- (data_full[i,c("pass")]+data_full[i,c("fail")])/((end_time%/%1000)-(start_time%/%1000))
      }
      colnames(data_full)[throughput_col] <- "throughput"
      
      #Received bytes Per Sec
      receivedBytesPerSec=ncol(data_full)+1
      for(i in 1:nrow(data_full)){
        data_full[i,receivedBytesPerSec] <-  data_full[i,c("receivedBytes_sum")]/((end_time%/%1000)-(start_time%/%1000))
      }
      colnames(data_full)[receivedBytesPerSec] <- "receivedBytesPerSec"
      
      #Sent bytes Per Sec
      sentBytesPerSec=ncol(data_full)+1
      for(i in 1:nrow(data_full)){
        data_full[i,sentBytesPerSec] <- data_full[i,c("sentBytes_sum")]/((end_time%/%1000)-(start_time%/%1000))
      }
      colnames(data_full)[sentBytesPerSec] <- "sentBytesPerSec"
      
      #add empty details column to be used in standalone comparision
      data_full['details'] <- ""
      
      #Check conditions - basic
      data_full <- stanadlone_check(data_full)
      #If dbscan alogirithm is enabled check transactions that exceeds threshold and provide info clusters that exceeds sla
      if(enable_dbscan & isCurrent){
        # Get DB scan summary for all the transactions that exceeds discard threshold
        dbscan_summary <- get_dbscan_summary(data_full,cdata)
        #Condition check and message to d=test result
        if(nrow(dbscan_summary)>0){
          data_full <- stanadlone_condition_check_dbscan(data_full, dbscan_summary,config)    
        }
      }
      rm(cdata)
      
      data_full <- data_full %>%
        mutate(file=get_filename(filename)) %>%
        mutate(algorithm=algorithm) %>%
        mutate(start_time=start_time) %>%
        mutate(end_time=end_time)

      old_errors <- data %>%
        subset(responseCode < 200 | responseCode >= 400) %>%
        group_by(label,responseCode,failureMessage) %>%
        summarise(fail=n())

      old_errors <- old_errors %>%
        mutate(file=get_filename(filename)) %>%
        mutate(algorithm=algorithm)

	    if(is_archive_current_test|!isCurrent){
	      #Write data to temp storage
	      write_delim(data_full, temp_results_store, delim = ",", col_names = TRUE, progress= FALSE, append=TRUE)
	      write_delim(old_errors, temp_errors_store, delim = ",", col_names = TRUE, progress= FALSE, append=TRUE)
	    }
      
      #Store data to old result just incase flow is used..
      old_results <<- rbind(old_results, data_full)
      old_errors <<- rbind(old_errors, old_errors)
    }
  }else{
    log_info(paste0("Archived Results found, skip processing : ",filename," Transactions found: ",nrow(data_full),"\n"))
  }
  
  err_pct <- get_txn_summary_error_pct(data_full)
  #get current test status - check if number of error transaction are less than allow per test
  standalone_status <- err_pct>standalone_max_txn_error_pct
  
  log_info(paste("********************",get_filename(filename),"status:",!standalone_status,",Transactions with Failed conditions: ", err_pct,"%"))
  
  if(isCurrent | show_base_result_issues_summary){
    #print issues from current result
    log_info("***************************************")
    log_info(paste("***","Issues: ",get_filename(filename),", data filtering algorithm: ",algorithm,"***\n"))
    print_error_from_summary(data_full)
    log_info("***************************************")
  }
 
  if(isCurrent){
    result_tmp <- data_full %>%
      mutate(testType="current") %>%
      mutate(no=as.numeric(current_result_no))
    
    temp_result_filemap <<- rbind(temp_result_filemap, data.frame(filename=get_filename(filename), temp_filename=paste0("CurrentTest",current_result_no), testType="current", result_no=current_result_no,actual_file=filename))
    
    current_result_no <<- current_result_no+1
  }else{
    result_tmp <- data_full %>%
      mutate(testType="base") %>%
      mutate(no=as.numeric(base_result_no))
    
    temp_result_filemap <<- rbind(temp_result_filemap, data.frame(filename=get_filename(filename), temp_filename=paste0("BaselineTest",base_result_no), testType="base", result_no=current_result_no,actual_file=filename))
    
    base_result_no <<- base_result_no+1
  }
  
  
  ###
  # filename <- subset(temp_result_filemap,filename==get_filename(filename))$temp_filename
  
  filename=get_filename(filename)
  ##Add details to overall summary
  if(isAddToFinalResult){
    if(isCurrent){
      if(nrow(subset(final_results, currentFilename == filename & is.na(baseFilename)))==0) {
        #final_results <<- rbind(final_results,data.frame(baseFilename=NA,currentFilename=get_filename(filename),status=!standalone_status,err_pct=err_pct,type="standalone"), stringsAsFactors=FALSE)
        final_results <<- rbind(final_results,data.frame(baseFilename=NA,currentFilename=filename,status=!standalone_status,err_pct=err_pct,type="standalone"), stringsAsFactors=FALSE)
      }
    }else{
      if(nrow(subset(final_results, baseFilename == filename & is.na(currentFilename)))==0) {
        final_results <<- rbind(final_results,data.frame(baseFilename=filename,currentFilename=NA,status=!standalone_status,err_pct=err_pct,type="standalone"), stringsAsFactors=FALSE)
      }
    }
  }
  
  
  #save all test results in one dataframe
  test_results_markdown <<- rbind(test_results_markdown,result_tmp, stringsAsFactors=FALSE)
  old_errors <- transform(old_errors, failureMessage=as.character(failureMessage))
  test_errors_markdown <<- rbind(test_errors_markdown,old_errors, stringsAsFactors=FALSE)  
  
  rm(data_full)
  
}

#Check each transaction for standalone thresholds
stanadlone_check <- function(data){
  
  data['skip'] <- NA
  data['sla_chk'] <- NA
  data['rt_notify_chk'] <- NA
  data['rt_alert_chk'] <- NA
  data['rt_prod_chk'] <- NA
  data['discard_chk'] <- NA
  data['error_pct_chk'] <- NA
  data['check'] <- NA
  data['app'] <- NA
  
  for(i in 1:nrow(data)){
    conf <- config %>%
      subset(grepl("s", type) & grepl(paste0("^",data[i,c("label")]), transaction))
    
    #Load default
    if(nrow(conf)==0){
      conf <- config %>%
        subset(grepl("s", type) & grepl("_default_", transaction))
    }
    
    if(conf[1,c("skip")]){
      data[i,c("skip")]=TRUE
      data[i,c("details")] <- conf[1,c("comment")]
      data[i,c("check")]=TRUE
      data[i,c("discard_chk")]=TRUE
      if(conf[1,c("app")]==0){
        data[i,c("app")]="NA" 
      }else{
        data[i,c("app")]=conf[1,c("app")]
      }
    }else{
      if(conf[1,c("rt_metric")]=="avg"){
        res <- stanadlone_contitions_check(data[i,c("avg")],data[i,c("discard_pct")],data[i,c("error_pct")],conf)
      }else if(conf[1,c("rt_metric")]=="pc_90"){
        res <- stanadlone_contitions_check(data[i,c("pc_90")],data[i,c("discard_pct")],data[i,c("error_pct")],conf)
      }else if(conf[1,c("rt_metric")]=="pc_95"){
        res <- stanadlone_contitions_check(data[i,c("pc_95")],data[i,c("discard_pct")],data[i,c("error_pct")],conf)
      }else{
        stop("allowed values for 'rt_metric' are avg,pc_90,pc_95")
      }
      
      data[i,c("sla_chk")]=res[["sla_chk"]]
      data[i,c("rt_notify_chk")]=res[["rt_notify_chk"]]
      data[i,c("rt_alert_chk")]=res[["rt_alert_chk"]]
      data[i,c("rt_prod_chk")]=res[["rt_prod_chk"]]
      data[i,c("rt_prod_chk")]=res[["rt_prod_chk"]]
      data[i,c("discard_chk")]=res[["discard_chk"]]
      data[i,c("error_pct_chk")]=res[["error_pct_chk"]]
      data[i,c("details")]=paste0(data[i,c("details")],res[["comment"]])
      data[i,c("check")]=res[["check"]]
      data[i,c("skip")]=FALSE
      if(conf[1,c("app")]==0){
        data[i,c("app")]="NA" 
      }else{
        data[i,c("app")]=conf[1,c("app")]
      }
    }
  }
  return(data)
}

#Check standalone conditions
stanadlone_contitions_check <- function(rt, discard_pc, error_pc,conf){
  
  sla <- as.double(conf[1,c("rt_sla")])
  rt_notify <- as.double(conf[1,c("rt_notify")])
  rt_alert <- as.double(conf[1,c("rt_alert")])
  rt_prod <- as.double(conf[1,c("rt_prod")])
  rt_dev_pct <- as.double(conf[1,c("rt_deviation_pct_allowed")])
  discard_pct_allow <- as.double(conf[1,c("discard_pct_allow")])
  error_deviation_pct_allowed <- as.double(conf[1,c("error_deviation_pct_allowed")])
  discard_deviation_pct_allowed <-  as.double(conf[1,c("discard_deviation_pct_allowed")])
  error_pct <- as.double(conf[1,c("error_pct")])
  check <- conf[1,c("check")]
  
  msg <- ""
  prod_chk <- TRUE
  sla_chk<- TRUE
  rt_notify_chk <- TRUE
  rt_alert_chk <- TRUE
  discard_chk <- TRUE
  error_pct_chk <- TRUE
  
  rt_temp_warning=""
  
  pos=sla+(sla*rt_dev_pct)
  neg=sla-(sla*rt_dev_pct)
  if(!threshold_criteria_check(rt,pos,neg)){
    msg <- paste0("<li>",conf[1,c("rt_metric")]," response time: ",rround(rt,2)," exceeded SLA: ",round(sla,2)," allowed between ",neg," and ",pos,"</li>")
    sla_chk<-FALSE
  }else{
    if(rt>sla){
      rt_temp_warning=paste0("<li>Warning: ",conf[1,c("rt_metric")]," response time: ",round(rt,2)," exceeded SLA threshold: ",round(sla,2),"</li>")
    }
  }
  
  pos=rt_notify+(rt_notify*rt_dev_pct)
  neg=rt_notify-(rt_notify*rt_dev_pct)
  if(!threshold_criteria_check(rt,pos,neg)){
    msg <- paste0("<li>",conf[1,c("rt_metric")]," response time: ",round(rt,2)," exceeded notify threshold: ",round(rt_notify,2),",notify team"," allowed between ",neg," and ",pos,"</li>")
    sla_chk<-TRUE
    rt_notify_chk <- FALSE
  }else{
    if(rt>rt_notify){
      rt_temp_warning=paste0("<li>Warning: ",conf[1,c("rt_metric")]," response time: ",round(rt,2)," exceeded Notify threshold: ",round(rt_notify,2),"</li>")
    }
  }
  
  pos=rt_alert+(rt_alert*rt_dev_pct)
  neg=rt_alert-(rt_alert*rt_dev_pct)
  if(!threshold_criteria_check(rt,pos,neg)){
    msg <- paste0("<li>",conf[1,c("rt_metric")]," response time: ",round(rt,2)," exceeded alert threshold: ",round(rt_alert,2),", alert team"," allowed between ",neg," and ",pos,"</li>")
    sla_chk<-TRUE
    rt_notify_chk <- TRUE
    rt_alert_chk <- FALSE
  }else{
    if(rt>rt_alert){
      rt_temp_warning=paste0("<li>Warning: ",conf[1,c("rt_metric")]," response time: ",round(rt,2)," exceeded Atlert threshold: ",round(rt_notify,2),"</li>")
    }
  }
  
  pos=rt_prod+(rt_prod*rt_dev_pct)
  neg=rt_prod-(rt_prod*rt_dev_pct)
  if(!threshold_criteria_check(rt,pos,neg)){
    msg <- paste0(msg,"<li>",conf[1,c("rt_metric")]," response time: ",round(rt,2)," exceeded production SLA: ",round(rt_prod,2)," allowed between ",neg," and ",pos,"</li>")
    prod_chk <- FALSE
  }else{
    if(rt>rt_prod){
      rt_temp_warning=paste0(msg,"<li>Warning: ",conf[1,c("rt_metric")]," response time: ",round(rt,2),",exceeded Prod threshold: ",round(rt_prod,2),"</li>")
    }
  }
  
  if(msg==""){
    msg <- paste0(msg,rt_temp_warning)
  }
  
  pos=discard_pct_allow+discard_deviation_pct_allowed
  neg=-discard_pct_allow-discard_deviation_pct_allowed
  if(!threshold_criteria_check(discard_pc,pos,neg)){
    msg <- paste0(msg,"<li>Discard %:",round(discard_pc,2),"%, is more than threshold:",round(discard_pct_allow,2),"%, allowed between ",neg,"% and ",pos,"% </li>")  
    discard_chk <- FALSE
  } else if(discard_pc > discard_pct_allow ){
    msg <- paste0(msg,"<li>Warning: Discard %:",round(discard_pc,2),"%, is more than threshold:",round(discard_pct_allow,2),"%</li>")  
  }
  
  pos=error_pct+error_deviation_pct_allowed
  neg=-error_pct-error_deviation_pct_allowed
  if(!threshold_criteria_check(error_pc,pos,neg)){
    msg <- paste0(msg,"<li>Error %: ",round(error_pc,2),"% is more than threshold: ",round(error_pct,2),"%, allowed between ",neg,"% and ",pos,"%</li>")
    error_pct_chk <- FALSE
  } else if(error_pc > error_pct ){
    msg <- paste0(msg,"<li>Warning: Error%: ",round(error_pc,2),"% is more than threshold: ",round(error_pct,2),"%</li>")
  }

  #Overall status check
  comp_check<-TRUE
  
  if(nchar(check)==0){
    msg <- paste0(msg,"<li>Validation not configured</li>")
  }else{
    for(i in 1:nchar(check)){
      if(substring(check, i, i)=="s"){
        comp_check <- comp_check & sla_chk
      }else if(substring(check, i, i)=="n") {
        comp_check <- comp_check & rt_notify_chk
      }else if(substring(check, i, i)=="a") {
        comp_check <- comp_check & rt_alert_chk
      }else if(substring(check, i, i)=="p") {
        comp_check <- comp_check & prod_chk
      }else if(substring(check, i, i)=="e") {
        comp_check <- comp_check & error_pct_chk
      }else if(substring(check, i, i)=="d") {
        comp_check <- comp_check & discard_chk
      }
      
      #if false exit loop
      if(!comp_check){
        break
      }
    }
  }
  
  return(list("check"=comp_check,"discard_chk"=discard_chk, "comment"=msg, "rt_prod_chk"=prod_chk, "sla_chk"=sla_chk, "rt_notify_chk"=rt_notify_chk, "rt_alert_chk"=rt_alert_chk, "error_pct_chk"=error_pct_chk))
}

#Get Summary for transactions that exceeds discard threshold
get_dbscan_summary <- function(result_summary, data){
  
  combined_dbscan_comp <- data.frame(label=character(),
                                     avg=double(),
                                     pc_90=double(),
                                     pc_95=double(),
                                     sd=double(),
                                     cluster=integer(),
                                     start=double(),
                                     end=double()
  )
  
  for(i in 1:nrow(result_summary)){
    if(!result_summary[i,c("discard_chk")]){
      log_debug(paste0("\nProcessing \"",result_summary[i,c("label")],"\" using DBScan algorithm ......"))
      test=dbscan_subset(data,result_summary[i,c("label")])[["processed"]]
      for(i in unique(test$cluster_no)){
        dbfull <- test %>%
          subset(cluster_no==i) %>%
          group_by(label) %>%
          summarise(
            avg=mean(elapsed),
            pc_90 =quantile(elapsed, probs =0.90, names = FALSE),
            pc_95 =quantile(elapsed, probs =0.95, names = FALSE),
            sd=sd(elapsed),
            cluster=i,
            start=format(as.POSIXct(first(timeStamp) / 1000, origin = "1970-01-01", tz = "EST"), "%Y-%m-%d %H:%M:%OS3"),
            end=format(as.POSIXct(last(timeStamp) / 1000, origin = "1970-01-01", tz = "EST"), "%Y-%m-%d %H:%M:%OS3")
          )
        if(nrow(combined_dbscan_comp)==0){
          combined_dbscan_comp <- dbfull
        }else{
          combined_dbscan_comp <- rbind(combined_dbscan_comp,dbfull)
        }
      }
    }
  }
  return(combined_dbscan_comp)
}

#Get info on the clusters that exceed threshold and append details for transactions
stanadlone_condition_check_dbscan <- function(test_summary, dbscan_summary,config) {
  
  if(nrow(subset(test_summary,discard_chk==FALSE))>0){
    for(i in 1:nrow(dbscan_summary)){
      for(j in 1:nrow(test_summary)){
        if(dbscan_summary[i,c("label")]==test_summary[j,c("label")]){
          
          conf_t <- config %>%
            subset(grepl("s", type) & grepl(paste0("^",dbscan_summary[i,c("label")]), transaction))
          #Load default
          if(nrow(conf_t)==0){
            conf_t <- config %>%
              subset(grepl("s", type) & grepl("_default_", transaction))
          }
          
          # check if any cluster rt is more than sla in any of the cluster
          if(dbscan_summary[i,c("avg")]>conf_t[1,c("rt_sla")] & conf_t[1,c("rt_metric")]=="avg"){
            test_summary[j,c("details")] <- paste0(test_summary[j,c("details")],"<li>",conf_t[1,c("rt_metric")]," response time: ",dbscan_summary[i,c("avg")]," exceeds SLA threshold ", conf_t[1,c("rt_sla")]," in Cluster: ",dbscan_summary[i,c("cluster")], ", start time: ", dbscan_summary[i,c("start")], " - end time: ", dbscan_summary[i,c("end")],"</li>")
          }else if(dbscan_summary[i,c("pc_90")]>conf_t[1,c("rt_sla")]  & conf_t[1,c("rt_metric")]=="pc_90"){
            test_summary[j,c("details")] <- paste0(test_summary[j,c("details")],"<li>",conf_t[1,c("rt_metric")]," response time: ",dbscan_summary[i,c("pc_90")]," exceeds SLA threshold ", conf_t[1,c("rt_sla")]," in Cluster: ",dbscan_summary[i,c("cluster")], ", start time: ", dbscan_summary[i,c("start")], " - end time: ", dbscan_summary[i,c("end")],"</li>")
          }else if(dbscan_summary[i,c("pc_95")]>conf_t[1,c("rt_sla")] & conf_t[1,c("rt_metric")]=="pc_95"){
            test_summary[j,c("details")] <- paste0(test_summary[j,c("details")],"<li>",conf_t[1,c("rt_metric")]," response time: ",dbscan_summary[i,c("pc_95")]," exceeds SLA threshold ", conf_t[1,c("rt_sla")]," in Cluster: ",dbscan_summary[i,c("cluster")], ", start time: ", dbscan_summary[i,c("start")], " - end time: ", dbscan_summary[i,c("end")],"</li>")
          }
          break
        }
      }
    }
  }
  
  return(test_summary)
}

#####################---Standalone Functions----########################

#####################  Comparison Functions ########################

#compare two results data and provide summary
get_comparison_summary <- function(current,base) {
  
  data_full <- merge(base,current,by="label",all = FALSE)
  
  final <- data_full %>% 
    select("label","pass.x","pass.y","avg.x","avg.y","pc_90.x","pc_90.y","pc_95.x","pc_95.y","fail.x","fail.y","inscope.x","inscope.y","app.x","discard_pct.x","discard_pct.y","ci_95.x","ci_95.y","kr.x","kr.y","throughput.x","throughput.y","connect.x","connect.y","receivedBytesPerSec.x","receivedBytesPerSec.y","sentBytesPerSec.x","sentBytesPerSec.y") %>%
    mutate(avg_rpd=percent_difference(as.double(avg.x),as.double(avg.y))) %>%
    mutate(pc_90_rpd=percent_difference(as.double(pc_90.x),as.double(pc_90.y))) %>%
    mutate(pc_95_rpd=percent_difference(as.double(pc_95.x),as.double(pc_95.y))) %>%
    mutate(pass_inscope_rpd=percent_difference(as.double(inscope.x),as.double(inscope.y)))
  
  # rename app.x column name to app
  names(final)[names(final) == 'app.x'] <- 'app'
  final['details'] <- ""
  
  final <- comparision_check(final)
  return(final)
}

#Comparison results check - 2 results
comparision_check <- function(data) {
  
  data['skip'] <- NA
  data['rt_rpd_chk'] <- NA
  data['pass_inscope_rpd_chk'] <- NA
  data['check'] <- TRUE
  
  for(i in 1:nrow(data)){
    conf <- config %>%
      subset(grepl("c", type) & grepl(paste0("^",data[i,c("label")]), transaction))
    
    #Load default
    if(nrow(conf)==0){
      conf <- config %>%
        subset(grepl("c", type) & grepl("_default_", transaction))
    }
    
    if(conf[1,c("skip")]){
      data[i,c("details")]=conf[1,c("comment")]
      data[i,c("skip")]=TRUE
    }else{
      if(conf[1,c("rt_metric")]=="avg"){
        res <- compare_contitions_check(data[i,c("avg_rpd")],data[i,c("pass_inscope_rpd")],conf)
      }else if(conf[1,c("rt_metric")]=="pc_90"){
        res <- compare_contitions_check(data[i,c("pc_90_rpd")],data[i,c("pass_inscope_rpd")],conf)
      }else if(conf[1,c("rt_metric")]=="pc_95"){
        res <- compare_contitions_check(data[i,c("pc_95_rpd")],data[i,c("pass_inscope_rpd")],conf)
      }else{
        stop("allowed values for 'rt_metric' are avg_rpd,pc_90_rpd,pc_95_rpd")
      }
      
      data[i,c("skip")]=FALSE
      data[i,c("details")]=paste0(data[i,c("details")],res[["comment"]])
      data[i,c("check")]=res[["check"]]
      data[i,c("rt_rpd_chk")]=res[["rt_rpd_chk"]]
      data[i,c("pass_inscope_rpd_chk")]=res[["pass_inscope_rpd_chk"]]
    }
  }
  return(data)
}

#Check for comparison conditions
compare_contitions_check <- function(rt_rpd, pass_inscope_rpd, conf){
  
  rt_rpd_allow <- conf[1,c("rt_rpd_allow")]
  samples_rpd_allow <- conf[1,c("samples_rpd_allow")]
  rt_rpd_deviation_pct_allowed <- conf[1,c("rt_rpd_deviation_pct_allowed")]
  check <- conf[1,c("check")]
  
  msg <- ""
  rt_rpd_chk <- TRUE
  pass_inscope_rpd_chk <- TRUE
  
  pos=rt_rpd_allow+rt_rpd_deviation_pct_allowed
  neg=-rt_rpd_allow-rt_rpd_deviation_pct_allowed
  if(!threshold_criteria_check(rt_rpd,pos,neg)){
    msg <- paste0("<li>Relative delta ",round(rt_rpd,2),"% exceeded Threshold:+-",round(rt_rpd_allow,2),"%, allowed between ",neg,"% and ",pos,"%</li>")
    rt_rpd_chk<-FALSE
  }else if(!threshold_criteria_check(rt_rpd,rt_rpd_allow,-rt_rpd_allow)){
    msg <- paste0("<li>Warning: Relative delta ",round(rt_rpd,2),"% exceeded Threshold :+-",round(rt_rpd_allow,2),"%</li>")
  }
  
  if(!threshold_criteria_check(pass_inscope_rpd,samples_rpd_allow,-samples_rpd_allow)){
    if(msg!=""){
      msg <- paste0(msg,"<li>Deviation of samples used in comparison between base and current test(successful samples only): ",round(pass_inscope_rpd,2),"%, exceeds allowed: +-",round(samples_rpd_allow,2),"%</li>")
    }else{
      msg <- paste0(msg,"<li>Deviation of samples used in comparison between base and current test(successful samples only): ",round(pass_inscope_rpd,2),"%, exceeds allowed: +-",round(samples_rpd_allow,2),"%</li>")
    }
    pass_inscope_rpd_chk <- FALSE
  }
  
  #Overall status check
  comp_check<-TRUE
  
  if(nchar(check)==0){
    if(msg!=""){
      msg <- paste0(msg,"<li>","Validation not configured</li>")
    }else{
      msg <- paste0(msg,"<li>Validation not configured</li>")
    }
  }else{
    for(i in 1:nchar(check)){
      if(substring(check, i, i)=="r"){
        comp_check <- comp_check & rt_rpd_chk
      }else if(substring(check, i, i)=="s") {
        comp_check <- comp_check & pass_inscope_rpd_chk
      }
      
      #if false exit loop
      if(!comp_check){
        break
      }
    }
  }
  
  return(list("check"=comp_check,"rt_rpd_chk"=rt_rpd_chk, "pass_inscope_rpd_chk"=pass_inscope_rpd_chk, "comment"=msg))
}

#compare current result file with each base file separately and compile summary of comparisons
multi_compare <- function(current,base) {
  
  files <- strsplit(current, ",")[[1]]
  for(file in files){
    get_results(file,algorithm,TRUE,TRUE)
  }
  
  files <- strsplit(base, ",")[[1]]
  for(file in files){
    get_results(file,algorithm,FALSE,TRUE)
  }
  
  for(i in 1:(base_result_no-1)){
    for(j in 1:(current_result_no-1)) {
      
      current_res_t=subset(test_results_markdown,testType=="current" & no==j)
      base_res_t=subset(test_results_markdown,testType=="base" & no==i)
      base_t <- head(base_res_t["file"],n=1)$file
      current_t <- head(current_res_t["file"],n=1)$file
      
      if(weighted_comparision!=TRUE | generate_vs_comparision_weighted){
        cat(paste0(base_t,"_vs_",current_t,"\n"))
        summary_comp=get_comparison_summary(current_res_t,base_res_t)
        err_pct <- get_txn_summary_error_pct(summary_comp)
        failure=err_pct>compare_max_txn_error_pct
        
        comparision <- summary_comp %>%
          mutate(vs=paste0(toString(base_t),"_VS_",toString(current_t))) %>%
          mutate(status=failure)
        comparision_results_markdown <<- rbind(comparision_results_markdown,comparision)
        
        ###
        # base_t <- subset(temp_result_filemap,filename==base_t)$temp_filename
        # current_t <- subset(temp_result_filemap,filename==current_t)$temp_filename
        
        
        final_results <<- rbind(final_results,data.frame(baseFilename=base_t,currentFilename=current_t,status=!failure,err_pct=err_pct,type="comparison"), stringsAsFactors=FALSE)
        log_info(paste("********************",paste0(toString(base_t),"_VS_",toString(current_t)),"status:",!failure,",Transactions with Failed conditions: ", err_pct,"%"))
        log_info("***************************************")
        log_info(paste("***",base_t, "vs", current_t,"***"))
        print_error_from_summary(summary_comp)
        log_info("***************************************")
      }
     
    }
  }
}

#####################---Comparision Functions----########################


#####################---Get Best Results from Available----########################

get_best_results <- function() {
  
  #get save current and base failed char array
  s_b=subset(temp_result_filemap,testType=='base')$filename
  s_c=subset(temp_result_filemap,testType=='current')$filename
  
  #Geneate unique combinations for curret and base with n selections
  sc_b=combn(s_b,number_of_result_file_to_be_merged_for_weighted_summary)
  sc_c=combn(s_c,number_of_result_file_to_be_merged_for_weighted_summary)
  c_b=rbind(sc_b, combined = apply(sc_b, 2, paste0, collapse = ","))["combined",]
  c_c=rbind(sc_c, combined = apply(sc_c, 2, paste0, collapse = ","))["combined",]
  
  #Get all unique combinations possible for n selections of current and base
  all_combinations <<- expand.grid(c_b,c_c)
  colnames(all_combinations) <<- c("current", "base")
  all_combinations['error_pct'] <<- NA
  
  #Check combination
  log_info(paste0("Started Processing combinations: ", nrow(all_combinations)))
  
  temp_test_results_markdown<<-test_results_markdown
  for(j in 1:nrow(all_combinations)){
    if(j%%100==0){
      log_debug(paste0("Processing combination no:",j))  
    }
    test_results_markdown <<- test_results_markdown[0,]
    temp_current_best<<-as.character(all_combinations[j,"current"])
    for(i in strsplit(temp_current_best, ",")[[1]]) {
      test_results_markdown <<- rbind(test_results_markdown,subset(temp_test_results_markdown,file==i))
    }
    temp_base_best<<-as.character(all_combinations[j,"base"])
    for(i in strsplit(temp_base_best, ",")[[1]]) {
      test_results_markdown <<- rbind(test_results_markdown,subset(temp_test_results_markdown,file==i))
    }
    
    wighted_comparision(FALSE)
  }
  log_info(paste0("Completed Processing combinations"))
  
  ##Rest Current and base comparison to best possible and start processing
  current_t<-all_combinations[all_combinations[,"error_pct"]==min(all_combinations[,"error_pct"]),]$current
  base_t<-all_combinations[all_combinations[,"error_pct"]==min(all_combinations[,"error_pct"]),]$base
  
  current <<- character(0)
  files <- strsplit(as.character(current_t), ",")[[1]]
  for(file in files){
    if(length(current)==0) {
      current <<- subset(temp_result_filemap,filename==file)$actual_file
    }else{
      current <<- paste0(current,",",subset(temp_result_filemap,filename==file)$actual_file)
    }
  }
  base <<- character(0)
  files <- strsplit(as.character(base_t), ",")[[1]]
  for(file in files){
    if(length(base)==0) {
      base <<- subset(temp_result_filemap,filename==file)$actual_file
    }else{
      base <<- paste0(base,",",subset(temp_result_filemap,filename==file)$actual_file)
    }
    
  }
  
  get_best_of_all_results_for_weighted_summary <<- FALSE
  final_results <<- final_results[0,]
  test_results_markdown <<- test_results_markdown[0,]
  test_errors_markdown<<- test_errors_markdown[0,]
  comparision_results_markdown<<- comparision_results_markdown[0,]
  temp_result_filemap<<- temp_result_filemap[0,]
  base_result_no <<- 1
  current_result_no <<- 1
  
  generate_results()
}


####################################################################################


#####################Copy data files to temp data directory to avoid Network delay during chunk file transfer##########################
create_temp_data_dir <- function(){
  #Delete temp directory - incase if exist
  delete_temp_data_dir()
  #Create temp directory - incase if exist
  dir.create(paste0(tempdir(),"\\data"))
}

copy_data_files_to_temp_dir <- function(dataFile) {
  log_info(paste("Copying file to temp directory: ", dataFile))
  fileName=get_filename(dataFile)
  fileTempNew=paste0(tempdir(),"\\data\\",fileName,".", tools::file_ext(dataFile))
  file.copy(dataFile, fileTempNew,copy.date = TRUE)
  return(fileTempNew)
}

delete_temp_data_dir <- function(){
  unlink(paste0(tempdir(),"\\data"), recursive = TRUE)  
}

copy_data_to_temp_dir <- function(){
  #Copy Current files copy to temp direcory and use files from temp
  files <- strsplit(current, ",")[[1]]
  current<<-""
  for(file in files){
    if(current==""){
      current<<-copy_data_files_to_temp_dir(file)
    }else{
      current<<-paste0(current,",",copy_data_files_to_temp_dir(file))
    }
  }
  
  #Copy Base files copy to temp direcory and use files from temp
  files <- strsplit(base, ",")[[1]]
  base<<-""
  for(file in files){
    if(base==""){
      base<<-copy_data_files_to_temp_dir(file)
    }else{
      base<<-paste0(base,",",copy_data_files_to_temp_dir(file))
    }
  }
}

######################################################################################


#####################  Markdown ########################

#get success samples from results file with EST timezone
get_success_samples <- function(filename) {
  
  data <- read_delim(filename, ",", col_names = TRUE, progress= FALSE, skip_empty_rows=TRUE) %>%
    select(timeStamp,elapsed, label, responseCode) %>%
    subset(responseCode >= 200 & responseCode < 400) %>% 
    # mutate(timeStamp_est=as.POSIXct(timeStamp/1000, origin="1970-01-01", tz="EST"))
    mutate(timeStamp_est= format(as.POSIXct(timeStamp / 1000, origin = "1970-01-01", tz = "EST"), "%Y-%m-%d %H:%M:%OS3"))
  
  if(tolower(algorithm) == "boxplot") {
    boxplot_data=data.frame()
    tran_scope <- data.frame(label=character(),inscope=double())
    transactions <- unique(data[,c("label")])
    
    for(i in transactions) {
      #filter name is Regex check if it matches
      boxplot_tran_data <- boxplot_subset(data,paste0("^",i))[["processed"]]
      boxplot_data <- rbind(boxplot_data,boxplot_tran_data)
      tran_scope <- rbind(tran_scope,data.frame(label=i,inscope=nrow(boxplot_tran_data), stringsAsFactors=FALSE))
    }
    data <- boxplot_data
  }
  return(data)
}

graph_data <- function(current, base, transaction_filter) {
  
  #Current
  curr=get_success_samples(current)
  pr1=curr %>%
    subset(grepl(transaction_filter, label)) %>%
    select("timeStamp_est","elapsed")
  pr2=curr %>% 
    select("timeStamp_est")
  curr=merge(pr1, pr2,by=c('timeStamp_est'), all=TRUE)
  
  #Base
  bs=get_success_samples(base)
  pr1=bs %>%
    subset(grepl(transaction_filter, label)) %>%
    select("timeStamp_est","elapsed")
  pr2=bs %>% 
    select("timeStamp_est")
  bs=merge(pr1, pr2,by=c('timeStamp_est'), all=TRUE)
  
  return(list("current"=curr,"base"=bs))
}

wighted_comparision <- function(addToFinalResult) {
  
  if(get_best_of_all_results_for_weighted_summary){
    temp_curr_aggregated=subset(temp_results_store_best_result,combination==temp_current_best)  
  }
  
  
  if(nrow(temp_curr_aggregated)==0| !get_best_of_all_results_for_weighted_summary){
    curr_aggregated <- subset(test_results_markdown,testType=="current") %>%
      group_by(label) %>% 
      summarise(pass=mean(pass),
                fail=mean(fail),
                avg=mean(avg),
                ci_95=mean(ci_95),
                sd=mean(sd),
                kr=mean(kr),
                discard_pct=mean(discard_pct),
                throughput=mean(throughput),
                connect=mean(connect),
                receivedBytesPerSec=mean(receivedBytesPerSec),
                sentBytesPerSec=mean(sentBytesPerSec))
    
    if(get_best_of_all_results_for_weighted_summary){
      temp_results_store_best_result_t <- curr_aggregated %>%
        mutate(combination=temp_current_best) 
      temp_results_store_best_result <<- rbind(temp_results_store_best_result,temp_results_store_best_result_t)
    }
  }else{
    curr_aggregated <- temp_curr_aggregated[ , -which(names(temp_curr_aggregated) %in% c("combination"))]
  }
  
  if(get_best_of_all_results_for_weighted_summary){
    temp_base_aggregated=subset(temp_results_store_best_result,combination==temp_base_best)  
  }
  
  
  if(nrow(temp_base_aggregated)==0 | !get_best_of_all_results_for_weighted_summary){
    base_aggregated <- subset(test_results_markdown,testType=="base") %>%
      group_by(label) %>% 
      summarise(pass=mean(pass),
                fail=mean(fail),
                avg=mean(avg),
                ci_95=mean(ci_95),
                sd=mean(sd),
                kr=mean(kr),
                discard_pct=mean(discard_pct),
                throughput=mean(throughput),
                connect=mean(connect),
                receivedBytesPerSec=mean(receivedBytesPerSec),
                sentBytesPerSec=mean(sentBytesPerSec))
    
    if(get_best_of_all_results_for_weighted_summary){
      temp_results_store_best_result_t <- base_aggregated %>%
        mutate(combination=temp_base_best) 
      temp_results_store_best_result <<- rbind(temp_results_store_best_result,temp_results_store_best_result_t)
    }
  }else{
    base_aggregated <- temp_base_aggregated[ , -which(names(temp_base_aggregated) %in% c("combination"))]
  }
  
  
  aggregated <- merge(base_aggregated,curr_aggregated,by = "label",all = TRUE)
  aggregated <- aggregated %>%
    mutate(delta=percent_difference(as.double(avg.x),as.double(avg.y))) %>%
    mutate(samples_rpd=percent_difference(as.double(pass.x),as.double(pass.y)))
  
  aggregated <<- aggregated %>% mutate_if(is.numeric, round, digits=2)
  
  aggregated$pass.x<-round(aggregated$pass.x, digits = 0)
  aggregated$fail.x<-round(aggregated$fail.x, digits = 0)
  aggregated$pass.y<-round(aggregated$pass.y, digits = 0)
  aggregated$fail.y<-round(aggregated$fail.y, digits = 0)
 
  aggregated['skip'] <- NA
  aggregated['rt_rpd_chk'] <- NA
  aggregated['check'] <- NA
  aggregated['details'] <- ""
  
  for(i in 1:nrow(aggregated)){
    conf <- config %>%
      subset(grepl("c", type) & grepl(paste0("^",aggregated[i,c("label")]), transaction))
    
    #Load default
    if(nrow(conf)==0){
      conf <- config %>%
        subset(grepl("c", type) & grepl("_default_", transaction))
    }
    
    if(conf[1,c("skip")]){
      aggregated[i,c("details")] <- conf[1,c("comment")]
      aggregated[i,c("skip")] <- TRUE
    }else{
      res <- compare_contitions_check(aggregated[i,c("delta")],aggregated[i,c("samples_rpd")],conf)
      aggregated[i,c("skip")] <- FALSE
      aggregated[i,c("details")] <- paste0(aggregated[i,c("details")],res[["comment"]])
      aggregated[i,c("check")] <- res[["check"]]
      aggregated[i,c("rt_rpd_chk")] <- res[["rt_rpd_chk"]]
    }
  }
  
  err_pct <- get_txn_summary_error_pct(aggregated)
  failure=err_pct>compare_max_txn_error_pct
  
  aggregated$check[aggregated$check == TRUE] <- "PASS"
  aggregated$check[aggregated$check == FALSE] <- "FAIL"
  
  if(addToFinalResult){
    base_name=""
    for(file in unique(subset(test_results_markdown,testType=="base")$file)){
      if(base_name==""){
        base_name <- get_filename(file)
      }else{
        base_name <- paste0(base_name, ", ", get_filename(file))
      }
    }
    current_name=""
    for(file in unique(subset(test_results_markdown,testType=="current")$file)){
      if(current_name==""){
        current_name <- get_filename(file)
      }else{
        current_name <- paste0(current_name, ", ", get_filename(file))
      }
    }
    final_results <<- rbind(final_results,data.frame(baseFilename=base_name,currentFilename=current_name,status=!failure,err_pct=err_pct,type="weighted_comparison"), stringsAsFactors=FALSE)
  }

  # Update error percent when checking best of the results
  if(get_best_of_all_results_for_weighted_summary & !addToFinalResult){
    all_combinations[all_combinations[,"current"]==temp_current_best & all_combinations[,"base"]==temp_base_best,]$error_pct <<- err_pct
  }
  
  return(aggregated)
}

build_html <- function(){
  
  if(enable_html_report){
    
    ##Optimize data for HTML report
    test_results_markdown <<- as.data.frame(test_results_markdown)
    final_results$status[final_results$status == TRUE] <<- "PASS"
    final_results$status[final_results$status == FALSE] <<- "FAIL"
    test_results_markdown$check[test_results_markdown$check == TRUE] <<- "PASS"
    test_results_markdown$check[test_results_markdown$check == FALSE] <<- "FAIL"
    comparision_results_markdown$check[comparision_results_markdown$check == TRUE] <<- "PASS"
    comparision_results_markdown$check[comparision_results_markdown$check == FALSE] <<- "FAIL"

    #round decimals to 2 digits    
    final_results <<- final_results %>% mutate_if(is.numeric, round, digits=2)
    test_results_markdown <<- test_results_markdown %>% mutate_if(is.numeric, round, digits=2)
    comparision_results_markdown <<- comparision_results_markdown %>% mutate_if(is.numeric, round, digits=2)
    
    
    #Change the path with #Sys.getenv("RSTUDIO_PANDOC") - This has to be set explicit
    Sys.setenv(RSTUDIO_PANDOC="C:/dev/install/RStudio/bin/pandoc")
    
    if(weighted_comparision==TRUE){
      aggregated <<- aggregated %>% mutate_if(is.numeric, round, digits=2)
      rmarkdown::render("pass_fail_2.Rmd", output_file = weighted_comparision_file)
    }else{
      rmarkdown::render("pass_fail_2.Rmd", output_file = paste0(get_filename(current),"_summary",".html"), output_dir = dirname(normalizePath(currentTemp)))
    }
  }
}

#####################---Markdown----########################

#################
##Run application
#################


generate_results <- function(){
  if(base!="NA"){
    compr_summary=multi_compare(current,base)
    }else{
      if(!grepl(",", base, fixed=TRUE)) {
        get_results(current,algorithm,TRUE,TRUE)
      }
    }
  
  if(weighted_comparision==TRUE){
    #Best combination is identified and get weighted comparison
    if(get_best_of_all_results_for_weighted_summary){
      if(nrow(comparision_results_markdown)>0){
        log_debug("clear comparision")
        comparision_results_markdown <<-  base::data.frame()
      }
      get_best_results()
    }else{
      aggregated <<- wighted_comparision(TRUE)  
    }
  }
}

run <- function() {
  load_commandLine_args()
  load_config(config_file_transaction,config_file_app)
  initialize_config()
  old_results <<- load_aggregated_result()
  old_errors <<- load_errors_store()
  log_debug("Config initalized")
  
  
  ###Create a copy of current file to be used with generating html report for current\current to base comparisions
  currentTemp<<- current
  if(copy_data_files_to_local_temp){
    #Create Temp data directory
    create_temp_data_dir()
    #Copy files to temporary data directory
    copy_data_to_temp_dir()
    use_filename_as_identifier <<- TRUE
  }
  
  generate_results()
  
  #Remove objects\folders before building html report
  # rm(old_errors, envir = .GlobalEnv)
  # rm(old_results, envir = .GlobalEnv)
  
  #Delete Temp data directory
  if(copy_data_files_to_local_temp){
    log_debug(tempdir())
    delete_temp_data_dir()
  }
  
  build_html()
  
  log_info("************Overall Summary************")
  print(final_results, quote = FALSE, row.names = FALSE)
  log_info("***************************************")
  
  if(base=="NA"){
    check_and_throw_error(final_results)
  }
}



log_debug("Loaded functions to memeory\n\n")

###RUN
run()


#parrallel processing in progess
# library(parallel)
# cn <- parallel::detectCores() - 1
# cl <- parallel::makeCluster(cn, type = "PSOCK")
# envir <- environment(run)
# parallel::clusterExport(cl, varlist = ls(envir), envir = envir)
# parSapply(cl,comb,FUN=parallel_try) # comb is passed on to as argument as x and comb is supposed to be array
# parallel_try <- function(x) {
# stopCluster(cl)


# # # dygraphs graph...........................................................
# library(xts)
# library(lubridate)
# filtered <- subset(data, label=="Login Request") %>%
#   select("timeStamp_est","elapsed")
# filtered$timeStamp_est = ymd_hms(filtered$timeStamp_est)
# don <- xts(x = filtered$elapsed, order.by = filtered$timeStamp_est)
# colnames(don)[2] <- "elapsed"
# dygraph(don)

