#' DNA extract QC
#'
#' Quality control for DNA extraction with standard format and re-test format.
#'
#' @usage
#' DNAextractQC(path = choose.files, LowerOD = 1.65, UpperOD = 2.20,
#' DNAcon = 10, outPath, type, db = NULL)
#'
#' @param path the name of the file which the data are to be read from.
#' (default: \code{choose.files}).
#' @param LowerOD lower limit of OD260/280 ratio (default: 1.65).
#' @param UpperOD upper limit of OD260/280 ratio (default: 2.20).
#' @param DNAcon DNA concentration (default: 10 µg/ml).
#' @param outPath a character string naming a output path.
#' @param type 1 : standard format/ 2 : retest format/ 3 : retest format (old).
#' @param db connection handle returned by \code{RODBC::odbcConnect}.
#' (default: \code{NULL}).
#' @export
#'
#' @import dplyr

DNAextractQC<-
  function(path= choose.files(), LowerOD= 1.65, UpperOD= 2.2, DNAcon= 10,
           outPath, type, db = NULL){

    options(scipen = 999)

    if (type == 1){
      rightFileName= grepl('^OpticsSampleData.*csv$', basename(path))
    } else if (type == 2){
      rightFileName= grepl('^DNA.*re.*test.*csv$', basename(path))
    }

    if (rightFileName){
      if (type == 1){
        line<-
          unlist(strsplit(readLines(path, skipNul = T,
                                    encoding = 'UTF-16LE')[3], '\t'))
        value<-
          line[3:(length(line)-1)] %>% matrix(., ncol = 5, byrow = T) %>%
          data.frame(.,stringsAsFactors = F)
        df1<-
          data.frame(extr_date = format(lubridate::ymd(line[1]), '%Y/%m/%d'),
                     extr_time = line[2],
                     extr_type = line[length(line)],
                     lapply(value, as.numeric),
                     extr_qc_time= Sys.time() %>% as.character(),
                     stringsAsFactors = F) %>%
          filter(!is.na(X1))
      } else if (type == 2){

        tryCatch(
          df<- read.csv(path, fileEncoding = 'UTF-16', sep = '\t'),
          warning = function(w){ df<- data.table::fread(path)},
          error = function(e){ df<- data.table::fread(path)}
          )

        Date= format(lubridate::ymd(stringr::str_extract(df[4,1], '\\d.*')), '%Y/%m/%d')
        time= format(lubridate::ymd_hms(
          paste(lubridate::today(), stringr::str_extract(df[5,1], '\\d.*'))), '%H:%M:%S')
        
        value<- data.frame(workid = as.character(unlist(df[13:nrow(df),2])),
                           ng_ul = as.numeric(unlist(df[13:nrow(df),3])),
                           a_260 = as.numeric(unlist(df[13:nrow(df),4])),
                           a_280 = as.numeric(unlist(df[13:nrow(df),5])),
                           a_260_280 = a_260/a_280
        )
        
        df1<- cbind.data.frame(Date = Date, Time = time, Type = 'DNA', value,
                               extrQC_time= Sys.time() %>% as.character()
        )
      } else if (type == 3){
        
        tryCatch(
          df<- read.csv(path, fileEncoding = 'UTF-16', sep = '\t')%>% .[,1:10],
          warning = function(w){ df<- data.table::fread(path) %>% .[,1:10] },
          error = function(e){ df<- data.table::fread(path) %>% .[,1:10] }
        )
        
        date_time= lubridate::mdy_hms(df$Date)
        Date= format(date_time, '%Y/%m/%d')
        time= format(date_time, '%H:%M:%S')
        
        value<- df[, c(2:4,6,7)]
        df1<-
          cbind.data.frame(Date = Date, Time = time, Type = 'DNA', value,
                           extrQC_time= Sys.time() %>% as.character()
          )
      }

      names(df1)[4:8]<- c('workid', 'ng_ul', '_260_280', '_260', '_280')

      DNAextract_Pass<-
        filter(df1, between(`_260_280`, LowerOD, UpperOD), `ng_ul` >= DNAcon) %>%
        mutate(status = 'Pass')

      DNAextract_Fail<-
        filter(df1, !workid %in% DNAextract_Pass$workid) %>%
        mutate(status = 'Fail')

      date= format(Sys.Date(), '%Y%m%d')
      min= min(df1$workid)
      max= max(df1$workid)

      if (type == 1){FileType = '_DNAextract'
      } else if (type == 2){FileType = '_retestDNA'}
      passFileName= paste0(date, FileType, '_Pass_', min, '_', max, '.csv')
      failFileName= paste0(date, FileType, '_Fail_', min, '_', max, '.csv')

      write.csv(DNAextract_Pass,
                file.path(outPath, 'Pass',  passFileName), row.names = F)

      if (!is.null(db)){
        # Write to ODBC
        RODBC::sqlSave(db, DNAextract_Pass, tablename = 'DNAextract_Pass',
                       append = T, varTypes = c(workid = 'int')
                       )
        }

      cat(crayon::bgBlue("== 完成 =="), "檔案名稱:", passFileName,
          paste(crayon::bgGreen('PASS:'), nrow(DNAextract_Pass), '支'),
          '\n', sep = '\n')

      if (nrow(DNAextract_Fail)!=0){
        write.csv(DNAextract_Fail,
                  file.path(outPath, 'Fail',  failFileName), row.names = F)

        if (!is.null(db)){
          # Write to ODBC
          RODBC::sqlSave(db, DNAextract_Fail, tablename = 'DNAextract_Fail',
                         append = T, varTypes = c(workid = 'int')
                         )
          }

        cat(crayon::bgBlue("== 完成 =="), "檔案名稱:", failFileName,
            paste(crayon::bgRed('FAIL:'), nrow(DNAextract_Fail), '支'),
            '\n', sep = '\n')
        print(DNAextract_Fail[,c(1:6)])
      }

    } else {
      cat(crayon::bgRed('== 檔案格式錯誤 ==\n'),
          crayon::bgBlue('== 結束程序 ==\n'))
    }
    if (!is.null(db)) {RODBC::odbcClose(db)}
  }

#DNAextractQC()

