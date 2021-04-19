#---- f.DNAextractQC ----
f.DNAextractQC<-
  function(path= choose.files(), LowerOD= 1.65, UpperOD= 2.2, DNAcon= 10,
           outPath, type, db = NULL){

    library(dplyr)
    library(data.table)
    library(lubridate)
    library(crayon)
    library(RODBC)
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
          data.frame(extr_date = format(ymd(line[1]), '%Y/%m/%d'),
                     extr_time = line[2],
                     extr_type = line[length(line)],
                     lapply(value, as.numeric),
                     extr_qc_time= Sys.time() %>% as.character(),
                     stringsAsFactors = F) %>%
          filter(!is.na(X1))
      } else if (type == 2){
        #df<- fread(path) %>% .[,1:10]
        df<- read.csv(path, fileEncoding = 'UTF-16', sep = '\t') %>% .[,1:10]
        date_time= mdy_hms(df$Date)
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
        sqlSave(db, DNAextract_Pass, tablename = 'DNAextract_Pass', append = T,
                varTypes = c(workid = 'int')
                )
        }

      cat(bgBlue("== 完成 =="), "檔案名稱:", passFileName,
          paste(bgGreen('PASS:'), nrow(DNAextract_Pass), '支'), '\n', sep = '\n')

      if (nrow(DNAextract_Fail)!=0){
        write.csv(DNAextract_Fail,
                  file.path(outPath, 'Fail',  failFileName), row.names = F)

        if (!is.null(db)){
          # Write to ODBC
          sqlSave(db, DNAextract_Fail, tablename = 'DNAextract_Fail', append = T,
                  varTypes = c(workid = 'int')
                  )
          }

        cat(bgBlue("== 完成 =="), "檔案名稱:", failFileName,
            paste(bgRed('FAIL:'), nrow(DNAextract_Fail), '支'), sep = '\n')
        print(DNAextract_Fail[,c(1:6)])
      }

    } else {
      cat(bgRed('== 檔案格式錯誤 ==\n'), bgBlue('== 結束程序 ==\n'))
    }
    if (!is.null(db)) {odbcClose(db)}
  }

#f.DNAextractQC()

