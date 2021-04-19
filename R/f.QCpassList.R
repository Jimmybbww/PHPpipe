#---- f.QCpassList ----
f.QCpassList<-
  function(outPath, db, myFirst, myLast){

    library(dplyr)
    library(data.table)
    library(crayon)
    options(scipen = 999)

    if (myFirst > myLast) {stop("首支編號不得大於末支編號 (myFirst > myLast)")}

    myList= c(myFirst:myLast)
    n.MyList= (myLast- myFirst)+1

    df <-
      sqlQuery(db,
               "SELECT
               extr_date, extr_time, extr_type, workid, ng_ul,
               _260_280, _260, _280, MIN(extr_qc_time), status
               FROM DNAextract_Pass
               GROUP BY workid
               ORDER BY workid;"
      )

    QCpassList<-
      df %>%
      mutate(dna = 800/ng_ul,
             h2o = 80 - dna,
             dilut_pass = if_else(ng_ul >= 25, 1, 0),
             box_time= Sys.time() %>% as.character() ) %>%
      filter(workid %in% myList)

    n.Sample= nrow(QCpassList)

    manual<- QCpassList %>% filter(dilut_pass == 0)

    n.manual= nrow(manual)

    if (n.MyList != 100){
      cat(bgRed("[警告]"), "需求數目為:", n.MyList, "支, 超過/不足 100 支\n")
    }

    if (n.MyList != n.Sample){
      cat(bgRed("[警告]"), "需求數目為:", n.MyList, "支, 實際數目:",
          sprintf('%03s', n.Sample), '支\n')
    }

    date= format(Sys.Date(), '%Y%m%d')
    min= min(QCpassList$workid)
    max= max(QCpassList$workid)
    FileName= paste0(date, '_PassList_', min, '_', max, '.csv')
    ManualName= paste0(date, '_NeedManual_', min, '_', max, '.csv')

    if(n.MyList != 100| n.MyList != n.Sample){

      if (readline("仍要輸出請輸入 [Yes]: ") == 'Yes'){
        write.csv(QCpassList, file.path(outPath, 'PassList', FileName), row.names = F)

        # Write to ODBC
        sqlSave(db, QCpassList, tablename = 'QCpassList', append = T,
                varTypes = c(workid = 'int')
        )

        cat(bgBlue("== 完成 =="), "檔案名稱:", FileName, sep = '\n')

        if (n.manual != 0){
          write.csv(manual, file.path(outPath, 'PassList', ManualName), row.names = F)
          cat(bgBlue("== 需手動稀釋 =="), "檔案名稱:", ManualName,
              paste(n.manual, '支'), sep = '\n')
        }

      } else {
        cat(bgBlue("== 取消匯出 =="))
      }

    } else {
      write.csv(QCpassList, file.path(outPath, 'PassList', FileName), row.names = F)

      # Write to ODBC
      sqlSave(db, QCpassList, tablename = 'QCpassList', append = T,
              varTypes = c(workid = 'int')
      )

      cat(bgBlue("== 完成 =="), "檔案名稱:", FileName, sep = '\n')

      if (n.manual != 0){
        write.csv(manual, file.path(outPath, 'PassList', ManualName), row.names = F)
        cat(bgBlue("== 需手動稀釋 =="), "檔案名稱:", ManualName,
            paste(n.manual, '支'), sep = '\n')
      }

    }
    odbcClose(db)
  }

# f.QCpassList()

