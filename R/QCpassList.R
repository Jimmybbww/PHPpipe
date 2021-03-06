#' QCpassList
#'
#' Check pass list after quality control for DNA extraction.
#'
#' @usage QCpassList(outPath, db, myFirst, myLast)
#' @param outPath a character string naming a output path.
#' @param db connection handle returned by \code{RODBC::odbcConnect}.
#' @param myFirst scanning the first work ID barcode on white box.
#' @param myLast scanning the last work ID barcode on white box.
#' @export
#'
#' @import dplyr

QCpassList<-
  function(outPath, db, myFirst, myLast, th_dna= 800, th_h2o= 80, th_con= 25){

    options(scipen = 999)

    if (myFirst > myLast) {stop("首支編號不得大於末支編號 (myFirst > myLast)")}

    myList= c(myFirst:myLast)
    n.MyList= (myLast- myFirst)+1

    df <-
      RODBC::sqlQuery(db,
      "SELECT
      extr_date, extr_time, extr_type, workid, ng_ul,
      _260_280, _260, _280, MIN(extr_qc_time), status
      FROM DNAextract_Pass
      GROUP BY workid
      ORDER BY workid;"
      )

    QCpassList<-
      df %>%
      mutate(dna = th_dna/ng_ul,
             h2o = th_h2o - dna,
             dilut_pass = if_else(ng_ul >= th_con, 1, 0),
             box_time= Sys.time() %>% as.character() ) %>%
      filter(workid %in% myList)

    n.Sample= nrow(QCpassList)

    manual<- QCpassList %>% filter(dilut_pass == 0)

    n.manual= nrow(manual)

    if (n.MyList != 100){
      cat(crayon::bgRed("[警告]"), "需求數目為:", n.MyList, "支, 超過/不足 100 支\n")
    }

    if (n.MyList != n.Sample){
      cat(crayon::bgRed("[警告]"), "需求數目為:", n.MyList, "支, 實際數目:",
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
        RODBC::sqlSave(db, QCpassList, tablename = 'QCpassList', append = T,
                       varTypes = c(workid = 'int')
                       )

        cat(crayon::bgBlue("== 完成 =="), "檔案名稱:", FileName, sep = '\n')

        if (n.manual != 0){
          write.csv(manual, file.path(outPath, 'PassList', ManualName), row.names = F)
          cat(crayon::bgBlue("== 需手動稀釋 =="), "檔案名稱:", ManualName,
              paste(n.manual, '支'), sep = '\n')
        }

      } else {
        cat(crayon::bgBlue("== 取消匯出 =="))
      }

    } else {
      write.csv(QCpassList, file.path(outPath, 'PassList', FileName), row.names = F)

      # Write to ODBC
      RODBC::sqlSave(db, QCpassList, tablename = 'QCpassList', append = T,
                     varTypes = c(workid = 'int')
                     )

      cat(crayon::bgBlue("== 完成 =="), "檔案名稱:", FileName, sep = '\n')

      if (n.manual != 0){
        write.csv(manual, file.path(outPath, 'PassList', ManualName), row.names = F)
        cat(crayon::bgBlue("== 需手動稀釋 =="), "檔案名稱:", ManualName,
            paste(n.manual, '支'), sep = '\n')
      }

    }
    RODBC::odbcClose(db)
  }

#QCpassList()

