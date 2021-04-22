#' Dilut2Tecan
#'
#' Create Tecan format (.gwl) and samples list (.txt).
#'
#' @usage
#' Dilut2Tecan(outPath, db, array, start, appendix = F, exclude = F, n.out = 95)
#' @param outPath a character string naming a output path.
#' @param db connection handle returned by `RODBC::odbcConnect`.
#' @param array array number.
#' @param start the first work ID to be diluted.
#' @param appendix a file containing re-diluted, manual diluted and exclusion samples.
#' @param exclude a *logical* value indicating whether the samples to be excluded.
#' @param n.out the maximum number of sample.
#' @export
#'
#' @import dplyr

Dilut2Tecan<-
  function(outPath, db, array, start, appendix = F, exclude = F, n.out= 95){

    options(scipen = 999)

    # 從 SQL 端查詢 QCpasslist 中 DNA 濃度>25 (dilut_pass = 1)
    df1<-
      RODBC::sqlQuery(db,
      "SELECT
      extr_date, extr_time, extr_type, workid, ng_ul,
      _260_280, _260, _280, extr_qc_time, status,
      dna, h2o, dilut_pass, MIN(box_time)
      FROM QCpasslist
      WHERE dilut_pass = 1
      GROUP BY workid
      ORDER BY workid;"
      )

    if (appendix){
      apdx = file.choose()
      # 讀入"重測"和"手動"的工作號
      retest = readxl::read_excel(apdx, sheet = 1) %>% .$Retest %>% as.numeric(.)
      manual = readxl::read_excel(apdx, sheet = 2) %>% .$Manual %>% as.numeric(.)

      # 讀入排除的工作號，並將 df1 中存在需排除的工作號排除
      if (exclude){
        ex<- readxl::read_excel(apdx, sheet = 3) %>% .$Exclude %>% as.numeric(.)
        df1<- df1 %>% filter(!workid %in% ex$Exclude)
      }
    } else {
      retest = c()
      manual = c()
    }

    filter.range<-
      function(st, n , data){
        srt = which( data$workid == st)
        end = srt + n - 1
        data$workid[srt:end]
      }

    main=
      filter.range(st = start,
                   n = n.out-(length(retest) + length(manual)),
                   data = df1)
    all= c(main, retest, manual)

    List.all<-  data.frame(workid = all, list_seq = 1:length(all))

    List2Txt<-
      df1 %>%
      inner_join(., List.all, by = 'workid') %>%
      arrange(list_seq) %>%
      mutate(seq = if_else(list_seq%%8== 0, 8, list_seq%%8),
             array = array,
             dilut_time = Sys.time() %>% as.character() )

    List2Gwl<- List2Txt %>% filter( workid %in% c(main, retest) )

    check.list<-
      data.frame(time = Sys.time(), array = array,
                 start = start, end = max(List2Gwl$workid),
                 retest = paste(retest, collapse = ';'),
                 manual = paste(manual, collapse = ';') )

    warning1= '需手動稀釋的清單中，存在 DNA 濃度 PASS 者，仍要繼續請輸入 [Yes]:'
    warning2= '需重測或手動稀釋的清單中，有不存在的workid，請檢查輸入，仍要繼續請輸入 [Yes]:'
    Ans1= ''
    Ans2= ''

    if ( any(df1$workid %in% manual) ){
      Ans1= readline(cat(caryon::bgRed('[警告]'),warning1))
      if ( Ans1 != 'Yes' ){ cat(caryon::bgBlue("== 取消匯出 ==")) }
    }

    if( !any(df1$workid %in% c(retest, manual)) | Ans1 == 'Yes' ){
      if ( nrow(List2Txt) != n.out ){
        Ans2 = readline(cat(caryon::bgRed('[警告]'),warning2))
        if ( Ans2 != 'Yes' ){ cat(caryon::bgBlue("== 取消匯出 ==")) }
      }

      if (nrow(List2Txt) == n.out | Ans2 == 'Yes'){
        # ---- create gwl ----
        n.gwl= nrow(List2Gwl)
        temp<-
          do.call(rbind, replicate(n.gwl, template.gwl()$body, simplify = F)) %>%
          mutate(n = 1:nrow(.),
                 row = if_else(n%%6== 0, 6, n%%6),
                 pos = ((n-1)%/%6)+1 )

        gwl<-
          left_join(temp, List2Gwl, by = c('pos' = 'list_seq')) %>%
          mutate(x2 = if_else(is.na(x2)&row!=1, pos, seq),
                 x4 = if_else(row==1, h2o, x4),
                 x4 = if_else(row==2, dna, x4),
                 done = if_else(row %in% seq(1:5), paste0(x1,x2,x3,x4,x5), x1)
                 ) %>%
          select(done) %>%
          rbind(template.gwl()$head, .)

        # ---- write gwl ----
        Name.gwl<-
          paste0('Array_', array, '_Dilut_',
                 List2Gwl[1,4],'_',List2Gwl[nrow(List2Gwl),4], '.gwl')

        write.table(gwl, file.path(outPath, 'gwl', Name.gwl),
                    quote = F, col.names = F, row.names = F)

        cat(caryon::bgBlue("== 完成 =="), "(.gwl) 檔案名稱:", Name.gwl,
            paste('此清單中有:', n.gwl, '支\n'), sep = '\n')

        # ---- create & write txt (pos, worknumber) ----
        Name.txt<-
          paste0('Array_',array,'_Dilut_',
                 List2Txt[1,4],'_',List2Txt[nrow(List2Txt),4], '.txt')

        txt<- List2Txt %>% mutate(done = paste0(list_seq,';',workid)) %>% select(done)
        csv<- List2Txt %>% select(-list_seq, -seq)
        write.table(txt, file.path(outPath, 'txt', Name.txt ),
                    quote = F, col.names = F, row.names = F)

        # Write to ODBC
        RODBC::sqlSave(db, csv, tablename = 'Dilut2Tecan', append = T,
                       varTypes = c(workid = 'int', array = 'int')
                       )

        cat(caryon::bgBlue("== 完成 =="), "(.txt) 檔案名稱:", Name.txt,
            paste('此清單中有:', nrow(txt), '支\n'), sep = '\n')

        # Write check.list
        write.table(check.list, file.path(outPath, "check.list.csv"), sep = ",",
                    col.names = !file.exists(file.path(outPath, "check.list.csv")),
                    row.names = F, append = T)

        cat(caryon::bgBlue("== 註 =="),
            "自動/重測/手動稀釋紀錄於\n檔案名稱: check.list.csv", sep = '\n')
      }

    }
    RODBC::odbcClose(db)
  }

#Dilut2Tecan()

