#---- f.SampleSheet ----
f.SampleSheet<-
  function(txt= file.choose(), tempPath, outPath, db = NULL, GT, plt, barcode){

    library(dplyr)
    library(lubridate)
    library(readxl)
    library(openxlsx)
    library(crayon)
    options(scipen = 999)

    tempName= file.path(tempPath, 'GeneTitanArrayPlateRegistration.xls')

    temp  <- read_excel(tempName, sheet = 1)
    sheet2<-
      read_excel(tempName, sheet = 2) %>%
      cbind.data.frame(` ` = rep('',nrow(.)),.)
    sheet3<- read_excel(tempName, sheet = 3)

    array= txt %>% basename() %>% strsplit(.,'_') %>% unlist() %>% .[2]

    if ( array == plt & grepl('.txt', txt) ){
      DNA<- read.csv(txt, header = F, sep = ';')

      if (nrow(DNA) == 95){
        t_DNA<-
          as.matrix(DNA[,2]) %>% rbind(., 9999999) %>%
          matrix(., ncol = 12) %>% t() %>%
          matrix(., ncol = 1)

        today= format(Sys.Date(), '%Y%m%d')

        SampleSheet<-
          temp %>%
          mutate(Project = 'Default',
                 `Sample File Name` =
                   paste0(today, '_GT', GT, '_ARRAY_', plt, '_',
                          `Probe Array Position`, '_', cbind(t_DNA) ),
                 `Sample File Name` =
                   gsub('H12_9999999','H12_Control', `Sample File Name`),
                 `Array Name` = `Sample File Name`,
                 `Sample File Path` =
                   paste0('D:\\Command_Console\\Data\\',
                          today, '_GT', GT, '_ARRAY_', plt),
                 Barcode = as.character(barcode) )

        fileName= paste0(today, '_GT', GT,  '_ARRAY_', plt)
        folderName= file.path(outPath, paste0('GT', GT), fileName)

        dir.create(folderName, showWarnings = F)

        write.xlsx(
          list('Samples' = SampleSheet,
               'DO_NOT_EDIT' = sheet2,
               'DO_NOT_EDIT_TEMPLATE_INFO' = sheet3),
          file.path(folderName,
                    paste0('GeneTitanArrayPlateRegistration', fileName, '.xls'))
          )

        if (!is.null(db)){
          # Write to ODBC
          SampleSheet1<-
            SampleSheet %>%
            mutate(SplSheet_time = Sys.time() %>% as.character())
          sqlSave(db, SampleSheet1, tablename = 'SampleSheet', append = T)
        }

        cat(bgBlue("== 完成 =="), "檔案名稱:",
            paste0('GeneTitanArrayPlateRegistration', fileName, '.xls'),
            paste('共:', nrow(SampleSheet), '支 (含 Control)\n'), sep = '\n')
      } else {
        cat(bgRed('[警告]'), '所選檔案檢體數量不為 95 支', bgBlue("== 結束 =="))
      }

    } else {
      cat(bgRed('[警告]'), '所選檔案格式不正確 or 檔案與輸入的盤號不相符\n')
    }
    if (!is.null(db)){odbcClose(db)}
  }

# f.SampleSheet()

