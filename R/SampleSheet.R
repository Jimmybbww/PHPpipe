#' SampleSheet
#'
#' Create sample sheet for Gene Titan Array Plate Registration format file.
#'
#' @usage
#' SampleSheet(txt= file.choose(), outPath, db = NULL, GT, plt, barcode)
#' @param txt the name of the file which the data are to be read from.
#' (default: \code{choose.files}).
#' @param outPath a character string naming a output path.
#' @param db connection handle returned by \code{RODBC::odbcConnect}.
#' (default: \code{NULL}).
#' @param GT machine number.
#' @param plt array number.
#' @param barcode scanning the barcode on the array.
#' @export
#'
#' @import dplyr

SampleSheet<-
  function(txt= file.choose(), outPath, db = NULL, GT, plt, barcode){

    options(scipen = 999)

    tempPath=
      system.file("extdata",
                  "GeneTitanArrayPlateRegistration.xls", package = "PHPpipe")

    temp  <- readxl::read_excel(tempPath, sheet = 1)
    sheet2<-
      readxl::read_excel(tempPath, sheet = 2) %>%
      cbind.data.frame(` ` = rep('',nrow(.)),.)
    sheet3<- readxl::read_excel(tempPath, sheet = 3)

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

        openxlsx::write.xlsx(
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
          RODBC::sqlSave(db, SampleSheet1, tablename = 'SampleSheet', append = T)
        }

        cat(crayon::bgBlue("== 完成 =="), "檔案名稱:",
            paste0('GeneTitanArrayPlateRegistration', fileName, '.xls'),
            paste('共:', nrow(SampleSheet), '支 (含 Control)\n'), sep = '\n')
      } else {
        cat(crayon::bgRed('[警告]'), '所選檔案檢體數量不為 95 支',
            crayon::bgBlue("== 結束 =="))
      }

    } else {
      cat(crayon::bgRed('[警告]'),
          '所選檔案格式不正確 or 檔案與輸入的盤號不相符\n')
    }
    if (!is.null(db)){RODBC::odbcClose(db)}
  }

#SampleSheet()

