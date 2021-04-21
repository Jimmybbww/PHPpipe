#' template.gwl
#'
#' Create template of Tecan format.
#' @export
#'
#' @importFrom dplyr

template.gwl<-
  function(){

    # ---- gwl head ----
    head<-
      tibble(done = c('B;', 'B;',
                      'C; ----==== Make the quantitation plate ====----',
                      'C; ----==== pass 2 ====----', 'B;')
      )

    # ---- gwl body ----
    body<-
      tibble(
        x1 = c('A;H2O;;Trough 100ml;',
               'A;DNA;;Eppendorf 100 Pos.;',
               'D;Dil;;MTPDil;',
               'A;Dil;;MTPDil;',
               'D;Dest;;Deepwell96;',
               'W;'
        ),
        x2 = c(NA, #change
               NA, #change
               NA, #change
               NA, #change
               NA, #change
               NA
        ),
        x3 = c(';;',
               ';;',
               ';;',
               ';;',
               ';;',
               ''
        ),
        x4 = c(NA, #change
               NA, #change
               80.0000,
               20,
               20,
               NA
        ),
        x5 = c(';Water1',
               ';DNA',
               ';DNA',
               ';Water1',
               ';Water1',
               ''
        )
      )
    return(list('head' = head, 'body' = body))
  }


