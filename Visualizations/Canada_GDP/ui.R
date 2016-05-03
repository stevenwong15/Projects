#=================================================================================
# ui
# steven wong, february 2016
#=================================================================================

fluidPage(
  theme = 'boostrap.css',

  fluidRow(
    
    column(3,
      titlePanel('Canadian Gross domestic product (GDP), 
                  by North American Industry Classification System (NAICS)'),
      selectInput(
        'priceType',
        'Select: Type of Prices',
        choices = prices,
        width = '100%'
        )
      ),

    column(9,
      dygraphOutput('plot_ts')
      )
    ),

  fluidRow(

    column(3,
      selectizeInput(
        'naicsType', 
        'Select: Industry by North American Industry Classification System (NAICS)', 
        choices = NAICS, 
        multiple = TRUE,
        selected = '21: MINING, QUARRYING, AND OIL AND GAS EXTRACTION',
        width = '100%'
        )
    ),
    
    column(9,
      dygraphOutput('plot_ts_per')
      )
  
  ),

  fluidRow(

    column(3,
      selectizeInput(
        'naicsType_detail', 
        'Drill Down: Industry by Detailed NAICS (within selected industries)', 
        choices = '21: COAL MINING', 
        multiple = TRUE,
        select = '21: COAL MINING',
        width = '100%'
        )
    ),
    
    column(9,
      dygraphOutput('plot_ts_detail_per')
      )
  
  ),

  fluidRow(
    column(12,
     'Note from Statistics Canada:'
     )
    ),
  fluidRow(
    column(12,
     '1. At the lowest level of detail, it may not be 
     possible to produce a homogeneous series from 1997 to the present. Only 
     industries and certain aggregates that provide good continuity back to 1997 
     have data from 1997 to 2006.'
     )
    )
)
