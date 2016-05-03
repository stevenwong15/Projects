#=================================================================================
# server
# steven wong, february 2016
#=================================================================================

#---------------------------------------------------------------------------------
# local variables

GDP_NAICS <- 
  GDP_Ranked %>% 
  filter(Position == 2) %>%
  group_by(PRICES, DATE) %>%
  mutate(VALUE_PER = VALUE_BILLION / sum(VALUE_BILLION, na.rm=T) * 100) %>%
  ungroup()

GDP_NAICS_detail <- 
  GDP_Ranked %>% 
  filter(With_Leaf == 0) %>%
  group_by(PRICES, DATE) %>%
  mutate(VALUE_PER = VALUE_BILLION / sum(VALUE_BILLION, na.rm=T) * 100) %>%
  ungroup()

#---------------------------------------------------------------------------------
# server

function(input, output, session){

  # detailed NAICS 
  NAICS_detail_choices <- reactive({

    NAICS_broad <-
      GDP_NAICS %>%
      filter(NAICS %in% input$naicsType) %>%
      select(Parent) %>%
      distinct(Parent)

    NAICS_detail <-
      GDP_Ranked %>%
      filter(Parent %in% NAICS_broad$Parent) %>%
      filter(With_Leaf == 0) %>%
      select(NAICS) %>%
      distinct(NAICS)

    NAICS_detail$NAICS

  })
  observe({
    updateSelectInput(session, 
                     'naicsType_detail', 
                     choices = NAICS_detail_choices(),
                     select = '21: COAL MINING')
  })

  # GDP overall
  output$plot_ts <- renderDygraph({
    
  	GDP_Total_Range <-
  	  GDP_Ranked %>% 
      filter(PRICES == input$priceType) %>%
      filter(Position == 0) %>%
      select(DATE, NAICS, VALUE_BILLION) %>%
      spread(NAICS, VALUE_BILLION)

    GDP_Plot <- xts(GDP_Total_Range[,-1], order.by = GDP_Total_Range$DATE)

  	dygraph(GDP_Plot, 
           group = 'gdp',
           main = "GDP (1997-01 to 2015-11)") %>%
    dyOptions(colors='black') %>%
    dyRangeSelector(height = 40, 
                    keepMouseZoom = F, 
                    fillColor = '', 
                    strokeColor = 'gray') %>%
    dyAxis("y", label = "GDP ($Billions CAD)")

  })

  # NAICS percentage
  output$plot_ts_per <- renderDygraph({
    
    GDP_NAICS_subset <-
      GDP_NAICS %>% 
      filter(PRICES == input$priceType) %>%
      filter(NAICS %in% input$naicsType)

    GDP_NAICS_subset_max <-
      GDP_NAICS_subset %>%
      group_by(DATE) %>%
      summarise(VALUE_PER = sum(VALUE_PER))

    GDP_NAICS_Range <-
      GDP_NAICS_subset %>%
      select(DATE, NAICS, VALUE_PER) %>%
      spread(NAICS, VALUE_PER)

    GDP_Per_Plot <- xts(GDP_NAICS_Range[,-1], order.by = GDP_NAICS_Range$DATE)

    dygraph(GDP_Per_Plot, 
            group = 'gdp',
            main = 'GDP Percentage Split by NAICS') %>%
    dyOptions(stackedGraph = T,
              fillAlpha = 0.5,
              includeZero = T,
              colors = RColorBrewer::brewer.pal(11, "RdBu")) %>%
    dyHighlight(highlightCircleSize = 3, 
                highlightSeriesBackgroundAlpha = 0.75,
                hideOnMouseOut = F,
                highlightSeriesOpts = list(strokeWidth = 2)) %>%
    dyLegend(width = 750) %>%
    dyAxis("y", label = "% GDP", valueRange = c(0, max(GDP_NAICS_subset_max$VALUE_PER)+5))

  })

  # NAICS detailed percentage
  output$plot_ts_detail_per <- renderDygraph({
    
    GDP_NAICS_detail_subset <-
      GDP_NAICS_detail %>% 
      filter(PRICES == input$priceType) %>%
      filter(NAICS %in% input$naicsType_detail)

    GDP_NAICS_detail_subset_max <-
      GDP_NAICS_detail_subset %>%
      group_by(DATE) %>%
      summarise(VALUE_PER = sum(VALUE_PER))

    GDP_NAICS_detail_Range <-
      GDP_NAICS_detail_subset %>% 
      select(DATE, NAICS, VALUE_PER) %>%
      spread(NAICS, VALUE_PER)

    GDP_detail_Per_Plot <- xts(GDP_NAICS_detail_Range[,-1], order.by = GDP_NAICS_detail_Range$DATE)

    dygraph(GDP_detail_Per_Plot, 
            group = 'gdp',
            main = 'GDP Percentage Split by Detailed NAICS') %>%
    dyOptions(stackedGraph = T,
              fillAlpha = 0.5,
              includeZero = T,
              colors = RColorBrewer::brewer.pal(11, "RdBu")) %>%
    dyHighlight(highlightCircleSize = 3, 
                highlightSeriesBackgroundAlpha = 0.75,
                hideOnMouseOut = F,
                highlightSeriesOpts = list(strokeWidth = 2)) %>%
    dyLegend(width = 750) %>%
    dyAxis("y", label = "% GDP", valueRange = c(0, max(GDP_NAICS_detail_subset_max$VALUE_PER)+2.5)) %>%
    dyEvent('2007-01-01', 'Change in Aggregation (Note 1)', labelLoc = 'bottom')

  })

}
