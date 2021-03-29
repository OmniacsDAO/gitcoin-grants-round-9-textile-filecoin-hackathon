									# pal <- colorNumeric(palette = "YlGnBu",domain = data_map[[input$select_column]])
										# leaflet(map_data) %>%
										# addTiles() %>%
										# addPolygons(
										# 				stroke = FALSE,
										# 				smoothFactor = 0,
										# 				fillOpacity = .8,
										# 				fillColor = ~pal(data_map[[input$select_column]]),
										# 				label = ~paste0(data_map$Country, ": ", data_map[[input$select_column]])
										# 		)%>%
										# addLegend(
										# 			title = gsub("Number of ","",input$select_column),
										# 			pal = pal,
										# 			values = ~data_map[[input$select_column]],
										# 			opacity = 1.0
    						# 					)



hcboxplot(
  outliers = TRUE,
  x = as.numeric(data_plot$fc_minPieceSize)/10^9,
  var = data_plot$miner_location_A3,
  name = "Length"
) %>%
hc_chart(type = "column") %>%
  hc_title(text = "AA") %>%
  hc_yAxis(title = list(text = "Height in metre"))
  

hchart(m_d$fc_askPrice,breaks=20) %>% hc_yAxis(type = "logarithmic") %>% hc_xAxis(type = "logarithmic")



  Distributions: relative power, min size, max size, active sectors, faulty sectors, text tile total, failed deals, distribution of miner locations, distribution of the time since the last success
Statistics: number of miners with non-0 power, num of miners with at least 1 active sector, num of active miners with at least 1 faulty sector, number of active sectors by location
Plots: askPrice vs askVerifiedPrice, heatmap of miner locations