## Load Libraries
library(shiny)
library(shinydashboard)
library(shinyalert)
library(ISOcodes)
library(highcharter)
library(DT)
library(lubridate)
options(scipen=999)

# setwd("~/Desktop/filecoin")
# shiny::runApp()

## Load Countries geojson
m_d <- readRDS("filecoin_miner_data.RDS")
country_df <- data.frame(ID=unique(m_d$miner_location),Country_Code=ISO_3166_1$Alpha_3[match(unique(m_d$miner_location),ISO_3166_1$Alpha_2)],Country_Name=ISO_3166_1$Name[match(unique(m_d$miner_location),ISO_3166_1$Alpha_2)])
country_df <- country_df[complete.cases(country_df),]
m_d$miner_location_A3 <- country_df$Country_Code[match(m_d$miner_location,country_df$ID)]
m_d$miner_location_Country <- country_df$Country_Name[match(m_d$miner_location,country_df$ID)]
m_d$fc_askPrice <- as.numeric(m_d$fc_askPrice)/10^18
m_d$fc_askVerifiedPrice <- as.numeric(m_d$fc_askVerifiedPrice)/10^18
m_d$fc_minPieceSize <- as.numeric(m_d$fc_minPieceSize)/1024^3
m_d$fc_maxPieceSize <- as.numeric(m_d$fc_maxPieceSize)/1024^3

## Cleaning functions
tfr_rate <- function(x)
{
	res <- sapply(unlist(strsplit(x,"<->")),function(x) as.numeric(strsplit(x,";")[[1]][2]))[which.max(sapply(unlist(strsplit(x,"<->")),function(x) as_datetime(strsplit(x,";")[[1]][1])))]
	if(length(res)==0) return(NA)
	paste(round(res,4),"MiB/s")
}

function(input, output, session)
{
	shinyalert(
				title = "Welcome Textile Community!",
				text="As part of the Gitcoin Round 9 textile Hackathon our team took on the task of creating a Miner Index Visual Explorer.  Using data from the new Filecoin Miner Index API, we've developed a collection of visualizations to help network enthusiasts get a high level overview of the health and performance of storage miners. The default page contains a global breakdown of miner activity and decentralization as measured by the percentage of miner power and the number of miners. Here performance is measured through the number of faulty sectors and retrieval failures.  In the Miner Analytics & Insights section we compute distributions to highlight group performance and expose outlying values that may be of interest. Lastly, in the Miner Comparison tab, we allow the users to compare and contrast individual miners directly.  We hope this application is not only insightful, but helps guide the community's data driven decision making and transparency efforts.",

			)
	observeEvent(input$htu, {
									shinyalert(
												title="Introduction",
												text = "The default page contains a global breakdown of miner activity and decentralization as measured by the percentage of miner power and the number of miners. Here performance is measured through the number of faulty sectors and retrieval failures.  In the Miner Analytics & Insights section we compute compute distributions to highlight group performance and expose outlying values that may be of interest. Lastly, in the Miner Comparison tab, we allow the users to compare and contrast individual miners directly. We hope this is a useful high level overview of the health and performance of storage miners on the Filecoin network.",
												type = "info"
											)
								}
				)

	#################################################################################
	## Spatial Miners Info
	#################################################################################
	spt_values <- reactiveValues(data_plot = NULL,data_map=NULL)
	
	output$sel_met_ui <- renderUI({
									if(input$tabs!="spatial") return(NULL)
									selectInput(
										"select_column",
										label = NULL,#h5("Select Metric"), 
										choices = c(
														"Number of Miners",
														"Miner Power %",
														"Number of Active Sectors",
														"Number of Faulty Sectors",
														"Number of Deals",
														"Number of Deal Failures",
														"Number of Retrievals",
														"Number of Retrieval Failures"
													), 
										selected = "Miner Power"
									)
							})
	output$hchart <- renderHighchart({
										if(is.null(input$select_column)) return(NULL)
										data_plot <- m_d[!is.na(m_d$miner_location_A3),]
										data_plot_s <- split(data_plot,data_plot$miner_location_A3)
										data_map <- data.frame(
																Country_A3 = names(data_plot_s),
																Country = sapply(data_plot_s,function(x) x$miner_location_Country[1]),
																`Number of Miners` = sapply(data_plot_s,nrow),
																`Miner Power %` = round(sapply(data_plot_s,function(x) sum(x$fc_relativePower))*100,4),
																`Number of Active Sectors` = sapply(data_plot_s,function(x) sum(as.numeric(x$fc_activeSectors))),
																`Number of Faulty Sectors` = sapply(data_plot_s,function(x) sum(as.numeric(x$fc_faultySectors))),
																`Number of Deals` = sapply(data_plot_s,function(x) sum(as.numeric(x$tt_deal_total))),
																`Number of Deal Failures` = sapply(data_plot_s,function(x) sum(as.numeric(x$tt_deal_failure))),
																`Number of Retrievals` = sapply(data_plot_s,function(x) sum(as.numeric(x$tt_retrieval_total))),
																`Number of Retrieval Failures` = sapply(data_plot_s,function(x) sum(as.numeric(x$tt_retrieval_failure))),
																check.names=FALSE
															)
										rownames(data_map) <- NULL
										spt_values$data_plot <- data_plot
										spt_values$data_map <- data_map
										hcmap(
												"custom/world-robinson-lowres", 
												data = data_map,
												name = gsub("Number of ","",input$select_column), 
												value = input$select_column,
												borderWidth = 0,
												nullColor = "#d3d3d3",
												joinBy = c("iso-a3", "Country_A3")
											) %>%
										hc_colorAxis(stops = color_stops(colors = viridisLite::inferno(8))) %>%
										hc_mapNavigation(enabled = TRUE) %>%
										hc_subtitle(text = paste(
											"Based on the data for the",
											nrow(data_plot),
											"miners out of",
											nrow(m_d),
											"miners, Who have specified their location and the location supplied is a valid Country Alpha-2 code."
										)) %>%
										hc_title(text=input$select_column) %>%
										hc_legend(layout = "vertical", align = "right")

								})
	output$spat_tab <- renderDataTable({
											if(is.null(spt_values$data_map)) return(NULL)
											data_dt <- spt_values$data_map[,-1]
											data_dt <- data_dt[order(data_dt$`Miner Power %`,decreasing=TRUE),c(1, 3, 2,4:9)]

											DT::datatable(data_dt,options = list(pageLength = 50, dom = 't',ordering=T),rownames= FALSE)
										})
	#################################################################################
	#################################################################################


	#################################################################################
	## Specific Miner Info
	#################################################################################
	output$sel_min_ui <- renderUI({
									if(input$tabs!="miner") return(NULL)
									selectizeInput(
													'sel_min', 
													label = NULL,#h5("Select Miner"), 
													choices = m_d$miner_address,
													multiple=TRUE,
													options = list(maxItems = 6),
													selected=m_d$miner_address[1]
												)
							})

	output$min_tab <- renderDataTable({
										# if(length(input$sel_min)<2) return(NULL)
										min_d <- t(m_d[m_d$miner_address %in% input$sel_min,])
										colnames(min_d) <- min_d[1,]
										min_d <- min_d[-1,,drop=FALSE]
										min_d <- min_d[c(31,2:9,11:18,28:29),,drop=FALSE]
										rownames(min_d) <- c(
																"Country",
																"Last Updated",
																"Miner Power %",
																"Ask Price (FIL)",
																"Ask Price Verified (FIL)",
																"Min Piece Size",
																"Max Piece Size",
																"Num Active Sectors",
																"Num Faulty Sectors",
																"Total Deals",
																"Last Deal",
																"Total Deals Failure",
																"Last Deal Failure",
																"Total Retrievals",
																"Last Retrieval",
																"Total Retrievals Failure",
																"Last Retrieval Failure",
																"Last Deal Transfer Rate",
																"Last Retrieval Transfer Rate"
															)
										min_d["Miner Power %",] <- paste(round(as.numeric(min_d["Miner Power %",])*100,4),"%")
										min_d["Last Updated",] <- as.character(as_datetime(min_d["Last Updated",]))
										min_d["Ask Price (FIL)",] <- ifelse(is.na(min_d["Ask Price (FIL)",]),NA,paste(min_d["Ask Price (FIL)",],"FIL"))
										min_d["Ask Price Verified (FIL)",] <- ifelse(is.na(min_d["Ask Price Verified (FIL)",]),NA,paste(min_d["Ask Price Verified (FIL)",],"FIL"))
										min_d["Min Piece Size",] <- paste(min_d["Min Piece Size",],"GB")
										min_d["Max Piece Size",] <- paste(min_d["Max Piece Size",],"GB")
										min_d["Last Deal",] <- as.character(as_datetime(min_d["Last Deal",]))
										min_d["Last Deal Failure",] <- as.character(as_datetime(min_d["Last Deal Failure",]))
										min_d["Last Retrieval",] <- as.character(as_datetime(min_d["Last Retrieval",]))
										min_d["Last Retrieval Failure",] <- as.character(as_datetime(min_d["Last Retrieval Failure",]))
										min_d["Last Deal Transfer Rate",] <- sapply(min_d["Last Deal Transfer Rate",],tfr_rate)
										min_d["Last Retrieval Transfer Rate",] <- sapply(min_d["Last Retrieval Transfer Rate",],tfr_rate)
										DT::datatable(min_d,options = list(scrollX = TRUE,pageLength = 50, dom = 't',ordering=F))
							})
	#################################################################################
	#################################################################################


	#################################################################################
	## Insights
	#################################################################################
	in_values <- reactiveValues(col_sel_r = NA)
	output$sel_met_in <- renderUI({
									if(input$tabs!="insights") return(NULL)
									selectInput(
										"select_column_in",
										label = NULL,#h5("Select Metric"), 
										choices = c(
														"Miner Power %",
														"Ask Price (FIL)",
														"Ask Price Verified (FIL)",
														"Min Piece Size",
														"Max Piece Size",
														"Num Active Sectors",
														"Num Faulty Sectors",
														"Total Deals",
														"Total Deals Failure",
														"Total Retrievals",
														"Total Retrievals Failure"
													), 
										selected = "Miner Power"
									)
							})

	output$in_qtl <- renderDataTable({
										if(is.null(input$select_column_in)) return(NULL)
										data_plot <- m_d
										if(input$select_column_in=="Miner Power %") in_values$col_sel_r <- "fc_relativePower"
										if(input$select_column_in=="Ask Price (FIL)") in_values$col_sel_r <- "fc_askPrice"
										if(input$select_column_in=="Ask Price Verified (FIL)") in_values$col_sel_r <- "fc_askVerifiedPrice"
										if(input$select_column_in=="Min Piece Size") in_values$col_sel_r <- "fc_minPieceSize"
										if(input$select_column_in=="Max Piece Size") in_values$col_sel_r <- "fc_maxPieceSize"
										if(input$select_column_in=="Num Active Sectors") in_values$col_sel_r <- "fc_activeSectors"
										if(input$select_column_in=="Num Faulty Sectors") in_values$col_sel_r <- "fc_faultySectors"
										if(input$select_column_in=="Total Deals") in_values$col_sel_r <- "tt_deal_total"
										if(input$select_column_in=="Total Deals Failure") in_values$col_sel_r <- "tt_deal_failure"
										if(input$select_column_in=="Total Retrievals") in_values$col_sel_r <- "tt_retrieval_total"
										if(input$select_column_in=="Total Retrievals Failure") in_values$col_sel_r <- "tt_retrieval_failure"
										qtl_df <- as.data.frame((quantile(as.numeric(data_plot[,in_values$col_sel_r]),1:10/10,na.rm=TRUE)))
										names(qtl_df) <- "Quantile Distribution"
										DT::datatable(qtl_df,options = list(dom = 't',ordering=F))
									})

	output$hchart_dis <- renderHighchart({
											if(is.null(input$select_column_in)) return(NULL)
											data_plot <- m_d
											hchart(
													as.numeric(data_plot[,in_values$col_sel_r]),
													breaks=ifelse(length(table(as.numeric(data_plot[,in_values$col_sel_r])))>20,20,length(table(as.numeric(data_plot[,in_values$col_sel_r])))),
													name = input$select_column_in
												) %>% 
											hc_yAxis(type = "logarithmic",title = list(text = "Count")) %>%
											hc_xAxis(title = list(text = input$select_column_in)) %>%
											hc_title(text = paste(input$select_column_in,"Histogram (Logarithmic Y Axis)")) %>%
											hc_legend(enabled=FALSE)

										})

	output$num_na_box <- renderValueBox({
											if(is.null(input$select_column_in)) return(valueBox(NA,NA))
											data_plot <- m_d
											valueBox(
														paste0(
																sum(is.na(as.numeric(data_plot[,in_values$col_sel_r]))),
																"/",
																length(data_plot[,in_values$col_sel_r])
															),
														"Missing Values",
														icon=icon("times-circle")
    												)
										})

	output$num_nz_box <- renderValueBox({
											if(is.null(input$select_column_in)) return(valueBox(NA,NA))
											data_plot <- m_d
											valueBox(
														paste0(
																sum(as.numeric(data_plot[,in_values$col_sel_r])==0,na.rm=TRUE),
																"/",
																length(data_plot[,in_values$col_sel_r])
															),
														"Zero Values",
														icon=icon("creative-commons-zero")
    												)
										})

	output$avg_box <- renderValueBox({
											if(is.null(input$select_column_in)) return(valueBox(NA,NA))
											data_plot <- m_d
											valueBox(
														formatC(mean(as.numeric(data_plot[,in_values$col_sel_r]),na.rm=TRUE),digits=2,format="e"),
														"Average Value",
														icon=icon("creative-commons-zero")
    												)
										})
	#################################################################################
	#################################################################################
}







