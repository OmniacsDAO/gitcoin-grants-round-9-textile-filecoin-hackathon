## Load Libraries
library(shiny)
library(shinydashboard)
library(shinyalert)
library(ISOcodes)
library(highcharter)
library(DT)
library(lubridate)
options(scipen=999)

dashboardPage(
	dashboardHeader(
						title = "Miner Index Visual Explorer",
						titleWidth = 300,
						tags$li(a(
									href = 'https://textile.io',
									target="_blank",
									img(src = 'textile-logo-3.png',title = "Textile", height = "30px"),
									style = "padding-top:10px; padding-bottom:10px;"
								),
						class = "dropdown"
						),
						tags$li(a(
									href = 'https://filecoin.io',
									target="_blank",
									img(src = 'filecoin-logo.svg',title = "Filecoin", height = "30px"),
									style = "padding-top:10px; padding-bottom:10px;"
								),
						class = "dropdown"
						)
					),

    dashboardSidebar(
    	tags$head(tags$style(HTML('.logo {
                              background-color: rgb(0, 144, 255) !important;
                              }
                              .navbar {
                              background-color: rgb(0, 144, 255) !important;
                              }
                              '))),
		sidebarMenu(
			id = "tabs",
			# h5(HTML(" Before Spacial Miner Distribution add<br/>&nbsp;some sample text.  I'll fill in an<br/>  introduction there")),
			# hr(),
			menuItem("Miner Spacial Statistics", tabName = "spatial", icon = icon("map-marked")),
			uiOutput("sel_met_ui"),
			hr(),
			menuItem("Miner Analytics & Insights", tabName = "insights", icon = icon("chart-line")),
			uiOutput("sel_met_in"),
			hr(),
			menuItem("Miner Comparison", tabName = "miner", icon = icon("searchengin")),
			uiOutput("sel_min_ui"),
			hr(),
			fluidRow(
    					column(2, offset = 0),
						column(2, offset = 0, actionButton(inputId = "htu", label = HTML("&nbsp;&nbsp;Introduction"), icon = icon("info"))),
						column(2, offset = 0)
					)
		)
	),

	dashboardBody(
		useShinyalert(),
		tabItems(
			tabItem(tabName = "spatial",
				fluidRow(
					column(width = 12,
						highchartOutput("hchart"),
						dataTableOutput("spat_tab")
					)
				)
			),
			tabItem(tabName = "miner",
				fluidRow(
					column(width = 12,
						dataTableOutput("min_tab")
					)
				)
			),
			tabItem(tabName = "insights",
				fluidRow(
					column(width = 9,
						highchartOutput("hchart_dis"),
						valueBoxOutput("num_na_box"),
						valueBoxOutput("num_nz_box"),
						valueBoxOutput("avg_box")
					),
					column(width = 3,
						dataTableOutput("in_qtl")
					)
				)
			)
		)
	)
)