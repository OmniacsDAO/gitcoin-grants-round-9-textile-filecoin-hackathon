## Load libraries
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
options(scipen=9999)

## API address
api_url <- "https://minerindex.hub.textile.io/v1/"


## Query miners
api_d <- list()
offset=0
limit=50
while(TRUE)
{
	api_res <- GET(paste0(api_url,"index/query?limit=",limit,"&sort.ascending=false&sort.field=TEXTILE_DEALS_TOTAL_SUCCESSFUL&offset=",offset))
	if(length(content(api_res,"parsed")$miners)==0) break()
	api_d <- c(api_d,content(api_res,"parsed")$miners)
	offset <- offset+40
	message(length(api_d))
}
null_to_na <- function(x) ifelse(is.null(x),NA,x)
parse_miner <- function(x)
{
	m_data <- data.frame(
							miner_address = x$miner$minerAddr,
							miner_location = x$miner$metadata$location,
							miner_updatedAt = null_to_na(x$miner$updatedAt),
							fc_relativePower = x$miner$filecoin$relativePower,
							fc_askPrice = x$miner$filecoin$askPrice,
							fc_askVerifiedPrice = x$miner$filecoin$askVerifiedPrice,
							fc_minPieceSize = x$miner$filecoin$minPieceSize,
							fc_maxPieceSize = x$miner$filecoin$maxPieceSize,
							fc_activeSectors = x$miner$filecoin$activeSectors,
							fc_faultySectors = x$miner$filecoin$faultySectors,
							fc_updatedAt = null_to_na(x$miner$filecoin$updatedAt),
							tt_deal_total = x$miner$textile$dealsSummary$total,
							tt_deal_last = null_to_na(x$miner$textile$dealsSummary$last),
							tt_deal_failure = x$miner$textile$dealsSummary$failures,
							tt_deal_failure_last = null_to_na(x$miner$textile$dealsSummary$lastFailure),
							tt_retrieval_total = x$miner$textile$retrievalsSummary$total,
							tt_retrieval_last = null_to_na(x$miner$textile$retrievalsSummary$last),
							tt_retrieval_failure = x$miner$textile$retrievalsSummary$failures,
							tt_retrieval_failure_last = null_to_na(x$miner$textile$retrievalsSummary$lastFailure),
							tt_updatedAt = null_to_na(x$miner$textile$updatedAt)
						)

	if(length(x$miner$textile$regions)==0) return(m_data)
	r21_data <- data.frame(
							r21_deal_total = x$miner$textile$regions$`021`$deals$total,
							r21_deal_last = null_to_na(x$miner$textile$regions$`021`$deals$last),
							r21_deal_failure = x$miner$textile$regions$`021`$deals$failures,
							r21_deal_failure_last = null_to_na(x$miner$textile$regions$`021`$deals$lastFailure),
							r21_retrieval_total = x$miner$textile$regions$`021`$retrievals$total,
							r21_retrieval_last = null_to_na(x$miner$textile$regions$`021`$retrievals$last),
							r21_retrieval_failure = x$miner$textile$regions$`021`$retrievals$failures,
							r21_retrieval_failure_last = null_to_na(x$miner$textile$regions$`021`$retrievals$lastFailure),
							r21_deal_tfr_rate = paste0(sapply(x$miner$textile$regions$`021`$deals$tailTransfers,"[[",1),";",sapply(x$miner$textile$regions$`021`$deals$tailTransfers,"[[",2),collapse="<->"),
							r21_retrieval_tfr_rate = paste0(sapply(x$miner$textile$regions$`021`$retrievals$tailTransfers,"[[",1),";",sapply(x$miner$textile$regions$`021`$retrievals$tailTransfers,"[[",2),collapse="<->")
						)
	if(r21_data$r21_deal_tfr_rate==";") r21_data$r21_deal_tfr_rate <- NA
	if(r21_data$r21_retrieval_tfr_rate==";") r21_data$r21_retrieval_tfr_rate <- NA

	return(cbind(m_data,r21_data))

}
miner_data <- do.call(bind_rows,lapply(api_d,parse_miner))
unique_miner <- unique(miner_data$miner_address)
unique_miner_idx <- match(unique_miner,miner_data$miner_address)
miner_data <- miner_data[unique_miner_idx,]

saveRDS(miner_data,"~/Desktop/filecoin/filecoin_miner_data.RDS")


# ## Deal Query
# selected_miners <- miner_data$miner_address[1]
# data_size_bytes <- 1000000
# num_days <- 180
# api_res <- GET(paste0(api_url,"calculator/calculate?dataSizeBytes=",data_size_bytes,"&durationDays=",num_days,"&",paste0("minerAddresses=",selected_miners,collapse="&")))
# content(api_res,"parsed")


# ## Read in miner data
# m_d <- readRDS("~/Desktop/filecoin/filecoin_miner_data.RDS")





