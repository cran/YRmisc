
#' @name xd.fred.tickers
#' @aliases xd.fred.tickers
#' @title Federal Reserve Bank of St. Louis Economic Data Tickers
#' @description This function returns a data contains information of data name, type and tickers
#' @usage xd.fred.tickers()
#' @examples xd.fred.tickers()


xd.fred.tickers <- function(){

  Name <- c("Consumer Price Index for All Urban Consumers: All Items",
            "Real Gross Domestic Product",
            "Industrial Production Index",
            "10-Year Treasury Constant Maturity Rate",
            "10-Year Treasury Constant Maturity Rate",
            "10-Year Treasury Constant Maturity Rate",
            "U.S. / Euro Foreign Exchange Rate",
            "Civilian Unemployment Rate",
            "All Employees: Total Nonfarm Payrolls",
            "4-Week Moving Average of Initial Claims",
            "Effective Federal Funds Rate",
            "Effective Federal Funds Rate",
            "Effective Federal Funds Rate",
            "Civilian Unemployment Rate")
  Frequency <- c("Monthly",
                 "Quarterly",
                 "Monthly",
                 "Daily",
                 "Weekly",
                 "Monthly",
                 "Daily",
                 "Monthly",
                 "Monthly",
                 "Weekly",
                 "Monthly",
                 "Daily",
                 "Weekly",
                 "Monthly")
  Ticker <- c("CPIAUCSL",
              "A191RL1Q225SBEA",
              "INDPRO",
              "DGS01",
              "WGS10YR",
              "GS10",
              "DEXUSEU",
              "UNRATE",
              "PAYEMS",
              "IC4WSA",
              "FEDFUNDS",
              "DFF",
              "FF",
              "UNRATE")

  df <- as.data.frame(cbind(Name, Frequency, Ticker))

  #df

  return(df)
}
