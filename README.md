In this project we build a data set of dfs prices for mobile banking, mobile money and fintechs across a range of roughly 23 countries.

We integrate IPA data, alongside our own research to do this. IPA data can be found at this link: https://dfs-prices.poverty-action.org/data-methodology.html

Our methodology is as follows:
1. For each country we assess the top providers for electronic money tranfsers in banking and mobile money sectors. We classify firms that provide these services as fintechs,
if they are non-bank and do not have an agent network. We aim to have at least 3 firms in each sector. 
2. We assess the IPA data for completeness regarding this list.
3. If the IPA data omits a provider, or has not been able to scrape these prices, we integrate this data. We do so by manually writing the data in in R.
4. Exchange rate data was recorded at the time that the data was written in. At a later stage in the project, we decided to pull exchange rate data using the qunatmod package in R.

To reproduce our results, download the IPA data set and integrate this into your working directory before running the file. 

