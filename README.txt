#Price and Volatility Forecast with SLR

Requires R to run. Libraries are presented in the code.

DISCLAIMER: This was done as an exercise for Finance Economics - don't expect any useful results from this.

Process is as simple as it gets:
 * Gets historical data on specified time period (Price and Volatility).
 * Gets the time period to run regression on (5 days - 5 points for regression, etc.)
 * Runs simple linear regression on both of these.
 * Returns results - If both slopes positive - hype is still strong, buy. If both slopes negative - it's about to go up, and etc. code is commented, figure it out yourself.
 * Adds visual graphs to illustrate this effect.
 
Also, at the end of the code, there is empirical data, used on the real-time "Top Stock Gainers" stocks to see how much real results it would produce.
(+2.6% lol)
