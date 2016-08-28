#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

#include<iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <Rcpp.h>
#include <algorithm>   
#include <iostream>     // std::cout
#include <vector>       // std::vector
#include <list>
#include <cmath>        /* log to base exp */
#include <math.h>       /* exp */
#include <time.h>

//https://google.github.io/styleguide/cppguide.html

//Rcpp::plugins(cpp11)]

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

//See <algorithm> header file for various algorithms
//http://www.drdobbs.com/stl-algorithms-vs-hand-written-loops/184401446
//http://www.cplusplus.com/reference/algorithm/
//inlining can be done for functions which are between 3-8 lines long and don't have for loop and switch statements - they will not have overhead

//Examples : 
//Trend : MA, MACD, Parabolic SAR, Put-call ratio
//Momentum : RSI, Stochastics, CCI, MFI, Intraday momentum index, Stochastic momentum index
//Volatility : Bollinger Bands, ATR, Std. Dev., Keltner Channels
//VOlume : Chaikin Oscillator, OBV, ROCV

//TODO : Do research on identifying support and resistance levels, price patterns(price action - they are the only leading indicators), Alligator and Profitunity trading system - said to be very good by some people
//TODO : Write code and strategy in a way that it can be used in other codes as well easily
//TODO : Code Bulkowski's patterns, Candlestick patterns and Western Chart patterns
//TODO : Code some Market Internals indicators
//TODO : Code indicators from Alvarezquanttrading like ConnorsRSI, HiLo Market timing indicator, Cumulative RSI, TRIN etc
//TODO : Code n-bar rules
//TODO : ATR with and without Gap adjustment
//TODO : Slope indicator, linear regression indicator

//Strategies to code : SMA, EMA, FRAMA, RSI, CCI, Supertrend, ATR, ROC, MACD, n-bar rules
//Bulkowski patterns - How to code the pattern : http://thepatternsite.com/HTFStudy.html, http://thepatternsite.com/SmallPatterns.html , http://thepatternsite.com/id75.html, http://thepatternsite.com/id74.html,  http://thepatternsite.com/Trendiness.html, 
//Short term trading strategies that work - RSI-2, TRIN etc
//IBS
//Linda Raschke : 2-period ROC modeling, Taylor modeling, and volatility breakout methods -- http://lindaraschke.net/research/
//VIX/VXV ratio (godotfinance working papers)

//Faster C++ code :
//https://people.cs.clemson.edu/~dhouse/courses/405/papers/optimize.pdf
//https://www.quora.com/How-can-I-reduce-execution-time-on-my-C-C++-code
//Use loops within function instead of function within loops to avoid repeatred calling of function - this has overhead
//Use reference (pointers) for passing containers (including vectors) to functions
//Use inline functions where possible as it has no overhead
//Preinitialized/Preallocated vector is one of the fastest containers - reserve preallocates memory but not initialize
//http://en.cppreference.com/w/cpp/types/numeric_limits/quiet_NaN and http://en.cppreference.com/w/cpp/types/numeric_limits/signaling_NaN

//Advantage of calling functions like SMA on full data is that overhead is avoided by repeated calling of function within main()
//Also, if multiple such functions are there, they can be run parallely in separate threads and then joined within main()

//Since it is better to pass containers by reference to functions to reduce overhead, if we are using functions in separate threads, 
//then normal pass by reference will not work - you need to use std::ref for this
//http://stackoverflow.com/questions/8250932/what-is-the-overhead-of-passing-a-reference
//http://stackoverflow.com/questions/5116756/difference-between-pointer-and-reference-as-thread-parameter
//http://stackoverflow.com/questions/8299545/why-does-passing-object-reference-arguments-to-thread-function-fails-to-compile
//http://jakascorner.com/blog/2016/01/arguments.html
//http://www.bogotobogo.com/cplusplus/multithreaded4_cplusplus11.php

// [[Rcpp::export]]
NumericVector SMA(vector<double> price_vector, double period){
  
//http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:moving_averages

   std::vector<double>::size_type price_vector_size = price_vector.size(); //This is the number of actual objects held in the vector, which is not necessarily equal to its storage capacity.

	//http://stackoverflow.com/questions/8480640/how-to-throw-a-c-exception -- throw statement
	if (period > price_vector_size) { throw std::invalid_argument("period too small for vector size"); } // period can't be greater than the length of data itself

	std::vector<double> sma(price_vector_size);

for (int i = period - 1; i < price_vector_size; ++i)
	{
		double sum = 0;
		for (int j = i - (period - 1); j <= i  ; ++j)
		{
			sum = sum + price_vector[j];
		}

		sma[i] = sum / period;
	}

return wrap(sma);
}


// [[Rcpp::export]]
NumericVector EMA(vector<double> price_vector, double period){

//http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:moving_averages

    std::vector<double>::size_type price_vector_size = price_vector.size(); //This is the number of actual objects held in the vector, which is not necessarily equal to its storage capacity.

  //http://stackoverflow.com/questions/8480640/how-to-throw-a-c-exception -- throw statement
	if (period > price_vector_size) { throw std::invalid_argument("period too small for vector size"); } // period can't be greater than the length of data itself

  std::vector<double> ema(price_vector_size);
  
  double sum_for_sma = 0;
  
  for(int i = 0; i < period; ++i)
  {
    sum_for_sma += price_vector[i];
  }
  
  double sma = sum_for_sma/period;
  ema[period-1] = sma;
  
  double multiplier = 2.0/(1 + period);

	for (int i = period; i < price_vector_size; ++i)
	{
    ema[i] = multiplier * price_vector[i] + (1 - multiplier) * ema[i-1];
	}

return wrap(ema);
}


vector<double> RunningMax(vector<double> price_vector, int period){
  
  std::vector<double>::size_type price_vector_size = price_vector.size(); //This is the number of actual objects held in the vector, which is not necessarily equal to its storage capacity.

  //http://stackoverflow.com/questions/8480640/how-to-throw-a-c-exception -- throw statement
  if (period > price_vector_size) { throw std::invalid_argument("period too small for vector size"); } // period can't be greater than the length of data itself
    
  std::vector<double> run_max(price_vector_size);
  
  //cout<<endl<<"printing max elements"<<endl;
    
    for (int i = period - 1; i < price_vector_size; ++i)
  {
    //http://www.cplusplus.com/reference/algorithm/max_element/
    run_max[i] = *std::max_element(price_vector.begin() + i - (period - 1), price_vector.begin() + i + 1);//note that the way max_element is defined, the second argument is the element just next to the end of range we want to get max from
    //cout << "i = "<<i<<" "<<*(price_vector.begin() + i - (period - 1))<<" "<<*(price_vector.begin() + i + 1)<<" "<<run_max[i]<<endl;
	}
  
  return run_max;
}

vector<double> RunningMin(vector<double> price_vector, int period){
  
  std::vector<double>::size_type price_vector_size = price_vector.size(); //This is the number of actual objects held in the vector, which is not necessarily equal to its storage capacity.

  //http://stackoverflow.com/questions/8480640/how-to-throw-a-c-exception -- throw statement
  if (period > price_vector_size) { throw std::invalid_argument("period too small for vector size"); } // period can't be greater than the length of data itself
    
  std::vector<double> run_min(price_vector_size);
    
  //cout<<endl<<"printing min elements"<<endl;
  
    for (int i = period - 1; i < price_vector_size; ++i)
  {
    //http://www.cplusplus.com/reference/algorithm/max_element/
    run_min[i] = *std::min_element(price_vector.begin() + i - (period - 1), price_vector.begin() + i + 1);//note that the way min_element is defined, the second argument is the element just next to the end of range we want to get min from
    //cout << "i = "<<i<<" "<<*(price_vector.begin() + i - (period - 1))<<" "<<*(price_vector.begin() + i + 1)<<" "<<run_min[i]<<endl;
  }
  return run_min;
}


// [[Rcpp::export]]
NumericVector FRAMA(vector<double> price_vector, int period)
{  
//FRAMA is the best technical indicator according to : http://etfhq.com/blog/2010/05/25/best-technical-indicators/
//FRAMA formula from : https://www.mql5.com/en/code/72 - the N2 in this description should be N2(i) = N(Length,i - Length) not N2(i) = N(Length,i + Length) as the future is not known (it would be look ahead bias at period i as we are considering i + Length periods at period i itself)
//https://quantstrattrader.wordpress.com/2014/06/22/the-continuing-search-for-robust-momentum-indicators-the-fractal-adaptive-moving-average/

   std::vector<double>::size_type price_vector_size = price_vector.size(); //This is the number of actual objects held in the vector, which is not necessarily equal to its storage capacity.

  //http://stackoverflow.com/questions/8480640/how-to-throw-a-c-exception -- throw statement
  if (period > price_vector_size) { throw std::invalid_argument("period too small for vector size"); } // period can't be greater than the length of data itself
  
  if((period%2) != 0) {period -= 1;}

 vector<double> N1(price_vector_size);
 vector<double> N2(price_vector_size);
 vector<double> N3(price_vector_size);
 
 vector<double> run_maxN1 = RunningMax(price_vector, period/2);
 vector<double> run_minN1 = RunningMin(price_vector, period/2);
 //vector<double> run_maxN2 = RunningMax(price_vector, period/2);
 //vector<double> run_minN2 = RunningMin(price_vector, period/2);
 vector<double> run_maxN3 = RunningMax(price_vector, period);
 vector<double> run_minN3 = RunningMin(price_vector, period);
 
  //ofstream myfile;
  //myfile.open ("C:/Users/IshanC/Desktop/IndicatorsTest/example.txt");
 
   //myfile << "N1" << endl;
   
   for (int i = period - 1; i < price_vector_size; ++i)
  {
    N1[i] = (run_maxN1[i] - run_minN1[i])/(period/2);
    //myfile << run_maxN1[i] << ":";
    //myfile << run_minN1[i] << ":";
    //myfile << N1[i] << " ";
  }
  
  //myfile << "N2" << endl;
  
   for (int i = period - 1; i < price_vector_size; ++i)
  {
    N2[i] = (run_maxN1[i-(period/2)] - run_minN1[i-(period/2)])/(period/2);
    //myfile << run_maxN1[i-(period/2)] << ":";
    //myfile << run_minN1[i-(period/2)] << ":";
    //myfile << N2[i] << " ";
  }
  
  //myfile << "N3" << endl;
    
   for (int i = period - 1; i < price_vector_size; ++i)
  {
    N3[i] = (run_maxN3[i] - run_minN3[i])/(period);
    //myfile << run_maxN3[i] << ":";
    //myfile << run_minN3[i] << ":";
    //myfile << N3[i] << " ";
  }
  
   vector<double> D(price_vector_size); // fractal dimension
   vector<double> A(price_vector_size); // exponential smoothing
   vector<double> frama(price_vector_size); // frama
   
   frama[period - 2] = price_vector[period - 2];
   
   //myfile << "frama[period - 2]" << frama[period - 2] << endl;
   
   for (int i = period - 1; i < price_vector_size; ++i)
  {
    D[i] =  (log(N1[i] + N2[i]) - log(N3[i]))/log(2);
    A[i] = exp(-4.6 * (D[i] - 1));
    frama[i] = A[i] * price_vector[i] + (1 - A[i]) * frama[i-1];
    //myfile << "D " << D[i] << ":" << "A " << A[i] << ":" << "frama " << frama[i] << endl;
  }
  
  //myfile.close();
  
  return wrap(frama);
}

// [[Rcpp::export]]
NumericVector RunningRSI(vector<double> price_vector, int period)
{
  //Note that the RSI output will be populated from subscript [period] onwards and unlike SMA, EMA where it is from subscript [period-1]
  //This is because calculation of U and D can start only from period as it represents a change
  //https://en.wikipedia.org/wiki/Relative_strength_index#Calculation
  //http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:relative_strength_index_rsi
  
  std::vector<double>::size_type price_vector_size = price_vector.size(); //This is the number of actual objects held in the vector, which is not necessarily equal to its storage capacity.

  //http://stackoverflow.com/questions/8480640/how-to-throw-a-c-exception -- throw statement
  if (period > price_vector_size) { throw std::invalid_argument("period too small for vector size"); } // period can't be greater than the length of data itself
  
  vector<double> U(price_vector_size);
  vector<double> D(price_vector_size);
  
  U[0] = D[0] = 0;
  
   for (int i = 1; i < price_vector_size; ++i)
  {
    if(price_vector[i] > price_vector[i-1]){
      U[i] = price_vector[i] - price_vector[i-1];
      D[i] = 0;
    } else if(price_vector[i] < price_vector[i-1]){
      U[i] = 0;
      D[i] = price_vector[i-1] - price_vector[i];
    } else {
      U[i] = 0;
      D[i] = 0;
    }
  }
  
  //https://en.wikipedia.org/wiki/Moving_average#Modified_moving_average
    vector<double> SMMA_U(price_vector_size);
    vector<double> SMMA_D(price_vector_size);
    vector<double> RS(price_vector_size);
    vector<double> RSI(price_vector_size);
            
    double sum_for_SMMA_U = 0;
    double sum_for_SMMA_D = 0;
    
     for(int i = 1; i <= period; ++i)
    {
      sum_for_SMMA_U += U[i];
      sum_for_SMMA_D += D[i];
    }
    
    SMMA_U[period] = sum_for_SMMA_U/period;
    SMMA_D[period] = sum_for_SMMA_D/period;
    RS[period] = SMMA_U[period]/SMMA_D[period];
    RSI[period] = ((SMMA_D[period] == 0)?100:(100 - 100/(1 + RS[period])));
    
     for(int i = period+1; i < price_vector_size; ++i)
    {
      SMMA_U[i] = ((period-1)*SMMA_U[i-1] + U[i])/period;
      SMMA_D[i] = ((period-1)*SMMA_D[i-1] + D[i])/period;
      RS[i] = SMMA_U[i]/SMMA_D[i];
      RSI[i] = ((SMMA_D[i] == 0)?100:(100 - 100/(1 + RS[i])));
    }
    
    return wrap(RSI);
}

// [[Rcpp::export]]
void RunningPercentileold(vector<double> price_vector, double nth_percentile, int period)
{ 
  //When repeated calculation of percentile is required - 
  //Faster method could be using 2 heaps : http://stackoverflow.com/questions/3738349/fast-algorithm-for-repeated-calculation-of-percentile
  //Example of median (50th percentile) using 2 heaps : http://www.geeksforgeeks.org/median-of-stream-of-integers-running-integers/
  //Another approach (complete coded function) : http://systematicinvestor.github.io/Run-Quantile-Rcpp
  std::vector<double>::size_type price_vector_size = price_vector.size(); //This is the number of actual objects held in the vector, which is not necessarily equal to its storage capacity.

  //http://stackoverflow.com/questions/8480640/how-to-throw-a-c-exception -- throw statement
  if (period > price_vector_size) { throw std::invalid_argument("period too small for vector size"); } // period can't be greater than the length of data itself

   vector<double> percentile(price_vector_size);
   
   std::nth_element(price_vector.begin(),price_vector.begin()+int(price_vector.size()*nth_percentile),price_vector.end());
   cout<<*(price_vector.begin()+int(price_vector.size()*nth_percentile)); 
   /*
   for (int i = period - 1; i < price_vector_size; ++i)
  {
    //http://www.cplusplus.com/reference/algorithm/max_element/
    std::nth_element(price_vector.begin() + i - (period - 1), price_vector.begin() + i - (period - 1) + int(period*nth_percentile), price_vector.begin() + i);
    percentile[i] = *(price_vector.begin() + i - (period - 1) + int(period*nth_percentile)); 
    cout << "i = "<<i<<" "<<*(price_vector.begin() + i - (period - 1))<<" "<<percentile[i]<<" "<<*(price_vector.begin() + i)<<endl;
  } */
  
  return;
}

// [[Rcpp::export]]
NumericVector RunningPercentile(vector<double> price_vector, double nth_percentile, int period)
{ 
  //When repeated calculation of percentile is required - 
  //Faster method could be using 2 heaps : http://stackoverflow.com/questions/3738349/fast-algorithm-for-repeated-calculation-of-percentile
  //Example of median (50th percentile) using 2 heaps : http://www.geeksforgeeks.org/median-of-stream-of-integers-running-integers/
  //Another approach (complete coded function) : http://systematicinvestor.github.io/Run-Quantile-Rcpp
  std::vector<double>::size_type price_vector_size = price_vector.size(); //This is the number of actual objects held in the vector, which is not necessarily equal to its storage capacity.

  //http://stackoverflow.com/questions/8480640/how-to-throw-a-c-exception -- throw statement
  if (period > price_vector_size) { throw std::invalid_argument("period too small for vector size"); } // period can't be greater than the length of data itself

  //First sort the entire vector
 // std::sort(price_vector.begin(), price_vector.end());
  
   vector<double> percentile(price_vector_size);
   vector<double> period_window_vector(period);
   
   //https://en.wikipedia.org/wiki/Percentile
   double rank = nth_percentile * period;
   
   int int_rank = int(rank);
   double frac_rank = rank - int_rank;
   
   cout<<"rank : "<<rank<<" "<<"int_rank : "<<int_rank<<" "<<"frac_rank : "<<frac_rank<<endl;

   for (int i = period - 1; i < price_vector_size; ++i)
  {
   period_window_vector.assign(price_vector.begin() + i - (period-1), price_vector.begin() + i + 1);
   //for(int j = 0; j < period; ++j){
   //cout<<*(period_window_vector.begin()+j)<< " ";
   //}
   //cout<<endl;
   std::sort(period_window_vector.begin(), period_window_vector.end());
   //for(int j = 0; j < period; ++j){
     //   cout<<*(period_window_vector.begin()+j)<< " ";
   //}
   //cout<<endl;
   //cout<<period_window_vector[int_rank - 1];
   //cout<<" ";
   //cout<<period_window_vector[int_rank];
   //cout<<endl;
   percentile[i] = period_window_vector[int_rank - 1] + frac_rank*(period_window_vector[int_rank] - period_window_vector[int_rank - 1]);
   //cout<<period_window_vector[int_rank - 1] + frac_rank*(period_window_vector[int_rank] - period_window_vector[int_rank - 1])<<endl;
   
 }
 return wrap(percentile);
}

//Connor's RSI
//http://www.marketcalls.in/amibroker/larry-connors-rsi-amibroker-afl-code.html
//https://www.tradingview.com/stock-charts-support/index.php/Connors_RSI_(CRSI)


//RSI : https://en.wikipedia.org/wiki/Relative_strength_index
//https://www.tradingview.com/chart/CELG/EVXQPaR9-Larry-Connors-RSI-2-Trading-System-Surprising-Win-Rate/

//CCI : https://en.wikipedia.org/wiki/Commodity_channel_index

//MACD : 

//ADX : 

//ATR : 

//IBS : 

//SMI (a version of Stochastic Oscillator) : http://www.investopedia.com/ask/answers/021315/what-difference-between-stochastic-oscillator-stochastic-momentum-index.asp
//ROC (Linda Raschke) :

//Supertrend :

//Donchian Channel :

//Bollinger Bands :

/*** R
library(zoo)
library(xts)
#CVB <- read.csv(file="C:/Users/IshanC/Desktop/CVBCRB/Idx_constminvol_10d_0.05.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
CVB <- read.csv(file="C:/Users/IshanC/Desktop/Subsectors/Subsectors2.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
CVB2 <- zoo(CVB)
CVB2 <- na.locf(CVB2)
CVB3 <- data.frame(coredata(CVB2), stringsAsFactors=FALSE)
head(CVB)
head(CVB2)
head(CVB3)
class(CVB$CLOSE.VALUE)
#class(CVB$Last)
#test<-c(7,8,4,15,16,14,13,8,20,17,21,11,90,80, 20, 100, 10, 30, 15)
test<-c(15,13,11,5,7,8,9,3,5)
#test<-c(44.3389,44.0902,44.1497,43.6124,44.3278,44.8264,45.0955,45.4245,45.8433,46.0826,45.8931,46.0328,45.614,46.282,46.282,46.0028,46.0328,46.4116,46.2222,45.6439,46.2122,46.2521,45.7137,46.4515,45.7835,45.3548,44.0288,44.1783,44.2181,44.5672,43.4205,42.6628,43.1314)
#rolling_frama_3 <- FRAMA(as.numeric(CVB3$CLOSE.VALUE), 3)
#rolling_frama_3 <- FRAMA(test, 8)
#RSI <- RSI(test,14)
system.time(
percentile <- RunningPercentile(CVB$Last, .79, 6)
)
#print(percentile)
#write.table(RSI, append = FALSE, file=paste("C:/Users/IshanC/Desktop/IndicatorsTest/RSI", ".csv", sep=""),  sep= ",",  quote=FALSE, row.names=FALSE, col.names = TRUE)
#rolling_sma_6 <- SMA(CVB3$CLOSE.VALUE, 6)
#write.table(cbind(as.numeric(CVB3$CLOSE.VALUE), rolling_ema_3), append = FALSE, file=paste("C:/Users/IshanC/Desktop/IndicatorsTest/rolling_ema_3.1", ".csv", sep=""),  sep= ",",  quote=FALSE, row.names=FALSE, col.names = TRUE)
#system.time(rolling_sma_3 <- RollingSMA(CVB$Last, 3))
#length(rolling_sma_3)
#w=seq(1,10,1)
#RollingSMA(w,3)
*/

/*
// [[Rcpp::export]]
NumericVector Signal(vector<double> short_rolling_sma, vector<double> long_rolling_sma, vector<double> price_vector){
  //http://stackoverflow.com/questions/24112893/within-c-functions-how-are-rcpp-objects-passed-to-other-functions-by-referen
  vector<int> signal;
  signal.reserve(price_vector.size());
  for(int i = 1; i < price_vector.size(); i++){
    if((short_rolling_sma[i-1] < long_rolling_sma[i-1]) & (short_rolling_sma[i] > long_rolling_sma[i]))
    {
      signal[i] = 1;//Go long
    } else if((short_rolling_sma[i-1] > long_rolling_sma[i-1]) & (short_rolling_sma[i] < long_rolling_sma[i]))
    {
      signal[i] = -1;//Go short
    }
  }
}
*/


