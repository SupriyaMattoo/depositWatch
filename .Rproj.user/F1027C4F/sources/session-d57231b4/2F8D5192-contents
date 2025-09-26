#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
DataFrame rollingStats(NumericVector x, int window) {
  int n = x.size();
  NumericVector roll_mean(n, NA_REAL);
  NumericVector roll_sd(n, NA_REAL);

  double sum = 0.0;
  double sumsq = 0.0;

  for (int i = 0; i < n; i++) {
    sum += x[i];
    sumsq += x[i] * x[i];

    if (i >= window) {
      sum -= x[i - window];
      sumsq -= x[i - window] * x[i - window];
    }

    if (i >= window - 1) {
      double mean = sum / window;
      double var = (sumsq / window) - (mean * mean);
      if (var < 0) var = 0; // numerical stability
      roll_mean[i] = mean;
      roll_sd[i] = sqrt(var);
    }
  }

  return DataFrame::create(
    Named("roll_mean") = roll_mean,
    Named("roll_sd") = roll_sd
  );
}


