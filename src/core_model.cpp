
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

Rcpp::List core_model(std::vector<double> ddt, std::vector<double> r1, std::vector<double> r2, 
    std::vector<double> r3, std::vector<double> f4, const double& a0, const double& u0)
{
    const int l = ddt.size();
    std::vector<double> a(l);
    std::vector<double> u(l);
    std::vector<double> e(l);
    double ati0 = a0;
    double uti0 = u0;
    double eti = 0.0;
    double ati, uti;

    for (int i = 0; i < l; i++) {
        // Make incorporation transfer (at *start* of interval) (if none then f4 = 1 and ati = a[i])
        ati = f4[i] * ati0;
        uti = (1 - f4[i]) * ati0 + uti0;

        // Calculate pools at *end* of ct[i]
        a[i] = ati * std::exp(-(r1[i] + r2[i]) * ddt[i]);
        u[i] = std::exp(-r3[i] * ddt[i]) * (r2[i] * ati * (std::exp((-r1[i] - r2[i] + r3[i]) * ddt[i]) - 1)/(-r1[i] - r2[i] + r3[i]) + uti);
        e[i] = eti + (uti - u[i]) +  (ati - a[i]);

        // save pools for next step
        ati0 = a[i];
        uti0 = u[i];
        eti = e[i];
    }

	return Rcpp::List::create(
		_["a"] = a,
		_["u"] = u,
		_["e"] = e);
}

