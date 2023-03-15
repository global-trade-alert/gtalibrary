#include <Rcpp.h>
using namespace Rcpp;

/*
gta_code_converter_cpp() is used in gta_hs_vintage_converter() and XX
to speed up the function. Instead of looping over every code that must be converted,
checking if it matches with a vintage hs code and then appending the results to a list in R,
this loop is implemented in c++.
Arguments:
    - codes_vinage & codes_2012: Two string vectors from the conversion matrix in gtalibrary::hs_vintages
    --> before passing them to the function, make sure that the pairs are unique
    - codes_convert: the vector of codes that should be converted to HS2012
    - message: If true, also returns a list of unconverted codes
Warning:
    The function does not check if the aruments satisfy all necessary conditions (eg. type, size etc.).
    This is implemented in R in gta_hs_vintage_converter() before the arguments are passed to gta_code_converter_cpp()
    Thus, it is not recommended to use this function outside of gta_hs_vintage_converter()
*/
// [[Rcpp::export]]
List datefunction(const DateVector &start, const DateVector &end, const Date &current_date, const bool current_year_todate = true)
{
    // define parameters
    const int n = start.size();
    const int current_year = current_date.getYear();
    const int day_today = current_date.getYearday();
    List list_years_valid(n);
    List list_share_of_year(n);
    Date start_date;
    Date end_date;

    // loop over each element in the input vectors
    for (int i = 0; i < n; ++i)
    {
        start_date = start[i];
        end_date = end[i];

        // Check for NAs in start and end
        if (start_date.is_na() || end_date.is_na())
        {
            throw std::invalid_argument("start and end cannot contain and NAs");
        }
        // Ensure that Start date is before end date
        if (start_date >= end_date)
        {
            throw std::invalid_argument("make sure that the start date is before the end date");
        }

        // calculate year  / day of year for start and end date
        const int year_start = start_date.getYear();
        const int year_end = end_date.getYear();
        const double day_start = start_date.getYearday();
        const double day_end = end_date.getYearday();

        std::vector<int> year_valid(year_end - year_start + 1);
        // R equivalent to seq(from = year_start, to = year_end, by = 1)
        std::iota(year_valid.begin(), year_valid.end(), year_start);
        // generate vector of same length as year_valid and prepopulate with 1s (shares for the first and last year are modified below)
        std::vector<double> share_of_year(year_end - year_start + 1, 1.0);

        // ensure right calculation of intra year duration for different cases
        if (year_end != year_start)
        {
            share_of_year[0] = (365.0 - day_start) / 365.0;
            share_of_year[year_end - year_start] = (current_year_todate && (year_end == current_year)) ? day_end / day_today : day_end / 365.0;
        }
        else
        {
            share_of_year[0] = (current_year_todate && (year_end == current_year)) ? (day_end - day_start) / day_today : (day_end - day_start) / 365.0;
        }

        // append results to list
        list_years_valid[i] = year_valid;
        list_share_of_year[i] = share_of_year;
    }

    // return list consisting of two lists (year in force and share of year)
    return List::create(Named("year_in_force") = list_years_valid, Named("share_of_year") = list_share_of_year);
}