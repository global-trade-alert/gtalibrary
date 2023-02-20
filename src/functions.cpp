// datefunction()
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List datefunction(const DateVector &start, const DateVector &end, const Date &current_date, const bool current_year_todate = true)
{
    const int n = start.size();
    const int current_year = current_date.getYear();
    const int day_today = current_date.getYearday();
    List list_years_valid(n);
    List list_share_of_year(n);
    Date start_date;
    Date end_date;

    for (int i = 0; i < n; ++i)
    {
        start_date = start[i];
        end_date = end[i];

        // Check for NAs in start and end
        if (start_date.is_na() || end_date.is_na())
        {
            throw std::invalid_argument("start and end cannot contain and NAs");
        }
        // control for errors
        if (start_date >= end_date)
        {
            throw std::invalid_argument("make sure that the start date is before the end date");
        }

        const int year_start = start_date.getYear();
        const int year_end = end_date.getYear();
        const int day_start = start_date.getYearday();
        const int day_end = end_date.getYearday();

        NumericVector year_valid(year_end - year_start + 1);
        std::iota(year_valid.begin(), year_valid.end(), year_start);
        std::vector<double> share_of_year(year_end - year_start + 1, 1.0);

        const double share_start = (365.0 - day_start) / 365.0;
        double share_end;

        share_end = (current_year_todate && (year_end == current_year) ? day_end / day_today : day_end / 365.0);
        share_of_year[0] = share_start;
        if (year_end > year_start)
        {
            share_of_year[year_end - year_start] = share_end;
        }

        list_years_valid[i] = year_valid;
        list_share_of_year[i] = share_of_year;
    }

    return List::create(Named("year_in_force") = list_years_valid, Named("share_of_year") = list_share_of_year);
}
