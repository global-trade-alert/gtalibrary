#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
std::vector<std::string> seq_quarters(const Date &start_date, const Date &end_date)
{
    std::vector<std::string> quarters;
    quarters.reserve(60); // reserve space for 60 quartres

    // determine start and end quarter
    const int start_quarter = (start_date.getMonth() - 1) / 3 + 1;
    const int end_quarter = (end_date.getMonth() - 1) / 3 + 1;
    const int start_year = start_date.getYear();
    const int end_year = end_date.getYear();
    std::string quarter_str;

    // Loop over all quarters between the start and end dates and append to quarters
    for (int year = start_year; year <= end_year; year++)
    {

        for (int quarter = 1; quarter <= 4; quarter++)
        {
            // Check if this quarter falls within the date range
            if (start_year < end_year)
            {
                quarter_str = std::to_string(year) + "-Q" + std::to_string(quarter);
                quarters.push_back(quarter_str);
            }
            else if (quarter <= end_quarter)
            {
                quarter_str = std::to_string(year) + "-Q" + std::to_string(quarter);
                quarters.push_back(quarter_str);
            }
            else
            {
                break;
            }
        }
    }
    // erase first start_quarter - 1 elements as loop started with quarter =1
    quarters.erase(quarters.begin(), quarters.begin() + (start_quarter - 1));
    return quarters;
}

// [[Rcpp::export]]
std::vector<std::string> seq_months(const Date &start_date, const Date &end_date)
{
    std::vector<std::string> months;
    months.reserve(200); // reserve space for 200 months

    // determine start and end quarter
    const int start_month = start_date.getMonth();
    const int end_month = end_date.getMonth();
    const int start_year = start_date.getYear();
    const int end_year = end_date.getYear();
    std::string month_str;

    // Loop over all quarters between the start and end dates and append to quarters
    for (int year = start_year; year <= end_year; year++)
    {

        for (int month = 1; month <= 12; month++)
        {
            // Check if this quarter falls within the date range
            if (start_year < end_year)
            {
                month_str = std::to_string(year) + "-" + std::to_string(month);
                months.push_back(month_str);
            }
            else if (month <= end_month)
            {
                month_str = std::to_string(year) + "-" + std::to_string(month);
                months.push_back(month_str);
            }
            else
            {
                break;
            }
        }
    }
    // erase first start_months - 1 elements as loop started with quarter =1
    months.erase(months.begin(), months.begin() + (start_month - 1));
    return months;
}

// method to seq years between two dates --> Helper function for exported function
// [[Rcpp::export]]
std::vector<std::string> seq_years(const Date &start_date, const Date &end_date)
{
    std::vector<std::string> years;
    years.reserve(200); // reserve space for 30 years

    // determine start and end year
    const int start_year = start_date.getYear();
    const int end_year = end_date.getYear();

    // Loop over all years between the start and end year and append to output vector
    for (int year = start_year; year <= end_year; year++)
    {
        years.push_back(std::to_string(year));
    }
    return years;
}