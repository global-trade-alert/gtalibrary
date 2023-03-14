#include <Rcpp.h>
using namespace Rcpp;

/*
gta_code_converter_cpp() is used in gta_hs_vintage_converter()
to speed up the function. Instead of looping over every code that must be converted,
checking if it matches with a vintage hs code and then appending the results to a list in R,
this loop is implemented in c++.
Arguments:
    - codes_vinage & codes_2012: Two string vectors from the conversion matrix in gtalibrary::hs_vintages
    --> before passing them to the function, make sure that the pairs are unique
    - codes: the vector of codes that should be converted to HS2012
Warning:
    The function does not check if the aruments satisfy all necessary conditions (eg. type, size etc.).
    This is implemented in R in gta_hs_vintage_converter() before the arguments are passed to gta_code_converter_cpp()
    Thus, it is not recommended to use this function outside of gta_hs_vintage_converter()
*/

// [[Rcpp::export]]
List gta_code_converter_cpp(const std::vector<std::string> &codes_2012,
                            const std::vector<std::string> &codes_vintage,
                            const std::vector<std::string> &codes)
{
    // Define objects
    const int n = codes_2012.size();
    const int m = codes.size();
    List results(m);

    // Create HS2 and HS4 codes from code_vintages // create 3 different maps of size n to reduce probability of hash collision
    std::unordered_multimap<std::string, std::string> hs_2012_vintage_map;
    hs_2012_vintage_map.reserve(n);
    std::unordered_multimap<std::string, std::string> hs_2012_vintage_4_map;
    hs_2012_vintage_4_map.reserve(n);
    std::unordered_multimap<std::string, std::string> hs_2012_vintage_2_map;
    hs_2012_vintage_2_map.reserve(n);
    std::vector<std::string> unconverted_codes;
    unconverted_codes.reserve(50); // reserve some memory
    std::vector<std::string> temp_results;
    temp_results.reserve(50); // pre-allocate some memory --> Enough for all codes if HS2 is supplied
    // assign values to maps
    for (int i = 0; i < n; i++)
    {
        hs_2012_vintage_map.insert({codes_vintage[i], codes_2012[i]});
        hs_2012_vintage_4_map.insert({codes_vintage[i].substr(0, 4), codes_2012[i]});
        hs_2012_vintage_2_map.insert({codes_vintage[i].substr(0, 2), codes_2012[i]});
    }

    // Loop over each code in codes_convert, check if it matches with 2, 4, 6 digit string of codes_vintage and if yes,
    // return the corresponding value in codes_2012. If no, store value in unconverted_codes
    for (int j = 0; j < m; j++)
    {
        if (codes[j].size() == 6)
        {
            auto range = hs_2012_vintage_map.equal_range(codes[j]);
            if (range.first != range.second)
            {
                for (auto it = range.first; it != range.second; ++it)
                {
                    temp_results.push_back(it->second);
                }
            }
            else
            {
                unconverted_codes.push_back(codes[j]);
            }
        }
        if (codes[j].size() == 4)
        {
            auto range = hs_2012_vintage_4_map.equal_range(codes[j]);
            if (range.first != range.second)
            {
                for (auto it = range.first; it != range.second; ++it)
                {
                    temp_results.push_back(it->second);
                }
            }
            else
            {
                unconverted_codes.push_back(codes[j]);
            }
        }
        if (codes[j].size() == 2)
        {
            auto range = hs_2012_vintage_2_map.equal_range(codes[j]);
            if (range.first != range.second)
            {
                for (auto it = range.first; it != range.second; ++it)
                {
                    temp_results.push_back(it->second);
                }
            }
            else
            {
                unconverted_codes.push_back(codes[j]);
            }
        }
        results[j] = wrap(temp_results);
        temp_results.clear(); // clear temporary results after they are appended to output list
    }
    return List::create(Named("converted") = results, Named("unconverted") = unconverted_codes);
}