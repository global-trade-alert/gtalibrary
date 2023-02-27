// [[Rcpp::export]]
List string_split_cpp(const std::vector<std::string> &input_string, const std::string &pattern)
{
    const int n = input_string.size();
    List out(n);
    std::vector<std::string> temp_results;
    temp_results.reserve(30); // reserve enough memory to prevent reallocation during push_back()
    std::string string;

    size_t pos = 0;
    std::string token;
    for (int i = 0; i < n; i++)
    {
        string = input_string[i];
        while ((pos = string.find(pattern)) != std::string::npos)
        {
            token = string.substr(0, pos);
            temp_results.push_back(token);
            string.erase(0, pos + pattern.length());
        }

        out[i] = temp_results; // push back the last substring after the last delimiter
        temp_results.clear();
    }

    return out;
}
