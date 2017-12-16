#include <algorithm>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <unordered_set>


inline bool is_hashtag(const std::string& str) {
    return str.size() > 0 && str[0] == '#';
}


inline bool is_url(const std::string& str) {
    return !is_hashtag(str);
}


inline bool starts_with_www(const std::string& str) {
    return (str.size() >= 4 &&
            str[0] == 'w' &&
            str[1] == 'w' &&
            str[2] == 'w' &&
            str[3] == '.');
}


inline bool is_letter(char c) {
    return c >= 'a' && c <= 'z';
}


inline bool is_number(const std::string& str) {
    for (char c: str) {
        if (!(c == '.' || (c >= '0' && c <= '9')))
            return false;
    }
    return true;
}


int skip_suffix(const std::string& str) {
    int size = str.size();
    int index = size - 1;
    bool found_extension;

    do {
        found_extension = false;
        int suffix_size = 0;

        // .xxx
        if (index > 2
            && str[index - 3] == '.'
            && is_letter(str[index - 2])
            && is_letter(str[index - 1])
            && is_letter(str[index]))
        {
            found_extension = true;
            suffix_size = 4;
        }
        // .xx
        else if (index > 1
                 && str[index - 2] == '.'
                 && is_letter(str[index - 1])
                 && is_letter(str[index]))
        {
            found_extension = true;
            suffix_size = 3;
        }
        // .x
        else if (index > 0 && str[index - 1] == '.' && is_letter(str[index]))
        {
            found_extension = true;
            suffix_size = 2;
        }

        index -= suffix_size;
    } while (found_extension);

    return index;
}


bool is_lower(const std::string& str) {
    for (char c: str) {
        if (c >= 'A' && c <= 'Z') return false;
    }
    return true;
}


std::string str_to_lower(const std::string& str) {
    if (is_lower(str)) return str;
    std::string tmp = str;
    std::transform(tmp.begin(), tmp.end(), tmp.begin(), ::tolower);
    return tmp;
}

std::unordered_set<std::string> read_words() {
    std::unordered_set<std::string> words;

    // Create a lazy iterator on words
    std::ifstream ifs("words.txt", std::ifstream::in);
    for_each(
        std::istream_iterator<std::string>(ifs),
        std::istream_iterator<std::string>(),
        [&words](const std::string& word) {
            words.emplace(str_to_lower(word));
    });
    ifs.close();

    return std::move(words);
}


std::pair<int, int> get_boundaries(const std::string& str) {
    int start = 0;
    int end = str.size() - 1;

    // Deal with start
    if (is_hashtag(str)) {
        ++start;
    }
    else {
        if (starts_with_www(str)) {
            start += 4;
        }
        end = skip_suffix(str);
    }

    return {start, end};
}


std::vector<std::string> tokenize(
        const std::string& str,
        const std::unordered_set<std::string>& words)
{
    std::vector<std::string> tokens;
    int end = str.size() - 1;
    size_t total_size = 0;

    // Look for longest prefix
    for (int i = end; i >= 0; --i) {
        auto substr = str.substr(0, i + 1);
        if (is_number(substr) || words.find(substr) != words.end()) {
            // Try to tokenize remaining string
            auto remaining_str = str.substr(i + 1, end - i);
            auto next_tokens = tokenize(remaining_str, words);
            if (remaining_str.size() == 0 || next_tokens.size() > 0) {
                tokens.push_back(substr);
                total_size += substr.size();
                for (const std::string& token: next_tokens) {
                    total_size += token.size();
                    tokens.push_back(token);
                }
                break;
            }
        }
    }

    // If we didn't found every part of the string
    // in the dict, just return the full string.
    if (total_size < str.size()) {
        tokens.clear();
    }

    return std::move(tokens);
}


void display_tokens(const std::vector<std::string>& tokens) {
     auto it = tokens.cbegin();
     std::cout << *it;
     ++it;
     while (it != tokens.cend()) {
         std::cout << ' ' << *it;
         ++it;
     }
     std::cout << std::endl;
}


int main() {
    int n;
    std::string str;

    // English dict
    auto words = read_words();

    std::cin >> n;
    while (n--) {
        std::cin >> str;
        std::pair<int, int> boundaries = get_boundaries(str);
        std::string substr = str.substr(
                boundaries.first,
                boundaries.second - boundaries.first + 1);

        auto tokens = tokenize(substr, words);

        if (tokens.size() > 0)
            display_tokens(tokens);
        else
            std::cout << substr << std::endl;

    }

    return 0;
}
