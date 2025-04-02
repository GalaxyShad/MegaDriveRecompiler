#ifndef CONFIG_H_
#define CONFIG_H_

#include "tinyint.h"
#include <string>
#include <vector>
#include <unordered_map>

struct RecompilerConfig {
    u32 start_adr;
    std::string binary_name; 
    std::unordered_map<u32, std::vector<i32>> known_xn_values;
    std::unordered_map<u32, std::string> known_labels;
};

#endif // CONFIG_H_