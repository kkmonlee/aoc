// g++ -std=c++17 -lgmpx -lgmp 21.cpp

#include <gmpxx.h>

#include <functional>
#include <iostream>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

typedef std::pair<int, int> position_t;
typedef std::tuple<position_t, position_t, int, int> cache_key_t;

#define CREATE_BUTTON_LOCATIONS(...) create_button_locations({__VA_ARGS__})

namespace std {
template <>
struct hash<cache_key_t> {
  size_t operator()(const cache_key_t& key) const {
    auto hash1 =
        hash<int>()(get<0>(key).first) ^ hash<int>()(get<0>(key).second);
    auto hash2 =
        hash<int>()(get<1>(key).first) ^ hash<int>()(get<1>(key).second);
    auto hash3 = hash<int>()(get<2>(key));
    auto hash4 = hash<int>()(get<3>(key));

    return hash1 ^ hash2 ^ hash3 ^ hash4;
  }
};
}  // namespace std

std::unordered_map<char, position_t> create_button_locations(const std::vector<std::string>& layout) {
  std::unordered_map<char, position_t> button_locations;
  for (size_t y = 0; y < layout.size(); ++y) {
    for (size_t x = 0; x < layout[y].size(); ++x) {
      button_locations[layout[y][x]] = {x, y};
    }
  }
  return button_locations;
}

std::unordered_map<char, position_t> button_locations = CREATE_BUTTON_LOCATIONS("789", "456", "123", "_0A");
std::unordered_map<char, position_t> button_locations_2 = CREATE_BUTTON_LOCATIONS("_^A", "<v>");

std::unordered_map<cache_key_t, mpz_class> walk_route_cache;

mpz_class walk_route(position_t from_pos, position_t to_pos, int depth, int bad_row,
                     const std::function<mpz_class(std::string, int, bool)>& route_code) {
                      
  cache_key_t cache_key = std::make_tuple(from_pos, to_pos, depth, bad_row);

  if (walk_route_cache.count(cache_key)) {
    return walk_route_cache[cache_key];
  }

  std::string horiz =
      (from_pos.first < to_pos.first)   ? std::string(to_pos.first - from_pos.first, '>')
      : (from_pos.first > to_pos.first) ? std::string(from_pos.first - to_pos.first, '<')
                                        : "";

  std::string vert =
      (from_pos.second < to_pos.second) ? std::string(to_pos.second - from_pos.second, 'v')
      : (from_pos.second > to_pos.second) ? std::string(from_pos.second - to_pos.second, '^')
                                          : "";

  std::vector<std::string> opts;

  if (horiz.empty()) {
    opts = {vert};
  } else if (vert.empty()) {
    opts = {horiz};
  } else {
    opts = {horiz + vert, vert + horiz};
    if (from_pos.first == 0 && to_pos.second == bad_row) {
      opts = {horiz + vert};
    } else if (to_pos.first == 0 && from_pos.second == bad_row) {
      opts = {vert + horiz};
    }
  }

  mpz_class result;
  mpz_pow_ui(result.get_mpz_t(), mpz_class(10).get_mpz_t(), 30);

  for (const auto& opt : opts) {
    result = std::min(result, route_code(opt + "A", depth - 1, true));
  }

  walk_route_cache[cache_key] = result;

  return result;
}

mpz_class route_code(std::string code, int depth, bool arrows) {
  if (depth <= 0) {
    return mpz_class(code.length());
  }

  const auto& locations = arrows ? button_locations_2 : button_locations;
  int bad_row = arrows ? 0 : 3;
  position_t pos = locations.at('A');
  mpz_class res = 0;

  for (char c : code) {
    position_t new_pos = locations.at(c);
    res += walk_route(pos, new_pos, depth, bad_row, route_code);
    pos = new_pos;
  }
  
  return res;
}

mpz_class solve(int depth) {
  std::vector<std::string> dat = {"459A", "671A", "846A", "285A", "083A"};
  mpz_class total = 0;

  for (const auto& i : dat) {
    std::string numeric_part = i.substr(0, i.size() - 1);
    numeric_part.erase(0, numeric_part.find_first_not_of('0'));
    if (numeric_part.empty()) numeric_part = "0";

    mpz_class multiplier = mpz_class(numeric_part);
    mpz_class result = route_code(i, depth, false);
    total += multiplier * result;
  }

  return total;
}

int main() {
  std::cout << solve(3) << std::endl;
  std::cout << solve(26) << std::endl;

  return 0;
}
