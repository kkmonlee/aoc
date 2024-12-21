// g++ -std=c++17 -lgmpx -lgmp 21.cpp

#include <gmpxx.h>

#include <functional>
#include <iostream>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

typedef std::pair<int, int> Position;
typedef std::tuple<Position, Position, int, int> CacheKey;

#define CREATE_BUTTON_LOCATIONS(...) CreateButtonLocations({__VA_ARGS__})

namespace std {
template <>
struct hash<CacheKey> {
  size_t operator()(const CacheKey& key) const {
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

std::unordered_map<char, Position> CreateButtonLocations(const std::vector<std::string>& layout) {
  std::unordered_map<char, Position> buttonLocations;
  for (size_t y = 0; y < layout.size(); ++y) {
    for (size_t x = 0; x < layout[y].size(); ++x) {
      buttonLocations[layout[y][x]] = {x, y};
    }
  }
  return buttonLocations;
}

std::unordered_map<char, Position> buttonLocations = CREATE_BUTTON_LOCATIONS("789", "456", "123", "_0A");
std::unordered_map<char, Position> buttonLocations2 = CREATE_BUTTON_LOCATIONS("_^A", "<v>");

std::unordered_map<CacheKey, mpz_class> walkRouteCache;

mpz_class WalkRoute(Position fromPos, Position toPos, int depth, int badRow,
                    const std::function<mpz_class(std::string, int, bool)>& routeCode) {
  CacheKey cacheKey = std::make_tuple(fromPos, toPos, depth, badRow);
  if (walkRouteCache.count(cacheKey)) {
    return walkRouteCache[cacheKey];
  }

  std::string horiz =
      (fromPos.first < toPos.first)   ? std::string(toPos.first - fromPos.first, '>')
      : (fromPos.first > toPos.first) ? std::string(fromPos.first - toPos.first, '<')
                                      : "";

  std::string vert =
      (fromPos.second < toPos.second) ? std::string(toPos.second - fromPos.second, 'v')
      : (fromPos.second > toPos.second) ? std::string(fromPos.second - toPos.second, '^')
                                        : "";

  std::vector<std::string> opts;
  if (horiz.empty()) {
    opts = {vert};
  } else if (vert.empty()) {
    opts = {horiz};
  } else {
    opts = {horiz + vert, vert + horiz};
    if (fromPos.first == 0 && toPos.second == badRow) {
      opts = {horiz + vert};
    } else if (toPos.first == 0 && fromPos.second == badRow) {
      opts = {vert + horiz};
    }
  }

  mpz_class result;
  mpz_pow_ui(result.get_mpz_t(), mpz_class(10).get_mpz_t(), 30);

  for (const auto& opt : opts) {
    result = std::min(result, routeCode(opt + "A", depth - 1, true));
  }

  walkRouteCache[cacheKey] = result;
  return result;
}

mpz_class RouteCode(std::string code, int depth, bool arrows) {
  if (depth <= 0) {
    return mpz_class(code.length());
  }

  const auto& locations = arrows ? buttonLocations2 : buttonLocations;
  int badRow = arrows ? 0 : 3;
  Position pos = locations.at('A');
  mpz_class res = 0;

  for (char c : code) {
    Position newPos = locations.at(c);
    res += WalkRoute(pos, newPos, depth, badRow, RouteCode);
    pos = newPos;
  }
  return res;
}

mpz_class Solve(int depth) {
  std::vector<std::string> dat = {"459A", "671A", "846A", "285A", "083A"};
  mpz_class total = 0;

  for (const auto& i : dat) {
    std::string numericPart = i.substr(0, i.size() - 1);
    numericPart.erase(0, numericPart.find_first_not_of('0'));
    if (numericPart.empty()) numericPart = "0";

    mpz_class multiplier = mpz_class(numericPart);
    mpz_class result = RouteCode(i, depth, false);
    total += multiplier * result;
  }

  return total;
}

int main() {
  std::cout << Solve(3) << std::endl;
  std::cout << Solve(26) << std::endl;

  return 0;
}
