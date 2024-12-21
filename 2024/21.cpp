// g++ -std=c++17 -lgmpx -lgmp 21.cpp

#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <tuple>
#include <functional>
#include <gmpxx.h>

using namespace std;

typedef pair<int, int> Position;
typedef tuple<Position, Position, int, int> CacheKey;

#define CREATE_BUTTON_LOCS(...) createButtonLocs({__VA_ARGS__})

namespace std {
    template <>
    struct hash<CacheKey> {
        size_t operator()(const CacheKey& key) const {
            auto hash1 = hash<int>()(get<0>(key).first) ^ hash<int>()(get<0>(key).second);
            auto hash2 = hash<int>()(get<1>(key).first) ^ hash<int>()(get<1>(key).second);
            auto hash3 = hash<int>()(get<2>(key));
            auto hash4 = hash<int>()(get<3>(key));
            return hash1 ^ hash2 ^ hash3 ^ hash4;
        }
    };
}

// Button locations
unordered_map<char, Position> createButtonLocs(const vector<string>& layout) {
    unordered_map<char, Position> buttonLocs;
    for (size_t y = 0; y < layout.size(); ++y) {
        for (size_t x = 0; x < layout[y].size(); ++x) {
            buttonLocs[layout[y][x]] = {x, y};
        }
    }
    return buttonLocs;
}

unordered_map<char, Position> buttonLocs = CREATE_BUTTON_LOCS("789", "456", "123", "_0A");
unordered_map<char, Position> buttonLocs2 = CREATE_BUTTON_LOCS("_^A", "<v>");

unordered_map<CacheKey, mpz_class> dorouteCache;

mpz_class doroute(Position fromPos, Position toPos, int depth, int badRow,
                  const function<mpz_class(string, int, bool)>& routecode) {
    CacheKey cacheKey = make_tuple(fromPos, toPos, depth, badRow);
    if (dorouteCache.count(cacheKey)) {
        return dorouteCache[cacheKey];
    }

    string horiz = (fromPos.first < toPos.first) ? string(toPos.first - fromPos.first, '>')
                   : (fromPos.first > toPos.first) ? string(fromPos.first - toPos.first, '<')
                   : "";

    string vert = (fromPos.second < toPos.second) ? string(toPos.second - fromPos.second, 'v')
                   : (fromPos.second > toPos.second) ? string(fromPos.second - toPos.second, '^')
                   : "";

    vector<string> opts;
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
        result = min(result, routecode(opt + "A", depth - 1, true));
    }

    dorouteCache[cacheKey] = result;
    return result;
}

mpz_class routecode(string code, int depth, bool arrows) {
    if (depth <= 0) {
        return mpz_class(code.length());
    }

    const auto& locs = arrows ? buttonLocs2 : buttonLocs;
    int badRow = arrows ? 0 : 3;
    Position pos = locs.at('A');
    mpz_class res = 0;

    for (char c : code) {
        Position newPos = locs.at(c);
        res += doroute(pos, newPos, depth, badRow, routecode);
        pos = newPos;
    }
    return res;
}

mpz_class solve(int depth) {
    vector<string> dat = {"459A", "671A", "846A", "285A", "083A"};
    mpz_class total = 0;

    for (const auto& i : dat) {
        string numericPart = i.substr(0, i.size() - 1);
        numericPart.erase(0, numericPart.find_first_not_of('0'));
        if (numericPart.empty()) numericPart = "0";

        mpz_class multiplier = mpz_class(numericPart);
        mpz_class result = routecode(i, depth, false);
        total += multiplier * result;
    }
    return total;
}

int main() {
    cout << solve(3) << endl;
    cout << solve(26) << endl;
    return 0;
}
