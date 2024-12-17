#include <bits/stdc++.h>
#include <chrono>

typedef long long ll;
typedef std::vector<int> vi;

#define FOR(i, n) for (int i = 0; i < (int)(n); ++i)
#define FORD(i, start, end) for (int i = (int)(start); i >= (int)(end); --i)
#define POW2(x) (1LL << (x))

inline ll combo_value(int op, ll A, ll B, ll C) {
  if (op < 4) return op;
  else if (op == 4) return A;
  else if (op == 5) return B;
  else if (op == 6) return C;
  
  throw std::runtime_error("invalid combo operand");
}

static vi parse(const std::string &program_str) {
  vi program;
  program.reserve(32);
  
  std::stringstream ss(program_str);
  std::string token;
  
  while (std::getline(ss, token, ',')) {
    program.push_back(std::stoi(token));
  }

  return program;
}

static std::string solve_part1(const std::string &program_str, ll A, ll B, ll C) {
  std::vector<ll> prog;
  
  {
    std::stringstream ss(program_str);
    std::string num;
  
    prog.reserve(32);
  
    while (std::getline(ss, num, ',')) prog.push_back(std::stoll(num));
  }

  ll a = A, b = B, c = C;
  int ip = 0;
  std::vector<std::string> output;
  
  output.reserve(prog.size() / 2);

  while (ip + 1 < (int)prog.size()) {
    int opcode = (int)prog[ip];
    int operand = (int)prog[ip + 1];

    switch (opcode) {
      // adv
      case 0: {
        ll p = combo_value(operand, a, b, c);
        a /= POW2(p);
        ip += 2;
      } break;

      // bxl
      case 1: {
        b ^= operand;
        ip += 2;
      } break;

      // bst
      case 2: {
        b = combo_value(operand, a, b, c) & 7;
        ip += 2;
      } break;

      // jnz
      case 3: {
        if (a != 0)
          ip = operand;
        else
          ip += 2;
      } break;

      // bxc
      case 4: {
        b ^= c;
        ip += 2;
      } break;

      // out
      case 5: {
        ll val = combo_value(operand, a, b, c) & 7; 
        output.push_back(std::to_string(val));
        ip += 2;
      } break;

      // bdv
      case 6: {
        ll p = combo_value(operand, a, b, c);
        b = a / POW2(p);
        ip += 2;
      } break;

      // cdv
      case 7: {
        ll p = combo_value(operand, a, b, c);
        c = a / POW2(p);
        ip += 2;
      } break;

      default:
        throw std::runtime_error("invalid opcode");
    }
  }

  std::string res;
  res.reserve(output.size() * 2);

  FOR(i, output.size()) {
    if (i > 0) res += ",";
    res += output[i];
  }

  return res;
}

inline bool produces_output(ll A_i, int out_val) {
  ll R = A_i & 7;
  ll b1 = R ^ 2;
  ll C = A_i / POW2(b1);
  
  return (((R ^ 5) ^ C) & 7) == out_val;
}

ll backward_solve_all(const vi &outputs) {
  std::unordered_set<ll> current = {0};
  current.reserve(64);

  FORD(i, (int)outputs.size() - 1, 0) {
    std::unordered_set<ll> next_set;
    next_set.reserve(current.size() * 8);

    for (auto A_next : current) {
      ll base = A_next * POW2(3);

      FOR(r, 8) {
        ll A_i = base + r;
        if (produces_output(A_i, outputs[i])) next_set.insert(A_i);
      }
    }

    if (next_set.empty()) return -1;
    current = std::move(next_set);
  }

  ll ans = -1;
  for (auto val : current)
    if (val > 0 && (ans == -1 || val < ans)) ans = val;

  return ans;
}


int main() {
  auto start = std::chrono::steady_clock::now();

  const std::string p1_input = "2,4,1,2,7,5,0,3,1,7,4,1,5,5,3,0";
  vi input_arr = parse(p1_input);

  std::string p1_output = solve_part1(p1_input, 27334280, 0, 0);
  std::cout << "Part 1: " << p1_output << "\n";

  ll p2_output = backward_solve_all(input_arr);

  if (p2_output == -1)
    std::cout << "no minimal positive A found\n";
  else
    std::cout << "Part 2: " << p2_output << "\n";

  auto end = std::chrono::steady_clock::now();
  std::chrono::duration<double> elapsed = end - start;
  std::cout << "Finished in " << elapsed.count() << " seconds\n";

  return 0;
}
