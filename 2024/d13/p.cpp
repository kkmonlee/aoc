// g++ -O3 -march=native -flto -std=c++17
#include <bits/stdc++.h>
using namespace std;

#define ll int64_t
#define vll vector<ll>
#define arr(n) array<ll, n>
#define pb push_back
#define rep(i, a, b) for (ll i = a; i < b; ++i)
#define each(x, a) for (auto &x : a)
#define sz(x) (ll)(x).size()

class Fraction
{
  ll num, den;

public:
  Fraction(ll n, ll d) : num(n), den(d)
  {
    if (den < 0)
    {
      num = -num;
      den = -den;
    }
  }
  bool is_integer() const { return abs(num) % abs(den) == 0; }
  ll to_integer() const { return num / den; }
  Fraction operator*(const Fraction &o) const { return {num * o.num, den * o.den}; }
  Fraction operator+(const Fraction &o) const { return {num * o.den + o.num * den, den * o.den}; }
};

inline ll parse_number(const string &line, size_t pos)
{
  while (pos < sz(line) && !isdigit(line[pos]))
    pos++;
  ll num = 0;
  while (pos < sz(line) && isdigit(line[pos]))
    num = num * 10 + (line[pos++] - '0');
  return num;
}

vector<arr(6)> load_data()
{
  vector<arr(6)> data;
  ifstream in("input.txt");
  string line;
  arr(6) cur;
  ll idx = 0;

  while (getline(in, line))
  {
    if (line.empty())
      continue;
    size_t x = line.find('X'), y = line.find('Y');
    if (x != string::npos && y != string::npos)
    {
      cur[idx++] = parse_number(line, x);
      cur[idx++] = parse_number(line, y);
      if (idx == 6)
      {
        data.pb(cur);
        idx = 0;
      }
    }
  }
  return data;
}

inline pair<ll, ll> solve(ll a, ll b, ll c, ll d, ll x, ll y)
{
  const ll det = a * d - b * c;
  if (!det)
    return {-1, -1};

  Fraction det_inv(1, det);
  Fraction ra = (Fraction(d, 1) * det_inv) * Fraction(x, 1) + (Fraction(-c, 1) * det_inv) * Fraction(y, 1);
  if (!ra.is_integer())
    return {-1, -1};

  Fraction rb = (Fraction(-b, 1) * det_inv) * Fraction(x, 1) + (Fraction(a, 1) * det_inv) * Fraction(y, 1);
  if (!rb.is_integer())
    return {-1, -1};

  ll av = ra.to_integer(), bv = rb.to_integer();
  if (av >= 0 && bv >= 0)
    return {av, bv};
  return {-1, -1};
}

int main()
{
  ios_base::sync_with_stdio(0);
  cin.tie(0);
  const auto data = load_data();

  // part 1
  ll ans1 = 0;
  each(line, data)
  {
    const auto [a, b] = solve(line[0], line[1], line[2], line[3], line[4], line[5]);
    if (a >= 0)
      ans1 += a * 3 + b;
  }

  // part 2
  constexpr ll offset = 10000000000000LL;
  ll ans2 = 0;
  each(line, data)
  {
    const auto [a, b] = solve(line[0], line[1], line[2], line[3],
                              line[4] + offset, line[5] + offset);
    if (a >= 0)
      ans2 += a * 3 + b;
  }

  cout << ans1 << endl
       << ans2 << endl;
}