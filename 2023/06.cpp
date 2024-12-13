#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <cmath>
#include <string>
using namespace std;

struct Range
{
  long long start, end;
};

Range solve(long long t, long long d)
{
  double disc = sqrt(t * t - 4.0 * d);
  double x1 = (t - disc) / 2.0;
  double x2 = (t + disc) / 2.0;

  return {
      static_cast<long long>(floor(x1)) + 1,
      static_cast<long long>(ceil(x2))};
}

vector<long long> parse_numbers(const string &line)
{
  vector<long long> numbers;
  istringstream iss(line.substr(line.find(':') + 1));
  long long n;
  while (iss >> n)
  {
    numbers.push_back(n);
  }
  return numbers;
}

long long concatenate_numbers(const vector<long long> &nums)
{
  string concat;
  for (auto n : nums)
  {
    concat += to_string(n);
  }
  return stoll(concat);
}

int main()
{
  ifstream file("input/input_06.txt");
  string time_line, distance_line;

  getline(file, time_line);
  getline(file, distance_line);

  vector<long long> times = parse_numbers(time_line);
  vector<long long> distances = parse_numbers(distance_line);

  long long result = 1;
  for (size_t i = 0; i < times.size(); i++)
  {
    Range r = solve(times[i], distances[i]);
    result *= (r.end - r.start);
  }
  cout << result << endl;

  long long big_time = concatenate_numbers(times);
  long long big_distance = concatenate_numbers(distances);
  Range r = solve(big_time, big_distance);
  cout << (r.end - r.start) << endl;

  return 0;
}