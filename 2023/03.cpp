#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <tuple>
using namespace std;

struct Number
{
  string value;
  int length;
};

struct Coord
{
  int x, y;
  bool operator<(const Coord &other) const
  {
    return tie(x, y) < tie(other.x, other.y);
  }
};

vector<Coord> get_neighbors(int x, int y, int length, int max_x, int max_y)
{
  vector<Coord> neighbors;
  for (int ny = y - 1; ny <= y + 1; ny++)
  {
    for (int nx = x - 1; nx <= x + length; nx++)
    {
      if (ny == y && nx >= x && nx < x + length)
        continue;
      if (nx >= 0 && nx < max_x && ny >= 0 && ny < max_y)
      {
        neighbors.push_back({nx, ny});
      }
    }
  }
  return neighbors;
}

int main()
{
  ifstream file("input/input_03.txt");
  vector<string> grid;
  string line;

  while (getline(file, line))
  {
    grid.push_back(line);
  }

  int height = grid.size();
  int width = grid[0].size();

  map<Coord, char> symbols;
  map<Coord, Number> numbers;

  // parse grid
  for (int y = 0; y < height; y++)
  {
    for (int x = 0; x < width; x++)
    {
      char c = grid[y][x];
      if (c != '.' && !isdigit(c))
      {
        symbols[{x, y}] = c;
      }
      if (isdigit(c))
      {
        if (x > 0 && isdigit(grid[y][x - 1]))
          continue;

        int x2 = x;
        string num;
        while (x2 < width && isdigit(grid[y][x2]))
        {
          num += grid[y][x2];
          x2++;
        }
        numbers[{x, y}] = {num, x2 - x};
      }
    }
  }

  // part 1
  vector<int> part_numbers;
  for (const auto &[coord, number] : numbers)
  {
    bool is_part = false;
    for (const auto &neighbor : get_neighbors(coord.x, coord.y, number.length, width, height))
    {
      if (symbols.count(neighbor) > 0)
      {
        is_part = true;
        break;
      }
    }
    if (is_part)
    {
      part_numbers.push_back(stoi(number.value));
    }
  }

  // part 2
  map<Coord, vector<int>> gear_numbers;
  for (const auto &[coord, number] : numbers)
  {
    for (const auto &neighbor : get_neighbors(coord.x, coord.y, number.length, width, height))
    {
      if (symbols.count(neighbor) > 0 && symbols[neighbor] == '*')
      {
        gear_numbers[neighbor].push_back(stoi(number.value));
      }
    }
  }

  int part1_sum = 0;
  for (int num : part_numbers)
  {
    part1_sum += num;
  }

  long long part2_sum = 0;
  for (const auto &[coord, numbers] : gear_numbers)
  {
    if (numbers.size() == 2)
    {
      part2_sum += (long long)numbers[0] * numbers[1];
    }
  }

  cout << part1_sum << endl;
  cout << part2_sum << endl;

  return 0;
}