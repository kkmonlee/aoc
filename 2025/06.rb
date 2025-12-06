lines = File.readlines("input/input_6.txt", chomp: true)
maxw = lines.map(&:length).max
grid = lines.map { _1.ljust(maxw).chars }
ops, rows = grid.last, grid[..-2]

groups = (0...maxw).chunk { |i| rows.all? { _1[i] == " " } && ops[i] == " " }
                   .reject(&:first).map(&:last)

calc = ->(vals, op) { op == "+" ? vals.sum : vals.reduce(:*) }

p1 = groups.sum { |g|
  op = g.map { ops[_1] }.join.strip
  vals = rows.map { |r| g.map { r[_1] }.join.strip }.reject(&:empty?).map(&:to_i)
  calc[vals, op]
}

p2 = groups.sum { |g|
  op = g.map { ops[_1] }.join.strip
  vals = g.map { |i| rows.map { _1[i] }.join.gsub(/\D/, "") }.reject(&:empty?).map(&:to_i)
  calc[vals, op]
}

puts "Part 1: #{p1}", "Part 2: #{p2}"