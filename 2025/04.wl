data = Import["input/input_4.txt", "Text"];

grid = Map[Boole[# == "@"] &, Characters /@ StringSplit[data, "\n"], {2}];
kernel = {{1,1,1}, {1,0,1}, {1,1,1}};
neighbours[g_] := ListConvolve[kernel, g, {2,2}, 0];
accessible[g_] := g * UnitStep[3 - neighbours[g]];
part1 = Total[accessible[grid], 2];
remove[g_] := g - accessible[g];
final = FixedPoint[remove, grid];
part2 = Total[grid - final, 2];

Print["Part 1: ", part1];
Print["Part 2: ", part2];