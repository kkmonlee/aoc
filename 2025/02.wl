ranges = ToExpression /@ StringSplit[#, "-"] & /@ StringSplit[Import["input/input_2.txt"], ","];
m[d_, k_] := (10^(d k) - 1)/(10^d - 1);
find[maxK_] := Total@Union@Flatten@Table[
  With[{mul = m[d, k], lo = If[d == 1, 1, 10^(d - 1)], hi = 10^d - 1},
    With[{a = Max[lo, Ceiling[s/mul]], b = Min[hi, Floor[e/mul]]},
      If[a <= b, Range[a, b] mul, {}]]],
  {{s, e}, ranges}, {d, 10}, {k, 2, maxK}];

Print["Part 1: ", find[2]]
Print["Part 2: ", find[24]]