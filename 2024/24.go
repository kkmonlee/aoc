package main

import (
	"fmt"
	"os"
	"sort"
	"strings"
)

type Operation struct {
	a, op, b string
}

func min(a, b string) string {
	if a < b {
		return a
	}
	return b
}

func max(a, b string) string {
	if a > b {
		return a
	}
	return b
}

type Circuit struct {
	vals map[string]int
	ops  map[string]Operation
}

func (c *Circuit) visit(vr string) int {
	if val, exists := c.vals[vr]; exists {
		return val
	}
	op := c.ops[vr]
	var result int
	switch op.op {
	case "AND":
		result = c.visit(op.a) & c.visit(op.b)
	case "OR":
		result = c.visit(op.a) | c.visit(op.b)
	case "XOR":
		result = c.visit(op.a) ^ c.visit(op.b)
	}
	c.vals[vr] = result
	return result
}

func main() {
	data, err := os.ReadFile("input/input_24.txt")
	if err != nil {
		panic(err)
	}

	sections := strings.Split(strings.TrimSpace(string(data)), "\n\n")

	circuit := &Circuit{
		vals: make(map[string]int),
		ops:  make(map[string]Operation),
	}

	for _, line := range strings.Split(sections[0], "\n") {
		parts := strings.Split(line, ": ")
		var val int
		fmt.Sscanf(parts[1], "%d", &val)
		circuit.vals[parts[0]] = val
	}

	var xs, ys, zs []string
	for _, line := range strings.Split(sections[1], "\n") {
		parts := strings.Fields(line)
		a, op, b, c := parts[0], parts[1], parts[2], parts[4]
		circuit.ops[c] = Operation{min(a, b), op, max(a, b)}
		if strings.HasPrefix(c, "z") {
			xs = append(xs, strings.Replace(c, "z", "x", 1))
			ys = append(ys, strings.Replace(c, "z", "y", 1))
			zs = append(zs, c)
		}
	}

	// p1
	sort.Strings(zs)
	p1 := 0
	for i, z := range zs {
		p1 += circuit.visit(z) << i
	}
	fmt.Printf("Part 1: %d\n", p1)

	// revmap
	opsrev := make(map[Operation]string)
	for out, op := range circuit.ops {
		opsrev[op] = out
	}

	sort.Strings(xs)
	sort.Strings(ys)
	sort.Strings(zs)

	c := ""
	cc := ""
	var swaps []string

	for i := 0; i < len(zs)-1; i++ {
		xo := opsrev[Operation{min(xs[i], ys[i]), "XOR", max(xs[i], ys[i])}]
		an := opsrev[Operation{min(xs[i], ys[i]), "AND", max(xs[i], ys[i])}]

		if c == "" {
			c = an
		} else {
			var pos []struct{ s1, s2, gt string }
			for op, out := range opsrev {
				if op.op == "AND" {
					if op.a == xo {
						pos = append(pos, struct{ s1, s2, gt string }{c, op.b, out})
					} else if op.b == xo {
						pos = append(pos, struct{ s1, s2, gt string }{c, op.a, out})
					} else if op.a == c {
						pos = append(pos, struct{ s1, s2, gt string }{xo, op.b, out})
					} else if op.b == c {
						pos = append(pos, struct{ s1, s2, gt string }{xo, op.a, out})
					}
				}
			}

			ccFound := false
			if _, exists := opsrev[Operation{min(xo, c), "AND", max(xo, c)}]; exists {
				cc = opsrev[Operation{min(xo, c), "AND", max(xo, c)}]
				ccFound = true
			}

			if !ccFound && len(pos) > 0 {
				s1, s2, gt := pos[0].s1, pos[0].s2, pos[0].gt
				sub := map[string]string{s1: s2, s2: s1}
				swaps = append(swaps, s1, s2)
				fmt.Printf("Found swap: %s, %s, %v, %s, %s, %s\n", xo, c, circuit.ops[gt], s1, s2, gt)

				if newC, exists := sub[c]; exists {
					c = newC
				}
				if newXo, exists := sub[xo]; exists {
					xo = newXo
				}
				if newAn, exists := sub[an]; exists {
					an = newAn
				}

				// update ops and opsrev
				newOps := make(map[string]Operation)
				for out, op := range circuit.ops {
					newOut := out
					if newOut2, exists := sub[out]; exists {
						newOut = newOut2
					}
					newOps[newOut] = op
				}
				circuit.ops = newOps

				opsrev = make(map[Operation]string)
				for out, op := range circuit.ops {
					opsrev[op] = out
				}

				cc = opsrev[Operation{min(xo, c), "AND", max(xo, c)}]
			}

			pos = nil
			for op, out := range opsrev {
				if op.op == "OR" {
					if op.a == cc {
						pos = append(pos, struct{ s1, s2, gt string }{an, op.b, out})
					} else if op.b == cc {
						pos = append(pos, struct{ s1, s2, gt string }{an, op.a, out})
					} else if op.a == an {
						pos = append(pos, struct{ s1, s2, gt string }{cc, op.b, out})
					} else if op.b == an {
						pos = append(pos, struct{ s1, s2, gt string }{cc, op.a, out})
					}
				}
			}

			cFound := false
			if _, exists := opsrev[Operation{min(cc, an), "OR", max(cc, an)}]; exists {
				c = opsrev[Operation{min(cc, an), "OR", max(cc, an)}]
				cFound = true
			}

			if !cFound && len(pos) > 0 {
				s1, s2, gt := pos[0].s1, pos[0].s2, pos[0].gt
				sub := map[string]string{s1: s2, s2: s1}
				swaps = append(swaps, s1, s2)
				fmt.Printf("Found swap: %s, %s, %v, %s, %s, %s\n", cc, an, circuit.ops[gt], s1, s2, gt)

				if newCc, exists := sub[cc]; exists {
					cc = newCc
				}
				if newAn, exists := sub[an]; exists {
					an = newAn
				}

				// update ops and opsrev
				newOps := make(map[string]Operation)
				for out, op := range circuit.ops {
					newOut := out
					if newOut2, exists := sub[out]; exists {
						newOut = newOut2
					}
					newOps[newOut] = op
				}
				circuit.ops = newOps

				opsrev = make(map[Operation]string)
				for out, op := range circuit.ops {
					opsrev[op] = out
				}

				c = opsrev[Operation{min(cc, an), "OR", max(cc, an)}]
			}
		}
		fmt.Printf("Position %d: %s, %s, %s, %s\n", i, xo, an, c, cc)
	}

	sort.Strings(swaps)
	result := strings.Join(swaps, ",")
	fmt.Printf("\nPart 2: %s\n", result)
}
