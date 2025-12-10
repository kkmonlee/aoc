using JuMP, HiGHS

function parse_line(line)
    target_match = match(r"\[([.#]+)\]", line)
    target = [c == '#' ? 1 : 0 for c in target_match[1]]
    
    buttons = [parse.(Int, split(m[1], ",")) .+ 1
               for m in eachmatch(r"\(([0-9,]+)\)", line)]
    
    joltage_match = match(r"\{([0-9,]+)\}", line)
    joltage = parse.(Int, split(joltage_match[1], ","))
    
    (target, buttons, joltage)
end

function solve_part1(target, buttons)
    n, k = length(target), length(buttons)
    
    # brute-force over GF(2) on 0 or 1 button presses
    minimum(
        count(i -> mask & (1 << (i-1)) != 0, 1:k)
        for mask in 0:(1 << k)-1
        if all(j -> xor((mask & (1 << (i-1)) != 0 && j in buttons[i] 
                         for i in 1:k)...) == (target[j] == 1), 1:n)
    )
end

function solve_part2(buttons, joltage)
    model = Model(HiGHS.Optimizer)
    set_silent(model)
    
    @variable(model, x[1:length(buttons)] >= 0, Int)
    
    for (j, t) in enumerate(joltage)
        @constraint(model, sum(x[i] for i in eachindex(buttons) if j in buttons[i]) == t)
    end
    
    @objective(model, Min, sum(x))
    # yeeeeeeet
    optimize!(model)
    
    Int(objective_value(model))
end

lines = readlines("input/input_10.txt")
machines = parse_line.(lines)

part1 = sum(solve_part1(t, b) for (t, b, _) in machines)
part2 = sum(solve_part2(b, j) for (_, b, j) in machines)

println("Part 1: $part1")
println("Part 2: $part2")