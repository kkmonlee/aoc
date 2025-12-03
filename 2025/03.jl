function maxjolt(s, k)
    n, p, r = length(s), 1, Char[]
    for i in 1:k
        j = argmax(s[p:n-k+i]) + p - 1
        push!(r, s[j]); p = j + 1
    end
    parse(Int, String(r))
end

lines = readlines("input/input_3.txt")
println("Part 1: ", sum(maxjolt.(lines, 2)))
println("Part 2: ", sum(maxjolt.(lines, 12)))