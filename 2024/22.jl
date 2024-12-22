function mix(secret::Int, value::Int)::Int
  xor(secret, value)
end

function prune(secret::Int)::Int
  mod(secret, 16777216)
end

function generate_next_secret(secret::Int)::Int
  # multiply by 64 and mix
  result = mix(secret, secret * 64)
  result = prune(result)
  
  # divide by 32 and mix
  divided_result = div(result, 32)
  result = mix(result, divided_result)
  result = prune(result)
  
  # multiply by 2048 and mix
  result = mix(result, result * 2048)
  prune(result)
end

function solve_part1(initial_secrets::Vector{Int}, n::Int=2000)::Int
  sum(initial_secrets) do initial_secret
      reduce((acc, _) -> generate_next_secret(acc), 1:n; init=initial_secret)
  end
end

function get_sequences(initial_secret::Int, num_changes::Int)
  prices = zeros(Int, num_changes + 1)
  changes = zeros(Int, num_changes)
  
  prices[1] = mod(initial_secret, 10)
  current_secret = initial_secret
  
  for i in 1:num_changes
      current_secret = generate_next_secret(current_secret)
      prices[i + 1] = mod(current_secret, 10)
      changes[i] = prices[i + 1] - prices[i]
  end
  
  # (sequence, price)
  [(Tuple(changes[i:i+3]), prices[i+4]) for i in 1:(num_changes-3)]
end

function solve_part2(initial_secrets::Vector{Int}, num_changes::Int=2000)::Int
  # store first occurrence of each sequence for each buyer
  sequence_data = Dict{NTuple{4,Int}, Dict{Int,Int}}()
  
  for (buyer_idx, initial_secret) in enumerate(initial_secrets)
      sequences = get_sequences(initial_secret, num_changes)
      for (sequence, price) in sequences
          buyer_prices = get!(Dict{Int,Int}, sequence_data, sequence)
          if !haskey(buyer_prices, buyer_idx)
              buyer_prices[buyer_idx] = price
          end
      end
  end
  
  max_result = maximum((sum(values(buyer_prices)), sequence) 
                      for (sequence, buyer_prices) in sequence_data)
  
  println("best sequence of changes => $(max_result[2])")
  max_result[1]
end

function main()
  initial_secrets = parse.(Int, readlines("input/input_22.txt"))
  
  part1_result = solve_part1(initial_secrets)
  println("Part 1: $part1_result")
  
  part2_result = solve_part2(initial_secrets)
  println("Part 2: $part2_result")
end

main()