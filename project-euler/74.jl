

function step(n::Int)
    s = 0
    for d in digits(n)
        s += factorial(d)
    end
    s
end


function solve(n::Int)
    memo = Dict{Int, Int}(
        0       => 1,
        1       => 1,
        2       => 1,
        145     => 1,
        169     => 3,
        871     => 2,
        872     => 2,
        1454    => 3,
        40585   => 1,
        45361   => 2,
        45362   => 2,
        363601  => 3,
    )

    res = 0
    chain = zeros(Int, 60)
    for tmp=1:n
        size = 1
        chain[1] = tmp

        # Find cycle
        cycle = get(memo, tmp, 0)
        while cycle == 0
            tmp = step(tmp)
            cycle = get(memo, tmp, 0)
            size += 1
            chain[size] = tmp
        end

        # Update memo
        size += cycle - 1

        # Update count of 60-length chains
        if size == 60
            res += 1
        end

        for j in 1:size
            memo[chain[j]] = size
            size -= 1
        end
       # println(chain[1], " ", size)
       # memo[chain[1]] = size
    end

    res
end

N = 1000000
@time solve(N)
@time solve(N)
@time solve(N)
@show solve(N)
