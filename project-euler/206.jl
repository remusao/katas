
function check(n::Int)
    d = digits(n)
end

function solve(limit::Int)
    res = 0
    for n=100:100:limit
        res = n * n
        check(res)
    end
    return res
end

@time @show solve(10^9)
@time solve(10^9)
