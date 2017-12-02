

function solve(bound::Int)
    nodup = Set{(Int, Int)}()
    hi = 1//2
    lo = 1//3
    for n=1:bound
        for d=n+1:bound
            fraction = n//d
            if lo < fraction && fraction < hi
                push!(nodup, (fraction.num, fraction.den))
            end
        end
    end
    return length(nodup)
end

function solve2(bound::Int)
    half = Array(Int, bound+1)
    third = Array(Int, bound+1)
    for i=1:bound
        half[i] = div(i, 2)
        third[i] = div(i, 3)
    end

    for i=1:bound
        for j=2:div(bound, i)
            half[i * j] -= half[i]
            third[i * j] -= third[i]
        end
    end
    total = 0
    for i=1:bound
        total = total + half[i] - third[i]
    end
    return total - 1
end

solve(12000)
@time solve(12000)
solve2(12000)
@time solve2(12000)
