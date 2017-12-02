
#=
Feyer series, we use dichotomic search
a//b -> (a + c)//(b + d) <- c//d
=#
function solve(limit::Int)
    lo = 3//8
    hi = 3//7
    while true
        new_frac = (num(lo) + num(hi))//(den(lo) + den(hi))
        if den(new_frac) > limit
            return lo
        end
        lo = new_frac
    end
end


bound = 1000000
@time @show solve(bound)
@time solve(bound)
