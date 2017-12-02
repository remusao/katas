
using Base.Test

#=
Test if a number is increasing
=#
function increasing(d)
    for i in 2:length(d)
        if d[i - 1] < d[i]
            return false
        end
    end
    return true
end

#=
Test if a number is decreasing
=#
function decreasing(d)
    for i in 2:length(d)
        if d[i - 1] > d[i]
            return false
        end
    end
    return true
end

#=
Test if a number is neither increasing nor decreasing
=#
bouncy(n) = !increasing(n) && !decreasing(n)

function solve()
    boun = 0
    for i=1:5000000
        if bouncy(collect(digits(i)))
            boun += 1
        end
        percent = float(boun) * 100.0 / float(i)
        if percent >= 99.0
            println(i)
            break
        end
    end
end

function test()
    @test increasing(134468) == true
    @test decreasing(66420) == true
    @test decreasing(111) == true
    @test decreasing(1) == true
    @test increasing(1) == true
    @test increasing(111) == true
    @test bouncy(155349) == true
end


solve()
@time solve()
