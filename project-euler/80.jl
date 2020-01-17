

function solve()
    total = 0
    for n in 1:100
        s = setprecision(5000) do
            sqrt(BigFloat(n))
        end
        if s != round(s)
            println(">>> ", n)
            for i in 1:100
                s *= 10
                println(i, ' ', convert(Int, round(s % 10)))
                total += convert(Int, round(s % 10))
            end
            return total
        end
    end
end

println(solve())
