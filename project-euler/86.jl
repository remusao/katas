
function main()
    count = 0
    a = 0
    while true
        a += 1
        for b=1:a, c=1:b
            shortest = sqrt(a * a + (b + c) * (b + c))
            if abs(shortest - round(Int, shortest)) < 1e-6
                count += 1
                if count > 1000000
                    println(a)
                    return
                end
            end
        end
    end
end


main()
