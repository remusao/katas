
const p = sort(primes(1000000))
const ps = Set{Int}(p)
concati(a::Int, b::Int) = a * 10 ^ (1 + trunc(Int, log10(b))) + b
cip(a::Int, b::Int) = in(concati(a, b), ps) && in(concati(b, a), ps)

function main()
    for p1 in p
        for p2 in p
            if p2 >= p1
                break
            elseif !cip(p1, p2)
                continue
            end
            for p3 in p
                if p3 >= p2
                    break
                elseif !(cip(p3, p2) && cip(p3, p1))
                    continue
                end
                for p4 in p
                    if p4 >= p3
                        break
                    elseif !(cip(p4, p3) && cip(p4, p2) && cip(p4, p1))
                        continue
                    end
                    for p5 in p
                        if p5 >= p4
                            break
                        elseif !(cip(p5, p4) && cip(p5, p3) && cip(p5, p2) && cip(p5, p1))
                            continue
                        end
                        return (p1, p2, p3, p4, p5)
                    end
                end
            end
        end
    end
end


println(sum(main()))
