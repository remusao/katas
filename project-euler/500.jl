
# Divisors or `x`
type Divisors
    x :: BigInt
    decomp :: Array{Any, 1}
end


function Divisors(x::BigInt)
    return Divisors(x, collect(values(factor(x))))
end


function Divisors{T <: Number}(decomp::Array{T, 1})
    primes_numbers = primes(100)
    while primes_numbers[end] < Base.length(decomp)
        primes_numbers = primes(length(primes_numbers) * 10)
    end
    x = BigInt(1)
    for (prime, exponent) in zip(primes_numbers[1:Base.length(decomp) + 1], decomp)
        x *= prime ^ exponent
    end
    return Divisors(x, decomp)
end


# Number of divisors
function length(d::Divisors)
    result = 1
    for div in d.decomp
        result *= div + 1
    end
    return result
end

# Find minimum number with `2^n` divisors
function solve(n::Integer; modulo=2)
    prime_lst = primes(7376507)
    exponents = ones(Int64, n)

    start = 1
    last = n
    while start < last
        while prime_lst[start] ^ (exponents[start] + 1) < prime_lst[last]
            exponents[start] = exponents[start] * 2 + 1
            exponents[last] = 0
            last -= 1
        end
        start += 1
    end

    product = 1
    for (i, exponent) in enumerate(exponents)
        product *= prime_lst[i] ^ exponent
        if modulo > 1
            product %= modulo
        end
    end

    return product
end


using PrimeSieve

# Find minimum number with `2^n` divisors
function solve2(n::Int, modulo::Int)
    prime_lst = PrimeSieve.primes(7376508)

    product = 1
    last = n
    j = 1
    while j <= last
        prime = prime_lst[j]
        cumul_prod = prime
        for i=last:-1:j+1
            last = i
            if (cumul_prod * prime) <= prime_lst[i]
                cumul_prod *= cumul_prod * prime
                cumul_prod %= modulo
                continue
            end
            break
        end
        product *= cumul_prod
        product %= modulo
        j += 1
    end

    return product
end

# for i=1:10
#     solve2(500500, 500500507)
# end
# @time println(solve2(500500, 500500507))
for i=1:10
    primes(1000000)
end

# for i in linrange(10, 1000000000, 10)
#     println(int(i))
#     print("Base\t\t=> ")
#     @time primes(int(i))
#     print("PrimeSieve\t=> ")
#     @time PrimeSieve.primes(int(i))
# end
for i=1:100
    println(i)
    PrimeSieve.primes(10000000000)
    gc()
end
