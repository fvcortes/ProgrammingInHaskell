function fact (n)
    acc = 1
    i = n
    while (i > 0) do
        acc = acc * i
        i = i - 1
    end
    return acc
end

function pow(n,m)
    i = m
    acc = 1
    while (i > 0) do
        acc = acc * n
        i = i - 1
    end
    return acc
end

print(pow (2,10))