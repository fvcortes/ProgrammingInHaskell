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

function powrec(n,m)
    if m == 0 then
        return 1
    else
        return n*powrec(n,m-1)
    end
end
print(pow (2,10))

print(powrec(2,10))