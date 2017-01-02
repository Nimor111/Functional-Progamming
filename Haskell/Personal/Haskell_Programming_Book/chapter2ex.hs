-- rewriting let exprs with where: let x = 3; y = 1000 in x * 3 + y
-- let y = 10; x = 10 * 5 + y in x * 5
-- let x = 7; y = negate x; z = y * 10 in z / x + y

example = x * 3 + y
  where x = 3
        y = 1000

example2 = x * 5
  where y = 10
        x = 10 * 5 + y

example3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

waxOn = x * 5
  where x = y ^ 2
        y = z + 8
        z = 7

triple x = x * 3
waxOff = triple
