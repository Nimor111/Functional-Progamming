defmodule First do
  def len([]), do: 0
  def len([_|xs]), do: 1 + length(xs)

  def last([]), do: "error"
  def last([x]), do: x
  def last([_|xs]), do: last(xs)

  def before_last([]), do: "error"
  def before_last([_]), do: "error"
  def before_last([x, _]), do: x
  def before_last([_|xs]), do: before_last(xs)
end

# IO.puts(First.len([]))
# IO.puts(First.len([1, 2, 3]))
# IO.puts(First.last([]))
# IO.puts(First.last([1]))
# IO.puts(First.last([1, 2, 3]))
IO.puts(First.before_last([1]))
IO.puts(First.before_last([1, 2]))
IO.puts(First.before_last([1, 2, 3]))
