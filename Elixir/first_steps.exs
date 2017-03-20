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

  def element_at([], _), do: "error"
  def element_at([x|_], 1), do: x
  def element_at([_|xs], y), do: element_at(xs, y - 1)

  def myReverse([]), do: []
  def myReverse(xs) when is_binary(xs), do: to_string(
                                              myReverse(to_char_list(xs))
                                            )
  def myReverse([x|xs]), do: myReverse(xs) ++ [x]
  def myReverse(_), do: []

  def isPalindrome(xs), do: xs == myReverse xs

  def flatten(xs), do: flatten(xs, []) |> Enum.reverse
  def flatten([x | xs], acc) when x == [], do: flatten(xs, acc)
  def flatten([x | xs], acc) when is_list(x), do: flatten(xs, flatten(x, acc))
  def flatten([x | xs], acc), do: flatten(xs, [x | acc])
  def flatten(_, acc), do: acc

  # def partial(f, a), do: (fn b -> f.(a,b) end)
  # partial((fn a, b -> a + b end), 2).(1) - partial application
  def compress([]), do: []
  def compress([x|xs]), do: [x | compress(Enum.filter(xs, 
                                          (fn b -> b != x end)))] 
end
#IO.puts(First.len([]))
# IO.puts(First.len([1, 2, 3]))
# IO.puts(First.last([]))
# IO.puts(First.last([1]))
# IO.puts(First.last([1, 2, 3]))
# IO.puts(First.before_last([1]))
# IO.puts(First.before_last([1, 2]))
# IO.puts(First.before_last([1, 2, 3]))
IO.puts(First.element_at([1, 2, 3, 4], 3))
IO.puts(First.element_at([], 3))
IO.puts(First.element_at([1, 2, 3, 4], 2))
