┌─╴First
  &fras "in.txt"
  regex "mul\\((\\d+),(\\d+)\\)"
  /+≡(/×⋕↘1)
└─╴

┌─╴Second
  &fras "in.txt"
  ▽≠@\n.
  ⬚"0"regex "(don't\\(\\).*?(\\z|do\\(\\))|mul\\((\\d+),(\\d+)\\))"
  /+≡(/×?⋕↘3)
└─╴
