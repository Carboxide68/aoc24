┌─╴First
  &fras "in.txt"
  /+≡(/×⋕↘1)reg "mul\\((\\d+),(\\d+)\\)"
└─╴

┌─╴Second
  &fras "in.txt"
  ▽≠@\n.
  ⬚"0"reg "(don't\\(\\).*?(\\z|do\\(\\))|mul\\((\\d+),(\\d+)\\))"
  /+≡(/×⋕↘3)
└─╴
