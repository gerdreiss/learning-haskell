module Chapter2_5
  ( foo
  , bar
  , baz
  , quux
  , corge
  , grault
  , garply
  , waldo
  ) where


-- Отметьте функции,
-- которые не могут привести к расходимости
-- ни на каком корректном наборе аргументов.

foo a = a
bar = const foo
baz x = const True
quux = let x = x in x
corge = "Sorry, my value was changed"
grault x 0 = x
grault x y = x
garply = grault 'q'
waldo = foo
