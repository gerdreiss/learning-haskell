
data Pet = Cat | Dog | Fish | Parrot String

hello :: Pet -> String
hello pet = 
    case pet of
        Cat -> "meow"
        Dog -> "woof"
        Fish -> "bubble"
        Parrot name -> "pretty " ++ name
        _ -> "grunt"  -- not the '_' character
