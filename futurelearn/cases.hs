
data Pet = Cat | Dog | Fish | Parrot String

hello : :Pet -> String
hello x = 
    case x of
        Cat -> "meow"
        Dog -> "woof"
        Fish -> "bubble"
        Parrot name -> "pretty " ++ name
        _ -> "grunt"  -- not the '_' character
