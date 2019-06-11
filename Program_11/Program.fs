type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }

let (.>.) x y = 
     if x.f = y.f then 
        if x.hours = y.hours then
            if x.minutes > y.minutes then true 
            else false
        elif x.hours > y.hours then true else false
     elif x.f = PM then true else false
