module History exposing (History, append, current, new, replace, undo)


type History a
    = History a (List a)


current : History a -> a
current (History c _) =
    c


append : a -> History a -> History a
append x (History c past) =
    History x (c :: past)


replace : a -> History a -> History a
replace x (History c past) =
    History x past


undo : History a -> History a
undo (History c past) =
    case past of
        [] ->
            History c []

        [ x ] ->
            History x []

        x :: xs ->
            History x xs


new : a -> History a
new x =
    History x []
