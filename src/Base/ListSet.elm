module Base.ListSet exposing
    ( Set
    , diff
    , empty
    , filter
    , foldl
    , foldr
    , fromList
    , group
    , insert
    , intersect
    , isEmpty
    , map
    , member
    , partition
    , product
    , remove
    , sample
    , singleton
    , size
    , toList
    , union
    , unsafe
    )

import Dict


type alias Set a =
    { list : List a }


unsafe : List a -> Set a
unsafe list =
    { list = list }


{-| Create an empty set.
-}
empty : Set a
empty =
    { list = [] }


{-| Create a set with one value.
-}
singleton : a -> Set a
singleton k =
    unsafe [ k ]


{-| Insert a value into a set.
-}
insert : a -> Set a -> Set a
insert k s =
    if List.member k s.list then
        s

    else
        unsafe <| k :: s.list


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : a -> Set a -> Set a
remove k s =
    unsafe <| List.filter ((/=) k) s.list


{-| Determine if a set is empty.
-}
isEmpty : Set a -> Bool
isEmpty { list } =
    List.isEmpty list


{-| Determine if a value is in a set.
-}
member : a -> Set a -> Bool
member k { list } =
    List.member k list


{-| Determine the number of elements in a set.
-}
size : Set a -> Int
size { list } =
    List.length list


{-| Get the union of two sets. Keep all values.
-}
union : Set a -> Set a -> Set a
union s1 s2 =
    if size s1 > size s2 then
        foldl insert s1 s2

    else
        foldl insert s2 s1


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : Set a -> Set a -> Set a
intersect set1 set2 =
    filter (\value -> member value set2) set1


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : Set a -> Set a -> Set a
diff set1 set2 =
    filter (\value -> not <| member value set2) set1


{-| Convert a set into a list.
-}
toList : Set a -> List a
toList { list } =
    list


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List a -> Set a
fromList xs =
    List.foldr insert empty xs


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> Set a -> b
foldl f b { list } =
    List.foldl f b list


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> Set a -> b
foldr f b { list } =
    List.foldr f b list


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (a -> b) -> Set a -> Set b
map f s =
    fromList (List.map f (toList s))


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (a -> Bool) -> Set a -> Set a
filter p { list } =
    unsafe <| List.filter p list


{-| -}
sample : Set a -> Maybe a
sample { list } =
    case list of
        h :: _ ->
            Just h

        _ ->
            Nothing


group : (x -> comparable) -> Set x -> Set ( comparable, Set x )
group f set =
    let
        add : comparable -> a -> Dict.Dict comparable (List a) -> Dict.Dict comparable (List a)
        add comparable a dict =
            Dict.get comparable dict
                |> Maybe.withDefault []
                |> (\list ->
                        a :: list
                   )
                |> (\list -> Dict.insert comparable list dict)
    in
    foldl (\x dict -> add (f x) x dict) Dict.empty set
        |> Dict.foldl (\k v s -> insert ( k, unsafe v ) s) empty


{-| Cartesian product of 2 Sets (i.e. all possible tuples of (a, b) from Set a, Set b).
-}
product : Set x -> Set y -> Set ( x, y )
product xs ys =
    let
        listProduct xs_ ys_ =
            List.concatMap (\x -> List.map (\y -> ( x, y )) ys_) xs_
    in
    unsafe <| listProduct (toList xs) (toList ys)


{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.
-}
partition : (a -> Bool) -> Set a -> ( Set a, Set a )
partition p { list } =
    let
        ( p1, p2 ) =
            List.partition p list
    in
    ( unsafe <| p1, unsafe <| p2 )
