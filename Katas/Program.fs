
open Strings

printf "abc bca is true: %b" <| stringIsPermutation "abc" "bca" |> ignore
printf "abc abca is false: %b" <| stringIsPermutation "abc" "bca" |> ignore
printf "abca"