module ListUtils exposing (..)

-- Utils
import Random
import Time exposing (posixToMillis)

getRandomIndex: List a -> Random.Seed -> (Int, Random.Seed)
getRandomIndex = Random.step << Random.int 1 << List.length

remove: a -> (List a -> List a)
remove item = List.filter (\ n -> n /= item)

append: List a -> a -> List a
append list item = List.append list [item]

get: Int -> List a -> Maybe a
get i = List.head << List.reverse << List.take i


-- LOGIC


type Seedoid = IsSeed Random.Seed | IsPosix Time.Posix | IsInt Int

shuffle: Seedoid -> List x -> List x
shuffle =
     (shuffleFromSeed []) << seedFromSeedoid

seedFromSeedoid seed =
        case seed of
           IsSeed s -> s
           IsPosix t -> createSeed (posixToMillis t)
           IsInt i-> createSeed i

createSeed: Int -> Random.Seed
createSeed t = Random.initialSeed t


shuffleFromSeed: List x -> Random.Seed -> List x -> List x
shuffleFromSeed result seed items =
    let
     (position, newSeed) = getRandomIndex items seed
     value = get position items -- Si no, pasar a Array y hacer get, que es O(n log n) en vez de O(n)
    in
     case value of
         Nothing -> result
         Just item -> shuffleFromSeed (append result item) newSeed (remove item items)
