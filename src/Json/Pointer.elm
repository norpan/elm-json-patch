module Json.Pointer exposing
    ( Pointer
    , addAt, removeAt, getAt
    , encoder, decoder
    )

{-| This module implements JSON Pointer as per
[RFC 6901](https://tools.ietf.org/html/rfc6901).


# Type

@docs Pointer


# Operations

@docs addAt, removeAt, getAt


# Encoder/Decoder

@docs encoder, decoder

-}

import Array
import Dict
import Json.Decode as JD
import Json.Encode as JE
import Regex
import String


{-| A type alias representing a JSON Pointer.

The JSON string `"/a/b/c"` corresponds to `[ "a", "b", "c" ]`

-}
type alias Pointer =
    List String


{-| Get the `Value` at the specified pointer.
-}
getAt : Pointer -> JD.Value -> Result String JD.Value
getAt path value =
    case path of
        [] ->
            Ok value

        [ name ] ->
            arrayOrObject value
                (\array ->
                    getArrayIndex name array
                        |> Result.andThen
                            (\index ->
                                get index array
                            )
                )
                (\object ->
                    Dict.get name object
                        |> Result.fromMaybe ("field not found: " ++ name)
                )

        name :: names ->
            arrayOrObject value
                (\array ->
                    getArrayIndex name array
                        |> Result.andThen
                            (\index ->
                                get index array
                                    |> Result.andThen (getAt names)
                            )
                )
                (\object ->
                    Dict.get name object
                        |> Result.fromMaybe ("field not found: " ++ name)
                        |> Result.andThen (getAt names)
                )


indexRegex : Regex.Regex
indexRegex =
    Regex.fromString "^(-|0|[1-9][0-9]*)$"
        |> Maybe.withDefault Regex.never


getArrayIndex : String -> Array.Array a -> Result String Int
getArrayIndex name array =
    if not (Regex.contains indexRegex name) then
        Err "bad index"

    else if name == "-" then
        Ok (Array.length array)

    else
        String.toInt name |> Result.fromMaybe "not an integer"


{-| Add the specified value at the specified pointer.

For adding to an array, this means inserting the value at the specified index
, shifting the following elements to the right.

For adding to an object, this means adding or replacing the specified field.

-}
addAt : Pointer -> JD.Value -> JD.Value -> Result String JD.Value
addAt path v value =
    handleCases
        { whenEmpty = Ok v
        , whenArray =
            \array index ->
                insert index v array
                    |> Result.map (Array.toList >> JE.list identity)
        , whenObject =
            \object field ->
                Ok (JE.object (Dict.toList (Dict.insert field v object)))
        , path = path
        , operation = \rest -> addAt rest v
        , value = value
        }


{-| Remove the specified value at the specified pointer.

For removing from an array, this means deleting the value at the specified index
, shifting the following elements to the left.

For removing from an object, this means removing the specified field.

-}
removeAt : Pointer -> JD.Value -> Result String JD.Value
removeAt path value =
    handleCases
        { whenEmpty = Ok JE.null
        , whenArray =
            \array index ->
                remove index array
                    |> Result.map (Array.toList >> JE.list identity)
        , whenObject =
            \object field ->
                if Dict.member field object then
                    Ok (JE.object (Dict.toList (Dict.remove field object)))

                else
                    Err ("removing nonexistent field: " ++ field)
        , path = path
        , operation = \rest -> removeAt rest
        , value = value
        }


handleCases :
    { whenEmpty : Result String JD.Value
    , whenArray : Array.Array JD.Value -> Int -> Result String JD.Value
    , whenObject : Dict.Dict String JD.Value -> String -> Result String JD.Value
    , path : Pointer
    , operation : Pointer -> JD.Value -> Result String JD.Value
    , value : JD.Value
    }
    -> Result String JD.Value
handleCases { whenEmpty, whenArray, whenObject, path, operation, value } =
    case path of
        [] ->
            whenEmpty

        [ name ] ->
            arrayOrObject value
                (\a ->
                    getArrayIndex name a
                        |> Result.andThen (whenArray a)
                )
                (\o -> whenObject o name)

        name :: names ->
            arrayOrObject value
                (\array ->
                    getArrayIndex name array
                        |> Result.andThen
                            (\index ->
                                get index array
                                    |> Result.andThen
                                        (\x ->
                                            operation names x
                                                |> Result.map (\v -> Array.set index v array |> Array.toList |> JE.list identity)
                                        )
                            )
                )
                (\object ->
                    Dict.get name object
                        |> Result.fromMaybe ("field not found: " ++ name)
                        |> Result.andThen (\x -> operation names x)
                        |> Result.map (\x -> JE.object (Dict.toList (Dict.insert name x object)))
                )


get : Int -> Array.Array a -> Result String a
get i array =
    Array.get i array
        |> Result.fromMaybe ("index out of bounds: " ++ String.fromInt i)


insert : Int -> a -> Array.Array a -> Result String (Array.Array a)
insert i a array =
    if i > Array.length array then
        Err ("index out of bounds: " ++ String.fromInt i)

    else
        Ok <|
            Array.append
                (Array.push a (Array.slice 0 i array))
                (Array.slice i (Array.length array) array)


remove : Int -> Array.Array a -> Result String (Array.Array a)
remove i array =
    if i >= Array.length array then
        Err ("index out of bounds: " ++ String.fromInt i)

    else
        Ok <|
            Array.append
                (Array.slice 0 i array)
                (Array.slice (i + 1) (Array.length array) array)


arrayOrObject :
    JD.Value
    -> (Array.Array JD.Value -> Result String a)
    -> (Dict.Dict String JD.Value -> Result String a)
    -> Result String a
arrayOrObject v ifArray ifObject =
    case
        JD.decodeValue (JD.list JD.value) v
    of
        Ok a ->
            Array.fromList a |> ifArray

        Err _ ->
            JD.decodeValue (JD.dict JD.value) v
                |> Result.mapError JD.errorToString
                |> Result.andThen ifObject



-- DECODER


{-| JSON decoder for `Pointer`
-}
decoder : JD.Decoder Pointer
decoder =
    JD.string |> JD.andThen pointerFromString


pointerFromString : String -> JD.Decoder Pointer
pointerFromString string =
    case splitAndUnescape string of
        [ "" ] ->
            JD.succeed []

        "" :: pointer ->
            JD.succeed pointer

        _ ->
            JD.fail "A JSON Pointer must start with / or be empty"


splitAndUnescape : String -> List String
splitAndUnescape string =
    string |> String.split "/" |> List.map unescape


unescape : String -> String
unescape string =
    string
        |> String.split "~1"
        |> String.join "/"
        |> String.split "~0"
        |> String.join "~"



-- ENCODER


{-| JSON encoder for `Pointer`
-}
encoder : Pointer -> JE.Value
encoder p =
    JE.string (pointerToString p)


pointerToString : Pointer -> String
pointerToString pointer =
    case pointer of
        [] ->
            ""

        _ ->
            pointer
                |> List.map escape
                |> String.join "/"
                |> String.append "/"


escape : String -> String
escape string =
    string
        |> String.split "~"
        |> String.join "~0"
        |> String.split "/"
        |> String.join "~1"
