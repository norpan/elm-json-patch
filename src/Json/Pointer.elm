module Json.Pointer exposing
    ( Pointer
    , addAt, removeAt, getAt
    , encoder, decoder
    , Error(..), errorToString
    )

{-| This module implements JSON Pointer as per
[RFC 6901](https://tools.ietf.org/html/rfc6901).


# Type

@docs Pointer


# Operations

@docs addAt, removeAt, getAt


# Encoder/Decoder

@docs encoder, decoder


# Errors

@docs Error, errorToString

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


{-| An error encountered while using a pointer.
-}
type Error
    = FieldNotFound String
    | BadIndex String
    | IndexOutOfBounds Int
    | JsonError JD.Error


{-| Get the `Value` at the specified pointer.
-}
getAt : Pointer -> JD.Value -> Result Error JD.Value
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
                        |> Result.fromMaybe (FieldNotFound name)
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
                        |> Result.fromMaybe (FieldNotFound name)
                        |> Result.andThen (getAt names)
                )


indexRegex : Regex.Regex
indexRegex =
    Regex.fromString "^(-|0|[1-9][0-9]*)$"
        |> Maybe.withDefault Regex.never


getArrayIndex : String -> Array.Array a -> Result Error Int
getArrayIndex name array =
    if not (Regex.contains indexRegex name) then
        Err (BadIndex name)

    else if name == "-" then
        Ok (Array.length array)

    else
        String.toInt name |> Result.fromMaybe (BadIndex name)


{-| Add the specified value at the specified pointer.

For adding to an array, this means inserting the value at the specified index
, shifting the following elements to the right.

For adding to an object, this means adding or replacing the specified field.

-}
addAt : Pointer -> JD.Value -> JD.Value -> Result Error JD.Value
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
removeAt : Pointer -> JD.Value -> Result Error JD.Value
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
                    Err (FieldNotFound field)
        , path = path
        , operation = \rest -> removeAt rest
        , value = value
        }


handleCases :
    { whenEmpty : Result Error JD.Value
    , whenArray : Array.Array JD.Value -> Int -> Result Error JD.Value
    , whenObject : Dict.Dict String JD.Value -> String -> Result Error JD.Value
    , path : Pointer
    , operation : Pointer -> JD.Value -> Result Error JD.Value
    , value : JD.Value
    }
    -> Result Error JD.Value
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
                        |> Result.fromMaybe (FieldNotFound name)
                        |> Result.andThen (\x -> operation names x)
                        |> Result.map (\x -> JE.object (Dict.toList (Dict.insert name x object)))
                )


get : Int -> Array.Array a -> Result Error a
get i array =
    Array.get i array
        |> Result.fromMaybe (IndexOutOfBounds i)


insert : Int -> a -> Array.Array a -> Result Error (Array.Array a)
insert i a array =
    if i > Array.length array then
        Err (IndexOutOfBounds i)

    else
        Ok <|
            Array.append
                (Array.push a (Array.slice 0 i array))
                (Array.slice i (Array.length array) array)


remove : Int -> Array.Array a -> Result Error (Array.Array a)
remove i array =
    if i >= Array.length array then
        Err (IndexOutOfBounds i)

    else
        Ok <|
            Array.append
                (Array.slice 0 i array)
                (Array.slice (i + 1) (Array.length array) array)


arrayOrObject :
    JD.Value
    -> (Array.Array JD.Value -> Result Error a)
    -> (Dict.Dict String JD.Value -> Result Error a)
    -> Result Error a
arrayOrObject v ifArray ifObject =
    case
        JD.decodeValue (JD.list JD.value) v
    of
        Ok a ->
            Array.fromList a |> ifArray

        Err _ ->
            JD.decodeValue (JD.dict JD.value) v
                |> Result.mapError JsonError
                |> Result.andThen ifObject


{-| Convert a pointer error into a `String` that is nice for debugging.
-}
errorToString : Error -> String
errorToString error =
    case error of
        FieldNotFound n ->
            "Field not found: '" ++ n ++ "'."

        BadIndex i ->
            "Bad index: '" ++ i ++ "'."

        IndexOutOfBounds i ->
            "Index out of bounds: '" ++ String.fromInt i ++ "'."

        JsonError e ->
            JD.errorToString e



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
