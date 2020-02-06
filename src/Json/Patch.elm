module Json.Patch exposing
    ( Patch, Operation(..)
    , apply
    , encoder, decoder
    , Error(..), TestFailure, errorToString
    )

{-| This module implements JSON Patch as per
[RFC 6902](https://tools.ietf.org/html/rfc6902).


# Types

@docs Patch, Operation


# Operation

@docs apply


# Encoder/Decoder

@docs encoder, decoder


# Errors

@docs Error, TestFailure, errorToString

-}

import Dict
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Json.Encode as JE
import Json.Pointer as Pointer exposing (..)


{-| A `Patch` is a list of operations that are performed one after another.
-}
type alias Patch =
    List Operation


{-| An operation, as defined by RFC 6902, section 4.
-}
type Operation
    = Add Pointer JD.Value
    | Remove Pointer
    | Replace Pointer JD.Value
    | Move Pointer Pointer
    | Copy Pointer Pointer
    | Test Pointer JD.Value


{-| An error encountered while applying a patch, alongside the context of the patch being applied when it was
encountered.
-}
type alias ErrorWithContext =
    { index : Int
    , operation : Operation
    , error : Error
    }


{-| An error encountered while applying a patch.
-}
type Error
    = PointerError Pointer.Error
    | TestFailed TestFailure


{-| An error where the actual value did not match the expected one in a test operation.
-}
type alias TestFailure =
    { path : Pointer
    , expected : JD.Value
    , actual : JD.Value
    }


{-| Apply a `Patch` to a `Value`

Operations are applied to the `Value` in order, and if one operation fails,
the whole `Patch` fails, and an error is returned instead.

-}
apply : Patch -> JD.Value -> Result ErrorWithContext JD.Value
apply =
    let
        internalApply index rest part =
            case rest of
                [] ->
                    Ok part

                p :: ps ->
                    applyOperation index p part
                        |> Result.andThen (internalApply (index + 1) ps)
    in
    internalApply 0


applyOperation : Int -> Operation -> JD.Value -> Result ErrorWithContext JD.Value
applyOperation index op value =
    let
        opResult =
            case op of
                Add path v ->
                    addAt path v value |> Result.mapError PointerError

                Remove path ->
                    removeAt path value |> Result.mapError PointerError

                Replace path v ->
                    removeAt path value
                        |> Result.andThen (addAt path v)
                        |> Result.mapError PointerError

                Move from to ->
                    getAt from value
                        |> Result.andThen
                            (\x ->
                                removeAt from value
                                    |> Result.andThen (addAt to x)
                            )
                        |> Result.mapError PointerError

                Copy from to ->
                    getAt from value
                        |> Result.andThen
                            (\x ->
                                addAt to x value
                            )
                        |> Result.mapError PointerError

                Test at v ->
                    getAt at value
                        |> Result.mapError PointerError
                        |> Result.andThen
                            (\v_ ->
                                if equals v v_ then
                                    Ok value

                                else
                                    Err (TestFailed { path = at, expected = v, actual = v_ })
                            )
    in
    opResult |> Result.mapError (ErrorWithContext index op)


{-| Convert a patch application error into a `String` that is nice for debugging.
-}
errorToString : ErrorWithContext -> String
errorToString { index, operation, error } =
    let
        issue =
            case error of
                PointerError e ->
                    Pointer.errorToString e

                TestFailed { actual } ->
                    "Test failed: got '" ++ (actual |> JE.encode 0) ++ "'."
    in
    "During operation " ++ String.fromInt index ++ " '" ++ (operation |> encodeOperation |> JE.object |> JE.encode 0) ++ "': \n" ++ issue



-- DECODER


{-| JSON decoder for `Patch`
-}
decoder : JD.Decoder Patch
decoder =
    JD.list decodeOperation


decodeOperation : JD.Decoder Operation
decodeOperation =
    JD.field "op" JD.string
        |> JD.andThen decodeOperationCase


decodeOperationCase : String -> JD.Decoder Operation
decodeOperationCase operation =
    case operation of
        "add" ->
            JD.succeed Add
                |> JDP.required "path" Pointer.decoder
                |> JDP.required "value" JD.value

        "remove" ->
            JD.succeed Remove
                |> JDP.required "path" Pointer.decoder

        "replace" ->
            JD.succeed Replace
                |> JDP.required "path" Pointer.decoder
                |> JDP.required "value" JD.value

        "move" ->
            JD.succeed Move
                |> JDP.required "from" Pointer.decoder
                |> JDP.required "path" Pointer.decoder

        "copy" ->
            JD.succeed Copy
                |> JDP.required "from" Pointer.decoder
                |> JDP.required "path" Pointer.decoder

        "test" ->
            JD.succeed Test
                |> JDP.required "path" Pointer.decoder
                |> JDP.required "value" JD.value

        _ ->
            JD.fail <| "Unkown operation `" ++ operation ++ "`"



-- ENCODER


{-| JSON encoder for `Patch`
-}
encoder : Patch -> JE.Value
encoder patch =
    List.map encodeOperation patch
        |> JE.list JE.object


encodeOperation : Operation -> List ( String, JD.Value )
encodeOperation operation =
    case operation of
        Add path value ->
            [ ( "op", JE.string "add" )
            , ( "path", Pointer.encoder path )
            , ( "value", value )
            ]

        Remove path ->
            [ ( "op", JE.string "remove" )
            , ( "path", Pointer.encoder path )
            ]

        Replace path value ->
            [ ( "op", JE.string "replace" )
            , ( "path", Pointer.encoder path )
            , ( "value", value )
            ]

        Move from path ->
            [ ( "op", JE.string "move" )
            , ( "from", Pointer.encoder from )
            , ( "path", Pointer.encoder path )
            ]

        Copy from path ->
            [ ( "op", JE.string "copy" )
            , ( "from", Pointer.encoder from )
            , ( "path", Pointer.encoder path )
            ]

        Test path value ->
            [ ( "op", JE.string "test" )
            , ( "path", Pointer.encoder path )
            , ( "value", value )
            ]


{-| Compare two JSON values for equality as defined by section 4.6 of RFC 6902.
-}
equals : JD.Value -> JD.Value -> Bool
equals v1 v2 =
    List.any identity
        [ JD.decodeValue (JD.nullable JD.string) v1
            == JD.decodeValue (JD.nullable JD.string) v2
        , JD.decodeValue JD.bool v1
            == JD.decodeValue JD.bool v2
        , JD.decodeValue JD.float v1
            == JD.decodeValue JD.float v2
        , Result.map2 (List.map2 equals)
            (JD.decodeValue (JD.list JD.value) v1)
            (JD.decodeValue (JD.list JD.value) v2)
            |> Result.map (List.any identity)
            |> Result.withDefault False
        , Result.map2 (List.map2 (\( k1, v1_ ) ( k2, v2_ ) -> k1 == k2 && equals v1_ v2_))
            (Result.map Dict.toList (JD.decodeValue (JD.dict JD.value) v1))
            (Result.map Dict.toList (JD.decodeValue (JD.dict JD.value) v2))
            |> Result.map (List.any identity)
            |> Result.withDefault False
        ]
