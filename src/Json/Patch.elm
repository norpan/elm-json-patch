module Json.Patch exposing (Patch, Operation(..), apply, decoder, encoder)

{-| This module implements JSON Patch as per
[RFC 6902](https://tools.ietf.org/html/rfc6902).

# Types
@docs Patch, Operation

# Operation
@docs apply

# Encoder/Decoder
@docs encoder, decoder
-}

import Json.Decode as JD
import Json.Decode.Pipeline as JD
import Json.Encode as JE
import Json.Pointer as Pointer
import Json.Pointer exposing (..)
import Dict


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


{-| Apply a `Patch` to a `Value`

Operations are applied to the `Value` in order, and if one operation fails,
the whole `Patch` fails, and an error is returned instead.
-}
apply : Patch -> JD.Value -> Result String JD.Value
apply patch value =
    case patch of
        [] ->
            Ok value

        p :: ps ->
            applyOperation p value
                |> Result.andThen (apply ps)


applyOperation : Operation -> JD.Value -> Result String JD.Value
applyOperation op value =
    case op of
        Add path v ->
            addAt path v value

        Remove path ->
            removeAt path value

        Replace path v ->
            removeAt path value
                |> Result.andThen (addAt path v)

        Move from to ->
            getAt from value
                |> Result.andThen
                    (\x ->
                        removeAt from value
                            |> Result.andThen (addAt to x)
                    )

        Copy from to ->
            getAt from value
                |> Result.andThen
                    (\x ->
                        addAt to x value
                    )

        Test at v ->
            getAt at value
                |> Result.andThen
                    (\v_ ->
                        if equals v v_ then
                            Ok value
                        else
                            Err "test failed"
                    )



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
            JD.decode Add
                |> JD.required "path" Pointer.decoder
                |> JD.required "value" JD.value

        "remove" ->
            JD.decode Remove
                |> JD.required "path" Pointer.decoder

        "replace" ->
            JD.decode Replace
                |> JD.required "path" Pointer.decoder
                |> JD.required "value" JD.value

        "move" ->
            JD.decode Move
                |> JD.required "from" Pointer.decoder
                |> JD.required "path" Pointer.decoder

        "copy" ->
            JD.decode Copy
                |> JD.required "from" Pointer.decoder
                |> JD.required "path" Pointer.decoder

        "test" ->
            JD.decode Test
                |> JD.required "path" Pointer.decoder
                |> JD.required "value" JD.value

        _ ->
            JD.fail <| "Unkown operation `" ++ operation ++ "`"



-- ENCODER


{-| JSON encoder for `Patch`
-}
encoder : Patch -> JE.Value
encoder patch =
    List.map encodeOperation patch
        |> List.map JE.object
        |> JE.list


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
        , Result.map2 (List.map2 (\( k1, v1 ) ( k2, v2 ) -> k1 == k2 && equals v1 v2))
            (Result.map Dict.toList (JD.decodeValue (JD.dict JD.value) v1))
            (Result.map Dict.toList (JD.decodeValue (JD.dict JD.value) v2))
            |> Result.map (List.any identity)
            |> Result.withDefault False
        ]
