module Json.Diff exposing (diff)

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Patch as Patch exposing (Patch)
import Json.Pointer exposing (Pointer)


type alias Options =
    { arrayDiff : Array Decode.Value -> Array Decode.Value -> Patch
    }


diff : Options -> Decode.Value -> Decode.Value -> Patch
diff opt new old =
    let
        values : Result Decode.Error ( Value, Value )
        values =
            Result.map2
                Tuple.pair
                (new |> Decode.decodeValue valueDecoder)
                (old |> Decode.decodeValue valueDecoder)
    in
    case values of
        Ok b ->
            case b of
                ( Array_ new_, Array_ old_ ) ->
                    opt.arrayDiff new_ old_

                ( Object_ new_, Object_ old_ ) ->
                    dictDiff opt new_ old_

                ( new_, old_ ) ->
                    if new_ == old_ then
                        []

                    else
                        [ Patch.Replace [] new
                        ]

        Err _ ->
            []


dictDiff : Options -> Dict String Decode.Value -> Dict String Decode.Value -> Patch
dictDiff opt new old =
    let
        newOnly : String -> Decode.Value -> Patch -> Patch
        newOnly k v acc =
            Patch.Add [ k ] v :: acc

        both : String -> Decode.Value -> Decode.Value -> Patch -> Patch
        both k new_ old_ acc =
            (diff opt new_ old_ |> List.map (mapOperationPointer (\v -> k :: v))) ++ acc

        oldOnly : String -> Decode.Value -> Patch -> Patch
        oldOnly k _ acc =
            Patch.Remove [ k ] :: acc
    in
    Dict.merge newOnly both oldOnly new old []
        |> List.reverse



--


mapOperationPointer : (Pointer -> Pointer) -> Patch.Operation -> Patch.Operation
mapOperationPointer fn a =
    case a of
        Patch.Add b c ->
            Patch.Add (fn b) c

        Patch.Remove b ->
            Patch.Remove (fn b)

        Patch.Replace b c ->
            Patch.Replace (fn b) c

        Patch.Move b c ->
            Patch.Move (fn b) (fn c)

        Patch.Copy b c ->
            Patch.Copy (fn b) (fn c)

        Patch.Test b c ->
            Patch.Test (fn b) c



--


type Value
    = Bool_ Bool
    | Int_ Int
    | Float_ Float
    | String_ String
    | Null_
    | Array_ (Array Decode.Value)
    | Object_ (Dict String Decode.Value)


valueDecoder : Decode.Decoder Value
valueDecoder =
    Decode.oneOf
        [ Decode.bool |> Decode.map Bool_
        , Decode.int |> Decode.map Int_
        , Decode.float |> Decode.map Float_
        , Decode.string |> Decode.map String_
        , Decode.null Null_
        , Decode.array Decode.value |> Decode.map Array_
        , Decode.dict Decode.value |> Decode.map Object_
        ]
