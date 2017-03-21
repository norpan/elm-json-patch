# JSON Patch

This library implements [RFC 6902](https://tools.ietf.org/html/rfc6902 RFC6902), JSON Patch (and [RFC 6901](https://tools.ietf.org/html/rfc6901), JSON Pointer) for Elm.

## Usage
Let's say you previously have gotten a JSON document as a `Value` from the server or via a port. Now you get a `Value` that is a JSON Patch for that document.

```elm
newDocument =
    Json.Decoder.decodeValue Json.Patch.decoder patch
        |> Result.andThen (\p -> Json.Patch.apply p document)
```

Patching needs to be done on the `Value` type, due to the type system (records can't be accessed by field name in Elm).

However, if you have an encoder/decoder pair for your Elm type, you can patch the Elm type like this:
```elm
newElmData =
    Json.Decoder.decodeValue Json.Patch.decoder patch
        |> Result.andThen
            (\p ->
                dataEncoder elmData
                    |> Json.Patch.apply p
                    |> Result.andThen (Json.Decoder.decodeValue dataDecoder)
            )
```
