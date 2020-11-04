module Form.Field exposing (Field, init, toActual, toRaw, update)


type Field a
    = Field (Internal a)


type alias Internal a =
    { raw : String
    , actual : Result String a
    , parser : String -> Result String a
    }


toRaw : Field a -> String
toRaw (Field { raw }) =
    raw


toActual : Field a -> Result String a
toActual (Field { actual }) =
    actual


init : { value : String, parser : String -> Result String a } -> Field a
init { value, parser } =
    Field { raw = value, parser = parser, actual = parser value }


update : String -> Field a -> Field a
update newRaw (Field field) =
    Field { field | raw = newRaw, actual = field.parser newRaw }
