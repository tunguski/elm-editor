module Eval.WebGL exposing (processor)

{-| The interpreter's elm-explorations/webgl + linear-algebra builtins, as an
{@link Eval.Core.Processor}. Most evaluate to opaque values the editor's preview just counts (the
small interpreter can't run GPU shaders; the JS backend does the real rendering). The exceptions are
the `Vec3`/`Vec2` ops on *concrete* vectors, which are computed for real — the examples' physics needs
the numbers (e.g. `Vec3.getY position > eyeLevel`). `Browser.Dom`/`WebGL.Texture` tasks are opaque
too. -}

import Eval.Core exposing (Core, Processor)
import Lang exposing (Globals, Value(..))


processor : Processor
processor =
    { names = names
    , arities = arities
    , run = run
    }


names : List String
names =
    [ "WebGL.toHtml", "WebGL.toHtmlWith", "WebGL.entity", "WebGL.entityWith" ]
        ++ [ "WebGL.triangles", "WebGL.indexedTriangles", "WebGL.lines", "WebGL.lineStrip", "WebGL.lineLoop", "WebGL.points", "WebGL.triangleStrip", "WebGL.triangleFan" ]
        ++ [ "WebGL.clearColor", "WebGL.depth", "WebGL.alpha", "WebGL.antialias", "WebGL.Texture.load", "WebGL.Texture.size" ]
        ++ [ "vec2", "vec3", "vec4" ]
        ++ [ "Mat4.makePerspective", "Mat4.makeLookAt", "Mat4.makeRotate", "Mat4.makeTranslate", "Mat4.makeScale", "Mat4.mul", "Mat4.mulAffine", "Mat4.transform", "Mat4.inverse", "Mat4.transpose", "Mat4.makeOrtho2D" ]
        ++ [ "Vec3.add", "Vec3.sub", "Vec3.scale", "Vec3.normalize", "Vec3.negate", "Vec3.dot", "Vec3.cross", "Vec3.length", "Vec3.distance", "Vec3.direction", "Vec3.getX", "Vec3.getY", "Vec3.getZ", "Vec3.setX", "Vec3.setY", "Vec3.setZ", "Vec3.i", "Vec3.j", "Vec3.k", "Vec3.fromRecord", "Vec3.toRecord" ]
        ++ [ "Vec2.add", "Vec2.sub", "Vec2.scale", "Vec2.normalize", "Vec2.length", "Vec2.getX", "Vec2.getY" ]
        ++ [ "Texture.load", "Texture.loadWith", "Texture.size", "Texture.nearest", "Texture.linear", "Texture.repeat", "Texture.clampToEdge", "Texture.mirroredRepeat", "Texture.nearestMipmapNearest", "Texture.linearMipmapLinear" ]
        ++ [ "Dom.getViewport", "Dom.getViewportOf", "Dom.setViewportOf" ]


arities : List ( Int, List String )
arities =
    [ ( 1, [ "WebGL.triangles", "WebGL.lines", "WebGL.lineStrip", "WebGL.lineLoop", "WebGL.points", "WebGL.triangleStrip", "WebGL.triangleFan", "WebGL.depth", "WebGL.alpha", "WebGL.Texture.load", "WebGL.Texture.size", "Mat4.makeTranslate", "Mat4.makeScale", "Mat4.inverse", "Mat4.transpose" ]
            ++ [ "Vec3.normalize", "Vec3.negate", "Vec3.length", "Vec3.getX", "Vec3.getY", "Vec3.getZ", "Vec3.fromRecord", "Vec3.toRecord", "Vec2.normalize", "Vec2.length", "Vec2.getX", "Vec2.getY", "Texture.load", "Texture.size" ]
      )
    , ( 3, [ "WebGL.toHtmlWith", "vec3", "Mat4.makeLookAt" ] )
    , ( 4, [ "WebGL.entity", "vec4", "WebGL.clearColor", "Mat4.makePerspective", "Mat4.makeOrtho2D" ] )
    , ( 5, [ "WebGL.entityWith" ] )
    ]


run : Core -> Globals -> String -> List Value -> Maybe (Result String Value)
run _ _ name args =
    if name == "WebGL.toHtml" then
        case args of
            [ attrs, entities ] ->
                Just (Ok (webglScene attrs entities))

            _ ->
                Just (Err "WebGL.toHtml needs attributes and entities")

    else if name == "WebGL.toHtmlWith" then
        case args of
            [ _, attrs, entities ] ->
                Just (Ok (webglScene attrs entities))

            _ ->
                Just (Err "WebGL.toHtmlWith needs options, attributes and entities")

    else if List.member name vec3Ops then
        -- Linear algebra on concrete vectors is computed for real; a non-concrete argument (a
        -- symbolic Mat4 result bound for the GPU) falls through to an opaque value instead.
        case vecBuiltin name args of
            Just result ->
                Just result

            Nothing ->
                Just (Ok (VCtor name args))

    else if List.member name names then
        -- Meshes, entities, vectors, matrices, textures, Dom tasks: opaque values the preview counts.
        Just (Ok (VCtor name args))

    else
        Nothing


{-| A WebGL scene as a structured value: the canvas attributes and the list of entities. The editor
renders it live via the JS WebGL runtime; the Java interpreter keeps it as data for tests. -}
webglScene : Value -> Value -> Value
webglScene attrs entities =
    VCtor "WebGL.scene" [ attrs, entities ]


{-| The linear-algebra builtins evaluated on concrete vectors (rather than kept opaque for the GPU). -}
vec3Ops : List String
vec3Ops =
    [ "Vec3.getX", "Vec3.getY", "Vec3.getZ", "Vec3.setX", "Vec3.setY", "Vec3.setZ" ]
        ++ [ "Vec3.add", "Vec3.sub", "Vec3.scale", "Vec3.negate", "Vec3.dot", "Vec3.length" ]
        ++ [ "Vec3.distance", "Vec3.normalize", "Vec3.cross", "Vec3.direction" ]
        ++ [ "Vec2.getX", "Vec2.getY" ]


{-| The three components of a concrete `vec3`, or `Nothing` when the value is symbolic. -}
vec3Of : Value -> Maybe ( Float, Float, Float )
vec3Of v =
    case v of
        VCtor "vec3" [ VNum x, VNum y, VNum z ] ->
            Just ( x, y, z )

        _ ->
            Nothing


{-| The two components of a concrete `vec2`. -}
vec2Of : Value -> Maybe ( Float, Float )
vec2Of v =
    case v of
        VCtor "vec2" [ VNum x, VNum y ] ->
            Just ( x, y )

        _ ->
            Nothing


mkVec3 : Float -> Float -> Float -> Value
mkVec3 x y z =
    VCtor "vec3" [ VNum x, VNum y, VNum z ]


{-| Lifts a binary op over two concrete `vec3`s (both must be concrete, else `Nothing`). -}
vec3Map2 : (( Float, Float, Float ) -> ( Float, Float, Float ) -> Value) -> Value -> Value -> Maybe (Result String Value)
vec3Map2 f a b =
    Maybe.map2 (\va vb -> Ok (f va vb)) (vec3Of a) (vec3Of b)


{-| The concrete-vector arithmetic; `Nothing` when an argument isn't a concrete `vec2`/`vec3`. -}
vecBuiltin : String -> List Value -> Maybe (Result String Value)
vecBuiltin name args =
    case ( name, args ) of
        ( "Vec3.getX", [ v ] ) ->
            Maybe.map (\( x, _, _ ) -> Ok (VNum x)) (vec3Of v)

        ( "Vec3.getY", [ v ] ) ->
            Maybe.map (\( _, y, _ ) -> Ok (VNum y)) (vec3Of v)

        ( "Vec3.getZ", [ v ] ) ->
            Maybe.map (\( _, _, z ) -> Ok (VNum z)) (vec3Of v)

        ( "Vec3.setX", [ VNum n, v ] ) ->
            Maybe.map (\( _, y, z ) -> Ok (mkVec3 n y z)) (vec3Of v)

        ( "Vec3.setY", [ VNum n, v ] ) ->
            Maybe.map (\( x, _, z ) -> Ok (mkVec3 x n z)) (vec3Of v)

        ( "Vec3.setZ", [ VNum n, v ] ) ->
            Maybe.map (\( x, y, _ ) -> Ok (mkVec3 x y n)) (vec3Of v)

        ( "Vec3.scale", [ VNum s, v ] ) ->
            Maybe.map (\( x, y, z ) -> Ok (mkVec3 (s * x) (s * y) (s * z))) (vec3Of v)

        ( "Vec3.negate", [ v ] ) ->
            Maybe.map (\( x, y, z ) -> Ok (mkVec3 (negate x) (negate y) (negate z))) (vec3Of v)

        ( "Vec3.add", [ a, b ] ) ->
            vec3Map2 (\( ax, ay, az ) ( bx, by, bz ) -> mkVec3 (ax + bx) (ay + by) (az + bz)) a b

        ( "Vec3.sub", [ a, b ] ) ->
            vec3Map2 (\( ax, ay, az ) ( bx, by, bz ) -> mkVec3 (ax - bx) (ay - by) (az - bz)) a b

        ( "Vec3.cross", [ a, b ] ) ->
            vec3Map2 (\( ax, ay, az ) ( bx, by, bz ) -> mkVec3 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)) a b

        ( "Vec3.dot", [ a, b ] ) ->
            Maybe.map2 (\( ax, ay, az ) ( bx, by, bz ) -> Ok (VNum (ax * bx + ay * by + az * bz))) (vec3Of a) (vec3Of b)

        ( "Vec3.length", [ v ] ) ->
            Maybe.map (\( x, y, z ) -> Ok (VNum (sqrt (x * x + y * y + z * z)))) (vec3Of v)

        ( "Vec3.distance", [ a, b ] ) ->
            Maybe.map2 (\( ax, ay, az ) ( bx, by, bz ) -> Ok (VNum (sqrt ((ax - bx) ^ 2 + (ay - by) ^ 2 + (az - bz) ^ 2)))) (vec3Of a) (vec3Of b)

        ( "Vec3.normalize", [ v ] ) ->
            Maybe.map (\( x, y, z ) -> Ok (normalizeVec3 x y z)) (vec3Of v)

        ( "Vec3.direction", [ a, b ] ) ->
            Maybe.map2 (\( ax, ay, az ) ( bx, by, bz ) -> Ok (normalizeVec3 (ax - bx) (ay - by) (az - bz))) (vec3Of a) (vec3Of b)

        ( "Vec2.getX", [ v ] ) ->
            Maybe.map (\( x, _ ) -> Ok (VNum x)) (vec2Of v)

        ( "Vec2.getY", [ v ] ) ->
            Maybe.map (\( _, y ) -> Ok (VNum y)) (vec2Of v)

        _ ->
            Nothing


normalizeVec3 : Float -> Float -> Float -> Value
normalizeVec3 x y z =
    let
        len =
            sqrt (x * x + y * y + z * z)
    in
    if len == 0 then
        mkVec3 0 0 0

    else
        mkVec3 (x / len) (y / len) (z / len)
