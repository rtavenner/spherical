import Html exposing (Html)
import Html.Lazy exposing (lazy)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import WebGL exposing (Mesh, Shader)
import Keyboard
import Matrices exposing (..)
import Symmetry
import Time
import AnimationFrame
import Set exposing (Set)
import Dict exposing (Dict)



type alias Model = {pos : Mat4, keys : Set Int, marks : List Vec4, marksMesh : Mesh Vertex}
type Msg = 
    KeyDown Int
    | KeyUp Int
    | Tick Float

main : Program Never Model Msg
main =
    Html.program
        { init = {pos = Mat4.identity, keys = Set.empty, marks = [], marksMesh = marksMesh []} ! []
        , view = lazy view
        , subscriptions = 
            (\_ -> 
                Sub.batch 
                    [ Keyboard.downs KeyDown
                    , Keyboard.ups KeyUp
                    , AnimationFrame.diffs (Tick << Time.inSeconds)]) 
        , update = update
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        KeyDown 32-> 
            updateMarksMesh {model | marks = transform4 model.pos (vec4 0 0 0 1) :: model.marks} ! [] 
        KeyDown k -> {model | keys = Set.insert k model.keys} ! []
        KeyUp   k -> {model | keys = Set.remove k model.keys} ! []
        Tick dt ->
            if not <| List.any (flip Set.member model.keys) (Dict.keys keys)
            then model ! []
            else
                { model
                | pos =
                    --model.pos
                    --|> Mat4.mul (if Set.member 73 model.keys then (turnUp      ( 0.5 * dt)) else Mat4.identity) 
                    --|> Mat4.mul (if Set.member 74 model.keys then (turnRight   (-0.5 * dt)) else Mat4.identity) 
                    --|> Mat4.mul (if Set.member 75 model.keys then (turnUp      (-0.5 * dt)) else Mat4.identity) 
                    --|> Mat4.mul (if Set.member 76 model.keys then (turnRight   ( 0.5 * dt)) else Mat4.identity) 
                    --|> Mat4.mul (if Set.member 79 model.keys then (spin        ( 0.5 * dt)) else Mat4.identity) 
                    --|> Mat4.mul (if Set.member 85 model.keys then (spin        (-0.5 * dt)) else Mat4.identity) 
                    --|> Mat4.mul (if Set.member 87 model.keys then (moveForward ( 0.5 * dt)) else Mat4.identity) 
                    --|> Mat4.mul (if Set.member 83 model.keys then (moveForward (-0.5 * dt)) else Mat4.identity) 
                    --|> Mat4.mul (if Set.member 65 model.keys then (moveRight   (-0.5 * dt)) else Mat4.identity) 
                    --|> Mat4.mul (if Set.member 68 model.keys then (moveRight   ( 0.5 * dt)) else Mat4.identity) 
                    --|> Mat4.mul (if Set.member 81 model.keys then (moveUp      ( 0.5 * dt)) else Mat4.identity) 
                    --|> Mat4.mul (if Set.member 69 model.keys then (moveUp      (-0.5 * dt)) else Mat4.identity) 
                    Set.foldr 
                        (\k pos -> 
                            case Dict.get k keys of
                                Nothing -> pos
                                Just f -> Mat4.mul (f dt) pos) 
                        model.pos 
                        model.keys
                } ! []

keys : Dict Int (Float -> Mat4)
keys = Dict.fromList
    [ (73, \dt -> turnUp      ( 0.5 * dt))
    , (74, \dt -> turnRight   (-0.5 * dt))
    , (75, \dt -> turnUp      (-0.5 * dt))
    , (76, \dt -> turnRight   ( 0.5 * dt))
    , (79, \dt -> spin        ( 0.5 * dt))
    , (85, \dt -> spin        (-0.5 * dt))
    , (87, \dt -> moveForward ( 0.5 * dt))
    , (83, \dt -> moveForward (-0.5 * dt))
    , (65, \dt -> moveRight   (-0.5 * dt))
    , (68, \dt -> moveRight   ( 0.5 * dt))
    , (81, \dt -> moveUp      ( 0.5 * dt))
    , (69, \dt -> moveUp      (-0.5 * dt))
    ]

updateMarksMesh : Model -> Model
updateMarksMesh model = {model | marksMesh = marksMesh model.marks}

view : Model -> Html Msg
view model =
    WebGL.toHtml
        [ width 1200
        , height 800
        , style [ ( "display", "block" ), ("border", "10px solid black"), ("background-color", "black") ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh2
            (uniforms model)
        , WebGL.entity
            vertexShader
            fragmentShader
            mesh
            (uniforms model)
        , WebGL.entity
            vertexShader
            fragmentShader
            model.marksMesh
            (uniforms model)
        ]

uniforms : Model -> Uniforms
uniforms model =
    { rotation = model.pos
    , perspective = Mat4.makePerspective 80 (3/2) 0.01 10
    , shade = 0.8
    }

type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , shade : Float
    }

type alias Vertex =
    { color : Vec3
    , position : Vec4 -- Should have x^2+y^2+z^2+w^2 == 1
    }

mkVertex : Vec3 -> Vec4 -> Vertex
mkVertex color position = Vertex color (Vec4.normalize position)

mesh =
    WebGL.triangles
        (List.concatMap
            (\m -> List.map (\(a,b,c) -> (mkVertex (vec3 1 1 1) a, mkVertex (vec3 1 1 1) b, mkVertex (vec3 1 1 1) c))
                -- Messed around with the numbers until it looked good.
                [ ( transform4 m (vec4 0.0000 0.2618 -0.1 1)
                  , transform4 m (vec4 0.1618 0.1618 -0.1618 1)
                  , transform4 m (vec4 0.1618 (0.1618 + 0.100) (-0.1618 - 0.162) (0.94)))
                ]
                )
            (Symmetry.h4))

mesh2 =
    WebGL.lines
        (List.concatMap
            (\m -> List.map (\(a,b) -> (mkVertex (vec3 0.5 0.5 0.5) a, mkVertex (vec3 0.5 0.5 0.5) b))
                [ ( transform4 m (vec4 0.0000 0.2618 -0.1 1)
                  , transform4 m (vec4 0.0000 0.2618  0.1 1))
                , ( transform4 m (vec4 0.1618 0.1618 -0.1618 1)
                  , transform4 m (vec4 0.1618 (0.1618 + 0.100) (-0.1618 - 0.162) (0.94)))
                ]
                )
            (Symmetry.h4))

marksMesh : List Vec4 -> Mesh Vertex
marksMesh marks =
    WebGL.triangles
        (List.concatMap
            (\v -> 
                let (x,y,z,w) = Vec4.toTuple v
                    v2 = Vec4.fromTuple (y,-x,w,-z) |> Vec4.scale 0.01
                    v3 = Vec4.fromTuple (z,w,-x,-y) |> Vec4.scale 0.01
                    v4 = Vec4.fromTuple (-w,z,-y,x) |> Vec4.scale 0.01
                    red = vec3 1 0 0
                in
                    [ ( mkVertex red (Vec4.add v v2)
                      , mkVertex red (Vec4.add v v3)
                      , mkVertex red (Vec4.add v v4))
                    , ( mkVertex red v
                      , mkVertex red (Vec4.add v v3)
                      , mkVertex red (Vec4.add v v4))
                    , ( mkVertex red (Vec4.add v v2)
                      , mkVertex red v
                      , mkVertex red (Vec4.add v v4))
                    , ( mkVertex red (Vec4.add v v2)
                      , mkVertex red (Vec4.add v v3)
                      , mkVertex red v)
                    ])
            marks)



vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec4 position;
        attribute vec3 color;
        uniform mat4 perspective;
        uniform mat4 rotation;
        varying vec3 vcolor;
        void main () {
            vec4 pos = rotation * position;
            gl_Position = perspective * (pos + vec4(0.0,0.0,0.0,1.0));
            vec4 relpos = pos - vec4(0.0,0.0,0.0,1.0);
            float dist2 = relpos.x * relpos.x + relpos.y * relpos.y + relpos.z * relpos.z + relpos.w * relpos.w;
            // vcolor = vec3(1.0,1.0,1.0) - ((1.0 - (dist2 / 4.0)) * (vec3(1.0,1.0,1.0) - color));
            vcolor = (1.0 - (dist2 / 4.0)) * color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        uniform float shade;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = clamp(shade * vec4(vcolor, 1), 0.1, 0.9);
        }

    |]
