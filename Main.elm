import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import WebGL exposing (Mesh, Shader)
import Keyboard
import Matrices exposing (..)
import Symmetry

type alias Model = Mat4
type Msg = Key Int

main : Program Never Model Msg
main =
    Html.program
        { init = ( Mat4.identity, Cmd.none )
        , view = view
        , subscriptions = (\_ -> Keyboard.downs Key) 
        , update = update
        }

update msg mat = 
    case msg of
        Key 73 -> Mat4.mul (turnUp       0.030) mat ! []
        Key 74 -> Mat4.mul (turnRight   -0.030) mat ! []
        Key 75 -> Mat4.mul (turnUp      -0.030) mat ! []
        Key 76 -> Mat4.mul (turnRight    0.030) mat ! []
        Key 87 -> Mat4.mul (moveForward  0.015) mat ! []
        Key 65 -> Mat4.mul (moveRight   -0.015) mat ! []
        Key 83 -> Mat4.mul (moveForward -0.015) mat ! []
        Key 68 -> Mat4.mul (moveRight    0.015) mat ! []
        Key _ -> mat ! []

view : Model -> Html Msg
view mat =
    WebGL.toHtml
        [ width 1200
        , height 800
        , style [ ( "display", "block" ), ("border", "10px solid black"), ("background-color", "black") ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh2
            { rotation = mat
            , perspective = Mat4.makePerspective 45 (3/2) 0.01 10
            , shade = 0.8
            }
        , WebGL.entity
            vertexShader
            fragmentShader
            mesh
            { rotation = mat
            , perspective = Mat4.makePerspective 45 (3/2) 0.01 10
            , shade = 0.8
            }
        ]


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
