module Symmetry exposing (h3,h4)
import Math.Matrix4 as Mat4 exposing (Mat4,mul)
import Matrices exposing (fromTuples, spin, moveForward)

type alias Symmetries = List Mat4

id = Mat4.identity

phi : Float
phi = (sqrt 5 + 1) / 2

triangular : Symmetries
triangular = 
    let rot1 = fromTuples
            ( (0,0,1,0)
            , (1,0,0,0)
            , (0,1,0,0)
            , (0,0,0,1))
    in 
        [ id
        , rot1
        , mul rot1 rot1
        ]

symmX =
    [ id
    , fromTuples
        ( ( 1, 0, 0,0)
        , ( 0,-1, 0,0)
        , ( 0, 0,-1,0)
        , ( 0, 0, 0,1)
        )
    ]
symmY =
    [ id
    , fromTuples
        ( (-1, 0, 0,0)
        , ( 0, 1, 0,0)
        , ( 0, 0,-1,0)
        , ( 0, 0, 0,1)
        )
    ]

rotateIntoPentPlane : Mat4
rotateIntoPentPlane =
    Matrices.turnUp (atan (1/phi))

rotateOutOfPentPlane : Mat4
rotateOutOfPentPlane =
    Matrices.turnUp (atan (-1/phi))


combineSymms : Symmetries -> Symmetries -> Symmetries
combineSymms a b = List.concatMap (\a -> List.map (mul a) b) a

tetrahedral : Symmetries
tetrahedral = combineSymms (combineSymms symmX symmY) triangular

h3pentplane : Symmetries
h3pentplane = combineSymms (combineSymms tetrahedral [rotateIntoPentPlane]) pentagonal

h3 : Symmetries
h3 = combineSymms h3pentplane [rotateOutOfPentPlane]

pentagonal : Symmetries
pentagonal =
    let rot n = spin (2*pi*n/5)
        --fromTuples
        --    ( (cos (2*pi*n/5),-(sin (2*pi*n/5)),0,0)
        --    , (sin (2*pi*n/5),  cos (2*pi*n/5) ,0,0)
        --    , (             0,                0,1,0)
        --    , (             0,                0,0,1)
        --    )
    in List.map (rot << toFloat) (List.range 0 4)

column : Symmetries
column = 
    let rot n = mul (moveForward (pi*n/5)) (spin (pi*n/5))
        --fromTuples
        --    ( (cos (pi*n/5),-(sin (pi*n/5)),           0,              0)
        --    , (sin (pi*n/5),  cos (pi*n/5) ,           0,              0)
        --    , (           0,              0,cos (pi*n/5),-(sin (pi*n/5)))
        --    , (           0,              0,sin (pi*n/5),  cos (pi*n/5) )
        --    )
    in List.map (rot << toFloat) (List.range 0 9)

column2 : Symmetries
column2 = combineSymms h3pentplane column

nextColumn : Mat4
nextColumn =
    Mat4.mul rotateOutOfPentPlane 
    <| Mat4.mul rotateOutOfPentPlane
    <| Mat4.mul (moveForward (pi/5))
    <| Mat4.mul (spin (4*pi/5))
    <| Mat4.mul rotateOutOfPentPlane
    <| Mat4.mul rotateOutOfPentPlane
    <| spin pi

h4 : Symmetries
h4 =
    let a = nextColumn
        b = Mat4.mul nextColumn nextColumn
    in  [ id -- our column

        , a  -- neigboring columns
        , Mat4.mul a (spin (2*pi/5))
        , Mat4.mul a (spin (4*pi/5))
        , Mat4.mul a (spin (6*pi/5))
        , Mat4.mul a (spin (8*pi/5))

        , b -- neigboring colums of antipode
        , Mat4.mul b (spin (2*pi/5))
        , Mat4.mul b (spin (4*pi/5))
        , Mat4.mul b (spin (6*pi/5))
        , Mat4.mul b (spin (8*pi/5))

        -- antipode
        , Mat4.mul a (Mat4.mul (spin (2*pi/5)) b)
        ]
        |> combineSymms column2




