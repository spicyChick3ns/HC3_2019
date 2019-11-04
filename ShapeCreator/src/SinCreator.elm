{-  template for your comment
Melissa G + Ayana N
User:  Our imagined user is a child, Mateo,  aged 9, who does not have much knowledge on sinusoidal functions, like the average 9-year-old. This child is from Australia and has minimal understanding of waves, such as what a crest and trough is, and the optimal wave to surf is.
Activity:  Mateo is engaged in surfing along the Gold Coast.
Emotion:  Mateo loves to surf and play guitar, like Cody Simpson, and would like to see how waves work and the meaning behind it.
Tasks:  Certain tasks with regards to surfing involves having an understanding of the heights of waves (amplitude), periods, wavelengths, and frequencies. 
Typical Interaction:  Mateo does not understand what the variables behind waves are as he does not know the definitions. So, Mateo takes a look at the definitions and adjusts the arrows, with new labels, to observe the change in behaviour of the wave. For example, when he increases the frequency, he has a bumpy ride, and can fall off!! So, he understands when the frequency of the sine function is increased, his wave will go quicker in a smaller amount of time, which helps visualize that the period is 1 divided by the frequency. He understands that when the amplitude is increased, he rides the wave at a higher distance. He understands when the phase shift is changed, it means the position of the wave is shifted horizontally from the usual position! 
Principle 1:  Affordances. The editable X Sin in the window was confusing to us, it was not related to editable Y Sin, and seemed to serve no clear purpose. We decided to edit that part out to avoid confusion to our user, Mateo, as well as other users.
Principle 2:  The main principle is is discoverability. The conceptual model in turn enhances the discoverability. We Added labels for what the arrow buttons do to change the sinusoidal function. We also added a textbox explaining what those labels mean, using simple math terms a child like Mateo would understand, from: https://www.mathsisfun.com/algebra/amplitude-period-frequency-phase-shift.html. The labels on the buttons are only amplitude, frequency and phase shift. However, the information in the textbox includes more terms such as period and wavelength, as they are relevant to the main terms.
-}

module SinCreator exposing (..)

{-
Copyright 2017-2019 Christopher Kumar Anand,  Adele Olejarz, Chinmay Sheth, Yaminah Qureshi, Graeme Crawley and students of McMaster University.  Based on the Shape Creator by Levin Noronha.

   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution, and cite the paper

   @article{d_Alves_2018,
   title={Using Elm to Introduce Algebraic Thinking to K-8 Students},
   volume={270},
   ISSN={2075-2180},
   url={http://dx.doi.org/10.4204/EPTCS.270.2},
   DOI={10.4204/eptcs.270.2},
   journal={Electronic Proceedings in Theoretical Computer Science},
   publisher={Open Publishing Association},
   author={d’ Alves, Curtis and Bouman, Tanya and Schankula, Christopher and Hogg, Jenell and Noronha, Levin and Horsman, Emily and Siddiqui, Rumsha and Anand, Christopher Kumar},
   year={2018},
   month={May},
   pages={18–36}
   }

   3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR AN, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

import Array exposing (..)
import GraphicSVG exposing (..)
import GraphicSVG.EllieApp exposing (..)
import List
import ShapeCreateAssets exposing (..)


init =
    { time = Nothing
    , currentTime = 0
    , notify = NotifyTap
    , uArg = 0
    , vArg = 0
    , editableArg = 0
    , uDilation = 1
    , vDilation = 1
    , editableDilation = 0
    , editableShift = 0
    , uScale = 5
    , vScale = 5
    , editableScale = 0
    , uShift = 0
    , uShiftScale = 1
    , u = 1
    , v = 1
    , rScale = 1
    , gScale = 1
    , bScale = 1
    , rFun = OneFun
    , bFun = UFun
    , gFun = VFun
    , sinGraph = []
    , cosGraph = []
    , vTransparency = 0.5
    , trigCycleU = Sin
    , trigCycleV = Sin
    , latestPointV = ( 0, 0, rgb 160 128 96 )
    , uTransform = ScaleU
    , moveX = ZeroFun
    , moveY = UFunZero
    , moveX1 = UFunZero
    , moveY1 = ZeroFun
    , transformFun = ZeroFun
    , uCosGraph = 0
    , uSinGraph = 0
    , editableYSinForTransforms = 0
    , r = 0
    , g = 0
    , b = 0
    , currentButton = None
    , buttonDownTime = 0
    , transformsRightArrowTransp = 0.25
    , transformsLeftArrowTransp = 0.25

    --, transformsNumTransp = 0.25
    , moveTextX = 0.25
    , moveTextY = 0.25
    , moveTextX1 = 0.25
    , moveTextY1 = 0.25
    , rTransp = 0.25
    , gTransp = 0.25
    , bTransp = 0.25
    , addAnotherFuncTransp = 0.25
    , uTextTransp = 0.5
    , vTextTransp = 0.5
    , maxAmplitude = 40
    , maxFrequency = 10
    , maxShift = 2 * Basics.pi
    , cosWaveLength = 200
    , sinWaveLength = 100
    }


type Msg m
    = Tick Float GetKeyState
    | TransM (m -> m)
    | Notif Notifications
    | R
    | G
    | B
    | UScalePlus
    | UDilationPlus
    | UShiftPlus
    | UScaleMinus
    | UDilationMinus
    | UShiftMinus
    | EditableScalePlus
    | EditableDilationPlus
    | EditableScaleMinus
    | EditableDilationMinus
    | VScalePlus
    | VScaleMinus
    | VDilationPlus
    | VDilationMinus
    | TrigCycleU
    | TrigCycleV
    | UTransforms
    | UTransformsReverse
      --| MoveX
      --| MoveY
      --| MoveX1
      --| MoveY1
      --| TransformsFunctionChange
    | RScalePlus
    | RScaleMinus
    | GScalePlus
    | GScaleMinus
    | BScalePlus
    | BScaleMinus
    | ButtonDown ButtonDir
    | MouseUp


type Notifications
    = NotifyTap
    | NotifyTapAt
    | NotifyEnter
    | NotifyEnterAt
    | NotifyLeave
    | NotifyLeaveAt
    | NotifyMouseMoveAt
    | NotifyMouseDown
    | NotifyMouseDownAt
    | NotifyMouseUp
    | NotifyMouseUpAt
    | NotifyTouchStart
    | NotifyTouchStartAt
    | NotifyTouchEnd
    | NotifyTouchEndAt
    | NotifyTouchMoveAt


type FunType
    = OneFun
    | UFun
    | VFun


type Trig
    = Sin
    | Cos


type ZeroFunType
    = ZeroFun
    | UFunZero
    | NegUFun
    | VFunZero
    | NegVFun


type Transforms
    = ScaleU
    | MoveX
    | MoveY
    | MoveCircle
    | URotate
    | ScaleX
    | ScaleY
    | MakeTransparent


type ButtonDir
    = AmplitudeUp
    | AmplitudeDown
    | FrequencyUp
    | FrequencyDown
    | ShiftUp
    | ShiftDown
    | EditableAmplitudeUp
    | EditableAmplitudeDown
    | EditableFrequencyUp
    | EditableFrequencyDown
    | RedUp
    | RedDown
    | BlueUp
    | BlueDown
    | GreenUp
    | GreenDown
    | None
    | VUP
    | VDown


update msg model =
    case msg of
        Tick t _ ->
            let
                uArg =
                    model.uArg + model.uDilation * (t - (t - 0.05))

                vArg =
                    model.vArg + model.vDilation * (t - (t - 0.05))

                editableArg =
                    model.editableArg + model.editableDilation * (t - (t - 0.05))

                currentTime =
                    case model.time of
                        Nothing ->
                            0

                        Just ct ->
                            ct

                u =
                    model.uScale * evalTrig model.trigCycleU uArg

                v =
                    model.vScale * evalTrig model.trigCycleV vArg

                r =
                    clamp 0 255 (abs (model.rScale * eval model.rFun u v))

                g =
                    clamp 0 255 (abs (model.gScale * eval model.gFun u v))

                b =
                    clamp 0 255 (abs (model.bScale * eval model.bFun u v))

                uSinGraph =
                    model.uScale * sin uArg

                sinGraphPoint =
                    ( 0, uSinGraph, rgb r g b )

                cosGraphPoint =
                    ( uCosGraph, 0, rgb r g b )

                uCosGraph =
                    model.uScale * cos uArg

                editableYSinForTransforms =
                    model.editableScale * cos editableArg
            in
            { model
                | time = Just t
                , uArg = uArg
                , vArg = vArg
                , currentTime = currentTime
                , u = u
                , v = v
                , sinGraph =
                    List.take 2470
                        ([ sinGraphPoint ]
                            ++ List.filterMap
                                (\( xx, yy, cc ) ->
                                    if xx >= model.sinWaveLength then
                                        Nothing

                                    else
                                        Just ( xx + 0.35, yy, cc )
                                )
                                model.sinGraph
                        )
                , cosGraph =
                    List.take 2470
                        ([ cosGraphPoint ]
                            ++ List.filterMap
                                (\( xx, yy, cc ) ->
                                    -- Subtract 130 to account for the ratio of the screen and remove excess
                                    if yy <= -model.cosWaveLength then
                                        Nothing

                                    else
                                        Just ( xx, yy - 0.35, cc )
                                )
                                model.cosGraph
                        )
                , r = r
                , g = g
                , b = b
                , uCosGraph = uCosGraph
                , uSinGraph = uSinGraph

                --, editableYSinForTransforms = editableYSinForTransforms
                , buttonDownTime =
                    case model.currentButton of
                        None ->
                            0

                        _ ->
                            model.buttonDownTime + 0.1
                , uScale =
                    case model.currentButton of
                        AmplitudeUp ->
                            if model.uScale < model.maxAmplitude then
                                model.uScale + curveX model.buttonDownTime

                            else if model.uScale > model.maxAmplitude then
                                model.maxAmplitude

                            else
                                model.uScale

                        AmplitudeDown ->
                            if model.uScale > -model.maxAmplitude then
                                model.uScale - curveX model.buttonDownTime

                            else if model.uScale < -model.maxAmplitude then
                                -model.maxAmplitude

                            else
                                model.uScale

                        _ ->
                            model.uScale
                , uDilation =
                    case model.currentButton of
                        FrequencyUp ->
                            if model.uDilation < model.maxFrequency then
                                model.uDilation + curveX model.buttonDownTime

                            else if model.uDilation > model.maxFrequency then
                                model.maxFrequency

                            else
                                model.uDilation

                        FrequencyDown ->
                            if model.uDilation > -model.maxFrequency then
                                model.uDilation - curveX model.buttonDownTime

                            else if model.uDilation < -model.maxFrequency then
                                -model.maxFrequency

                            else
                                model.uDilation

                        _ ->
                            model.uDilation
                , uShift =
                    case model.currentButton of
                        ShiftUp ->
                            model.uShift + curveX model.buttonDownTime

                        ShiftDown ->
                            model.uShift - curveX model.buttonDownTime

                        _ ->
                            model.uShift
                , editableScale =
                    case model.currentButton of
                        EditableAmplitudeUp ->
                            if model.editableScale < model.maxAmplitude then
                                model.editableScale + curveX model.buttonDownTime

                            else if model.editableScale > model.maxAmplitude then
                                model.maxAmplitude

                            else
                                model.editableScale

                        EditableAmplitudeDown ->
                            if model.editableScale > -model.maxAmplitude then
                                model.editableScale - curveX model.buttonDownTime

                            else if model.editableScale < -model.maxAmplitude then
                                -model.maxAmplitude

                            else
                                model.editableScale

                        _ ->
                            model.editableScale
                , editableDilation =
                    case model.currentButton of
                        EditableFrequencyUp ->
                            if model.editableDilation < model.maxFrequency then
                                model.editableDilation + curveX model.buttonDownTime

                            else if model.editableDilation > model.maxFrequency then
                                model.maxFrequency

                            else
                                model.editableDilation

                        EditableFrequencyDown ->
                            if model.editableDilation > -model.maxFrequency then
                                model.editableDilation - curveX model.buttonDownTime

                            else if model.editableDilation < -model.maxFrequency then
                                -model.maxFrequency

                            else
                                model.editableDilation

                        _ ->
                            model.editableDilation
                , editableShift =
                    case model.currentButton of
                        ShiftUp ->
                            model.editableShift + curveX model.buttonDownTime

                        ShiftDown ->
                            model.editableShift - curveX model.buttonDownTime

                        _ ->
                            model.editableShift
                , rScale =
                    case model.currentButton of
                        RedUp ->
                            if model.rScale < 253 then
                                model.rScale + curveX model.buttonDownTime

                            else
                                model.rScale

                        RedDown ->
                            if model.rScale > 2 then
                                model.rScale - curveX model.buttonDownTime

                            else
                                model.rScale

                        _ ->
                            model.rScale
                , bScale =
                    case model.currentButton of
                        BlueUp ->
                            if model.bScale < 253 then
                                model.bScale + curveX model.buttonDownTime

                            else
                                model.bScale

                        BlueDown ->
                            if model.bScale > 2 then
                                model.bScale - curveX model.buttonDownTime

                            else
                                model.bScale

                        _ ->
                            model.bScale
                , gScale =
                    case model.currentButton of
                        GreenUp ->
                            if model.gScale < 252 then
                                model.gScale + curveX model.buttonDownTime

                            else
                                model.gScale

                        GreenDown ->
                            if model.gScale > 2 then
                                model.gScale - curveX model.buttonDownTime

                            else
                                model.gScale

                        _ ->
                            model.gScale
                , vScale =
                    case model.currentButton of
                        VUP ->
                            if model.vScale < 48 then
                                model.vScale + curveX model.buttonDownTime

                            else
                                model.vScale

                        VDown ->
                            if model.vScale > -48 then
                                model.vScale - curveX model.buttonDownTime

                            else
                                model.vScale

                        _ ->
                            model.vScale
            }

        TransM t ->
            t model

        -- ran out of room for notifications, but left them here for a possible future improvement
        Notif notif ->
            { model | notify = notif }

        R ->
            { model | rFun = cycleFun model.rFun }

        G ->
            { model | gFun = cycleFun model.gFun }

        B ->
            { model | bFun = cycleFun model.bFun }

        RScalePlus ->
            { model
                | rScale =
                    if model.rScale < 255 then
                        model.rScale + 1

                    else
                        model.rScale
            }

        RScaleMinus ->
            { model
                | rScale =
                    if model.rScale > 0 then
                        model.rScale - 1

                    else
                        model.rScale
            }

        GScalePlus ->
            { model
                | gScale =
                    if model.gScale < 255 then
                        model.gScale + 1

                    else
                        model.gScale
            }

        GScaleMinus ->
            { model
                | gScale =
                    if model.gScale > 0 then
                        model.gScale - 1

                    else
                        model.gScale
            }

        BScalePlus ->
            { model
                | bScale =
                    if model.bScale < 255 then
                        model.bScale + 1

                    else
                        model.bScale
            }

        BScaleMinus ->
            { model
                | bScale =
                    if model.bScale > 0 then
                        model.bScale - 1

                    else
                        model.bScale
            }

        UScalePlus ->
            { model
                | uScale =
                    if model.uScale < model.maxAmplitude then
                        model.uScale + 1

                    else
                        model.uScale
            }

        UScaleMinus ->
            { model
                | uScale =
                    if model.uScale > -model.maxAmplitude then
                        model.uScale - 1

                    else
                        model.uScale
            }

        UDilationPlus ->
            { model
                | uDilation =
                    if model.uDilation < model.maxFrequency then
                        model.uDilation + 1

                    else
                        model.uDilation
            }

        UDilationMinus ->
            { model
                | uDilation =
                    if model.uDilation > 0 then
                        model.uDilation - 1

                    else
                        model.uDilation
            }

        UShiftPlus ->
            { model
                | uArg =
                    model.uArg + model.uShiftScale * Basics.pi / 4
                , uShift = model.uShift + model.uShiftScale
            }

        UShiftMinus ->
            { model
                | uArg =
                    model.uArg - model.uShiftScale * Basics.pi / 4
                , uShift = model.uShift - model.uShiftScale
            }

        EditableScalePlus ->
            { model
                | editableScale =
                    if model.editableScale < model.maxAmplitude then
                        model.editableScale + 1

                    else
                        model.editableScale
            }

        EditableScaleMinus ->
            { model
                | editableScale =
                    if model.editableScale > -model.maxAmplitude then
                        model.editableScale - 1

                    else
                        model.editableScale
            }

        EditableDilationPlus ->
            { model
                | editableDilation =
                    if model.editableDilation < model.maxFrequency then
                        model.editableDilation + 1

                    else
                        model.editableDilation
            }

        EditableDilationMinus ->
            { model
                | editableDilation =
                    if model.editableDilation > -model.maxFrequency then
                        model.editableDilation - 1

                    else
                        model.editableDilation
            }

        VScalePlus ->
            { model
                | vScale =
                    if model.vScale < model.maxAmplitude then
                        model.vScale + 1

                    else
                        model.vScale
            }

        VScaleMinus ->
            { model
                | vScale =
                    if model.vScale > -model.maxAmplitude then
                        model.vScale - 1

                    else
                        model.vScale
            }

        VDilationPlus ->
            { model
                | vDilation =
                    if model.vDilation < model.maxFrequency then
                        model.vDilation + 1

                    else
                        model.vDilation
            }

        VDilationMinus ->
            { model | vDilation = model.vDilation - 1 }

        TrigCycleU ->
            { model | trigCycleU = cycleTrig model.trigCycleU }

        TrigCycleV ->
            { model | trigCycleV = cycleTrig model.trigCycleV }

        UTransforms ->
            { model | uTransform = cycleTransforms model.uTransform }

        UTransformsReverse ->
            { model | uTransform = cycleTransformsReverse model.uTransform }

        {-
           MoveX ->
               { model | moveX = cycleFunZero model.moveX }

           MoveY ->
               { model | moveY = cycleFunZero model.moveY }

           MoveX1 ->
               { model | moveX1 = cycleFunZero model.moveX1 }

           MoveY1 ->
               { model | moveY1 = cycleFunZero model.moveY1 }
           TransformsFunctionChange ->
                  { model | transformFun = cycleFunZero model.transformFun }
        -}
        ButtonDown dir ->
            { model | currentButton = dir }

        MouseUp ->
            { model | currentButton = None }



-- make the Collage fit in VGA screen minus menu bars, for Chromebooks and iPads


eval f u v =
    case f of
        OneFun ->
            u

        UFun ->
            u

        VFun ->
            v


showFun f u v =
    case f of
        OneFun ->
            "u"

        UFun ->
            "u"

        VFun ->
            "v"


cycleFun f =
    case f of
        OneFun ->
            UFun

        UFun ->
            VFun

        VFun ->
            OneFun


cycleTrig f =
    case f of
        Sin ->
            Cos

        Cos ->
            Sin


textTrig f =
    case f of
        Sin ->
            "sin"

        Cos ->
            "cos"


evalTrig f u =
    case f of
        Sin ->
            sin u

        Cos ->
            cos u


cycleFunZero f =
    case f of
        ZeroFun ->
            UFunZero

        UFunZero ->
            NegUFun

        NegUFun ->
            VFunZero

        VFunZero ->
            NegVFun

        NegVFun ->
            ZeroFun


moveText mv =
    case mv of
        ZeroFun ->
            "u"

        UFunZero ->
            "u"

        NegUFun ->
            "-u"

        VFunZero ->
            "v"

        NegVFun ->
            "-v"



{-
   moveFun mv model =
       let
           u =
               model.u

           v =
               model.v
       in
       case mv of
           ZeroFun ->
               u

           UFunZero ->
               u

           NegUFun ->
               -u

           VFunZero ->
               v

           NegVFun ->
               -v
-}


cycleTransforms tr =
    case tr of
        ScaleU ->
            URotate

        URotate ->
            ScaleX

        ScaleX ->
            ScaleY

        ScaleY ->
            MakeTransparent

        MakeTransparent ->
            MoveX

        MoveX ->
            MoveY

        MoveY ->
            MoveCircle

        MoveCircle ->

            ScaleU


cycleTransformsReverse tr =
    case tr of
        URotate ->
            ScaleU

        ScaleX ->
            URotate

        ScaleY ->
            ScaleX

        MakeTransparent ->
            ScaleY

        MoveX ->
            MakeTransparent

        MoveY ->
            MoveX

        MoveCircle ->
            MoveY


        ScaleU ->
            MoveCircle


applyTransforms tr model =
    let
        u =
            model.u
    in
    case tr of
        ScaleU ->
            scale ((model.uSinGraph + model.uScale) / 10)

        MoveX ->
            move ( model.uCosGraph, 0 )

        MoveY ->
            move ( 0, model.uSinGraph )

        MoveCircle ->
            move ( model.uCosGraph, model.uSinGraph )

        URotate ->
            rotate (u / 10)

        ScaleX ->
            scaleX ((model.uCosGraph + model.uScale) / 10)

        ScaleY ->
            scaleY ((model.uSinGraph + model.uScale) / 10)

        MakeTransparent ->
            makeTransparent u



applyTransformsText tr =
    case tr of
        MoveX ->
            " move x "

        MoveY ->
            " move y "

        MoveCircle ->
            " move in a circle "

        ScaleU ->
            " scale "

        URotate ->
            " rotate "

        ScaleX ->
            " scaleX "

        ScaleY ->
            " scaleY "

        MakeTransparent ->
            " makeTransparent "



applyTransformsYourCode model tr =
    case tr of
        MoveX ->
            "|> move (" ++ String.fromFloat model.uScale ++ "*cos(model.time) , 0)"

        MoveY ->
            "|> move (0 , " ++ String.fromFloat model.uScale ++ "*sin(model.time))"

        MoveCircle ->
            "|> move (" ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model ++ ", " ++ String.fromFloat model.uScale ++ "*cos(" ++ cosinString model

        ScaleU ->
            "|> scale " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model

        URotate ->
            "|> rotate " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model

        ScaleX ->
            "|> scaleX " ++ String.fromFloat model.uScale ++ "*cos(" ++ cosinString model

        ScaleY ->
            "|> scaleY " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model

        MakeTransparent ->
            "|> makeTransparent " ++ String.fromFloat model.uScale ++ "*sin(" ++ cosinString model




-- change you app's state based on your new messages


numGraphPoints model =
    round 2505


curveX x =
    Basics.toFloat (round (clamp 0 12 (x ^ 2) / 4))


sinCurve model =
    let
        points =
            List.map2 (\x y -> ( x, y )) model.sinGraph (List.drop 1 model.sinGraph)
    in
    List.take (numGraphPoints model) (List.map (\( ( a, b, col1 ), ( c, d, col2 ) ) -> line ( a, b ) ( c, d ) |> outlined (solid 1) col1) points)


cosCurve model =
    let
        points =
            List.map2 (\x y -> ( x, y )) model.cosGraph (List.drop 1 model.cosGraph)
    in
    List.take (numGraphPoints model - 1) (List.map (\( ( a, b, col1 ), ( c, d, col2 ) ) -> line ( a, b ) ( c, d ) |> outlined (solid 1) col1) points)


cosinString model =
    let
        fraction =
            if (model.uShift / 8 * 2) < 0 then
                showDigits 5 (model.uShift / 8 * 2)

            else
                "+" ++ showDigits 4 (model.uShift / 8 * 2)
    in
    showDigits 2 model.uDilation ++ "*model.time" ++ fraction ++ "*Pi)"


view model =
    let
        uScale =
            model.uScale

        u =
            model.u

        v =
            model.v

        uArg =
            model.uArg

        x1 =
            if model.uTransform == MakeTransparent then
                90

            else
                45

        notTrigCycleU =
            if model.trigCycleU == Sin then
                cos

            else
                sin

        tt str =
            str |> text |> serif |> italic |> size 10 |> filled titleColour

        x2 =
            if model.uTransform == MakeTransparent then
                116

            else
                81

        yourCodeGroup =
            group
                [ rect 200 100 |> outlined (solid 1) red |> makeTransparent 0.25 |> move ( 100, 20 )
                , copiable "--Add these new definitions to your code" |> move ( 0, 60 )
                , copiable ("u = " ++ String.fromFloat model.uScale ++ "*" ++ textTrig model.trigCycleU ++ "(" ++ String.fromFloat model.uDilation ++ "*model.time+" ++ String.fromFloat model.uShift ++ ")") |> move ( 0, 50 )
                , copiable "mySquare = square 15" |> move ( 0, 30 )
                , copiable ("  |> outlined (solid 0.25) rgb (" ++ String.fromFloat model.rScale ++ "*" ++ showFun model.rFun u v ++ " " ++ String.fromFloat model.gScale ++ "*" ++ showFun model.gFun u v ++ " " ++ String.fromFloat model.bScale ++ "*" ++ showFun model.bFun u v ++ ")") |> move ( 35, 20 )
                , copiable ("  " ++ applyTransformsYourCode model model.uTransform) |> move ( 35, 10 )
                , copiable ("  |> move(" ++ moveText model.moveX1 ++ "," ++ moveText model.moveY1 ++ ")") |> move ( 35, 0 )
                , copiable "--Add the following code to your shapes:" |> move ( 0, -10 )
                , copiable "mySquare" |> move ( 10, -20 )
                ]

        transformsGraphicsGroup =
            group
                [ rect 210 200 |> outlined (solid 1) red |> makeTransparent 0.25 |> move ( 45, 70 )
                , square 15 |> outlined (solid 1) (rgb model.r model.g model.b) |> applyTransforms model.uTransform model |> move ( 45, 60 )
                , group
                    [ text (applyTransformsText model.uTransform) |> size 10 |> filled black |> move ( 4, 105 )
                    , triangle 8 |> filled (rgb 255 10 10) |> rotate (degrees 180) |> notifyTap UTransformsReverse |> move ( -70, 105 ) |> notifyLeave (TransM (\m -> { m | transformsLeftArrowTransp = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsLeftArrowTransp = 1 })) |> makeTransparent model.transformsLeftArrowTransp
                    , triangle 8 |> filled (rgb 255 10 10) |> notifyTap UTransforms |> move ( 100, 105 ) |> notifyLeave (TransM (\m -> { m | transformsRightArrowTransp = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsRightArrowTransp = 1 })) |> makeTransparent model.transformsRightArrowTransp

                    --, text (moveText model.transformFun) |> size 10 |> filled black |> notifyTap TransformsFunctionChange |> move ( x1, 105 ) |> notifyLeave (TransM (\m -> { m | transformsNumTransp = 0.25 })) |> notifyEnter (TransM (\m -> { m | transformsNumTransp = 1 })) |> makeTransparent model.transformsNumTransp
                    ]
                    |> move ( 30, 50 )
                ]
        functionDefText = 
            group 
                [
                    definition "Amplitude: is the height from"  |> move ( 0, 160 )
                    , definition "  the center line to the peak " |> move ( 0, 150 )
                    , definition "  (or to the trough). Or we" |> move ( 0, 140 )
                    , definition "  can measure the height" |> move ( 0, 130 )
                    , definition "  from highest to lowest" |> move (0, 120)
                    , definition "   points and divide that" |> move (0, 110)
                    , definition "  by 2." |> move (0, 100)
                    , definition "Wavelength: the distance" |> move (0, 90)
                    , definition "  between crests of a wave" |> move (0, 80)
                    , definition "Phase Shift: how far the" |> move (0, 60)
                    , definition "  function is shifted" |> move (0, 50)
                    , definition "  horizontally from the usual" |> move (0, 40)
                    , definition "  position." |> move (0, 30)
                    , definition "Period goes from one peak to" |> move (95, 160)
                    , definition "  the next (or from any point" |> move (95, 150)
                    , definition "  to the next matching point)." |> move (95, 140)
                    , definition "  The Period is 1" |> move (95, 130)
                    , definition "  divided by the Frequency." |> move (95, 120)
                    ,definition "The Frequency is how often" |> move (95, 100)
                    , definition "  something happens per unit" |> move (95 , 90)
                    , definition "  of time (per '1')." |> move (95, 80)
                    , definition "  Frequency is 1÷Period." |> move (95, 70)
                    , definition "  When the frequency is per" |> move (95, 60)
                    , definition "  second it's called 'Hertz'." |> move (95, 50)
                ]
        {-
           moveGraphicsY =
               group
                   [ rect 120 140 |> outlined (solid 1) red |> makeTransparent 0.25 |> move ( 10, -50 )
                   , square 15 |> filled (rgb model.r model.g model.b) |> move ( moveFun model.moveX model, moveFun model.moveY model ) |> move ( 10, -60 )
                   , text "|>" |> fixedwidth |> size 10 |> filled black |> move ( -40, 0 )
                   , text "move(" |> fixedwidth |> size 10 |> filled black |> move ( -25, 0 )
                   , text (moveText model.moveX) |> fixedwidth |> size 10 |> filled black |> notifyTap MoveX |> move ( 3, 0 ) |> notifyEnter (TransM (\m -> { m | moveTextX = 1 })) |> notifyLeave (TransM (\m -> { m | moveTextX = 0.25 })) |> makeTransparent model.moveTextX
                   , text "," |> fixedwidth |> size 10 |> filled black |> move ( 14, 0 )
                   , text (moveText model.moveY) |> fixedwidth |> size 10 |> filled black |> notifyTap MoveY |> move ( 21, 0 ) |> notifyEnter (TransM (\m -> { m | moveTextY = 1 })) |> notifyLeave (TransM (\m -> { m | moveTextY = 0.25 })) |> makeTransparent model.moveTextY
                   , text ")" |> fixedwidth |> size 10 |> filled black |> move ( 31, 0 )
                   ]

           moveGraphicsX =
               group
                   [ rect 120 140 |> outlined (solid 1) red |> makeTransparent 0.25 |> move ( 10, -220 )
                   , square 15 |> filled (rgb model.r model.g model.b) |> move ( moveFun model.moveX1 model, moveFun model.moveY1 model ) |> move ( 10, -230 )
                   , text "|>" |> fixedwidth |> size 10 |> filled black |> move ( -40, -170 )
                   , text "move(" |> fixedwidth |> size 10 |> filled black |> move ( -25, -170 )
                   , text (moveText model.moveX1) |> fixedwidth |> size 10 |> filled black |> notifyTap MoveX1 |> move ( 3, -170 ) |> notifyEnter (TransM (\m -> { m | moveTextX1 = 1 })) |> notifyLeave (TransM (\m -> { m | moveTextX1 = 0.25 })) |> makeTransparent model.moveTextX1
                   , text "," |> fixedwidth |> size 10 |> filled black |> move ( 14, -170 )
                   , text (moveText model.moveY1) |> fixedwidth |> size 10 |> filled black |> notifyTap MoveY1 |> move ( 21, -170 ) |> notifyEnter (TransM (\m -> { m | moveTextY1 = 1 })) |> notifyLeave (TransM (\m -> { m | moveTextY1 = 0.25 })) |> makeTransparent model.moveTextY1
                   , text ")" |> fixedwidth |> size 10 |> filled black |> move ( 31, -170 )
                   ]
        -}
        setofTriangles =
            group
                [ upArrow |> notifyTap UDilationPlus |> move ( -67, -5 ) |> notifyMouseDown (ButtonDown FrequencyUp) |> notifyMouseUp (ButtonDown None)
                , upArrow |> notifyTap UScalePlus |> move ( -111, -5 ) |> notifyMouseDown (ButtonDown AmplitudeUp) |> notifyMouseUp (ButtonDown None)
                , upArrow |> notifyTap UShiftPlus |> move ( 17, -5 ) |> notifyMouseDown (ButtonDown ShiftUp) |> notifyMouseUp (ButtonDown None)
                , downArrow |> notifyTap UDilationMinus |> move ( -67, -20 ) |> notifyMouseDown (ButtonDown FrequencyDown) |> notifyMouseUp (ButtonDown None)
                , downArrow |> notifyTap UScaleMinus |> move ( -111, -20 ) |> notifyMouseDown (ButtonDown AmplitudeDown) |> notifyMouseUp (ButtonDown None)
                , downArrow |> notifyTap UShiftMinus |> move ( 17, -20 ) |> notifyMouseDown (ButtonDown ShiftDown) |> notifyMouseUp (ButtonDown None)

                --, polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> makeTransparent 0.25 |> notifyTap VScalePlus |> move ( 60, -5 ) |> rotate (degrees 0) |> notifyMouseDown (ButtonDown VUP) |> notifyMouseUp (ButtonDown None)
                --, polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> makeTransparent 0.25 |> notifyTap VDilationPlus |> move ( 95, -5 )
                --, polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> makeTransparent 0.25 |> notifyTap VScaleMinus |> rotate (degrees 180) |> move ( 60, -20 ) |> notifyMouseDown (ButtonDown VDown) |> notifyMouseUp (ButtonDown None)
                --, polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled red |> makeTransparent 0.25 |> notifyTap VDilationMinus |> rotate (degrees 180) |> move ( 95, -20 )
                ]
        functionDefinitions = 
            group 
                [
                    definition "Frequency" |> move ( -67, -10 )
                    , definition "Amplitude" |> move ( -111, -5 )
                    , definition "Phase shift" |> move ( 17, -5 )
                ]
        rgbGraphics =
            group
                [ rect 140 50 |> outlined (solid 1) red |> makeTransparent 0.25 |> move ( 43, -5 )
                , text "rgb " |> fixedwidth |> size 10 |> filled black |> move ( -25, 0 )
                , text ("(" ++ String.fromFloat model.rScale ++ "*" ++ showFun model.rFun u v ++ ")") |> fixedwidth |> size 10 |> filled black |> move ( -5, 0 ) |> notifyTap R |> notifyEnter (TransM (\m -> { m | rTransp = 1 })) |> notifyLeave (TransM (\m -> { m | rTransp = 0.25 })) |> makeTransparent model.rTransp
                , text ("(" ++ String.fromFloat model.gScale ++ "*" ++ showFun model.gFun u v ++ ")") |> fixedwidth |> size 10 |> filled black |> move ( 35, 0 ) |> notifyTap G |> notifyEnter (TransM (\m -> { m | gTransp = 1 })) |> notifyLeave (TransM (\m -> { m | gTransp = 0.25 })) |> makeTransparent model.gTransp
                , text ("(" ++ String.fromFloat model.bScale ++ "*" ++ showFun model.bFun u v ++ ")") |> fixedwidth |> size 10 |> filled black |> move ( 73, 0 ) |> notifyTap B |> notifyEnter (TransM (\m -> { m | bTransp = 1 })) |> notifyLeave (TransM (\m -> { m | bTransp = 0.25 })) |> makeTransparent model.bTransp
                , triangle 8 |> filled (rgb 255 10 10) |> notifyTap RScalePlus |> move ( 5, -10 ) |> rotate (degrees 90) |> notifyMouseDown (ButtonDown RedUp) |> notifyMouseUp (ButtonDown None)
                , triangle 8 |> filled (rgb 180 140 140) |> notifyTap RScaleMinus |> rotate (degrees -90) |> move ( 15, -10 ) |> notifyMouseDown (ButtonDown RedDown) |> notifyMouseUp (ButtonDown None)
                , triangle 8 |> filled (rgb 10 255 10) |> notifyTap GScalePlus |> rotate (degrees 90) |> move ( 43, -10 ) |> notifyMouseDown (ButtonDown GreenUp) |> notifyMouseUp (ButtonDown None)
                , triangle 8 |> filled (rgb 140 180 140) |> notifyTap GScaleMinus |> rotate (degrees -90) |> move ( 53, -10 ) |> notifyMouseDown (ButtonDown GreenDown) |> notifyMouseUp (ButtonDown None)
                , triangle 8 |> filled (rgb 10 10 255) |> notifyTap BScalePlus |> rotate (degrees 90) |> move ( 80, -10 ) |> notifyMouseDown (ButtonDown BlueUp) |> notifyMouseUp (ButtonDown None)
                , triangle 8 |> filled (rgb 140 140 180) |> notifyTap BScaleMinus |> rotate (degrees -90) |> move ( 90, -10 ) |> notifyMouseDown (ButtonDown BlueDown) |> notifyMouseUp (ButtonDown None)
                ]

        -- Circle that rotates in time with the sin & cosin waves
        circleGraphics =
            group
                [ line ( -50, 50 ) ( -50 + model.uScale * notTrigCycleU uArg, 50 + u ) |> outlined (solid 1) (rgb model.r model.g model.b) |> makeTransparent 0.25
                , line ( -50 + model.uScale * notTrigCycleU uArg, 50 + u ) ( 0, 50 + model.uSinGraph ) |> outlined (solid 1) (rgb model.r model.g model.b) |> makeTransparent 0.5
                , line ( -50 + model.uScale * notTrigCycleU uArg, 50 + u ) ( model.uCosGraph - 50, 0 ) |> outlined (solid 1) (rgb model.r model.g model.b) |> makeTransparent 0.5
                , circle 2 |> filled (rgb model.r model.g model.b) |> move ( 0, 50 + model.uSinGraph )
                , circle 2 |> filled (rgb model.r model.g model.b) |> move ( model.uCosGraph - 50, 0 )
                , circle (abs uScale) |> outlined (solid 1) black |> move ( -50, 50 )
                , circle 2 |> filled (rgb model.r model.g model.b) |> move ( -50 + model.uScale * notTrigCycleU uArg, 50 + u )
                ]

        titlesText =
            group
                [ tt "1. Modify your functions!" |> move ( -50, 175 )
                , tt "2. Choose a colour!" |> move ( 140, 125 )
                , tt "3. Apply Transforms!" |> move ( 140, 35 )
                , tt "4. Move it!" |> move ( -220, 5 )
                , text "--The move function below will be in 'Your Code!' " |> serif |> italic |> size 6 |> filled titleColour |> move ( -250, -5 )
                , tt "5. Your Code!" |> move ( 40, -70 )
                ]

        cosLabel =
            text (String.fromFloat model.uScale ++ "* cos(" ++ cosinString model) |> fixedwidth |> size 8 |> filled black |> rotate (degrees 90) |> move ( -110, -82 ) |> notifyTap (TransM (\m -> { m | trigCycleU = Cos }))
    in
    [ graphPaperCustom 10 1 (rgb 255 137 5) |> makeTransparent 0.25 -- axes and selected coordinate ticks
    , group
        [ rect 1000 0.5 |> filled brown
        , rect 0.5 1000 |> filled brown
        , group (sinCurve model) |> move ( 0, 50 )
        , group (cosCurve model) |> move ( -50, 0 )
        , trigGraphAxis model |> move ( -185, 70 )
        , circleGraphics
        ]
        |> move ( -140, 60 )
    , titlesText |> makeTransparent 0
    , cosLabel |> move ( -127, 67 )
    , transformsGraphicsGroup |> move ( 105, -110 )
    , group 
        [
            rect 90 175 |> outlined (solid 2) green |> makeTransparent 0.25 |> move (-55, -10 )
            , rect 95 175 |> outlined (solid 2) green |> makeTransparent 0.25 |> move (38, -10 )

            , functionDefText |> move ( -100, -90 )
        ] |> move (-40,-20)
    , group
        [ functionText model |> move ( 5, 150 )
        , setofTriangles |> move ( 0, 165 )
        , functionDefinitions |> move ( 0, 185 )
        ]
        |> move ( -20, 0 )

    , yourCodeGroup |> move ( 40, 110 )
    ]


upArrow =
    polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, 12 ) ] |> filled (rgba 255 0 0 0.6)


downArrow =
    polygon [ ( -6, 0 ), ( 6, 0 ), ( 0, -12 ) ] |> filled (rgba 255 0 0 0.6)


trigGraphAxis model =
    group
        [ rect 0.5 105 |> filled black |> move ( 185, -18 )
        , rect model.sinWaveLength 0.5 |> filled black |> move ( 185 + model.sinWaveLength / 2, -20 )

        -- Subtract 130 to account for the ratio of the screen and remove excess
        , rect 105 0.5 |> filled black |> move ( 132, -70 )
        , rect 0.5 model.cosWaveLength |> filled black |> move ( 135, -70 - model.cosWaveLength / 2 )
        ]


functionText model =
    group
        [ text (showDigits 2 model.uScale ++ "*" ++ textTrig model.trigCycleU ++ "(" ++ cosinString model) |> fixedwidth |> size 10 |> filled black |> move ( -120, 0 )
        ]


showDigits width x =
    "      " ++ String.fromFloat x |> String.right width


titleColour =
    rgba 200 0 0 0.95

definition str = 
    str |> text |> size 5 |> fixedwidth |> filled darkGreen

copiable str =
    str |> text |> selectable |> fixedwidth |> size 6 |> filled black

copiable2 str =
    str |> text |> selectable |> fixedwidth |> size 9 |> filled black
