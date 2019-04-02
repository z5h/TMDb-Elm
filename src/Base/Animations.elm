module Base.Animations exposing
    ( Animations
    , Msg
    , all
    , animate
    , animateAll
    , animated
    , animatedOneOf
    , done
    , filterMap
    , init
    , isAnimating
    , subscriptions
    , update
    )

import Base.ListSet as Set exposing (Set)
import Browser.Events
import ComponentResult exposing (ComponentResult)
import Dict exposing (Dict)
import Ease
import Task
import Time exposing (Posix)


type State
    = Created
    | Running


type alias Animation animatable =
    { item : animatable
    , start : Posix
    , durationMilliseconds : Int
    , state : State
    }


type alias Animations animatable =
    { animations : Set (Animation animatable)
    , time : Posix
    }


type Msg animatable
    = Tick Posix
    | AddAnimation (Animation animatable)


isRunningNow : Posix -> Animation any -> Bool
isRunningNow posix animation =
    let
        now =
            Time.posixToMillis posix

        start =
            Time.posixToMillis animation.start

        end =
            start + animation.durationMilliseconds
    in
    start <= now && now <= end


filterMap : (a -> Maybe b) -> Animations a -> Animations b
filterMap itemToMaybeItem model =
    let
        newAnimations =
            Set.foldl
                (\a bs ->
                    case itemToMaybeItem a.item of
                        Just transformedItem ->
                            Set.insert
                                { start = a.start
                                , durationMilliseconds = a.durationMilliseconds
                                , item = transformedItem
                                , state = a.state
                                }
                                bs

                        Nothing ->
                            bs
                )
                Set.empty
                model.animations
    in
    { time = model.time, animations = newAnimations }


all : Animations animatable -> List animatable
all model =
    model.animations
        |> Set.toList
        |> List.map .item


progress : Animation any -> Posix -> Float
progress animation posix =
    let
        now =
            Time.posixToMillis posix |> toFloat

        start =
            Time.posixToMillis animation.start |> toFloat

        end =
            start + toFloat animation.durationMilliseconds
    in
    if Time.posixToMillis animation.start == 0 then
        0.0

    else if now >= end then
        1.0

    else
        Ease.inOutCubic
            ((now - start) / toFloat animation.durationMilliseconds)


init : Animations animatable
init =
    { animations = Set.empty
    , time = Time.millisToPosix 0
    }


add :
    Animation animatable
    -> Set (Animation animatable)
    -> Set (Animation animatable)
add animation set =
    set
        |> Set.filter (\a -> a.item /= animation.item)
        |> Set.insert animation


update :
    Msg animatable
    -> Animations animatable
    -> ComponentResult (Animations animatable) (Msg animatable) externalMsg err
update msg model =
    case msg of
        Tick posix ->
            ComponentResult.withModel
                { model
                    | time = posix
                    , animations =
                        model.animations |> Set.filter (isRunningNow posix)
                }

        AddAnimation animation ->
            ComponentResult.withModel
                { model
                    | time = animation.start
                    , animations = model.animations |> add animation
                }


animate :
    animatable
    -> Int
    -> Animations animatable
    -> ComponentResult (Animations animatable) (Msg animatable) externalMsg err
animate item duration model =
    ComponentResult.withModel
        { model
            | animations =
                model.animations
                    |> add
                        { item = item
                        , start = Time.millisToPosix 0
                        , durationMilliseconds = duration
                        , state = Created
                        }
        }
        |> ComponentResult.withCmd
            (Time.now
                |> Task.perform
                    (\time ->
                        AddAnimation
                            { item = item
                            , start = time
                            , durationMilliseconds = duration
                            , state = Running
                            }
                    )
            )


animateAll :
    List animatable
    -> Int
    -> Animations animatable
    -> ComponentResult (Animations animatable) (Msg animatable) externalMsg err
animateAll animatables duration model =
    let
        updatedModel =
            List.foldl
                (\animation model_ ->
                    { model
                        | animations =
                            model_.animations
                                |> add
                                    { item = animation
                                    , start = Time.millisToPosix 0
                                    , durationMilliseconds = duration
                                    , state = Created
                                    }
                    }
                )
                model
                animatables

        cmds =
            animatables
                |> List.map
                    (\animatable ->
                        Time.now
                            |> Task.perform
                                (\time ->
                                    AddAnimation
                                        { item = animatable
                                        , start = time
                                        , durationMilliseconds = duration
                                        , state = Running
                                        }
                                )
                    )
    in
    ComponentResult.withModel updatedModel
        |> ComponentResult.withCmds cmds


done : (Float -> a) -> a
done f =
    f 1.0


animated : Animations animatable -> animatable -> (Float -> a) -> Maybe a
animated model item f =
    Set.filter (\a -> a.item == item) model.animations
        |> Set.sample
        |> Maybe.map
            (\animation ->
                f <| progress animation model.time
            )


animatedOneOf :
    Animations animatable
    -> List ( animatable, Float -> a )
    -> Maybe a
animatedOneOf model animatablesAndF =
    let
        allAnimations =
            all model
    in
    animatablesAndF
        |> List.filter
            (\( animatable, f ) ->
                List.member animatable allAnimations
            )
        |> List.head
        |> Maybe.andThen (\( animatable, f ) -> animated model animatable f)


isAnimating : Animations animatable -> animatable -> Bool
isAnimating model animatable =
    Set.filter (\a -> a.item == animatable) model.animations
        |> (not << Set.isEmpty)


subscriptions : Animations animatable -> Sub (Msg animatable)
subscriptions model =
    if Set.isEmpty model.animations then
        Browser.Events.onAnimationFrame Tick
        -- Sub.none -- bug

    else
        Browser.Events.onAnimationFrame Tick
