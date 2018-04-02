port module Update exposing (update)

import Array exposing (Array)
import Maybe exposing (..)
import Model exposing (..)
import Messages exposing (..)
import Time exposing (Time)
import Random exposing (Generator(..))
import Set

pairGenerator: Generator Coordinate
pairGenerator  =
      Random.pair (Random.int 0 (playFieldMatrixSize.columns-1)) (Random.int 0 (playFieldMatrixSize.rows-1))
      |> Random.map (\(col,row)->{x= col, y=row})

randomizedMonster:Square->Generator Square
randomizedMonster monster=
    let
        coord = getCoordinate monster
    in
      Random.int 0 3
      |> Random.map (\v->
          case v of
              0->Monster coord Horizontal 1
              1->Monster coord Vertical 1
              2->Monster coord Horizontal -1
              3->Monster coord Vertical -1
              _->Wall coord --just to make compiler happy
      )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            (  model
             , Random.generate RandomizedField (Random.list 40 pairGenerator))
        Tick time ->
             animate (min time 25) model
        PlantBomb->
            (plantBomb model
            , Cmd.none)
        PlayerStartedMoving direction speed->
            ({model | player= Monster (getCoordinate model.player) direction speed, playerMoving=True}
            , Cmd.none)
        PlayerStopped->
            ({model| playerMoving=False}
            , Cmd.none)
        RandomizedField coords->
            (startGame model coords
            , Cmd.none)
        RandomizedMonster old new ->
            ( model
                |>replaceSquare old new
            , Cmd.none )
        Noop ->
            ( model
            , Cmd.none )

removeDuplicates: List a->List a->List a
removeDuplicates seen remaining =
    case remaining of
       []->seen
       h::tl->if (List.member h seen)
          then removeDuplicates seen tl
          else removeDuplicates (h::seen) tl


startGame: Model->List Coordinate->Model
startGame model randomCoords=
    let
        slots = randomCoords
                    |>removeDuplicates []
                    |>List.filter (\p->rem (p.x+1) 2 /= 0
                                || rem p.y 2 /=0)
                    |>List.filter (\p->p.x>2||p.y>2)--reseved for player

        monsterCoords = List.take 3 slots
        wallCoords = List.drop 3 slots
        pillars = Array.initialize (playFieldMatrixSize.columns//2) (\c->
                             Array.initialize (playFieldMatrixSize.rows//2) (\r->
                                     Pillar {x= 2*c+1, y=2* r}))
                       |>Array.toList
                       |>List.concatMap Array.toList

        monsters = List.map (\c->Monster c Vertical 1) monsterCoords
        walls = List.map (\c->Wall c) wallCoords
    in
      {model| cells = monsters
        |> List.append walls
        |> List.append pillars
        , state = Playing}

animate : Time -> Model -> (Model, Cmd Msg)
animate elapsed model =
    model
        |> checkMonsterEatenPlayer
        |> advancePlayer
        |> checkMonsterEatenPlayer
        |> advanceBombs
        |> advanceMonsters

checkMonsterEatenPlayer : Model -> Model
checkMonsterEatenPlayer model =
        let
                playerCoord = getCoordinate model.player
                maybeMonster = findAt playerCoord model
        in
            case (maybeMonster) of
            Just _-> {model|state=Stopped}
            Nothing -> model

wallsAndBombs: Square->Maybe Square
wallsAndBombs square =
   case square of
       Wall _ -> Just square
       Pillar _ -> Just square
       Bomb _ _ -> Just square
       _ -> Nothing

blowUpable: Square->Maybe Square
blowUpable square =
   case square of
       Wall _ -> Just square
       Monster _ _ _ -> Just square
       _ -> Nothing

getCoordinate: Square->Coordinate
getCoordinate square =
    case square of
        Wall coordinate->coordinate
        Pillar coordinate->coordinate
        Monster coordinate _ _ ->coordinate
        Bomb coordinate _ ->coordinate

overlaps: Coordinate->List Square->Bool
overlaps  coordinate squares =
    List.map getCoordinate squares
    |> List.any (\v->v==coordinate)

maybeAdvance : Square->Model->Maybe Coordinate
maybeAdvance square model =
    let
        nextPos = case square of
                        Monster location direction speed -> Just {
                                x = location.x+(if direction==Horizontal then speed else 0),
                                y = location.y+(if direction==Vertical then speed else 0)}
                        _ -> Nothing
        maybeMove nextCoord  = model.cells
                                      |>List.filterMap wallsAndBombs
                                      |>overlaps nextCoord
                                      |>\b->if b== False then Just nextCoord else Nothing
        insidePlayingField coord = if
                            coord.x>=0
                            && coord.x < playFieldMatrixSize.columns
                            && coord.y>=0
                            && coord.y < playFieldMatrixSize.rows
                            then
                                Just coord
                            else
                                Nothing
    in
        nextPos
          |> Maybe.andThen insidePlayingField
          |> Maybe.andThen maybeMove


advanceOrRandomize: Square->Model->(Square, Cmd Msg)
advanceOrRandomize monster model=
    case monster of
        Monster coord direction speed-> case (maybeAdvance monster model) of
                                            Just newCoord->(Monster newCoord direction speed, Cmd.none)
                                            Nothing->(monster, Random.generate (\newMonster->
                                                    RandomizedMonster monster newMonster
                                                    ) (randomizedMonster monster))
        other->(other, Cmd.none)

advanceMonsters: Model->(Model, Cmd Msg)
advanceMonsters model =
    let
        cellsAndEffects = model.cells
           |> List.map (\v->advanceOrRandomize v model)
           |> List.unzip

    in
       ({model|cells = Tuple.first(cellsAndEffects)},
        Cmd.batch (Tuple.second cellsAndEffects))

replaceSquare: Square->Square->Model->Model
replaceSquare old new model=
    let
        newSquares = model.cells
                     |> List.map (\v->if v == old then new else v)
    in
        {model| cells=newSquares}

removeSquare: Square->Model->Model
removeSquare old model=
    let
        newSquares = model.cells
                     |> List.filterMap (\v->if v == old then Nothing else Just v)
    in
        {model| cells=newSquares}


findAt: Coordinate->Model->Maybe Square
findAt coordinate model =
        model.cells
        |> List.filterMap blowUpable
        |> List.filter (\v-> getCoordinate v == coordinate)
        |> List.head

blowUpCell: Maybe Square->Model->Model
blowUpCell square model =
       case square of
        Just square -> removeSquare square model
        Nothing->model

knockOffEverything: Coordinate->Model->Model
knockOffEverything coordinate model=
    let
        cell = findAt coordinate model
    in
        if (getCoordinate model.player)==coordinate
        then {model|state=Stopped}
        else blowUpCell cell model

blowUp: Coordinate->Model->Model
blowUp bombCoordinate model =
       model
    |> knockOffEverything {bombCoordinate|x=bombCoordinate.x +1}
    |> knockOffEverything {bombCoordinate|x=bombCoordinate.x - 1}
    |> knockOffEverything {bombCoordinate|y=bombCoordinate.y +1}
    |> knockOffEverything {bombCoordinate|y=bombCoordinate.y - 1}
    |> knockOffEverything bombCoordinate

bombSaysTick: Square->Model->Model
bombSaysTick square model=
    case square of
        Bomb coord timeToLive->if timeToLive <=0 then
                blowUp coord model
                |> removeSquare square
            else
                model
                |> replaceSquare square (Bomb coord (timeToLive - 1))
        _->model


advanceBombList: List Square->Model->Model
advanceBombList bombs model =
    case bombs of
        []-> model
        h::tl-> advanceBombList tl (bombSaysTick h model)

advanceBombs: Model->Model
advanceBombs model =
    advanceBombList model.cells model

copyMonsterWithCoordinates: Square->Coordinate->Square
copyMonsterWithCoordinates src coords=
    case src of
        Monster coordinate direction speed -> Monster coords direction speed
        other ->other

plantBombAt: Coordinate->Model->Model
plantBombAt coordinate model=
    {model|cells=(Bomb coordinate 30)::model.cells}

plantBomb: Model->Model
plantBomb model=
        case model.player of
            Monster coord _ _ -> plantBombAt coord model
            _-> model


advancePlayer: Model->Model
advancePlayer model=
    let
        nextPlayerLocation = maybeAdvance model.player model
    in
     if model.playerMoving
     then case nextPlayerLocation of
        Just coord -> {model|player=copyMonsterWithCoordinates model.player coord}
        Nothing -> model
     else
        model