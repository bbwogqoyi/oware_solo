module Oware
open System

type PlayerOption =
  | South
  | North

type State =
  | InProgress
  | SouthWon
  | NorthWon
  | Draw

type GameState = {
  player:PlayerOption
  board:(int*int*int*int*int*int*int*int*int*int*int*int)
  score:(int*int)
  index:int
  state:State
}

type IndexOperation =
  | Decrement
  | Increment

/// No questions ... It works
let _addSeed houseIndex board =
  match houseIndex, board with
  | 01, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a+1,b,c,d,e,f,a',b',c',d',e',f')
  | 02, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b+1,c,d,e,f,a',b',c',d',e',f') 
  | 03, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c+1,d,e,f,a',b',c',d',e',f') 
  | 04, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d+1,e,f,a',b',c',d',e',f')
  | 05, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e+1,f,a',b',c',d',e',f')
  | 06, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f+1,a',b',c',d',e',f')
  | 07, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a'+1,b',c',d',e',f')
  | 08, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a',b'+1,c',d',e',f')
  | 09, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a',b',c'+1,d',e',f')
  | 10, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a',b',c',d'+1,e',f')
  | 11, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a',b',c',d',e'+1,f')
  | 12, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a',b',c',d',e',f'+1)
  | _ -> failwith "index is out-of-bound"

let _emptyHouse houseIndex board =
  match houseIndex, board with
  | 01, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (0,b,c,d,e,f,a',b',c',d',e',f')
  | 02, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,0,c,d,e,f,a',b',c',d',e',f') 
  | 03, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,0,d,e,f,a',b',c',d',e',f') 
  | 04, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,0,e,f,a',b',c',d',e',f')
  | 05, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,0,f,a',b',c',d',e',f')
  | 06, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,0,a',b',c',d',e',f')
  | 07, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,0,b',c',d',e',f')
  | 08, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a',0,c',d',e',f')
  | 09, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a',b',0,d',e',f')
  | 10, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a',b',c',0,e',f')
  | 11, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a',b',c',d',0,f')
  | 12, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a',b',c',d',e',0)
  | _ -> failwith "index is out-of-bound"

let _updateIndex (op:IndexOperation) index =
  let value = 
    match op with
    | Increment -> index+1
    | Decrement -> index-1
  match value>12, value<1 with 
  | true, _ -> 1
  | _, true -> 12
  | _,_ -> value

let _isDraw (s, n) = s=n && s=24   

let _switchPlayer gameState =
  match gameState.player, gameState.state=InProgress with
  | South, true -> { gameState with player=North }
  | North, true -> { gameState with player=South }
  | _ -> gameState

let _updateGameState gameState =
  let score = 
    match gameState.player, gameState.score with
    | South, (s, _) -> s
    | North, (_, n) -> n

  match (gameState.player, (score>=25), (_isDraw gameState.score)) with
  | South, true, _ -> { gameState with state=SouthWon }
  | North, true, _ -> { gameState with state=NorthWon }
  | _, false, true -> { gameState with state=Draw }
  | _ -> gameState

let _isHouseOwnedByPLayer (player:PlayerOption) (houseIndex:int) =
  match player with
  | South -> houseIndex<7
  | North -> houseIndex>=7

let getSeeds houseIndex {board=board} : int=
  match houseIndex, board with
  | 01, (a,_,_,_,_,_,_,_,_,_,_,_)  -> a
  | 02, (_,b,_,_,_,_,_,_,_,_,_,_)  -> b
  | 03, (_,_,c,_,_,_,_,_,_,_,_,_)  -> c
  | 04, (_,_,_,d,_,_,_,_,_,_,_,_)  -> d
  | 05, (_,_,_,_,e,_,_,_,_,_,_,_)  -> e
  | 06, (_,_,_,_,_,f,_,_,_,_,_,_)  -> f
  | 07, (_,_,_,_,_,_,a',_,_,_,_,_)  -> a'
  | 08, (_,_,_,_,_,_,_,b',_,_,_,_)  -> b'
  | 09, (_,_,_,_,_,_,_,_,c',_,_,_)  -> c'
  | 10, (_,_,_,_,_,_,_,_,_,d',_,_)  -> d'
  | 11, (_,_,_,_,_,_,_,_,_,_,e',_)  -> e'
  | 12, (_,_,_,_,_,_,_,_,_,_,_,f')  -> f'
  | _ -> failwith "index is out-of-bound"

let _getPlayerHouseList (player:PlayerOption) = 
  match player with
  | South -> List.init 6 (fun x -> x+7) 
  | North -> List.init 6 (fun x -> x+1)

let _isValidMove gameState =
  let rec checkOnOpponent count houses = 
    match houses with
    | [] -> count
    | entry::rest -> 
      let numOfSeeds = getSeeds entry gameState
      checkOnOpponent (count+numOfSeeds) rest

  let opponentHouseList = _getPlayerHouseList gameState.player
  
  let canOpponentPlay = (checkOnOpponent 0 opponentHouseList) > 0
  let isDraw = _isDraw gameState.score
  canOpponentPlay || isDraw

let _newScore previousGameState gameState =
  let rec helper index board score =
    let isPlayers = (_isHouseOwnedByPLayer gameState.player index)
    match isPlayers with
    | true ->  { gameState with score=score; board=board}
    | false ->
      let numOfSeeds = getSeeds index gameState
      match numOfSeeds with
      | 2 | 3 ->
        let updatedBoard = (_emptyHouse index board)
        let updatedScore =
          match gameState.player, score with
          | South, (s, n) -> (s+numOfSeeds, n)
          | North, (s, n) -> (s, n+numOfSeeds)
        
        let (newScore, newBoard) =
          match ( _isValidMove { gameState with board=updatedBoard; score=updatedScore} ) with
          | false -> (score, board)
          | true -> 
            let newScore = 
              match gameState.player, score with
              | South, (s, n) -> (s+numOfSeeds, n)
              | North, (s, n) -> (s, n+numOfSeeds)
            (newScore, updatedBoard)

        helper (_updateIndex Decrement index) newBoard newScore
      | _ ->
        { gameState with score=score; board=board}

  let finalState = helper gameState.index gameState.board gameState.score 
  match _isValidMove finalState with
  | false -> previousGameState
  | true -> finalState

let _executePlay gameState houseIndex numOfSeeds =
  let rec distributeSeeds index numOfSeeds board =
     match numOfSeeds>0, index=houseIndex with
     | false,_ -> ((_updateIndex Decrement index), board)
     | true, true ->
        distributeSeeds (_updateIndex Increment index) (numOfSeeds) board
     | true, false -> 
       let newBoard = _addSeed index board
       distributeSeeds (_updateIndex Increment index) (numOfSeeds-1) newBoard

  let updatedBoard = _emptyHouse houseIndex gameState.board
  let startIndex = _updateIndex Increment houseIndex 
  let (lastHouse, newBoard) = distributeSeeds startIndex numOfSeeds updatedBoard

  match ( _isValidMove { gameState with board=newBoard } ) with
  | false -> gameState
  | true ->
    _newScore gameState { gameState with board=newBoard ; index=lastHouse }
    |> _updateGameState
    |> _switchPlayer 

let useHouse (houseIndex:int) gameState =
  let numOfSeeds = getSeeds houseIndex gameState
  let isPlayerHouse = _isHouseOwnedByPLayer gameState.player houseIndex
  let contiuneGame = gameState.state = InProgress
  match numOfSeeds>0 && isPlayerHouse && contiuneGame with 
  | false -> gameState
  | true -> _executePlay gameState houseIndex numOfSeeds

let start (position:PlayerOption) = 
  { player=position; score=(0,0); board=(4,4,4, 4,4,4, 4,4,4, 4,4,4); index=0; state=InProgress }

let score gameState = gameState.score

let gameState gameState = 
  match gameState.state with
  | Draw -> "Game ended in a draw"
  | SouthWon -> "South won"
  | NorthWon -> "North won"
  | InProgress ->
    match gameState.player with
    | South -> "South's turn"
    | North -> "North's turn"

let playGame numbers =
  let rec play xs game =
      match xs with
      | [] -> game
      | x::xs -> play xs (useHouse x game)
  play numbers (start South)

let __getUserInput () =
  let rec getConsoleInput () = 
    let retry () = printfn "Invalid selection, try again" |> getConsoleInput
    let input = System.Console.ReadLine()
    match (not (String.IsNullOrWhiteSpace input)) && (String.forall System.Char.IsDigit input)  with
    | false -> retry ()
    | true -> 
      let selectedHouse = int input
      match selectedHouse>=1 && selectedHouse<=12 with
      | false -> retry ()
      | true -> selectedHouse
  getConsoleInput ()

let __displayGameBoard gameState = 
  let _south = List.map (fun house -> getSeeds house gameState) (_getPlayerHouseList South)
  let _north = List.map (fun house -> getSeeds house gameState) (_getPlayerHouseList North)

  printfn "%A" _south
  // let north = String.concat "{0} {1} {2} {3} {4} {5}" getSeeds 12 gameState

//let playGameInteractive () =
//  let rec play game =
//      match xs with
//      | [] -> game
//      | x::xs -> play xs (useHouse x game)
//  play (start South)


[<EntryPoint>]
let main _ =
  let game = (start South)
  __displayGameBoard game


  //printfn "%s" (gameState game)
  //__getUserInput () |> ignore
  0 // return an integer exit code