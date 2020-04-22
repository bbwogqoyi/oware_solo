module Oware

type StartingPosition =
    | South
    | North

/// No questions ... It works
let addSeed houseIndex board =
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

let emptyHouse houseIndex board =
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

let incrementIndex index =
  match index=12 with 
  | true -> 1
  | false -> index+1

let getSeeds houseIndex (_, board) : int=
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

let useHouse (houseIndex:int) gameState =
  let rec distributeSeeds index numOfSeeds board =
    match numOfSeeds>0 with
    | false -> board
    | true -> 
      let newBoard = addSeed index board
      distributeSeeds (incrementIndex index) (numOfSeeds-1) newBoard

  let (player, board) = gameState
  let numOfSeeds = getSeeds houseIndex gameState
  let updatedBoard = emptyHouse houseIndex board
  let startIndex = incrementIndex houseIndex
  let newBoard = distributeSeeds startIndex numOfSeeds updatedBoard

  match player with
  | South -> (North, newBoard)
  | North -> (South, newBoard)

let start (position:StartingPosition) = 
  let initialBoard = (4,4,4, 4,4,4, 4,4,4, 4,4,4)
  ( position, initialBoard )

let score board = failwith "Not implemented"

let gameState board = failwith "Not implemented"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code

//let board = gameState.board
//let numOfSeeds = board.[boardIndex]
//let numOfCycles, reminder = numOfSeeds/12, numOfSeeds%12