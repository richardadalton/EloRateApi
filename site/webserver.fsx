#I "../packages/FAKE/tools/"
#I "../paket-files/richardadalton"

#r "../packages/FAKE/tools/FakeLib.dll"
open Fake

#r "../packages/Suave/lib/net40/Suave.dll"
open Suave.Http.Successful
open Suave.Http.Applicatives
open Suave.Http.RequestErrors
open Suave.Http
open Suave.Web
open Suave.Types


#load "cqagent/src/CQAgent.fsx"
open CQAgent

#load "Restful.fsx"
open EloRateApi.Rest

type PlayerResource = {
  Id : int
  Name : string
  Points : int
  Retired : bool
}

type GameResource = {
  Id: int
  Winner : string
  Loser : string
}


type State = {
               Players: PlayerResource list;
               NextPlayerId: int;
               Games: GameResource list;
               NextGameId: int
             }

module Model =

    let initialState = { Players = []; NextPlayerId = 1; Games = []; NextGameId = 1 }



[<AutoOpen>]
module Player =
    let getPlayerById id = Seq.tryFind (fun (p: PlayerResource) -> p.Id = id)

    let removePlayer id = List.filter (fun (p: PlayerResource) -> p.Id <> id)



    let public GetAll (model: CQAgent<State>) () =
        model.Query (fun (s: State) -> s.Players)

    let public GetItem (model: CQAgent<State>) id =
        model.Query (fun (s: State) -> getPlayerById id s.Players)

    let public Create (model: CQAgent<State>) (player: PlayerResource) =
        model.Command (fun (s: State) ->
            let newPlayer = {player with Id = s.NextPlayerId}
            (newPlayer, {s with Players = newPlayer::s.Players;
                                NextPlayerId = s.NextPlayerId + 1}))

    let public DeleteItem (model: CQAgent<State>) id =
        model.Command (fun (s: State) ->
            ((), { s with Players = removePlayer id s.Players }))

    let public Update (model: CQAgent<State>) (updatedPlayer: PlayerResource) =
        model.Command (fun (s: State) ->
            let existing = getPlayerById updatedPlayer.Id s.Players
            match existing with
            | Some e -> (Some updatedPlayer, {s with Players = updatedPlayer::removePlayer updatedPlayer.Id s.Players })
            | None -> (None, s))

    let public UpdateById (model: CQAgent<State>) id (player: PlayerResource) =
        model.Command (fun (s: State) ->
            let existing = getPlayerById id s.Players
            match existing with
            | Some e ->
                    let newPlayer = {player with Id = e.Id}
                    (Some newPlayer, { s with Players = newPlayer::removePlayer e.Id s.Players })
            | None -> (None, s))





[<AutoOpen>]
module Game =

    let getGameById id = Seq.tryFind (fun (g: GameResource) -> g.Id = id)

    let removeGame id = List.filter (fun (g: GameResource) -> g.Id <> id)



    let public GetAll (model: CQAgent<State>) () =
        model.Query (fun (s: State) -> s.Games)

    let public GetItem (model: CQAgent<State>) id =
        model.Query (fun (s: State) -> getGameById id s.Games)

    let public Create (model: CQAgent<State>) (game: GameResource) =
        model.Command (fun (s: State) ->
            let newGame = {game with Id = s.NextGameId}
            (newGame, {s with Games = newGame::s.Games;
                              NextGameId = s.NextGameId + 1}))

    let public DeleteItem (model: CQAgent<State>) id =
        model.Command (fun (s: State) ->
            ((), { s with Games = removeGame id s.Games }))





let model = new CQAgent<State>(Model.initialState)

Player.Create model { Id = 0; Name="Sandra"; Points=1100; Retired=true }    |> ignore
Player.Create model { Id = 0; Name="Richard"; Points=1100; Retired=false }  |> ignore
Player.Create model { Id = 0; Name="Tom"; Points=1000; Retired=false }      |> ignore
Player.Create model { Id = 0; Name="Mary"; Points=1000; Retired=false }     |> ignore
Player.Create model { Id = 0; Name="Harry"; Points=1000; Retired=false }    |> ignore
Player.Create model { Id = 0; Name="Jane"; Points=1000; Retired=false }     |> ignore

Game.Create model { Id = 0; Winner="Sandra"; Loser="Richard" }              |> ignore
Game.Create model { Id = 0; Winner="Tom"; Loser="Jane" }                    |> ignore
Game.Create model { Id = 0; Winner="Mary"; Loser="Harry" }                  |> ignore
Game.Create model { Id = 0; Winner="Richard"; Loser="Harry" }               |> ignore



let PlayerRoutes = choose [
                        Get "players" (Player.GetAll model)
                        GetById "players" (Player.GetItem model)
                        Post "players" (Player.Create model)
                        Delete "players" (Player.DeleteItem model)
                        Put "players" (Player.Update model)
                        PutById "players" (Player.UpdateById model)
                        ]

let GameRoutes = choose [
                        Get "games" (Player.GetAll model)
                        GetById "games" (Player.GetItem model)
                        Post "games" (Player.Create model)
                        Delete "games" (Player.DeleteItem model)
                        ]


let routes = choose [
              PlayerRoutes
              GameRoutes
              (NOT_FOUND "Huh?")
            ]

let serverConfig =
    let port = getBuildParamOrDefault "port" "8083" |> Suave.Sockets.Port.Parse
    { defaultConfig with bindings = [ HttpBinding.mk HTTP System.Net.IPAddress.Loopback port ] }

startWebServer serverConfig routes
