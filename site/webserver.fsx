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

#load "Model.fsx"
open EloRateApi.Model

#load "Restful.fsx"
open EloRateApi.Rest



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
