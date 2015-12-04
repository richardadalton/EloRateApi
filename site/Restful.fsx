namespace EloRateApi.Rest

#r "../packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
open Newtonsoft.Json
open Newtonsoft.Json.Serialization

#r "../packages/Suave/lib/net40/Suave.dll"
open Suave.Http.Successful
open Suave.Http.Applicatives
open Suave.Http.RequestErrors
open Suave.Http
open Suave.Web
open Suave.Types


[<AutoOpen>]
module Restful =

    let JSON v =
        let jsonSerializerSettings = new JsonSerializerSettings()
        jsonSerializerSettings.ContractResolver <- new CamelCasePropertyNamesContractResolver()

        JsonConvert.SerializeObject(v, jsonSerializerSettings) |> OK
        >>= Writers.setMimeType "application/json; charset=utf-8"



    let fromJson<'a> json = JsonConvert.DeserializeObject(json, typeof<'a>) :?> 'a



    let getResourceFromReq<'a> (req : HttpRequest) =
        let getString rawForm = System.Text.Encoding.UTF8.GetString(rawForm)
        req.rawForm |> getString |> fromJson<'a>



    let Get resourceName resource =
        let resourcePath = "/" + resourceName
        let get = warbler (fun _ -> resource () |> JSON)

        GET >>= path resourcePath >>= get



    let GetById resourceName resource =
        let resourcePath = "/" + resourceName
        let resourceIdPath =
            new PrintfFormat<(int -> string),unit,string,string,int>(resourcePath + "/%d")

        let handleResource requestError = function
            | Some r -> r |> JSON
            | _ -> requestError

        let getResourceById =
            resource >> handleResource (NOT_FOUND "Resource not found")

        GET >>= pathScan resourceIdPath getResourceById



    let Delete resourceName delete =
        let resourcePath = "/" + resourceName
        let resourceIdPath =
            new PrintfFormat<(int -> string),unit,string,string,int>(resourcePath + "/%d")

        let deleteResourceById id =
            delete id
            NO_CONTENT

        DELETE >>= pathScan resourceIdPath deleteResourceById



    let Post resourceName create =
        let resourcePath = "/" + resourceName
        POST >>= path resourcePath >>=
            request (getResourceFromReq >> create >> JSON)



    let Put resourceName update =
        let resourcePath = "/" + resourceName
        let resourceIdPath =
          new PrintfFormat<(int -> string),unit,string,string,int>(resourcePath + "/%d")

        let badRequest = BAD_REQUEST "Resource not found"

        let handleResource requestError = function
            | Some r -> r |> JSON
            | _ -> requestError

        PUT >>= path resourcePath >>= request (getResourceFromReq >> update >> handleResource badRequest)



    let PutById resourceName resource =
        let resourcePath = "/" + resourceName
        let resourceIdPath =
          new PrintfFormat<(int -> string),unit,string,string,int>(resourcePath + "/%d")

        let badRequest = BAD_REQUEST "Resource not found"

        let handleResource requestError = function
            | Some r -> r |> JSON
            | _ -> requestError

        let updateResourceById id =
          request (getResourceFromReq >> (resource id) >> handleResource badRequest)

        PUT >>= pathScan resourceIdPath updateResourceById
