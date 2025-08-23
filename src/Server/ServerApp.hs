{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Server.ServerApp (serverApp) where

import Miso 
import Miso.Html.Element as H
import Miso.Html.Property as P
import Network.HTTP.Types
import Network.Wai (responseLBS)
import Network.Wai.Application.Static (defaultWebAppSettings)
import Servant

import App.Component 
import App.Model
import Network.Api

-------------------------------------------------------------------------------
-- server routing
-------------------------------------------------------------------------------

type ClientRoutes = Get '[HTML] Page

type ServerApi
  =    PublicApi
  :<|> ClientRoutes
  :<|> Raw

newtype Page = Page AppComponent

handlePublicApi :: Server PublicApi
handlePublicApi 
  =    serveDirectoryWith (defaultWebAppSettings "public")

handleClientRoutes :: Server ClientRoutes
handleClientRoutes 
  = pure (Page $ appComponent " ")

handle404 :: Application
handle404 _ respond' =
  respond' $
    responseLBS status404 [("Content-Type", "text/html")] $
      toHtml [ p_ [] [ "page not found" ] ]

-------------------------------------------------------------------------------
-- server rendering
-------------------------------------------------------------------------------

instance ToHtml Page where
  toHtml (Page x) =
    toHtml
      [ doctype_
      , html_
        [ lang_ "en" ]
        [ head_ 
          [ P.title_ "miso-maze" ]
          [ meta_ [ charset_ "utf-8" ]
          , meta_ [ name_ "viewport" , content_ "width=device-width, initial-scale=1" ]
          , link_
            [ rel_ "icon"
            , href_ (mkStaticUri "favicon.ico")
            , type_ "image/x-icon"
            ]
          , script_ [ src_ (mkStaticUri "index.js"), type_ "module" ] ""
          , body_ [] [ toView @App.Model.Model x] ]
        ]
      ]

-------------------------------------------------------------------------------
-- server app
-------------------------------------------------------------------------------

serverApp :: Application
serverApp = serve (Proxy @ServerApi) serverHandlers
  where
    serverHandlers 
      =    handlePublicApi
      :<|> handleClientRoutes
      :<|> Tagged handle404

