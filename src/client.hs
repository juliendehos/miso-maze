{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Miso
import Network.URI

import App.Component (appComponent)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run (miso (appComponent . uri2url))

uri2url :: URI -> MisoString
uri2url uri =
  case uriAuthority uri of
    Nothing -> ""
    Just URIAuth{..} -> ms (uriRegName <> uriPort)

