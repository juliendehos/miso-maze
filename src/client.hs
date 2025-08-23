
import Miso

import App.Component

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = run $ startComponent appComponent

