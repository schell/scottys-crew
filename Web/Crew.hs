module Web.Crew (
    startScotty,
    module C
) where

import Web.Crew.Templates as C
import Web.Crew.Session as C
import Web.Crew.User as C
--import Data.Configurator
import Control.Exception
import Web.Scotty
import Data.Acid
import Data.Acid.Local

startScotty :: Int -> (AcidState Users -> ScottyM ()) -> IO ()
startScotty port f = do
    --(cfg, _) <- autoReload autoConfig [Required "btci.config"]
    --users <- lookupDefault "initialUsers" cfg []
    bracket (openLocalState initialUsers)
            createCheckpointAndClose
            (\acid -> scotty port $ do
                f acid
                userRoutes acid)
