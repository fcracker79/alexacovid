module RegionColors where

import Text.HTML.Scalpel ( scrapeURL, attr, chroots, anySelector )
import Control.Monad.Trans.Maybe(MaybeT)
import Control.Monad.IO.Class(liftIO)
import Control.Applicative(empty)

getRegionColors :: MaybeT IO [(String, String)]
getRegionColors = do
    r <- liftIO $ scrapeURL "https://covidzone.info" scraper
    case r of
        Nothing -> return empty
        Just x -> return x
    where scraper = chroots "path" $ do
                                     d <- attr "d" anySelector
                                     region  <- attr "title" anySelector
                                     color <- attr "color" anySelector
                                     return (region, color)