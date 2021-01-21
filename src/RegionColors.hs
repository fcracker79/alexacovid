module RegionColors where

import Text.HTML.Scalpel ( scrapeURL, attr, chroots, anySelector )


getRegionColors :: IO (Maybe [(String, String)])
getRegionColors = scrapeURL "https://covidzone.info" scraper
    where scraper = chroots "path" $ do
                                     d <- attr "d" anySelector
                                     region  <- attr "title" anySelector
                                     color <- attr "color" anySelector
                                     return (region, color)