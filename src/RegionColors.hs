module RegionColors (getRegionColors) where

import Text.HTML.Scalpel ( scrapeURL, attr, chroots, anySelector )
import Control.Monad.Trans.Maybe(MaybeT(..))
import Control.Monad.IO.Class(liftIO)
import Control.Applicative(empty)
import Control.Monad (guard)
import Data.Map.Strict(fromList, Map)
import Data.Map (compose)


transcodingMap :: Map String String
transcodingMap = fromList [
    ("Abruzzo","Abruzzo"),
    ("Basilicata","Basilicata"),
    ("Calabria","Calabria"),
    ("Campania","Campania"),
    ("Emilia-Romagna","Emilia-Romagna"),
    ("Friuli-Venezia Giulia","Friuli-Venezia Giulia"),
    ("Lazio","Lazio"),
    ("Liguria","Liguria"),
    ("Lombardy","Lombardia"),
    ("Marche","Marche"),
    ("Molise","Molise"),
    ("Piemonte","Piemonte"),
    ("Puglia","Puglia"),
    ("Sardinia","Sardegna"),
    ("Sicily","Sicilia"),
    ("Tuscany","Toscana"),
    ("Trentino-South Tyrol", "Trentino Alto Adige (P.A. Bolzano)"),
--    ("Trentino Alto Adige (P.A. Trento)","giallo"),
    ("Umbria","Umbria"),
    ("Aosta", "Valle d'Aosta"),
    ("Aosta Valley", "Valle d'Aosta"),
    ("Veneto","Veneto")]

getRegionColorsList :: MaybeT IO [(String, String)]
getRegionColorsList = MaybeT $ do
                               scrapeURL "https://covidzone.info" scraper
                               where scraper = chroots "path" $ do
                                     d <- attr "d" anySelector
                                     region  <- attr "title" anySelector
                                     guard (region /= "")
                                     color <- attr "color" anySelector
                                     guard (color /= "")
                                     return (region, color)

getRegionColors :: MaybeT IO (Map String String)
getRegionColors = flip compose transcodingMap . fromList <$> getRegionColorsList