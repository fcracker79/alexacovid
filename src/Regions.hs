module Regions(ItalianRegion(..), regionByCap) where


data ItalianRegion = 
    Abruzzo | Basilicata | Calabria | Campania | Emilia | Friuli | Lazio | 
    Liguria | Lombardia | Marche | Molise | Piemonte| Puglia | Sardegna | 
    Sicilia | Umbria | ValleDAosta | Toscana | Trentino | ValDAosta | Veneto 
    deriving(Show, Eq, Ord, Read)

regionsByCaps :: [(Int, Int, ItalianRegion)]
regionsByCaps = [
        (00010, 2011,  Lazio),
        (05010, 06135, Umbria),
        (07010, 09170, Sardegna),
        (10010, 28925, Piemonte),
        (11010, 11100, ValDAosta),
        (12071, 19137, Liguria),
        (16192, 46100, Lombardia),
        (29010, 48100, Emilia),
        (30010, 45100, Veneto),
        (33010, 34170, Friuli),
        (38010, 39100, Trentino),
        (50010, 59100, Toscana),
        (60010, 63900, Marche),
        (64010, 67100, Abruzzo),
        (70010, 76125, Puglia),
        (75010, 85100, Basilicata),
        (80010, 84135, Campania),
        (86010, 86170, Molise),
        (87010, 89900, Calabria),
        (90010, 98168, Sicilia)
    ]


recRegionByCap :: [(Int, Int, ItalianRegion)] -> Int -> Maybe ItalianRegion
recRegionByCap [] _ = Nothing
recRegionByCap ((from, to, region):vs) c = if c >= from && c <= to then Just region else recRegionByCap vs c 

regionByCap :: Int -> Maybe ItalianRegion
regionByCap = recRegionByCap regionsByCaps
