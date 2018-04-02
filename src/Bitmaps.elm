module Bitmaps exposing (..)
import Array
import Color exposing (Color)
import Collage

bitmap16Size: Float
bitmap16Size =
    pixelSize * 16

pixelSize: Float
pixelSize = 3

toColor: Char->Maybe Color
toColor char=
    case char of
        'R' -> Just Color.red
        'X' -> Just Color.black
        '0' -> Just Color.white
        'I' -> Nothing-- Color.gray
        'P' -> Just Color.lightBrown
        'G' -> Just Color.lightBlue
        '_' -> Nothing
        _->Just Color.purple

pixel: Int->Int->Char->Maybe Collage.Form
pixel col row clrChar=
    let
        drawPixel clr =
         Collage.square  pixelSize
                 |> Collage.filled clr
                 |> Collage.move ((toFloat col) * pixelSize,
                         (16 - toFloat row) * pixelSize)
    in
        toColor clrChar
        |> Maybe.map drawPixel

draw16: String->Collage.Form
draw16 bitmap=
    let
        indexes = Array.initialize 16 (\r->Array.initialize 16 (\c->(r,c)))
            |>Array.toList
            |>List.concatMap Array.toList
        pixels = bitmap
            |> String.toList
            |> List.filter (\c->Just Color.purple /= toColor c)
            |> List.map2 (,) indexes
    in
        pixels
            |> List.filterMap (\((row, col), char)-> (pixel col row char))
            |> Collage.group

-- optimisation for sprites with lots of gray pixels
draw16OnGray: String->Collage.Form
draw16OnGray bitmap=
    let
        grayBackground = Collage.square (bitmap16Size)
                             |> Collage.filled Color.gray
                             |> Collage.move (bitmap16Size/2,bitmap16Size/2)
    in
       grayBackground::[draw16 bitmap]
        |> Collage.group


playerDown: Collage.Form
playerDown =
    draw16 playerDownBmp

playerUp: Collage.Form
playerUp =
    draw16 playerUpBmp

playerLeft: Collage.Form
playerLeft =
    draw16 playerLeftBmp

playerRight: Collage.Form
playerRight =
    draw16 playerRightBmp

bomb: Collage.Form
bomb =
    draw16 bombBmp

monster: Collage.Form
monster =
    draw16 monsterBmp

wall: Collage.Form
wall =
    draw16OnGray wallBmp

pillar: Collage.Form
pillar =
    draw16OnGray pillarBmp

playerDownBmp: String
playerDownBmp =
   """
   _____000000_____
   ____00000000____
   ____0P_PP_P0____
   ____0P_PP_P0____
   ____00000000____
   _____000000_____
   ______GGGG______
   ____00GGGG00____
   ___00GGGGGG00___
   __PP0GGGGGG0PP__
   __PP_GGGGGG_PP__
   _____GGGGGG_____
   _____00GG00_____
   _____00__00_____
   _____PP__PP_____
   _____PP__PP_____
   """
playerUpBmp: String
playerUpBmp =
   """
   _____000000_____
   ____000GG000____
   ____00000G00____
   ____00000000____
   ____00000000____
   _____000000_____
   ______GGGG______
   ____0GGGGGG0____
   ___00GGGGGG00___
   __PP0GGPPGG0PP__
   __PP_000000_PP__
   _____GGGGGG_____
   _____00GG00_____
   _____00__00_____
   _____PP__PP_____
   _____PP__PP_____
   """
playerRightBmp: String
playerRightBmp =
      """
      ________________
      __P0_000000_____
      ____00000000____
      ___000PP_P_P____
      ___0000P_P_P____
      ____00000000____
      ____GG00000_____
      ____GGGGGG______
      ___G000GGG0_____
      ___000GGGG0_____
      ___00PGGGGPP____
      ___G0PPGGGPP____
      ___GGPPGGG______
      ____00G00_______
      ____00_PP_______
      ____PP_PPP______
      """
playerLeftBmp: String
playerLeftBmp =
      """
      ________________
      ____000000_0P___
      ___00000000_____
      ___P_P_PP000____
      ___P_P_P0000____
      ___00000000_____
      ____00000GG_____
      _____GGGGGG_____
      ____0GGG000G____
      ____0GGGG000____
      ___PPGGGGP00____
      ___PPGGGPP0G____
      _____GGGPPGG____
      ______00G00_____
      ______PP_00_____
      _____PPP_PP_____
      """
monsterBmp: String
monsterBmp =
   """
   _____XXXXXX_____
   ___XXXPPPPXXX___
   __XXPPPPPPPPXX__
   _XXPPPPPPPPPPXX_
   _XPPPPPPPPPPPPX_
   _XPPP00PP00PPPX_
   _XPPPX0PPX0PPPX_
   _XPPPX0PPX0PPPX_
   _XPPPPPPPPPPPPX_
   _XXPPPPPPPPPPXX_
   __XXPPPXXPPPXX__
   ___XXPPPPPPXX___
   ____XXPPPPXX____
   _____XXPPXX_____
   ______X00X______
   ______XXXX______
   """
pillarBmp: String
pillarBmp =
   """
   000000000000000X
   0IIIIIIIIIIIIIIX
   0IIIIIIIIIIIIIIX
   0IIIIIIIIIIIIIIX
   0IIIIIIIIIIIIIIX
   0IIIIIIIIIIIIIIX
   0IIIIIIIIIIIIIIX
   0IIIIIIIIIIIIIIX
   0IIIIIIIIIIIIIIX
   0IIIIIIIIIIIIIIX
   0IIIIIIIIIIIIIIX
   0IIIIIIIIIIIIIIX
   0IIIIIIIIIIIIIIX
   0IIIIIIIIIIIIIIX
   0XXXXXXXXXXXXXIX
   0XXXXXXXXXXXXXXX
   """

bombBmp: String
bombBmp =
   """
   _________000_0_0
   _____XXX00XX____
   ___XXXXX0XXXX_0_
   __XX00XX0XXXXX__
   _XX00XXXXXXXXXX_
   _XX00XXXXXXXXXX_
   XX00XXXXXXXXXXXX
   XX00XXXXXXXXXXXX
   XXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXX
   XXXXXXXXXXXXXXXX
   _XXXXXXXXXXXXXX_
   _XXXXXXXXXXXXXX_
   __XXXXXXXXXXXX__
   ___XXXXXXXXXX___
   _____XXXXXX_____
   """
wallBmp: String
wallBmp =
   """
   XXXXXXXXXXXXXXXX
   X00000000000000X
   0IIIIIIIIIIIIIIX
   0IIIIIIIIIIIIIIX
   0IIIIIIIIIIIIIIX
   XXXXXXXXXXXXXXXX
   00000XX000000000
   IIIIIX0IIIIIIIII
   IIIIIX0IIIIIIIII
   IIIIIX0IIIIIIIII
   XXXXXXXXXXXXXXXX
   000000000XX00000
   IIIIIIIIIX0IIIII
   IIIIIIIIIX0IIIII
   IIIIIIIIIX0IIIII
   XXXXXXXXXXXXXXXX
   """