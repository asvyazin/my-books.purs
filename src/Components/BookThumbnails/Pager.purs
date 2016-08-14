module Components.BookThumbnails.Pager where


import Components.Wrappers.Router (link)
import Data.Array (concat, (..))
import Data.Tuple (Tuple(Tuple))
import Prelude
import React.DOM (nav, ul, li, text, span) as R
import React.DOM.Props (aria, className) as R
import Thermite as T


type Props =
  { currentPage :: Int
  , pageCount :: Int
  }


spec :: forall eff state action. T.Spec eff state Props action
spec =
  T.simpleSpec T.defaultPerformAction render
  where
    render _ props _ _ =
      if props.pageCount == 0
      then []
      else
        case rangeToShow props.currentPage props.pageCount of
          Tuple start end ->
            [ R.nav
              [ R.aria { label : "Pager" } ]
              [ R.ul
                [ R.className "pagination" ]
                (concat
                 [ renderStart start
                 , renderRange start end props.currentPage
                 , renderEnd end props.pageCount
                 ]
                )
              ]
            ]

    renderStart 0 =
      []
    renderStart 1 =
      [ renderItem 0 [] ]
    renderStart _ =
      [ renderItem 0 [], renderThreeDots ]

    renderEnd end pageCount =
      case pageCount - end of
        1 ->
          []
        2 ->
          [ renderItem (pageCount - 1) [] ]
        _ ->
          [ renderThreeDots
          , renderItem (pageCount - 1) []
          ]

    renderRange start end currentPage =
      map (renderRangeItem currentPage) (start .. end)

    renderRangeItem currentPage index =
      let
        attrs =
          if currentPage == index
          then [ R.className "active disabled" ]
          else []
      in
        renderItem index attrs

    renderItem index attrs =
      let
        pageNumberLink = show index
        pageNumber = show (index + 1)
      in
        R.li attrs
        [ link
          { to : "/page/" <> pageNumberLink }
          [ R.text pageNumber ]
        ]

    renderThreeDots =
      R.li [ R.className "disabled" ] [ R.span [] [ R.text "..." ] ]


rangeToShow :: Int -> Int -> Tuple Int Int
rangeToShow currentPage pageCount =
  if pageCount <= 5
  then Tuple 0 (pageCount - 1)
  else
    let
      possibleStart = currentPage - 2
      possibleEnd = currentPage + 2
    in
      if possibleStart <= 0
      then Tuple 0 4
      else if possibleEnd >= pageCount - 1
           then Tuple (pageCount - 5) (pageCount - 1)
           else Tuple possibleStart possibleEnd
