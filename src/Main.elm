module Main exposing (..)
import Browser
import Html exposing (Html, button, div, text, input, a, i, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events exposing (onInput)
import String exposing (toFloat)

type Model
  = CalculatingRecipe RecipeParameters Recipe
  | CalculatingEnriched RecipeParameters EnrichedRecipe

type alias RecipeParameters = 
  { totalFlour: Int
  , hydration: Float
  , starterPercent: Float
  , saltPercent: Float
  }

type alias Recipe = 
  { recipeFlour: Int
  , recipeWater: Int
  , starterAmount: Int
  , saltAmount: Int
  , totalDoughWeight: Int
  }

type alias EnrichedRecipe = 
  { recipeFlour: Int
  , recipeMilk: Int
  , recipeEggs: Int
  , starterAmount: Int
  , saltAmount: Int
  , recipeSugar: Int
  , recipeButter: Int
  , totalDoughWeight: Int
  }

calculateRecipe: RecipeParameters -> Recipe
calculateRecipe parms =
  let 
    starterAmount = round (Basics.toFloat parms.totalFlour * parms.starterPercent/100.0)
    starterWater = starterAmount // 2
    starterFlour = starterWater
    totalWater = round (Basics.toFloat parms.totalFlour * parms.hydration/100.0)
    recipeFlour = parms.totalFlour - starterFlour
    recipeWater = totalWater - starterWater
    saltAmount = round (Basics.toFloat parms.totalFlour * parms.saltPercent/100.0)
    totalDoughWeight = (recipeFlour + recipeWater + starterAmount + saltAmount)
  in
    { recipeFlour = recipeFlour
    , recipeWater = recipeWater
    , starterAmount = starterAmount 
    , saltAmount = saltAmount
    , totalDoughWeight = totalDoughWeight
    }

calculateEnrichedRecipe: RecipeParameters -> EnrichedRecipe
calculateEnrichedRecipe parms = 
  let 
    starterAmount = round (Basics.toFloat parms.totalFlour * parms.starterPercent/100.0)
    starterWater = starterAmount // 2
    starterFlour = starterWater
    totalWater = round (Basics.toFloat parms.totalFlour * parms.hydration/100.0)
    recipeFlour = parms.totalFlour - starterFlour
    recipeHydration = totalWater - starterWater
    recipeMilk = round (Basics.toFloat recipeHydration * 0.67)
    recipeEggs = round (Basics.toFloat recipeHydration * 0.33)
    recipeButter = round(Basics.toFloat parms.totalFlour * 0.4)
    saltAmount = round (Basics.toFloat parms.totalFlour * parms.saltPercent/100.0)
    recipeSugar = round (Basics.toFloat parms.totalFlour * parms.saltPercent * 2/100.0)
    totalDoughWeight = (recipeFlour + recipeMilk + recipeEggs + recipeButter + starterAmount + saltAmount + recipeSugar)
  in
    { recipeFlour = recipeFlour
    , recipeMilk = recipeMilk
    , recipeEggs = recipeEggs
    , starterAmount = starterAmount 
    , saltAmount = saltAmount
    , recipeButter = recipeButter
    , recipeSugar = recipeSugar
    , totalDoughWeight = totalDoughWeight
    }


initParms: RecipeParameters
initParms =  { totalFlour = 500
        , hydration = 70
        , starterPercent = 20
        , saltPercent = 2
        }

-- convertRecipeForm: RecipeForm -> RecipeParameters
-- convertRecipeForm form =
--   { totalFlour = Maybe.withDefault 0 (String.toInt form.totalFlour)
--   , hydration = Maybe.withDefault 0.0 (String.toFloat form.hydration)
--   , starterPercent = Maybe.withDefault 0.0 (String.toFloat form.starterPercent)
--   , saltPercent = Maybe.withDefault 0.0 (String.toFloat form.saltPercent)
--   }

init:  Model
init =  CalculatingRecipe initParms (calculateRecipe initParms)

main: Program () Model Msg
main =
  Browser.sandbox { init = init, update = update, view = view }

type Msg = 
      ChangeTotalFlour String
      | ChangeHydration String
      | ChangeStarter String
      | SelectEnriched
      | SelectNormal
    
update: Msg->Model->Model
update msg model =
  case model of
     CalculatingRecipe parms rec ->
      case msg of
        ChangeTotalFlour newFlour -> 
          let
            parameters = parms |> (\p -> {p | totalFlour = Maybe.withDefault 0 (String.toInt newFlour)})
          in
            CalculatingRecipe parameters (calculateRecipe parameters)
        ChangeHydration newHydration ->
          let 
            parameters = parms |> \p -> {p | hydration = Maybe.withDefault 0.0 (String.toFloat newHydration)}
            recipe = calculateRecipe parameters
          in
            CalculatingRecipe parameters (calculateRecipe parameters)
        ChangeStarter newStarter ->
          let 
            parameters = parms |> \p -> {p | starterPercent = Maybe.withDefault 0.0 (String.toFloat newStarter)}
          in
            CalculatingRecipe parameters (calculateRecipe parameters)
        SelectEnriched -> 
            CalculatingEnriched parms (calculateEnrichedRecipe parms)
        SelectNormal -> 
            CalculatingRecipe parms (calculateRecipe parms)
     CalculatingEnriched parms rec ->
      case msg of
        ChangeTotalFlour newFlour -> 
          let
            parameters = parms |> (\p -> {p | totalFlour = Maybe.withDefault 0 (String.toInt newFlour)})
          in
            CalculatingEnriched parameters (calculateEnrichedRecipe parameters)
        ChangeHydration newHydration ->
          let 
            parameters = parms |> \p -> {p | hydration = Maybe.withDefault 0.0 (String.toFloat newHydration)}
            recipe = calculateRecipe parameters
          in
            CalculatingEnriched parameters (calculateEnrichedRecipe parameters)
        ChangeStarter newStarter ->
          let 
            parameters = parms |> \p -> {p | starterPercent = Maybe.withDefault 0.0 (String.toFloat newStarter)}
          in
            CalculatingEnriched parameters (calculateEnrichedRecipe parameters)
        SelectEnriched -> 
            CalculatingEnriched parms (calculateEnrichedRecipe parms)
        SelectNormal -> 
            CalculatingRecipe parms (calculateRecipe parms)

-- class="w3-panel w3-white w3-card w3-display-container"
view: Model->Html Msg
view model = 
  case model of
     CalculatingRecipe parms recipe ->
          div [][ header
          , div [class "w3-container w3-content"]
              [ 
                typeSelector Normal
                , inputParameters parms
                , div [class "w3-panel w3-white w3-card w3-display-container", id "parameters"][ 
                  div [][
                    text "Recipe Flour "
                    , text (String.fromInt recipe.recipeFlour)
                  ]
                  ,div [][
                      text "Recipe Water "
                    , text (String.fromInt recipe.recipeWater)
                  ]
                  ,div [][
                      text "Starter "
                    ,           text (String.fromInt recipe.starterAmount)
                  ]
                  ,div [][
                      text "Salt "
                    , text (String.fromInt recipe.saltAmount)
                  ]
                  ,div [][
                      text "Total Dough Weight "
                    , text (String.fromInt recipe.totalDoughWeight)
                  ]
                ]
              ]]
     CalculatingEnriched parms recipe ->
          div [][ header
          , div [class "w3-container w3-content"]
              [ 
                typeSelector Enriched
                , inputParameters parms
                , div [class "w3-panel w3-white w3-card w3-display-container", id "parameters"][ 
                  div [][
                    text "Recipe Flour "
                    , text (String.fromInt recipe.recipeFlour)
                  ]
                  ,div [][
                      text "Recipe Milk "
                    , text (String.fromInt recipe.recipeMilk)
                  ]
                  ,div [][
                      text "Recipe Eggs "
                    , text (String.fromInt recipe.recipeEggs)
                  ]
                  ,div [][
                      text " (Milk + Eggs) "
                    , text (String.fromInt (recipe.recipeMilk + recipe.recipeEggs))
                  ]
                  ,div [][
                      text "Starter "
                    ,           text (String.fromInt recipe.starterAmount)
                  ]
                  ,div [][
                      text "Salt "
                    , text (String.fromInt recipe.saltAmount)
                  ]
                  ,div [][
                      text "Recipe Sugar "
                    , text (String.fromInt recipe.recipeSugar)
                  ]
                  ,div [][
                      text "Recipe Butter "
                    , text (String.fromInt recipe.recipeButter)
                  ]
                  ,div [][
                      text "Total Dough Weight "
                    , text (String.fromInt recipe.totalDoughWeight)
                  ]
                ]
              ]]

--     <div class="w3-bar w3-large w3-theme-d4">
      --   <a href="#" class="w3-bar-item w3-button"><i class="fa fa-bars"></i></a>
      --   <span class="w3-bar-item">THE SOURDOUGH MASTER</span>
      --   <a href="#" class="w3-bar-item w3-button w3-right"><i class="fa fa-search"></i></a>
      -- </div>
      

header: Html Msg
header = div [class "w3-bar w3-large w3-theme-d4"]
  [a [href "#", class "w3-bar-item w3-button"][i [class "fa fa-bars"] []]
  , span [class "w3-bar-item"] [text "SOURDOUGH MASTER"]
  ]

type Formula 
    = Normal
    | Enriched 


typeSelector: Formula -> Html Msg
typeSelector formula = div [class "w3-panel w3-white w3-card w3-display-container"][
    case formula of
        Normal ->
            div [][button [class "selected", onClick SelectNormal][text "Normal"]
                    , button [class "normal", onClick SelectEnriched][text "Enriched"]
                    ]
        Enriched ->
            div [][button [class "normal", onClick SelectNormal][text "Normal"]
                    , button [class "selected", onClick SelectEnriched][text "Enriched"]
                    ]
    ]

inputParameters: RecipeParameters -> Html Msg
inputParameters parms = 
            div [class "w3-panel w3-white w3-card w3-display-container", id "parameters"][ 
                  div [][
                  text "Total Flour "
                  , input [size 4, placeholder "Total Flour", value (String.fromInt parms.totalFlour), onInput ChangeTotalFlour][]
                ]
                ,  div [][
                  text "Hydration "
                  , input [size 3, placeholder "Hydration", value (String.fromFloat parms.hydration), onInput ChangeHydration][]
                  , text "%"                  
                ]
                ,  div [][
                  text "Percent of starter "
                  , input [size 3, placeholder "Percent of starter", value (String.fromFloat parms.starterPercent), onInput ChangeStarter][]
                  ,text "%"
                  ]
                ]
