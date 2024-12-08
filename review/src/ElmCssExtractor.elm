module ElmCssExtractor exposing (rule)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Json.Encode
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newProjectRuleSchema "ElmCssExtractor" []
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withDataExtractor dataExtractor
        |> Rule.fromProjectRuleSchema


moduleVisitor schema =
    schema
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor
        |> Rule.withExpressionExitVisitor expressionExitVisitor


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator (\x -> x.acc)


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts x y =
    x ++ y


type alias AccItem =
    { moduleName : ModuleName
    , range : Range
    , sourceCode : String
    , nonCssRanges : List Range
    , simpleRanges : List Range
    , countTotal : Int
    }


type alias ProjectContext =
    List AccItem


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable key name extract projectContext ->
            { lookupTable = lookupTable
            , moduleKey = key
            , moduleName = name
            , sourceCodeExtractor = extract
            , acc = projectContext
            , ignoredRange = Nothing
            }
        )
        |> Rule.withModuleNameLookupTable
        |> Rule.withModuleKey
        |> Rule.withModuleName
        |> Rule.withSourceCodeExtractor


isFromCssModule : ModuleNameLookupTable -> Node Expression -> Bool
isFromCssModule lookupTable node =
    case ModuleNameLookupTable.moduleNameFor lookupTable node |> Maybe.andThen List.head of
        Just "Css" ->
            True

        _ ->
            False



-- So your context probably needs to be a record: { ignoredRanges : List Range, isCurrentlyIgnored : Bool }.


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , moduleKey : Rule.ModuleKey
    , moduleName : ModuleName
    , sourceCodeExtractor : Range -> String
    , acc : List AccItem
    , ignoredRange : Maybe Range
    }


{-| Only contains values or functions from `Css` module(s) _or_ literals
(strings, ints, floats)
-}
isSimpleCss : Node Expression -> Bool
isSimpleCss node =
    case node of
        Node _ (Expression.Literal _) ->
            True

        Node _ (Expression.FunctionOrValue ("Css" :: _) _) ->
            True

        Node _ (Expression.Application nodes) ->
            List.all isSimpleCss nodes

        Node _ (Expression.ParenthesizedExpression node_) ->
            isSimpleCss node_

        Node _ (Expression.ListExpr nodes) ->
            List.all isSimpleCss nodes

        Node _ (Expression.Integer _) ->
            True

        Node _ (Expression.Floatable _) ->
            True

        Node _ (Expression.OperatorApplication _ _ left right) ->
            List.all isSimpleCss [ left, right ]

        _ ->
            False


expressionEnterVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionEnterVisitor node context =
    case context.ignoredRange of
        Just _ ->
            ( [], context )

        _ ->
            case node of
                Node range (Expression.ListExpr nodes) ->
                    case List.partition (isFromCssModule context.lookupTable) nodes of
                        ( [], _ ) ->
                            ( [], context )

                        ( _, nonCssNodes ) ->
                            let
                                item =
                                    { moduleName = context.moduleName
                                    , range = range
                                    , sourceCode = context.sourceCodeExtractor range
                                    , nonCssRanges = List.map Node.range nonCssNodes
                                    , simpleRanges = nodes |> List.filter isSimpleCss |> List.map Node.range
                                    , countTotal = List.length nodes
                                    }
                            in
                            ( --[ Rule.errorForModule context.moduleKey { message = "Found Css module", details = [] } (Node.range node) ]
                              []
                            , { context
                                | acc = item :: context.acc
                                , ignoredRange = Just range
                              }
                            )

                _ ->
                    ( [], context )


expressionExitVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionExitVisitor node context =
    case node of
        Node range _ ->
            if context.ignoredRange == Just range then
                ( []
                , { context
                    | ignoredRange = Nothing
                  }
                )

            else
                ( [], context )


dataExtractor : ProjectContext -> Json.Encode.Value
dataExtractor =
    Json.Encode.list encodeAccItem


encodeRange : Range -> Json.Encode.Value
encodeRange range =
    Json.Encode.object
        [ ( "start"
          , Json.Encode.object
                [ ( "row", Json.Encode.int range.start.row )
                , ( "column", Json.Encode.int range.start.column )
                ]
          )
        , ( "end"
          , Json.Encode.object
                [ ( "row", Json.Encode.int range.end.row )
                , ( "column", Json.Encode.int range.end.column )
                ]
          )
        ]


encodeAccItem : AccItem -> Json.Encode.Value
encodeAccItem item =
    Json.Encode.object
        [ ( "moduleName", Json.Encode.string (item.moduleName |> String.join ".") )
        , ( "range"
          , encodeRange item.range
          )
        , ( "sourceCode", Json.Encode.string item.sourceCode )
        , ( "nonCssRanges"
          , Json.Encode.list encodeRange
                item.nonCssRanges
          )
        , ( "nonCssRelativeRanges"
          , Json.Encode.list encodeRange
                (item.nonCssRanges
                    |> List.map
                        (\range_ ->
                            let
                                range =
                                    { start = { row = range_.start.row - item.range.start.row, column = range_.start.column }
                                    , end = { row = range_.end.row - item.range.start.row, column = range_.end.column }
                                    }
                            in
                            range
                        )
                )
          )
        , ( "simpleRelativeRanges"
          , Json.Encode.list encodeRange
                (item.simpleRanges
                    |> List.map
                        (\range_ ->
                            let
                                range =
                                    { start = { row = range_.start.row - item.range.start.row, column = range_.start.column }
                                    , end = { row = range_.end.row - item.range.start.row, column = range_.end.column }
                                    }
                            in
                            range
                        )
                )
          )
        , ( "countTotal", Json.Encode.int item.countTotal )
        ]



-- Json.Encode.list (\( x, y ) -> Json.Encode.list identity [ Json.Encode.string x, Json.Encode.list Json.Encode.string y )
-- Json.Encode.list (\( x, y ) -> Json.Encode.string (x ++ " => " ++ String.join ", " y))
