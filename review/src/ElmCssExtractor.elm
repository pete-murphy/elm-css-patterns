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
            , isCurrentlyIgnored = False
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
    , isCurrentlyIgnored : Bool
    }


expressionEnterVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionEnterVisitor node context =
    if context.isCurrentlyIgnored then
        ( [], context )

    else
        case node of
            Node range (Expression.ListExpr nodes) ->
                case List.partition (isFromCssModule context.lookupTable) nodes of
                    ( [], _ ) ->
                        ( [], context )

                    ( _, nonCssNodes ) ->
                        let
                            item =
                                { moduleName = context.moduleName
                                , range = Node.range node
                                , sourceCode = context.sourceCodeExtractor (Node.range node)
                                , nonCssRanges = List.map Node.range nonCssNodes
                                }
                        in
                        ( --[ Rule.errorForModule context.moduleKey { message = "Found Css module", details = [] } (Node.range node) ]
                          []
                        , { context
                            | acc = item :: context.acc
                            , isCurrentlyIgnored = True
                            , ignoredRange = Just range
                          }
                        )

            _ ->
                ( [], context )


expressionExitVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionExitVisitor node context =
    case node of
        Node range _ ->
            if context.isCurrentlyIgnored && context.ignoredRange == Just range then
                ( []
                , { context
                    | isCurrentlyIgnored = False
                    , ignoredRange = Nothing
                  }
                )

            else
                ( [], context )


dataExtractor : ProjectContext -> Json.Encode.Value
dataExtractor =
    Json.Encode.list encodeAccItem


encodeAccItem : AccItem -> Json.Encode.Value
encodeAccItem item =
    Json.Encode.object
        [ ( "moduleName", Json.Encode.string (item.moduleName |> String.join ".") )
        , ( "range"
          , Json.Encode.object
                [ ( "start"
                  , Json.Encode.object
                        [ ( "row", Json.Encode.int item.range.start.row )
                        , ( "column", Json.Encode.int item.range.start.column )
                        ]
                  )
                , ( "end"
                  , Json.Encode.object
                        [ ( "row", Json.Encode.int item.range.end.row )
                        , ( "column", Json.Encode.int item.range.end.column )
                        ]
                  )
                ]
          )
        , ( "sourceCode", Json.Encode.string item.sourceCode )
        , ( "nonCssRanges"
          , Json.Encode.list
                (\range ->
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
                )
                item.nonCssRanges
          )
        , ( "nonCssRelativeRanges"
          , Json.Encode.list
                (\range_ ->
                    let
                        range =
                            { start = { row = range_.start.row - item.range.start.row, column = range_.start.column }
                            , end = { row = range_.end.row - item.range.start.row, column = range_.end.column }
                            }
                    in
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
                                , ( "column"
                                  , Json.Encode.int range.end.column
                                  )
                                ]
                          )
                        ]
                )
                item.nonCssRanges
          )
        ]



-- Json.Encode.list (\( x, y ) -> Json.Encode.list identity [ Json.Encode.string x, Json.Encode.list Json.Encode.string y )
-- Json.Encode.list (\( x, y ) -> Json.Encode.string (x ++ " => " ++ String.join ", " y))
