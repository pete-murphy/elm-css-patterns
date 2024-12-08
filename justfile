default:
    echo 'Hello, world!'

review-watch:
    just review --watch

review args="":
    elm-review --report=json --extract {{args}} | jq '.extracts.ElmCssExtractor'
