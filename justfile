default:
    echo 'Hello, world!'

review args="":
    elm-review --report=json --extract {{args}} | jq '.extracts.ElmCssExtractor'
