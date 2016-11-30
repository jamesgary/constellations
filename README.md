# Tangelo

Playable [here](https://s3-us-west-1.amazonaws.com/tangelo/index.html)!

## To Run

```
# Requires https://github.com/tomekwi/elm-live
# Runs on http://localhost:8000

./bin/run

# which does:

elm-live main.elm --output=public/js/main.js --dir=public/
```

## Just To Make

```
elm-make main.elm --output=public/js/main.js
```

## To Deploy to S3

```
./bin/deploy
```
