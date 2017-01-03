# Tangelo

Playable [here](https://s3-us-west-1.amazonaws.com/tangelo/index.html)!

## To Run

For compiling elm to js:

```
# Requires https://github.com/tomekwi/elm-live
# Runs on http://localhost:8000

./bin/run

# which does:

elm-live main.elm --output=public/js/main.js --dir=public/
```

For compiling sass to css:

```
./bin/sass

# which does:

sass --watch sass/main.scss:public/css/main.css
```

## Just To Make

```
elm-make main.elm --output=public/js/main.js && sass sass/main.scss:public/css/main.css
```

## To Deploy to S3

```
./bin/deploy
```
