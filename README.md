# Constellations

Playable at [http://constellationsgame.com.s3-website-us-east-1.amazonaws.com/](http://constellationsgame.com.s3-website-us-east-1.amazonaws.com/)!

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
