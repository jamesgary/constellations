# Constellations

Playable at [constellationsgame.com](http://constellationsgame.com)!

## To Run

For compiling elm to js:

```
# Uses https://github.com/tomekwi/elm-live
# Runs on http://localhost:8000

./bin/run

# which does:

npx elm-live main.elm --output=public/js/main.js --dir=public/
```

For compiling sass to css:

```
./bin/sass.sh

# which does:

npx sass --watch sass/main.scss:public/css/main.css
```

## Just To Make

```
./bin/make.sh

# which does:

npx elm-make main.elm --output=public/js/main.js && npx sass sass/main.scss:public/css/main.css
```

## To Deploy to S3

```
./bin/deploy
```
