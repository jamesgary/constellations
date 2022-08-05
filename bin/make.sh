#!/bin/bash
npx elm make src/Main.elm --output=public/js/main.js && npx sass sass/main.scss:public/css/main.css
