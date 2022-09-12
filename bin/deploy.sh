#!/bin/bash

echo "Make sure you've ran `npm run build` first!"

aws s3 cp build s3://constellationsgame.com/ --recursive --grants=read=uri=http://acs.amazonaws.com/groups/global/AllUsers

echo "Deployed to http://constellationsgame.com.s3-website-us-east-1.amazonaws.com/ !"
