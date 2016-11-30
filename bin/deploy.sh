#!/bin/bash
aws s3 cp public s3://tangelo/ --recursive --grants=read=uri=http://acs.amazonaws.com/groups/global/AllUsers
echo "Deployed to https://s3-us-west-1.amazonaws.com/tangelo/index.html!"
