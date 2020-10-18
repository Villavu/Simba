#!/bin/sh

rm -r macos/Simba.app/Contents/MacOS/*
cp ../../../Simba-Darwin64 macos/Simba.app/Contents/MacOS/
cp -R macos/Simba.app ../../../Simba.app
zip -r ../../../Simba-Darwin64.zip ../../../Simba.app