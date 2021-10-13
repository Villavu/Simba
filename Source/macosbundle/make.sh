#!/bin/sh
 
echo "Creating macOS application bundle"
 
rm -r ../Simba-Darwin64.zip
rm -r ../Simba-Darwin64.app
cp -R macosbundle/Simba.app ../
 
cd ..
cp Simba-Darwin64 Simba.app
 
zip -r Simba-Darwin64.zip Simba.app