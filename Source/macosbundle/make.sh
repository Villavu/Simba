#!/bin/sh
 
echo "Creating macOS application bundle"
 
rm -r ../Simba.zip
rm -r ../Simba.app
cp -R macosbundle/Simba.app ../
 
cd ..
cp Simba Simba.app/Contents/MacOS/
rm Simba.app/Contents/MacOS/.gitkeep