#!/bin/bash

lazbuild=$1
buildmode=$2

$lazbuild --build-mode=$buildmode ../SimbaScript/SimbaScript.lpi || exit 1 
$lazbuild ../SimbaResources/SimbaResources.lpi || exit 1

../SimbaResources/SimbaResources $3 $4 $5 $6 $7 $8 $9 || exit 1