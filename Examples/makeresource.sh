fpcres="C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpcres.exe"

> examples.rc
find . -name '*.simba' -printf '"%f" RCDATA "%f"\n' >> examples.rc

$fpcres examples.rc -of res