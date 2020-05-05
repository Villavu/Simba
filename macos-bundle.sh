#!/bin/sh

rm -rf Simba.app

app_name=Simba
plist_file=$app_name.app/Contents/Info.plist
app_file=Simba-Darwin64
app_version=1.4
app_build=1400

pkg_info_contents="APPLMAG#"

mkdir -p $app_name.app
mkdir -p $app_name.app/Contents
mkdir -p $app_name.app/Contents/MacOS
mkdir -p $app_name.app/Contents/Frameworks
mkdir -p $app_name.app/Contents/Resources

cp $app_file $app_name.app/Contents/MacOS/$app_name
cp Images/Simba.icns $app_name.app/Contents/Resources

echo $pkg_info_contents > $app_name.app/Contents/PkgInfo
echo '<?xml version="1.0" encoding="UTF-8"?>' >> $plist_file
echo '<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">' >> $plist_file
echo '<plist version="1.0">' >> $plist_file
echo '<dict>' >> $plist_file
echo '	<key>CFBundleDevelopmentRegion</key>' >> $plist_file
echo '	<string>English</string>' >> $plist_file
echo '	<key>CFBundleExecutable</key>' >> $plist_file
echo '	<string>'$app_name'</string>' >> $plist_file
echo '	<key>CFBundleName</key>' >> $plist_file
echo '	<string>Simba</string>' >> $plist_file
echo '	<key>CFBundleIdentifier</key>' >> $plist_file
echo '	<string>com.srl.'$app_name'</string>' >> $plist_file
echo '	<key>CFBundleIconFile</key>' >> $plist_file
echo '	<string>Simba.icns</string>' >> $plist_file
echo '	<key>CFBundleInfoDictionaryVersion</key>' >> $plist_file
echo '	<string>6.0</string>' >> $plist_file
echo '	<key>CFBundlePackageType</key>' >> $plist_file
echo '	<string>APPL</string>' >> $plist_file
echo '	<key>CFBundleSignature</key>' >> $plist_file
echo '	<string>Simb</string>' >> $plist_file
echo '	<key>CFBundleShortVersionString</key>' >> $plist_file
echo '	<string>'app_version'</string>' >> $plist_file
echo '	<key>CFBundleVersion</key>' >> $plist_file
echo '	<string>'app_build'</string>' >> $plist_file
echo '	<key>CSResourcesFileMapped</key>' >> $plist_file
echo '	<false/>' >> $plist_file
echo '	<key>CFBundleDocumentTypes</key>' >> $plist_file
echo '	<array>' >> $plist_file
echo '		<dict>' >> $plist_file
echo '			<key>CFBundleTypeRole</key>' >> $plist_file
echo '			<string>Viewer</string>' >> $plist_file
echo '			<key>CFBundleTypeName</key>' >> $plist_file
echo '			<string>Simba Scripts</string>' >> $plist_file
echo '			<key>CFBundleTypeExtensions</key>' >> $plist_file
echo '			<array>' >> $plist_file
echo '				<string>simba</string>' >> $plist_file
echo '			</array>' >> $plist_file
echo '			<key>CFBundleTypeIconFiles</key>' >> $plist_file
echo '			<array>' >> $plist_file
echo '				<string>Simba.icns</string>' >> $plist_file
echo '			</array>' >> $plist_file
echo '		</dict>' >> $plist_file
echo '	</array>' >> $plist_file
echo '	<key>NSHighResolutionCapable</key>' >> $plist_file
echo '	<true/>' >> $plist_file
echo '</dict>' >> $plist_file
echo '</plist>' >> $plist_file
