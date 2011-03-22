{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    lpexportedmethods.inc for the Mufasa Macro Library
}
//Other
with TLapeType_OverloadedMethod.Create(Compiler, nil) do
begin
  with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltString)], [lptNormal], [TLapeGlobalVar(nil)]) do
    addMethod(NewGlobalVar(@lpWriteLn_Str));

  with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltString), getBaseType(ltStaticArray)], [lptNormal, lptNormal], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)]) do
    addMethod(NewGlobalVar(@lpWriteLn_FMT));    
  
  with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32)], [lptNormal], [TLapeGlobalVar(nil)]) do
    addMethod(NewGlobalVar(@lpWriteLn_Int32));
  
  with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt64)], [lptNormal], [TLapeGlobalVar(nil)]) do
    addMethod(NewGlobalVar(@lpWriteLn_Int64));
  
  with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltBoolean)], [lptNormal], [TLapeGlobalVar(nil)]) do
    addMethod(NewGlobalVar(@lpWriteLn_Bool));
    
  with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltExtended)], [lptNormal], [TLapeGlobalVar(nil)]) do
    addMethod(NewGlobalVar(@lpWriteLn_Extended));
    
  addGlobalVar(NewGlobalVar('WriteLn'));
end;

//Mouse
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32), getBaseType(ltInt32)], [lptNormal, lptNormal], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)]) do
  addGlobalVar(NewGlobalVar(@lpMoveMouse, 'MoveMouse'));
  
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32)], [lptNormal, lptNormal, lptNormal], [TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil)]) do
begin
  addGlobalVar(NewGlobalVar(@lpScrollMouse, 'ScrollMouse'));
  addGlobalVar(NewGlobalVar(@lpHoldMouse, 'HoldMouse'));
  addGlobalVar(NewGlobalVar(@lpReleaseMouse, 'ReleaseMouse'));
  addGlobalVar(NewGlobalVar(@lpClickMouse, 'ClickMouse'));
end;
  
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32), getBaseType(ltInt32)], [lptVar, lptVar], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)]) do
  addGlobalVar(NewGlobalVar(@lpGetMousePos, 'GetMousePos'));
  
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32)], [lptNormal], [TLapeGlobalVar(nil)], GetBaseType(ltBoolean)) do
  addGlobalVar(NewGlobalVar(@lpIsMouseButtonDown, 'IsMouseButtonDown'));

//Keyboard
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32)], [lptNormal], [TLapeGlobalVar(nil)]) do
begin
  addGlobalVar(NewGlobalVar(@lpKeyUp, 'KeyUp'));
  addGlobalVar(NewGlobalVar(@lpKeyDown, 'KeyDown'));
  addGlobalVar(NewGlobalVar(@lpPressKey, 'PressKey'));
end;

with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltString)], [lptConst], [TLapeGlobalVar(nil)]) do
  addGlobalVar(NewGlobalVar(@lpSendKeys, 'SendKeys'));
  
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32)], [lptNormal], [TLapeGlobalVar(nil)], GetBaseType(ltBoolean)) do
  addGlobalVar(NewGlobalVar(@lpisKeyDown, 'isKeyDown'));
  
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltChar)], [lptNormal], [TLapeGlobalVar(nil)], GetBaseType(ltInt32)) do
  addGlobalVar(NewGlobalVar(@lpGetKeyCode, 'GetKeyCode'));

//Settings
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltString)], [lptConst], [TLapeGlobalVar(nil)], GetBaseType(ltBoolean)) do
begin
  addGlobalVar(NewGlobalVar(@lpKeyIsSetting, 'KeyIsSetting'));
  addGlobalVar(NewGlobalVar(@lpKeyIsDirectory, 'KeyIsDirectory'));
  addGlobalVar(NewGlobalVar(@lpDeleteSetting, 'DeleteSetting'));
  addGlobalVar(NewGlobalVar(@lpDeleteSubSettings, 'DeleteSubSettings'));
end;
 
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltString), getBaseType(ltDynArray)], [lptConst, lptVar], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], GetBaseType(ltBoolean)) do
  addGlobalVar(NewGlobalVar(@lpListSettings, 'ListSettings'));
   
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltString), getBaseType(ltString)], [lptConst, lptConst], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], GetBaseType(ltString)) do
  addGlobalVar(NewGlobalVar(@lpSetSettingValue, 'SetSettingValue'));
  
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltString)], [lptConst], [TLapeGlobalVar(nil)]) do
  addGlobalVar(NewGlobalVar(@lpGetSettingValue, 'GetSettingValue'));
  
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltString), getBaseType(ltString)], [lptConst, lptConst], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], GetBaseType(ltString)) do
  addGlobalVar(NewGlobalVar(@lpGetSettingValueDef, 'GetSettingValueDef'));
  
//ColorConv
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32)], [lptConst, lptVar, lptVar, lptVar], [TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil)]) do
  addGlobalVar(NewGlobalVar(@lpColorToRGB, 'ColorToRGB'));
  
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32)], [lptConst, lptConst, lptConst], [TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil)], GetBaseType(ltInt32)) do
  addGlobalVar(NewGlobalVar(@lpRGBToColor, 'RGBToColor'));

with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32), getBaseType(ltExtended), getBaseType(ltExtended), getBaseType(ltExtended)], [lptConst, lptVar, lptVar, lptVar], [TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil)]) do
begin
  addGlobalVar(NewGlobalVar(@lpColorToHSL, 'ColorToHSL'));
  addGlobalVar(NewGlobalVar(@lpColorToXYZ, 'ColorToXYZ'));
end;
  
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltExtended), getBaseType(ltExtended), getBaseType(ltExtended)], [lptConst, lptConst, lptConst], [TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil)], GetBaseType(ltInt32)) do
begin
  addGlobalVar(NewGlobalVar(@lpHSLToColor, 'HSLToColor'));
  addGlobalVar(NewGlobalVar(@lpXYZToColor, 'XYZToColor'));  
end;

with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltExtended), getBaseType(ltExtended), getBaseType(ltExtended)], [lptConst, lptConst, lptConst, lptVar, lptVar, lptVar], [TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil)]) do
begin
  addGlobalVar(NewGlobalVar(@lpRGBToHSL, 'RGBToHSL'));
  addGlobalVar(NewGlobalVar(@lpRGBToXYZ, 'RGBToXYZ'));
end;

with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltExtended), getBaseType(ltExtended), getBaseType(ltExtended), getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32)], [lptConst, lptConst, lptConst, lptVar, lptVar, lptVar], [TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil)]) do
begin
  addGlobalVar(NewGlobalVar(@lpHSLToRGB, 'HSLtoRGB'));
  addGlobalVar(NewGlobalVar(@lpXYZToRGB, 'XYZToRGB'));
end;

//Colour
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32), getBaseType(ltInt32)], [lptConst, lptConst], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], GetBaseType(ltInt32)) do
  addGlobalVar(NewGlobalVar(@lpGetColor, 'GetColor'));

with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltDynArray), getBaseType(ltDynArray)], [lptConst, lptVar], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)], GetBaseType(ltDynArray)) do
  addGlobalVar(NewGlobalVar(@lpGetColorsWrap, 'GetColorsWrap'));

with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltDynArray)], [lptConst], [TLapeGlobalVar(nil)], GetBaseType(ltDynArray)) do
  addGlobalVar(NewGlobalVar(@lpGetColors, 'GetColors'));
  
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32)], [lptVar, lptVar, lptConst, lptConst, lptConst, lptConst, lptConst], [TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil)], GetBaseType(ltBoolean)) do
begin
  addGlobalVar(NewGlobalVar(@lpFindColor, 'FindColor'));
  addGlobalVar(NewGlobalVar(@lpFindColorSpiral, 'FindColorSpiral'));
end;

with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32),getBaseType(ltInt32)], [lptVar, lptVar, lptConst, lptConst, lptConst, lptConst, lptConst, lptConst], [TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil)], GetBaseType(ltBoolean)) do  
begin
  addGlobalVar(NewGlobalVar(@lpFindColorToleranceOptimised, 'FindColorToleranceOptimised'));
  addGlobalVar(NewGlobalVar(@lpFindColorTolerance, 'FindColorTolerance'));
  addGlobalVar(NewGlobalVar(@lpFindColorSpiralTolerance, 'FindColorSpiralTolerance'));
end;

with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32)], [lptConst], [TLapeGlobalVar(nil)]) do
  addGlobalVar(NewGlobalVar(@lpSetColorToleranceSpeed, 'SetColorToleranceSpeed'));

with TLapeType_ExternalMethod.Create(Compiler, [], [], [], GetBaseType(ltInt32)) do
  addGlobalVar(NewGlobalVar(@lpGetToleranceSpeed, 'GetToleranceSpeed'));
  
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltExtended), getBaseType(ltExtended)], [lptConst, lptConst], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)]) do
  addGlobalVar(NewGlobalVar(@lpSetToleranceSpeed2Modifiers, 'SetToleranceSpeed2Modifiers'));
  
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltExtended), getBaseType(ltExtended)], [lptVar, lptVar], [TLapeGlobalVar(nil), TLapeGlobalVar(nil)]) do
  addGlobalVar(NewGlobalVar(@lpGetToleranceSpeed2Modifiers, 'GetToleranceSpeed2Modifiers'));
  
with TLapeType_ExternalMethod.Create(Compiler, [getBaseType(ltInt32), getBaseType(ltInt32), getBaseType(ltInt32)], [lptConst, lptConst, lptConst], [TLapeGlobalVar(nil), TLapeGlobalVar(nil), TLapeGlobalVar(nil)], GetBaseType(ltBoolean)) do
  addGlobalVar(NewGlobalVar(@lpSimilarColors, 'SimilarColors'));

  