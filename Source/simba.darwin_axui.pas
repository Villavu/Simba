{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.darwin_axui;

{$i simba.inc}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  simba.base, simba.ide_initialization;

type
  TAXUIWindowInfo = record
    PID: UInt32;
    Title: String;
    Children: array of record
      Bounds: TBox;
      ClassName: String;
    end;
  end;

function AXUI_GetWindowInfo(PID: UInt32): TAXUIWindowInfo;
function AXUI_GetWindowClass(PID: UInt32): String;
function AXUI_GetWindowTitle(PID: UInt32): String;

implementation

uses
  MacOSAll, CocoaAll, CocoaUtils;

function AXIsProcessTrustedWithOptions(opts: NSDictionary): Boolean; external name '_AXIsProcessTrustedWithOptions';

var
  kAXTrustedCheckOptionPrompt: NSString; cvar; external;

  AXTitleStr, AXRoleStr, AXButtonStr, AXImageStr, AXScrollAreaStr,
  AXToolbarStr, AXMenuBarStr, AXChildrenStr, AXGroupStr, AXWindowStr,
  AXSplitGroup, AXApplicationStr, AXListStr, AXTabGroupStr, AXFrameStr, AXMainWindowStr: CFStringRef;

procedure CreateCFStrings;
begin
  CreateCFString('AXList', AXListStr);
  CreateCFString('AXWindow', AXWindowStr);
  CreateCFString('AXGroup', AXGroupStr);
  CreateCFString('AXRole', AXRoleStr);
  CreateCFString('AXButton', AXButtonStr);
  CreateCFString('AXImage', AXImageStr);
  CreateCFString('AXScrollArea', AXScrollAreaStr);
  CreateCFString('AXToolbar', AXToolbarStr);
  CreateCFString('AXMenuBar', AXMenuBarStr);
  CreateCFString('AXChildren', AXChildrenStr);
  CreateCFString('AXSplitGroup', AXSplitGroup);
  CreateCFString('AXApplication', AXApplicationStr);
  CreateCFString('AXTabGroup', AXTabGroupStr);
  CreateCFString('AXFrame', AXFrameStr);
  CreateCFString('AXTitle', AXTitleStr);
  CreateCFString('AXMainWindow', AXMainWindowStr);
end;

procedure RequestAccessibility;
begin
  if not AXIsProcessTrusted() then
    AXIsProcessTrustedWithOptions(NSDictionary.dictionaryWithObjectsAndKeys(NSNumber.numberWithBool(True), id(kAXTrustedCheckOptionPrompt), nil));
end;

function GetElementBounds(Element: AXUIElementRef): TBox;
var
  Value: CFTypeRef;
  Rect: NSRect;
begin
  Result := TBox.ZERO;

  Value := nil;
  if (AXUIElementCopyAttributeValue(Element, AXFrameStr, Value) = kAXErrorSuccess) and (Value <> nil) then
  begin
    if AXValueGetValue(Value, kAXValueCGRectType, @Rect) then
      Result := TBox(NSRectToRect(Rect));

    CFRelease(Value);
  end;
end;

function AXUI_GetWindowTitle(Element: AXUIElementRef): String;
var
  MainWindowValue, TitleValue: CFTypeRef;
begin
  Result := '';

  TitleValue := nil;
  MainWindowValue := nil;

  if (AXUIElementCopyAttributeValue(Element, AXMainWindowStr, MainWindowValue) = kAXErrorSuccess) then
  begin
    if (AXUIElementCopyAttributeValue(MainWindowValue, AXTitleStr, TitleValue) = kAXErrorSuccess) then
      Result := CFStringToStr(TitleValue);

    CFRelease(TitleValue);
    CFRelease(MainWindowValue);
  end else
  if (AXUIElementCopyAttributeValue(Element, AXTitleStr, TitleValue) = kAXErrorSuccess) then
  begin
    Result := CFStringToStr(TitleValue);

    CFRelease(TitleValue);
  end;
end;

function AXUI_GetWindowTitle(PID: UInt32): String;
var
  Element: AXUIElementRef;
begin
  Element := AXUIElementCreateApplication(PID);
  if (Element <> nil) then
  begin
    Result := AXUI_GetWindowTitle(Element);

    CFRelease(Element);
  end;
end;

function AXUI_GetWindowClass(Element: AXUIElementRef): String;
var
  MainWindowValue, RoleValue: CFTypeRef;
begin
  Result := '';

  RoleValue := nil;
  MainWindowValue := nil;

  if (AXUIElementCopyAttributeValue(Element, AXMainWindowStr, MainWindowValue) = kAXErrorSuccess) then
  begin
    if (AXUIElementCopyAttributeValue(MainWindowValue, AXRoleStr, RoleValue) = kAXErrorSuccess) then
      Result := CFStringToStr(RoleValue);

    CFRelease(RoleValue);
    CFRelease(MainWindowValue);
  end else
  if (AXUIElementCopyAttributeValue(Element, AXRoleStr, RoleValue) = kAXErrorSuccess) then
  begin
    Result := CFStringToStr(RoleValue);

    CFRelease(RoleValue);
  end;
end;

function AXUI_GetWindowClass(PID: UInt32): String;
var
  Element: AXUIElementRef;
begin
  Element := AXUIElementCreateApplication(PID);
  if (Element <> nil) then
  begin
    Result := AXUI_GetWindowTitle(Element);

    CFRelease(Element);
  end;
end;

function AXUI_GetWindowInfo(PID: UInt32): TAXUIWindowInfo;

  procedure FindChildren(Element: AXUIElementRef);
  var
    I: Integer;
    Value: CFTypeRef;
    Children: CFArrayRef;
    ChildrenCount: CFIndex;
    B: TBox;
  begin
    Children := nil;
    Value := nil;

    try
      if (AXUIElementCopyAttributeValue(Element, AXRoleStr, Value) <> kAXErrorSuccess) then
        Exit;

      if CFEqual(Value, AXTabGroupStr)    or CFEqual(Value, AXListStr)   or CFEqual(Value, AXSplitGroup) or
         CFEqual(Value, AXApplicationStr) or CFEqual(Value, AXWindowStr) or CFEqual(Value, AXGroupStr)   or
         CFEqual(Value, AXScrollAreaStr) then
      begin
        B := GetElementBounds(Element);
        if (B.Width > 1) and (B.Height > 1) and (B.X1 > 0) and (B.Y1 > 0) then
        begin
          SetLength(Result.Children, Length(Result.Children) + 1);

          Result.Children[High(Result.Children)].Bounds := B;
          Result.Children[High(Result.Children)].ClassName := CFStringToStr(Value);
        end;

        Children := nil;
        ChildrenCount := 0;
        if (AXUIElementCopyAttributeValue(Element, AXChildrenStr, Children) = kAXErrorSuccess) then
          ChildrenCount := CFArrayGetCount(Children);
        if (ChildrenCount > 0) then
        begin
          if CFEqual(Value, AXGroupStr) or CFEqual(Value, AXScrollAreaStr) then
            Exit;

          for I := 0 to ChildrenCount - 1 do
            FindChildren(CFArrayGetValueAtIndex(Children, I));
        end;
      end;
    finally
      if (Children <> nil) then
        CFRelease(Children);
      if (Value <> nil) then
        CFRelease(Value);
    end;
  end;

var
  Element: AXUIElementRef;
begin
  Result := Default(TAXUIWindowInfo);

  Element := AXUIElementCreateApplication(PID);
  if (Element <> nil) then
  begin
    Result.PID := PID;
    Result.Title := AXUI_GetWindowTitle(Element);

    FindChildren(Element);

    CFRelease(Element);
  end;
end;

initialization
  //SimbaIDEInitialization_AddBeforeShow(@RequestAccessibility, 'RequestAccessibility');

  CreateCFStrings();

end.
