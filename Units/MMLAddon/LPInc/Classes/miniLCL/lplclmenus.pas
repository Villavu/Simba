unit lplclmenus;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper;

procedure RegisterLCLMenus(Compiler: TLapeCompiler);

implementation
   uses MufasaTypes, forms, lplclsystem, lplclgraphics, Menus, lclType, graphics;

type
  PMenuItem = ^TMenuItem;
  PMenu = ^TMenu;
  PMainMenu = ^TMainMenu;
  PTranslateString = ^TTranslateString;
  PBitmap = ^TBitmap;
  TMenuItemArray = array of TMenuItem;
  PMenuItemArray = ^TMenuItemArray;

procedure Register_TMenu_Forward(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TMenu', 'TLCLComponent');
  end;
end;

//Read: FCompStyle: LongInt;
procedure TMenuItem_FCompStyle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongInt(Result)^ := PMenuItem(Params^[0])^.FCompStyle;
end;

//Write: FCompStyle: LongInt;
procedure TMenuItem_FCompStyle_Write(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.FCompStyle := PLongInt(Params^[1])^;
end;

//function Find(const ACaption: string): TMenuItem;
procedure TMenuItem_Find(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMenuItem(Result)^ := PMenuItem(Params^[0])^.Find(PlpString(Params^[1])^);
end;

//function GetParentMenu: TMenu; virtual;
procedure TMenuItem_GetParentMenu(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMenu(Result)^ := PMenuItem(Params^[0])^.GetParentMenu();
end;

//function GetIsRightToLeft:Boolean; virtual;
procedure TMenuItem_GetIsRightToLeft(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.GetIsRightToLeft();
end;

//function HandleAllocated : Boolean;
procedure TMenuItem_HandleAllocated(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.HandleAllocated();
end;

//function HasIcon: boolean; virtual;
procedure TMenuItem_HasIcon(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.HasIcon();
end;

//procedure InitiateAction; virtual;
procedure TMenuItem_InitiateAction(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.InitiateAction();
end;

//procedure IntfDoSelect; virtual;
procedure TMenuItem_IntfDoSelect(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.IntfDoSelect();
end;

//function IndexOf(Item: TMenuItem): Integer;
procedure TMenuItem_IndexOf(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.IndexOf(PMenuItem(Params^[1])^);
end;

//function IndexOfCaption(const ACaption: string): Integer; virtual;
procedure TMenuItem_IndexOfCaption(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.IndexOfCaption(PlpString(Params^[1])^);
end;

//function VisibleIndexOf(Item: TMenuItem): Integer;
procedure TMenuItem_VisibleIndexOf(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.VisibleIndexOf(PMenuItem(Params^[1])^);
end;

//procedure Add(Item: TMenuItem);
procedure TMenuItem_Add(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.Add(PMenuItem(Params^[1])^);
end;

//procedure AddSeparator;
procedure TMenuItem_AddSeparator(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.AddSeparator();
end;

//procedure Click; virtual;
procedure TMenuItem_Click(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.Click();
end;

//procedure Delete(Index: Integer);
procedure TMenuItem_Delete(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

//procedure HandleNeeded; virtual;
procedure TMenuItem_HandleNeeded(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.HandleNeeded();
end;

//procedure Insert(Index: Integer; Item: TMenuItem);
procedure TMenuItem_Insert(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.Insert(PInteger(Params^[1])^, PMenuItem(Params^[2])^);
end;

//procedure RecreateHandle; virtual;
procedure TMenuItem_RecreateHandle(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.RecreateHandle();
end;

//procedure Remove(Item: TMenuItem);
procedure TMenuItem_Remove(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.Remove(PMenuItem(Params^[1])^);
end;

//function IsCheckItem: boolean; virtual;
procedure TMenuItem_IsCheckItem(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.IsCheckItem();
end;

//function IsLine: Boolean;
procedure TMenuItem_IsLine(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.IsLine();
end;

//function IsInMenuBar: boolean; virtual;
procedure TMenuItem_IsInMenuBar(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.IsInMenuBar();
end;

//procedure Clear;
procedure TMenuItem_Clear(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.Clear();
end;

//function HasBitmap: boolean;
procedure TMenuItem_HasBitmap(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.HasBitmap();
end;

//function GetIconSize: TPoint; virtual;
procedure TMenuItem_GetIconSize(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPoint(Result)^ := PMenuItem(Params^[0])^.GetIconSize();
end;

//procedure RemoveHandlerOnDestroy(const OnDestroyEvent: TNotifyEvent);
procedure TMenuItem_RemoveHandlerOnDestroy(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.RemoveHandlerOnDestroy(PNotifyEvent(Params^[1])^);
end;

//Read: property Count: Integer read GetCount;
procedure TMenuItem_Count_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.Count;
end;

//Read: property Items[Index: Integer]: TMenuItem read GetItem; default;
procedure TMenuItem_Items_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMenuItem(Result)^ := PMenuItem(Params^[0])^.Items[PInteger(Params^[1])^];
end;

//Read: property MenuIndex: Integer read GetMenuIndex write SetMenuIndex;
procedure TMenuItem_MenuIndex_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.MenuIndex;
end;

//Write: property MenuIndex: Integer read GetMenuIndex write SetMenuIndex;
procedure TMenuItem_MenuIndex_Write(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.MenuIndex := PInteger(Params^[1])^;
end;

//Read: property Menu: TMenu read FMenu;
procedure TMenuItem_Menu_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMenu(Result)^ := PMenuItem(Params^[0])^.Menu;
end;

//Read: property Parent: TMenuItem read GetParent;
procedure TMenuItem_Parent_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMenuItem(Result)^ := PMenuItem(Params^[0])^.Parent;
end;

//Read: property Command: Word read FCommand;
procedure TMenuItem_Command_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PWord(Result)^ := PMenuItem(Params^[0])^.Command;
end;

//function MenuVisibleIndex: integer;
procedure TMenuItem_MenuVisibleIndex(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PMenuItem(Params^[0])^.MenuVisibleIndex();
end;

//Read: property AutoCheck: boolean read FAutoCheck write SetAutoCheck default False;
procedure TMenuItem_AutoCheck_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.AutoCheck;
end;

//Write: property AutoCheck: boolean read FAutoCheck write SetAutoCheck default False;
procedure TMenuItem_AutoCheck_Write(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.AutoCheck := Pboolean(Params^[1])^;
end;

//Read: property Default: Boolean read FDefault write SetDefault default False;
procedure TMenuItem_Default_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.Default;
end;

//Write: property Default: Boolean read FDefault write SetDefault default False;
procedure TMenuItem_Default_Write(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.Default := PBoolean(Params^[1])^;
end;

//Read: property Bitmap: TBitmap read GetBitmap write SetBitmap stored IsBitmapStored;
procedure TMenuItem_Bitmap_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBitmap(Result)^ := PMenuItem(Params^[0])^.Bitmap;
end;

//Write: property Bitmap: TBitmap read GetBitmap write SetBitmap stored IsBitmapStored;
procedure TMenuItem_Bitmap_Write(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.Bitmap := PBitmap(Params^[1])^;
end;

//Read: property GroupIndex: Byte read FGroupIndex write SetGroupIndex default 0;
procedure TMenuItem_GroupIndex_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PByte(Result)^ := PMenuItem(Params^[0])^.GroupIndex;
end;

//Write: property GroupIndex: Byte read FGroupIndex write SetGroupIndex default 0;
procedure TMenuItem_GroupIndex_Write(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.GroupIndex := PByte(Params^[1])^;
end;

//Read: property Hint: TTranslateString read FHint write FHint stored IsHintStored;
procedure TMenuItem_Hint_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PTranslateString(Result)^ := PMenuItem(Params^[0])^.Hint;
end;

//Write: property Hint: TTranslateString read FHint write FHint stored IsHintStored;
procedure TMenuItem_Hint_Write(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.Hint := PTranslateString(Params^[1])^;
end;

//Read: property RadioItem: Boolean read FRadioItem write SetRadioItem default False;
procedure TMenuItem_RadioItem_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.RadioItem;
end;

//Write: property RadioItem: Boolean read FRadioItem write SetRadioItem default False;
procedure TMenuItem_RadioItem_Write(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.RadioItem := PBoolean(Params^[1])^;
end;

//Read: property RightJustify: boolean read FRightJustify write SetRightJustify default False;
procedure TMenuItem_RightJustify_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.RightJustify;
end;

//Write: property RightJustify: boolean read FRightJustify write SetRightJustify default False;
procedure TMenuItem_RightJustify_Write(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.RightJustify := Pboolean(Params^[1])^;
end;

procedure TMenuItem_OnClick_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
var
  Component: TComponent;
begin
  Component := PMenuItem(Params^[0])^.FindComponent('OnClickEvent');

  if (Assigned(Component)) then
    PClickWrapper(Result)^ := TOnClickWrapper(Component).InternalMethod
  else
    PClickWrapper(Result)^ := nil;
end;

procedure TMenuItem_OnClick_Write(const Params: PParamArray); lape_extdecl
var
  Component: TComponent;
begin
  Component := PMenuItem(Params^[0])^.FindComponent('OnClickEvent');
  if (not Assigned(Component)) then
  begin
    Component := TOnClickWrapper.Create(PMenuItem(Params^[0])^);
    Component.Name := 'OnClickEvent';
  end;

  with TOnClickWrapper(Component) do
  begin
    InternalMethod := PClickWrapper(Params^[1])^;
    PMenuItem(Params^[0])^.OnClick := @OnClick;
  end;
end;


//constructor Create();
procedure TMenuItem_Init(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^ := TMenuItem.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TMenuItem_Free(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.Free();
end;

procedure TMenuItem_Caption_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PMenuItem(Params^[0])^.Caption;
end;

procedure TMenuItem_Caption_Write(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.Caption := PTranslateString(Params^[1])^;
end;

procedure TMenuItem_Checked_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.Checked;
end;

procedure TMenuItem_Checked_Write(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.Checked := PBoolean(Params^[1])^;
end;

procedure TMenuItem_AddEx(const Params: PParamArray); lape_extdecl
begin
  PMenuItem(Params^[0])^.Add(PMenuItemArray(Params^[1])^);
end;

procedure TMenuItem_AddMenu(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMenuItem(Result)^ := TMenuItem.Create(PMenu(Params^[0])^);
  PMenuItem(Result)^.Caption := PLPString(Params^[1])^;
  PMenuItem(Params^[0])^.Add(PMenuItem(Result)^);
end;

procedure Register_TMenuItem(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TMenuItem', 'TLCLComponent');

    addClassVar('TMenuItem', 'FCompStyle', 'LongInt', @TMenuItem_FCompStyle_Read, @TMenuItem_FCompStyle_Write);
    addGlobalFunc('function TMenuItem.Find(const ACaption: string): TMenuItem;', @TMenuItem_Find);
    addGlobalFunc('function TMenuItem.GetParentMenu(): TMenu;', @TMenuItem_GetParentMenu);
    addGlobalFunc('function TMenuItem.GetIsRightToLeft(): Boolean;', @TMenuItem_GetIsRightToLeft);
    addGlobalFunc('function TMenuItem.HandleAllocated(): Boolean;', @TMenuItem_HandleAllocated);
    addGlobalFunc('function TMenuItem.HasIcon(): boolean;', @TMenuItem_HasIcon);
    addGlobalFunc('procedure TMenuItem.InitiateAction();', @TMenuItem_InitiateAction);
    addGlobalFunc('procedure TMenuItem.IntfDoSelect();', @TMenuItem_IntfDoSelect);
    addGlobalFunc('function TMenuItem.IndexOf(Item: TMenuItem): Integer;', @TMenuItem_IndexOf);
    addGlobalFunc('function TMenuItem.IndexOfCaption(const ACaption: string): Integer;', @TMenuItem_IndexOfCaption);
    addGlobalFunc('function TMenuItem.VisibleIndexOf(Item: TMenuItem): Integer;', @TMenuItem_VisibleIndexOf);
    addGlobalFunc('procedure TMenuItem.Add(Item: TMenuItem);', @TMenuItem_Add);
    addGlobalFunc('procedure TMenuItem.AddEx(Items: array of TMenuItem);', @TMenuItem_AddEx);
    addGlobalFunc('procedure TMenuItem.AddSeparator();', @TMenuItem_AddSeparator);
    addGlobalFunc('procedure TMenuItem.Click();', @TMenuItem_Click);
    addGlobalFunc('procedure TMenuItem.Delete(Index: Integer);', @TMenuItem_Delete);
    addGlobalFunc('procedure TMenuItem.HandleNeeded();', @TMenuItem_HandleNeeded);
    addGlobalFunc('procedure TMenuItem.Insert(Index: Integer; Item: TMenuItem);', @TMenuItem_Insert);
    addGlobalFunc('procedure TMenuItem.RecreateHandle();', @TMenuItem_RecreateHandle);
    addGlobalFunc('procedure TMenuItem.Remove(Item: TMenuItem);', @TMenuItem_Remove);
    addGlobalFunc('function TMenuItem.IsCheckItem(): boolean;', @TMenuItem_IsCheckItem);
    addGlobalFunc('function TMenuItem.IsLine(): Boolean;', @TMenuItem_IsLine);
    addGlobalFunc('function TMenuItem.IsInMenuBar(): boolean;', @TMenuItem_IsInMenuBar);
    addGlobalFunc('procedure TMenuItem.Clear();', @TMenuItem_Clear);
    addGlobalFunc('function TMenuItem.HasBitmap(): boolean;', @TMenuItem_HasBitmap);
    addGlobalFunc('function TMenuItem.GetIconSize(): TPoint;', @TMenuItem_GetIconSize);
    addGlobalFunc('function TMenuItem.AddMenu(s: string): TMenuItem;', @TMenuItem_AddMenu);
    addClassVar('TMenuItem', 'Count', 'Integer', @TMenuItem_Count_Read, nil);
    addClassVar('TMenuItem', 'Items', 'TMenuItem', @TMenuItem_Items_Read, nil);
    addClassVar('TMenuItem', 'Hint', 'String', @TMenuItem_Hint_Read, @TMenuItem_Hint_Write);
    addClassVar('TMenuItem', 'Checked', 'Boolean', @TMenuItem_Checked_Read, @TMenuItem_Checked_Write);
    addClassVar('TMenuItem', 'MenuIndex', 'Integer', @TMenuItem_MenuIndex_Read, @TMenuItem_MenuIndex_Write);
    addClassVar('TMenuItem', 'Menu', 'TMenu', @TMenuItem_Menu_Read, nil);
    addClassVar('TMenuItem', 'Parent', 'TMenuItem', @TMenuItem_Parent_Read, nil);
    addClassVar('TMenuItem', 'Command', 'Word', @TMenuItem_Command_Read, nil);
    addClassVar('TMenuItem', 'AutoCheck', 'boolean', @TMenuItem_AutoCheck_Read, @TMenuItem_AutoCheck_Write);
    addClassVar('TMenuItem', 'Default', 'Boolean', @TMenuItem_Default_Read, @TMenuItem_Default_Write);
    addClassVar('TMenuItem', 'Bitmap', 'TBitmap', @TMenuItem_Bitmap_Read, @TMenuItem_Bitmap_Write);
    addClassVar('TMenuItem', 'GroupIndex', 'Byte', @TMenuItem_GroupIndex_Read, @TMenuItem_GroupIndex_Write);
    addClassVar('TMenuItem', 'RadioItem', 'Boolean', @TMenuItem_RadioItem_Read, @TMenuItem_RadioItem_Write);
    addClassVar('TMenuItem', 'RightJustify', 'boolean', @TMenuItem_RightJustify_Read, @TMenuItem_RightJustify_Write);
    addClassVar('TMenuItem', 'OnClick', 'TNotifyEvent', @TMenuItem_OnClick_Read, @TMenuItem_OnClick_Write);
    addClassVar('TMenuItem', 'Caption', 'String', @TMenuItem_Caption_Read, @TMenuItem_Caption_Write);
    addGlobalFunc('procedure TMenuItem.Init(AOwner: TComponent);', @TMenuItem_Init);
    addGlobalFunc('procedure TMenuItem.Free();', @TMenuItem_Free);
  end;
end;

//Read: FCompStyle: LongInt;
procedure TMenu_FCompStyle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongInt(Result)^ := PMenu(Params^[0])^.FCompStyle;
end;

//Write: FCompStyle: LongInt;
procedure TMenu_FCompStyle_Write(const Params: PParamArray); lape_extdecl
begin
  PMenu(Params^[0])^.FCompStyle := PLongInt(Params^[1])^;
end;

//procedure DestroyHandle; virtual;
procedure TMenu_DestroyHandle(const Params: PParamArray); lape_extdecl
begin
  PMenu(Params^[0])^.DestroyHandle();
end;

//function HandleAllocated: Boolean;
procedure TMenu_HandleAllocated(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.HandleAllocated();
end;

//function IsRightToLeft: Boolean; virtual;
procedure TMenu_IsRightToLeft(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.IsRightToLeft();
end;

//function UseRightToLeftAlignment: Boolean; virtual;
procedure TMenu_UseRightToLeftAlignment(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.UseRightToLeftAlignment();
end;

//function UseRightToLeftReading: Boolean; virtual;
procedure TMenu_UseRightToLeftReading(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.UseRightToLeftReading();
end;

//procedure HandleNeeded;
procedure TMenu_HandleNeeded(const Params: PParamArray); lape_extdecl
begin
  PMenu(Params^[0])^.HandleNeeded();
end;

//function DispatchCommand(ACommand: Word): Boolean;
procedure TMenu_DispatchCommand(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.DispatchCommand(PWord(Params^[1])^);
end;

//Read: property Parent: TComponent read FParent write SetParent;
procedure TMenu_Parent_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PComponent(Result)^ := PMenu(Params^[0])^.Parent;
end;

//Write: property Parent: TComponent read FParent write SetParent;
procedure TMenu_Parent_Write(const Params: PParamArray); lape_extdecl
begin
  PMenu(Params^[0])^.Parent := PComponent(Params^[1])^;
end;

//Read: property ShortcutHandled: boolean read FShortcutHandled write FShortcutHandled;
procedure TMenu_ShortcutHandled_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PMenu(Params^[0])^.ShortcutHandled;
end;

//Write: property ShortcutHandled: boolean read FShortcutHandled write FShortcutHandled;
procedure TMenu_ShortcutHandled_Write(const Params: PParamArray); lape_extdecl
begin
  PMenu(Params^[0])^.ShortcutHandled := Pboolean(Params^[1])^;
end;

//Read: property ParentBidiMode:Boolean read FParentBidiMode write SetParentBidiMode default True;
procedure TMenu_ParentBidiMode_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.ParentBidiMode;
end;

//Write: property ParentBidiMode:Boolean read FParentBidiMode write SetParentBidiMode default True;
procedure TMenu_ParentBidiMode_Write(const Params: PParamArray); lape_extdecl
begin
  PMenu(Params^[0])^.ParentBidiMode := PBoolean(Params^[1])^;
end;

//Read: property Items: TMenuItem read FItems;
procedure TMenu_Items_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMenuItem(Result)^ := PMenu(Params^[0])^.Items;
end;

//constructor Create();
procedure TMenu_Init(const Params: PParamArray); lape_extdecl
begin
  PMenu(Params^[0])^ := TMenu.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TMenu_Free(const Params: PParamArray); lape_extdecl
begin
  PMenu(Params^[0])^.Free();
end;

procedure TMenu_AddMenu(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMenuItem(Result)^ := TMenuItem.Create(PMenu(Params^[0])^);
  PMenuItem(Result)^.Caption := PLPString(Params^[1])^;
  PMenu(Params^[0])^.Items.Add(PMenuItem(Result)^);
end;

procedure Register_TMenu(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClassVar('TMenu', 'FCompStyle', 'LongInt', @TMenu_FCompStyle_Read, @TMenu_FCompStyle_Write);
    addGlobalFunc('procedure TMenu.DestroyHandle();', @TMenu_DestroyHandle);
    addGlobalFunc('function TMenu.HandleAllocated(): Boolean;', @TMenu_HandleAllocated);
    addGlobalFunc('function TMenu.IsRightToLeft(): Boolean;', @TMenu_IsRightToLeft);
    addGlobalFunc('function TMenu.UseRightToLeftAlignment(): Boolean;', @TMenu_UseRightToLeftAlignment);
    addGlobalFunc('function TMenu.UseRightToLeftReading(): Boolean;', @TMenu_UseRightToLeftReading);
    addGlobalFunc('procedure TMenu.HandleNeeded();', @TMenu_HandleNeeded);
    addGlobalFunc('function TMenu.DispatchCommand(ACommand: Word): Boolean;', @TMenu_DispatchCommand);
    addGlobalFunc('function TMenu.AddMenu(name: string): TMenuItem;', @TMenu_AddMenu);
    addClassVar('TMenu', 'Parent', 'TComponent', @TMenu_Parent_Read, @TMenu_Parent_Write);
    addClassVar('TMenu', 'ShortcutHandled', 'boolean', @TMenu_ShortcutHandled_Read, @TMenu_ShortcutHandled_Write);
    addClassVar('TMenu', 'Items', 'TMenuItem', @TMenu_Items_Read, nil);
    addGlobalFunc('procedure TMenu.Init(AOwner: TComponent);', @TMenu_Init);
    addGlobalFunc('procedure TMenu.Free();', @TMenu_Free);
  end;
end;

//constructor Create();
procedure TMainMenu_Init(const Params: PParamArray); lape_extdecl
begin
  PMainMenu(Params^[0])^ := TMainMenu.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TMainMenu_Free(const Params: PParamArray); lape_extdecl
begin
  PMainMenu(Params^[0])^.Free();
end;

procedure Register_TMainMenu(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TMainMenu', 'TMenu');

    addGlobalFunc('procedure TMainMenu.Init(AOwner: TComponent);', @TMainMenu_Init);
    addGlobalFunc('procedure TMainMenu.Free();', @TMainMenu_Free);
  end;
end;


procedure RegisterLCLMenus(Compiler: TLapeCompiler);
begin
  Register_TMenu_Forward(Compiler);
  Register_TMenuItem(Compiler);
  Register_TMenu(Compiler);
  Register_TMainMenu(Compiler);
end;


end.

