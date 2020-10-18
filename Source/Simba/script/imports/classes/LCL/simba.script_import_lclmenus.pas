unit simba.script_import_lclmenus;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_LCLMenus(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  forms, menus, lcltype, graphics;

type
  PComponent = ^TComponent;
  PNotifyEvent = ^TNotifyEvent;
  PMenuItem = ^TMenuItem;
  PMenu = ^TMenu;
  PMainMenu = ^TMainMenu;
  PTranslateString = ^TTranslateString;
  PBitmap = ^TBitmap;
  TMenuItemArray = array of TMenuItem;
  PMenuItemArray = ^TMenuItemArray;

procedure Lape_Import_Lape_TMenu_Forward(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TMenu', 'TLCLComponent');
  end;
end;

//Read: FCompStyle: LongInt;
procedure Lape_TMenuItem_FCompStyle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongInt(Result)^ := PMenuItem(Params^[0])^.FCompStyle;
end;

//Write: FCompStyle: LongInt;
procedure Lape_TMenuItem_FCompStyle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.FCompStyle := PLongInt(Params^[1])^;
end;

//function Find(const ACaption: string): TMenuItem;
procedure Lape_TMenuItem_Find(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := PMenuItem(Params^[0])^.Find(PlpString(Params^[1])^);
end;

//function GetParentMenu: TMenu; virtual;
procedure Lape_TMenuItem_GetParentMenu(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenu(Result)^ := PMenuItem(Params^[0])^.GetParentMenu();
end;

//function GetIsRightToLeft:Boolean; virtual;
procedure Lape_TMenuItem_GetIsRightToLeft(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.GetIsRightToLeft();
end;

//function HandleAllocated : Boolean;
procedure Lape_TMenuItem_HandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.HandleAllocated();
end;

//function HasIcon: boolean; virtual;
procedure Lape_TMenuItem_HasIcon(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.HasIcon();
end;

//procedure InitiateAction; virtual;
procedure Lape_TMenuItem_InitiateAction(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.InitiateAction();
end;

//procedure IntfDoSelect; virtual;
procedure Lape_TMenuItem_IntfDoSelect(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.IntfDoSelect();
end;

//function IndexOf(Item: TMenuItem): Integer;
procedure Lape_TMenuItem_IndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.IndexOf(PMenuItem(Params^[1])^);
end;

//function IndexOfCaption(const ACaption: string): Integer; virtual;
procedure Lape_TMenuItem_IndexOfCaption(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.IndexOfCaption(PlpString(Params^[1])^);
end;

//function VisibleIndexOf(Item: TMenuItem): Integer;
procedure Lape_TMenuItem_VisibleIndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.VisibleIndexOf(PMenuItem(Params^[1])^);
end;

//procedure Add(Item: TMenuItem);
procedure Lape_TMenuItem_Add(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Add(PMenuItem(Params^[1])^);
end;

//procedure AddSeparator;
procedure Lape_TMenuItem_AddSeparator(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.AddSeparator();
end;

//procedure Click; virtual;
procedure Lape_TMenuItem_Click(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Click();
end;

//procedure Delete(Index: Integer);
procedure Lape_TMenuItem_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

//procedure HandleNeeded; virtual;
procedure Lape_TMenuItem_HandleNeeded(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.HandleNeeded();
end;

//procedure Insert(Index: Integer; Item: TMenuItem);
procedure Lape_TMenuItem_Insert(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Insert(PInteger(Params^[1])^, PMenuItem(Params^[2])^);
end;

//procedure RecreateHandle; virtual;
procedure Lape_TMenuItem_RecreateHandle(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.RecreateHandle();
end;

//procedure Remove(Item: TMenuItem);
procedure Lape_TMenuItem_Remove(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Remove(PMenuItem(Params^[1])^);
end;

//function IsCheckItem: boolean; virtual;
procedure Lape_TMenuItem_IsCheckItem(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.IsCheckItem();
end;

//function IsLine: Boolean;
procedure Lape_TMenuItem_IsLine(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.IsLine();
end;

//function IsInMenuBar: boolean; virtual;
procedure Lape_TMenuItem_IsInMenuBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.IsInMenuBar();
end;

//procedure Clear;
procedure Lape_TMenuItem_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Clear();
end;

//function HasBitmap: boolean;
procedure Lape_TMenuItem_HasBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.HasBitmap();
end;

//procedure RemoveHandlerOnDestroy(const OnDestroyEvent: TNotifyEvent);
procedure Lape_TMenuItem_RemoveHandlerOnDestroy(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.RemoveHandlerOnDestroy(PNotifyEvent(Params^[1])^);
end;

//Read: property Count: Integer read GetCount;
procedure Lape_TMenuItem_Count_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.Count;
end;

//Read: property Items[Index: Integer]: TMenuItem read GetItem; default;
procedure Lape_TMenuItem_Items_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := PMenuItem(Params^[0])^.Items[PInteger(Params^[1])^];
end;

//Read: property MenuIndex: Integer read GetMenuIndex write SetMenuIndex;
procedure Lape_TMenuItem_MenuIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.MenuIndex;
end;

//Write: property MenuIndex: Integer read GetMenuIndex write SetMenuIndex;
procedure Lape_TMenuItem_MenuIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.MenuIndex := PInteger(Params^[1])^;
end;

//Read: property Menu: TMenu read FMenu;
procedure Lape_TMenuItem_Menu_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenu(Result)^ := PMenuItem(Params^[0])^.Menu;
end;

//Read: property Parent: TMenuItem read GetParent;
procedure Lape_TMenuItem_Parent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := PMenuItem(Params^[0])^.Parent;
end;

//Read: property Command: Word read FCommand;
procedure Lape_TMenuItem_Command_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWord(Result)^ := PMenuItem(Params^[0])^.Command;
end;

//function MenuVisibleIndex: integer;
procedure Lape_TMenuItem_MenuVisibleIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMenuItem(Params^[0])^.MenuVisibleIndex();
end;

//Read: property AutoCheck: boolean read FAutoCheck write SetAutoCheck default False;
procedure Lape_TMenuItem_AutoCheck_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.AutoCheck;
end;

//Write: property AutoCheck: boolean read FAutoCheck write SetAutoCheck default False;
procedure Lape_TMenuItem_AutoCheck_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.AutoCheck := Pboolean(Params^[1])^;
end;

//Read: property Default: Boolean read FDefault write SetDefault default False;
procedure Lape_TMenuItem_Default_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.Default;
end;

//Write: property Default: Boolean read FDefault write SetDefault default False;
procedure Lape_TMenuItem_Default_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Default := PBoolean(Params^[1])^;
end;

//Read: property Bitmap: TBitmap read GetBitmap write SetBitmap stored IsBitmapStored;
procedure Lape_TMenuItem_Bitmap_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBitmap(Result)^ := PMenuItem(Params^[0])^.Bitmap;
end;

//Write: property Bitmap: TBitmap read GetBitmap write SetBitmap stored IsBitmapStored;
procedure Lape_TMenuItem_Bitmap_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Bitmap := PBitmap(Params^[1])^;
end;

//Read: property GroupIndex: Byte read FGroupIndex write SetGroupIndex default 0;
procedure Lape_TMenuItem_GroupIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PByte(Result)^ := PMenuItem(Params^[0])^.GroupIndex;
end;

//Write: property GroupIndex: Byte read FGroupIndex write SetGroupIndex default 0;
procedure Lape_TMenuItem_GroupIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.GroupIndex := PByte(Params^[1])^;
end;

//Read: property Hint: TTranslateString read FHint write FHint stored IsHintStored;
procedure Lape_TMenuItem_Hint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PTranslateString(Result)^ := PMenuItem(Params^[0])^.Hint;
end;

//Write: property Hint: TTranslateString read FHint write FHint stored IsHintStored;
procedure Lape_TMenuItem_Hint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Hint := PTranslateString(Params^[1])^;
end;

//Read: property RadioItem: Boolean read FRadioItem write SetRadioItem default False;
procedure Lape_TMenuItem_RadioItem_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.RadioItem;
end;

//Write: property RadioItem: Boolean read FRadioItem write SetRadioItem default False;
procedure Lape_TMenuItem_RadioItem_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.RadioItem := PBoolean(Params^[1])^;
end;

//Read: property RightJustify: boolean read FRightJustify write SetRightJustify default False;
procedure Lape_TMenuItem_RightJustify_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.RightJustify;
end;

//Write: property RightJustify: boolean read FRightJustify write SetRightJustify default False;
procedure Lape_TMenuItem_RightJustify_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.RightJustify := Pboolean(Params^[1])^;
end;

procedure Lape_TMenuItem_OnClick_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PMenuItem(Params^[0])^.OnClick;
end;

procedure Lape_TMenuItem_OnClick_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.OnClick := PNotifyEvent(Params^[1])^;
end;


//constructor Create();
procedure Lape_TMenuItem_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^ := TMenuItem.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TMenuItem_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Free();
end;

procedure Lape_TMenuItem_Caption_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMenuItem(Params^[0])^.Caption;
end;

procedure Lape_TMenuItem_Caption_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Caption := PTranslateString(Params^[1])^;
end;

procedure Lape_TMenuItem_Checked_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.Checked;
end;

procedure Lape_TMenuItem_Checked_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Checked := PBoolean(Params^[1])^;
end;

procedure Lape_TMenuItem_AddEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Add(PMenuItemArray(Params^[1])^);
end;

procedure Lape_TMenuItem_AddMenu(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := TMenuItem.Create(PMenu(Params^[0])^);
  PMenuItem(Result)^.Caption := PLPString(Params^[1])^;
  PMenuItem(Params^[0])^.Add(PMenuItem(Result)^);
end;

procedure Lape_Import_TMenuItem(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TMenuItem', 'TLCLComponent');

    addClassVar('TMenuItem', 'FCompStyle', 'LongInt', @Lape_TMenuItem_FCompStyle_Read, @Lape_TMenuItem_FCompStyle_Write);
    addGlobalFunc('function TMenuItem.Find(const ACaption: string): TMenuItem; constref;', @Lape_TMenuItem_Find);
    addGlobalFunc('function TMenuItem.GetParentMenu(): TMenu; constref;', @Lape_TMenuItem_GetParentMenu);
    addGlobalFunc('function TMenuItem.GetIsRightToLeft(): Boolean; constref;', @Lape_TMenuItem_GetIsRightToLeft);
    addGlobalFunc('function TMenuItem.HandleAllocated(): Boolean; constref;', @Lape_TMenuItem_HandleAllocated);
    addGlobalFunc('function TMenuItem.HasIcon(): boolean; constref;', @Lape_TMenuItem_HasIcon);
    addGlobalFunc('procedure TMenuItem.InitiateAction(); constref;', @Lape_TMenuItem_InitiateAction);
    addGlobalFunc('procedure TMenuItem.IntfDoSelect(); constref;', @Lape_TMenuItem_IntfDoSelect);
    addGlobalFunc('function TMenuItem.IndexOf(Item: TMenuItem): Integer; constref;', @Lape_TMenuItem_IndexOf);
    addGlobalFunc('function TMenuItem.IndexOfCaption(const ACaption: string): Integer; constref;', @Lape_TMenuItem_IndexOfCaption);
    addGlobalFunc('function TMenuItem.VisibleIndexOf(Item: TMenuItem): Integer; constref;', @Lape_TMenuItem_VisibleIndexOf);
    addGlobalFunc('procedure TMenuItem.Add(Item: TMenuItem); constref;', @Lape_TMenuItem_Add);
    addGlobalFunc('procedure TMenuItem.AddEx(Items: array of TMenuItem); constref;', @Lape_TMenuItem_AddEx);
    addGlobalFunc('procedure TMenuItem.AddSeparator(); constref;', @Lape_TMenuItem_AddSeparator);
    addGlobalFunc('procedure TMenuItem.Click(); constref;', @Lape_TMenuItem_Click);
    addGlobalFunc('procedure TMenuItem.Delete(Index: Integer); constref;', @Lape_TMenuItem_Delete);
    addGlobalFunc('procedure TMenuItem.HandleNeeded(); constref;', @Lape_TMenuItem_HandleNeeded);
    addGlobalFunc('procedure TMenuItem.Insert(Index: Integer; Item: TMenuItem); constref;', @Lape_TMenuItem_Insert);
    addGlobalFunc('procedure TMenuItem.RecreateHandle(); constref;', @Lape_TMenuItem_RecreateHandle);
    addGlobalFunc('procedure TMenuItem.Remove(Item: TMenuItem); constref;', @Lape_TMenuItem_Remove);
    addGlobalFunc('function TMenuItem.IsCheckItem(): boolean; constref;', @Lape_TMenuItem_IsCheckItem);
    addGlobalFunc('function TMenuItem.IsLine(): Boolean; constref;', @Lape_TMenuItem_IsLine);
    addGlobalFunc('function TMenuItem.IsInMenuBar(): boolean; constref;', @Lape_TMenuItem_IsInMenuBar);
    addGlobalFunc('procedure TMenuItem.Clear(); constref;', @Lape_TMenuItem_Clear);
    addGlobalFunc('function TMenuItem.HasBitmap(): boolean; constref;', @Lape_TMenuItem_HasBitmap);
    addGlobalFunc('function TMenuItem.AddMenu(s: string): TMenuItem; constref;', @Lape_TMenuItem_AddMenu);
    addClassVar('TMenuItem', 'Count', 'Integer', @Lape_TMenuItem_Count_Read, nil);
    addClassVar('TMenuItem', 'Items', 'TMenuItem', @Lape_TMenuItem_Items_Read, nil);
    addClassVar('TMenuItem', 'Hint', 'String', @Lape_TMenuItem_Hint_Read, @Lape_TMenuItem_Hint_Write);
    addClassVar('TMenuItem', 'Checked', 'Boolean', @Lape_TMenuItem_Checked_Read, @Lape_TMenuItem_Checked_Write);
    addClassVar('TMenuItem', 'MenuIndex', 'Integer', @Lape_TMenuItem_MenuIndex_Read, @Lape_TMenuItem_MenuIndex_Write);
    addClassVar('TMenuItem', 'Menu', 'TMenu', @Lape_TMenuItem_Menu_Read, nil);
    addClassVar('TMenuItem', 'Parent', 'TMenuItem', @Lape_TMenuItem_Parent_Read, nil);
    addClassVar('TMenuItem', 'Command', 'Word', @Lape_TMenuItem_Command_Read, nil);
    addClassVar('TMenuItem', 'AutoCheck', 'boolean', @Lape_TMenuItem_AutoCheck_Read, @Lape_TMenuItem_AutoCheck_Write);
    addClassVar('TMenuItem', 'Default', 'Boolean', @Lape_TMenuItem_Default_Read, @Lape_TMenuItem_Default_Write);
    addClassVar('TMenuItem', 'Bitmap', 'TBitmap', @Lape_TMenuItem_Bitmap_Read, @Lape_TMenuItem_Bitmap_Write);
    addClassVar('TMenuItem', 'GroupIndex', 'Byte', @Lape_TMenuItem_GroupIndex_Read, @Lape_TMenuItem_GroupIndex_Write);
    addClassVar('TMenuItem', 'RadioItem', 'Boolean', @Lape_TMenuItem_RadioItem_Read, @Lape_TMenuItem_RadioItem_Write);
    addClassVar('TMenuItem', 'RightJustify', 'boolean', @Lape_TMenuItem_RightJustify_Read, @Lape_TMenuItem_RightJustify_Write);
    addClassVar('TMenuItem', 'OnClick', 'TNotifyEvent', @Lape_TMenuItem_OnClick_Read, @Lape_TMenuItem_OnClick_Write);
    addClassVar('TMenuItem', 'Caption', 'String', @Lape_TMenuItem_Caption_Read, @Lape_TMenuItem_Caption_Write);
    addGlobalFunc('procedure TMenuItem.Init(AOwner: TComponent); override;', @Lape_TMenuItem_Init);
   // addGlobalFunc('procedure TMenuItem.Free(); constref;', @Lape_TMenuItem_Free);
  end;
end;

//Read: FCompStyle: LongInt;
procedure Lape_TMenu_FCompStyle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongInt(Result)^ := PMenu(Params^[0])^.FCompStyle;
end;

//Write: FCompStyle: LongInt;
procedure Lape_TMenu_FCompStyle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.FCompStyle := PLongInt(Params^[1])^;
end;

//procedure DestroyHandle; virtual;
procedure Lape_TMenu_DestroyHandle(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.DestroyHandle();
end;

//function HandleAllocated: Boolean;
procedure Lape_TMenu_HandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.HandleAllocated();
end;

//function IsRightToLeft: Boolean; virtual;
procedure Lape_TMenu_IsRightToLeft(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.IsRightToLeft();
end;

//function UseRightToLeftAlignment: Boolean; virtual;
procedure Lape_TMenu_UseRightToLeftAlignment(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.UseRightToLeftAlignment();
end;

//function UseRightToLeftReading: Boolean; virtual;
procedure Lape_TMenu_UseRightToLeftReading(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.UseRightToLeftReading();
end;

//procedure HandleNeeded;
procedure Lape_TMenu_HandleNeeded(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.HandleNeeded();
end;

//function DispatchCommand(ACommand: Word): Boolean;
procedure Lape_TMenu_DispatchCommand(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.DispatchCommand(PWord(Params^[1])^);
end;

//Read: property Parent: TComponent read FParent write SetParent;
procedure Lape_TMenu_Parent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComponent(Result)^ := PMenu(Params^[0])^.Parent;
end;

//Write: property Parent: TComponent read FParent write SetParent;
procedure Lape_TMenu_Parent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.Parent := PComponent(Params^[1])^;
end;

//Read: property ShortcutHandled: boolean read FShortcutHandled write FShortcutHandled;
procedure Lape_TMenu_ShortcutHandled_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenu(Params^[0])^.ShortcutHandled;
end;

//Write: property ShortcutHandled: boolean read FShortcutHandled write FShortcutHandled;
procedure Lape_TMenu_ShortcutHandled_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.ShortcutHandled := Pboolean(Params^[1])^;
end;

//Read: property ParentBidiMode:Boolean read FParentBidiMode write SetParentBidiMode default True;
procedure Lape_TMenu_ParentBidiMode_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.ParentBidiMode;
end;

//Write: property ParentBidiMode:Boolean read FParentBidiMode write SetParentBidiMode default True;
procedure Lape_TMenu_ParentBidiMode_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.ParentBidiMode := PBoolean(Params^[1])^;
end;

//Read: property Items: TMenuItem read FItems;
procedure Lape_TMenu_Items_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := PMenu(Params^[0])^.Items;
end;

//constructor Create();
procedure Lape_TMenu_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^ := TMenu.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TMenu_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.Free();
end;

procedure Lape_TMenu_AddMenu(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := TMenuItem.Create(PMenu(Params^[0])^);
  PMenuItem(Result)^.Caption := PLPString(Params^[1])^;
  PMenu(Params^[0])^.Items.Add(PMenuItem(Result)^);
end;

procedure Lape_Import_TMenu(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClassVar('TMenu', 'FCompStyle', 'LongInt', @Lape_TMenu_FCompStyle_Read, @Lape_TMenu_FCompStyle_Write);
    addGlobalFunc('procedure TMenu.DestroyHandle(); constref;', @Lape_TMenu_DestroyHandle);
    addGlobalFunc('function TMenu.HandleAllocated(): Boolean; constref;', @Lape_TMenu_HandleAllocated);
    addGlobalFunc('function TMenu.IsRightToLeft(): Boolean; constref;', @Lape_TMenu_IsRightToLeft);
    addGlobalFunc('function TMenu.UseRightToLeftAlignment(): Boolean; constref;', @Lape_TMenu_UseRightToLeftAlignment);
    addGlobalFunc('function TMenu.UseRightToLeftReading(): Boolean; constref;', @Lape_TMenu_UseRightToLeftReading);
    addGlobalFunc('procedure TMenu.HandleNeeded(); constref;', @Lape_TMenu_HandleNeeded);
    addGlobalFunc('function TMenu.DispatchCommand(ACommand: Word): Boolean; constref;', @Lape_TMenu_DispatchCommand);
    addGlobalFunc('function TMenu.AddMenu(name: string): TMenuItem; constref;', @Lape_TMenu_AddMenu);
    addClassVar('TMenu', 'Parent', 'TComponent', @Lape_TMenu_Parent_Read, @Lape_TMenu_Parent_Write);
    addClassVar('TMenu', 'ShortcutHandled', 'boolean', @Lape_TMenu_ShortcutHandled_Read, @Lape_TMenu_ShortcutHandled_Write);
    addClassVar('TMenu', 'Items', 'TMenuItem', @Lape_TMenu_Items_Read, nil);
    addGlobalFunc('procedure TMenu.Init(AOwner: TComponent); override;', @Lape_TMenu_Init);
    //addGlobalFunc('procedure TMenu.Free(); constref;', @Lape_TMenu_Free);
  end;
end;

//constructor Create();
procedure Lape_TMainMenu_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMainMenu(Params^[0])^ := TMainMenu.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TMainMenu_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMainMenu(Params^[0])^.Free();
end;

procedure Lape_Import_TMainMenu(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TMainMenu', 'TMenu');

    addGlobalFunc('procedure TMainMenu.Init(AOwner: TComponent); override;', @Lape_TMainMenu_Init);
    //addGlobalFunc('procedure TMainMenu.Free(); constref;', @Lape_TMainMenu_Free);
  end;
end;

procedure Lape_Import_LCLMenus(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  Lape_Import_Lape_TMenu_Forward(Compiler);
  Lape_Import_TMenuItem(Compiler);
  Lape_Import_TMenu(Compiler);
  Lape_Import_TMainMenu(Compiler);
end;

end.

