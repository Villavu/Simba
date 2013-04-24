unit lplclstdctrls;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils,lpcompiler, lptypes, lpClassHelper;

procedure RegisterLCLStdCtrls(Compiler: TLapeCompiler);

implementation
  uses MufasaTypes,stdctrls,forms,lplclsystem,lplclgraphics,lplclcontrols;

type
  PCustomScrollBar = ^TCustomScrollBar;
  PScrollBarKind = ^TScrollBarKind;
  PScrollEvent = ^TScrollEvent;
  PScrollCode = ^TScrollCode;
  PScrollStyle = ^TScrollStyle;
  PScrollBar = ^TScrollBar;
  //Combobox
  PCustomComboBox = ^TCustomComboBox;
  PComboBoxStyle = ^TcomboBoxStyle;
  PComboBox = ^TComboBox;

{TCustomScrollBar}

//constructor Create(AOwner: TComponent);
procedure TCustomScrollBar_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomScrollBar(Params^[0])^ := TCustomScrollBar.Create(PComponent(Params^[1])^);
end;

//procedure SetParams(APosition, AMin, AMax, APageSize: Integer);
procedure TCustomScrollBar_SetParams(const Params: PParamArray); lape_extdecl
begin
  PCustomScrollBar(Params^[0])^.SetParams(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

//procedure SetParams(APosition, AMin, AMax: Integer);
procedure TCustomScrollBar_SetParamsEx(const Params: PParamArray); lape_extdecl
begin
  PCustomScrollBar(Params^[0])^.SetParams(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

//Read: property Kind: TScrollBarKind read Kind write Kind;
procedure TCustomScrollBar_Kind_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PScrollBarKind(Result)^ := PCustomScrollBar(Params^[0])^.Kind;
end;

//Write: property Kind: TScrollBarKind read Kind write Kind;
procedure TCustomScrollBar_Kind_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomScrollBar(Params^[0])^.Kind := PScrollBarKind(Params^[1])^;
end;

//Read: property LargeChange: integer read LargeChange write LargeChange;
procedure TCustomScrollBar_LargeChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomScrollBar(Params^[0])^.LargeChange;
end;

//Write: property LargeChange: integer read LargeChange write LargeChange;
procedure TCustomScrollBar_LargeChange_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomScrollBar(Params^[0])^.LargeChange := Pinteger(Params^[1])^;
end;

//Read: property Max: Integer read Max write Max;
procedure TCustomScrollBar_Max_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.Max;
end;

//Write: property Max: Integer read Max write Max;
procedure TCustomScrollBar_Max_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomScrollBar(Params^[0])^.Max := PInteger(Params^[1])^;
end;

//Read: property Min: Integer read Min write Min;
procedure TCustomScrollBar_Min_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.Min;
end;

//Write: property Min: Integer read Min write Min;
procedure TCustomScrollBar_Min_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomScrollBar(Params^[0])^.Min := PInteger(Params^[1])^;
end;

//Read: property PageSize: Integer read PageSize write PageSize;
procedure TCustomScrollBar_PageSize_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.PageSize;
end;

//Write: property PageSize: Integer read PageSize write PageSize;
procedure TCustomScrollBar_PageSize_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomScrollBar(Params^[0])^.PageSize := PInteger(Params^[1])^;
end;

//Read: property Position: Integer read Position write Position;
procedure TCustomScrollBar_Position_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.Position;
end;

//Write: property Position: Integer read Position write Position;
procedure TCustomScrollBar_Position_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomScrollBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

//Read: property SmallChange: integer read SmallChange write SmallChange;
procedure TCustomScrollBar_SmallChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomScrollBar(Params^[0])^.SmallChange;
end;

//Write: property SmallChange: integer read SmallChange write SmallChange;
procedure TCustomScrollBar_SmallChange_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomScrollBar(Params^[0])^.SmallChange := Pinteger(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure TCustomScrollBar_OnChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomScrollBar(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure TCustomScrollBar_OnChange_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomScrollBar(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//Read: property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
procedure TCustomScrollBar_OnScroll_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PScrollEvent(Result)^ := PCustomScrollBar(Params^[0])^.OnScroll;
end;

//Write: property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
procedure TCustomScrollBar_OnScroll_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomScrollBar(Params^[0])^.OnScroll := PScrollEvent(Params^[1])^;
end;

//procedure Free();
procedure TCustomScrollBar_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomScrollBar(Params^[0])^.Free();
end;

procedure Register_TCustomScrollBar(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TCustomScrollBar', 'TWinControl');

    addGlobalFunc('procedure TCustomScrollBar.Init(AOwner: TComponent);', @TCustomScrollBar_Init);
    addGlobalFunc('procedure TCustomScrollBar.SetParams(APosition, AMin, AMax, APageSize: Integer);', @TCustomScrollBar_SetParams);
    addGlobalFunc('procedure TCustomScrollBar.SetParams(APosition, AMin, AMax: Integer); overload;', @TCustomScrollBar_SetParamsEx);
    addClassVar(Compiler, 'TCustomScrollBar', 'Kind', 'TScrollBarKind', @TCustomScrollBar_Kind_Read, @TCustomScrollBar_Kind_Write);
    addClassVar(Compiler, 'TCustomScrollBar', 'LargeChange', 'integer', @TCustomScrollBar_LargeChange_Read, @TCustomScrollBar_LargeChange_Write);
    addClassVar(Compiler, 'TCustomScrollBar', 'Max', 'Integer', @TCustomScrollBar_Max_Read, @TCustomScrollBar_Max_Write);
    addClassVar(Compiler, 'TCustomScrollBar', 'Min', 'Integer', @TCustomScrollBar_Min_Read, @TCustomScrollBar_Min_Write);
    addClassVar(Compiler, 'TCustomScrollBar', 'PageSize', 'Integer', @TCustomScrollBar_PageSize_Read, @TCustomScrollBar_PageSize_Write);
    addClassVar(Compiler, 'TCustomScrollBar', 'Position', 'Integer', @TCustomScrollBar_Position_Read, @TCustomScrollBar_Position_Write);
    addClassVar(Compiler, 'TCustomScrollBar', 'SmallChange', 'integer', @TCustomScrollBar_SmallChange_Read, @TCustomScrollBar_SmallChange_Write);
    addClassVar(Compiler, 'TCustomScrollBar', 'OnChange', 'TNotifyEvent', @TCustomScrollBar_OnChange_Read, @TCustomScrollBar_OnChange_Write);
    addClassVar(Compiler, 'TCustomScrollBar', 'OnScroll', 'TScrollEvent', @TCustomScrollBar_OnScroll_Read, @TCustomScrollBar_OnScroll_Write);
    addGlobalFunc('procedure TCustomScrollBar.Free();', @TCustomScrollBar_Free);
  end;
end;

{TScrollBar}

//constructor Create();
procedure TScrollBar_Init(const Params: PParamArray); lape_extdecl
begin
  PScrollBar(Params^[0])^ := TScrollBar.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TScrollBar_Free(const Params: PParamArray); lape_extdecl
begin
  PScrollBar(Params^[0])^.Free();
end;

procedure Register_TScrollBar(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TScrollBar', 'TCustomScrollBar');

    addGlobalFunc('procedure TScrollBar.Init(AOwner: TComponent);', @TScrollBar_Init);
    addGlobalFunc('procedure TScrollBar.Free();', @TScrollBar_Free);
  end;
end;

{TCombobox and TCustomComboBox}

//constructor Create(TheOwner: TComponent);
procedure TCustomComboBox_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^ := TCustomComboBox.Create(PComponent(Params^[1])^);
end;

//procedure IntfGetItems;
procedure TCustomComboBox_IntfGetItems(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.IntfGetItems();
end;

//procedure AddItem(const Item: String; AnObject: TObject);
procedure TCustomComboBox_AddItem(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.AddItem(PlpString(Params^[1])^, PObject(Params^[2])^);
end;

//procedure AddHistoryItem(const Item: string; MaxHistoryCount: integer;SetAsText, CaseSensitive: boolean);
procedure TCustomComboBox_AddHistoryItem(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.AddHistoryItem(PlpString(Params^[1])^, Pinteger(Params^[2])^, Pboolean(Params^[3])^, Pboolean(Params^[4])^);
end;

//procedure AddHistoryItem(const Item: string; AnObject: TObject;MaxHistoryCount: integer; SetAsText, CaseSensitive: boolean);
procedure TCustomComboBox_AddHistoryItemEx(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.AddHistoryItem(PlpString(Params^[1])^, PObject(Params^[2])^, Pinteger(Params^[3])^, Pboolean(Params^[4])^, Pboolean(Params^[5])^);
end;

//procedure Clear;
procedure TCustomComboBox_Clear(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.Clear();
end;

//procedure ClearSelection;
procedure TCustomComboBox_ClearSelection(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.ClearSelection();
end;

//Read: property DroppedDown: Boolean read DroppedDown write DroppedDown;
procedure TCustomComboBox_DroppedDown_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.DroppedDown;
end;

//Write: property DroppedDown: Boolean read DroppedDown write DroppedDown;
procedure TCustomComboBox_DroppedDown_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.DroppedDown := PBoolean(Params^[1])^;
end;

//procedure SelectAll;
procedure TCustomComboBox_SelectAll(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.SelectAll();
end;

//Read: property AutoComplete: boolean read AutoComplete write AutoComplete;
procedure TCustomComboBox_AutoComplete_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomComboBox(Params^[0])^.AutoComplete;
end;

//Write: property AutoComplete: boolean read AutoComplete write AutoComplete;
procedure TCustomComboBox_AutoComplete_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.AutoComplete := Pboolean(Params^[1])^;
end;

//Read: property AutoDropDown: Boolean read AutoDropDown write AutoDropDown;
procedure TCustomComboBox_AutoDropDown_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoDropDown;
end;

//Write: property AutoDropDown: Boolean read AutoDropDown write AutoDropDown;
procedure TCustomComboBox_AutoDropDown_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.AutoDropDown := PBoolean(Params^[1])^;
end;

//Read: property AutoSelect: Boolean read AutoSelect write AutoSelect;
procedure TCustomComboBox_AutoSelect_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoSelect;
end;

//Write: property AutoSelect: Boolean read AutoSelect write AutoSelect;
procedure TCustomComboBox_AutoSelect_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.AutoSelect := PBoolean(Params^[1])^;
end;

//Read: property AutoSelected: Boolean read AutoSelected write AutoSelected;
procedure TCustomComboBox_AutoSelected_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoSelected;
end;

//Write: property AutoSelected: Boolean read AutoSelected write AutoSelected;
procedure TCustomComboBox_AutoSelected_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.AutoSelected := PBoolean(Params^[1])^;
end;

//Read: property ArrowKeysTraverseList: Boolean read ArrowKeysTraverseList write ArrowKeysTraverseList;
procedure TCustomComboBox_ArrowKeysTraverseList_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.ArrowKeysTraverseList;
end;

//Write: property ArrowKeysTraverseList: Boolean read ArrowKeysTraverseList write ArrowKeysTraverseList;
procedure TCustomComboBox_ArrowKeysTraverseList_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.ArrowKeysTraverseList := PBoolean(Params^[1])^;
end;

//Read: property Canvas: TCanvas read FCanvas;
procedure TCustomComboBox_Canvas_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCanvas(Result)^ := PCustomComboBox(Params^[0])^.Canvas;
end;

//Read: property DropDownCount: Integer read DropDownCount write DropDownCount default 8;
procedure TCustomComboBox_DropDownCount_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomComboBox(Params^[0])^.DropDownCount;
end;

//Write: property DropDownCount: Integer read DropDownCount write DropDownCount default 8;
procedure TCustomComboBox_DropDownCount_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.DropDownCount := PInteger(Params^[1])^;
end;

//Read: property Items: TStrings read Items write Items;
procedure TCustomComboBox_Items_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStrings(Result)^ := PCustomComboBox(Params^[0])^.Items;
end;

//Write: property Items: TStrings read Items write Items;
procedure TCustomComboBox_Items_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.Items := PStrings(Params^[1])^;
end;

//Read: property ItemIndex: integer read ItemIndex write ItemIndex;
procedure TCustomComboBox_ItemIndex_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomComboBox(Params^[0])^.ItemIndex;
end;

//Write: property ItemIndex: integer read ItemIndex write ItemIndex;
procedure TCustomComboBox_ItemIndex_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.ItemIndex := Pinteger(Params^[1])^;
end;

//Read: property ReadOnly: Boolean read ReadOnly write ReadOnly;
procedure TCustomComboBox_ReadOnly_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.ReadOnly;
end;

//Write: property ReadOnly: Boolean read ReadOnly write ReadOnly;
procedure TCustomComboBox_ReadOnly_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.ReadOnly := PBoolean(Params^[1])^;
end;

//Read: property SelLength: integer read SelLength write SelLength;
procedure TCustomComboBox_SelLength_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomComboBox(Params^[0])^.SelLength;
end;

//Write: property SelLength: integer read SelLength write SelLength;
procedure TCustomComboBox_SelLength_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.SelLength := Pinteger(Params^[1])^;
end;

//Read: property SelStart: integer read SelStart write SelStart;
procedure TCustomComboBox_SelStart_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomComboBox(Params^[0])^.SelStart;
end;

//Write: property SelStart: integer read SelStart write SelStart;
procedure TCustomComboBox_SelStart_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.SelStart := Pinteger(Params^[1])^;
end;

//Read: property SelText: String read SelText write SetSelText;
procedure TCustomComboBox_SelText_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PCustomComboBox(Params^[0])^.SelText;
end;

//Write: property SelText: String read SelText write SetSelText;
procedure TCustomComboBox_SelText_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.SelText := PlpString(Params^[1])^;
end;

//Read: property Style: TComboBoxStyle read FStyle write SetStyle default csDropDown;
procedure TCustomComboBox_Style_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PComboBoxStyle(Result)^ := PCustomComboBox(Params^[0])^.Style;
end;

//Write: property Style: TComboBoxStyle read FStyle write SetStyle default csDropDown;
procedure TCustomComboBox_Style_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.Style := PComboBoxStyle(Params^[1])^;
end;

//Read: property Text: string read Text write Text;
procedure TCustomComboBox_Text_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PCustomComboBox(Params^[0])^.Text;
end;

//Write: property Text: string read Text write Text;
procedure TCustomComboBox_Text_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.Text := PlpString(Params^[1])^;
end;

//procedure Free();
procedure TCustomComboBox_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomComboBox(Params^[0])^.Free();
end;

procedure Register_TCustomComboBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TCustomComboBox', 'TWinControl');

    addGlobalFunc('procedure TCustomComboBox.Init(TheOwner: TComponent);', @TCustomComboBox_Init);
    addGlobalFunc('procedure TCustomComboBox.IntfGetItems();', @TCustomComboBox_IntfGetItems);
    addGlobalFunc('procedure TCustomComboBox.AddItem(const Item: String; AnObject: TObject);', @TCustomComboBox_AddItem);
    addGlobalFunc('procedure TCustomComboBox.AddHistoryItem(const Item: string; MaxHistoryCount: integer;SetAsText, CaseSensitive: boolean);', @TCustomComboBox_AddHistoryItem);
    addGlobalFunc('procedure TCustomComboBox.AddHistoryItem(const Item: string; AnObject: TObject;MaxHistoryCount: integer; SetAsText, CaseSensitive: boolean); overload;', @TCustomComboBox_AddHistoryItemEx);
    addGlobalFunc('procedure TCustomComboBox.Clear();', @TCustomComboBox_Clear);
    addGlobalFunc('procedure TCustomComboBox.ClearSelection();', @TCustomComboBox_ClearSelection);
    addClassVar(Compiler, 'TCustomComboBox', 'DroppedDown', 'Boolean', @TCustomComboBox_DroppedDown_Read, @TCustomComboBox_DroppedDown_Write);
    addGlobalFunc('procedure TCustomComboBox.SelectAll();', @TCustomComboBox_SelectAll);
    addClassVar(Compiler, 'TCustomComboBox', 'AutoComplete', 'boolean', @TCustomComboBox_AutoComplete_Read, @TCustomComboBox_AutoComplete_Write);
    addClassVar(Compiler, 'TCustomComboBox', 'AutoDropDown', 'Boolean', @TCustomComboBox_AutoDropDown_Read, @TCustomComboBox_AutoDropDown_Write);
    addClassVar(Compiler, 'TCustomComboBox', 'AutoSelect', 'Boolean', @TCustomComboBox_AutoSelect_Read, @TCustomComboBox_AutoSelect_Write);
    addClassVar(Compiler, 'TCustomComboBox', 'AutoSelected', 'Boolean', @TCustomComboBox_AutoSelected_Read, @TCustomComboBox_AutoSelected_Write);
    addClassVar(Compiler, 'TCustomComboBox', 'ArrowKeysTraverseList', 'Boolean', @TCustomComboBox_ArrowKeysTraverseList_Read, @TCustomComboBox_ArrowKeysTraverseList_Write);
    addClassVar(Compiler, 'TCustomComboBox', 'Canvas', 'TCanvas', @TCustomComboBox_Canvas_Read, nil);
    addClassVar(Compiler, 'TCustomComboBox', 'DropDownCount', 'Integer', @TCustomComboBox_DropDownCount_Read, @TCustomComboBox_DropDownCount_Write);
    addClassVar(Compiler, 'TCustomComboBox', 'Items', 'TStrings', @TCustomComboBox_Items_Read, @TCustomComboBox_Items_Write);
    addClassVar(Compiler, 'TCustomComboBox', 'ItemIndex', 'integer', @TCustomComboBox_ItemIndex_Read, @TCustomComboBox_ItemIndex_Write);
    addClassVar(Compiler, 'TCustomComboBox', 'ReadOnly', 'Boolean', @TCustomComboBox_ReadOnly_Read, @TCustomComboBox_ReadOnly_Write);
    addClassVar(Compiler, 'TCustomComboBox', 'SelLength', 'integer', @TCustomComboBox_SelLength_Read, @TCustomComboBox_SelLength_Write);
    addClassVar(Compiler, 'TCustomComboBox', 'SelStart', 'integer', @TCustomComboBox_SelStart_Read, @TCustomComboBox_SelStart_Write);
    addClassVar(Compiler, 'TCustomComboBox', 'SelText', 'String', @TCustomComboBox_SelText_Read, @TCustomComboBox_SelText_Write);
    addClassVar(Compiler, 'TCustomComboBox', 'Style', 'TComboBoxStyle', @TCustomComboBox_Style_Read, @TCustomComboBox_Style_Write);
    addClassVar(Compiler, 'TCustomComboBox', 'Text', 'string', @TCustomComboBox_Text_Read, @TCustomComboBox_Text_Write);
    addGlobalFunc('procedure TCustomComboBox.Free();', @TCustomComboBox_Free);
  end;
end;

//constructor Create(TheOwner: TComponent);
procedure TComboBox_Init(const Params: PParamArray); lape_extdecl
begin
  PComboBox(Params^[0])^ := TComboBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TComboBox_Free(const Params: PParamArray); lape_extdecl
begin
  PComboBox(Params^[0])^.Free();
end;

procedure Register_TComboBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TComboBox', 'TCustomComboBox');

    addGlobalFunc('procedure TComboBox.Init(TheOwner: TComponent);', @TComboBox_Init);
    addGlobalFunc('procedure TComboBox.Free();', @TComboBox_Free);
  end;
end;

{TListBox}

procedure RegisterLCLStdCtrls(Compiler: TLapeCompiler);
begin
  with Compiler do
   begin
     addGlobalType('(ssNone, ssHorizontal, ssVertical, ssBoth,ssAutoHorizontal, ssAutoVertical, ssAutoBoth)','TScrollStyle');
     addGlobalType('(scLineUp,scLineDown, scPageUp,scPageDown,scPosition, scTrack,scTop,scBottom,scEndScroll)','TScrollCode');
     addGlobalType('procedure(Sender: TObject; ScrollCode: TScrollCode;var ScrollPos: Integer)','TScrollEvent');
     addGlobalType('(csDropDown,csSimple,csDropDownList,csOwnerDrawFixed,csOwnerDrawVariable)','TComboBoxStyle');
   end;
  Register_TCustomScrollBar(Compiler);
  Register_TScrollBar(Compiler);
  Register_TCustomComboBox(Compiler);
  Register_TComboBox(Compiler);
end;

end.

