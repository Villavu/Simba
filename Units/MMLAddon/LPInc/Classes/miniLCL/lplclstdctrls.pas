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
  //list box
  PCustomListBox = ^TCustomListBox;
  PListBox = ^TListBox;
  //TEdit
  PCustomEdit =^TCustomEdit;
  PEdit = ^TEdit;
  //TMemo
  PMemoScrollBar = ^TMemoScrollBar;
  PCustomMemo = ^TCustomMemo;
  PMemo = ^TMemo;
  //TStaticText
  PCustomStaticText = ^TCustomStaticText;
  PAlignment = ^TAlignment;
  PStaticBorderStyle = ^TStaticBorderStyle;
  PStaticText = ^TStaticText;
  //Button control
  PButtonControl = ^TButtonControl;
  PCustomButton = ^TCustomButton;
  PButton = ^TButton;
  //CheckBox
  PCheckBoxState = ^TCheckBoxState;
  PCustomCheckBox = ^TCustomCheckBox;
  PCheckBox = ^TCheckBox;
  //TLabel
  PCustomLabel = ^TCustomLabel;
  PLabel = ^TLabel;
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
//constructor Create(TheOwner: TComponent);
procedure TCustomListBox_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^ := TCustomListBox.Create(PComponent(Params^[1])^);
end;

//procedure AddItem(const Item: String; AnObject: TObject);
procedure TCustomListBox_AddItem(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.AddItem(PlpString(Params^[1])^, PObject(Params^[2])^);
end;

//procedure Click;
procedure TCustomListBox_Click(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.Click();
end;

//procedure Clear;
procedure TCustomListBox_Clear(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.Clear();
end;

//procedure ClearSelection;
procedure TCustomListBox_ClearSelection(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.ClearSelection();
end;

//function GetIndexAtXY(X, Y: integer): integer;
procedure TCustomListBox_GetIndexAtXY(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.GetIndexAtXY(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function GetIndexAtY(Y: integer): integer;
procedure TCustomListBox_GetIndexAtY(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.GetIndexAtY(Pinteger(Params^[1])^);
end;

//function GetSelectedText: string;
procedure TCustomListBox_GetSelectedText(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PCustomListBox(Params^[0])^.GetSelectedText();
end;

//function ItemAtPos(const Pos: TPoint; Existing: Boolean): Integer;
procedure TCustomListBox_ItemAtPos(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.ItemAtPos(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

//function ItemRect(Index: Integer): TRect;
procedure TCustomListBox_ItemRect(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PRect(Result)^ := PCustomListBox(Params^[0])^.ItemRect(PInteger(Params^[1])^);
end;

//function ItemVisible(Index: Integer): boolean;
procedure TCustomListBox_ItemVisible(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ItemVisible(PInteger(Params^[1])^);
end;

//function ItemFullyVisible(Index: Integer): boolean;
procedure TCustomListBox_ItemFullyVisible(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ItemFullyVisible(PInteger(Params^[1])^);
end;

//procedure LockSelectionChange;
procedure TCustomListBox_LockSelectionChange(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.LockSelectionChange();
end;

//procedure MakeCurrentVisible;
procedure TCustomListBox_MakeCurrentVisible(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.MakeCurrentVisible();
end;

//procedure MeasureItem(Index: Integer; var TheHeight: Integer); virtual;
procedure TCustomListBox_MeasureItem(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.MeasureItem(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure SelectAll; virtual;
procedure TCustomListBox_SelectAll(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.SelectAll();
end;

//procedure UnlockSelectionChange;
procedure TCustomListBox_UnlockSelectionChange(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.UnlockSelectionChange();
end;

//Read: property Canvas: TCanvas read FCanvas;
procedure TCustomListBox_Canvas_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCanvas(Result)^ := PCustomListBox(Params^[0])^.Canvas;
end;

//Read: property ClickOnSelChange: boolean read ClickOnSelChange write ClickOnSelChange;
procedure TCustomListBox_ClickOnSelChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ClickOnSelChange;
end;

//Write: property ClickOnSelChange: boolean read ClickOnSelChange write ClickOnSelChange;
procedure TCustomListBox_ClickOnSelChange_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.ClickOnSelChange := Pboolean(Params^[1])^;
end;

//Read: property Columns: Integer read Columns write Columns ;
procedure TCustomListBox_Columns_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.Columns;
end;

//Write: property Columns: Integer read Columns write Columns ;
procedure TCustomListBox_Columns_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.Columns := PInteger(Params^[1])^;
end;

//Read: property Count: Integer read Count;
procedure TCustomListBox_Count_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.Count;
end;

//Read: property ExtendedSelect: boolean read ExtendedSelect write ExtendedSelect;
procedure TCustomListBox_ExtendedSelect_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ExtendedSelect;
end;

//Write: property ExtendedSelect: boolean read ExtendedSelect write ExtendedSelect;
procedure TCustomListBox_ExtendedSelect_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.ExtendedSelect := Pboolean(Params^[1])^;
end;

//Read: property ItemHeight: Integer read ItemHeight write ItemHeight;
procedure TCustomListBox_ItemHeight_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.ItemHeight;
end;

//Write: property ItemHeight: Integer read ItemHeight write ItemHeight;
procedure TCustomListBox_ItemHeight_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.ItemHeight := PInteger(Params^[1])^;
end;

//Read: property ItemIndex: integer read ItemIndex write ItemIndex;
procedure TCustomListBox_ItemIndex_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.ItemIndex;
end;

//Write: property ItemIndex: integer read ItemIndex write ItemIndex;
procedure TCustomListBox_ItemIndex_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.ItemIndex := Pinteger(Params^[1])^;
end;

//Read: property Items: TStrings read Items write Items;
procedure TCustomListBox_Items_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStrings(Result)^ := PCustomListBox(Params^[0])^.Items;
end;

//Write: property Items: TStrings read Items write Items;
procedure TCustomListBox_Items_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.Items := PStrings(Params^[1])^;
end;

//Read: property MultiSelect: boolean read MultiSelect write MultiSelect;
procedure TCustomListBox_MultiSelect_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.MultiSelect;
end;

//Write: property MultiSelect: boolean read MultiSelect write MultiSelect;
procedure TCustomListBox_MultiSelect_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.MultiSelect := Pboolean(Params^[1])^;
end;

//Read: property ScrollWidth: Integer read ScrollWidth write ScrollWidth;
procedure TCustomListBox_ScrollWidth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.ScrollWidth;
end;

//Write: property ScrollWidth: Integer read ScrollWidth write ScrollWidth;
procedure TCustomListBox_ScrollWidth_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.ScrollWidth := PInteger(Params^[1])^;
end;

//Read: property SelCount: integer read SelCount;
procedure TCustomListBox_SelCount_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.SelCount;
end;

//Read: property Sorted: boolean read Sorted write Sorted;
procedure TCustomListBox_Sorted_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.Sorted;
end;

//Write: property Sorted: boolean read Sorted write Sorted;
procedure TCustomListBox_Sorted_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.Sorted := Pboolean(Params^[1])^;
end;

//Read: property TopIndex: Integer read TopIndex write TopIndex;
procedure TCustomListBox_TopIndex_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.TopIndex;
end;

//Write: property TopIndex: Integer read TopIndex write TopIndex;
procedure TCustomListBox_TopIndex_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.TopIndex := PInteger(Params^[1])^;
end;

//procedure Free();
procedure TCustomListBox_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomListBox(Params^[0])^.Free();
end;

procedure Register_TCustomListBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TCustomListBox', 'TWinControl');

    addGlobalFunc('procedure TCustomListBox.Init(TheOwner: TComponent);', @TCustomListBox_Init);
    addGlobalFunc('procedure TCustomListBox.AddItem(const Item: String; AnObject: TObject);', @TCustomListBox_AddItem);
    addGlobalFunc('procedure TCustomListBox.Click();', @TCustomListBox_Click);
    addGlobalFunc('procedure TCustomListBox.Clear();', @TCustomListBox_Clear);
    addGlobalFunc('procedure TCustomListBox.ClearSelection();', @TCustomListBox_ClearSelection);
    addGlobalFunc('function TCustomListBox.GetIndexAtXY(X, Y: integer): integer;', @TCustomListBox_GetIndexAtXY);
    addGlobalFunc('function TCustomListBox.GetIndexAtY(Y: integer): integer;', @TCustomListBox_GetIndexAtY);
    addGlobalFunc('function TCustomListBox.GetSelectedText(): string;', @TCustomListBox_GetSelectedText);
    addGlobalFunc('function TCustomListBox.ItemAtPos(const Pos: TPoint; Existing: Boolean): Integer;', @TCustomListBox_ItemAtPos);
    addGlobalFunc('function TCustomListBox.ItemRect(Index: Integer): TRect;', @TCustomListBox_ItemRect);
    addGlobalFunc('function TCustomListBox.ItemVisible(Index: Integer): boolean;', @TCustomListBox_ItemVisible);
    addGlobalFunc('function TCustomListBox.ItemFullyVisible(Index: Integer): boolean;', @TCustomListBox_ItemFullyVisible);
    addGlobalFunc('procedure TCustomListBox.LockSelectionChange();', @TCustomListBox_LockSelectionChange);
    addGlobalFunc('procedure TCustomListBox.MakeCurrentVisible();', @TCustomListBox_MakeCurrentVisible);
    addGlobalFunc('procedure TCustomListBox.MeasureItem(Index: Integer; var TheHeight: Integer);', @TCustomListBox_MeasureItem);
    addGlobalFunc('procedure TCustomListBox.SelectAll();', @TCustomListBox_SelectAll);
    addGlobalFunc('procedure TCustomListBox.UnlockSelectionChange();', @TCustomListBox_UnlockSelectionChange);
    addClassVar(Compiler, 'TCustomListBox', 'Canvas', 'TCanvas', @TCustomListBox_Canvas_Read, nil);
    addClassVar(Compiler, 'TCustomListBox', 'ClickOnSelChange', 'boolean', @TCustomListBox_ClickOnSelChange_Read, @TCustomListBox_ClickOnSelChange_Write);
    addClassVar(Compiler, 'TCustomListBox', 'Columns', 'Integer', @TCustomListBox_Columns_Read, @TCustomListBox_Columns_Write);
    addClassVar(Compiler, 'TCustomListBox', 'Count', 'Integer', @TCustomListBox_Count_Read, nil);
    addClassVar(Compiler, 'TCustomListBox', 'ExtendedSelect', 'boolean', @TCustomListBox_ExtendedSelect_Read, @TCustomListBox_ExtendedSelect_Write);
    addClassVar(Compiler, 'TCustomListBox', 'ItemHeight', 'Integer', @TCustomListBox_ItemHeight_Read, @TCustomListBox_ItemHeight_Write);
    addClassVar(Compiler, 'TCustomListBox', 'ItemIndex', 'integer', @TCustomListBox_ItemIndex_Read, @TCustomListBox_ItemIndex_Write);
    addClassVar(Compiler, 'TCustomListBox', 'Items', 'TStrings', @TCustomListBox_Items_Read, @TCustomListBox_Items_Write);
    addClassVar(Compiler, 'TCustomListBox', 'MultiSelect', 'boolean', @TCustomListBox_MultiSelect_Read, @TCustomListBox_MultiSelect_Write);
    addClassVar(Compiler, 'TCustomListBox', 'ScrollWidth', 'Integer', @TCustomListBox_ScrollWidth_Read, @TCustomListBox_ScrollWidth_Write);
    addClassVar(Compiler, 'TCustomListBox', 'SelCount', 'integer', @TCustomListBox_SelCount_Read, nil);
    addClassVar(Compiler, 'TCustomListBox', 'Sorted', 'boolean', @TCustomListBox_Sorted_Read, @TCustomListBox_Sorted_Write);
    addClassVar(Compiler, 'TCustomListBox', 'TopIndex', 'Integer', @TCustomListBox_TopIndex_Read, @TCustomListBox_TopIndex_Write);
    addGlobalFunc('procedure TCustomListBox.Free();', @TCustomListBox_Free);
  end;
end;

//constructor Create(TheOwner: TComponent);
procedure TListBox_Init(const Params: PParamArray); lape_extdecl
begin
  PListBox(Params^[0])^ := TListBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TListBox_Free(const Params: PParamArray); lape_extdecl
begin
  PListBox(Params^[0])^.Free();
end;

procedure Register_TListBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TListBox', 'TCustomListBox');

    addGlobalFunc('procedure TListBox.Init(TheOwner: TComponent);', @TListBox_Init);
    addGlobalFunc('procedure TListBox.Free();', @TListBox_Free);
  end;
end;

{TEdit}
//constructor Create(AOwner: TComponent);
procedure TCustomEdit_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^ := TCustomEdit.Create(PComponent(Params^[1])^);
end;

//procedure Clear;
procedure TCustomEdit_Clear(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.Clear();
end;

//procedure SelectAll;
procedure TCustomEdit_SelectAll(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.SelectAll();
end;

//procedure ClearSelection;
procedure TCustomEdit_ClearSelection(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.ClearSelection();
end;

//procedure CopyToClipboard;
procedure TCustomEdit_CopyToClipboard(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.CopyToClipboard();
end;

//procedure CutToClipboard;
procedure TCustomEdit_CutToClipboard(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.CutToClipboard();
end;

//procedure PasteFromClipboard;
procedure TCustomEdit_PasteFromClipboard(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.PasteFromClipboard();
end;

//procedure Undo;
procedure TCustomEdit_Undo(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.Undo();
end;

//Read: property CanUndo: Boolean read CanUndo;
procedure TCustomEdit_CanUndo_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.CanUndo;
end;

//Read: property CaretPos: TPoint read CaretPos write CaretPos;
procedure TCustomEdit_CaretPos_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPoint(Result)^ := PCustomEdit(Params^[0])^.CaretPos;
end;

//Write: property CaretPos: TPoint read CaretPos write CaretPos;
procedure TCustomEdit_CaretPos_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.CaretPos := PPoint(Params^[1])^;
end;

//Read: property HideSelection: Boolean read HideSelection write HideSelection;
procedure TCustomEdit_HideSelection_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.HideSelection;
end;

//Write: property HideSelection: Boolean read HideSelection write HideSelection;
procedure TCustomEdit_HideSelection_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.HideSelection := PBoolean(Params^[1])^;
end;

//Read: property MaxLength: Integer read MaxLength write MaxLength;
procedure TCustomEdit_MaxLength_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomEdit(Params^[0])^.MaxLength;
end;

//Write: property MaxLength: Integer read MaxLength write MaxLength;
procedure TCustomEdit_MaxLength_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.MaxLength := PInteger(Params^[1])^;
end;

//Read: property Modified: Boolean read Modified write Modified;
procedure TCustomEdit_Modified_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.Modified;
end;

//Write: property Modified: Boolean read Modified write Modified;
procedure TCustomEdit_Modified_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.Modified := PBoolean(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure TCustomEdit_OnChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomEdit(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure TCustomEdit_OnChange_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//Read: property PasswordChar: Char read PasswordChar write PasswordChar;
procedure TCustomEdit_PasswordChar_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PChar(Result)^ := PCustomEdit(Params^[0])^.PasswordChar;
end;

//Write: property PasswordChar: Char read PasswordChar write PasswordChar;
procedure TCustomEdit_PasswordChar_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.PasswordChar := PChar(Params^[1])^;
end;

//Read: property ReadOnly: Boolean read ReadOnly write ReadOnly;
procedure TCustomEdit_ReadOnly_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.ReadOnly;
end;

//Write: property ReadOnly: Boolean read ReadOnly write ReadOnly;
procedure TCustomEdit_ReadOnly_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.ReadOnly := PBoolean(Params^[1])^;
end;

//Read: property SelLength: integer read SelLength write SelLength;
procedure TCustomEdit_SelLength_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomEdit(Params^[0])^.SelLength;
end;

//Write: property SelLength: integer read SelLength write SelLength;
procedure TCustomEdit_SelLength_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.SelLength := Pinteger(Params^[1])^;
end;

//Read: property SelStart: integer read SelStart write SelStart;
procedure TCustomEdit_SelStart_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCustomEdit(Params^[0])^.SelStart;
end;

//Write: property SelStart: integer read SelStart write SelStart;
procedure TCustomEdit_SelStart_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.SelStart := Pinteger(Params^[1])^;
end;

//Read: property SelText: String read SelText write SelText;
procedure TCustomEdit_SelText_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PCustomEdit(Params^[0])^.SelText;
end;

//Write: property SelText: String read SelText write SelText;
procedure TCustomEdit_SelText_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.SelText := PlpString(Params^[1])^;
end;

//Read: property Text: string read Text write Text;
procedure TCustomEdit_Text_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PCustomEdit(Params^[0])^.Text;
end;

//Write: property Text: string read Text write Text;
procedure TCustomEdit_Text_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.Text := PlpString(Params^[1])^;
end;

//procedure Free();
procedure TCustomEdit_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomEdit(Params^[0])^.Free();
end;

procedure Register_TCustomEdit(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TCustomEdit', 'TWinControl');

    addGlobalFunc('procedure TCustomEdit.Init(AOwner: TComponent);', @TCustomEdit_Init);
    addGlobalFunc('procedure TCustomEdit.Clear();', @TCustomEdit_Clear);
    addGlobalFunc('procedure TCustomEdit.SelectAll();', @TCustomEdit_SelectAll);
    addGlobalFunc('procedure TCustomEdit.ClearSelection();', @TCustomEdit_ClearSelection);
    addGlobalFunc('procedure TCustomEdit.CopyToClipboard();', @TCustomEdit_CopyToClipboard);
    addGlobalFunc('procedure TCustomEdit.CutToClipboard();', @TCustomEdit_CutToClipboard);
    addGlobalFunc('procedure TCustomEdit.PasteFromClipboard();', @TCustomEdit_PasteFromClipboard);
    addGlobalFunc('procedure TCustomEdit.Undo();', @TCustomEdit_Undo);
    addClassVar(Compiler, 'TCustomEdit', 'CanUndo', 'Boolean', @TCustomEdit_CanUndo_Read, nil);
    addClassVar(Compiler, 'TCustomEdit', 'CaretPos', 'TPoint', @TCustomEdit_CaretPos_Read, @TCustomEdit_CaretPos_Write);
    addClassVar(Compiler, 'TCustomEdit', 'HideSelection', 'Boolean', @TCustomEdit_HideSelection_Read, @TCustomEdit_HideSelection_Write);
    addClassVar(Compiler, 'TCustomEdit', 'MaxLength', 'Integer', @TCustomEdit_MaxLength_Read, @TCustomEdit_MaxLength_Write);
    addClassVar(Compiler, 'TCustomEdit', 'Modified', 'Boolean', @TCustomEdit_Modified_Read, @TCustomEdit_Modified_Write);
    addClassVar(Compiler, 'TCustomEdit', 'OnChange', 'TNotifyEvent', @TCustomEdit_OnChange_Read, @TCustomEdit_OnChange_Write);
    addClassVar(Compiler, 'TCustomEdit', 'PasswordChar', 'Char', @TCustomEdit_PasswordChar_Read, @TCustomEdit_PasswordChar_Write);
    addClassVar(Compiler, 'TCustomEdit', 'ReadOnly', 'Boolean', @TCustomEdit_ReadOnly_Read, @TCustomEdit_ReadOnly_Write);
    addClassVar(Compiler, 'TCustomEdit', 'SelLength', 'integer', @TCustomEdit_SelLength_Read, @TCustomEdit_SelLength_Write);
    addClassVar(Compiler, 'TCustomEdit', 'SelStart', 'integer', @TCustomEdit_SelStart_Read, @TCustomEdit_SelStart_Write);
    addClassVar(Compiler, 'TCustomEdit', 'SelText', 'String', @TCustomEdit_SelText_Read, @TCustomEdit_SelText_Write);
    addClassVar(Compiler, 'TCustomEdit', 'Text', 'string', @TCustomEdit_Text_Read, @TCustomEdit_Text_Write);
    addGlobalFunc('procedure TCustomEdit.Free();', @TCustomEdit_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure TEdit_Init(const Params: PParamArray); lape_extdecl
begin
  PEdit(Params^[0])^ := TEdit.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TEdit_Free(const Params: PParamArray); lape_extdecl
begin
  PEdit(Params^[0])^.Free();
end;

procedure Register_TEdit(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TEdit', 'TCustomEdit');

    addGlobalFunc('procedure TEdit.Init(AOwner: TComponent);', @TEdit_Init);
    addGlobalFunc('procedure TEdit.Free();', @TEdit_Free);
  end;
end;
{TMemo}
//constructor Create(AOwner: TComponent);
procedure TMemoScrollbar_Init(const Params: PParamArray); lape_extdecl
begin
  PMemoScrollbar(Params^[0])^ := TMemoScrollbar.Create(PWinControl(Params^[1])^, PScrollBarKind(Params^[2])^);
end;

//procedure Free();
procedure TMemoScrollbar_Free(const Params: PParamArray); lape_extdecl
begin
  PMemoScrollbar(Params^[0])^.Free();
end;

procedure Register_TMemoScrollbar(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TMemoScrollbar', 'TControlScrollBar');

    addGlobalFunc('procedure TMemoScrollbar.Init(AControl: TWinControl; AKind: TScrollBarKind);', @TMemoScrollbar_Init);
    addGlobalFunc('procedure TMemoScrollbar.Free();', @TMemoScrollbar_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure TCustomMemo_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomMemo(Params^[0])^ := TCustomMemo.Create(PComponent(Params^[1])^);
end;

//procedure Append(const Value: String);
procedure TCustomMemo_Append(const Params: PParamArray); lape_extdecl
begin
  PCustomMemo(Params^[0])^.Append(PlpString(Params^[1])^);
end;

//Read: property Lines: TStrings read Lines write Lines;
procedure TCustomMemo_Lines_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStrings(Result)^ := PCustomMemo(Params^[0])^.Lines;
end;

//Write: property Lines: TStrings read Lines write Lines;
procedure TCustomMemo_Lines_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomMemo(Params^[0])^.Lines := PStrings(Params^[1])^;
end;

//Read: property HorzScrollBar: TMemoScrollBar read HorzScrollBar write HorzScrollBar;
procedure TCustomMemo_HorzScrollBar_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMemoScrollBar(Result)^ := PCustomMemo(Params^[0])^.HorzScrollBar;
end;

//Write: property HorzScrollBar: TMemoScrollBar read HorzScrollBar write HorzScrollBar;
procedure TCustomMemo_HorzScrollBar_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomMemo(Params^[0])^.HorzScrollBar := PMemoScrollBar(Params^[1])^;
end;

//Read: property VertScrollBar: TMemoScrollBar read VertScrollBar write VertScrollBar;
procedure TCustomMemo_VertScrollBar_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PMemoScrollBar(Result)^ := PCustomMemo(Params^[0])^.VertScrollBar;
end;

//Write: property VertScrollBar: TMemoScrollBar read VertScrollBar write VertScrollBar;
procedure TCustomMemo_VertScrollBar_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomMemo(Params^[0])^.VertScrollBar := PMemoScrollBar(Params^[1])^;
end;

//Read: property ScrollBars: TScrollStyle read ScrollBars write ScrollBars;
procedure TCustomMemo_ScrollBars_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PScrollStyle(Result)^ := PCustomMemo(Params^[0])^.ScrollBars;
end;

//Write: property ScrollBars: TScrollStyle read ScrollBars write ScrollBars;
procedure TCustomMemo_ScrollBars_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomMemo(Params^[0])^.ScrollBars := PScrollStyle(Params^[1])^;
end;

//Read: property WantReturns: Boolean read WantReturns write WantReturns;
procedure TCustomMemo_WantReturns_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomMemo(Params^[0])^.WantReturns;
end;

//Write: property WantReturns: Boolean read WantReturns write WantReturns;
procedure TCustomMemo_WantReturns_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomMemo(Params^[0])^.WantReturns := PBoolean(Params^[1])^;
end;

//Read: property WantTabs: Boolean read WantTabs write WantTabs;
procedure TCustomMemo_WantTabs_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomMemo(Params^[0])^.WantTabs;
end;

//Write: property WantTabs: Boolean read WantTabs write WantTabs;
procedure TCustomMemo_WantTabs_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomMemo(Params^[0])^.WantTabs := PBoolean(Params^[1])^;
end;

//Read: property WordWrap: Boolean read WordWrap write WordWrap;
procedure TCustomMemo_WordWrap_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomMemo(Params^[0])^.WordWrap;
end;

//Write: property WordWrap: Boolean read WordWrap write WordWrap;
procedure TCustomMemo_WordWrap_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomMemo(Params^[0])^.WordWrap := PBoolean(Params^[1])^;
end;

//procedure Free();
procedure TCustomMemo_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomMemo(Params^[0])^.Free();
end;

procedure Register_TCustomMemo(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TCustomMemo', 'TCustomEdit');

    addGlobalFunc('procedure TCustomMemo.Init(AOwner: TComponent);', @TCustomMemo_Init);
    addGlobalFunc('procedure TCustomMemo.Append(const Value: String);', @TCustomMemo_Append);
    addClassVar(Compiler, 'TCustomMemo', 'Lines', 'TStrings', @TCustomMemo_Lines_Read, @TCustomMemo_Lines_Write);
    addClassVar(Compiler, 'TCustomMemo', 'HorzScrollBar', 'TMemoScrollBar', @TCustomMemo_HorzScrollBar_Read, @TCustomMemo_HorzScrollBar_Write);
    addClassVar(Compiler, 'TCustomMemo', 'VertScrollBar', 'TMemoScrollBar', @TCustomMemo_VertScrollBar_Read, @TCustomMemo_VertScrollBar_Write);
    addClassVar(Compiler, 'TCustomMemo', 'ScrollBars', 'TScrollStyle', @TCustomMemo_ScrollBars_Read, @TCustomMemo_ScrollBars_Write);
    addClassVar(Compiler, 'TCustomMemo', 'WantReturns', 'Boolean', @TCustomMemo_WantReturns_Read, @TCustomMemo_WantReturns_Write);
    addClassVar(Compiler, 'TCustomMemo', 'WantTabs', 'Boolean', @TCustomMemo_WantTabs_Read, @TCustomMemo_WantTabs_Write);
    addClassVar(Compiler, 'TCustomMemo', 'WordWrap', 'Boolean', @TCustomMemo_WordWrap_Read, @TCustomMemo_WordWrap_Write);
    addGlobalFunc('procedure TCustomMemo.Free();', @TCustomMemo_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure TMemo_Init(const Params: PParamArray); lape_extdecl
begin
  PMemo(Params^[0])^ := TMemo.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TMemo_Free(const Params: PParamArray); lape_extdecl
begin
  PMemo(Params^[0])^.Free();
end;

procedure Register_TMemo(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TMemo', 'TCustomMemo');

    addGlobalFunc('procedure TMemo.Init(AOwner: TComponent);', @TMemo_Init);
    addGlobalFunc('procedure TMemo.Free();', @TMemo_Free);
  end;
end;


{TStaticText}
//constructor Create(AOwner: TComponent);
procedure TCustomStaticText_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomStaticText(Params^[0])^ := TCustomStaticText.Create(PComponent(Params^[1])^);
end;

//Read: property Alignment: TAlignment read Alignment write Alignment;
procedure TCustomStaticText_Alignment_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PAlignment(Result)^ := PCustomStaticText(Params^[0])^.Alignment;
end;

//Write: property Alignment: TAlignment read Alignment write Alignment;
procedure TCustomStaticText_Alignment_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomStaticText(Params^[0])^.Alignment := PAlignment(Params^[1])^;
end;

//Read: property BorderStyle: TStaticBorderStyle read StaticBorderStyle write StaticBorderStyle;
procedure TCustomStaticText_BorderStyle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStaticBorderStyle(Result)^ := PCustomStaticText(Params^[0])^.BorderStyle;
end;

//Write: property BorderStyle: TStaticBorderStyle read StaticBorderStyle write StaticBorderStyle;
procedure TCustomStaticText_BorderStyle_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomStaticText(Params^[0])^.BorderStyle := PStaticBorderStyle(Params^[1])^;
end;

//Read: property FocusControl: TWinControl read FocusControl write FocusControl;
procedure TCustomStaticText_FocusControl_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PWinControl(Result)^ := PCustomStaticText(Params^[0])^.FocusControl;
end;

//Write: property FocusControl: TWinControl read FocusControl write FocusControl;
procedure TCustomStaticText_FocusControl_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomStaticText(Params^[0])^.FocusControl := PWinControl(Params^[1])^;
end;

//Read: property ShowAccelChar: boolean read ShowAccelChar write ShowAccelChar;
procedure TCustomStaticText_ShowAccelChar_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomStaticText(Params^[0])^.ShowAccelChar;
end;

//Write: property ShowAccelChar: boolean read ShowAccelChar write ShowAccelChar;
procedure TCustomStaticText_ShowAccelChar_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomStaticText(Params^[0])^.ShowAccelChar := Pboolean(Params^[1])^;
end;

//Read: property Transparent: Boolean read Transparent write Transparent;
procedure TCustomStaticText_Transparent_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomStaticText(Params^[0])^.Transparent;
end;

//Write: property Transparent: Boolean read Transparent write Transparent;
procedure TCustomStaticText_Transparent_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomStaticText(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

//procedure Free();
procedure TCustomStaticText_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomStaticText(Params^[0])^.Free();
end;

procedure Register_TCustomStaticText(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TCustomStaticText', 'TWinControl');

    addGlobalFunc('procedure TCustomStaticText.Init(AOwner: TComponent);', @TCustomStaticText_Init);
    addClassVar(Compiler, 'TCustomStaticText', 'Alignment', 'TAlignment', @TCustomStaticText_Alignment_Read, @TCustomStaticText_Alignment_Write);
    addClassVar(Compiler, 'TCustomStaticText', 'BorderStyle', 'TStaticBorderStyle', @TCustomStaticText_BorderStyle_Read, @TCustomStaticText_BorderStyle_Write);
    addClassVar(Compiler, 'TCustomStaticText', 'FocusControl', 'TWinControl', @TCustomStaticText_FocusControl_Read, @TCustomStaticText_FocusControl_Write);
    addClassVar(Compiler, 'TCustomStaticText', 'ShowAccelChar', 'boolean', @TCustomStaticText_ShowAccelChar_Read, @TCustomStaticText_ShowAccelChar_Write);
    addClassVar(Compiler, 'TCustomStaticText', 'Transparent', 'Boolean', @TCustomStaticText_Transparent_Read, @TCustomStaticText_Transparent_Write);
    addGlobalFunc('procedure TCustomStaticText.Free();', @TCustomStaticText_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure TStaticText_Init(const Params: PParamArray); lape_extdecl
begin
  PStaticText(Params^[0])^ := TStaticText.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TStaticText_Free(const Params: PParamArray); lape_extdecl
begin
  PStaticText(Params^[0])^.Free();
end;

procedure Register_TStaticText(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TStaticText', 'TCustomStaticText');

    addGlobalFunc('procedure TStaticText.Init(AOwner: TComponent);', @TStaticText_Init);
    addGlobalFunc('procedure TStaticText.Free();', @TStaticText_Free);
  end;
end;


{TButton}

//constructor Create(TheOwner: TComponent);
procedure TButtonControl_Init(const Params: PParamArray); lape_extdecl
begin
  PButtonControl(Params^[0])^ := TButtonControl.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TButtonControl_Free(const Params: PParamArray); lape_extdecl
begin
  PButtonControl(Params^[0])^.Free();
end;

procedure Register_TButtonControl(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TButtonControl', 'TWinControl');

    addGlobalFunc('procedure TButtonControl.Init(TheOwner: TComponent);', @TButtonControl_Init);
    addGlobalFunc('procedure TButtonControl.Free();', @TButtonControl_Free);
  end;
end;

//constructor Create(TheOwner: TComponent);
procedure TCustomButton_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomButton(Params^[0])^ := TCustomButton.Create(PComponent(Params^[1])^);
end;

//procedure ExecuteDefaultAction;
procedure TCustomButton_ExecuteDefaultAction(const Params: PParamArray); lape_extdecl
begin
  PCustomButton(Params^[0])^.ExecuteDefaultAction();
end;

//procedure ExecuteCancelAction;
procedure TCustomButton_ExecuteCancelAction(const Params: PParamArray); lape_extdecl
begin
  PCustomButton(Params^[0])^.ExecuteCancelAction();
end;

//procedure ActiveDefaultControlChanged(NewControl: TControl);
procedure TCustomButton_ActiveDefaultControlChanged(const Params: PParamArray); lape_extdecl
begin
  PCustomButton(Params^[0])^.ActiveDefaultControlChanged(PControl(Params^[1])^);
end;

//procedure UpdateRolesForForm;
procedure TCustomButton_UpdateRolesForForm(const Params: PParamArray); lape_extdecl
begin
  PCustomButton(Params^[0])^.UpdateRolesForForm();
end;

//function UseRightToLeftAlignment: Boolean;
procedure TCustomButton_UseRightToLeftAlignment(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomButton(Params^[0])^.UseRightToLeftAlignment();
end;

//Read: property Active: boolean read Active;
procedure TCustomButton_Active_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomButton(Params^[0])^.Active;
end;

//Read: property Default: Boolean read Default write Default;
procedure TCustomButton_Default_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomButton(Params^[0])^.Default;
end;

//Write: property Default: Boolean read Default write Default;
procedure TCustomButton_Default_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomButton(Params^[0])^.Default := PBoolean(Params^[1])^;
end;

//Read: property Cancel: Boolean read Cancel write Cancel;
procedure TCustomButton_Cancel_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomButton(Params^[0])^.Cancel;
end;

//Write: property Cancel: Boolean read Cancel write Cancel;
procedure TCustomButton_Cancel_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomButton(Params^[0])^.Cancel := PBoolean(Params^[1])^;
end;

//procedure Free();
procedure TCustomButton_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomButton(Params^[0])^.Free();
end;

procedure Register_TCustomButton(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TCustomButton', 'TButtonControl');

    addGlobalFunc('procedure TCustomButton.Init(TheOwner: TComponent);', @TCustomButton_Init);
    addGlobalFunc('procedure TCustomButton.ExecuteDefaultAction();', @TCustomButton_ExecuteDefaultAction);
    addGlobalFunc('procedure TCustomButton.ExecuteCancelAction();', @TCustomButton_ExecuteCancelAction);
    addGlobalFunc('procedure TCustomButton.ActiveDefaultControlChanged(NewControl: TControl);', @TCustomButton_ActiveDefaultControlChanged);
    addGlobalFunc('procedure TCustomButton.UpdateRolesForForm();', @TCustomButton_UpdateRolesForForm);
    addGlobalFunc('function TCustomButton.UseRightToLeftAlignment(): Boolean;', @TCustomButton_UseRightToLeftAlignment);
    addClassVar(Compiler, 'TCustomButton', 'Active', 'boolean', @TCustomButton_Active_Read, nil);
    addClassVar(Compiler, 'TCustomButton', 'Default', 'Boolean', @TCustomButton_Default_Read, @TCustomButton_Default_Write);
    addClassVar(Compiler, 'TCustomButton', 'Cancel', 'Boolean', @TCustomButton_Cancel_Read, @TCustomButton_Cancel_Write);
    addGlobalFunc('procedure TCustomButton.Free();', @TCustomButton_Free);
  end;
end;

//constructor Create(TheOwner: TComponent);
procedure TButton_Init(const Params: PParamArray); lape_extdecl
begin
  PButton(Params^[0])^ := TButton.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TButton_Free(const Params: PParamArray); lape_extdecl
begin
  PButton(Params^[0])^.Free();
end;

procedure Register_TButton(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TButton', 'TCustomButton');

    addGlobalFunc('procedure TButton.Init(TheOwner: TComponent);', @TButton_Init);
    addGlobalFunc('procedure TButton.Free();', @TButton_Free);
  end;
end;
{TCheckBox}

//constructor Create(TheOwner: TComponent);
procedure TCustomCheckBox_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomCheckBox(Params^[0])^ := TCustomCheckBox.Create(PComponent(Params^[1])^);
end;

//Read: property AllowGrayed: Boolean read AllowGrayed write AllowGrayed;
procedure TCustomCheckBox_AllowGrayed_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomCheckBox(Params^[0])^.AllowGrayed;
end;

//Write: property AllowGrayed: Boolean read AllowGrayed write AllowGrayed;
procedure TCustomCheckBox_AllowGrayed_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomCheckBox(Params^[0])^.AllowGrayed := PBoolean(Params^[1])^;
end;

//Read: property State: TCheckBoxState read State write State;
procedure TCustomCheckBox_State_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCheckBoxState(Result)^ := PCustomCheckBox(Params^[0])^.State;
end;

//Write: property State: TCheckBoxState read State write State;
procedure TCustomCheckBox_State_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomCheckBox(Params^[0])^.State := PCheckBoxState(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure TCustomCheckBox_OnChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCustomCheckBox(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure TCustomCheckBox_OnChange_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomCheckBox(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//procedure Free();
procedure TCustomCheckBox_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomCheckBox(Params^[0])^.Free();
end;

procedure Register_TCustomCheckBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TCustomCheckBox', 'TButtonControl');

    addGlobalFunc('procedure TCustomCheckBox.Init(TheOwner: TComponent);', @TCustomCheckBox_Init);
    addClassVar(Compiler, 'TCustomCheckBox', 'AllowGrayed', 'Boolean', @TCustomCheckBox_AllowGrayed_Read, @TCustomCheckBox_AllowGrayed_Write);
    addClassVar(Compiler, 'TCustomCheckBox', 'State', 'TCheckBoxState', @TCustomCheckBox_State_Read, @TCustomCheckBox_State_Write);
    addClassVar(Compiler, 'TCustomCheckBox', 'OnChange', 'TNotifyEvent', @TCustomCheckBox_OnChange_Read, @TCustomCheckBox_OnChange_Write);
    addGlobalFunc('procedure TCustomCheckBox.Free();', @TCustomCheckBox_Free);
  end;
end;


//constructor Create(TheOwner: TComponent);
procedure TCheckBox_Init(const Params: PParamArray); lape_extdecl
begin
  PCheckBox(Params^[0])^ := TCheckBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TCheckBox_Free(const Params: PParamArray); lape_extdecl
begin
  PCheckBox(Params^[0])^.Free();
end;

procedure Register_TCheckBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TCheckBox', 'TCustomCheckBox');

    addGlobalFunc('procedure TCheckBox.Init(TheOwner: TComponent);', @TCheckBox_Init);
    addGlobalFunc('procedure TCheckBox.Free();', @TCheckBox_Free);
  end;
end;
{TLabel}

//constructor Create(TheOwner: TComponent);
procedure TCustomLabel_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomLabel(Params^[0])^ := TCustomLabel.Create(PComponent(Params^[1])^);
end;

//function ColorIsStored: boolean;
procedure TCustomLabel_ColorIsStored(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomLabel(Params^[0])^.ColorIsStored();
end;

//function AdjustFontForOptimalFill: Boolean;
procedure TCustomLabel_AdjustFontForOptimalFill(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomLabel(Params^[0])^.AdjustFontForOptimalFill();
end;

//procedure Paint;
procedure TCustomLabel_Paint(const Params: PParamArray); lape_extdecl
begin
  PCustomLabel(Params^[0])^.Paint();
end;

//procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer);
procedure TCustomLabel_SetBounds(const Params: PParamArray); lape_extdecl
begin
  PCustomLabel(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure Free();
procedure TCustomLabel_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomLabel(Params^[0])^.Free();
end;

procedure Register_TCustomLabel(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TCustomLabel', 'TGraphicControl');

    addGlobalFunc('procedure TCustomLabel.Init(TheOwner: TComponent);', @TCustomLabel_Init);
    addGlobalFunc('function TCustomLabel.ColorIsStored(): boolean;', @TCustomLabel_ColorIsStored);
    addGlobalFunc('function TCustomLabel.AdjustFontForOptimalFill(): Boolean;', @TCustomLabel_AdjustFontForOptimalFill);
    addGlobalFunc('procedure TCustomLabel.Paint();', @TCustomLabel_Paint);
    addGlobalFunc('procedure TCustomLabel.SetBounds(aLeft, aTop, aWidth, aHeight: integer);', @TCustomLabel_SetBounds);
    addGlobalFunc('procedure TCustomLabel.Free();', @TCustomLabel_Free);
  end;
end;
//constructor Create(TheOwner: TComponent);
procedure TLabel_Init(const Params: PParamArray); lape_extdecl
begin
  PLabel(Params^[0])^ := TLabel.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TLabel_Free(const Params: PParamArray); lape_extdecl
begin
  PLabel(Params^[0])^.Free();
end;

procedure Register_TLabel(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, 'TLabel', 'TCustomLabel');

    addGlobalFunc('procedure TLabel.Init(TheOwner: TComponent);', @TLabel_Init);
    addGlobalFunc('procedure TLabel.Free();', @TLabel_Free);
  end;
end;
{}
procedure RegisterLCLStdCtrls(Compiler: TLapeCompiler);
begin
  with Compiler do
   begin
     addGlobalType('(ssNone, ssHorizontal, ssVertical, ssBoth,ssAutoHorizontal, ssAutoVertical, ssAutoBoth)','TScrollStyle');
     addGlobalType('(scLineUp,scLineDown, scPageUp,scPageDown,scPosition, scTrack,scTop,scBottom,scEndScroll)','TScrollCode');
     addGlobalType('procedure(Sender: TObject; ScrollCode: TScrollCode;var ScrollPos: Integer)','TScrollEvent');
     addGlobalType('(csDropDown,csSimple,csDropDownList,csOwnerDrawFixed,csOwnerDrawVariable)','TComboBoxStyle');
     addGlobalType('(sbsNone, sbsSingle, sbsSunken)','TStaticBorderStyle');
     addGlobalType('(taLeftJustify, taRightJustify, taCenter)','TAlignment');
     addGlobalType('(cbUnchecked, cbChecked, cbGrayed)','TCheckBoxState');
   end;
  Register_TCustomScrollBar(Compiler);
  Register_TScrollBar(Compiler);
  Register_TCustomComboBox(Compiler);
  Register_TComboBox(Compiler);
  Register_TCustomListBox(Compiler);
  Register_TListBox(Compiler);
  Register_TCustomEdit(Compiler);
  Register_TEdit(Compiler);
  Register_TMemoScrollbar(Compiler);
  Register_TCustomMemo(Compiler);
  Register_TMemo(Compiler);
  Register_TCustomStaticText(Compiler);
  Register_TStaticText(Compiler);
  Register_TButtonControl(Compiler);
  Register_TCustomButton(Compiler);
  Register_TButton(Compiler);
  Register_TCustomCheckBox(Compiler);
  Register_TCheckBox(Compiler);
  Register_TCustomLabel(Compiler);
  Register_TLabel(Compiler);

end;

end.

