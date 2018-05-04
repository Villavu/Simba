unit lplclstdctrls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, ffi, script_imports;

procedure RegisterLCLStdCtrls(Compiler: TLapeCompiler);

implementation
  uses MufasaTypes,stdctrls,forms,lplclsystem,lplclgraphics,lplclcontrols, Buttons;

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
  PListBoxStyle = ^TListBoxStyle;
  PCustomListBox = ^TCustomListBox;
  PListBox = ^TListBox;
  //TEdit
  PCustomEdit =^TCustomEdit;
  PEdit = ^TEdit;
  //TGroupBox
  PCustomGroupBox = ^TCustomGroupBox;
  PGroupBox = ^TGroupBox;
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
  //TSpeedButton
  PCustomSpeedButton = ^TCustomSpeedButton;
  PSpeedButton = ^TSpeedButton;
  PButtonLayout = ^TButtonLayout;
  PDrawItemEvent = ^TDrawItemEvent;

{TCustomScrollBar}

//constructor Create(AOwner: TComponent);
procedure TCustomScrollBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^ := TCustomScrollBar.Create(PComponent(Params^[1])^);
end;

//procedure SetParams(APosition, AMin, AMax, APageSize: Integer);
procedure TCustomScrollBar_SetParams(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.SetParams(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

//procedure SetParams(APosition, AMin, AMax: Integer);
procedure TCustomScrollBar_SetParamsEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.SetParams(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

//Read: property Kind: TScrollBarKind read Kind write Kind;
procedure TCustomScrollBar_Kind_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollBarKind(Result)^ := PCustomScrollBar(Params^[0])^.Kind;
end;

//Write: property Kind: TScrollBarKind read Kind write Kind;
procedure TCustomScrollBar_Kind_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.Kind := PScrollBarKind(Params^[1])^;
end;

//Read: property LargeChange: integer read LargeChange write LargeChange;
procedure TCustomScrollBar_LargeChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomScrollBar(Params^[0])^.LargeChange;
end;

//Write: property LargeChange: integer read LargeChange write LargeChange;
procedure TCustomScrollBar_LargeChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.LargeChange := Pinteger(Params^[1])^;
end;

//Read: property Max: Integer read Max write Max;
procedure TCustomScrollBar_Max_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.Max;
end;

//Write: property Max: Integer read Max write Max;
procedure TCustomScrollBar_Max_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.Max := PInteger(Params^[1])^;
end;

//Read: property Min: Integer read Min write Min;
procedure TCustomScrollBar_Min_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.Min;
end;

//Write: property Min: Integer read Min write Min;
procedure TCustomScrollBar_Min_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.Min := PInteger(Params^[1])^;
end;

//Read: property PageSize: Integer read PageSize write PageSize;
procedure TCustomScrollBar_PageSize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.PageSize;
end;

//Write: property PageSize: Integer read PageSize write PageSize;
procedure TCustomScrollBar_PageSize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.PageSize := PInteger(Params^[1])^;
end;

//Read: property Position: Integer read Position write Position;
procedure TCustomScrollBar_Position_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.Position;
end;

//Write: property Position: Integer read Position write Position;
procedure TCustomScrollBar_Position_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

//Read: property SmallChange: integer read SmallChange write SmallChange;
procedure TCustomScrollBar_SmallChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomScrollBar(Params^[0])^.SmallChange;
end;

//Write: property SmallChange: integer read SmallChange write SmallChange;
procedure TCustomScrollBar_SmallChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.SmallChange := Pinteger(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure TCustomScrollBar_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomScrollBar(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure TCustomScrollBar_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//Read: property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
procedure TCustomScrollBar_OnScroll_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollEvent(Result)^ := PCustomScrollBar(Params^[0])^.OnScroll;
end;

//Write: property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
procedure TCustomScrollBar_OnScroll_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.OnScroll := PScrollEvent(Params^[1])^;
end;

//procedure Free();
procedure TCustomScrollBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.Free();
end;

procedure Register_TCustomScrollBar(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomScrollBar', 'TWinControl');

    addGlobalFunc('procedure TCustomScrollBar.Init(AOwner: TComponent);', @TCustomScrollBar_Init);
    addGlobalFunc('procedure TCustomScrollBar.SetParams(APosition, AMin, AMax, APageSize: Integer); constref;', @TCustomScrollBar_SetParams);
    addGlobalFunc('procedure TCustomScrollBar.SetParams(APosition, AMin, AMax: Integer); constref; overload;', @TCustomScrollBar_SetParamsEx);
    addClassVar('TCustomScrollBar', 'Kind', 'TScrollBarKind', @TCustomScrollBar_Kind_Read, @TCustomScrollBar_Kind_Write);
    addClassVar('TCustomScrollBar', 'LargeChange', 'integer', @TCustomScrollBar_LargeChange_Read, @TCustomScrollBar_LargeChange_Write);
    addClassVar('TCustomScrollBar', 'Max', 'Integer', @TCustomScrollBar_Max_Read, @TCustomScrollBar_Max_Write);
    addClassVar('TCustomScrollBar', 'Min', 'Integer', @TCustomScrollBar_Min_Read, @TCustomScrollBar_Min_Write);
    addClassVar('TCustomScrollBar', 'PageSize', 'Integer', @TCustomScrollBar_PageSize_Read, @TCustomScrollBar_PageSize_Write);
    addClassVar('TCustomScrollBar', 'Position', 'Integer', @TCustomScrollBar_Position_Read, @TCustomScrollBar_Position_Write);
    addClassVar('TCustomScrollBar', 'SmallChange', 'integer', @TCustomScrollBar_SmallChange_Read, @TCustomScrollBar_SmallChange_Write);
    addClassVar('TCustomScrollBar', 'OnChange', 'TNotifyEvent', @TCustomScrollBar_OnChange_Read, @TCustomScrollBar_OnChange_Write);
    addClassVar('TCustomScrollBar', 'OnScroll', 'TScrollEvent', @TCustomScrollBar_OnScroll_Read, @TCustomScrollBar_OnScroll_Write);
    addGlobalFunc('procedure TCustomScrollBar.Free(); constref;', @TCustomScrollBar_Free);
  end;
end;

{TScrollBar}

//constructor Create();
procedure TScrollBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollBar(Params^[0])^ := TScrollBar.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TScrollBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollBar(Params^[0])^.Free();
end;

procedure Register_TScrollBar(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TScrollBar', 'TCustomScrollBar');

    addGlobalFunc('procedure TScrollBar.Init(AOwner: TComponent);', @TScrollBar_Init);
    addGlobalFunc('procedure TScrollBar.Free(); constref;', @TScrollBar_Free);
  end;
end;

{TCombobox and TCustomComboBox}

//constructor Create(TheOwner: TComponent);
procedure TCustomComboBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^ := TCustomComboBox.Create(PComponent(Params^[1])^);
end;

//procedure IntfGetItems;
procedure TCustomComboBox_IntfGetItems(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.IntfGetItems();
end;

//procedure AddItem(const Item: String; AnObject: TObject);
procedure TCustomComboBox_AddItem(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AddItem(PlpString(Params^[1])^, PObject(Params^[2])^);
end;

//procedure AddHistoryItem(const Item: string; MaxHistoryCount: integer;SetAsText, CaseSensitive: boolean);
procedure TCustomComboBox_AddHistoryItem(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AddHistoryItem(PlpString(Params^[1])^, Pinteger(Params^[2])^, Pboolean(Params^[3])^, Pboolean(Params^[4])^);
end;

//procedure AddHistoryItem(const Item: string; AnObject: TObject;MaxHistoryCount: integer; SetAsText, CaseSensitive: boolean);
procedure TCustomComboBox_AddHistoryItemEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AddHistoryItem(PlpString(Params^[1])^, PObject(Params^[2])^, Pinteger(Params^[3])^, Pboolean(Params^[4])^, Pboolean(Params^[5])^);
end;

//procedure Clear;
procedure TCustomComboBox_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.Clear();
end;

//procedure ClearSelection;
procedure TCustomComboBox_ClearSelection(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.ClearSelection();
end;

//Read: property DroppedDown: Boolean read DroppedDown write DroppedDown;
procedure TCustomComboBox_DroppedDown_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.DroppedDown;
end;

//Write: property DroppedDown: Boolean read DroppedDown write DroppedDown;
procedure TCustomComboBox_DroppedDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.DroppedDown := PBoolean(Params^[1])^;
end;

//procedure SelectAll;
procedure TCustomComboBox_SelectAll(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.SelectAll();
end;

//Read: property AutoComplete: boolean read AutoComplete write AutoComplete;
procedure TCustomComboBox_AutoComplete_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomComboBox(Params^[0])^.AutoComplete;
end;

//Write: property AutoComplete: boolean read AutoComplete write AutoComplete;
procedure TCustomComboBox_AutoComplete_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AutoComplete := Pboolean(Params^[1])^;
end;

//Read: property AutoDropDown: Boolean read AutoDropDown write AutoDropDown;
procedure TCustomComboBox_AutoDropDown_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoDropDown;
end;

//Write: property AutoDropDown: Boolean read AutoDropDown write AutoDropDown;
procedure TCustomComboBox_AutoDropDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AutoDropDown := PBoolean(Params^[1])^;
end;

//Read: property AutoSelect: Boolean read AutoSelect write AutoSelect;
procedure TCustomComboBox_AutoSelect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoSelect;
end;

//Write: property AutoSelect: Boolean read AutoSelect write AutoSelect;
procedure TCustomComboBox_AutoSelect_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AutoSelect := PBoolean(Params^[1])^;
end;

//Read: property AutoSelected: Boolean read AutoSelected write AutoSelected;
procedure TCustomComboBox_AutoSelected_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoSelected;
end;

//Write: property AutoSelected: Boolean read AutoSelected write AutoSelected;
procedure TCustomComboBox_AutoSelected_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AutoSelected := PBoolean(Params^[1])^;
end;

//Read: property ArrowKeysTraverseList: Boolean read ArrowKeysTraverseList write ArrowKeysTraverseList;
procedure TCustomComboBox_ArrowKeysTraverseList_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.ArrowKeysTraverseList;
end;

//Write: property ArrowKeysTraverseList: Boolean read ArrowKeysTraverseList write ArrowKeysTraverseList;
procedure TCustomComboBox_ArrowKeysTraverseList_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.ArrowKeysTraverseList := PBoolean(Params^[1])^;
end;

//Read: property Canvas: TCanvas read FCanvas;
procedure TCustomComboBox_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PCustomComboBox(Params^[0])^.Canvas;
end;

//Read: property DropDownCount: Integer read DropDownCount write DropDownCount default 8;
procedure TCustomComboBox_DropDownCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomComboBox(Params^[0])^.DropDownCount;
end;

//Write: property DropDownCount: Integer read DropDownCount write DropDownCount default 8;
procedure TCustomComboBox_DropDownCount_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.DropDownCount := PInteger(Params^[1])^;
end;

//Read: property Items: TStrings read Items write Items;
procedure TCustomComboBox_Items_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PCustomComboBox(Params^[0])^.Items;
end;

//Write: property Items: TStrings read Items write Items;
procedure TCustomComboBox_Items_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.Items := PStrings(Params^[1])^;
end;

//Read: property ItemIndex: integer read ItemIndex write ItemIndex;
procedure TCustomComboBox_ItemIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomComboBox(Params^[0])^.ItemIndex;
end;

//Write: property ItemIndex: integer read ItemIndex write ItemIndex;
procedure TCustomComboBox_ItemIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.ItemIndex := Pinteger(Params^[1])^;
end;

//Read: property ReadOnly: Boolean read ReadOnly write ReadOnly;
procedure TCustomComboBox_ReadOnly_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.ReadOnly;
end;

//Write: property ReadOnly: Boolean read ReadOnly write ReadOnly;
procedure TCustomComboBox_ReadOnly_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.ReadOnly := PBoolean(Params^[1])^;
end;

//Read: property SelLength: integer read SelLength write SelLength;
procedure TCustomComboBox_SelLength_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomComboBox(Params^[0])^.SelLength;
end;

//Write: property SelLength: integer read SelLength write SelLength;
procedure TCustomComboBox_SelLength_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.SelLength := Pinteger(Params^[1])^;
end;

//Read: property SelStart: integer read SelStart write SelStart;
procedure TCustomComboBox_SelStart_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomComboBox(Params^[0])^.SelStart;
end;

//Write: property SelStart: integer read SelStart write SelStart;
procedure TCustomComboBox_SelStart_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.SelStart := Pinteger(Params^[1])^;
end;

//Read: property SelText: String read SelText write SetSelText;
procedure TCustomComboBox_SelText_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomComboBox(Params^[0])^.SelText;
end;

//Write: property SelText: String read SelText write SetSelText;
procedure TCustomComboBox_SelText_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.SelText := PlpString(Params^[1])^;
end;

//Read: property Style: TComboBoxStyle read FStyle write SetStyle default csDropDown;
procedure TCustomComboBox_Style_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComboBoxStyle(Result)^ := PCustomComboBox(Params^[0])^.Style;
end;

//Write: property Style: TComboBoxStyle read FStyle write SetStyle default csDropDown;
procedure TCustomComboBox_Style_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.Style := PComboBoxStyle(Params^[1])^;
end;

//Read: property Text: string read Text write Text;
procedure TCustomComboBox_Text_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomComboBox(Params^[0])^.Text;
end;

//Write: property Text: string read Text write Text;
procedure TCustomComboBox_Text_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.Text := PlpString(Params^[1])^;
end;

//procedure Free();
procedure TCustomComboBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.Free();
end;

procedure Register_TCustomComboBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomComboBox', 'TWinControl');

    addGlobalFunc('procedure TCustomComboBox.Init(TheOwner: TComponent);', @TCustomComboBox_Init);
    addGlobalFunc('procedure TCustomComboBox.IntfGetItems(); constref;', @TCustomComboBox_IntfGetItems);
    addGlobalFunc('procedure TCustomComboBox.AddItem(const Item: String; AnObject: TObject); constref;', @TCustomComboBox_AddItem);
    addGlobalFunc('procedure TCustomComboBox.AddHistoryItem(const Item: string; MaxHistoryCount: integer;SetAsText, CaseSensitive: boolean); constref;', @TCustomComboBox_AddHistoryItem);
    addGlobalFunc('procedure TCustomComboBox.AddHistoryItem(const Item: string; AnObject: TObject;MaxHistoryCount: integer; SetAsText, CaseSensitive: boolean); constref; overload;', @TCustomComboBox_AddHistoryItemEx);
    addGlobalFunc('procedure TCustomComboBox.Clear(); constref;', @TCustomComboBox_Clear);
    addGlobalFunc('procedure TCustomComboBox.ClearSelection(); constref;', @TCustomComboBox_ClearSelection);
    addClassVar('TCustomComboBox', 'DroppedDown', 'Boolean', @TCustomComboBox_DroppedDown_Read, @TCustomComboBox_DroppedDown_Write);
    addGlobalFunc('procedure TCustomComboBox.SelectAll(); constref;', @TCustomComboBox_SelectAll);
    addClassVar('TCustomComboBox', 'AutoComplete', 'boolean', @TCustomComboBox_AutoComplete_Read, @TCustomComboBox_AutoComplete_Write);
    addClassVar('TCustomComboBox', 'AutoDropDown', 'Boolean', @TCustomComboBox_AutoDropDown_Read, @TCustomComboBox_AutoDropDown_Write);
    addClassVar('TCustomComboBox', 'AutoSelect', 'Boolean', @TCustomComboBox_AutoSelect_Read, @TCustomComboBox_AutoSelect_Write);
    addClassVar('TCustomComboBox', 'AutoSelected', 'Boolean', @TCustomComboBox_AutoSelected_Read, @TCustomComboBox_AutoSelected_Write);
    addClassVar('TCustomComboBox', 'ArrowKeysTraverseList', 'Boolean', @TCustomComboBox_ArrowKeysTraverseList_Read, @TCustomComboBox_ArrowKeysTraverseList_Write);
    addClassVar('TCustomComboBox', 'Canvas', 'TCanvas', @TCustomComboBox_Canvas_Read);
    addClassVar('TCustomComboBox', 'DropDownCount', 'Integer', @TCustomComboBox_DropDownCount_Read, @TCustomComboBox_DropDownCount_Write);
    addClassVar('TCustomComboBox', 'Items', 'TStrings', @TCustomComboBox_Items_Read, @TCustomComboBox_Items_Write);
    addClassVar('TCustomComboBox', 'ItemIndex', 'integer', @TCustomComboBox_ItemIndex_Read, @TCustomComboBox_ItemIndex_Write);
    addClassVar('TCustomComboBox', 'ReadOnly', 'Boolean', @TCustomComboBox_ReadOnly_Read, @TCustomComboBox_ReadOnly_Write);
    addClassVar('TCustomComboBox', 'SelLength', 'integer', @TCustomComboBox_SelLength_Read, @TCustomComboBox_SelLength_Write);
    addClassVar('TCustomComboBox', 'SelStart', 'integer', @TCustomComboBox_SelStart_Read, @TCustomComboBox_SelStart_Write);
    addClassVar('TCustomComboBox', 'SelText', 'String', @TCustomComboBox_SelText_Read, @TCustomComboBox_SelText_Write);
    addClassVar('TCustomComboBox', 'Style', 'TComboBoxStyle', @TCustomComboBox_Style_Read, @TCustomComboBox_Style_Write);
    addClassVar('TCustomComboBox', 'Text', 'string', @TCustomComboBox_Text_Read, @TCustomComboBox_Text_Write);
    addGlobalFunc('procedure TCustomComboBox.Free(); constref;', @TCustomComboBox_Free);
  end;
end;

//constructor Create(TheOwner: TComponent);
procedure TComboBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComboBox(Params^[0])^ := TComboBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TComboBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComboBox(Params^[0])^.Free();
end;

procedure Register_TComboBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TComboBox', 'TCustomComboBox');

    addGlobalFunc('procedure TComboBox.Init(TheOwner: TComponent);', @TComboBox_Init);
    addGlobalFunc('procedure TComboBox.Free(); constref;', @TComboBox_Free);
  end;
end;

{TListBox}
//constructor Create(TheOwner: TComponent);
procedure TCustomListBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^ := TCustomListBox.Create(PComponent(Params^[1])^);
end;

//procedure AddItem(const Item: String; AnObject: TObject);
procedure TCustomListBox_AddItem(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.AddItem(PlpString(Params^[1])^, PObject(Params^[2])^);
end;

//procedure Click;
procedure TCustomListBox_Click(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Click();
end;

//procedure Clear;
procedure TCustomListBox_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Clear();
end;

//procedure ClearSelection;
procedure TCustomListBox_ClearSelection(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ClearSelection();
end;

//function GetIndexAtXY(X, Y: integer): integer;
procedure TCustomListBox_GetIndexAtXY(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.GetIndexAtXY(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function GetIndexAtY(Y: integer): integer;
procedure TCustomListBox_GetIndexAtY(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.GetIndexAtY(Pinteger(Params^[1])^);
end;

//function GetSelectedText: string;
procedure TCustomListBox_GetSelectedText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomListBox(Params^[0])^.GetSelectedText();
end;

//function ItemAtPos(const Pos: TPoint; Existing: Boolean): Integer;
procedure TCustomListBox_ItemAtPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.ItemAtPos(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

//function ItemRect(Index: Integer): TRect;
procedure TCustomListBox_ItemRect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRect(Result)^ := PCustomListBox(Params^[0])^.ItemRect(PInteger(Params^[1])^);
end;

//function ItemVisible(Index: Integer): boolean;
procedure TCustomListBox_ItemVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ItemVisible(PInteger(Params^[1])^);
end;

//function ItemFullyVisible(Index: Integer): boolean;
procedure TCustomListBox_ItemFullyVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ItemFullyVisible(PInteger(Params^[1])^);
end;

//procedure LockSelectionChange;
procedure TCustomListBox_LockSelectionChange(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.LockSelectionChange();
end;

//procedure MakeCurrentVisible;
procedure TCustomListBox_MakeCurrentVisible(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.MakeCurrentVisible();
end;

//procedure MeasureItem(Index: Integer; var TheHeight: Integer); virtual;
procedure TCustomListBox_MeasureItem(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.MeasureItem(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure SelectAll; virtual;
procedure TCustomListBox_SelectAll(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.SelectAll();
end;

//procedure UnlockSelectionChange;
procedure TCustomListBox_UnlockSelectionChange(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.UnlockSelectionChange();
end;

//Read: property Canvas: TCanvas read FCanvas;
procedure TCustomListBox_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PCustomListBox(Params^[0])^.Canvas;
end;

//Read: property ClickOnSelChange: boolean read ClickOnSelChange write ClickOnSelChange;
procedure TCustomListBox_ClickOnSelChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ClickOnSelChange;
end;

//Write: property ClickOnSelChange: boolean read ClickOnSelChange write ClickOnSelChange;
procedure TCustomListBox_ClickOnSelChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ClickOnSelChange := Pboolean(Params^[1])^;
end;

//Read: property Columns: Integer read Columns write Columns ;
procedure TCustomListBox_Columns_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.Columns;
end;

//Write: property Columns: Integer read Columns write Columns ;
procedure TCustomListBox_Columns_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Columns := PInteger(Params^[1])^;
end;

//Read: property Count: Integer read Count;
procedure TCustomListBox_Count_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.Count;
end;

//Read: property ExtendedSelect: boolean read ExtendedSelect write ExtendedSelect;
procedure TCustomListBox_ExtendedSelect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ExtendedSelect;
end;

//Write: property ExtendedSelect: boolean read ExtendedSelect write ExtendedSelect;
procedure TCustomListBox_ExtendedSelect_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ExtendedSelect := Pboolean(Params^[1])^;
end;

//Read: property ItemHeight: Integer read ItemHeight write ItemHeight;
procedure TCustomListBox_ItemHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.ItemHeight;
end;

//Write: property ItemHeight: Integer read ItemHeight write ItemHeight;
procedure TCustomListBox_ItemHeight_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ItemHeight := PInteger(Params^[1])^;
end;

//Read: property ItemIndex: integer read ItemIndex write ItemIndex;
procedure TCustomListBox_ItemIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.ItemIndex;
end;

//Write: property ItemIndex: integer read ItemIndex write ItemIndex;
procedure TCustomListBox_ItemIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ItemIndex := Pinteger(Params^[1])^;
end;

//Read: property Items: TStrings read Items write Items;
procedure TCustomListBox_Items_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PCustomListBox(Params^[0])^.Items;
end;

//Write: property Items: TStrings read Items write Items;
procedure TCustomListBox_Items_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Items := PStrings(Params^[1])^;
end;

//Read: property MultiSelect: boolean read MultiSelect write MultiSelect;
procedure TCustomListBox_MultiSelect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.MultiSelect;
end;

//Write: property MultiSelect: boolean read MultiSelect write MultiSelect;
procedure TCustomListBox_MultiSelect_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.MultiSelect := Pboolean(Params^[1])^;
end;

//Read: property ScrollWidth: Integer read ScrollWidth write ScrollWidth;
procedure TCustomListBox_ScrollWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.ScrollWidth;
end;

//Write: property ScrollWidth: Integer read ScrollWidth write ScrollWidth;
procedure TCustomListBox_ScrollWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ScrollWidth := PInteger(Params^[1])^;
end;

//Read: property SelCount: integer read SelCount;
procedure TCustomListBox_SelCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.SelCount;
end;

//Read: property Sorted: boolean read Sorted write Sorted;
procedure TCustomListBox_Sorted_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.Sorted;
end;

//Write: property Sorted: boolean read Sorted write Sorted;
procedure TCustomListBox_Sorted_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Sorted := Pboolean(Params^[1])^;
end;

//Read: property TopIndex: Integer read TopIndex write TopIndex;
procedure TCustomListBox_TopIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.TopIndex;
end;

//Write: property TopIndex: Integer read TopIndex write TopIndex;
procedure TCustomListBox_TopIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.TopIndex := PInteger(Params^[1])^;
end;

//Read: property Style: TListBoxStyle;
procedure TCustomListBox_Style_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PListBoxStyle(Result)^ := PCustomListBox(Params^[0])^.Style;
end;

//Write: property Style: TListBoxStyle;
procedure TCustomListBox_Style_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Style := PListBoxStyle(Params^[1])^;
end;

//procedure Free();
procedure TCustomListBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Free();
end;

procedure TCustomListBox_OnDrawItem_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.OnDrawItem := PDrawItemEvent(Params^[1])^;
end;

procedure Register_TCustomListBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomListBox', 'TWinControl');

    addGlobalFunc('procedure TCustomListBox.Init(TheOwner: TComponent);', @TCustomListBox_Init);
    addGlobalFunc('procedure TCustomListBox.AddItem(const Item: String; AnObject: TObject); constref;', @TCustomListBox_AddItem);
    addGlobalFunc('procedure TCustomListBox.Click(); constref;', @TCustomListBox_Click);
    addGlobalFunc('procedure TCustomListBox.Clear(); constref;', @TCustomListBox_Clear);
    addGlobalFunc('procedure TCustomListBox.ClearSelection(); constref;', @TCustomListBox_ClearSelection);
    addGlobalFunc('function TCustomListBox.GetIndexAtXY(X, Y: integer): integer; constref;', @TCustomListBox_GetIndexAtXY);
    addGlobalFunc('function TCustomListBox.GetIndexAtY(Y: integer): integer; constref;', @TCustomListBox_GetIndexAtY);
    addGlobalFunc('function TCustomListBox.GetSelectedText(): string; constref;', @TCustomListBox_GetSelectedText);
    addGlobalFunc('function TCustomListBox.ItemAtPos(const Pos: TPoint; Existing: Boolean): Integer; constref;', @TCustomListBox_ItemAtPos);
    addGlobalFunc('function TCustomListBox.ItemRect(Index: Integer): TRect; constref;', @TCustomListBox_ItemRect);
    addGlobalFunc('function TCustomListBox.ItemVisible(Index: Integer): boolean; constref;', @TCustomListBox_ItemVisible);
    addGlobalFunc('function TCustomListBox.ItemFullyVisible(Index: Integer): boolean; constref;', @TCustomListBox_ItemFullyVisible);
    addGlobalFunc('procedure TCustomListBox.LockSelectionChange(); constref;', @TCustomListBox_LockSelectionChange);
    addGlobalFunc('procedure TCustomListBox.MakeCurrentVisible(); constref;', @TCustomListBox_MakeCurrentVisible);
    addGlobalFunc('procedure TCustomListBox.MeasureItem(Index: Integer; var TheHeight: Integer); constref;', @TCustomListBox_MeasureItem);
    addGlobalFunc('procedure TCustomListBox.SelectAll(); constref;', @TCustomListBox_SelectAll);
    addGlobalFunc('procedure TCustomListBox.UnlockSelectionChange(); constref;', @TCustomListBox_UnlockSelectionChange);
    addClassVar('TCustomListBox', 'Canvas', 'TCanvas', @TCustomListBox_Canvas_Read);
    addClassVar('TCustomListBox', 'ClickOnSelChange', 'boolean', @TCustomListBox_ClickOnSelChange_Read, @TCustomListBox_ClickOnSelChange_Write);
    addClassVar('TCustomListBox', 'Columns', 'Integer', @TCustomListBox_Columns_Read, @TCustomListBox_Columns_Write);
    addClassVar('TCustomListBox', 'Count', 'Integer', @TCustomListBox_Count_Read);
    addClassVar('TCustomListBox', 'ExtendedSelect', 'boolean', @TCustomListBox_ExtendedSelect_Read, @TCustomListBox_ExtendedSelect_Write);
    addClassVar('TCustomListBox', 'ItemHeight', 'Integer', @TCustomListBox_ItemHeight_Read, @TCustomListBox_ItemHeight_Write);
    addClassVar('TCustomListBox', 'ItemIndex', 'integer', @TCustomListBox_ItemIndex_Read, @TCustomListBox_ItemIndex_Write);
    addClassVar('TCustomListBox', 'Items', 'TStrings', @TCustomListBox_Items_Read, @TCustomListBox_Items_Write);
    addClassVar('TCustomListBox', 'MultiSelect', 'boolean', @TCustomListBox_MultiSelect_Read, @TCustomListBox_MultiSelect_Write);
    addClassVar('TCustomListBox', 'ScrollWidth', 'Integer', @TCustomListBox_ScrollWidth_Read, @TCustomListBox_ScrollWidth_Write);
    addClassVar('TCustomListBox', 'SelCount', 'integer', @TCustomListBox_SelCount_Read);
    addClassVar('TCustomListBox', 'Sorted', 'boolean', @TCustomListBox_Sorted_Read, @TCustomListBox_Sorted_Write);
    addClassVar('TCustomListBox', 'TopIndex', 'Integer', @TCustomListBox_TopIndex_Read, @TCustomListBox_TopIndex_Write);
    addClassVar('TCustomListBox', 'OnDrawItem', 'TDrawItemEvent', nil, @TCustomListBox_OnDrawItem_Write);
    addClassVar('TCustomListBox', 'Style', 'TListBoxStyle', @TCustomListBox_Style_Read, @TCustomListBox_Style_Write);
    addGlobalFunc('procedure TCustomListBox.Free(); constref;', @TCustomListBox_Free);
  end;
end;

//constructor Create(TheOwner: TComponent);
procedure TListBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PListBox(Params^[0])^ := TListBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TListBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PListBox(Params^[0])^.Free();
end;

procedure Register_TListBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TListBox', 'TCustomListBox');

    addGlobalFunc('procedure TListBox.Init(TheOwner: TComponent);', @TListBox_Init);
    addGlobalFunc('procedure TListBox.Free(); constref;', @TListBox_Free);
  end;
end;

{TEdit}
//constructor Create(AOwner: TComponent);
procedure TCustomEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^ := TCustomEdit.Create(PComponent(Params^[1])^);
end;

//procedure Clear;
procedure TCustomEdit_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.Clear();
end;

//procedure SelectAll;
procedure TCustomEdit_SelectAll(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.SelectAll();
end;

//procedure ClearSelection;
procedure TCustomEdit_ClearSelection(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.ClearSelection();
end;

//procedure CopyToClipboard;
procedure TCustomEdit_CopyToClipboard(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.CopyToClipboard();
end;

//procedure CutToClipboard;
procedure TCustomEdit_CutToClipboard(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.CutToClipboard();
end;

//procedure PasteFromClipboard;
procedure TCustomEdit_PasteFromClipboard(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.PasteFromClipboard();
end;

//procedure Undo;
procedure TCustomEdit_Undo(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.Undo();
end;

//Read: property CanUndo: Boolean read CanUndo;
procedure TCustomEdit_CanUndo_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.CanUndo;
end;

//Read: property CaretPos: TPoint read CaretPos write CaretPos;
procedure TCustomEdit_CaretPos_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PCustomEdit(Params^[0])^.CaretPos;
end;

//Write: property CaretPos: TPoint read CaretPos write CaretPos;
procedure TCustomEdit_CaretPos_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.CaretPos := PPoint(Params^[1])^;
end;

//Read: property HideSelection: Boolean read HideSelection write HideSelection;
procedure TCustomEdit_HideSelection_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.HideSelection;
end;

//Write: property HideSelection: Boolean read HideSelection write HideSelection;
procedure TCustomEdit_HideSelection_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.HideSelection := PBoolean(Params^[1])^;
end;

//Read: property MaxLength: Integer read MaxLength write MaxLength;
procedure TCustomEdit_MaxLength_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomEdit(Params^[0])^.MaxLength;
end;

//Write: property MaxLength: Integer read MaxLength write MaxLength;
procedure TCustomEdit_MaxLength_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.MaxLength := PInteger(Params^[1])^;
end;

//Read: property Modified: Boolean read Modified write Modified;
procedure TCustomEdit_Modified_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.Modified;
end;

//Write: property Modified: Boolean read Modified write Modified;
procedure TCustomEdit_Modified_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.Modified := PBoolean(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure TCustomEdit_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomEdit(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure TCustomEdit_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//Read: property PasswordChar: Char read PasswordChar write PasswordChar;
procedure TCustomEdit_PasswordChar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PChar(Result)^ := PCustomEdit(Params^[0])^.PasswordChar;
end;

//Write: property PasswordChar: Char read PasswordChar write PasswordChar;
procedure TCustomEdit_PasswordChar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.PasswordChar := PChar(Params^[1])^;
end;

//Read: property ReadOnly: Boolean read ReadOnly write ReadOnly;
procedure TCustomEdit_ReadOnly_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.ReadOnly;
end;

//Write: property ReadOnly: Boolean read ReadOnly write ReadOnly;
procedure TCustomEdit_ReadOnly_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.ReadOnly := PBoolean(Params^[1])^;
end;

//Read: property SelLength: integer read SelLength write SelLength;
procedure TCustomEdit_SelLength_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomEdit(Params^[0])^.SelLength;
end;

//Write: property SelLength: integer read SelLength write SelLength;
procedure TCustomEdit_SelLength_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.SelLength := Pinteger(Params^[1])^;
end;

//Read: property SelStart: integer read SelStart write SelStart;
procedure TCustomEdit_SelStart_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomEdit(Params^[0])^.SelStart;
end;

//Write: property SelStart: integer read SelStart write SelStart;
procedure TCustomEdit_SelStart_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.SelStart := Pinteger(Params^[1])^;
end;

//Read: property SelText: String read SelText write SelText;
procedure TCustomEdit_SelText_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomEdit(Params^[0])^.SelText;
end;

//Write: property SelText: String read SelText write SelText;
procedure TCustomEdit_SelText_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.SelText := PlpString(Params^[1])^;
end;

//Read: property Text: string read Text write Text;
procedure TCustomEdit_Text_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomEdit(Params^[0])^.Text;
end;

//Write: property Text: string read Text write Text;
procedure TCustomEdit_Text_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.Text := PlpString(Params^[1])^;
end;

//procedure Free();
procedure TCustomEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.Free();
end;

procedure Register_TCustomEdit(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomEdit', 'TWinControl');

    addGlobalFunc('procedure TCustomEdit.Init(AOwner: TComponent);', @TCustomEdit_Init);
    addGlobalFunc('procedure TCustomEdit.Clear(); constref;', @TCustomEdit_Clear);
    addGlobalFunc('procedure TCustomEdit.SelectAll(); constref;', @TCustomEdit_SelectAll);
    addGlobalFunc('procedure TCustomEdit.ClearSelection(); constref;', @TCustomEdit_ClearSelection);
    addGlobalFunc('procedure TCustomEdit.CopyToClipboard(); constref;', @TCustomEdit_CopyToClipboard);
    addGlobalFunc('procedure TCustomEdit.CutToClipboard(); constref;', @TCustomEdit_CutToClipboard);
    addGlobalFunc('procedure TCustomEdit.PasteFromClipboard(); constref;', @TCustomEdit_PasteFromClipboard);
    addGlobalFunc('procedure TCustomEdit.Undo(); constref;', @TCustomEdit_Undo);
    addClassVar('TCustomEdit', 'CanUndo', 'Boolean', @TCustomEdit_CanUndo_Read);
    addClassVar('TCustomEdit', 'CaretPos', 'TPoint', @TCustomEdit_CaretPos_Read, @TCustomEdit_CaretPos_Write);
    addClassVar('TCustomEdit', 'HideSelection', 'Boolean', @TCustomEdit_HideSelection_Read, @TCustomEdit_HideSelection_Write);
    addClassVar('TCustomEdit', 'MaxLength', 'Integer', @TCustomEdit_MaxLength_Read, @TCustomEdit_MaxLength_Write);
    addClassVar('TCustomEdit', 'Modified', 'Boolean', @TCustomEdit_Modified_Read, @TCustomEdit_Modified_Write);
    addClassVar('TCustomEdit', 'OnChange', 'TNotifyEvent', @TCustomEdit_OnChange_Read, @TCustomEdit_OnChange_Write);
    addClassVar('TCustomEdit', 'PasswordChar', 'Char', @TCustomEdit_PasswordChar_Read, @TCustomEdit_PasswordChar_Write);
    addClassVar('TCustomEdit', 'ReadOnly', 'Boolean', @TCustomEdit_ReadOnly_Read, @TCustomEdit_ReadOnly_Write);
    addClassVar('TCustomEdit', 'SelLength', 'integer', @TCustomEdit_SelLength_Read, @TCustomEdit_SelLength_Write);
    addClassVar('TCustomEdit', 'SelStart', 'integer', @TCustomEdit_SelStart_Read, @TCustomEdit_SelStart_Write);
    addClassVar('TCustomEdit', 'SelText', 'String', @TCustomEdit_SelText_Read, @TCustomEdit_SelText_Write);
    addClassVar('TCustomEdit', 'Text', 'string', @TCustomEdit_Text_Read, @TCustomEdit_Text_Write);
    addGlobalFunc('procedure TCustomEdit.Free(); constref;', @TCustomEdit_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure TEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEdit(Params^[0])^ := TEdit.Create(PComponent(Params^[1])^);
end;

procedure TEdit_OnEditingDone_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PEdit(Params^[0])^.OnEditingDone;
end;

procedure TEdit_OnEditingDone_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEdit(Params^[0])^.OnEditingDone := PNotifyEvent(Params^[1])^;
end;

//procedure Free();
procedure TEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEdit(Params^[0])^.Free();
end;

procedure Register_TEdit(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TEdit', 'TCustomEdit');

    addClassVar('TEdit', 'OnEditingDone', 'TNotifyEvent', @TEdit_OnEditingDone_Read, @TEdit_OnEditingDone_Write);
    addGlobalFunc('procedure TEdit.Init(AOwner: TComponent);', @TEdit_Init);
    addGlobalFunc('procedure TEdit.Free(); constref;', @TEdit_Free);
  end;
end;

procedure TCustomGroupBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomGroupBox(Params^[0])^ := TCustomGroupBox.Create(PComponent(Params^[1])^);
end;

procedure TCustomGroupBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomGroupBox(Params^[0])^.Free();
end;

procedure Register_TCustomGroupBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomGroupBox', 'TWinControl');

    addGlobalFunc('procedure TCustomGroupBox.Init(AOwner: TComponent);', @TCustomGroupBox_Init);
    addGlobalFunc('procedure TCustomGroupBox.Free(); constref;', @TCustomGroupBox_Free);
  end;
end;

procedure TGroupBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PGroupBox(Params^[0])^ := TGroupBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TGroupBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PGroupBox(Params^[0])^.Free();
end;

procedure Register_TGroupBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TGroupBox', 'TCustomGroupBox');

    addGlobalFunc('procedure TGroupBox.Init(AOwner: TComponent);', @TGroupBox_Init);
    addGlobalFunc('procedure TGroupBox.Free(); constref;', @TGroupBox_Free);
  end;
end;

{TMemo}
//constructor Create(AOwner: TComponent);
procedure TMemoScrollbar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemoScrollbar(Params^[0])^ := TMemoScrollbar.Create(PWinControl(Params^[1])^, PScrollBarKind(Params^[2])^);
end;

//procedure Free();
procedure TMemoScrollbar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemoScrollbar(Params^[0])^.Free();
end;

procedure Register_TMemoScrollbar(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TMemoScrollbar', 'TControlScrollBar');

    addGlobalFunc('procedure TMemoScrollbar.Init(AControl: TWinControl; AKind: TScrollBarKind);', @TMemoScrollbar_Init);
    addGlobalFunc('procedure TMemoScrollbar.Free(); constref;', @TMemoScrollbar_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure TCustomMemo_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^ := TCustomMemo.Create(PComponent(Params^[1])^);
end;

//procedure Append(const Value: String);
procedure TCustomMemo_Append(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.Append(PlpString(Params^[1])^);
end;

//Read: property Lines: TStrings read Lines write Lines;
procedure TCustomMemo_Lines_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PCustomMemo(Params^[0])^.Lines;
end;

//Write: property Lines: TStrings read Lines write Lines;
procedure TCustomMemo_Lines_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.Lines := PStrings(Params^[1])^;
end;

//Read: property HorzScrollBar: TMemoScrollBar read HorzScrollBar write HorzScrollBar;
procedure TCustomMemo_HorzScrollBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemoScrollBar(Result)^ := PCustomMemo(Params^[0])^.HorzScrollBar;
end;

//Write: property HorzScrollBar: TMemoScrollBar read HorzScrollBar write HorzScrollBar;
procedure TCustomMemo_HorzScrollBar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.HorzScrollBar := PMemoScrollBar(Params^[1])^;
end;

//Read: property VertScrollBar: TMemoScrollBar read VertScrollBar write VertScrollBar;
procedure TCustomMemo_VertScrollBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemoScrollBar(Result)^ := PCustomMemo(Params^[0])^.VertScrollBar;
end;

//Write: property VertScrollBar: TMemoScrollBar read VertScrollBar write VertScrollBar;
procedure TCustomMemo_VertScrollBar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.VertScrollBar := PMemoScrollBar(Params^[1])^;
end;

//Read: property ScrollBars: TScrollStyle read ScrollBars write ScrollBars;
procedure TCustomMemo_ScrollBars_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollStyle(Result)^ := PCustomMemo(Params^[0])^.ScrollBars;
end;

//Write: property ScrollBars: TScrollStyle read ScrollBars write ScrollBars;
procedure TCustomMemo_ScrollBars_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.ScrollBars := PScrollStyle(Params^[1])^;
end;

//Read: property WantReturns: Boolean read WantReturns write WantReturns;
procedure TCustomMemo_WantReturns_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomMemo(Params^[0])^.WantReturns;
end;

//Write: property WantReturns: Boolean read WantReturns write WantReturns;
procedure TCustomMemo_WantReturns_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.WantReturns := PBoolean(Params^[1])^;
end;

//Read: property WantTabs: Boolean read WantTabs write WantTabs;
procedure TCustomMemo_WantTabs_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomMemo(Params^[0])^.WantTabs;
end;

//Write: property WantTabs: Boolean read WantTabs write WantTabs;
procedure TCustomMemo_WantTabs_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.WantTabs := PBoolean(Params^[1])^;
end;

//Read: property WordWrap: Boolean read WordWrap write WordWrap;
procedure TCustomMemo_WordWrap_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomMemo(Params^[0])^.WordWrap;
end;

//Write: property WordWrap: Boolean read WordWrap write WordWrap;
procedure TCustomMemo_WordWrap_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.WordWrap := PBoolean(Params^[1])^;
end;

//procedure Free();
procedure TCustomMemo_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.Free();
end;

procedure Register_TCustomMemo(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomMemo', 'TCustomEdit');

    addGlobalFunc('procedure TCustomMemo.Init(AOwner: TComponent);', @TCustomMemo_Init);
    addGlobalFunc('procedure TCustomMemo.Append(const Value: String); constref;', @TCustomMemo_Append);
    addClassVar('TCustomMemo', 'Lines', 'TStrings', @TCustomMemo_Lines_Read, @TCustomMemo_Lines_Write);
    addClassVar('TCustomMemo', 'HorzScrollBar', 'TMemoScrollBar', @TCustomMemo_HorzScrollBar_Read, @TCustomMemo_HorzScrollBar_Write);
    addClassVar('TCustomMemo', 'VertScrollBar', 'TMemoScrollBar', @TCustomMemo_VertScrollBar_Read, @TCustomMemo_VertScrollBar_Write);
    addClassVar('TCustomMemo', 'ScrollBars', 'TScrollStyle', @TCustomMemo_ScrollBars_Read, @TCustomMemo_ScrollBars_Write);
    addClassVar('TCustomMemo', 'WantReturns', 'Boolean', @TCustomMemo_WantReturns_Read, @TCustomMemo_WantReturns_Write);
    addClassVar('TCustomMemo', 'WantTabs', 'Boolean', @TCustomMemo_WantTabs_Read, @TCustomMemo_WantTabs_Write);
    addClassVar('TCustomMemo', 'WordWrap', 'Boolean', @TCustomMemo_WordWrap_Read, @TCustomMemo_WordWrap_Write);
    addGlobalFunc('procedure TCustomMemo.Free(); constref;', @TCustomMemo_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure TMemo_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemo(Params^[0])^ := TMemo.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TMemo_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemo(Params^[0])^.Free();
end;

procedure Register_TMemo(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TMemo', 'TCustomMemo');

    addGlobalFunc('procedure TMemo.Init(AOwner: TComponent);', @TMemo_Init);
    addGlobalFunc('procedure TMemo.Free(); constref;', @TMemo_Free);
  end;
end;


{TStaticText}
//constructor Create(AOwner: TComponent);
procedure TCustomStaticText_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomStaticText(Params^[0])^ := TCustomStaticText.Create(PComponent(Params^[1])^);
end;

//Read: property Alignment: TAlignment read Alignment write Alignment;
procedure TCustomStaticText_Alignment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAlignment(Result)^ := PCustomStaticText(Params^[0])^.Alignment;
end;

//Write: property Alignment: TAlignment read Alignment write Alignment;
procedure TCustomStaticText_Alignment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomStaticText(Params^[0])^.Alignment := PAlignment(Params^[1])^;
end;

//Read: property BorderStyle: TStaticBorderStyle read StaticBorderStyle write StaticBorderStyle;
procedure TCustomStaticText_BorderStyle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStaticBorderStyle(Result)^ := PCustomStaticText(Params^[0])^.BorderStyle;
end;

//Write: property BorderStyle: TStaticBorderStyle read StaticBorderStyle write StaticBorderStyle;
procedure TCustomStaticText_BorderStyle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomStaticText(Params^[0])^.BorderStyle := PStaticBorderStyle(Params^[1])^;
end;

//Read: property FocusControl: TWinControl read FocusControl write FocusControl;
procedure TCustomStaticText_FocusControl_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Result)^ := PCustomStaticText(Params^[0])^.FocusControl;
end;

//Write: property FocusControl: TWinControl read FocusControl write FocusControl;
procedure TCustomStaticText_FocusControl_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomStaticText(Params^[0])^.FocusControl := PWinControl(Params^[1])^;
end;

//Read: property ShowAccelChar: boolean read ShowAccelChar write ShowAccelChar;
procedure TCustomStaticText_ShowAccelChar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomStaticText(Params^[0])^.ShowAccelChar;
end;

//Write: property ShowAccelChar: boolean read ShowAccelChar write ShowAccelChar;
procedure TCustomStaticText_ShowAccelChar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomStaticText(Params^[0])^.ShowAccelChar := Pboolean(Params^[1])^;
end;

//Read: property Transparent: Boolean read Transparent write Transparent;
procedure TCustomStaticText_Transparent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomStaticText(Params^[0])^.Transparent;
end;

//Write: property Transparent: Boolean read Transparent write Transparent;
procedure TCustomStaticText_Transparent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomStaticText(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

//procedure Free();
procedure TCustomStaticText_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomStaticText(Params^[0])^.Free();
end;

procedure Register_TCustomStaticText(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomStaticText', 'TWinControl');

    addGlobalFunc('procedure TCustomStaticText.Init(AOwner: TComponent);', @TCustomStaticText_Init);
    addClassVar('TCustomStaticText', 'Alignment', 'TAlignment', @TCustomStaticText_Alignment_Read, @TCustomStaticText_Alignment_Write);
    addClassVar('TCustomStaticText', 'BorderStyle', 'TStaticBorderStyle', @TCustomStaticText_BorderStyle_Read, @TCustomStaticText_BorderStyle_Write);
    addClassVar('TCustomStaticText', 'FocusControl', 'TWinControl', @TCustomStaticText_FocusControl_Read, @TCustomStaticText_FocusControl_Write);
    addClassVar('TCustomStaticText', 'ShowAccelChar', 'boolean', @TCustomStaticText_ShowAccelChar_Read, @TCustomStaticText_ShowAccelChar_Write);
    addClassVar('TCustomStaticText', 'Transparent', 'Boolean', @TCustomStaticText_Transparent_Read, @TCustomStaticText_Transparent_Write);
    addGlobalFunc('procedure TCustomStaticText.Free(); constref;', @TCustomStaticText_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure TStaticText_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStaticText(Params^[0])^ := TStaticText.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TStaticText_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStaticText(Params^[0])^.Free();
end;

procedure Register_TStaticText(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TStaticText', 'TCustomStaticText');

    addGlobalFunc('procedure TStaticText.Init(AOwner: TComponent);', @TStaticText_Init);
    addGlobalFunc('procedure TStaticText.Free(); constref;', @TStaticText_Free);
  end;
end;


{TButton}

//constructor Create(TheOwner: TComponent);
procedure TButtonControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PButtonControl(Params^[0])^ := TButtonControl.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TButtonControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PButtonControl(Params^[0])^.Free();
end;

procedure Register_TButtonControl(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TButtonControl', 'TWinControl');

    addGlobalFunc('procedure TButtonControl.Init(TheOwner: TComponent);', @TButtonControl_Init);
    addGlobalFunc('procedure TButtonControl.Free(); constref;', @TButtonControl_Free);
  end;
end;

//constructor Create(TheOwner: TComponent);
procedure TCustomButton_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^ := TCustomButton.Create(PComponent(Params^[1])^);
end;

//procedure ExecuteDefaultAction;
procedure TCustomButton_ExecuteDefaultAction(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.ExecuteDefaultAction();
end;

//procedure ExecuteCancelAction;
procedure TCustomButton_ExecuteCancelAction(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.ExecuteCancelAction();
end;

//procedure ActiveDefaultControlChanged(NewControl: TControl);
procedure TCustomButton_ActiveDefaultControlChanged(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.ActiveDefaultControlChanged(PControl(Params^[1])^);
end;

//procedure UpdateRolesForForm;
procedure TCustomButton_UpdateRolesForForm(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.UpdateRolesForForm();
end;

//function UseRightToLeftAlignment: Boolean;
procedure TCustomButton_UseRightToLeftAlignment(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomButton(Params^[0])^.UseRightToLeftAlignment();
end;

//Read: property Active: boolean read Active;
procedure TCustomButton_Active_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomButton(Params^[0])^.Active;
end;

//Read: property Default: Boolean read Default write Default;
procedure TCustomButton_Default_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomButton(Params^[0])^.Default;
end;

//Write: property Default: Boolean read Default write Default;
procedure TCustomButton_Default_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.Default := PBoolean(Params^[1])^;
end;

//Read: property Cancel: Boolean read Cancel write Cancel;
procedure TCustomButton_Cancel_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomButton(Params^[0])^.Cancel;
end;

//Write: property Cancel: Boolean read Cancel write Cancel;
procedure TCustomButton_Cancel_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.Cancel := PBoolean(Params^[1])^;
end;

//procedure Free();
procedure TCustomButton_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.Free();
end;

procedure Register_TCustomButton(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomButton', 'TButtonControl');

    addGlobalFunc('procedure TCustomButton.Init(TheOwner: TComponent);', @TCustomButton_Init);
    addGlobalFunc('procedure TCustomButton.ExecuteDefaultAction(); constref;', @TCustomButton_ExecuteDefaultAction);
    addGlobalFunc('procedure TCustomButton.ExecuteCancelAction(); constref;', @TCustomButton_ExecuteCancelAction);
    addGlobalFunc('procedure TCustomButton.ActiveDefaultControlChanged(NewControl: TControl); constref;', @TCustomButton_ActiveDefaultControlChanged);
    addGlobalFunc('procedure TCustomButton.UpdateRolesForForm(); constref;', @TCustomButton_UpdateRolesForForm);
    addGlobalFunc('function TCustomButton.UseRightToLeftAlignment(): Boolean; constref;', @TCustomButton_UseRightToLeftAlignment);
    addClassVar('TCustomButton', 'Active', 'boolean', @TCustomButton_Active_Read);
    addClassVar('TCustomButton', 'Default', 'Boolean', @TCustomButton_Default_Read, @TCustomButton_Default_Write);
    addClassVar('TCustomButton', 'Cancel', 'Boolean', @TCustomButton_Cancel_Read, @TCustomButton_Cancel_Write);
    addGlobalFunc('procedure TCustomButton.Free(); constref;', @TCustomButton_Free);
  end;
end;

//constructor Create(TheOwner: TComponent);
procedure TButton_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PButton(Params^[0])^ := TButton.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TButton_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PButton(Params^[0])^.Free();
end;

procedure Register_TButton(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TButton', 'TCustomButton');

    addGlobalFunc('procedure TButton.Init(TheOwner: TComponent);', @TButton_Init);
    addGlobalFunc('procedure TButton.Free(); constref;', @TButton_Free);
  end;
end;
{TCheckBox}

//constructor Create(TheOwner: TComponent);
procedure TCustomCheckBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckBox(Params^[0])^ := TCustomCheckBox.Create(PComponent(Params^[1])^);
end;

//Read: property AllowGrayed: Boolean read AllowGrayed write AllowGrayed;
procedure TCustomCheckBox_AllowGrayed_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomCheckBox(Params^[0])^.AllowGrayed;
end;

//Write: property AllowGrayed: Boolean read AllowGrayed write AllowGrayed;
procedure TCustomCheckBox_AllowGrayed_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckBox(Params^[0])^.AllowGrayed := PBoolean(Params^[1])^;
end;

//Read: property State: TCheckBoxState read State write State;
procedure TCustomCheckBox_State_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCheckBoxState(Result)^ := PCustomCheckBox(Params^[0])^.State;
end;

//Write: property State: TCheckBoxState read State write State;
procedure TCustomCheckBox_State_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckBox(Params^[0])^.State := PCheckBoxState(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure TCustomCheckBox_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomCheckBox(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure TCustomCheckBox_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckBox(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//procedure Free();
procedure TCustomCheckBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckBox(Params^[0])^.Free();
end;

procedure Register_TCustomCheckBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomCheckBox', 'TButtonControl');

    addGlobalFunc('procedure TCustomCheckBox.Init(TheOwner: TComponent);', @TCustomCheckBox_Init);
    addClassVar('TCustomCheckBox', 'AllowGrayed', 'Boolean', @TCustomCheckBox_AllowGrayed_Read, @TCustomCheckBox_AllowGrayed_Write);
    addClassVar('TCustomCheckBox', 'State', 'TCheckBoxState', @TCustomCheckBox_State_Read, @TCustomCheckBox_State_Write);
    addClassVar('TCustomCheckBox', 'OnChange', 'TNotifyEvent', @TCustomCheckBox_OnChange_Read, @TCustomCheckBox_OnChange_Write);
    addGlobalFunc('procedure TCustomCheckBox.Free(); constref;', @TCustomCheckBox_Free);
  end;
end;

//constructor Create(TheOwner: TComponent);
procedure TCheckBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCheckBox(Params^[0])^ := TCheckBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TCheckBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCheckBox(Params^[0])^.Free();
end;

procedure Register_TCheckBox(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCheckBox', 'TCustomCheckBox');

    addGlobalFunc('procedure TCheckBox.Init(TheOwner: TComponent);', @TCheckBox_Init);
    addGlobalFunc('procedure TCheckBox.Free(); constref;', @TCheckBox_Free);
  end;
end;
{TLabel}

//constructor Create(TheOwner: TComponent);
procedure TCustomLabel_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomLabel(Params^[0])^ := TCustomLabel.Create(PComponent(Params^[1])^);
end;

//function ColorIsStored: boolean;
procedure TCustomLabel_ColorIsStored(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomLabel(Params^[0])^.ColorIsStored();
end;

//function AdjustFontForOptimalFill: Boolean;
procedure TCustomLabel_AdjustFontForOptimalFill(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomLabel(Params^[0])^.AdjustFontForOptimalFill();
end;

//procedure Paint;
procedure TCustomLabel_Paint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomLabel(Params^[0])^.Paint();
end;

//procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer);
procedure TCustomLabel_SetBounds(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomLabel(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure Free();
procedure TCustomLabel_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomLabel(Params^[0])^.Free();
end;

procedure Register_TCustomLabel(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomLabel', 'TGraphicControl');

    addGlobalFunc('procedure TCustomLabel.Init(TheOwner: TComponent);', @TCustomLabel_Init);
    addGlobalFunc('function TCustomLabel.ColorIsStored(): boolean; constref;', @TCustomLabel_ColorIsStored);
    addGlobalFunc('function TCustomLabel.AdjustFontForOptimalFill(): Boolean; constref;', @TCustomLabel_AdjustFontForOptimalFill);
    addGlobalFunc('procedure TCustomLabel.Paint(); constref;', @TCustomLabel_Paint);
    addGlobalFunc('procedure TCustomLabel.SetBounds(aLeft, aTop, aWidth, aHeight: integer); constref;', @TCustomLabel_SetBounds);
    addGlobalFunc('procedure TCustomLabel.Free(); constref;', @TCustomLabel_Free);
  end;
end;
//constructor Create(TheOwner: TComponent);
procedure TLabel_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLabel(Params^[0])^ := TLabel.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TLabel_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLabel(Params^[0])^.Free();
end;

procedure Register_TLabel(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TLabel', 'TCustomLabel');

    addGlobalFunc('procedure TLabel.Init(TheOwner: TComponent);', @TLabel_Init);
    addGlobalFunc('procedure TLabel.Free(); constref;', @TLabel_Free);
  end;
end;

//function FindDownButton: TCustomSpeedButton;
procedure TCustomSpeedButton_FindDownButton(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Result)^ := PCustomSpeedButton(Params^[0])^.FindDownButton();
end;

//procedure LoadGlyphFromLazarusResource(const AName: String);
procedure TCustomSpeedButton_LoadGlyphFromLazarusResource(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.LoadGlyphFromLazarusResource(PlpString(Params^[1])^);
end;

//Read: property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default false;
procedure TCustomSpeedButton_AllowAllUp_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.AllowAllUp;
end;

//Write: property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default false;
procedure TCustomSpeedButton_AllowAllUp_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.AllowAllUp := PBoolean(Params^[1])^;
end;

//Read: property Down: Boolean read FDown write SetDown default false;
procedure TCustomSpeedButton_Down_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.Down;
end;

//Write: property Down: Boolean read FDown write SetDown default false;
procedure TCustomSpeedButton_Down_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Down := PBoolean(Params^[1])^;
end;

//Read: property Flat: Boolean read FFlat write SetFlat default false;
procedure TCustomSpeedButton_Flat_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.Flat;
end;

//Write: property Flat: Boolean read FFlat write SetFlat default false;
procedure TCustomSpeedButton_Flat_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Flat := PBoolean(Params^[1])^;
end;

//Read: property Glyph: TBitmap read GetGlyph write SetGlyph;
procedure TCustomSpeedButton_Glyph_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBitmap(Result)^ := PCustomSpeedButton(Params^[0])^.Glyph;
end;

//Write: property Glyph: TBitmap read GetGlyph write SetGlyph;
procedure TCustomSpeedButton_Glyph_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Glyph := PBitmap(Params^[1])^;
end;

//Read: property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
procedure TCustomSpeedButton_GroupIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomSpeedButton(Params^[0])^.GroupIndex;
end;

//Write: property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
procedure TCustomSpeedButton_GroupIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.GroupIndex := PInteger(Params^[1])^;
end;

//Read: property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
procedure TCustomSpeedButton_Layout_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PButtonLayout(Result)^ := PCustomSpeedButton(Params^[0])^.Layout;
end;

//Write: property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
procedure TCustomSpeedButton_Layout_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Layout := PButtonLayout(Params^[1])^;
end;

//Read: property Margin: integer read FMargin write SetMargin default -1;
procedure TCustomSpeedButton_Margin_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpeedButton(Params^[0])^.Margin;
end;

//Write: property Margin: integer read FMargin write SetMargin default -1;
procedure TCustomSpeedButton_Margin_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Margin := Pinteger(Params^[1])^;
end;

//Read: property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs default 1;
procedure TCustomSpeedButton_NumGlyphs_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomSpeedButton(Params^[0])^.NumGlyphs;
end;

//Write: property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs default 1;
procedure TCustomSpeedButton_NumGlyphs_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.NumGlyphs := PInteger(Params^[1])^;
end;

//Read: property ShowAccelChar: boolean read FShowAccelChar write SetShowAccelChar default true;
procedure TCustomSpeedButton_ShowAccelChar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomSpeedButton(Params^[0])^.ShowAccelChar;
end;

//Write: property ShowAccelChar: boolean read FShowAccelChar write SetShowAccelChar default true;
procedure TCustomSpeedButton_ShowAccelChar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.ShowAccelChar := Pboolean(Params^[1])^;
end;

//Read: property ShowCaption: boolean read FShowCaption write SetShowCaption default true;
procedure TCustomSpeedButton_ShowCaption_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomSpeedButton(Params^[0])^.ShowCaption;
end;

//Write: property ShowCaption: boolean read FShowCaption write SetShowCaption default true;
procedure TCustomSpeedButton_ShowCaption_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.ShowCaption := Pboolean(Params^[1])^;
end;

//Read: property Spacing: integer read FSpacing write SetSpacing default 4;
procedure TCustomSpeedButton_Spacing_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpeedButton(Params^[0])^.Spacing;
end;

//Write: property Spacing: integer read FSpacing write SetSpacing default 4;
procedure TCustomSpeedButton_Spacing_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Spacing := Pinteger(Params^[1])^;
end;

//Read: property Transparent: Boolean read GetTransparent write SetTransparent default true;
procedure TCustomSpeedButton_Transparent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.Transparent;
end;

//Write: property Transparent: Boolean read GetTransparent write SetTransparent default true;
procedure TCustomSpeedButton_Transparent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

//constructor Create();
procedure TCustomSpeedButton_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^ := TCustomSpeedButton.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TCustomSpeedButton_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Free();
end;

procedure Register_TCustomSpeedButton(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomSpeedButton', 'TGraphicControl');

    addGlobalFunc('function TCustomSpeedButton.FindDownButton(): TCustomSpeedButton; constref;', @TCustomSpeedButton_FindDownButton);
    addGlobalFunc('procedure TCustomSpeedButton.LoadGlyphFromLazarusResource(const AName: String); constref;', @TCustomSpeedButton_LoadGlyphFromLazarusResource);
    addClassVar('TCustomSpeedButton', 'AllowAllUp', 'Boolean', @TCustomSpeedButton_AllowAllUp_Read, @TCustomSpeedButton_AllowAllUp_Write);
    addClassVar('TCustomSpeedButton', 'Down', 'Boolean', @TCustomSpeedButton_Down_Read, @TCustomSpeedButton_Down_Write);
    addClassVar('TCustomSpeedButton', 'Flat', 'Boolean', @TCustomSpeedButton_Flat_Read, @TCustomSpeedButton_Flat_Write);
    addClassVar('TCustomSpeedButton', 'Glyph', 'TBitmap', @TCustomSpeedButton_Glyph_Read, @TCustomSpeedButton_Glyph_Write);
    addClassVar('TCustomSpeedButton', 'GroupIndex', 'Integer', @TCustomSpeedButton_GroupIndex_Read, @TCustomSpeedButton_GroupIndex_Write);
    addClassVar('TCustomSpeedButton', 'Layout', 'TButtonLayout', @TCustomSpeedButton_Layout_Read, @TCustomSpeedButton_Layout_Write);
    addClassVar('TCustomSpeedButton', 'Margin', 'integer', @TCustomSpeedButton_Margin_Read, @TCustomSpeedButton_Margin_Write);
    addClassVar('TCustomSpeedButton', 'NumGlyphs', 'Integer', @TCustomSpeedButton_NumGlyphs_Read, @TCustomSpeedButton_NumGlyphs_Write);
    addClassVar('TCustomSpeedButton', 'ShowAccelChar', 'boolean', @TCustomSpeedButton_ShowAccelChar_Read, @TCustomSpeedButton_ShowAccelChar_Write);
    addClassVar('TCustomSpeedButton', 'ShowCaption', 'boolean', @TCustomSpeedButton_ShowCaption_Read, @TCustomSpeedButton_ShowCaption_Write);
    addClassVar('TCustomSpeedButton', 'Spacing', 'integer', @TCustomSpeedButton_Spacing_Read, @TCustomSpeedButton_Spacing_Write);
    addClassVar('TCustomSpeedButton', 'Transparent', 'Boolean', @TCustomSpeedButton_Transparent_Read, @TCustomSpeedButton_Transparent_Write);
    addGlobalFunc('procedure TCustomSpeedButton.Init(AOwner: TComponent);', @TCustomSpeedButton_Init);
    addGlobalFunc('procedure TCustomSpeedButton.Free(); constref;', @TCustomSpeedButton_Free);
  end;
end;

//constructor Create();
procedure TSpeedButton_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSpeedButton(Params^[0])^ := TSpeedButton.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TSpeedButton_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSpeedButton(Params^[0])^.Free();
end;

procedure Register_TSpeedButton(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TSpeedButton', 'TCustomSpeedButton');

    addGlobalFunc('procedure TSpeedButton.Init(AOwner: TComponent);', @TSpeedButton_Init);
    addGlobalFunc('procedure TSpeedButton.Free(); constref;', @TSpeedButton_Free);
  end;
end;


{}
procedure RegisterLCLStdCtrls(Compiler: TLapeCompiler);
begin
  with Compiler do
   begin
     addGlobalType('(ssNone, ssHorizontal, ssVertical, ssBoth,ssAutoHorizontal, ssAutoVertical, ssAutoBoth)','TScrollStyle');
     addGlobalType('(scLineUp,scLineDown, scPageUp,scPageDown,scPosition, scTrack,scTop,scBottom,scEndScroll)','TScrollCode');
     addGlobalType('procedure(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer)', 'TScrollEvent', FFI_DEFAULT_ABI);
     addGlobalType('(odSelected, odGrayed, odDisabled, odChecked, odFocused, odDefault, odHotLight, odInactive, odNoAccel, odNoFocusRect, odReserved1, odReserved2, odComboBoxEdit, odPainted)' , 'TOwnerDrawStateType');
     addGlobalType('set of TOwnerDrawStateType', 'TOwnerDrawState');
     addGlobalType('procedure(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState)', 'TDrawItemEvent', FFI_DEFAULT_ABI);
     addGlobalType('(csDropDown,csSimple,csDropDownList,csOwnerDrawFixed,csOwnerDrawVariable)','TComboBoxStyle');
     addGlobalType('(lbStandard, lbOwnerDrawFixed, lbOwnerDrawVariable, lbVirtual)', 'TListBoxStyle');
     addGlobalType('(sbsNone, sbsSingle, sbsSunken)','TStaticBorderStyle');
     addGlobalType('(taLeftJustify, taRightJustify, taCenter)','TAlignment');
     addGlobalType('(cbUnchecked, cbChecked, cbGrayed)','TCheckBoxState');
     addGlobalType('(blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom)', 'TButtonLayout');
   end;
  Register_TCustomScrollBar(Compiler);
  Register_TScrollBar(Compiler);
  Register_TCustomComboBox(Compiler);
  Register_TComboBox(Compiler);
  Register_TCustomListBox(Compiler);
  Register_TListBox(Compiler);
  Register_TCustomEdit(Compiler);
  Register_TEdit(Compiler);
  Register_TCustomGroupBox(Compiler);
  Register_TGroupBox(Compiler);
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
  Register_TCustomSpeedButton(Compiler);
  Register_TSpeedButton(Compiler);
end;

end.
