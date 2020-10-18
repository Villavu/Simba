unit simba.script_import_lclstdctrls;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_LCLStdCtrls(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  stdctrls, forms, buttons, controls, graphics;

type
  PObject = ^TObject;
  PComponent = ^TComponent;
  PNotifyEvent = ^TNotifyEvent;
  PCustomScrollBar = ^TCustomScrollBar;
  PScrollBarKind = ^TScrollBarKind;
  PScrollEvent = ^TScrollEvent;
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
  // TRadioButton
  PRadioButton = ^TRadioButton;
  PWinControl = ^TWinControl;
  PControl = ^TControl;
  PBitmap = ^TBitmap;
  PRect = ^TRect;

{TCustomScrollBar}

//constructor Create(AOwner: TComponent);
procedure Lape_TCustomScrollBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^ := TCustomScrollBar.Create(PComponent(Params^[1])^);
end;

//procedure SetParams(APosition, AMin, AMax, APageSize: Integer);
procedure Lape_TCustomScrollBar_SetParams(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.SetParams(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

//procedure SetParams(APosition, AMin, AMax: Integer);
procedure Lape_TCustomScrollBar_SetParamsEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.SetParams(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

//Read: property Kind: TScrollBarKind read Kind write Kind;
procedure Lape_TCustomScrollBar_Kind_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollBarKind(Result)^ := PCustomScrollBar(Params^[0])^.Kind;
end;

//Write: property Kind: TScrollBarKind read Kind write Kind;
procedure Lape_TCustomScrollBar_Kind_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.Kind := PScrollBarKind(Params^[1])^;
end;

//Read: property LargeChange: integer read LargeChange write LargeChange;
procedure Lape_TCustomScrollBar_LargeChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomScrollBar(Params^[0])^.LargeChange;
end;

//Write: property LargeChange: integer read LargeChange write LargeChange;
procedure Lape_TCustomScrollBar_LargeChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.LargeChange := Pinteger(Params^[1])^;
end;

//Read: property Max: Integer read Max write Max;
procedure Lape_TCustomScrollBar_Max_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.Max;
end;

//Write: property Max: Integer read Max write Max;
procedure Lape_TCustomScrollBar_Max_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.Max := PInteger(Params^[1])^;
end;

//Read: property Min: Integer read Min write Min;
procedure Lape_TCustomScrollBar_Min_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.Min;
end;

//Write: property Min: Integer read Min write Min;
procedure Lape_TCustomScrollBar_Min_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.Min := PInteger(Params^[1])^;
end;

//Read: property PageSize: Integer read PageSize write PageSize;
procedure Lape_TCustomScrollBar_PageSize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.PageSize;
end;

//Write: property PageSize: Integer read PageSize write PageSize;
procedure Lape_TCustomScrollBar_PageSize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.PageSize := PInteger(Params^[1])^;
end;

//Read: property Position: Integer read Position write Position;
procedure Lape_TCustomScrollBar_Position_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.Position;
end;

//Write: property Position: Integer read Position write Position;
procedure Lape_TCustomScrollBar_Position_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

//Read: property SmallChange: integer read SmallChange write SmallChange;
procedure Lape_TCustomScrollBar_SmallChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomScrollBar(Params^[0])^.SmallChange;
end;

//Write: property SmallChange: integer read SmallChange write SmallChange;
procedure Lape_TCustomScrollBar_SmallChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.SmallChange := Pinteger(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure Lape_TCustomScrollBar_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomScrollBar(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read FOnChange write FOnChange;
procedure Lape_TCustomScrollBar_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//Read: property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
procedure Lape_TCustomScrollBar_OnScroll_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollEvent(Result)^ := PCustomScrollBar(Params^[0])^.OnScroll;
end;

//Write: property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
procedure Lape_TCustomScrollBar_OnScroll_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.OnScroll := PScrollEvent(Params^[1])^;
end;

//procedure Free();
procedure Lape_TCustomScrollBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomScrollBar(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomScrollBar', 'TWinControl');

    addGlobalFunc('procedure TCustomScrollBar.Init(AOwner: TComponent); override;', @Lape_TCustomScrollBar_Init);
    addGlobalFunc('procedure TCustomScrollBar.SetParams(APosition, AMin, AMax, APageSize: Integer); constref;', @Lape_TCustomScrollBar_SetParams);
    addGlobalFunc('procedure TCustomScrollBar.SetParams(APosition, AMin, AMax: Integer); constref; overload;', @Lape_TCustomScrollBar_SetParamsEx);
    addClassVar('TCustomScrollBar', 'Kind', 'TScrollBarKind', @Lape_TCustomScrollBar_Kind_Read, @Lape_TCustomScrollBar_Kind_Write);
    addClassVar('TCustomScrollBar', 'LargeChange', 'integer', @Lape_TCustomScrollBar_LargeChange_Read, @Lape_TCustomScrollBar_LargeChange_Write);
    addClassVar('TCustomScrollBar', 'Max', 'Integer', @Lape_TCustomScrollBar_Max_Read, @Lape_TCustomScrollBar_Max_Write);
    addClassVar('TCustomScrollBar', 'Min', 'Integer', @Lape_TCustomScrollBar_Min_Read, @Lape_TCustomScrollBar_Min_Write);
    addClassVar('TCustomScrollBar', 'PageSize', 'Integer', @Lape_TCustomScrollBar_PageSize_Read, @Lape_TCustomScrollBar_PageSize_Write);
    addClassVar('TCustomScrollBar', 'Position', 'Integer', @Lape_TCustomScrollBar_Position_Read, @Lape_TCustomScrollBar_Position_Write);
    addClassVar('TCustomScrollBar', 'SmallChange', 'integer', @Lape_TCustomScrollBar_SmallChange_Read, @Lape_TCustomScrollBar_SmallChange_Write);
    addClassVar('TCustomScrollBar', 'OnChange', 'TNotifyEvent', @Lape_TCustomScrollBar_OnChange_Read, @Lape_TCustomScrollBar_OnChange_Write);
    addClassVar('TCustomScrollBar', 'OnScroll', 'TScrollEvent', @Lape_TCustomScrollBar_OnScroll_Read, @Lape_TCustomScrollBar_OnScroll_Write);
    //addGlobalFunc('procedure TCustomScrollBar.Free(); constref;', @Lape_TCustomScrollBar_Free);
  end;
end;

{TScrollBar}

//constructor Create();
procedure Lape_TScrollBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollBar(Params^[0])^ := TScrollBar.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TScrollBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollBar(Params^[0])^.Free();
end;

procedure Lape_Import_TScrollBar(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TScrollBar', 'TCustomScrollBar');

    addGlobalFunc('procedure TScrollBar.Init(AOwner: TComponent); override;', @Lape_TScrollBar_Init);
    //addGlobalFunc('procedure TScrollBar.Free(); constref;', @Lape_TScrollBar_Free);
  end;
end;

{TCombobox and TCustomComboBox}

//constructor Create(TheOwner: TComponent);
procedure Lape_TCustomComboBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^ := TCustomComboBox.Create(PComponent(Params^[1])^);
end;

//procedure IntfGetItems;
procedure Lape_TCustomComboBox_IntfGetItems(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.IntfGetItems();
end;

//procedure AddItem(const Item: String; AnObject: TObject);
procedure Lape_TCustomComboBox_AddItem(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AddItem(PlpString(Params^[1])^, PObject(Params^[2])^);
end;

//procedure AddHistoryItem(const Item: string; MaxHistoryCount: integer;SetAsText, CaseSensitive: boolean);
procedure Lape_TCustomComboBox_AddHistoryItem(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AddHistoryItem(PlpString(Params^[1])^, Pinteger(Params^[2])^, Pboolean(Params^[3])^, Pboolean(Params^[4])^);
end;

//procedure AddHistoryItem(const Item: string; AnObject: TObject;MaxHistoryCount: integer; SetAsText, CaseSensitive: boolean);
procedure Lape_TCustomComboBox_AddHistoryItemEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AddHistoryItem(PlpString(Params^[1])^, PObject(Params^[2])^, Pinteger(Params^[3])^, Pboolean(Params^[4])^, Pboolean(Params^[5])^);
end;

//procedure Clear;
procedure Lape_TCustomComboBox_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.Clear();
end;

//procedure ClearSelection;
procedure Lape_TCustomComboBox_ClearSelection(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.ClearSelection();
end;

//Read: property DroppedDown: Boolean read DroppedDown write DroppedDown;
procedure Lape_TCustomComboBox_DroppedDown_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.DroppedDown;
end;

//Write: property DroppedDown: Boolean read DroppedDown write DroppedDown;
procedure Lape_TCustomComboBox_DroppedDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.DroppedDown := PBoolean(Params^[1])^;
end;

//procedure SelectAll;
procedure Lape_TCustomComboBox_SelectAll(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.SelectAll();
end;

//Read: property AutoComplete: boolean read AutoComplete write AutoComplete;
procedure Lape_TCustomComboBox_AutoComplete_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomComboBox(Params^[0])^.AutoComplete;
end;

//Write: property AutoComplete: boolean read AutoComplete write AutoComplete;
procedure Lape_TCustomComboBox_AutoComplete_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AutoComplete := Pboolean(Params^[1])^;
end;

//Read: property AutoDropDown: Boolean read AutoDropDown write AutoDropDown;
procedure Lape_TCustomComboBox_AutoDropDown_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoDropDown;
end;

//Write: property AutoDropDown: Boolean read AutoDropDown write AutoDropDown;
procedure Lape_TCustomComboBox_AutoDropDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AutoDropDown := PBoolean(Params^[1])^;
end;

//Read: property AutoSelect: Boolean read AutoSelect write AutoSelect;
procedure Lape_TCustomComboBox_AutoSelect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoSelect;
end;

//Write: property AutoSelect: Boolean read AutoSelect write AutoSelect;
procedure Lape_TCustomComboBox_AutoSelect_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AutoSelect := PBoolean(Params^[1])^;
end;

//Read: property AutoSelected: Boolean read AutoSelected write AutoSelected;
procedure Lape_TCustomComboBox_AutoSelected_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoSelected;
end;

//Write: property AutoSelected: Boolean read AutoSelected write AutoSelected;
procedure Lape_TCustomComboBox_AutoSelected_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AutoSelected := PBoolean(Params^[1])^;
end;

//Read: property ArrowKeysTraverseList: Boolean read ArrowKeysTraverseList write ArrowKeysTraverseList;
procedure Lape_TCustomComboBox_ArrowKeysTraverseList_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.ArrowKeysTraverseList;
end;

//Write: property ArrowKeysTraverseList: Boolean read ArrowKeysTraverseList write ArrowKeysTraverseList;
procedure Lape_TCustomComboBox_ArrowKeysTraverseList_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.ArrowKeysTraverseList := PBoolean(Params^[1])^;
end;

//Read: property Canvas: TCanvas read FCanvas;
procedure Lape_TCustomComboBox_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PCustomComboBox(Params^[0])^.Canvas;
end;

//Read: property DropDownCount: Integer read DropDownCount write DropDownCount default 8;
procedure Lape_TCustomComboBox_DropDownCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomComboBox(Params^[0])^.DropDownCount;
end;

//Write: property DropDownCount: Integer read DropDownCount write DropDownCount default 8;
procedure Lape_TCustomComboBox_DropDownCount_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.DropDownCount := PInteger(Params^[1])^;
end;

//Read: property Items: TStrings read Items write Items;
procedure Lape_TCustomComboBox_Items_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PCustomComboBox(Params^[0])^.Items;
end;

//Write: property Items: TStrings read Items write Items;
procedure Lape_TCustomComboBox_Items_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.Items := PStrings(Params^[1])^;
end;

//Read: property ItemIndex: integer read ItemIndex write ItemIndex;
procedure Lape_TCustomComboBox_ItemIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomComboBox(Params^[0])^.ItemIndex;
end;

//Write: property ItemIndex: integer read ItemIndex write ItemIndex;
procedure Lape_TCustomComboBox_ItemIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.ItemIndex := Pinteger(Params^[1])^;
end;

//Read: property ReadOnly: Boolean read ReadOnly write ReadOnly;
procedure Lape_TCustomComboBox_ReadOnly_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.ReadOnly;
end;

//Write: property ReadOnly: Boolean read ReadOnly write ReadOnly;
procedure Lape_TCustomComboBox_ReadOnly_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.ReadOnly := PBoolean(Params^[1])^;
end;

//Read: property SelLength: integer read SelLength write SelLength;
procedure Lape_TCustomComboBox_SelLength_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomComboBox(Params^[0])^.SelLength;
end;

//Write: property SelLength: integer read SelLength write SelLength;
procedure Lape_TCustomComboBox_SelLength_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.SelLength := Pinteger(Params^[1])^;
end;

//Read: property SelStart: integer read SelStart write SelStart;
procedure Lape_TCustomComboBox_SelStart_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomComboBox(Params^[0])^.SelStart;
end;

//Write: property SelStart: integer read SelStart write SelStart;
procedure Lape_TCustomComboBox_SelStart_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.SelStart := Pinteger(Params^[1])^;
end;

//Read: property SelText: String read SelText write SetSelText;
procedure Lape_TCustomComboBox_SelText_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomComboBox(Params^[0])^.SelText;
end;

//Write: property SelText: String read SelText write SetSelText;
procedure Lape_TCustomComboBox_SelText_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.SelText := PlpString(Params^[1])^;
end;

//Read: property Style: TComboBoxStyle read FStyle write SetStyle default csDropDown;
procedure Lape_TCustomComboBox_Style_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComboBoxStyle(Result)^ := PCustomComboBox(Params^[0])^.Style;
end;

//Write: property Style: TComboBoxStyle read FStyle write SetStyle default csDropDown;
procedure Lape_TCustomComboBox_Style_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.Style := PComboBoxStyle(Params^[1])^;
end;

//Read: property Text: string read Text write Text;
procedure Lape_TCustomComboBox_Text_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomComboBox(Params^[0])^.Text;
end;

//Write: property Text: string read Text write Text;
procedure Lape_TCustomComboBox_Text_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.Text := PlpString(Params^[1])^;
end;

//procedure Free();
procedure Lape_TCustomComboBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomComboBox(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomComboBox', 'TWinControl');

    addGlobalFunc('procedure TCustomComboBox.Init(TheOwner: TComponent); override;', @Lape_TCustomComboBox_Init);
    addGlobalFunc('procedure TCustomComboBox.IntfGetItems(); constref;', @Lape_TCustomComboBox_IntfGetItems);
    addGlobalFunc('procedure TCustomComboBox.AddItem(const Item: String; AnObject: TObject); constref;', @Lape_TCustomComboBox_AddItem);
    addGlobalFunc('procedure TCustomComboBox.AddHistoryItem(const Item: string; MaxHistoryCount: integer;SetAsText, CaseSensitive: boolean); constref;', @Lape_TCustomComboBox_AddHistoryItem);
    addGlobalFunc('procedure TCustomComboBox.AddHistoryItem(const Item: string; AnObject: TObject;MaxHistoryCount: integer; SetAsText, CaseSensitive: boolean); constref; overload;', @Lape_TCustomComboBox_AddHistoryItemEx);
    addGlobalFunc('procedure TCustomComboBox.Clear(); constref;', @Lape_TCustomComboBox_Clear);
    addGlobalFunc('procedure TCustomComboBox.ClearSelection(); constref;', @Lape_TCustomComboBox_ClearSelection);
    addClassVar('TCustomComboBox', 'DroppedDown', 'Boolean', @Lape_TCustomComboBox_DroppedDown_Read, @Lape_TCustomComboBox_DroppedDown_Write);
    addGlobalFunc('procedure TCustomComboBox.SelectAll(); constref;', @Lape_TCustomComboBox_SelectAll);
    addClassVar('TCustomComboBox', 'AutoComplete', 'boolean', @Lape_TCustomComboBox_AutoComplete_Read, @Lape_TCustomComboBox_AutoComplete_Write);
    addClassVar('TCustomComboBox', 'AutoDropDown', 'Boolean', @Lape_TCustomComboBox_AutoDropDown_Read, @Lape_TCustomComboBox_AutoDropDown_Write);
    addClassVar('TCustomComboBox', 'AutoSelect', 'Boolean', @Lape_TCustomComboBox_AutoSelect_Read, @Lape_TCustomComboBox_AutoSelect_Write);
    addClassVar('TCustomComboBox', 'AutoSelected', 'Boolean', @Lape_TCustomComboBox_AutoSelected_Read, @Lape_TCustomComboBox_AutoSelected_Write);
    addClassVar('TCustomComboBox', 'ArrowKeysTraverseList', 'Boolean', @Lape_TCustomComboBox_ArrowKeysTraverseList_Read, @Lape_TCustomComboBox_ArrowKeysTraverseList_Write);
    addClassVar('TCustomComboBox', 'Canvas', 'TCanvas', @Lape_TCustomComboBox_Canvas_Read);
    addClassVar('TCustomComboBox', 'DropDownCount', 'Integer', @Lape_TCustomComboBox_DropDownCount_Read, @Lape_TCustomComboBox_DropDownCount_Write);
    addClassVar('TCustomComboBox', 'Items', 'TStrings', @Lape_TCustomComboBox_Items_Read, @Lape_TCustomComboBox_Items_Write);
    addClassVar('TCustomComboBox', 'ItemIndex', 'integer', @Lape_TCustomComboBox_ItemIndex_Read, @Lape_TCustomComboBox_ItemIndex_Write);
    addClassVar('TCustomComboBox', 'ReadOnly', 'Boolean', @Lape_TCustomComboBox_ReadOnly_Read, @Lape_TCustomComboBox_ReadOnly_Write);
    addClassVar('TCustomComboBox', 'SelLength', 'integer', @Lape_TCustomComboBox_SelLength_Read, @Lape_TCustomComboBox_SelLength_Write);
    addClassVar('TCustomComboBox', 'SelStart', 'integer', @Lape_TCustomComboBox_SelStart_Read, @Lape_TCustomComboBox_SelStart_Write);
    addClassVar('TCustomComboBox', 'SelText', 'String', @Lape_TCustomComboBox_SelText_Read, @Lape_TCustomComboBox_SelText_Write);
    addClassVar('TCustomComboBox', 'Style', 'TComboBoxStyle', @Lape_TCustomComboBox_Style_Read, @Lape_TCustomComboBox_Style_Write);
    addClassVar('TCustomComboBox', 'Text', 'string', @Lape_TCustomComboBox_Text_Read, @Lape_TCustomComboBox_Text_Write);
    //addGlobalFunc('procedure TCustomComboBox.Free(); constref;', @Lape_TCustomComboBox_Free);
  end;
end;

//constructor Create(TheOwner: TComponent);
procedure Lape_TComboBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComboBox(Params^[0])^ := TComboBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TComboBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PComboBox(Params^[0])^.Free();
end;

procedure Lape_Import_TComboBox(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TComboBox', 'TCustomComboBox');

    addGlobalFunc('procedure TComboBox.Init(TheOwner: TComponent); override;', @Lape_TComboBox_Init);
    //addGlobalFunc('procedure TComboBox.Free(); constref;', @Lape_TComboBox_Free);
  end;
end;

{TListBox}
//constructor Create(TheOwner: TComponent);
procedure Lape_TCustomListBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^ := TCustomListBox.Create(PComponent(Params^[1])^);
end;

//procedure AddItem(const Item: String; AnObject: TObject);
procedure Lape_TCustomListBox_AddItem(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.AddItem(PlpString(Params^[1])^, PObject(Params^[2])^);
end;

//procedure Click;
procedure Lape_TCustomListBox_Click(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Click();
end;

//procedure Clear;
procedure Lape_TCustomListBox_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Clear();
end;

//procedure ClearSelection;
procedure Lape_TCustomListBox_ClearSelection(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ClearSelection();
end;

//function GetIndexAtXY(X, Y: integer): integer;
procedure Lape_TCustomListBox_GetIndexAtXY(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.GetIndexAtXY(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

//function GetIndexAtY(Y: integer): integer;
procedure Lape_TCustomListBox_GetIndexAtY(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.GetIndexAtY(Pinteger(Params^[1])^);
end;

//function GetSelectedText: string;
procedure Lape_TCustomListBox_GetSelectedText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomListBox(Params^[0])^.GetSelectedText();
end;

//function ItemAtPos(const Pos: TPoint; Existing: Boolean): Integer;
procedure Lape_TCustomListBox_ItemAtPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.ItemAtPos(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

//function ItemRect(Index: Integer): TRect;
procedure Lape_TCustomListBox_ItemRect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRect(Result)^ := PCustomListBox(Params^[0])^.ItemRect(PInteger(Params^[1])^);
end;

//function ItemVisible(Index: Integer): boolean;
procedure Lape_TCustomListBox_ItemVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ItemVisible(PInteger(Params^[1])^);
end;

//function ItemFullyVisible(Index: Integer): boolean;
procedure Lape_TCustomListBox_ItemFullyVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ItemFullyVisible(PInteger(Params^[1])^);
end;

//procedure LockSelectionChange;
procedure Lape_TCustomListBox_LockSelectionChange(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.LockSelectionChange();
end;

//procedure MakeCurrentVisible;
procedure Lape_TCustomListBox_MakeCurrentVisible(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.MakeCurrentVisible();
end;

//procedure MeasureItem(Index: Integer; var TheHeight: Integer); virtual;
procedure Lape_TCustomListBox_MeasureItem(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.MeasureItem(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure SelectAll; virtual;
procedure Lape_TCustomListBox_SelectAll(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.SelectAll();
end;

//procedure UnlockSelectionChange;
procedure Lape_TCustomListBox_UnlockSelectionChange(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.UnlockSelectionChange();
end;

//Read: property Canvas: TCanvas read FCanvas;
procedure Lape_TCustomListBox_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PCustomListBox(Params^[0])^.Canvas;
end;

//Read: property ClickOnSelChange: boolean read ClickOnSelChange write ClickOnSelChange;
procedure Lape_TCustomListBox_ClickOnSelChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ClickOnSelChange;
end;

//Write: property ClickOnSelChange: boolean read ClickOnSelChange write ClickOnSelChange;
procedure Lape_TCustomListBox_ClickOnSelChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ClickOnSelChange := Pboolean(Params^[1])^;
end;

//Read: property Columns: Integer read Columns write Columns ;
procedure Lape_TCustomListBox_Columns_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.Columns;
end;

//Write: property Columns: Integer read Columns write Columns ;
procedure Lape_TCustomListBox_Columns_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Columns := PInteger(Params^[1])^;
end;

//Read: property Count: Integer read Count;
procedure Lape_TCustomListBox_Count_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.Count;
end;

//Read: property ExtendedSelect: boolean read ExtendedSelect write ExtendedSelect;
procedure Lape_TCustomListBox_ExtendedSelect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ExtendedSelect;
end;

//Write: property ExtendedSelect: boolean read ExtendedSelect write ExtendedSelect;
procedure Lape_TCustomListBox_ExtendedSelect_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ExtendedSelect := Pboolean(Params^[1])^;
end;

//Read: property ItemHeight: Integer read ItemHeight write ItemHeight;
procedure Lape_TCustomListBox_ItemHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.ItemHeight;
end;

//Write: property ItemHeight: Integer read ItemHeight write ItemHeight;
procedure Lape_TCustomListBox_ItemHeight_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ItemHeight := PInteger(Params^[1])^;
end;

//Read: property ItemIndex: integer read ItemIndex write ItemIndex;
procedure Lape_TCustomListBox_ItemIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.ItemIndex;
end;

//Write: property ItemIndex: integer read ItemIndex write ItemIndex;
procedure Lape_TCustomListBox_ItemIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ItemIndex := Pinteger(Params^[1])^;
end;

//Read: property Items: TStrings read Items write Items;
procedure Lape_TCustomListBox_Items_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PCustomListBox(Params^[0])^.Items;
end;

//Write: property Items: TStrings read Items write Items;
procedure Lape_TCustomListBox_Items_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Items := PStrings(Params^[1])^;
end;

//Read: property MultiSelect: boolean read MultiSelect write MultiSelect;
procedure Lape_TCustomListBox_MultiSelect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.MultiSelect;
end;

//Write: property MultiSelect: boolean read MultiSelect write MultiSelect;
procedure Lape_TCustomListBox_MultiSelect_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.MultiSelect := Pboolean(Params^[1])^;
end;

//Read: property ScrollWidth: Integer read ScrollWidth write ScrollWidth;
procedure Lape_TCustomListBox_ScrollWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.ScrollWidth;
end;

//Write: property ScrollWidth: Integer read ScrollWidth write ScrollWidth;
procedure Lape_TCustomListBox_ScrollWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ScrollWidth := PInteger(Params^[1])^;
end;

//Read: property SelCount: integer read SelCount;
procedure Lape_TCustomListBox_SelCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.SelCount;
end;

//Read: property Sorted: boolean read Sorted write Sorted;
procedure Lape_TCustomListBox_Sorted_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.Sorted;
end;

//Write: property Sorted: boolean read Sorted write Sorted;
procedure Lape_TCustomListBox_Sorted_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Sorted := Pboolean(Params^[1])^;
end;

//Read: property TopIndex: Integer read TopIndex write TopIndex;
procedure Lape_TCustomListBox_TopIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.TopIndex;
end;

//Write: property TopIndex: Integer read TopIndex write TopIndex;
procedure Lape_TCustomListBox_TopIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.TopIndex := PInteger(Params^[1])^;
end;

//Read: property Style: TListBoxStyle;
procedure Lape_TCustomListBox_Style_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PListBoxStyle(Result)^ := PCustomListBox(Params^[0])^.Style;
end;

//Write: property Style: TListBoxStyle;
procedure Lape_TCustomListBox_Style_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Style := PListBoxStyle(Params^[1])^;
end;

//procedure Free();
procedure Lape_TCustomListBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Free();
end;

procedure Lape_TCustomListBox_OnDrawItem_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.OnDrawItem := PDrawItemEvent(Params^[1])^;
end;

procedure Lape_Import_TCustomListBox(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomListBox', 'TWinControl');

    addGlobalFunc('procedure TCustomListBox.Init(TheOwner: TComponent); override;', @Lape_TCustomListBox_Init);
    addGlobalFunc('procedure TCustomListBox.AddItem(const Item: String; AnObject: TObject); constref;', @Lape_TCustomListBox_AddItem);
    addGlobalFunc('procedure TCustomListBox.Click(); constref;', @Lape_TCustomListBox_Click);
    addGlobalFunc('procedure TCustomListBox.Clear(); constref;', @Lape_TCustomListBox_Clear);
    addGlobalFunc('procedure TCustomListBox.ClearSelection(); constref;', @Lape_TCustomListBox_ClearSelection);
    addGlobalFunc('function TCustomListBox.GetIndexAtXY(X, Y: integer): integer; constref;', @Lape_TCustomListBox_GetIndexAtXY);
    addGlobalFunc('function TCustomListBox.GetIndexAtY(Y: integer): integer; constref;', @Lape_TCustomListBox_GetIndexAtY);
    addGlobalFunc('function TCustomListBox.GetSelectedText(): string; constref;', @Lape_TCustomListBox_GetSelectedText);
    addGlobalFunc('function TCustomListBox.ItemAtPos(const Pos: TPoint; Existing: Boolean): Integer; constref;', @Lape_TCustomListBox_ItemAtPos);
    addGlobalFunc('function TCustomListBox.ItemRect(Index: Integer): TRect; constref;', @Lape_TCustomListBox_ItemRect);
    addGlobalFunc('function TCustomListBox.ItemVisible(Index: Integer): boolean; constref;', @Lape_TCustomListBox_ItemVisible);
    addGlobalFunc('function TCustomListBox.ItemFullyVisible(Index: Integer): boolean; constref;', @Lape_TCustomListBox_ItemFullyVisible);
    addGlobalFunc('procedure TCustomListBox.LockSelectionChange(); constref;', @Lape_TCustomListBox_LockSelectionChange);
    addGlobalFunc('procedure TCustomListBox.MakeCurrentVisible(); constref;', @Lape_TCustomListBox_MakeCurrentVisible);
    addGlobalFunc('procedure TCustomListBox.MeasureItem(Index: Integer; var TheHeight: Integer); constref;', @Lape_TCustomListBox_MeasureItem);
    addGlobalFunc('procedure TCustomListBox.SelectAll(); constref;', @Lape_TCustomListBox_SelectAll);
    addGlobalFunc('procedure TCustomListBox.UnlockSelectionChange(); constref;', @Lape_TCustomListBox_UnlockSelectionChange);
    addClassVar('TCustomListBox', 'Canvas', 'TCanvas', @Lape_TCustomListBox_Canvas_Read);
    addClassVar('TCustomListBox', 'ClickOnSelChange', 'boolean', @Lape_TCustomListBox_ClickOnSelChange_Read, @Lape_TCustomListBox_ClickOnSelChange_Write);
    addClassVar('TCustomListBox', 'Columns', 'Integer', @Lape_TCustomListBox_Columns_Read, @Lape_TCustomListBox_Columns_Write);
    addClassVar('TCustomListBox', 'Count', 'Integer', @Lape_TCustomListBox_Count_Read);
    addClassVar('TCustomListBox', 'ExtendedSelect', 'boolean', @Lape_TCustomListBox_ExtendedSelect_Read, @Lape_TCustomListBox_ExtendedSelect_Write);
    addClassVar('TCustomListBox', 'ItemHeight', 'Integer', @Lape_TCustomListBox_ItemHeight_Read, @Lape_TCustomListBox_ItemHeight_Write);
    addClassVar('TCustomListBox', 'ItemIndex', 'integer', @Lape_TCustomListBox_ItemIndex_Read, @Lape_TCustomListBox_ItemIndex_Write);
    addClassVar('TCustomListBox', 'Items', 'TStrings', @Lape_TCustomListBox_Items_Read, @Lape_TCustomListBox_Items_Write);
    addClassVar('TCustomListBox', 'MultiSelect', 'boolean', @Lape_TCustomListBox_MultiSelect_Read, @Lape_TCustomListBox_MultiSelect_Write);
    addClassVar('TCustomListBox', 'ScrollWidth', 'Integer', @Lape_TCustomListBox_ScrollWidth_Read, @Lape_TCustomListBox_ScrollWidth_Write);
    addClassVar('TCustomListBox', 'SelCount', 'integer', @Lape_TCustomListBox_SelCount_Read);
    addClassVar('TCustomListBox', 'Sorted', 'boolean', @Lape_TCustomListBox_Sorted_Read, @Lape_TCustomListBox_Sorted_Write);
    addClassVar('TCustomListBox', 'TopIndex', 'Integer', @Lape_TCustomListBox_TopIndex_Read, @Lape_TCustomListBox_TopIndex_Write);
    addClassVar('TCustomListBox', 'OnDrawItem', 'TDrawItemEvent', nil, @Lape_TCustomListBox_OnDrawItem_Write);
    addClassVar('TCustomListBox', 'Style', 'TListBoxStyle', @Lape_TCustomListBox_Style_Read, @Lape_TCustomListBox_Style_Write);
    //addGlobalFunc('procedure TCustomListBox.Free(); constref;', @Lape_TCustomListBox_Free);
  end;
end;

//constructor Create(TheOwner: TComponent);
procedure Lape_TListBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PListBox(Params^[0])^ := TListBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TListBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PListBox(Params^[0])^.Free();
end;

procedure Lape_Import_TListBox(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TListBox', 'TCustomListBox');

    addGlobalFunc('procedure TListBox.Init(TheOwner: TComponent); override;', @Lape_TListBox_Init);
   // addGlobalFunc('procedure TListBox.Free(); constref;', @Lape_TListBox_Free);
  end;
end;

{TEdit}
//constructor Create(AOwner: TComponent);
procedure Lape_TCustomEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^ := TCustomEdit.Create(PComponent(Params^[1])^);
end;

//procedure Clear;
procedure Lape_TCustomEdit_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.Clear();
end;

//procedure SelectAll;
procedure Lape_TCustomEdit_SelectAll(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.SelectAll();
end;

//procedure ClearSelection;
procedure Lape_TCustomEdit_ClearSelection(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.ClearSelection();
end;

//procedure CopyToClipboard;
procedure Lape_TCustomEdit_CopyToClipboard(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.CopyToClipboard();
end;

//procedure CutToClipboard;
procedure Lape_TCustomEdit_CutToClipboard(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.CutToClipboard();
end;

//procedure PasteFromClipboard;
procedure Lape_TCustomEdit_PasteFromClipboard(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.PasteFromClipboard();
end;

//procedure Undo;
procedure Lape_TCustomEdit_Undo(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.Undo();
end;

//Read: property CanUndo: Boolean read CanUndo;
procedure Lape_TCustomEdit_CanUndo_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.CanUndo;
end;

//Read: property CaretPos: TPoint read CaretPos write CaretPos;
procedure Lape_TCustomEdit_CaretPos_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PCustomEdit(Params^[0])^.CaretPos;
end;

//Write: property CaretPos: TPoint read CaretPos write CaretPos;
procedure Lape_TCustomEdit_CaretPos_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.CaretPos := PPoint(Params^[1])^;
end;

//Read: property HideSelection: Boolean read HideSelection write HideSelection;
procedure Lape_TCustomEdit_HideSelection_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.HideSelection;
end;

//Write: property HideSelection: Boolean read HideSelection write HideSelection;
procedure Lape_TCustomEdit_HideSelection_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.HideSelection := PBoolean(Params^[1])^;
end;

//Read: property MaxLength: Integer read MaxLength write MaxLength;
procedure Lape_TCustomEdit_MaxLength_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomEdit(Params^[0])^.MaxLength;
end;

//Write: property MaxLength: Integer read MaxLength write MaxLength;
procedure Lape_TCustomEdit_MaxLength_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.MaxLength := PInteger(Params^[1])^;
end;

//Read: property Modified: Boolean read Modified write Modified;
procedure Lape_TCustomEdit_Modified_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.Modified;
end;

//Write: property Modified: Boolean read Modified write Modified;
procedure Lape_TCustomEdit_Modified_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.Modified := PBoolean(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure Lape_TCustomEdit_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomEdit(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure Lape_TCustomEdit_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//Read: property PasswordChar: Char read PasswordChar write PasswordChar;
procedure Lape_TCustomEdit_PasswordChar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PChar(Result)^ := PCustomEdit(Params^[0])^.PasswordChar;
end;

//Write: property PasswordChar: Char read PasswordChar write PasswordChar;
procedure Lape_TCustomEdit_PasswordChar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.PasswordChar := PChar(Params^[1])^;
end;

//Read: property ReadOnly: Boolean read ReadOnly write ReadOnly;
procedure Lape_TCustomEdit_ReadOnly_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.ReadOnly;
end;

//Write: property ReadOnly: Boolean read ReadOnly write ReadOnly;
procedure Lape_TCustomEdit_ReadOnly_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.ReadOnly := PBoolean(Params^[1])^;
end;

//Read: property SelLength: integer read SelLength write SelLength;
procedure Lape_TCustomEdit_SelLength_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomEdit(Params^[0])^.SelLength;
end;

//Write: property SelLength: integer read SelLength write SelLength;
procedure Lape_TCustomEdit_SelLength_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.SelLength := Pinteger(Params^[1])^;
end;

//Read: property SelStart: integer read SelStart write SelStart;
procedure Lape_TCustomEdit_SelStart_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomEdit(Params^[0])^.SelStart;
end;

//Write: property SelStart: integer read SelStart write SelStart;
procedure Lape_TCustomEdit_SelStart_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.SelStart := Pinteger(Params^[1])^;
end;

//Read: property SelText: String read SelText write SelText;
procedure Lape_TCustomEdit_SelText_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomEdit(Params^[0])^.SelText;
end;

//Write: property SelText: String read SelText write SelText;
procedure Lape_TCustomEdit_SelText_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.SelText := PlpString(Params^[1])^;
end;

//Read: property Text: string read Text write Text;
procedure Lape_TCustomEdit_Text_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomEdit(Params^[0])^.Text;
end;

//Write: property Text: string read Text write Text;
procedure Lape_TCustomEdit_Text_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.Text := PlpString(Params^[1])^;
end;

//procedure Free();
procedure Lape_TCustomEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomEdit(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomEdit', 'TWinControl');

    addGlobalFunc('procedure TCustomEdit.Init(AOwner: TComponent); override;', @Lape_TCustomEdit_Init);
    addGlobalFunc('procedure TCustomEdit.Clear(); constref;', @Lape_TCustomEdit_Clear);
    addGlobalFunc('procedure TCustomEdit.SelectAll(); constref;', @Lape_TCustomEdit_SelectAll);
    addGlobalFunc('procedure TCustomEdit.ClearSelection(); constref;', @Lape_TCustomEdit_ClearSelection);
    addGlobalFunc('procedure TCustomEdit.CopyToClipboard(); constref;', @Lape_TCustomEdit_CopyToClipboard);
    addGlobalFunc('procedure TCustomEdit.CutToClipboard(); constref;', @Lape_TCustomEdit_CutToClipboard);
    addGlobalFunc('procedure TCustomEdit.PasteFromClipboard(); constref;', @Lape_TCustomEdit_PasteFromClipboard);
    addGlobalFunc('procedure TCustomEdit.Undo(); constref;', @Lape_TCustomEdit_Undo);
    addClassVar('TCustomEdit', 'CanUndo', 'Boolean', @Lape_TCustomEdit_CanUndo_Read);
    addClassVar('TCustomEdit', 'CaretPos', 'TPoint', @Lape_TCustomEdit_CaretPos_Read, @Lape_TCustomEdit_CaretPos_Write);
    addClassVar('TCustomEdit', 'HideSelection', 'Boolean', @Lape_TCustomEdit_HideSelection_Read, @Lape_TCustomEdit_HideSelection_Write);
    addClassVar('TCustomEdit', 'MaxLength', 'Integer', @Lape_TCustomEdit_MaxLength_Read, @Lape_TCustomEdit_MaxLength_Write);
    addClassVar('TCustomEdit', 'Modified', 'Boolean', @Lape_TCustomEdit_Modified_Read, @Lape_TCustomEdit_Modified_Write);
    addClassVar('TCustomEdit', 'OnChange', 'TNotifyEvent', @Lape_TCustomEdit_OnChange_Read, @Lape_TCustomEdit_OnChange_Write);
    addClassVar('TCustomEdit', 'PasswordChar', 'Char', @Lape_TCustomEdit_PasswordChar_Read, @Lape_TCustomEdit_PasswordChar_Write);
    addClassVar('TCustomEdit', 'ReadOnly', 'Boolean', @Lape_TCustomEdit_ReadOnly_Read, @Lape_TCustomEdit_ReadOnly_Write);
    addClassVar('TCustomEdit', 'SelLength', 'integer', @Lape_TCustomEdit_SelLength_Read, @Lape_TCustomEdit_SelLength_Write);
    addClassVar('TCustomEdit', 'SelStart', 'integer', @Lape_TCustomEdit_SelStart_Read, @Lape_TCustomEdit_SelStart_Write);
    addClassVar('TCustomEdit', 'SelText', 'String', @Lape_TCustomEdit_SelText_Read, @Lape_TCustomEdit_SelText_Write);
    addClassVar('TCustomEdit', 'Text', 'string', @Lape_TCustomEdit_Text_Read, @Lape_TCustomEdit_Text_Write);
    //addGlobalFunc('procedure TCustomEdit.Free(); constref;', @Lape_TCustomEdit_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure Lape_TEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEdit(Params^[0])^ := TEdit.Create(PComponent(Params^[1])^);
end;

procedure Lape_TEdit_OnEditingDone_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PEdit(Params^[0])^.OnEditingDone;
end;

procedure Lape_TEdit_OnEditingDone_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEdit(Params^[0])^.OnEditingDone := PNotifyEvent(Params^[1])^;
end;

//procedure Free();
procedure Lape_TEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PEdit(Params^[0])^.Free();
end;

procedure Lape_Import_TEdit(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TEdit', 'TCustomEdit');

    addClassVar('TEdit', 'OnEditingDone', 'TNotifyEvent', @Lape_TEdit_OnEditingDone_Read, @Lape_TEdit_OnEditingDone_Write);
    addGlobalFunc('procedure TEdit.Init(AOwner: TComponent); override;', @Lape_TEdit_Init);
    //addGlobalFunc('procedure TEdit.Free(); constref;', @Lape_TEdit_Free);
  end;
end;

procedure Lape_TCustomGroupBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomGroupBox(Params^[0])^ := TCustomGroupBox.Create(PComponent(Params^[1])^);
end;

procedure Lape_TCustomGroupBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomGroupBox(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomGroupBox(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomGroupBox', 'TWinControl');

    addGlobalFunc('procedure TCustomGroupBox.Init(AOwner: TComponent); override;', @Lape_TCustomGroupBox_Init);
    //addGlobalFunc('procedure TCustomGroupBox.Free(); constref;', @Lape_TCustomGroupBox_Free);
  end;
end;

procedure Lape_TGroupBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PGroupBox(Params^[0])^ := TGroupBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TGroupBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PGroupBox(Params^[0])^.Free();
end;

procedure Lape_Import_TGroupBox(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TGroupBox', 'TCustomGroupBox');

    addGlobalFunc('procedure TGroupBox.Init(AOwner: TComponent); override;', @Lape_TGroupBox_Init);
    //addGlobalFunc('procedure TGroupBox.Free(); constref;', @Lape_TGroupBox_Free);
  end;
end;

{TMemo}
//constructor Create(AOwner: TComponent);
procedure Lape_TMemoScrollbar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemoScrollbar(Params^[0])^ := TMemoScrollbar.Create(PWinControl(Params^[1])^, PScrollBarKind(Params^[2])^);
end;

//procedure Free();
procedure Lape_TMemoScrollbar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemoScrollbar(Params^[0])^.Free();
end;

procedure Lape_Import_TMemoScrollbar(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TMemoScrollbar', 'TControlScrollBar');

    addGlobalFunc('procedure TMemoScrollbar.Init(AControl: TWinControl; AKind: TScrollBarKind);', @Lape_TMemoScrollbar_Init);
    //addGlobalFunc('procedure TMemoScrollbar.Free(); constref;', @Lape_TMemoScrollbar_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure Lape_TCustomMemo_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^ := TCustomMemo.Create(PComponent(Params^[1])^);
end;

//procedure Append(const Value: String);
procedure Lape_TCustomMemo_Append(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.Append(PlpString(Params^[1])^);
end;

//Read: property Lines: TStrings read Lines write Lines;
procedure Lape_TCustomMemo_Lines_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PCustomMemo(Params^[0])^.Lines;
end;

//Write: property Lines: TStrings read Lines write Lines;
procedure Lape_TCustomMemo_Lines_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.Lines := PStrings(Params^[1])^;
end;

//Read: property HorzScrollBar: TMemoScrollBar read HorzScrollBar write HorzScrollBar;
procedure Lape_TCustomMemo_HorzScrollBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemoScrollBar(Result)^ := PCustomMemo(Params^[0])^.HorzScrollBar;
end;

//Write: property HorzScrollBar: TMemoScrollBar read HorzScrollBar write HorzScrollBar;
procedure Lape_TCustomMemo_HorzScrollBar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.HorzScrollBar := PMemoScrollBar(Params^[1])^;
end;

//Read: property VertScrollBar: TMemoScrollBar read VertScrollBar write VertScrollBar;
procedure Lape_TCustomMemo_VertScrollBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemoScrollBar(Result)^ := PCustomMemo(Params^[0])^.VertScrollBar;
end;

//Write: property VertScrollBar: TMemoScrollBar read VertScrollBar write VertScrollBar;
procedure Lape_TCustomMemo_VertScrollBar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.VertScrollBar := PMemoScrollBar(Params^[1])^;
end;

//Read: property ScrollBars: TScrollStyle read ScrollBars write ScrollBars;
procedure Lape_TCustomMemo_ScrollBars_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PScrollStyle(Result)^ := PCustomMemo(Params^[0])^.ScrollBars;
end;

//Write: property ScrollBars: TScrollStyle read ScrollBars write ScrollBars;
procedure Lape_TCustomMemo_ScrollBars_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.ScrollBars := PScrollStyle(Params^[1])^;
end;

//Read: property WantReturns: Boolean read WantReturns write WantReturns;
procedure Lape_TCustomMemo_WantReturns_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomMemo(Params^[0])^.WantReturns;
end;

//Write: property WantReturns: Boolean read WantReturns write WantReturns;
procedure Lape_TCustomMemo_WantReturns_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.WantReturns := PBoolean(Params^[1])^;
end;

//Read: property WantTabs: Boolean read WantTabs write WantTabs;
procedure Lape_TCustomMemo_WantTabs_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomMemo(Params^[0])^.WantTabs;
end;

//Write: property WantTabs: Boolean read WantTabs write WantTabs;
procedure Lape_TCustomMemo_WantTabs_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.WantTabs := PBoolean(Params^[1])^;
end;

//Read: property WordWrap: Boolean read WordWrap write WordWrap;
procedure Lape_TCustomMemo_WordWrap_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomMemo(Params^[0])^.WordWrap;
end;

//Write: property WordWrap: Boolean read WordWrap write WordWrap;
procedure Lape_TCustomMemo_WordWrap_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.WordWrap := PBoolean(Params^[1])^;
end;

//procedure Free();
procedure Lape_TCustomMemo_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomMemo(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomMemo', 'TCustomEdit');

    addGlobalFunc('procedure TCustomMemo.Init(AOwner: TComponent); override;', @Lape_TCustomMemo_Init);
    addGlobalFunc('procedure TCustomMemo.Append(const Value: String); constref;', @Lape_TCustomMemo_Append);
    addClassVar('TCustomMemo', 'Lines', 'TStrings', @Lape_TCustomMemo_Lines_Read, @Lape_TCustomMemo_Lines_Write);
    addClassVar('TCustomMemo', 'HorzScrollBar', 'TMemoScrollBar', @Lape_TCustomMemo_HorzScrollBar_Read, @Lape_TCustomMemo_HorzScrollBar_Write);
    addClassVar('TCustomMemo', 'VertScrollBar', 'TMemoScrollBar', @Lape_TCustomMemo_VertScrollBar_Read, @Lape_TCustomMemo_VertScrollBar_Write);
    addClassVar('TCustomMemo', 'ScrollBars', 'TScrollStyle', @Lape_TCustomMemo_ScrollBars_Read, @Lape_TCustomMemo_ScrollBars_Write);
    addClassVar('TCustomMemo', 'WantReturns', 'Boolean', @Lape_TCustomMemo_WantReturns_Read, @Lape_TCustomMemo_WantReturns_Write);
    addClassVar('TCustomMemo', 'WantTabs', 'Boolean', @Lape_TCustomMemo_WantTabs_Read, @Lape_TCustomMemo_WantTabs_Write);
    addClassVar('TCustomMemo', 'WordWrap', 'Boolean', @Lape_TCustomMemo_WordWrap_Read, @Lape_TCustomMemo_WordWrap_Write);
    //addGlobalFunc('procedure TCustomMemo.Free(); constref;', @Lape_TCustomMemo_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure Lape_TMemo_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemo(Params^[0])^ := TMemo.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TMemo_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMemo(Params^[0])^.Free();
end;

procedure Lape_Import_TMemo(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TMemo', 'TCustomMemo');

    addGlobalFunc('procedure TMemo.Init(AOwner: TComponent); override;', @Lape_TMemo_Init);
    //addGlobalFunc('procedure TMemo.Free(); constref;', @Lape_TMemo_Free);
  end;
end;


{TStaticText}
//constructor Create(AOwner: TComponent);
procedure Lape_TCustomStaticText_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomStaticText(Params^[0])^ := TCustomStaticText.Create(PComponent(Params^[1])^);
end;

//Read: property Alignment: TAlignment read Alignment write Alignment;
procedure Lape_TCustomStaticText_Alignment_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PAlignment(Result)^ := PCustomStaticText(Params^[0])^.Alignment;
end;

//Write: property Alignment: TAlignment read Alignment write Alignment;
procedure Lape_TCustomStaticText_Alignment_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomStaticText(Params^[0])^.Alignment := PAlignment(Params^[1])^;
end;

//Read: property BorderStyle: TStaticBorderStyle read StaticBorderStyle write StaticBorderStyle;
procedure Lape_TCustomStaticText_BorderStyle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStaticBorderStyle(Result)^ := PCustomStaticText(Params^[0])^.BorderStyle;
end;

//Write: property BorderStyle: TStaticBorderStyle read StaticBorderStyle write StaticBorderStyle;
procedure Lape_TCustomStaticText_BorderStyle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomStaticText(Params^[0])^.BorderStyle := PStaticBorderStyle(Params^[1])^;
end;

//Read: property FocusControl: TWinControl read FocusControl write FocusControl;
procedure Lape_TCustomStaticText_FocusControl_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWinControl(Result)^ := PCustomStaticText(Params^[0])^.FocusControl;
end;

//Write: property FocusControl: TWinControl read FocusControl write FocusControl;
procedure Lape_TCustomStaticText_FocusControl_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomStaticText(Params^[0])^.FocusControl := PWinControl(Params^[1])^;
end;

//Read: property ShowAccelChar: boolean read ShowAccelChar write ShowAccelChar;
procedure Lape_TCustomStaticText_ShowAccelChar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomStaticText(Params^[0])^.ShowAccelChar;
end;

//Write: property ShowAccelChar: boolean read ShowAccelChar write ShowAccelChar;
procedure Lape_TCustomStaticText_ShowAccelChar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomStaticText(Params^[0])^.ShowAccelChar := Pboolean(Params^[1])^;
end;

//Read: property Transparent: Boolean read Transparent write Transparent;
procedure Lape_TCustomStaticText_Transparent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomStaticText(Params^[0])^.Transparent;
end;

//Write: property Transparent: Boolean read Transparent write Transparent;
procedure Lape_TCustomStaticText_Transparent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomStaticText(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

//procedure Free();
procedure Lape_TCustomStaticText_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomStaticText(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomStaticText(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomStaticText', 'TWinControl');

    addGlobalFunc('procedure TCustomStaticText.Init(AOwner: TComponent); override;', @Lape_TCustomStaticText_Init);
    addClassVar('TCustomStaticText', 'Alignment', 'TAlignment', @Lape_TCustomStaticText_Alignment_Read, @Lape_TCustomStaticText_Alignment_Write);
    addClassVar('TCustomStaticText', 'BorderStyle', 'TStaticBorderStyle', @Lape_TCustomStaticText_BorderStyle_Read, @Lape_TCustomStaticText_BorderStyle_Write);
    addClassVar('TCustomStaticText', 'FocusControl', 'TWinControl', @Lape_TCustomStaticText_FocusControl_Read, @Lape_TCustomStaticText_FocusControl_Write);
    addClassVar('TCustomStaticText', 'ShowAccelChar', 'boolean', @Lape_TCustomStaticText_ShowAccelChar_Read, @Lape_TCustomStaticText_ShowAccelChar_Write);
    addClassVar('TCustomStaticText', 'Transparent', 'Boolean', @Lape_TCustomStaticText_Transparent_Read, @Lape_TCustomStaticText_Transparent_Write);
   // addGlobalFunc('procedure TCustomStaticText.Free(); constref;', @Lape_TCustomStaticText_Free);
  end;
end;

//constructor Create(AOwner: TComponent);
procedure Lape_TStaticText_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStaticText(Params^[0])^ := TStaticText.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TStaticText_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStaticText(Params^[0])^.Free();
end;

procedure Lape_Import_TStaticText(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TStaticText', 'TCustomStaticText');

    addGlobalFunc('procedure TStaticText.Init(AOwner: TComponent); override;', @Lape_TStaticText_Init);
    //addGlobalFunc('procedure TStaticText.Free(); constref;', @Lape_TStaticText_Free);
  end;
end;


{TButton}

//constructor Create(TheOwner: TComponent);
procedure Lape_TButtonControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PButtonControl(Params^[0])^ := TButtonControl.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TButtonControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PButtonControl(Params^[0])^.Free();
end;

procedure Lape_Import_TButtonControl(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TButtonControl', 'TWinControl');

    addGlobalFunc('procedure TButtonControl.Init(TheOwner: TComponent); override;', @Lape_TButtonControl_Init);
    //addGlobalFunc('procedure TButtonControl.Free(); constref;', @Lape_TButtonControl_Free);
  end;
end;

//constructor Create(TheOwner: TComponent);
procedure Lape_TCustomButton_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^ := TCustomButton.Create(PComponent(Params^[1])^);
end;

//procedure ExecuteDefaultAction;
procedure Lape_TCustomButton_ExecuteDefaultAction(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.ExecuteDefaultAction();
end;

//procedure ExecuteCancelAction;
procedure Lape_TCustomButton_ExecuteCancelAction(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.ExecuteCancelAction();
end;

//procedure ActiveDefaultControlChanged(NewControl: TControl);
procedure Lape_TCustomButton_ActiveDefaultControlChanged(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.ActiveDefaultControlChanged(PControl(Params^[1])^);
end;

//procedure UpdateRolesForForm;
procedure Lape_TCustomButton_UpdateRolesForForm(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.UpdateRolesForForm();
end;

//function UseRightToLeftAlignment: Boolean;
procedure Lape_TCustomButton_UseRightToLeftAlignment(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomButton(Params^[0])^.UseRightToLeftAlignment();
end;

//Read: property Active: boolean read Active;
procedure Lape_TCustomButton_Active_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomButton(Params^[0])^.Active;
end;

//Read: property Default: Boolean read Default write Default;
procedure Lape_TCustomButton_Default_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomButton(Params^[0])^.Default;
end;

//Write: property Default: Boolean read Default write Default;
procedure Lape_TCustomButton_Default_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.Default := PBoolean(Params^[1])^;
end;

//Read: property Cancel: Boolean read Cancel write Cancel;
procedure Lape_TCustomButton_Cancel_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomButton(Params^[0])^.Cancel;
end;

//Write: property Cancel: Boolean read Cancel write Cancel;
procedure Lape_TCustomButton_Cancel_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.Cancel := PBoolean(Params^[1])^;
end;

//procedure Free();
procedure Lape_TCustomButton_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomButton(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomButton', 'TButtonControl');

    addGlobalFunc('procedure TCustomButton.Init(TheOwner: TComponent); override;', @Lape_TCustomButton_Init);
    //addGlobalFunc('procedure TCustomButton.ExecuteDefaultAction(); constref;', @Lape_TCustomButton_ExecuteDefaultAction);
    //addGlobalFunc('procedure TCustomButton.ExecuteCancelAction(); constref;', @Lape_TCustomButton_ExecuteCancelAction);
    //addGlobalFunc('procedure TCustomButton.ActiveDefaultControlChanged(NewControl: TControl); constref;', @Lape_TCustomButton_ActiveDefaultControlChanged);
    //addGlobalFunc('procedure TCustomButton.UpdateRolesForForm(); constref;', @Lape_TCustomButton_UpdateRolesForForm);
    //addGlobalFunc('function TCustomButton.UseRightToLeftAlignment(): Boolean; constref;', @Lape_TCustomButton_UseRightToLeftAlignment);
    addClassVar('TCustomButton', 'Active', 'boolean', @Lape_TCustomButton_Active_Read);
    addClassVar('TCustomButton', 'Default', 'Boolean', @Lape_TCustomButton_Default_Read, @Lape_TCustomButton_Default_Write);
    addClassVar('TCustomButton', 'Cancel', 'Boolean', @Lape_TCustomButton_Cancel_Read, @Lape_TCustomButton_Cancel_Write);
    //addGlobalFunc('procedure TCustomButton.Free(); constref;', @Lape_TCustomButton_Free);
  end;
end;

//constructor Create(TheOwner: TComponent);
procedure Lape_TButton_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PButton(Params^[0])^ := TButton.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TButton_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PButton(Params^[0])^.Free();
end;

procedure Lape_Import_TButton(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TButton', 'TCustomButton');

    addGlobalFunc('procedure TButton.Init(TheOwner: TComponent); override;', @Lape_TButton_Init);
    //addGlobalFunc('procedure TButton.Free(); constref;', @Lape_TButton_Free);
  end;
end;
{TCheckBox}

//constructor Create(TheOwner: TComponent);
procedure Lape_TCustomCheckBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckBox(Params^[0])^ := TCustomCheckBox.Create(PComponent(Params^[1])^);
end;

//Read: property AllowGrayed: Boolean read AllowGrayed write AllowGrayed;
procedure Lape_TCustomCheckBox_AllowGrayed_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomCheckBox(Params^[0])^.AllowGrayed;
end;

//Write: property AllowGrayed: Boolean read AllowGrayed write AllowGrayed;
procedure Lape_TCustomCheckBox_AllowGrayed_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckBox(Params^[0])^.AllowGrayed := PBoolean(Params^[1])^;
end;

//Read: property State: TCheckBoxState read State write State;
procedure Lape_TCustomCheckBox_State_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCheckBoxState(Result)^ := PCustomCheckBox(Params^[0])^.State;
end;

//Write: property State: TCheckBoxState read State write State;
procedure Lape_TCustomCheckBox_State_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckBox(Params^[0])^.State := PCheckBoxState(Params^[1])^;
end;

//Read: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure Lape_TCustomCheckBox_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomCheckBox(Params^[0])^.OnChange;
end;

//Write: property OnChange: TNotifyEvent read OnChange write OnChange;
procedure Lape_TCustomCheckBox_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckBox(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

//procedure Free();
procedure Lape_TCustomCheckBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomCheckBox(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomCheckBox(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomCheckBox', 'TButtonControl');

    addGlobalFunc('procedure TCustomCheckBox.Init(TheOwner: TComponent); override;', @Lape_TCustomCheckBox_Init);
    addClassVar('TCustomCheckBox', 'AllowGrayed', 'Boolean', @Lape_TCustomCheckBox_AllowGrayed_Read, @Lape_TCustomCheckBox_AllowGrayed_Write);
    addClassVar('TCustomCheckBox', 'State', 'TCheckBoxState', @Lape_TCustomCheckBox_State_Read, @Lape_TCustomCheckBox_State_Write);
    addClassVar('TCustomCheckBox', 'OnChange', 'TNotifyEvent', @Lape_TCustomCheckBox_OnChange_Read, @Lape_TCustomCheckBox_OnChange_Write);
   // addGlobalFunc('procedure TCustomCheckBox.Free(); constref;', @Lape_TCustomCheckBox_Free);
  end;
end;

//constructor Create(TheOwner: TComponent);
procedure Lape_TCheckBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCheckBox(Params^[0])^ := TCheckBox.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TCheckBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCheckBox(Params^[0])^.Free();
end;

procedure Lape_Import_TCheckBox(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCheckBox', 'TCustomCheckBox');

    addGlobalFunc('procedure TCheckBox.Init(TheOwner: TComponent); override;', @Lape_TCheckBox_Init);
    //addGlobalFunc('procedure TCheckBox.Free(); constref;', @Lape_TCheckBox_Free);
  end;
end;
{TLabel}

//constructor Create(TheOwner: TComponent);
procedure Lape_TCustomLabel_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomLabel(Params^[0])^ := TCustomLabel.Create(PComponent(Params^[1])^);
end;

//function ColorIsStored: boolean;
procedure Lape_TCustomLabel_ColorIsStored(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomLabel(Params^[0])^.ColorIsStored();
end;

//function AdjustFontForOptimalFill: Boolean;
procedure Lape_TCustomLabel_AdjustFontForOptimalFill(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomLabel(Params^[0])^.AdjustFontForOptimalFill();
end;

//procedure Paint;
procedure Lape_TCustomLabel_Paint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomLabel(Params^[0])^.Paint();
end;

//procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer);
procedure Lape_TCustomLabel_SetBounds(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomLabel(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

//procedure Free();
procedure Lape_TCustomLabel_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomLabel(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomLabel(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomLabel', 'TGraphicControl');

    addGlobalFunc('procedure TCustomLabel.Init(TheOwner: TComponent); override;', @Lape_TCustomLabel_Init);
    addGlobalFunc('function TCustomLabel.ColorIsStored(): boolean; constref;', @Lape_TCustomLabel_ColorIsStored);
    addGlobalFunc('function TCustomLabel.AdjustFontForOptimalFill(): Boolean; constref;', @Lape_TCustomLabel_AdjustFontForOptimalFill);
    addGlobalFunc('procedure TCustomLabel.Paint(); constref;', @Lape_TCustomLabel_Paint);
    //addGlobalFunc('procedure TCustomLabel.SetBounds(aLeft, aTop, aWidth, aHeight: integer); constref;', @Lape_TCustomLabel_SetBounds);
    //addGlobalFunc('procedure TCustomLabel.Free(); constref;', @Lape_TCustomLabel_Free);
  end;
end;
//constructor Create(TheOwner: TComponent);
procedure Lape_TLabel_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLabel(Params^[0])^ := TLabel.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TLabel_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLabel(Params^[0])^.Free();
end;

procedure Lape_Import_TLabel(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TLabel', 'TCustomLabel');

    addGlobalFunc('procedure TLabel.Init(TheOwner: TComponent); override;', @Lape_TLabel_Init);
    //addGlobalFunc('procedure TLabel.Free(); constref;', @Lape_TLabel_Free);
  end;
end;

//function FindDownButton: TCustomSpeedButton;
procedure Lape_TCustomSpeedButton_FindDownButton(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Result)^ := PCustomSpeedButton(Params^[0])^.FindDownButton();
end;

//procedure LoadGlyphFromLazarusResource(const AName: String);
procedure Lape_TCustomSpeedButton_LoadGlyphFromLazarusResource(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.LoadGlyphFromLazarusResource(PlpString(Params^[1])^);
end;

//Read: property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default false;
procedure Lape_TCustomSpeedButton_AllowAllUp_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.AllowAllUp;
end;

//Write: property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default false;
procedure Lape_TCustomSpeedButton_AllowAllUp_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.AllowAllUp := PBoolean(Params^[1])^;
end;

//Read: property Down: Boolean read FDown write SetDown default false;
procedure Lape_TCustomSpeedButton_Down_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.Down;
end;

//Write: property Down: Boolean read FDown write SetDown default false;
procedure Lape_TCustomSpeedButton_Down_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Down := PBoolean(Params^[1])^;
end;

//Read: property Flat: Boolean read FFlat write SetFlat default false;
procedure Lape_TCustomSpeedButton_Flat_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.Flat;
end;

//Write: property Flat: Boolean read FFlat write SetFlat default false;
procedure Lape_TCustomSpeedButton_Flat_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Flat := PBoolean(Params^[1])^;
end;

//Read: property Glyph: TBitmap read GetGlyph write SetGlyph;
procedure Lape_TCustomSpeedButton_Glyph_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBitmap(Result)^ := PCustomSpeedButton(Params^[0])^.Glyph;
end;

//Write: property Glyph: TBitmap read GetGlyph write SetGlyph;
procedure Lape_TCustomSpeedButton_Glyph_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Glyph := PBitmap(Params^[1])^;
end;

//Read: property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
procedure Lape_TCustomSpeedButton_GroupIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomSpeedButton(Params^[0])^.GroupIndex;
end;

//Write: property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
procedure Lape_TCustomSpeedButton_GroupIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.GroupIndex := PInteger(Params^[1])^;
end;

//Read: property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
procedure Lape_TCustomSpeedButton_Layout_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PButtonLayout(Result)^ := PCustomSpeedButton(Params^[0])^.Layout;
end;

//Write: property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
procedure Lape_TCustomSpeedButton_Layout_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Layout := PButtonLayout(Params^[1])^;
end;

//Read: property Margin: integer read FMargin write SetMargin default -1;
procedure Lape_TCustomSpeedButton_Margin_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpeedButton(Params^[0])^.Margin;
end;

//Write: property Margin: integer read FMargin write SetMargin default -1;
procedure Lape_TCustomSpeedButton_Margin_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Margin := Pinteger(Params^[1])^;
end;

//Read: property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs default 1;
procedure Lape_TCustomSpeedButton_NumGlyphs_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomSpeedButton(Params^[0])^.NumGlyphs;
end;

//Write: property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs default 1;
procedure Lape_TCustomSpeedButton_NumGlyphs_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.NumGlyphs := PInteger(Params^[1])^;
end;

//Read: property ShowAccelChar: boolean read FShowAccelChar write SetShowAccelChar default true;
procedure Lape_TCustomSpeedButton_ShowAccelChar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomSpeedButton(Params^[0])^.ShowAccelChar;
end;

//Write: property ShowAccelChar: boolean read FShowAccelChar write SetShowAccelChar default true;
procedure Lape_TCustomSpeedButton_ShowAccelChar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.ShowAccelChar := Pboolean(Params^[1])^;
end;

//Read: property ShowCaption: boolean read FShowCaption write SetShowCaption default true;
procedure Lape_TCustomSpeedButton_ShowCaption_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomSpeedButton(Params^[0])^.ShowCaption;
end;

//Write: property ShowCaption: boolean read FShowCaption write SetShowCaption default true;
procedure Lape_TCustomSpeedButton_ShowCaption_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.ShowCaption := Pboolean(Params^[1])^;
end;

//Read: property Spacing: integer read FSpacing write SetSpacing default 4;
procedure Lape_TCustomSpeedButton_Spacing_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpeedButton(Params^[0])^.Spacing;
end;

//Write: property Spacing: integer read FSpacing write SetSpacing default 4;
procedure Lape_TCustomSpeedButton_Spacing_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Spacing := Pinteger(Params^[1])^;
end;

//Read: property Transparent: Boolean read GetTransparent write SetTransparent default true;
procedure Lape_TCustomSpeedButton_Transparent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.Transparent;
end;

//Write: property Transparent: Boolean read GetTransparent write SetTransparent default true;
procedure Lape_TCustomSpeedButton_Transparent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

//constructor Create();
procedure Lape_TCustomSpeedButton_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^ := TCustomSpeedButton.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TCustomSpeedButton_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Free();
end;

procedure Lape_Import_TCustomSpeedButton(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCustomSpeedButton', 'TGraphicControl');

    addGlobalFunc('function TCustomSpeedButton.FindDownButton(): TCustomSpeedButton; constref;', @Lape_TCustomSpeedButton_FindDownButton);
    addGlobalFunc('procedure TCustomSpeedButton.LoadGlyphFromLazarusResource(const AName: String); constref;', @Lape_TCustomSpeedButton_LoadGlyphFromLazarusResource);
    addClassVar('TCustomSpeedButton', 'AllowAllUp', 'Boolean', @Lape_TCustomSpeedButton_AllowAllUp_Read, @Lape_TCustomSpeedButton_AllowAllUp_Write);
    addClassVar('TCustomSpeedButton', 'Down', 'Boolean', @Lape_TCustomSpeedButton_Down_Read, @Lape_TCustomSpeedButton_Down_Write);
    addClassVar('TCustomSpeedButton', 'Flat', 'Boolean', @Lape_TCustomSpeedButton_Flat_Read, @Lape_TCustomSpeedButton_Flat_Write);
    addClassVar('TCustomSpeedButton', 'Glyph', 'TBitmap', @Lape_TCustomSpeedButton_Glyph_Read, @Lape_TCustomSpeedButton_Glyph_Write);
    addClassVar('TCustomSpeedButton', 'GroupIndex', 'Integer', @Lape_TCustomSpeedButton_GroupIndex_Read, @Lape_TCustomSpeedButton_GroupIndex_Write);
    addClassVar('TCustomSpeedButton', 'Layout', 'TButtonLayout', @Lape_TCustomSpeedButton_Layout_Read, @Lape_TCustomSpeedButton_Layout_Write);
    addClassVar('TCustomSpeedButton', 'Margin', 'integer', @Lape_TCustomSpeedButton_Margin_Read, @Lape_TCustomSpeedButton_Margin_Write);
    addClassVar('TCustomSpeedButton', 'NumGlyphs', 'Integer', @Lape_TCustomSpeedButton_NumGlyphs_Read, @Lape_TCustomSpeedButton_NumGlyphs_Write);
    addClassVar('TCustomSpeedButton', 'ShowAccelChar', 'boolean', @Lape_TCustomSpeedButton_ShowAccelChar_Read, @Lape_TCustomSpeedButton_ShowAccelChar_Write);
    addClassVar('TCustomSpeedButton', 'ShowCaption', 'boolean', @Lape_TCustomSpeedButton_ShowCaption_Read, @Lape_TCustomSpeedButton_ShowCaption_Write);
    addClassVar('TCustomSpeedButton', 'Spacing', 'integer', @Lape_TCustomSpeedButton_Spacing_Read, @Lape_TCustomSpeedButton_Spacing_Write);
    addClassVar('TCustomSpeedButton', 'Transparent', 'Boolean', @Lape_TCustomSpeedButton_Transparent_Read, @Lape_TCustomSpeedButton_Transparent_Write);
    addGlobalFunc('procedure TCustomSpeedButton.Init(AOwner: TComponent); override;', @Lape_TCustomSpeedButton_Init);
    //addGlobalFunc('procedure TCustomSpeedButton.Free(); constref;', @Lape_TCustomSpeedButton_Free);
  end;
end;

//constructor Create();
procedure Lape_TSpeedButton_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSpeedButton(Params^[0])^ := TSpeedButton.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TSpeedButton_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PSpeedButton(Params^[0])^.Free();
end;

procedure Lape_Import_TSpeedButton(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TSpeedButton', 'TCustomSpeedButton');

    addGlobalFunc('procedure TSpeedButton.Init(AOwner: TComponent); override;', @Lape_TSpeedButton_Init);
    //addGlobalFunc('procedure TSpeedButton.Free(); constref;', @Lape_TSpeedButton_Free);
  end;
end;

//constructor Create();
procedure Lape_TRadioButton_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRadioButton(Params^[0])^ := TRadioButton.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TRadioButton_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PRadioButton(Params^[0])^.Free();
end;

procedure Lape_Import_TRadioButton(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TRadioButton', 'TCustomCheckBox');

    addGlobalFunc('procedure TRadioButton.Init(AOwner: TComponent); override;', @Lape_TRadioButton_Init);
   // addGlobalFunc('procedure TRadioButton.Free(); constref;', @Lape_TRadioButton_Free);
  end;
end;

procedure Lape_Import_LCLStdCtrls(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
   begin
     addGlobalType('(ssNone, ssHorizontal, ssVertical, ssBoth,ssAutoHorizontal, ssAutoVertical, ssAutoBoth)','TScrollStyle');
     addGlobalType('(scLineUp,scLineDown, scPageUp,scPageDown,scPosition, scTrack,scTop,scBottom,scEndScroll)','TScrollCode');
     addGlobalType('procedure(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer) of object', 'TScrollEvent', FFI_DEFAULT_ABI);
     addGlobalType('(odSelected, odGrayed, odDisabled, odChecked, odFocused, odDefault, odHotLight, odInactive, odNoAccel, odNoFocusRect, odReserved1, odReserved2, odComboBoxEdit, odPainted)' , 'TOwnerDrawStateType');
     addGlobalType('set of TOwnerDrawStateType', 'TOwnerDrawState');
     addGlobalType('procedure(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState) of object', 'TDrawItemEvent', FFI_DEFAULT_ABI);
     addGlobalType('(csDropDown,csSimple,csDropDownList,csOwnerDrawFixed,csOwnerDrawVariable)','TComboBoxStyle');
     addGlobalType('(lbStandard, lbOwnerDrawFixed, lbOwnerDrawVariable, lbVirtual)', 'TListBoxStyle');
     addGlobalType('(sbsNone, sbsSingle, sbsSunken)','TStaticBorderStyle');
     addGlobalType('(taLeftJustify, taRightJustify, taCenter)','TAlignment');
     addGlobalType('(cbUnchecked, cbChecked, cbGrayed)','TCheckBoxState');
     addGlobalType('(blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom)', 'TButtonLayout');
   end;
  Lape_Import_TCustomScrollBar(Compiler);
  Lape_Import_TScrollBar(Compiler);
  Lape_Import_TCustomComboBox(Compiler);
  Lape_Import_TComboBox(Compiler);
  Lape_Import_TCustomListBox(Compiler);
  Lape_Import_TListBox(Compiler);
  Lape_Import_TCustomEdit(Compiler);
  Lape_Import_TEdit(Compiler);
  Lape_Import_TCustomGroupBox(Compiler);
  Lape_Import_TGroupBox(Compiler);
  Lape_Import_TMemoScrollbar(Compiler);
  Lape_Import_TCustomMemo(Compiler);
  Lape_Import_TMemo(Compiler);
  Lape_Import_TCustomStaticText(Compiler);
  Lape_Import_TStaticText(Compiler);
  Lape_Import_TButtonControl(Compiler);
  Lape_Import_TCustomButton(Compiler);
  Lape_Import_TButton(Compiler);
  Lape_Import_TCustomCheckBox(Compiler);
  Lape_Import_TCheckBox(Compiler);
  Lape_Import_TCustomLabel(Compiler);
  Lape_Import_TLabel(Compiler);
  Lape_Import_TCustomSpeedButton(Compiler);
  Lape_Import_TSpeedButton(Compiler);
  Lape_Import_TRadioButton(Compiler);
end;

end.
