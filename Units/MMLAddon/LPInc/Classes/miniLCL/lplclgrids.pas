unit lplclgrids;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpCompiler, lpTypes, lpClassHelper;

procedure RegisterLCLGrids(Compiler: TLapeCompiler);

implementation

uses
  MufasaTypes, lplclsystem, ValEdit, Grids, Graphics, controls;

type
  PValueListEditor = ^TValueListEditor;
  PCustomGrid = ^TCustomGrid;
  PSortOrder = ^TSortOrder;
  PRect = ^TRect;
  PGridZone = ^TGridZone;
  PWinControl = ^TWinControl;
  PColumnButtonStyle = ^TColumnButtonStyle;
  PShiftState = ^TShiftState;
  PGridCoord = ^TGridCoord;
  PCustomDrawGrid = ^TCustomDrawGrid;
  PGridDrawState = ^TGridDrawState;
  PGridOperationEvent = ^TGridOperationEvent;
  PGetEditEvent = ^TGetEditEvent;
  PSetEditEvent = ^TSetEditEvent;
  POnSelectCellEvent = ^TOnSelectCellEvent;
  PCustomStringGrid = ^TCustomStringGrid;
  PGridZoneSet = ^TGridZoneSet;
  PKeyOptions = ^TKeyOptions;
  PValueListStrings = ^TValueListStrings;
  PGetPickListEvent = ^TGetPickListEvent;
  POnValidateEvent = ^TOnValidateEvent;
  PStringGrid = ^TStringGrid;
  PGridOptions = ^TGridOptions;
  PGridColumns = ^TGridColumns;
  PGridColumn = ^TGridColumn;
  PCollectionItemClass = ^TCollectionItemClass;
  PAlignment = ^TAlignment;
  PFont = ^TFont;
  PTextLayout = ^TTextLayout;
  PPrefixOption = ^TPrefixOption;
  PGridColumnTitle = ^TGridColumnTitle;

//procedure FillTitleDefaultFont;
procedure TGridColumnTitle_FillTitleDefaultFont(const Params: PParamArray); lape_extdecl
begin
  PGridColumnTitle(Params^[0])^.FillTitleDefaultFont();
end;

//function IsDefault: boolean;
procedure TGridColumnTitle_IsDefault(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PGridColumnTitle(Params^[0])^.IsDefault();
end;

//Read: property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
procedure TGridColumnTitle_Alignment_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PAlignment(Result)^ := PGridColumnTitle(Params^[0])^.Alignment;
end;

//Write: property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
procedure TGridColumnTitle_Alignment_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumnTitle(Params^[0])^.Alignment := PAlignment(Params^[1])^;
end;

//Read: property Caption: TCaption read GetCaption write SetCaption stored IsCaptionStored;
procedure TGridColumnTitle_Caption_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PGridColumnTitle(Params^[0])^.Caption;
end;

//Write: property Caption: TCaption read GetCaption write SetCaption stored IsCaptionStored;
procedure TGridColumnTitle_Caption_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumnTitle(Params^[0])^.Caption := PlpString(Params^[1])^;
end;

//Read: property Color: TColor read GetColor write SetColor stored IsColorStored;
procedure TGridColumnTitle_Color_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PColor(Result)^ := PGridColumnTitle(Params^[0])^.Color;
end;

//Write: property Color: TColor read GetColor write SetColor stored IsColorStored;
procedure TGridColumnTitle_Color_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumnTitle(Params^[0])^.Color := PColor(Params^[1])^;
end;

//Read: property Font: TFont read GetFont write SetFont stored IsFontStored;
procedure TGridColumnTitle_Font_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PFont(Result)^ := PGridColumnTitle(Params^[0])^.Font;
end;

//Write: property Font: TFont read GetFont write SetFont stored IsFontStored;
procedure TGridColumnTitle_Font_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumnTitle(Params^[0])^.Font := PFont(Params^[1])^;
end;

//Read: property PrefixOption: TPrefixOption read FPrefixOption write SetPrefixOption default poNone;
procedure TGridColumnTitle_PrefixOption_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPrefixOption(Result)^ := PGridColumnTitle(Params^[0])^.PrefixOption;
end;

//Write: property PrefixOption: TPrefixOption read FPrefixOption write SetPrefixOption default poNone;
procedure TGridColumnTitle_PrefixOption_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumnTitle(Params^[0])^.PrefixOption := PPrefixOption(Params^[1])^;
end;

//procedure Free();
procedure TGridColumnTitle_Free(const Params: PParamArray); lape_extdecl
begin
  PGridColumnTitle(Params^[0])^.Free();
end;

//Read: property Layout: TTextLayout read GetLayout write SetLayout stored IsLayoutStored;
procedure TGridColumnTitle_Layout_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PTextLayout(Result)^ := PGridColumnTitle(Params^[0])^.Layout;
end;

//Write: property Layout: TTextLayout read GetLayout write SetLayout stored IsLayoutStored;
procedure TGridColumnTitle_Layout_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumnTitle(Params^[0])^.Layout := PTextLayout(Params^[1])^;
end;

procedure Register_TGridColumnTitle(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TGridColumnTitle', 'TPersistent');

    addGlobalFunc('procedure TGridColumnTitle.FillTitleDefaultFont();', @TGridColumnTitle_FillTitleDefaultFont);
    addGlobalFunc('function TGridColumnTitle.IsDefault(): boolean;', @TGridColumnTitle_IsDefault);
    addClassVar('TGridColumnTitle', 'Alignment', 'TAlignment', @TGridColumnTitle_Alignment_Read, @TGridColumnTitle_Alignment_Write);
    addClassVar('TGridColumnTitle', 'Caption', 'TCaption', @TGridColumnTitle_Caption_Read, @TGridColumnTitle_Caption_Write);
    addClassVar('TGridColumnTitle', 'Color', 'TColor', @TGridColumnTitle_Color_Read, @TGridColumnTitle_Color_Write);
    addClassVar('TGridColumnTitle', 'Font', 'TFont', @TGridColumnTitle_Font_Read, @TGridColumnTitle_Font_Write);
    addClassVar('TGridColumnTitle', 'Layout', 'TTextLayout', @TGridColumnTitle_Layout_Read, @TGridColumnTitle_Layout_Write);
    addClassVar('TGridColumnTitle', 'PrefixOption', 'TPrefixOption', @TGridColumnTitle_PrefixOption_Read, @TGridColumnTitle_PrefixOption_Write);
    addGlobalFunc('procedure TGridColumnTitle.Free();', @TGridColumnTitle_Free);
  end;
end;


//procedure FillDefaultFont;
procedure TGridColumn_FillDefaultFont(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.FillDefaultFont();
end;

//function  IsDefault: boolean; virtual;
procedure TGridColumn_IsDefault(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PGridColumn(Params^[0])^.IsDefault();
end;

//Read: property Grid: TCustomGrid read GetGrid;
procedure TGridColumn_Grid_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PObject(Result)^ := TObject(PGridColumn(Params^[0])^.Grid);
end;

//Read: property WidthChanged: boolean read FWidthChanged;
procedure TGridColumn_WidthChanged_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PGridColumn(Params^[0])^.WidthChanged;
end;

//Read: property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
procedure TGridColumn_Alignment_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PAlignment(Result)^ := PGridColumn(Params^[0])^.Alignment;
end;

//Write: property Alignment: TAlignment read GetAlignment write SetAlignment stored IsAlignmentStored;
procedure TGridColumn_Alignment_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.Alignment := PAlignment(Params^[1])^;
end;

//Read: property ButtonStyle: TColumnButtonStyle read FButtonStyle write SetButtonStyle default cbsAuto;
procedure TGridColumn_ButtonStyle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PColumnButtonStyle(Result)^ := PGridColumn(Params^[0])^.ButtonStyle;
end;

//Write: property ButtonStyle: TColumnButtonStyle read FButtonStyle write SetButtonStyle default cbsAuto;
procedure TGridColumn_ButtonStyle_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.ButtonStyle := PColumnButtonStyle(Params^[1])^;
end;

//Read: property Color: TColor read GetColor write SetColor stored IsColorStored;
procedure TGridColumn_Color_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PColor(Result)^ := PGridColumn(Params^[0])^.Color;
end;

//Write: property Color: TColor read GetColor write SetColor stored IsColorStored;
procedure TGridColumn_Color_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.Color := PColor(Params^[1])^;
end;

//Read: property DropDownRows: Longint read FDropDownRows write FDropDownRows default 7;
procedure TGridColumn_DropDownRows_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongint(Result)^ := PGridColumn(Params^[0])^.DropDownRows;
end;

//Write: property DropDownRows: Longint read FDropDownRows write FDropDownRows default 7;
procedure TGridColumn_DropDownRows_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.DropDownRows := PLongint(Params^[1])^;
end;

//Read: property Expanded: Boolean read GetExpanded write SetExpanded default True;
procedure TGridColumn_Expanded_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PGridColumn(Params^[0])^.Expanded;
end;

//Write: property Expanded: Boolean read GetExpanded write SetExpanded default True;
procedure TGridColumn_Expanded_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.Expanded := PBoolean(Params^[1])^;
end;

//Read: property Font: TFont read GetFont write SetFont stored IsFontStored;
procedure TGridColumn_Font_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PFont(Result)^ := PGridColumn(Params^[0])^.Font;
end;

//Write: property Font: TFont read GetFont write SetFont stored IsFontStored;
procedure TGridColumn_Font_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.Font := PFont(Params^[1])^;
end;

//Read: property Layout: TTextLayout read GetLayout write SetLayout stored IsLayoutStored;
procedure TGridColumn_Layout_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PTextLayout(Result)^ := PGridColumn(Params^[0])^.Layout;
end;

//Write: property Layout: TTextLayout read GetLayout write SetLayout stored IsLayoutStored;
procedure TGridColumn_Layout_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.Layout := PTextLayout(Params^[1])^;
end;

//Read: property MinSize: Integer read GetMinSize write SetMinSize stored IsMinSizeStored;
procedure TGridColumn_MinSize_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PGridColumn(Params^[0])^.MinSize;
end;

//Write: property MinSize: Integer read GetMinSize write SetMinSize stored IsMinSizeStored;
procedure TGridColumn_MinSize_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.MinSize := PInteger(Params^[1])^;
end;

//Read: property MaxSize: Integer read GetMaxSize write SetMaxSize stored isMaxSizeStored;
procedure TGridColumn_MaxSize_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PGridColumn(Params^[0])^.MaxSize;
end;

//Write: property MaxSize: Integer read GetMaxSize write SetMaxSize stored isMaxSizeStored;
procedure TGridColumn_MaxSize_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.MaxSize := PInteger(Params^[1])^;
end;

//Read: property PickList: TStrings read GetPickList write SetPickList;
procedure TGridColumn_PickList_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStrings(Result)^ := PGridColumn(Params^[0])^.PickList;
end;

//Write: property PickList: TStrings read GetPickList write SetPickList;
procedure TGridColumn_PickList_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.PickList := PStrings(Params^[1])^;
end;

//Read: property ReadOnly: Boolean read GetReadOnly write SetReadOnly stored IsReadOnlyStored;
procedure TGridColumn_ReadOnly_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PGridColumn(Params^[0])^.ReadOnly;
end;

//Write: property ReadOnly: Boolean read GetReadOnly write SetReadOnly stored IsReadOnlyStored;
procedure TGridColumn_ReadOnly_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.ReadOnly := PBoolean(Params^[1])^;
end;

//Read: property SizePriority: Integer read GetSizePriority write SetSizePriority stored IsSizePriorityStored default 1;
procedure TGridColumn_SizePriority_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PGridColumn(Params^[0])^.SizePriority;
end;

//Write: property SizePriority: Integer read GetSizePriority write SetSizePriority stored IsSizePriorityStored default 1;
procedure TGridColumn_SizePriority_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.SizePriority := PInteger(Params^[1])^;
end;

//Read: property Tag: Integer read FTag write FTag default 0;
procedure TGridColumn_Tag_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PGridColumn(Params^[0])^.Tag;
end;

//Write: property Tag: Integer read FTag write FTag default 0;
procedure TGridColumn_Tag_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.Tag := PInteger(Params^[1])^;
end;

//Read: property Title: TGridColumnTitle read FTitle write SetTitle;
procedure TGridColumn_Title_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PGridColumnTitle(Result)^ := PGridColumn(Params^[0])^.Title
end;

//Write: property Title: TGridColumnTitle read FTitle write SetTitle;
procedure TGridColumn_Title_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.Title := PGridColumnTitle(Params^[1])^;
end;

//Read: property Width: Integer read GetWidth write SetWidth stored IsWidthStored default DEFCOLWIDTH;
procedure TGridColumn_Width_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PGridColumn(Params^[0])^.Width;
end;

//Write: property Width: Integer read GetWidth write SetWidth stored IsWidthStored default DEFCOLWIDTH;
procedure TGridColumn_Width_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.Width := PInteger(Params^[1])^;
end;

//Read: property Visible: Boolean read GetVisible write SetVisible stored IsVisibleStored default true;
procedure TGridColumn_Visible_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PGridColumn(Params^[0])^.Visible;
end;

//Write: property Visible: Boolean read GetVisible write SetVisible stored IsVisibleStored default true;
procedure TGridColumn_Visible_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.Visible := PBoolean(Params^[1])^;
end;

//constructor Create();
procedure TGridColumn_Init(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^ := TGridColumn.Create(PCollection(Params^[1])^);
end;

//procedure Free();
procedure TGridColumn_Free(const Params: PParamArray); lape_extdecl
begin
  PGridColumn(Params^[0])^.Free();
end;

procedure Register_TGridColumn(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TGridColumn', 'TCollectionItem');

    addGlobalFunc('procedure TGridColumn.FillDefaultFont();', @TGridColumn_FillDefaultFont);
    addGlobalFunc('function TGridColumn.IsDefault(): boolean;', @TGridColumn_IsDefault);
    addClassVar('TGridColumn', 'Grid', 'TObject', @TGridColumn_Grid_Read, nil);
    addClassVar('TGridColumn', 'WidthChanged', 'boolean', @TGridColumn_WidthChanged_Read, nil);
    addClassVar('TGridColumn', 'Alignment', 'TAlignment', @TGridColumn_Alignment_Read, @TGridColumn_Alignment_Write);
    addClassVar('TGridColumn', 'ButtonStyle', 'TColumnButtonStyle', @TGridColumn_ButtonStyle_Read, @TGridColumn_ButtonStyle_Write);
    addClassVar('TGridColumn', 'Color', 'TColor', @TGridColumn_Color_Read, @TGridColumn_Color_Write);
    addClassVar('TGridColumn', 'DropDownRows', 'Longint', @TGridColumn_DropDownRows_Read, @TGridColumn_DropDownRows_Write);
    addClassVar('TGridColumn', 'Expanded', 'Boolean', @TGridColumn_Expanded_Read, @TGridColumn_Expanded_Write);
    addClassVar('TGridColumn', 'Font', 'TFont', @TGridColumn_Font_Read, @TGridColumn_Font_Write);
    addClassVar('TGridColumn', 'Layout', 'TTextLayout', @TGridColumn_Layout_Read, @TGridColumn_Layout_Write);
    addClassVar('TGridColumn', 'MinSize', 'Integer', @TGridColumn_MinSize_Read, @TGridColumn_MinSize_Write);
    addClassVar('TGridColumn', 'MaxSize', 'Integer', @TGridColumn_MaxSize_Read, @TGridColumn_MaxSize_Write);
    addClassVar('TGridColumn', 'PickList', 'TStrings', @TGridColumn_PickList_Read, @TGridColumn_PickList_Write);
    addClassVar('TGridColumn', 'ReadOnly', 'Boolean', @TGridColumn_ReadOnly_Read, @TGridColumn_ReadOnly_Write);
    addClassVar('TGridColumn', 'SizePriority', 'Integer', @TGridColumn_SizePriority_Read, @TGridColumn_SizePriority_Write);
    addClassVar('TGridColumn', 'Tag', 'Integer', @TGridColumn_Tag_Read, @TGridColumn_Tag_Write);
    addClassVar('TGridColumn', 'Title', 'TGridColumnTitle', @TGridColumn_Title_Read, @TGridColumn_Title_Write);
    addClassVar('TGridColumn', 'Width', 'Integer', @TGridColumn_Width_Read, @TGridColumn_Width_Write);
    addClassVar('TGridColumn', 'Visible', 'Boolean', @TGridColumn_Visible_Read, @TGridColumn_Visible_Write);
    addGlobalFunc('procedure TGridColumn.Init(ACollection: TCollection);', @TGridColumn_Init);
    addGlobalFunc('procedure TGridColumn.Free();', @TGridColumn_Free);
  end;
end;

//function Add: TGridColumn;
procedure TGridColumns_Add(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PGridColumn(Result)^ := PGridColumns(Params^[0])^.Add();
end;

//procedure Clear;
procedure TGridColumns_Clear(const Params: PParamArray); lape_extdecl
begin
  PGridColumns(Params^[0])^.Clear();
end;

//function RealIndex(Index: Integer): Integer;
procedure TGridColumns_RealIndex(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PGridColumns(Params^[0])^.RealIndex(PInteger(Params^[1])^);
end;

//function IndexOf(Column: TGridColumn): Integer;
procedure TGridColumns_IndexOf(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PGridColumns(Params^[0])^.IndexOf(PGridColumn(Params^[1])^);
end;

//function IsDefault: boolean;
procedure TGridColumns_IsDefault(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PGridColumns(Params^[0])^.IsDefault();
end;

//function HasIndex(Index: Integer): boolean;
procedure TGridColumns_HasIndex(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PGridColumns(Params^[0])^.HasIndex(PInteger(Params^[1])^);
end;

//function VisibleIndex(Index: Integer): Integer;
procedure TGridColumns_VisibleIndex(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PGridColumns(Params^[0])^.VisibleIndex(PInteger(Params^[1])^);
end;

//Read: property Grid: TCustomGrid read FGrid;
procedure TGridColumns_Grid_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PObject(Result)^ := TObject(PGridColumns(Params^[0])^.Grid);
end;

//Read: property Items[Index: Integer]: TGridColumn read GetColumn write SetColumn; default;
procedure TGridColumns_Items_Index_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PGridColumn(Result)^ := PGridColumns(Params^[0])^.Items[PInteger(Params^[1])^];
end;

//Write: property Items[Index: Integer]: TGridColumn read GetColumn write SetColumn; default;
procedure TGridColumns_Items_Index_Write(const Params: PParamArray); lape_extdecl
begin
  PGridColumns(Params^[0])^.Items[PInteger(Params^[1])^] := PGridColumn(Params^[2])^;
end;

//Read: property VisibleCount: Integer read GetVisibleCount;
procedure TGridColumns_VisibleCount_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PGridColumns(Params^[0])^.VisibleCount;
end;

//Read: property Enabled: Boolean read GetEnabled;
procedure TGridColumns_Enabled_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PGridColumns(Params^[0])^.Enabled;
end;

procedure Register_TGridColumns(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TGridColumns', 'TCollection');

    addGlobalFunc('function TGridColumns.Add(): TGridColumn;', @TGridColumns_Add);
    addGlobalFunc('procedure TGridColumns.Clear();', @TGridColumns_Clear);
    addGlobalFunc('function TGridColumns.RealIndex(Index: Integer): Integer;', @TGridColumns_RealIndex);
    addGlobalFunc('function TGridColumns.IndexOf(Column: TGridColumn): Integer;', @TGridColumns_IndexOf);
    addGlobalFunc('function TGridColumns.IsDefault(): boolean;', @TGridColumns_IsDefault);
    addGlobalFunc('function TGridColumns.HasIndex(Index: Integer): boolean;', @TGridColumns_HasIndex);
    addGlobalFunc('function TGridColumns.VisibleIndex(Index: Integer): Integer;', @TGridColumns_VisibleIndex);
    addClassVar('TGridColumns', 'Grid', 'TObject', @TGridColumns_Grid_Read, nil);
    addClassVar('TGridColumns', 'Items', 'TGridColumn', @TGridColumns_Items_Index_Read, @TGridColumns_Items_Index_Write, True);
    addClassVar('TGridColumns', 'VisibleCount', 'Integer', @TGridColumns_VisibleCount_Read, nil);
    addClassVar('TGridColumns', 'Enabled', 'Boolean', @TGridColumns_Enabled_Read, nil);
  end;
end;

//procedure Invalidate; override;
procedure TCustomGrid_Invalidate(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.Invalidate();
end;

//procedure EditingDone; override;
procedure TCustomGrid_EditingDone(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.EditingDone();
end;

//procedure AutoAdjustColumns;
procedure TCustomGrid_AutoAdjustColumns(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.AutoAdjustColumns();
end;

//procedure BeginUpdate;
procedure TCustomGrid_BeginUpdate(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.BeginUpdate();
end;

//function  CellRect(ACol, ARow: Integer): TRect;
procedure TCustomGrid_CellRect(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PRect(Result)^ := PCustomGrid(Params^[0])^.CellRect(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//function  CellToGridZone(aCol,aRow: Integer): TGridZone;
procedure TCustomGrid_CellToGridZone(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PGridZone(Result)^ := PCustomGrid(Params^[0])^.CellToGridZone(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure CheckPosition;
procedure TCustomGrid_CheckPosition(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.CheckPosition();
end;

//procedure Clear;
procedure TCustomGrid_Clear(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.Clear();
end;

//function  EditorByStyle(Style: TColumnButtonStyle): TWinControl; virtual;
procedure TCustomGrid_EditorByStyle(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PWinControl(Result)^ := PCustomGrid(Params^[0])^.EditorByStyle(PColumnButtonStyle(Params^[1])^);
end;

//procedure EditorKeyDown(Sender: TObject; var Key:Word; Shift:TShiftState);
procedure TCustomGrid_EditorKeyDown(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.EditorKeyDown(PObject(Params^[1])^, PWord(Params^[2])^, PShiftState(Params^[3])^);
end;

//procedure EditorKeyPress(Sender: TObject; var Key: Char);
procedure TCustomGrid_EditorKeyPress(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.EditorKeyPress(PObject(Params^[1])^, PChar(Params^[2])^);
end;

//procedure EditorKeyUp(Sender: TObject; var key:Word; shift:TShiftState);
procedure TCustomGrid_EditorKeyUp(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.EditorKeyUp(PObject(Params^[1])^, PWord(Params^[2])^, PShiftState(Params^[3])^);
end;

//procedure EndUpdate(aRefresh: boolean = true);
procedure TCustomGrid_EndUpdate(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.EndUpdate(Pboolean(Params^[1])^);
end;

//function  Focused: Boolean; override;
procedure TCustomGrid_Focused(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomGrid(Params^[0])^.Focused();
end;

//procedure InvalidateCell(aCol, aRow: Integer); overload;
procedure TCustomGrid_InvalidateCell(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.InvalidateCell(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure InvalidateCol(ACol: Integer);
procedure TCustomGrid_InvalidateCol(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.InvalidateCol(PInteger(Params^[1])^);
end;

//procedure InvalidateRange(const aRange: TRect);
procedure TCustomGrid_InvalidateRange(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.InvalidateRange(PRect(Params^[1])^);
end;

//procedure InvalidateRow(ARow: Integer);
procedure TCustomGrid_InvalidateRow(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.InvalidateRow(PInteger(Params^[1])^);
end;

//function  IsCellVisible(aCol, aRow: Integer): Boolean;
procedure TCustomGrid_IsCellVisible(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomGrid(Params^[0])^.IsCellVisible(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//function  IsFixedCellVisible(aCol, aRow: Integer): boolean;
procedure TCustomGrid_IsFixedCellVisible(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCustomGrid(Params^[0])^.IsFixedCellVisible(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure LoadFromFile(FileName: string);
procedure TCustomGrid_LoadFromFile(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.LoadFromFile(PlpString(Params^[1])^);
end;

//procedure LoadFromStream(AStream: TStream);
procedure TCustomGrid_LoadFromStream(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.LoadFromStream(PStream(Params^[1])^);
end;

//function  MouseCoord(X,Y: Integer): TGridCoord;
procedure TCustomGrid_MouseCoord(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PGridCoord(Result)^ := PCustomGrid(Params^[0])^.MouseCoord(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//function  MouseToCell(const Mouse: TPoint): TPoint; overload;
procedure TCustomGrid_MouseToCell(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPoint(Result)^ := PCustomGrid(Params^[0])^.MouseToCell(PPoint(Params^[1])^);
end;

//procedure MouseToCell(X,Y: Integer; var ACol,ARow: Longint); overload;
procedure TCustomGrid_MouseToCellEx(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.MouseToCell(PInteger(Params^[1])^, PInteger(Params^[2])^, PLongint(Params^[3])^, PLongint(Params^[4])^);
end;

//function  MouseToLogcell(Mouse: TPoint): TPoint;
procedure TCustomGrid_MouseToLogcell(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PPoint(Result)^ := PCustomGrid(Params^[0])^.MouseToLogcell(PPoint(Params^[1])^);
end;

//function  MouseToGridZone(X,Y: Integer): TGridZone;
procedure TCustomGrid_MouseToGridZone(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PGridZone(Result)^ := PCustomGrid(Params^[0])^.MouseToGridZone(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure SaveToFile(FileName: string);
procedure TCustomGrid_SaveToFile(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.SaveToFile(PlpString(Params^[1])^);
end;

//procedure SaveToStream(AStream: TStream);
procedure TCustomGrid_SaveToStream(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.SaveToStream(PStream(Params^[1])^);
end;

//procedure SetFocus; override;
procedure TCustomGrid_SetFocus(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.SetFocus();
end;

//Read: property SortOrder: TSortOrder read FSortOrder write FSortOrder;
procedure TCustomGrid_SortOrder_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PSortOrder(Result)^ := PCustomGrid(Params^[0])^.SortOrder;
end;

//Write: property SortOrder: TSortOrder read FSortOrder write FSortOrder;
procedure TCustomGrid_SortOrder_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.SortOrder := PSortOrder(Params^[1])^;
end;

//constructor Create();
procedure TCustomGrid_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^ := TCustomGrid.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TCustomGrid_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomGrid(Params^[0])^.Free();
end;

procedure Register_TCustomGrid(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomGrid', 'TCustomControl');

    addGlobalFunc('procedure TCustomGrid.Invalidate();', @TCustomGrid_Invalidate);
    addGlobalFunc('procedure TCustomGrid.EditingDone();', @TCustomGrid_EditingDone);
    addGlobalFunc('procedure TCustomGrid.AutoAdjustColumns();', @TCustomGrid_AutoAdjustColumns);
    addGlobalFunc('procedure TCustomGrid.BeginUpdate();', @TCustomGrid_BeginUpdate);
    addGlobalFunc('function TCustomGrid.CellRect(ACol, ARow: Integer): TRect;', @TCustomGrid_CellRect);
    addGlobalFunc('function TCustomGrid.CellToGridZone(aCol,aRow: Integer): TGridZone;', @TCustomGrid_CellToGridZone);
    addGlobalFunc('procedure TCustomGrid.CheckPosition();', @TCustomGrid_CheckPosition);
    addGlobalFunc('procedure TCustomGrid.Clear();', @TCustomGrid_Clear);
    addGlobalFunc('function TCustomGrid.EditorByStyle(Style: TColumnButtonStyle): TWinControl;', @TCustomGrid_EditorByStyle);
    addGlobalFunc('procedure TCustomGrid.EditorKeyDown(Sender: TObject; var Key:Word; Shift:TShiftState);', @TCustomGrid_EditorKeyDown);
    addGlobalFunc('procedure TCustomGrid.EditorKeyPress(Sender: TObject; var Key: Char);', @TCustomGrid_EditorKeyPress);
    addGlobalFunc('procedure TCustomGrid.EditorKeyUp(Sender: TObject; var key:Word; shift:TShiftState);', @TCustomGrid_EditorKeyUp);
    addGlobalFunc('procedure TCustomGrid.EndUpdate(aRefresh: boolean = true);', @TCustomGrid_EndUpdate);
    addGlobalFunc('function TCustomGrid.Focused(): Boolean;', @TCustomGrid_Focused);
    addGlobalFunc('procedure TCustomGrid.InvalidateCell(aCol, aRow: Integer);', @TCustomGrid_InvalidateCell);
    addGlobalFunc('procedure TCustomGrid.InvalidateCol(ACol: Integer);', @TCustomGrid_InvalidateCol);
    addGlobalFunc('procedure TCustomGrid.InvalidateRange(const aRange: TRect);', @TCustomGrid_InvalidateRange);
    addGlobalFunc('procedure TCustomGrid.InvalidateRow(ARow: Integer);', @TCustomGrid_InvalidateRow);
    addGlobalFunc('function TCustomGrid.IsCellVisible(aCol, aRow: Integer): Boolean;', @TCustomGrid_IsCellVisible);
    addGlobalFunc('function TCustomGrid.IsFixedCellVisible(aCol, aRow: Integer): boolean;', @TCustomGrid_IsFixedCellVisible);
    addGlobalFunc('procedure TCustomGrid.LoadFromFile(FileName: string);', @TCustomGrid_LoadFromFile);
    addGlobalFunc('procedure TCustomGrid.LoadFromStream(AStream: TStream);', @TCustomGrid_LoadFromStream);
    addGlobalFunc('function TCustomGrid.MouseCoord(X,Y: Integer): TGridCoord;', @TCustomGrid_MouseCoord);
    addGlobalFunc('function TCustomGrid.MouseToCell(const Mouse: TPoint): TPoint;', @TCustomGrid_MouseToCell);
    addGlobalFunc('procedure TCustomGrid.MouseToCell(X,Y: Integer; var ACol,ARow: Longint); overload;', @TCustomGrid_MouseToCellEx);
    addGlobalFunc('function TCustomGrid.MouseToLogcell(Mouse: TPoint): TPoint;', @TCustomGrid_MouseToLogcell);
    addGlobalFunc('function TCustomGrid.MouseToGridZone(X,Y: Integer): TGridZone;', @TCustomGrid_MouseToGridZone);
    addGlobalFunc('procedure TCustomGrid.SaveToFile(FileName: string);', @TCustomGrid_SaveToFile);
    addGlobalFunc('procedure TCustomGrid.SaveToStream(AStream: TStream);', @TCustomGrid_SaveToStream);
    addGlobalFunc('procedure TCustomGrid.SetFocus;', @TCustomGrid_SetFocus);
    addClassVar('TCustomGrid', 'SortOrder', 'TSortOrder', @TCustomGrid_SortOrder_Read, @TCustomGrid_SortOrder_Write);
    addGlobalFunc('procedure TCustomGrid.Init(TheOwner: TComponent);', @TCustomGrid_Init);
    addGlobalFunc('procedure TCustomGrid.Free();', @TCustomGrid_Free);
  end;
end;

procedure TCustomDrawGrid_Create(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^ := TCustomDrawGrid.Create(PComponent(Params^[1])^);
end;

//procedure DeleteColRow(IsColumn: Boolean; index: Integer);
procedure TCustomDrawGrid_DeleteColRow(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.DeleteColRow(PBoolean(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure DeleteCol(Index: Integer); virtual;
procedure TCustomDrawGrid_DeleteCol(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.DeleteCol(PInteger(Params^[1])^);
end;

//procedure DeleteRow(Index: Integer); virtual;
procedure TCustomDrawGrid_DeleteRow(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.DeleteRow(PInteger(Params^[1])^);
end;

//procedure ExchangeColRow(IsColumn: Boolean; index, WithIndex: Integer);
procedure TCustomDrawGrid_ExchangeColRow(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.ExchangeColRow(PBoolean(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

//procedure InsertColRow(IsColumn: boolean; index: integer);
procedure TCustomDrawGrid_InsertColRow(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.InsertColRow(Pboolean(Params^[1])^, Pinteger(Params^[2])^);
end;

//procedure MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer);
procedure TCustomDrawGrid_MoveColRow(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.MoveColRow(PBoolean(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

//procedure SortColRow(IsColumn: Boolean; index:Integer); overload;
procedure TCustomDrawGrid_SortColRow(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.SortColRow(PBoolean(Params^[1])^, PInteger(Params^[2])^);
end;

//procedure SortColRow(IsColumn: Boolean; Index,FromIndex,ToIndex: Integer); overload;
procedure TCustomDrawGrid_SortColRowEx(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.SortColRow(PBoolean(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

//procedure DefaultDrawCell(aCol,aRow: Integer; var aRect: TRect; aState:TGridDrawState); virtual;
procedure TCustomDrawGrid_DefaultDrawCell(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.DefaultDrawCell(PInteger(Params^[1])^, PInteger(Params^[2])^, PRect(Params^[3])^, PGridDrawState(Params^[4])^);
end;

//Read: property OnColRowDeleted: TgridOperationEvent read FOnColRowDeleted write FOnColRowDeleted;
procedure TCustomDrawGrid_OnColRowDeleted_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PgridOperationEvent(Result)^ := PCustomDrawGrid(Params^[0])^.OnColRowDeleted;
end;

//Write: property OnColRowDeleted: TgridOperationEvent read FOnColRowDeleted write FOnColRowDeleted;
procedure TCustomDrawGrid_OnColRowDeleted_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.OnColRowDeleted := PgridOperationEvent(Params^[1])^;
end;

//Read: property OnColRowExchanged: TgridOperationEvent read FOnColRowExchanged write FOnColRowExchanged;
procedure TCustomDrawGrid_OnColRowExchanged_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PgridOperationEvent(Result)^ := PCustomDrawGrid(Params^[0])^.OnColRowExchanged;
end;

//Write: property OnColRowExchanged: TgridOperationEvent read FOnColRowExchanged write FOnColRowExchanged;
procedure TCustomDrawGrid_OnColRowExchanged_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.OnColRowExchanged := PgridOperationEvent(Params^[1])^;
end;

//Read: property OnColRowInserted: TGridOperationEvent read FOnColRowInserted write FOnColRowInserted;
procedure TCustomDrawGrid_OnColRowInserted_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PGridOperationEvent(Result)^ := PCustomDrawGrid(Params^[0])^.OnColRowInserted;
end;

//Write: property OnColRowInserted: TGridOperationEvent read FOnColRowInserted write FOnColRowInserted;
procedure TCustomDrawGrid_OnColRowInserted_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.OnColRowInserted := PGridOperationEvent(Params^[1])^;
end;

//Read: property OnColRowMoved: TgridOperationEvent read FOnColRowMoved write FOnColRowMoved;
procedure TCustomDrawGrid_OnColRowMoved_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PgridOperationEvent(Result)^ := PCustomDrawGrid(Params^[0])^.OnColRowMoved;
end;

//Write: property OnColRowMoved: TgridOperationEvent read FOnColRowMoved write FOnColRowMoved;
procedure TCustomDrawGrid_OnColRowMoved_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.OnColRowMoved := PgridOperationEvent(Params^[1])^;
end;

//Read: property OnGetEditMask: TGetEditEvent read FOnGetEditMask write FOnGetEditMask;
procedure TCustomDrawGrid_OnGetEditMask_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PGetEditEvent(Result)^ := PCustomDrawGrid(Params^[0])^.OnGetEditMask;
end;

//Write: property OnGetEditMask: TGetEditEvent read FOnGetEditMask write FOnGetEditMask;
procedure TCustomDrawGrid_OnGetEditMask_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.OnGetEditMask := PGetEditEvent(Params^[1])^;
end;

//Read: property OnGetEditText: TGetEditEvent read FOnGetEditText write FOnGetEditText;
procedure TCustomDrawGrid_OnGetEditText_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PGetEditEvent(Result)^ := PCustomDrawGrid(Params^[0])^.OnGetEditText;
end;

//Write: property OnGetEditText: TGetEditEvent read FOnGetEditText write FOnGetEditText;
procedure TCustomDrawGrid_OnGetEditText_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.OnGetEditText := PGetEditEvent(Params^[1])^;
end;

//Read: property OnSelectCell: TOnSelectCellEvent read FOnSelectCell write FOnSelectCell;
procedure TCustomDrawGrid_OnSelectCell_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  POnSelectCellEvent(Result)^ := PCustomDrawGrid(Params^[0])^.OnSelectCell;
end;

//Write: property OnSelectCell: TOnSelectCellEvent read FOnSelectCell write FOnSelectCell;
procedure TCustomDrawGrid_OnSelectCell_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.OnSelectCell := POnSelectCellEvent(Params^[1])^;
end;

//Read: property OnSetEditText: TSetEditEvent read FOnSetEditText write FOnSetEditText;
procedure TCustomDrawGrid_OnSetEditText_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PSetEditEvent(Result)^ := PCustomDrawGrid(Params^[0])^.OnSetEditText;
end;

//Write: property OnSetEditText: TSetEditEvent read FOnSetEditText write FOnSetEditText;
procedure TCustomDrawGrid_OnSetEditText_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.OnSetEditText := PSetEditEvent(Params^[1])^;
end;

//constructor Create();
procedure TCustomDrawGrid_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^ := TCustomDrawGrid.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TCustomDrawGrid_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.Free();
end;

procedure TCustomDrawGrid_RowCount_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomDrawGrid(Params^[0])^.RowCount;
end;

procedure TCustomDrawGrid_RowCount_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.RowCount := PInteger(Params^[1])^;
end;

procedure TCustomDrawGrid_ColCount_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomDrawGrid(Params^[0])^.ColCount;
end;

procedure TCustomDrawGrid_ColCount_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.ColCount := PInteger(Params^[1])^;
end;

procedure TCustomDrawGrid_Options_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PGridOptions(Result)^ := PCustomDrawGrid(Params^[0])^.Options;
end;

procedure TCustomDrawGrid_Options_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.Options := PGridOptions(Params^[1])^;
end;

procedure TCustomDrawGrid_ColWidths_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.ColWidths[PInteger(Params^[1])^] := PInteger(Params^[2])^;
end;

procedure TCustomDrawGrid_ColWidths_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomDrawGrid(Params^[0])^.ColWidths[PInteger(Params^[1])^];
end;

procedure TCustomDrawGrid_RowHeights_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.RowHeights[PInteger(Params^[1])^] := PInteger(Params^[2])^;
end;

procedure TCustomDrawGrid_RowHeights_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomDrawGrid(Params^[0])^.RowHeights[PInteger(Params^[1])^];
end;

procedure TCustomDrawGrid_FixedCols_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.FixedCols := PInteger(Params^[1])^;
end;

procedure TCustomDrawGrid_FixedCols_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomDrawGrid(Params^[0])^.FixedCols;
end;

procedure TCustomDrawGrid_FixedRows_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.FixedRows := PInteger(Params^[1])^;
end;

procedure TCustomDrawGrid_FixedRows_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomDrawGrid(Params^[0])^.FixedRows;
end;

procedure TCustomDrawGrid_DefaultColWidth_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.DefaultColWidth := PInteger(Params^[1])^;
end;

procedure TCustomDrawGrid_DefaultColWidth_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomDrawGrid(Params^[0])^.DefaultColWidth;
end;

procedure TCustomDrawGrid_DefaultRowHeight_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.DefaultRowHeight := PInteger(Params^[1])^;
end;

procedure TCustomDrawGrid_DefaultRowHeight_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomDrawGrid(Params^[0])^.DefaultColWidth;
end;

procedure TCustomDrawGrid_DefaultDrawing_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.DefaultDrawing := PBoolean(Params^[1])^;
end;

procedure TCustomDrawGrid_DefaultDrawing_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PCustomDrawGrid(Params^[0])^.DefaultDrawing;
end;

procedure TCustomDrawGrid_Row(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomDrawGrid(Params^[0])^.Row;
end;

procedure TCustomDrawGrid_Col(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PCustomDrawGrid(Params^[0])^.Col;
end;

procedure TCustomDrawGrid_OnDrawCell_Write(const Params: PParamArray); lape_extdecl
var
  Component: TComponent;
begin
  Component := PCustomDrawGrid(Params^[0])^.FindComponent('DrawCell');
  if (not Assigned(Component)) then
  begin
    Component := TOnDrawCellWrapper.Create(PCustomDrawGrid(Params^[0])^);
    Component.Name := 'DrawCell';
  end;

  with TOnDrawCellWrapper(Component) do
  begin
    InternalMethod := PDrawCellEventWrapper(Params^[1])^;
    PCustomDrawGrid(Params^[0])^.OnDrawCell := @DrawCell;
  end;
end;

procedure TCustomDrawGrid_Columns_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomDrawGrid(Params^[0])^.Columns := PGridColumns(Params^[1])^;
end;

procedure TCustomDrawGrid_Columns_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PGridColumns(Result)^ := PCustomDrawGrid(Params^[0])^.Columns;
end;

procedure Register_TCustomDrawGrid(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomDrawGrid', 'TCustomGrid');

    addGlobalFunc('procedure TCustomDrawGrid.DeleteColRow(IsColumn: Boolean; index: Integer);', @TCustomDrawGrid_DeleteColRow);
    addGlobalFunc('procedure TCustomDrawGrid.DeleteCol(Index: Integer);', @TCustomDrawGrid_DeleteCol);
    addGlobalFunc('procedure TCustomDrawGrid.DeleteRow(Index: Integer);', @TCustomDrawGrid_DeleteRow);
    addGlobalFunc('procedure TCustomDrawGrid.ExchangeColRow(IsColumn: Boolean; index, WithIndex: Integer);', @TCustomDrawGrid_ExchangeColRow);
    addGlobalFunc('procedure TCustomDrawGrid.InsertColRow(IsColumn: boolean; index: integer);', @TCustomDrawGrid_InsertColRow);
    addGlobalFunc('procedure TCustomDrawGrid.MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer);', @TCustomDrawGrid_MoveColRow);
    addGlobalFunc('procedure TCustomDrawGrid.SortColRow(IsColumn: Boolean; index:Integer);', @TCustomDrawGrid_SortColRow);
    addGlobalFunc('procedure TCustomDrawGrid.SortColRow(IsColumn: Boolean; Index,FromIndex,ToIndex: Integer); overload;', @TCustomDrawGrid_SortColRowEx);
    addGlobalFunc('procedure TCustomDrawGrid.DefaultDrawCell(aCol,aRow: Integer; var aRect: TRect; aState:TGridDrawState);', @TCustomDrawGrid_DefaultDrawCell);
    addGlobalFunc('function TCustomDrawGrid.Row(): Integer', @TCustomDrawGrid_Row);
    addGlobalFunc('function TCustomDrawGrid.Col(): Integer', @TCustomDrawGrid_Col);
    addClassVar('TCustomDrawGrid', 'OnColRowDeleted', 'TGridOperationEvent', @TCustomDrawGrid_OnColRowDeleted_Read, @TCustomDrawGrid_OnColRowDeleted_Write);
    addClassVar('TCustomDrawGrid', 'OnColRowExchanged', 'TGridOperationEvent', @TCustomDrawGrid_OnColRowExchanged_Read, @TCustomDrawGrid_OnColRowExchanged_Write);
    addClassVar('TCustomDrawGrid', 'OnColRowInserted', 'TGridOperationEvent', @TCustomDrawGrid_OnColRowInserted_Read, @TCustomDrawGrid_OnColRowInserted_Write);
    addClassVar('TCustomDrawGrid', 'OnColRowMoved', 'TgridOperationEvent', @TCustomDrawGrid_OnColRowMoved_Read, @TCustomDrawGrid_OnColRowMoved_Write);
    addClassVar('TCustomDrawGrid', 'OnGetEditMask', 'TGetEditEvent', @TCustomDrawGrid_OnGetEditMask_Read, @TCustomDrawGrid_OnGetEditMask_Write);
    addClassVar('TCustomDrawGrid', 'OnGetEditText', 'TGetEditEvent', @TCustomDrawGrid_OnGetEditText_Read, @TCustomDrawGrid_OnGetEditText_Write);
    addClassVar('TCustomDrawGrid', 'OnSelectCell', 'TOnSelectCellEvent', @TCustomDrawGrid_OnSelectCell_Read, @TCustomDrawGrid_OnSelectCell_Write);
    addClassVar('TCustomDrawGrid', 'OnSetEditText', 'TSetEditEvent', @TCustomDrawGrid_OnSetEditText_Read, @TCustomDrawGrid_OnSetEditText_Write);
    addClassVar('TCustomDrawGrid', 'RowCount', 'Integer', @TCustomDrawGrid_RowCount_Read, @TCustomDrawGrid_RowCount_Write);
    addClassVar('TCustomDrawGrid', 'ColCount', 'Integer', @TCustomDrawGrid_ColCount_Read, @TCustomDrawGrid_ColCount_Write);
    addClassVar('TCustomDrawGrid', 'Options', 'TGridOptions', @TCustomDrawGrid_Options_Read, @TCustomDrawGrid_Options_Write);
    addClassVar('TCustomDrawGrid', 'ColWidths', 'Integer', @TCustomDrawGrid_ColWidths_Read, @TCustomDrawGrid_ColWidths_Write, True);
    addClassVar('TCustomDrawGrid', 'RowHeights', 'Integer', @TCustomDrawGrid_RowHeights_Read, @TCustomDrawGrid_RowHeights_Write, True);
    addClassVar('TCustomDrawGrid', 'FixedRows', 'Integer', @TCustomDrawGrid_FixedRows_Read, @TCustomDrawGrid_FixedRows_Write);
    addClassVar('TCustomDrawGrid', 'FixedCols', 'Integer', @TCustomDrawGrid_FixedCols_Read, @TCustomDrawGrid_FixedCols_Write);
    addClassVar('TCustomDrawGrid', 'DefaultRowWidth', 'Integer', @TCustomDrawGrid_DefaultColWidth_Read, @TCustomDrawGrid_DefaultColWidth_Write);
    addClassVar('TCustomDrawGrid', 'DefaultRowHeight', 'Integer', @TCustomDrawGrid_DefaultRowHeight_Read, @TCustomDrawGrid_DefaultRowHeight_Write);
    addClassVar('TCustomDrawGrid', 'DefaultDrawing', 'Boolean', @TCustomDrawGrid_DefaultDrawing_Read, @TCustomDrawGrid_DefaultDrawing_Write);
    addClassVar('TCustomDrawGrid', 'OnDrawCell', 'TOnDrawCell', nil, @TCustomDrawGrid_OnDrawCell_Write);
    addClassVar('TCustomDrawGrid', 'Columns', 'TGridColumns', @TCustomDrawGrid_Columns_Read, @TCustomDrawGrid_Columns_Write);
    addGlobalFunc('procedure TCustomDrawGrid.Init(AOwner: TComponent);', @TCustomDrawGrid_Init);
    addGlobalFunc('procedure TCustomDrawGrid.Free();', @TCustomDrawGrid_Free);
  end;
end;

//procedure AutoSizeColumn(aCol: Integer);
procedure TCustomStringGrid_AutoSizeColumn(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.AutoSizeColumn(PInteger(Params^[1])^);
end;

//procedure AutoSizeColumns;
procedure TCustomStringGrid_AutoSizeColumns(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.AutoSizeColumns();
end;

//procedure Clean; overload;
procedure TCustomStringGrid_Clean(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.Clean();
end;

//procedure Clean(CleanOptions: TGridZoneSet); overload;
procedure TCustomStringGrid_CleanEx(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.Clean(PGridZoneSet(Params^[1])^);
end;

//procedure Clean(aRect: TRect; CleanOptions: TGridZoneSet); overload;
procedure TCustomStringGrid_CleanExEx(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.Clean(PRect(Params^[1])^, PGridZoneSet(Params^[2])^);
end;

//procedure Clean(StartCol,StartRow,EndCol,EndRow: integer; CleanOptions: TGridZoneSet); overload;
procedure TCustomStringGrid_CleanExExEx(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.Clean(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^, PGridZoneSet(Params^[5])^);
end;

//procedure CopyToClipboard(AUseSelection: boolean = false);
procedure TCustomStringGrid_CopyToClipboard(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.CopyToClipboard(Pboolean(Params^[1])^);
end;

//procedure InsertRowWithValues(Index: Integer; Values: array of String);
procedure TCustomStringGrid_InsertRowWithValues(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.InsertRowWithValues(PInteger(Params^[1])^, PStringArray(Params^[2])^);
end;

//procedure LoadFromCSVStream(AStream: TStream; ADelimiter: Char=','; WithHeader: boolean=true);
procedure TCustomStringGrid_LoadFromCSVStream(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.LoadFromCSVStream(PStream(Params^[1])^, PChar(Params^[2])^, Pboolean(Params^[3])^);
end;

//procedure LoadFromCSVFile(AFilename: string; ADelimiter: Char=','; WithHeader: boolean=true);
procedure TCustomStringGrid_LoadFromCSVFile(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.LoadFromCSVFile(PlpString(Params^[1])^, PChar(Params^[2])^, Pboolean(Params^[3])^);
end;

//procedure SaveToCSVFile(AFileName: string; ADelimiter: Char=','; WithHeader: boolean=true;
procedure TCustomStringGrid_SaveToCSVFile(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.SaveToCSVFile(PlpString(Params^[1])^, PChar(Params^[2])^, Pboolean(Params^[3])^);
end;

//Read: property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
procedure TCustomStringGrid_Cells_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PCustomStringGrid(Params^[0])^.Cells[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

//Write: property Cells[ACol, ARow: Integer]: string read GetCells write SetCells;
procedure TCustomStringGrid_Cells_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.Cells[PInteger(Params^[1])^, PInteger(Params^[2])^] := PlpString(Params^[3])^;
end;

//Read: property Cols[index: Integer]: TStrings read GetCols write SetCols;
procedure TCustomStringGrid_Cols_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStrings(Result)^ := PCustomStringGrid(Params^[0])^.Cols[PInteger(Params^[1])^];
end;

//Write: property Cols[index: Integer]: TStrings read GetCols write SetCols;
procedure TCustomStringGrid_Cols_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.Cols[PInteger(Params^[1])^] := PStrings(Params^[2])^;
end;

//Read: property Objects[ACol, ARow: Integer]: TObject read GetObjects write SetObjects;
procedure TCustomStringGrid_Objects_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PObject(Result)^ := PCustomStringGrid(Params^[0])^.Objects[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

//Write: property Objects[ACol, ARow: Integer]: TObject read GetObjects write SetObjects;
procedure TCustomStringGrid_Objects_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.Objects[PInteger(Params^[1])^, PInteger(Params^[2])^] := PObject(Params^[3])^;
end;

//Read: property Rows[index: Integer]: TStrings read GetRows write SetRows;
procedure TCustomStringGrid_Rows_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStrings(Result)^ := PCustomStringGrid(Params^[0])^.Rows[PInteger(Params^[1])^];
end;

//Write: property Rows[index: Integer]: TStrings read GetRows write SetRows;
procedure TCustomStringGrid_Rows_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.Rows[PInteger(Params^[1])^] := PStrings(Params^[2])^;
end;

//constructor Create();
procedure TCustomStringGrid_Init(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^ := TCustomStringGrid.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TCustomStringGrid_Free(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.Free();
end;

procedure TCustomStringGrid_Cell_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PCustomStringGrid(Params^[0])^.Cells[PInteger(Params^[1])^, PInteger(Params^[2])^];
end;

procedure TCustomStringGrid_Cell_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.Cells[PInteger(Params^[1])^, PInteger(Params^[2])^] := PlpString(Params^[3])^;
end;

procedure TCustomStringGrid_Object_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PObject(Result)^ := PCustomStringGrid(Params^[0])^.Objects[PInteger(Params^[1])^, PInteger(Params^[2])^]
end;

procedure TCustomStringGrid_Object_Write(const Params: PParamArray); lape_extdecl
begin
  PCustomStringGrid(Params^[0])^.Objects[PInteger(Params^[1])^, PInteger(Params^[2])^] := PObject(Params^[3])^;
end;

procedure Register_TCustomStringGrid(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCustomStringGrid', 'TCustomDrawGrid');

    addGlobalFunc('procedure TCustomStringGrid.AutoSizeColumn(aCol: Integer);', @TCustomStringGrid_AutoSizeColumn);
    addGlobalFunc('procedure TCustomStringGrid.AutoSizeColumns();', @TCustomStringGrid_AutoSizeColumns);
    addGlobalFunc('procedure TCustomStringGrid.Clean();', @TCustomStringGrid_Clean);
    addGlobalFunc('procedure TCustomStringGrid.Clean(CleanOptions: TGridZoneSet); overload;', @TCustomStringGrid_CleanEx);
    addGlobalFunc('procedure TCustomStringGrid.Clean(aRect: TRect; CleanOptions: TGridZoneSet); overload;', @TCustomStringGrid_CleanExEx);
    addGlobalFunc('procedure TCustomStringGrid.Clean(StartCol,StartRow,EndCol,EndRow: integer; CleanOptions: TGridZoneSet); overload;', @TCustomStringGrid_CleanExExEx);
    addGlobalFunc('procedure TCustomStringGrid.CopyToClipboard(AUseSelection: boolean = false);', @TCustomStringGrid_CopyToClipboard);
    addGlobalFunc('procedure TCustomStringGrid.InsertRowWithValues(Index: Integer; Values: array of String);', @TCustomStringGrid_InsertRowWithValues);
    addGlobalFunc('procedure TCustomStringGrid.LoadFromCSVFile(AFilename: string; ADelimiter: Char; WithHeader: boolean);', @TCustomStringGrid_LoadFromCSVFile);
    addGlobalFunc('procedure TCustomStringGrid.SaveToCSVFile(AFileName: string; ADelimiter: Char; WithHeader: boolean);', @TCustomStringGrid_SaveToCSVFile);
    addGlobalFunc('procedure TCustomStringGrid.setCell(ACol, ARow: Integer; Value: String);', @TCustomStringGrid_Cell_Write);
    addGlobalFunc('function TCustomStringGrid.getCell(ACol, ARow: Integer): String;', @TCustomStringGrid_Cell_Read);
    addGlobalFunc('procedure TCustomStringGrid.setObject(ACol, ARow: Integer; Value: TObject);', @TCustomStringGrid_Object_Write);
    addGlobalFunc('function TCustomStringGrid.getObject(ACol, ARow: Integer): TObject;', @TCustomStringGrid_Object_Read);
    addClassVar('TCustomStringGrid', 'Cols', 'TStrings', @TCustomStringGrid_Cols_Read, @TCustomStringGrid_Cols_Write, True);
    addClassVar('TCustomStringGrid', 'Rows', 'TStrings', @TCustomStringGrid_Rows_Read, @TCustomStringGrid_Rows_Write, True);
    addGlobalFunc('procedure TCustomStringGrid.Init(AOwner: TComponent);', @TCustomStringGrid_Init);
    addGlobalFunc('procedure TCustomStringGrid.Free();', @TCustomStringGrid_Free);
  end;
end;

//constructor Create();
procedure TStringGrid_Init(const Params: PParamArray); lape_extdecl
begin
  PStringGrid(Params^[0])^ := TStringGrid.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TStringGrid_Free(const Params: PParamArray); lape_extdecl
begin
  PStringGrid(Params^[0])^.Free();
end;

procedure Register_TStringGrid(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TStringGrid', 'TCustomStringGrid');

    addGlobalFunc('procedure TStringGrid.Init(TheOwner: TComponent);', @TStringGrid_Init);
    addGlobalFunc('procedure TStringGrid.Free();', @TStringGrid_Free);
  end;
end;

procedure RegisterLCLGrids(Compiler: TLapeCompiler);
begin
  with (Compiler) do
  begin
    addGlobalType('TPoint', 'TGridCoord');
    addGlobalType('TRect', 'TGridRect');
    addGlobalType('(gzNormal, gzFixedCols, gzFixedRows, gzFixedCells, gzInvalid)', 'TGridZone');
    addGlobalType('set of TGridZone', 'TGridZoneSet');
    addGlobalType('(gdSelected, gdFocused, gdFixed, gdHot, gdPushed, gdRowHighlight)', 'TGridZoneEnums');
    addGlobalType('set of TGridZoneEnums', 'TGridDrawState');
    addGlobalType('(gsNormal, gsSelecting, gsRowSizing, gsColSizing, gsRowMoving, gsColMoving, gsHeaderClicking, gsButtonColumnClicking)', 'TGridState');
    addGlobalType('(cbsAuto, cbsEllipsis, cbsNone, cbsPickList, cbsCheckboxColumn, cbsButton, cbsButtonColumn)', 'TColumnButtonStyle');
    addGlobalType('(soAscending, soDescending)', 'TSortOrder');
    addGlobalType('(keyEdit, keyAdd, keyDelete, keyUnique)', 'TKeyOption');
    addGlobalType('set of TKeyOption', 'TKeyOptions');
    addGlobalType('(esSimple, esEllipsis, esPickList)', 'TEditStyle');
    addGlobalType('(tlTop, tlCenter, tlBottom)', 'TTextLayout');
    addGlobalType('(poNone, poHeaderClick)', 'TPrefixOption');

    addGlobalType('(goFixedVertLine, goFixedHorzLine, goVertLine,  goHorzLine, goRangeSelect,goDrawFocusSelected, goRowSizing, goColSizing,goRowMoving, goColMoving, goEditing, goAutoAddRows, goTabs, goRowSelect, goAlwaysShowEditor, goThumbTracking, goColSpanning, goRelaxedRowSelect, goDblClickAutoSize, goSmoothScroll, goFixedRowNumbering, goScrollKeepVisible, goHeaderHotTracking, goHeaderPushedLook, goSelectionActive, goFixedColSizing, goDontScrollPartCell, goCellHints, goTruncCellHints, goCellEllipsis, goAutoAddRowsSkipContentCheck, goRowHighlight)', 'TGridOption');
    addGlobalType('set of TGridOption', 'TGridOptions');

    addGlobalType('procedure(Sender: TObject; ACol, ARow: Integer; var Value: string)', 'TGetEditEvent');
    addGlobalType('procedure(Sender: TObject; ACol, ARow: Integer; const Value: string)', 'TSetEditEvent');
    addGlobalType('procedure(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState)', 'TOnDrawCell');
    addGlobalType('procedure(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean)', 'TOnSelectCellEvent');
    addGlobalType('procedure(Sender: TObject; aCol, aRow: Integer)', 'TOnSelectEvent');
    addGlobalType('procedure(Sender: TObject; IsColumn: Boolean; sIndex, tIndex: Integer)', 'TGridOperationEvent');
    addGlobalType('procedure(Sender: TObject; const KeyName: String; Values: TStrings)', 'TGetPickListEvent');
    addGlobalType('procedure(Sender: TObject; ACol, ARow: Longint; const KeyName, KeyValue: string)', 'TOnValidateEvent');
  end;

  Register_TGridColumnTitle(Compiler);
  Register_TGridColumn(Compiler);
  Register_TGridColumns(Compiler);
  Register_TCustomGrid(Compiler);
  Register_TCustomDrawGrid(Compiler);
  Register_TCustomStringGrid(Compiler);
  Register_TStringGrid(Compiler);
end;

end.

