unit simba.import_lcl_stdctrls;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportLCLStdCtrls(Compiler: TSimbaScript_Compiler);

implementation

uses
  controls, extctrls, stdctrls, comctrls, forms, graphics, buttons, lptypes, ffi;

type
  PObject = ^TObject;
  PComponent = ^TComponent;
  PAlignment = ^TAlignment;
  PTextLayout = ^TTextLayout;
  PButton = ^TButton;
  PButtonControl = ^TButtonControl;
  PButtonLayout = ^TButtonLayout;
  PCheckBox = ^TCheckBox;
  PCheckBoxState = ^TCheckBoxState;
  PComboBox = ^TComboBox;
  PComboBoxStyle = ^TComboBoxStyle;
  PCustomButton = ^TCustomButton;
  PCustomCheckBox = ^TCustomCheckBox;
  PCustomComboBox = ^TCustomComboBox;
  PCustomEdit = ^TCustomEdit;
  PCustomGroupBox = ^TCustomGroupBox;
  PCustomLabel = ^TCustomLabel;
  PCustomListBox = ^TCustomListBox;
  PCustomMemo = ^TCustomMemo;
  PCustomSpeedButton = ^TCustomSpeedButton;
  PDrawItemEvent = ^TDrawItemEvent;
  PMeasureItemEvent = ^TMeasureItemEvent;
  PSelectionChangeEvent = ^TSelectionChangeEvent;
  PEdit = ^TEdit;
  PGroupBox = ^TGroupBox;
  PLabel = ^TLabel;
  PListBox = ^TCustomListBox;
  PListBoxStyle = ^TListBoxStyle;
  PMemo = ^TMemo;
  PMemoScrollbar = ^TMemoScrollbar;
  PRadioButton = ^TRadioButton;
  PScrollStyle = ^TScrollStyle;
  PSpeedButton = ^TSpeedButton;
  PBitmap = ^TBitmap;
  PNotifyEvent = ^TNotifyEvent;
  PStrings = ^TStrings;
  PPoint = ^TPoint;
  PRect = ^TRect;
  PCanvas = ^TCanvas;
  PMouseMoveEvent = ^TMouseMoveEvent;
  PMouseEvent = ^TMouseEvent;

procedure _LapeCustomComboBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Result)^ := TCustomComboBox.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomComboBox_AddItem(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.AddItem(PString(Params^[1])^, PObject(Params^[2])^);
end;

procedure _LapeCustomComboBox_AddHistoryItem(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.AddHistoryItem(PString(Params^[1])^, Pinteger(Params^[2])^, PBoolean(Params^[3])^, PBoolean(Params^[4])^);
end;

procedure _LapeCustomComboBox_AddHistoryItemEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.AddHistoryItem(PString(Params^[1])^, PObject(Params^[2])^, Pinteger(Params^[3])^, PBoolean(Params^[4])^, PBoolean(Params^[5])^);
end;

procedure _LapeCustomComboBox_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.Clear();
end;

procedure _LapeCustomComboBox_ClearSelection(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.ClearSelection();
end;

procedure _LapeCustomComboBox_DroppedDown_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.DroppedDown;
end;

procedure _LapeCustomComboBox_DroppedDown_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.DroppedDown := PBoolean(Params^[1])^;
end;

procedure _LapeCustomComboBox_SelectAll(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.SelectAll();
end;

procedure _LapeCustomComboBox_AutoComplete_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoComplete;
end;

procedure _LapeCustomComboBox_AutoComplete_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.AutoComplete := PBoolean(Params^[1])^;
end;

procedure _LapeCustomComboBox_AutoDropDown_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoDropDown;
end;

procedure _LapeCustomComboBox_AutoDropDown_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.AutoDropDown := PBoolean(Params^[1])^;
end;

procedure _LapeCustomComboBox_AutoSelect_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoSelect;
end;

procedure _LapeCustomComboBox_AutoSelect_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.AutoSelect := PBoolean(Params^[1])^;
end;

procedure _LapeCustomComboBox_AutoSelected_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoSelected;
end;

procedure _LapeCustomComboBox_AutoSelected_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.AutoSelected := PBoolean(Params^[1])^;
end;

procedure _LapeCustomComboBox_ArrowKeysTraverseList_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.ArrowKeysTraverseList;
end;

procedure _LapeCustomComboBox_ArrowKeysTraverseList_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.ArrowKeysTraverseList := PBoolean(Params^[1])^;
end;

procedure _LapeCustomComboBox_Canvas_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Result)^ := PCustomComboBox(Params^[0])^.Canvas;
end;

procedure _LapeCustomComboBox_DropDownCount_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomComboBox(Params^[0])^.DropDownCount;
end;

procedure _LapeCustomComboBox_DropDownCount_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.DropDownCount := PInteger(Params^[1])^;
end;

procedure _LapeCustomComboBox_Items_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Result)^ := PCustomComboBox(Params^[0])^.Items;
end;

procedure _LapeCustomComboBox_Items_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.Items := PStrings(Params^[1])^;
end;

procedure _LapeCustomComboBox_ItemIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomComboBox(Params^[0])^.ItemIndex;
end;

procedure _LapeCustomComboBox_ItemIndex_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.ItemIndex := Pinteger(Params^[1])^;
end;

procedure _LapeCustomComboBox_ReadOnly_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.ReadOnly;
end;

procedure _LapeCustomComboBox_ReadOnly_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.ReadOnly := PBoolean(Params^[1])^;
end;

procedure _LapeCustomComboBox_SelLength_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomComboBox(Params^[0])^.SelLength;
end;

procedure _LapeCustomComboBox_SelLength_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.SelLength := Pinteger(Params^[1])^;
end;

procedure _LapeCustomComboBox_SelStart_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomComboBox(Params^[0])^.SelStart;
end;

procedure _LapeCustomComboBox_SelStart_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.SelStart := Pinteger(Params^[1])^;
end;

procedure _LapeCustomComboBox_SelText_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PCustomComboBox(Params^[0])^.SelText;
end;

procedure _LapeCustomComboBox_SelText_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.SelText := PString(Params^[1])^;
end;

procedure _LapeCustomComboBox_Style_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PComboBoxStyle(Result)^ := PCustomComboBox(Params^[0])^.Style;
end;

procedure _LapeCustomComboBox_Style_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.Style := PComboBoxStyle(Params^[1])^;
end;

procedure _LapeCustomComboBox_Text_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PCustomComboBox(Params^[0])^.Text;
end;

procedure _LapeCustomComboBox_Text_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.Text := PString(Params^[1])^;
end;

procedure _LapeComboBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PComboBox(Result)^ := TComboBox.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomComboBox_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PComboBox(Params^[0])^.OnChange;
end;

procedure _LapeCustomComboBox_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PComboBox(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomListBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Result)^ := TCustomListBox.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomListBox_AddItem(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.AddItem(PString(Params^[1])^, PObject(Params^[2])^);
end;

procedure _LapeCustomListBox_Click(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.Click();
end;

procedure _LapeCustomListBox_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.Clear();
end;

procedure _LapeCustomListBox_ClearSelection(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.ClearSelection();
end;

procedure _LapeCustomListBox_GetIndexAtXY(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.GetIndexAtXY(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeCustomListBox_GetIndexAtY(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.GetIndexAtY(Pinteger(Params^[1])^);
end;

procedure _LapeCustomListBox_GetSelectedText(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PCustomListBox(Params^[0])^.GetSelectedText();
end;

procedure _LapeCustomListBox_ItemAtPos(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.ItemAtPos(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeCustomListBox_ItemRect(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRect(Result)^ := PCustomListBox(Params^[0])^.ItemRect(PInteger(Params^[1])^);
end;

procedure _LapeCustomListBox_ItemVisible(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomListBox(Params^[0])^.ItemVisible(PInteger(Params^[1])^);
end;

procedure _LapeCustomListBox_ItemFullyVisible(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomListBox(Params^[0])^.ItemFullyVisible(PInteger(Params^[1])^);
end;

procedure _LapeCustomListBox_LockSelectionChange(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.LockSelectionChange();
end;

procedure _LapeCustomListBox_MakeCurrentVisible(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.MakeCurrentVisible();
end;

procedure _LapeCustomListBox_MeasureItem(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.MeasureItem(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCustomListBox_SelectAll(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.SelectAll();
end;

procedure _LapeCustomListBox_UnlockSelectionChange(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.UnlockSelectionChange();
end;

procedure _LapeCustomListBox_DrawFocusRect_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := lboDrawFocusRect in PCustomListBox(Params^[0])^.Options;
end;

procedure _LapeCustomListBox_DrawFocusRect_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.Options := PCustomListBox(Params^[0])^.Options + [lboDrawFocusRect];
end;

procedure _LapeCustomListBox_Canvas_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCanvas(Result)^ := PCustomListBox(Params^[0])^.Canvas;
end;

procedure _LapeCustomListBox_ClickOnSelChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomListBox(Params^[0])^.ClickOnSelChange;
end;

procedure _LapeCustomListBox_ClickOnSelChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.ClickOnSelChange := PBoolean(Params^[1])^;
end;

procedure _LapeCustomListBox_Columns_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.Columns;
end;

procedure _LapeCustomListBox_Columns_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.Columns := PInteger(Params^[1])^;
end;

procedure _LapeCustomListBox_Count_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.Count;
end;

procedure _LapeCustomListBox_ExtendedSelect_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomListBox(Params^[0])^.ExtendedSelect;
end;

procedure _LapeCustomListBox_ExtendedSelect_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.ExtendedSelect := PBoolean(Params^[1])^;
end;

procedure _LapeCustomListBox_ItemHeight_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.ItemHeight;
end;

procedure _LapeCustomListBox_ItemHeight_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.ItemHeight := PInteger(Params^[1])^;
end;

procedure _LapeCustomListBox_ItemIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.ItemIndex;
end;

procedure _LapeCustomListBox_ItemIndex_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.ItemIndex := Pinteger(Params^[1])^;
end;

procedure _LapeCustomListBox_Items_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Result)^ := PCustomListBox(Params^[0])^.Items;
end;

procedure _LapeCustomListBox_Items_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.Items := PStrings(Params^[1])^;
end;

procedure _LapeCustomListBox_MultiSelect_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomListBox(Params^[0])^.MultiSelect;
end;

procedure _LapeCustomListBox_MultiSelect_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.MultiSelect := PBoolean(Params^[1])^;
end;

procedure _LapeCustomListBox_ScrollWidth_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.ScrollWidth;
end;

procedure _LapeCustomListBox_ScrollWidth_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.ScrollWidth := PInteger(Params^[1])^;
end;

procedure _LapeCustomListBox_SelCount_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.SelCount;
end;

procedure _LapeCustomListBox_Sorted_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomListBox(Params^[0])^.Sorted;
end;

procedure _LapeCustomListBox_Sorted_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.Sorted := PBoolean(Params^[1])^;
end;

procedure _LapeCustomListBox_TopIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.TopIndex;
end;

procedure _LapeCustomListBox_TopIndex_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.TopIndex := PInteger(Params^[1])^;
end;

procedure _LapeCustomListBox_OnDblClick_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCustomListBox(Params^[0])^.OnDblClick;
end;

procedure _LapeCustomListBox_OnDblClick_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.OnDblClick := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomListBox_Style_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PListBoxStyle(Result)^ := PCustomListBox(Params^[0])^.Style;
end;

procedure _LapeCustomListBox_Style_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.Style := PListBoxStyle(Params^[1])^;
end;

procedure _LapeCustomListBox_OnSelectionChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSelectionChangeEvent(Result)^ := PCustomListBox(Params^[0])^.OnSelectionChange;
end;

procedure _LapeCustomListBox_OnSelectionChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.OnSelectionChange := PSelectionChangeEvent(Params^[1])^;
end;

procedure _LapeCustomListBox_OnMeasureItem_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMeasureItemEvent(Result)^ := PCustomListBox(Params^[0])^.OnMeasureItem;
end;

procedure _LapeCustomListBox_OnMeasureItem_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.OnMeasureItem := PMeasureItemEvent(Params^[1])^;
end;

procedure _LapeCustomListBox_OnDrawItem_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDrawItemEvent(Result)^ := PCustomListBox(Params^[0])^.OnDrawItem;
end;

procedure _LapeCustomListBox_OnDrawItem_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.OnDrawItem := PDrawItemEvent(Params^[1])^;
end;

procedure _LapeListBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PListBox(Result)^ := TListBox.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomEdit_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Result)^ := TCustomEdit.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomEdit_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.Clear();
end;

procedure _LapeCustomEdit_SelectAll(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.SelectAll();
end;

procedure _LapeCustomEdit_ClearSelection(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.ClearSelection();
end;

procedure _LapeCustomEdit_CopyToClipboard(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.CopyToClipboard();
end;

procedure _LapeCustomEdit_CutToClipboard(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.CutToClipboard();
end;

procedure _LapeCustomEdit_PasteFromClipboard(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.PasteFromClipboard();
end;

procedure _LapeCustomEdit_Undo(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.Undo();
end;

procedure _LapeCustomEdit_CanUndo_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.CanUndo;
end;

procedure _LapeCustomEdit_CaretPos_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PCustomEdit(Params^[0])^.CaretPos;
end;

procedure _LapeCustomEdit_CaretPos_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.CaretPos := PPoint(Params^[1])^;
end;

procedure _LapeCustomEdit_HideSelection_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.HideSelection;
end;

procedure _LapeCustomEdit_HideSelection_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.HideSelection := PBoolean(Params^[1])^;
end;

procedure _LapeCustomEdit_MaxLength_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomEdit(Params^[0])^.MaxLength;
end;

procedure _LapeCustomEdit_MaxLength_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.MaxLength := PInteger(Params^[1])^;
end;

procedure _LapeCustomEdit_Modified_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.Modified;
end;

procedure _LapeCustomEdit_Modified_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.Modified := PBoolean(Params^[1])^;
end;

procedure _LapeCustomEdit_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCustomEdit(Params^[0])^.OnChange;
end;

procedure _LapeCustomEdit_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomEdit_PasswordChar_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PChar(Result)^ := PCustomEdit(Params^[0])^.PasswordChar;
end;

procedure _LapeCustomEdit_PasswordChar_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.PasswordChar := PChar(Params^[1])^;
end;

procedure _LapeCustomEdit_ReadOnly_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.ReadOnly;
end;

procedure _LapeCustomEdit_ReadOnly_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.ReadOnly := PBoolean(Params^[1])^;
end;

procedure _LapeCustomEdit_SelLength_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomEdit(Params^[0])^.SelLength;
end;

procedure _LapeCustomEdit_SelLength_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.SelLength := Pinteger(Params^[1])^;
end;

procedure _LapeCustomEdit_SelStart_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomEdit(Params^[0])^.SelStart;
end;

procedure _LapeCustomEdit_SelStart_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.SelStart := Pinteger(Params^[1])^;
end;

procedure _LapeCustomEdit_SelText_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PCustomEdit(Params^[0])^.SelText;
end;

procedure _LapeCustomEdit_SelText_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.SelText := PString(Params^[1])^;
end;

procedure _LapeCustomEdit_Text_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PCustomEdit(Params^[0])^.Text;
end;

procedure _LapeCustomEdit_Text_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.Text := PString(Params^[1])^;
end;

procedure _LapeEdit_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PEdit(Result)^ := TEdit.Create(PComponent(Params^[0])^);
end;

procedure _LapeEdit_OnEditingDone_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PEdit(Params^[0])^.OnEditingDone;
end;

procedure _LapeEdit_OnEditingDone_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PEdit(Params^[0])^.OnEditingDone := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomGroupBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomGroupBox(Result)^ := TCustomGroupBox.Create(PComponent(Params^[0])^);
end;

procedure _LapeGroupBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PGroupBox(Result)^ := TGroupBox.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomMemo_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomMemo(Result)^ := TCustomMemo.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomMemo_Append(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomMemo(Params^[0])^.Append(PString(Params^[1])^);
end;

procedure _LapeCustomMemo_Lines_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Result)^ := PCustomMemo(Params^[0])^.Lines;
end;

procedure _LapeCustomMemo_Lines_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomMemo(Params^[0])^.Lines := PStrings(Params^[1])^;
end;

procedure _LapeCustomMemo_HorzScrollBar_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMemoScrollBar(Result)^ := PCustomMemo(Params^[0])^.HorzScrollBar;
end;

procedure _LapeCustomMemo_HorzScrollBar_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomMemo(Params^[0])^.HorzScrollBar := PMemoScrollBar(Params^[1])^;
end;

procedure _LapeCustomMemo_VertScrollBar_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMemoScrollBar(Result)^ := PCustomMemo(Params^[0])^.VertScrollBar;
end;

procedure _LapeCustomMemo_VertScrollBar_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomMemo(Params^[0])^.VertScrollBar := PMemoScrollBar(Params^[1])^;
end;

procedure _LapeCustomMemo_ScrollBars_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PScrollStyle(Result)^ := PCustomMemo(Params^[0])^.ScrollBars;
end;

procedure _LapeCustomMemo_ScrollBars_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomMemo(Params^[0])^.ScrollBars := PScrollStyle(Params^[1])^;
end;

procedure _LapeCustomMemo_WantReturns_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomMemo(Params^[0])^.WantReturns;
end;

procedure _LapeCustomMemo_WantReturns_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomMemo(Params^[0])^.WantReturns := PBoolean(Params^[1])^;
end;

procedure _LapeCustomMemo_WantTabs_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomMemo(Params^[0])^.WantTabs;
end;

procedure _LapeCustomMemo_WantTabs_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomMemo(Params^[0])^.WantTabs := PBoolean(Params^[1])^;
end;

procedure _LapeCustomMemo_WordWrap_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomMemo(Params^[0])^.WordWrap;
end;

procedure _LapeCustomMemo_WordWrap_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomMemo(Params^[0])^.WordWrap := PBoolean(Params^[1])^;
end;

procedure _LapeMemo_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMemo(Result)^ := TMemo.Create(PComponent(Params^[0])^);
end;

procedure _LapeButtonControl_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PButtonControl(Result)^ := TButtonControl.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomButton_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomButton(Result)^ := TCustomButton.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomButton_Active_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomButton(Params^[0])^.Active;
end;

procedure _LapeCustomButton_Default_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomButton(Params^[0])^.Default;
end;

procedure _LapeCustomButton_Default_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomButton(Params^[0])^.Default := PBoolean(Params^[1])^;
end;

procedure _LapeCustomButton_Cancel_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomButton(Params^[0])^.Cancel;
end;

procedure _LapeCustomButton_Cancel_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomButton(Params^[0])^.Cancel := PBoolean(Params^[1])^;
end;

procedure _LapeButton_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PButton(Result)^ := TButton.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomCheckBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckBox(Result)^ := TCustomCheckBox.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomCheckBox_AllowGrayed_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomCheckBox(Params^[0])^.AllowGrayed;
end;

procedure _LapeCustomCheckBox_AllowGrayed_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckBox(Params^[0])^.AllowGrayed := PBoolean(Params^[1])^;
end;

procedure _LapeCustomCheckBox_State_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckBoxState(Result)^ := PCustomCheckBox(Params^[0])^.State;
end;

procedure _LapeCustomCheckBox_State_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckBox(Params^[0])^.State := PCheckBoxState(Params^[1])^;
end;

procedure _LapeCustomCheckBox_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PCustomCheckBox(Params^[0])^.OnChange;
end;

procedure _LapeCustomCheckBox_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckBox(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCheckBox_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckBox(Result)^ := TCheckBox.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomLabel_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomLabel(Result)^ := TCustomLabel.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomLabel_AdjustFontForOptimalFill(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomLabel(Params^[0])^.AdjustFontForOptimalFill();
end;

procedure _LapeCustomLabel_SetBounds(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomLabel(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeLabel_Alignment_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PAlignment(Result)^ := PLabel(Params^[0])^.Alignment;
end;

procedure _LapeLabel_Alignment_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLabel(Params^[0])^.Alignment := PAlignment(Params^[1])^;
end;

procedure _LapeLabel_Layout_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTextLayout(Result)^ := PLabel(Params^[0])^.Layout;
end;

procedure _LapeLabel_Layout_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLabel(Params^[0])^.Layout := PTextLayout(Params^[1])^;
end;

procedure _LapeLabel_Transparent_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLabel(Params^[0])^.Transparent;
end;

procedure _LapeLabel_Transparent_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLabel(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

procedure _LapeLabel_WordWrap_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLabel(Params^[0])^.WordWrap;
end;

procedure _LapeLabel_WordWrap_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLabel(Params^[0])^.WordWrap := PBoolean(Params^[1])^;
end;

procedure _LapeLabel_OptimalFill_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PLabel(Params^[0])^.OptimalFill;
end;

procedure _LapeLabel_OptimalFill_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLabel(Params^[0])^.OptimalFill := PBoolean(Params^[1])^;
end;

procedure _LapeLabel_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PLabel(Result)^ := TLabel.Create(PComponent(Params^[0])^);
end;

procedure _LapeSpeedButton_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Result)^ := TSpeedButton.Create(PComponent(Params^[0])^);
end;

procedure _LapeSpeedButton_OnMouseEnter_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PSpeedButton(Params^[0])^.OnMouseEnter;
end;

procedure _LapeSpeedButton_OnMouseEnter_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^.OnMouseEnter := PNotifyEvent(Params^[1])^;
end;

procedure _LapeSpeedButton_OnMouseLeave_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PSpeedButton(Params^[0])^.OnMouseLeave;
end;

procedure _LapeSpeedButton_OnMouseLeave_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^.OnMouseLeave := PNotifyEvent(Params^[1])^;
end;

procedure _LapeSpeedButton_OnMouseMove_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseMoveEvent(Result)^ := PSpeedButton(Params^[0])^.OnMouseMove;
end;

procedure _LapeSpeedButton_OnMouseMove_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^.OnMouseMove := PMouseMoveEvent(Params^[1])^;
end;

procedure _LapeSpeedButton_OnMouseDown_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseEvent(Result)^ := PSpeedButton(Params^[0])^.OnMouseDown;
end;

procedure _LapeSpeedButton_OnMouseDown_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^.OnMouseDown := PMouseEvent(Params^[1])^;
end;

procedure _LapeSpeedButton_OnMouseUp_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMouseEvent(Result)^ := PSpeedButton(Params^[0])^.OnMouseUp;
end;

procedure _LapeSpeedButton_OnMouseUp_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^.OnMouseUp := PMouseEvent(Params^[1])^;
end;

procedure _LapeSpeedButton_OnPaint_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PSpeedButton(Params^[0])^.OnPaint;
end;

procedure _LapeSpeedButton_OnPaint_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^.OnPaint := PNotifyEvent(Params^[1])^;
end;

procedure _LapeSpeedButton_Glyph_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Result)^ := PSpeedButton(Params^[0])^.Glyph;
end;

procedure _LapeSpeedButton_Down_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSpeedButton(Params^[0])^.Down;
end;

procedure _LapeSpeedButton_Down_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^.Down := PBoolean(Params^[1])^;
end;

procedure _LapeSpeedButton_Flat_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSpeedButton(Params^[0])^.Flat;
end;

procedure _LapeSpeedButton_Flat_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^.Flat := PBoolean(Params^[1])^;
end;

procedure _LapeSpeedButton_Layout_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PButtonLayout(Result)^ := PSpeedButton(Params^[0])^.Layout;
end;

procedure _LapeSpeedButton_Layout_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^.Layout := PButtonLayout(Params^[1])^;
end;

procedure _LapeSpeedButton_Margin_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PSpeedButton(Params^[0])^.Margin;
end;

procedure _LapeSpeedButton_Margin_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^.Margin := Pinteger(Params^[1])^;
end;

procedure _LapeSpeedButton_ShowCaption_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSpeedButton(Params^[0])^.ShowCaption;
end;

procedure _LapeSpeedButton_ShowCaption_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^.ShowCaption := PBoolean(Params^[1])^;
end;

procedure _LapeSpeedButton_Spacing_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PSpeedButton(Params^[0])^.Spacing;
end;

procedure _LapeSpeedButton_Spacing_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^.Spacing := Pinteger(Params^[1])^;
end;

procedure _LapeSpeedButton_Transparent_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PSpeedButton(Params^[0])^.Transparent;
end;

procedure _LapeSpeedButton_Transparent_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

procedure _LapeSpeedButton_Glyph_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^.Glyph := PBitmap(Params^[1])^;
end;

procedure _LapeRadioButton_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRadioButton(Result)^ := TRadioButton.Create(PComponent(Params^[0])^);
end;

procedure ImportLCLStdCtrls(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType('(ssNone, ssHorizontal, ssVertical, ssBoth, ssAutoHorizontal, ssAutoVertical, ssAutoBoth)', 'TLazScrollStyle');
    addGlobalType('(odSelected, odGrayed, odDisabled, odChecked, odFocused, odDefault, odHotLight, odInactive, odNoAccel, odNoFocusRect, odReserved1, odReserved2, odComboBoxEdit, odPainted)', 'TLazOwnerDrawStateType');
    addGlobalType('set of TLazOwnerDrawStateType', 'TLazOwnerDrawState');
    addGlobalType('procedure(Control: TLazWinControl; Index: Integer; ARect: TLazRect; State: TLazOwnerDrawState) of object', 'TLazDrawItemEvent', FFI_DEFAULT_ABI);
    addGlobalType('(csDropDown,csSimple,csDropDownList,csOwnerDrawFixed,csOwnerDrawVariable)', 'TLazComboBoxStyle');
    addGlobalType('(lbStandard, lbOwnerDrawFixed, lbOwnerDrawVariable, lbVirtual)', 'TLazListBoxStyle');
    addGlobalType('(taLeftJustify, taRightJustify, taCenter)', 'TLazAlignment');
    addGlobalType('(cbUnchecked, cbChecked, cbGrayed)', 'TLazCheckBoxState');
    addGlobalType('(blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom)', 'TLazButtonLayout');
    addGlobalType('procedure(Sender: TObject; User: Boolean) of object', 'TLazSelectionChangeEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Control: TLazWinControl; Index: Integer; var AHeight: Integer) of object', 'TLazMeasureItemEvent', FFI_DEFAULT_ABI);

    addClass('TLazCustomComboBox', 'TLazWinControl');
    addClassConstructor('TLazCustomComboBox', '(TheOwner: TLazComponent)', @_LapeCustomComboBox_Create);
    addGlobalFunc('procedure TLazCustomComboBox.AddItem(const Item: String; AnObject: TObject);', @_LapeCustomComboBox_AddItem);
    addGlobalFunc('procedure TLazCustomComboBox.AddHistoryItem(const Item: string; MaxHistoryCount: Integer;SetAsText, CaseSensitive: Boolean);', @_LapeCustomComboBox_AddHistoryItem);
    addGlobalFunc('procedure TLazCustomComboBox.AddHistoryItem(const Item: string; AnObject: TObject;MaxHistoryCount: Integer; SetAsText, CaseSensitive: Boolean); overload', @_LapeCustomComboBox_AddHistoryItemEx);
    addGlobalFunc('procedure TLazCustomComboBox.Clear;', @_LapeCustomComboBox_Clear);
    addGlobalFunc('procedure TLazCustomComboBox.ClearSelection;', @_LapeCustomComboBox_ClearSelection);
    addGlobalFunc('procedure TLazCustomComboBox.SelectAll;', @_LapeCustomComboBox_SelectAll);
    addClassVar('TLazCustomComboBox', 'DroppedDown', 'Boolean', @_LapeCustomComboBox_DroppedDown_Read, @_LapeCustomComboBox_DroppedDown_Write);
    addClassVar('TLazCustomComboBox', 'AutoComplete', 'Boolean', @_LapeCustomComboBox_AutoComplete_Read, @_LapeCustomComboBox_AutoComplete_Write);
    addClassVar('TLazCustomComboBox', 'AutoDropDown', 'Boolean', @_LapeCustomComboBox_AutoDropDown_Read, @_LapeCustomComboBox_AutoDropDown_Write);
    addClassVar('TLazCustomComboBox', 'AutoSelect', 'Boolean', @_LapeCustomComboBox_AutoSelect_Read, @_LapeCustomComboBox_AutoSelect_Write);
    addClassVar('TLazCustomComboBox', 'AutoSelected', 'Boolean', @_LapeCustomComboBox_AutoSelected_Read, @_LapeCustomComboBox_AutoSelected_Write);
    addClassVar('TLazCustomComboBox', 'ArrowKeysTraverseList', 'Boolean', @_LapeCustomComboBox_ArrowKeysTraverseList_Read, @_LapeCustomComboBox_ArrowKeysTraverseList_Write);
    addClassVar('TLazCustomComboBox', 'Canvas', 'TLazCanvas', @_LapeCustomComboBox_Canvas_Read);
    addClassVar('TLazCustomComboBox', 'DropDownCount', 'Integer', @_LapeCustomComboBox_DropDownCount_Read, @_LapeCustomComboBox_DropDownCount_Write);
    addClassVar('TLazCustomComboBox', 'Items', 'TLazStrings', @_LapeCustomComboBox_Items_Read, @_LapeCustomComboBox_Items_Write);
    addClassVar('TLazCustomComboBox', 'ItemIndex', 'Integer', @_LapeCustomComboBox_ItemIndex_Read, @_LapeCustomComboBox_ItemIndex_Write);
    addClassVar('TLazCustomComboBox', 'ReadOnly', 'Boolean', @_LapeCustomComboBox_ReadOnly_Read, @_LapeCustomComboBox_ReadOnly_Write);
    addClassVar('TLazCustomComboBox', 'SelLength', 'Integer', @_LapeCustomComboBox_SelLength_Read, @_LapeCustomComboBox_SelLength_Write);
    addClassVar('TLazCustomComboBox', 'SelStart', 'Integer', @_LapeCustomComboBox_SelStart_Read, @_LapeCustomComboBox_SelStart_Write);
    addClassVar('TLazCustomComboBox', 'SelText', 'String', @_LapeCustomComboBox_SelText_Read, @_LapeCustomComboBox_SelText_Write);
    addClassVar('TLazCustomComboBox', 'Style', 'TLazComboBoxStyle', @_LapeCustomComboBox_Style_Read, @_LapeCustomComboBox_Style_Write);
    addClassVar('TLazCustomComboBox', 'Text', 'string', @_LapeCustomComboBox_Text_Read, @_LapeCustomComboBox_Text_Write);

    addClass('TLazComboBox', 'TLazCustomComboBox');
    addClassConstructor('TLazComboBox', '(TheOwner: TLazComponent)', @_LapeComboBox_Create);
    addClassVar('TLazComboBox', 'OnChange', 'TLazNotifyEvent', @_LapeCustomComboBox_OnChange_Read, @_LapeCustomComboBox_OnChange_Write);

    addClass('TLazCustomListBox', 'TLazWinControl');
    addClassConstructor('TLazCustomListBox', '(TheOwner: TLazComponent)', @_LapeCustomListBox_Create);
    addGlobalFunc('procedure TLazCustomListBox.AddItem(const Item: String; AnObject: TObject);', @_LapeCustomListBox_AddItem);
    addGlobalFunc('procedure TLazCustomListBox.Click;', @_LapeCustomListBox_Click);
    addGlobalFunc('procedure TLazCustomListBox.Clear;', @_LapeCustomListBox_Clear);
    addGlobalFunc('procedure TLazCustomListBox.ClearSelection;', @_LapeCustomListBox_ClearSelection);
    addGlobalFunc('function TLazCustomListBox.GetIndexAtXY(X, Y: Integer): Integer;', @_LapeCustomListBox_GetIndexAtXY);
    addGlobalFunc('function TLazCustomListBox.GetIndexAtY(Y: Integer): Integer;', @_LapeCustomListBox_GetIndexAtY);
    addGlobalFunc('function TLazCustomListBox.GetSelectedText: string;', @_LapeCustomListBox_GetSelectedText);
    addGlobalFunc('function TLazCustomListBox.ItemAtPos(const Pos: TPoint; Existing: Boolean): Integer;', @_LapeCustomListBox_ItemAtPos);
    addGlobalFunc('function TLazCustomListBox.ItemRect(Index: Integer): TLazRect;', @_LapeCustomListBox_ItemRect);
    addGlobalFunc('function TLazCustomListBox.ItemVisible(Index: Integer): Boolean;', @_LapeCustomListBox_ItemVisible);
    addGlobalFunc('function TLazCustomListBox.ItemFullyVisible(Index: Integer): Boolean;', @_LapeCustomListBox_ItemFullyVisible);
    addGlobalFunc('procedure TLazCustomListBox.LockSelectionChange;', @_LapeCustomListBox_LockSelectionChange);
    addGlobalFunc('procedure TLazCustomListBox.MakeCurrentVisible;', @_LapeCustomListBox_MakeCurrentVisible);
    addGlobalFunc('procedure TLazCustomListBox.MeasureItem(Index: Integer; var TheHeight: Integer);', @_LapeCustomListBox_MeasureItem);
    addGlobalFunc('procedure TLazCustomListBox.SelectAll;', @_LapeCustomListBox_SelectAll);
    addGlobalFunc('procedure TLazCustomListBox.UnlockSelectionChange;', @_LapeCustomListBox_UnlockSelectionChange);
    addClassVar('TLazCustomListBox', 'DrawFocusRect', 'Boolean', @_LapeCustomListBox_DrawFocusRect_Read, @_LapeCustomListBox_DrawFocusRect_Write);
    addClassVar('TLazCustomListBox', 'Canvas', 'TLazCanvas', @_LapeCustomListBox_Canvas_Read);
    addClassVar('TLazCustomListBox', 'ClickOnSelChange', 'Boolean', @_LapeCustomListBox_ClickOnSelChange_Read, @_LapeCustomListBox_ClickOnSelChange_Write);
    addClassVar('TLazCustomListBox', 'Columns', 'Integer', @_LapeCustomListBox_Columns_Read, @_LapeCustomListBox_Columns_Write);
    addClassVar('TLazCustomListBox', 'Count', 'Integer', @_LapeCustomListBox_Count_Read);
    addClassVar('TLazCustomListBox', 'ExtendedSelect', 'Boolean', @_LapeCustomListBox_ExtendedSelect_Read, @_LapeCustomListBox_ExtendedSelect_Write);
    addClassVar('TLazCustomListBox', 'ItemHeight', 'Integer', @_LapeCustomListBox_ItemHeight_Read, @_LapeCustomListBox_ItemHeight_Write);
    addClassVar('TLazCustomListBox', 'ItemIndex', 'Integer', @_LapeCustomListBox_ItemIndex_Read, @_LapeCustomListBox_ItemIndex_Write);
    addClassVar('TLazCustomListBox', 'Items', 'TLazStrings', @_LapeCustomListBox_Items_Read, @_LapeCustomListBox_Items_Write);
    addClassVar('TLazCustomListBox', 'MultiSelect', 'Boolean', @_LapeCustomListBox_MultiSelect_Read, @_LapeCustomListBox_MultiSelect_Write);
    addClassVar('TLazCustomListBox', 'ScrollWidth', 'Integer', @_LapeCustomListBox_ScrollWidth_Read, @_LapeCustomListBox_ScrollWidth_Write);
    addClassVar('TLazCustomListBox', 'SelCount', 'Integer', @_LapeCustomListBox_SelCount_Read);
    addClassVar('TLazCustomListBox', 'Sorted', 'Boolean', @_LapeCustomListBox_Sorted_Read, @_LapeCustomListBox_Sorted_Write);
    addClassVar('TLazCustomListBox', 'TopIndex', 'Integer', @_LapeCustomListBox_TopIndex_Read, @_LapeCustomListBox_TopIndex_Write);
    addClassVar('TLazCustomListBox', 'Style', 'TLazListBoxStyle', @_LapeCustomListBox_Style_Read, @_LapeCustomListBox_Style_Write);
    addClassVar('TLazCustomListBox', 'OnDblClick', 'TLazNotifyEvent', @_LapeCustomListBox_OnDblClick_Read, @_LapeCustomListBox_OnDblClick_Write);
    addClassVar('TLazCustomListBox', 'OnDrawItem', 'TLazDrawItemEvent', @_LapeCustomListBox_OnDrawItem_Read, @_LapeCustomListBox_OnDrawItem_Write);
    addClassVar('TLazCustomListBox', 'OnMeasureItemEvent', 'TLazMeasureItemEvent', @_LapeCustomListBox_OnMeasureItem_Read, @_LapeCustomListBox_OnMeasureItem_Write);
    addClassVar('TLazCustomListBox', 'OnSelectionChange', 'TLazSelectionChangeEvent', @_LapeCustomListBox_OnSelectionChange_Read, @_LapeCustomListBox_OnSelectionChange_Write);

    addClass('TLazListBox', 'TLazCustomListBox');
    addClassConstructor('TLazListBox', '(AOwner: TLazComponent)', @_LapeListBox_Create);

    addClass('TLazCustomEdit', 'TLazWinControl');
    addClassConstructor('TLazCustomEdit', '(AOwner: TLazComponent)', @_LapeCustomEdit_Create);
    addGlobalFunc('procedure TLazCustomEdit.Clear;', @_LapeCustomEdit_Clear);
    addGlobalFunc('procedure TLazCustomEdit.SelectAll;', @_LapeCustomEdit_SelectAll);
    addGlobalFunc('procedure TLazCustomEdit.ClearSelection;', @_LapeCustomEdit_ClearSelection);
    addGlobalFunc('procedure TLazCustomEdit.CopyToClipboard;', @_LapeCustomEdit_CopyToClipboard);
    addGlobalFunc('procedure TLazCustomEdit.CutToClipboard;', @_LapeCustomEdit_CutToClipboard);
    addGlobalFunc('procedure TLazCustomEdit.PasteFromClipboard;', @_LapeCustomEdit_PasteFromClipboard);
    addGlobalFunc('procedure TLazCustomEdit.Undo;', @_LapeCustomEdit_Undo);
    addClassVar('TLazCustomEdit', 'CanUndo', 'Boolean', @_LapeCustomEdit_CanUndo_Read);
    addClassVar('TLazCustomEdit', 'CaretPos', 'TPoint', @_LapeCustomEdit_CaretPos_Read, @_LapeCustomEdit_CaretPos_Write);
    addClassVar('TLazCustomEdit', 'HideSelection', 'Boolean', @_LapeCustomEdit_HideSelection_Read, @_LapeCustomEdit_HideSelection_Write);
    addClassVar('TLazCustomEdit', 'MaxLength', 'Integer', @_LapeCustomEdit_MaxLength_Read, @_LapeCustomEdit_MaxLength_Write);
    addClassVar('TLazCustomEdit', 'Modified', 'Boolean', @_LapeCustomEdit_Modified_Read, @_LapeCustomEdit_Modified_Write);
    addClassVar('TLazCustomEdit', 'OnChange', 'TLazNotifyEvent', @_LapeCustomEdit_OnChange_Read, @_LapeCustomEdit_OnChange_Write);
    addClassVar('TLazCustomEdit', 'PasswordChar', 'Char', @_LapeCustomEdit_PasswordChar_Read, @_LapeCustomEdit_PasswordChar_Write);
    addClassVar('TLazCustomEdit', 'ReadOnly', 'Boolean', @_LapeCustomEdit_ReadOnly_Read, @_LapeCustomEdit_ReadOnly_Write);
    addClassVar('TLazCustomEdit', 'SelLength', 'Integer', @_LapeCustomEdit_SelLength_Read, @_LapeCustomEdit_SelLength_Write);
    addClassVar('TLazCustomEdit', 'SelStart', 'Integer', @_LapeCustomEdit_SelStart_Read, @_LapeCustomEdit_SelStart_Write);
    addClassVar('TLazCustomEdit', 'SelText', 'String', @_LapeCustomEdit_SelText_Read, @_LapeCustomEdit_SelText_Write);
    addClassVar('TLazCustomEdit', 'Text', 'string', @_LapeCustomEdit_Text_Read, @_LapeCustomEdit_Text_Write);

    addClass('TLazEdit', 'TLazCustomEdit');
    addClassVar('TLazEdit', 'OnEditingDone', 'TLazNotifyEvent', @_LapeEdit_OnEditingDone_Read, @_LapeEdit_OnEditingDone_Write);
    addClassConstructor('TLazEdit', '(AOwner: TLazComponent)', @_LapeEdit_Create);

    addClass('TLazCustomGroupBox', 'TLazWinControl');
    addClassConstructor('TLazCustomGroupBox', '(AOwner: TLazComponent)', @_LapeCustomGroupBox_Create);

    addClass('TLazGroupBox', 'TLazCustomGroupBox');
    addClassConstructor('TLazGroupBox', '(AOwner: TLazComponent)', @_LapeGroupBox_Create);

    addClass('TLazMemoScrollBar', 'TLazControlScrollBar');
    addClass('TLazCustomMemo', 'TLazCustomEdit');
    addClassConstructor('TLazCustomMemo', '(AOwner: TLazComponent)', @_LapeCustomMemo_Create);
    addGlobalFunc('procedure TLazCustomMemo.Append(const Value: String);', @_LapeCustomMemo_Append);
    addClassVar('TLazCustomMemo', 'Lines', 'TLazStrings', @_LapeCustomMemo_Lines_Read, @_LapeCustomMemo_Lines_Write);
    addClassVar('TLazCustomMemo', 'HorzScrollBar', 'TLazMemoScrollBar', @_LapeCustomMemo_HorzScrollBar_Read, @_LapeCustomMemo_HorzScrollBar_Write);
    addClassVar('TLazCustomMemo', 'VertScrollBar', 'TLazMemoScrollBar', @_LapeCustomMemo_VertScrollBar_Read, @_LapeCustomMemo_VertScrollBar_Write);
    addClassVar('TLazCustomMemo', 'ScrollBars', 'TLazScrollStyle', @_LapeCustomMemo_ScrollBars_Read, @_LapeCustomMemo_ScrollBars_Write);
    addClassVar('TLazCustomMemo', 'WantReturns', 'Boolean', @_LapeCustomMemo_WantReturns_Read, @_LapeCustomMemo_WantReturns_Write);
    addClassVar('TLazCustomMemo', 'WantTabs', 'Boolean', @_LapeCustomMemo_WantTabs_Read, @_LapeCustomMemo_WantTabs_Write);
    addClassVar('TLazCustomMemo', 'WordWrap', 'Boolean', @_LapeCustomMemo_WordWrap_Read, @_LapeCustomMemo_WordWrap_Write);

    addClass('TLazMemo', 'TLazCustomMemo');
    addClassConstructor('TLazMemo', '(AOwner: TLazComponent)', @_LapeMemo_Create);

    addClass('TLazButtonControl', 'TLazWinControl');
    addClassConstructor('TLazButtonControl', '(TheOwner: TLazComponent)', @_LapeButtonControl_Create);

    addClass('TLazCustomButton', 'TLazButtonControl');
    addClassConstructor('TLazCustomButton', '(TheOwner: TLazComponent)', @_LapeCustomButton_Create);
    addClassVar('TLazCustomButton', 'Active', 'Boolean', @_LapeCustomButton_Active_Read);
    addClassVar('TLazCustomButton', 'Default', 'Boolean', @_LapeCustomButton_Default_Read, @_LapeCustomButton_Default_Write);
    addClassVar('TLazCustomButton', 'Cancel', 'Boolean', @_LapeCustomButton_Cancel_Read, @_LapeCustomButton_Cancel_Write);

    addClass('TLazButton', 'TLazCustomButton');
    addClassConstructor('TLazButton', '(TheOwner: TLazComponent)', @_LapeButton_Create);

    addClass('TLazCustomCheckBox', 'TLazButtonControl');
    addClassConstructor('TLazCustomCheckBox', '(TheOwner: TLazComponent)', @_LapeCustomCheckBox_Create);
    addClassVar('TLazCustomCheckBox', 'AllowGrayed', 'Boolean', @_LapeCustomCheckBox_AllowGrayed_Read, @_LapeCustomCheckBox_AllowGrayed_Write);
    addClassVar('TLazCustomCheckBox', 'State', 'TLazCheckBoxState', @_LapeCustomCheckBox_State_Read, @_LapeCustomCheckBox_State_Write);
    addClassVar('TLazCustomCheckBox', 'OnChange', 'TLazNotifyEvent', @_LapeCustomCheckBox_OnChange_Read, @_LapeCustomCheckBox_OnChange_Write);

    addClass('TLazCheckBox', 'TLazCustomCheckBox');
    addClassConstructor('TLazCheckBox', '(TheOwner: TLazComponent)', @_LapeCheckBox_Create);

    addClass('TLazCustomLabel', 'TLazGraphicControl');
    addClassConstructor('TLazCustomLabel', '(TheOwner: TLazComponent)', @_LapeCustomLabel_Create);
    addGlobalFunc('function TLazCustomLabel.AdjustFontForOptimalFill: Boolean;', @_LapeCustomLabel_AdjustFontForOptimalFill);

    addClass('TLazLabel', 'TLazCustomLabel');
    addClassVar('TLazLabel', 'Alignment', 'TLazAlignment', @_LapeLabel_Alignment_Read, @_LapeLabel_Alignment_Write);
    addClassVar('TLazLabel', 'Layout', 'TLazTextLayout', @_LapeLabel_Layout_Read, @_LapeLabel_Layout_Write);
    addClassVar('TLazLabel', 'Transparent', 'Boolean', @_LapeLabel_Transparent_Read, @_LapeLabel_Transparent_Write);
    addClassVar('TLazLabel', 'WordWrap', 'Boolean', @_LapeLabel_WordWrap_Read, @_LapeLabel_WordWrap_Write);
    addClassVar('TLazLabel', 'OptimalFill', 'Boolean', @_LapeLabel_OptimalFill_Read, @_LapeLabel_OptimalFill_Write);
    addClassConstructor('TLazLabel', '(TheOwner: TLazComponent)', @_LapeLabel_Create);

    addClass('TLazSpeedButton', 'TLazGraphicControl');
    addClassConstructor('TLazSpeedButton', '(AOwner: TLazComponent)', @_LapeSpeedButton_Create);

    addClassVar('TLazSpeedButton', 'OnMouseEnter', 'TLazNotifyEvent', @_LapeSpeedButton_OnMouseEnter_Read, @_LapeSpeedButton_OnMouseEnter_Write);
    addClassVar('TLazSpeedButton', 'OnMouseLeave', 'TLazNotifyEvent', @_LapeSpeedButton_OnMouseLeave_Read, @_LapeSpeedButton_OnMouseLeave_Write);
    addClassVar('TLazSpeedButton', 'OnMouseMove', 'TLazMouseMoveEvent', @_LapeSpeedButton_OnMouseMove_Read, @_LapeSpeedButton_OnMouseMove_Write);
    addClassVar('TLazSpeedButton', 'OnMouseDown', 'TLazMouseEvent', @_LapeSpeedButton_OnMouseDown_Read, @_LapeSpeedButton_OnMouseDown_Write);
    addClassVar('TLazSpeedButton', 'OnMouseUp', 'TLazMouseEvent', @_LapeSpeedButton_OnMouseUp_Read, @_LapeSpeedButton_OnMouseUp_Write);
    addClassVar('TLazSpeedButton', 'OnPaint', 'TLazNotifyEvent', @_LapeSpeedButton_OnPaint_Read, @_LapeSpeedButton_OnPaint_Write);
    addClassVar('TLazSpeedButton', 'Glyph', 'TLazBitmap', @_LapeSpeedButton_Glyph_Read, @_LapeSpeedButton_Glyph_Write);
    addClassVar('TLazSpeedButton', 'Down', 'Boolean', @_LapeSpeedButton_Down_Read, @_LapeSpeedButton_Down_Write);
    addClassVar('TLazSpeedButton', 'Flat', 'Boolean', @_LapeSpeedButton_Flat_Read, @_LapeSpeedButton_Flat_Write);
    addClassVar('TLazSpeedButton', 'Layout', 'TLazButtonLayout', @_LapeSpeedButton_Layout_Read, @_LapeSpeedButton_Layout_Write);
    addClassVar('TLazSpeedButton', 'Margin', 'Integer', @_LapeSpeedButton_Margin_Read, @_LapeSpeedButton_Margin_Write);
    addClassVar('TLazSpeedButton', 'ShowCaption', 'Boolean', @_LapeSpeedButton_ShowCaption_Read, @_LapeSpeedButton_ShowCaption_Write);
    addClassVar('TLazSpeedButton', 'Spacing', 'Integer', @_LapeSpeedButton_Spacing_Read, @_LapeSpeedButton_Spacing_Write);
    addClassVar('TLazSpeedButton', 'Transparent', 'Boolean', @_LapeSpeedButton_Transparent_Read, @_LapeSpeedButton_Transparent_Write);

    addClass('TLazRadioButton', 'TLazCustomCheckBox');
    addClassConstructor('TLazRadioButton', '(AOwner: TLazComponent)', @_LapeRadioButton_Create);
  end;
end;

end.

