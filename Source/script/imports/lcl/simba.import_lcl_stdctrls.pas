unit simba.import_lcl_stdctrls;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.script_compiler;

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

procedure _LapeCustomComboBox_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^ := TCustomComboBox.Create(PComponent(Params^[1])^);
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

procedure _LapeCustomComboBox_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomComboBox(Params^[0])^.Free();
end;

procedure _LapeComboBox_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PComboBox(Params^[0])^ := TComboBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomComboBox_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PComboBox(Params^[0])^.OnChange;
end;

procedure _LapeCustomComboBox_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PComboBox(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeComboBox_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PComboBox(Params^[0])^.Free();
end;

procedure _LapeCustomListBox_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^ := TCustomListBox.Create(PComponent(Params^[1])^);
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

procedure _LapeCustomListBox_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomListBox(Params^[0])^.Free();
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

procedure _LapeListBox_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PListBox(Params^[0])^ := TListBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeListBox_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PListBox(Params^[0])^.Free();
end;

procedure _LapeCustomEdit_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^ := TCustomEdit.Create(PComponent(Params^[1])^);
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

procedure _LapeCustomEdit_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomEdit(Params^[0])^.Free();
end;

procedure _LapeEdit_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PEdit(Params^[0])^ := TEdit.Create(PComponent(Params^[1])^);
end;

procedure _LapeEdit_OnEditingDone_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PEdit(Params^[0])^.OnEditingDone;
end;

procedure _LapeEdit_OnEditingDone_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PEdit(Params^[0])^.OnEditingDone := PNotifyEvent(Params^[1])^;
end;

procedure _LapeEdit_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PEdit(Params^[0])^.Free();
end;

procedure _LapeCustomGroupBox_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomGroupBox(Params^[0])^ := TCustomGroupBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomGroupBox_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomGroupBox(Params^[0])^.Free();
end;

procedure _LapeGroupBox_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGroupBox(Params^[0])^ := TGroupBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeGroupBox_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PGroupBox(Params^[0])^.Free();
end;

procedure _LapeCustomMemo_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomMemo(Params^[0])^ := TCustomMemo.Create(PComponent(Params^[1])^);
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

procedure _LapeCustomMemo_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomMemo(Params^[0])^.Free();
end;

procedure _LapeMemo_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMemo(Params^[0])^ := TMemo.Create(PComponent(Params^[1])^);
end;

procedure _LapeMemo_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMemo(Params^[0])^.Free();
end;

procedure _LapeButtonControl_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PButtonControl(Params^[0])^ := TButtonControl.Create(PComponent(Params^[1])^);
end;

procedure _LapeButtonControl_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PButtonControl(Params^[0])^.Free();
end;

procedure _LapeCustomButton_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomButton(Params^[0])^ := TCustomButton.Create(PComponent(Params^[1])^);
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

procedure _LapeCustomButton_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomButton(Params^[0])^.Free();
end;

procedure _LapeButton_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PButton(Params^[0])^ := TButton.Create(PComponent(Params^[1])^);
end;

procedure _LapeButton_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PButton(Params^[0])^.Free();
end;

procedure _LapeCustomCheckBox_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckBox(Params^[0])^ := TCustomCheckBox.Create(PComponent(Params^[1])^);
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

procedure _LapeCustomCheckBox_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomCheckBox(Params^[0])^.Free();
end;

procedure _LapeCheckBox_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckBox(Params^[0])^ := TCheckBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeCheckBox_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCheckBox(Params^[0])^.Free();
end;

procedure _LapeCustomLabel_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomLabel(Params^[0])^ := TCustomLabel.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomLabel_AdjustFontForOptimalFill(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomLabel(Params^[0])^.AdjustFontForOptimalFill();
end;

procedure _LapeCustomLabel_SetBounds(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomLabel(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeCustomLabel_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomLabel(Params^[0])^.Free();
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

procedure _LapeLabel_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLabel(Params^[0])^ := TLabel.Create(PComponent(Params^[1])^);
end;

procedure _LapeLabel_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PLabel(Params^[0])^.Free();
end;

procedure _LapeCustomSpeedButton_FindDownButton(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpeedButton(Result)^ := PCustomSpeedButton(Params^[0])^.FindDownButton();
end;

procedure _LapeCustomSpeedButton_AllowAllUp_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.AllowAllUp;
end;

procedure _LapeCustomSpeedButton_AllowAllUp_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpeedButton(Params^[0])^.AllowAllUp := PBoolean(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Down_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.Down;
end;

procedure _LapeCustomSpeedButton_Down_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpeedButton(Params^[0])^.Down := PBoolean(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Flat_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.Flat;
end;

procedure _LapeCustomSpeedButton_Flat_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpeedButton(Params^[0])^.Flat := PBoolean(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Glyph_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Result)^ := PCustomSpeedButton(Params^[0])^.Glyph;
end;

procedure _LapeCustomSpeedButton_Glyph_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpeedButton(Params^[0])^.Glyph := PBitmap(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Layout_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PButtonLayout(Result)^ := PCustomSpeedButton(Params^[0])^.Layout;
end;

procedure _LapeCustomSpeedButton_Layout_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpeedButton(Params^[0])^.Layout := PButtonLayout(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Margin_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomSpeedButton(Params^[0])^.Margin;
end;

procedure _LapeCustomSpeedButton_Margin_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpeedButton(Params^[0])^.Margin := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_ShowCaption_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.ShowCaption;
end;

procedure _LapeCustomSpeedButton_ShowCaption_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpeedButton(Params^[0])^.ShowCaption := PBoolean(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Spacing_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomSpeedButton(Params^[0])^.Spacing;
end;

procedure _LapeCustomSpeedButton_Spacing_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpeedButton(Params^[0])^.Spacing := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Transparent_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.Transparent;
end;

procedure _LapeCustomSpeedButton_Transparent_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpeedButton(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpeedButton(Params^[0])^ := TCustomSpeedButton.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomSpeedButton_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpeedButton(Params^[0])^.Free();
end;

procedure _LapeSpeedButton_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^ := TSpeedButton.Create(PComponent(Params^[1])^);
end;

procedure _LapeSpeedButton_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PSpeedButton(Params^[0])^.Free();
end;

procedure _LapeRadioButton_Init(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PRadioButton(Params^[0])^ := TRadioButton.Create(PComponent(Params^[1])^);
end;

procedure _LapeRadioButton_Free(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PRadioButton(Params^[0])^.Free();
end;

procedure ImportLCLStdCtrls(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addGlobalType('(ssNone, ssHorizontal, ssVertical, ssBoth, ssAutoHorizontal, ssAutoVertical, ssAutoBoth)', 'TScrollStyle');
    addGlobalType('(odSelected, odGrayed, odDisabled, odChecked, odFocused, odDefault, odHotLight, odInactive, odNoAccel, odNoFocusRect, odReserved1, odReserved2, odComboBoxEdit, odPainted)', 'TOwnerDrawStateType');
    addGlobalType('set of TOwnerDrawStateType', 'TOwnerDrawState');
    addGlobalType('procedure(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState) of object', 'TDrawItemEvent', FFI_DEFAULT_ABI);
    addGlobalType('(csDropDown,csSimple,csDropDownList,csOwnerDrawFixed,csOwnerDrawVariable)', 'TComboBoxStyle');
    addGlobalType('(lbStandard, lbOwnerDrawFixed, lbOwnerDrawVariable, lbVirtual)', 'TListBoxStyle');
    addGlobalType('(taLeftJustify, taRightJustify, taCenter)', 'TAlignment');
    addGlobalType('(cbUnchecked, cbChecked, cbGrayed)', 'TCheckBoxState');
    addGlobalType('(blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom)', 'TButtonLayout');
    addGlobalType('procedure(Sender: TObject; User: Boolean) of object', 'TSelectionChangeEvent', FFI_DEFAULT_ABI);
    addGlobalType('procedure(Control: TWinControl; Index: Integer; var AHeight: Integer) of object', 'TMeasureItemEvent', FFI_DEFAULT_ABI);

    addClass('TCustomComboBox', 'TWinControl');
    addGlobalFunc('procedure TCustomComboBox.Init(TheOwner: TComponent); override', @_LapeCustomComboBox_Init);
    addGlobalFunc('procedure TCustomComboBox.AddItem(const Item: String; AnObject: TObject);', @_LapeCustomComboBox_AddItem);
    addGlobalFunc('procedure TCustomComboBox.AddHistoryItem(const Item: string; MaxHistoryCount: Integer;SetAsText, CaseSensitive: Boolean);', @_LapeCustomComboBox_AddHistoryItem);
    addGlobalFunc('procedure TCustomComboBox.AddHistoryItem(const Item: string; AnObject: TObject;MaxHistoryCount: Integer; SetAsText, CaseSensitive: Boolean); overload', @_LapeCustomComboBox_AddHistoryItemEx);
    addGlobalFunc('procedure TCustomComboBox.Clear;', @_LapeCustomComboBox_Clear);
    addGlobalFunc('procedure TCustomComboBox.ClearSelection;', @_LapeCustomComboBox_ClearSelection);
    addGlobalFunc('procedure TCustomComboBox.SelectAll;', @_LapeCustomComboBox_SelectAll);
    addClassVar('TCustomComboBox', 'DroppedDown', 'Boolean', @_LapeCustomComboBox_DroppedDown_Read, @_LapeCustomComboBox_DroppedDown_Write);
    addClassVar('TCustomComboBox', 'AutoComplete', 'Boolean', @_LapeCustomComboBox_AutoComplete_Read, @_LapeCustomComboBox_AutoComplete_Write);
    addClassVar('TCustomComboBox', 'AutoDropDown', 'Boolean', @_LapeCustomComboBox_AutoDropDown_Read, @_LapeCustomComboBox_AutoDropDown_Write);
    addClassVar('TCustomComboBox', 'AutoSelect', 'Boolean', @_LapeCustomComboBox_AutoSelect_Read, @_LapeCustomComboBox_AutoSelect_Write);
    addClassVar('TCustomComboBox', 'AutoSelected', 'Boolean', @_LapeCustomComboBox_AutoSelected_Read, @_LapeCustomComboBox_AutoSelected_Write);
    addClassVar('TCustomComboBox', 'ArrowKeysTraverseList', 'Boolean', @_LapeCustomComboBox_ArrowKeysTraverseList_Read, @_LapeCustomComboBox_ArrowKeysTraverseList_Write);
    addClassVar('TCustomComboBox', 'Canvas', 'TCanvas', @_LapeCustomComboBox_Canvas_Read);
    addClassVar('TCustomComboBox', 'DropDownCount', 'Integer', @_LapeCustomComboBox_DropDownCount_Read, @_LapeCustomComboBox_DropDownCount_Write);
    addClassVar('TCustomComboBox', 'Items', 'TStrings', @_LapeCustomComboBox_Items_Read, @_LapeCustomComboBox_Items_Write);
    addClassVar('TCustomComboBox', 'ItemIndex', 'Integer', @_LapeCustomComboBox_ItemIndex_Read, @_LapeCustomComboBox_ItemIndex_Write);
    addClassVar('TCustomComboBox', 'ReadOnly', 'Boolean', @_LapeCustomComboBox_ReadOnly_Read, @_LapeCustomComboBox_ReadOnly_Write);
    addClassVar('TCustomComboBox', 'SelLength', 'Integer', @_LapeCustomComboBox_SelLength_Read, @_LapeCustomComboBox_SelLength_Write);
    addClassVar('TCustomComboBox', 'SelStart', 'Integer', @_LapeCustomComboBox_SelStart_Read, @_LapeCustomComboBox_SelStart_Write);
    addClassVar('TCustomComboBox', 'SelText', 'String', @_LapeCustomComboBox_SelText_Read, @_LapeCustomComboBox_SelText_Write);
    addClassVar('TCustomComboBox', 'Style', 'TComboBoxStyle', @_LapeCustomComboBox_Style_Read, @_LapeCustomComboBox_Style_Write);
    addClassVar('TCustomComboBox', 'Text', 'string', @_LapeCustomComboBox_Text_Read, @_LapeCustomComboBox_Text_Write);

    addClass('TComboBox', 'TCustomComboBox');
    addGlobalFunc('procedure TComboBox.Init(TheOwner: TComponent); override', @_LapeComboBox_Init);
    addClassVar('TComboBox', 'OnChange', 'TNotifyEvent', @_LapeCustomComboBox_OnChange_Read, @_LapeCustomComboBox_OnChange_Write);

    addClass('TCustomListBox', 'TWinControl');
    addGlobalFunc('procedure TCustomListBox.Init(TheOwner: TComponent); override', @_LapeCustomListBox_Init);
    addGlobalFunc('procedure TCustomListBox.AddItem(const Item: String; AnObject: TObject);', @_LapeCustomListBox_AddItem);
    addGlobalFunc('procedure TCustomListBox.Click;', @_LapeCustomListBox_Click);
    addGlobalFunc('procedure TCustomListBox.Clear;', @_LapeCustomListBox_Clear);
    addGlobalFunc('procedure TCustomListBox.ClearSelection;', @_LapeCustomListBox_ClearSelection);
    addGlobalFunc('function TCustomListBox.GetIndexAtXY(X, Y: Integer): Integer;', @_LapeCustomListBox_GetIndexAtXY);
    addGlobalFunc('function TCustomListBox.GetIndexAtY(Y: Integer): Integer;', @_LapeCustomListBox_GetIndexAtY);
    addGlobalFunc('function TCustomListBox.GetSelectedText: string;', @_LapeCustomListBox_GetSelectedText);
    addGlobalFunc('function TCustomListBox.ItemAtPos(const Pos: TPoint; Existing: Boolean): Integer;', @_LapeCustomListBox_ItemAtPos);
    addGlobalFunc('function TCustomListBox.ItemRect(Index: Integer): TRect;', @_LapeCustomListBox_ItemRect);
    addGlobalFunc('function TCustomListBox.ItemVisible(Index: Integer): Boolean;', @_LapeCustomListBox_ItemVisible);
    addGlobalFunc('function TCustomListBox.ItemFullyVisible(Index: Integer): Boolean;', @_LapeCustomListBox_ItemFullyVisible);
    addGlobalFunc('procedure TCustomListBox.LockSelectionChange;', @_LapeCustomListBox_LockSelectionChange);
    addGlobalFunc('procedure TCustomListBox.MakeCurrentVisible;', @_LapeCustomListBox_MakeCurrentVisible);
    addGlobalFunc('procedure TCustomListBox.MeasureItem(Index: Integer; var TheHeight: Integer);', @_LapeCustomListBox_MeasureItem);
    addGlobalFunc('procedure TCustomListBox.SelectAll;', @_LapeCustomListBox_SelectAll);
    addGlobalFunc('procedure TCustomListBox.UnlockSelectionChange;', @_LapeCustomListBox_UnlockSelectionChange);
    addClassVar('TCustomListBox', 'DrawFocusRect', 'Boolean', @_LapeCustomListBox_DrawFocusRect_Read, @_LapeCustomListBox_DrawFocusRect_Write);
    addClassVar('TCustomListBox', 'Canvas', 'TCanvas', @_LapeCustomListBox_Canvas_Read);
    addClassVar('TCustomListBox', 'ClickOnSelChange', 'Boolean', @_LapeCustomListBox_ClickOnSelChange_Read, @_LapeCustomListBox_ClickOnSelChange_Write);
    addClassVar('TCustomListBox', 'Columns', 'Integer', @_LapeCustomListBox_Columns_Read, @_LapeCustomListBox_Columns_Write);
    addClassVar('TCustomListBox', 'Count', 'Integer', @_LapeCustomListBox_Count_Read);
    addClassVar('TCustomListBox', 'ExtendedSelect', 'Boolean', @_LapeCustomListBox_ExtendedSelect_Read, @_LapeCustomListBox_ExtendedSelect_Write);
    addClassVar('TCustomListBox', 'ItemHeight', 'Integer', @_LapeCustomListBox_ItemHeight_Read, @_LapeCustomListBox_ItemHeight_Write);
    addClassVar('TCustomListBox', 'ItemIndex', 'Integer', @_LapeCustomListBox_ItemIndex_Read, @_LapeCustomListBox_ItemIndex_Write);
    addClassVar('TCustomListBox', 'Items', 'TStrings', @_LapeCustomListBox_Items_Read, @_LapeCustomListBox_Items_Write);
    addClassVar('TCustomListBox', 'MultiSelect', 'Boolean', @_LapeCustomListBox_MultiSelect_Read, @_LapeCustomListBox_MultiSelect_Write);
    addClassVar('TCustomListBox', 'ScrollWidth', 'Integer', @_LapeCustomListBox_ScrollWidth_Read, @_LapeCustomListBox_ScrollWidth_Write);
    addClassVar('TCustomListBox', 'SelCount', 'Integer', @_LapeCustomListBox_SelCount_Read);
    addClassVar('TCustomListBox', 'Sorted', 'Boolean', @_LapeCustomListBox_Sorted_Read, @_LapeCustomListBox_Sorted_Write);
    addClassVar('TCustomListBox', 'TopIndex', 'Integer', @_LapeCustomListBox_TopIndex_Read, @_LapeCustomListBox_TopIndex_Write);
    addClassVar('TCustomListBox', 'Style', 'TListBoxStyle', @_LapeCustomListBox_Style_Read, @_LapeCustomListBox_Style_Write);
    addClassVar('TCustomListBox', 'OnDblClick', 'TNotifyEvent', @_LapeCustomListBox_OnDblClick_Read, @_LapeCustomListBox_OnDblClick_Write);
    addClassVar('TCustomListBox', 'OnDrawItem', 'TDrawItemEvent', @_LapeCustomListBox_OnDrawItem_Read, @_LapeCustomListBox_OnDrawItem_Write);
    addClassVar('TCustomListBox', 'OnMeasureItemEvent', 'TMeasureItemEvent', @_LapeCustomListBox_OnMeasureItem_Read, @_LapeCustomListBox_OnMeasureItem_Write);
    addClassVar('TCustomListBox', 'OnSelectionChange', 'TSelectionChangeEvent', @_LapeCustomListBox_OnSelectionChange_Read, @_LapeCustomListBox_OnSelectionChange_Write);

    addClass('TListBox', 'TCustomListBox');
    addGlobalFunc('procedure TListBox.Init(AOwner: TComponent); override', @_LapeListBox_Init);

    addClass('TCustomEdit', 'TWinControl');
    addGlobalFunc('procedure TCustomEdit.Init(AOwner: TComponent); override', @_LapeCustomEdit_Init);
    addGlobalFunc('procedure TCustomEdit.Clear;', @_LapeCustomEdit_Clear);
    addGlobalFunc('procedure TCustomEdit.SelectAll;', @_LapeCustomEdit_SelectAll);
    addGlobalFunc('procedure TCustomEdit.ClearSelection;', @_LapeCustomEdit_ClearSelection);
    addGlobalFunc('procedure TCustomEdit.CopyToClipboard;', @_LapeCustomEdit_CopyToClipboard);
    addGlobalFunc('procedure TCustomEdit.CutToClipboard;', @_LapeCustomEdit_CutToClipboard);
    addGlobalFunc('procedure TCustomEdit.PasteFromClipboard;', @_LapeCustomEdit_PasteFromClipboard);
    addGlobalFunc('procedure TCustomEdit.Undo;', @_LapeCustomEdit_Undo);
    addClassVar('TCustomEdit', 'CanUndo', 'Boolean', @_LapeCustomEdit_CanUndo_Read);
    addClassVar('TCustomEdit', 'CaretPos', 'TPoint', @_LapeCustomEdit_CaretPos_Read, @_LapeCustomEdit_CaretPos_Write);
    addClassVar('TCustomEdit', 'HideSelection', 'Boolean', @_LapeCustomEdit_HideSelection_Read, @_LapeCustomEdit_HideSelection_Write);
    addClassVar('TCustomEdit', 'MaxLength', 'Integer', @_LapeCustomEdit_MaxLength_Read, @_LapeCustomEdit_MaxLength_Write);
    addClassVar('TCustomEdit', 'Modified', 'Boolean', @_LapeCustomEdit_Modified_Read, @_LapeCustomEdit_Modified_Write);
    addClassVar('TCustomEdit', 'OnChange', 'TNotifyEvent', @_LapeCustomEdit_OnChange_Read, @_LapeCustomEdit_OnChange_Write);
    addClassVar('TCustomEdit', 'PasswordChar', 'Char', @_LapeCustomEdit_PasswordChar_Read, @_LapeCustomEdit_PasswordChar_Write);
    addClassVar('TCustomEdit', 'ReadOnly', 'Boolean', @_LapeCustomEdit_ReadOnly_Read, @_LapeCustomEdit_ReadOnly_Write);
    addClassVar('TCustomEdit', 'SelLength', 'Integer', @_LapeCustomEdit_SelLength_Read, @_LapeCustomEdit_SelLength_Write);
    addClassVar('TCustomEdit', 'SelStart', 'Integer', @_LapeCustomEdit_SelStart_Read, @_LapeCustomEdit_SelStart_Write);
    addClassVar('TCustomEdit', 'SelText', 'String', @_LapeCustomEdit_SelText_Read, @_LapeCustomEdit_SelText_Write);
    addClassVar('TCustomEdit', 'Text', 'string', @_LapeCustomEdit_Text_Read, @_LapeCustomEdit_Text_Write);

    addClass('TEdit', 'TCustomEdit');
    addClassVar('TEdit', 'OnEditingDone', 'TNotifyEvent', @_LapeEdit_OnEditingDone_Read, @_LapeEdit_OnEditingDone_Write);
    addGlobalFunc('procedure TEdit.Init(AOwner: TComponent); override', @_LapeEdit_Init);

    addClass('TCustomGroupBox', 'TWinControl');
    addGlobalFunc('procedure TCustomGroupBox.Init(AOwner: TComponent); override', @_LapeCustomGroupBox_Init);

    addClass('TGroupBox', 'TCustomGroupBox');
    addGlobalFunc('procedure TGroupBox.Init(AOwner: TComponent); override', @_LapeGroupBox_Init);

    addClass('TMemoScrollbar', 'TControlScrollBar');
    addClass('TCustomMemo', 'TCustomEdit');
    addGlobalFunc('procedure TCustomMemo.Init(AOwner: TComponent); override', @_LapeCustomMemo_Init);
    addGlobalFunc('procedure TCustomMemo.Append(const Value: String);', @_LapeCustomMemo_Append);
    addClassVar('TCustomMemo', 'Lines', 'TStrings', @_LapeCustomMemo_Lines_Read, @_LapeCustomMemo_Lines_Write);
    addClassVar('TCustomMemo', 'HorzScrollBar', 'TMemoScrollBar', @_LapeCustomMemo_HorzScrollBar_Read, @_LapeCustomMemo_HorzScrollBar_Write);
    addClassVar('TCustomMemo', 'VertScrollBar', 'TMemoScrollBar', @_LapeCustomMemo_VertScrollBar_Read, @_LapeCustomMemo_VertScrollBar_Write);
    addClassVar('TCustomMemo', 'ScrollBars', 'TScrollStyle', @_LapeCustomMemo_ScrollBars_Read, @_LapeCustomMemo_ScrollBars_Write);
    addClassVar('TCustomMemo', 'WantReturns', 'Boolean', @_LapeCustomMemo_WantReturns_Read, @_LapeCustomMemo_WantReturns_Write);
    addClassVar('TCustomMemo', 'WantTabs', 'Boolean', @_LapeCustomMemo_WantTabs_Read, @_LapeCustomMemo_WantTabs_Write);
    addClassVar('TCustomMemo', 'WordWrap', 'Boolean', @_LapeCustomMemo_WordWrap_Read, @_LapeCustomMemo_WordWrap_Write);

    addClass('TMemo', 'TCustomMemo');
    addGlobalFunc('procedure TMemo.Init(AOwner: TComponent); override', @_LapeMemo_Init);

    addClass('TButtonControl', 'TWinControl');
    addGlobalFunc('procedure TButtonControl.Init(TheOwner: TComponent); override', @_LapeButtonControl_Init);

    addClass('TCustomButton', 'TButtonControl');
    addGlobalFunc('procedure TCustomButton.Init(TheOwner: TComponent); override', @_LapeCustomButton_Init);
    addClassVar('TCustomButton', 'Active', 'Boolean', @_LapeCustomButton_Active_Read);
    addClassVar('TCustomButton', 'Default', 'Boolean', @_LapeCustomButton_Default_Read, @_LapeCustomButton_Default_Write);
    addClassVar('TCustomButton', 'Cancel', 'Boolean', @_LapeCustomButton_Cancel_Read, @_LapeCustomButton_Cancel_Write);

    addClass('TButton', 'TCustomButton');
    addGlobalFunc('procedure TButton.Init(TheOwner: TComponent); override', @_LapeButton_Init);

    addClass('TCustomCheckBox', 'TButtonControl');
    addGlobalFunc('procedure TCustomCheckBox.Init(TheOwner: TComponent); override', @_LapeCustomCheckBox_Init);
    addClassVar('TCustomCheckBox', 'AllowGrayed', 'Boolean', @_LapeCustomCheckBox_AllowGrayed_Read, @_LapeCustomCheckBox_AllowGrayed_Write);
    addClassVar('TCustomCheckBox', 'State', 'TCheckBoxState', @_LapeCustomCheckBox_State_Read, @_LapeCustomCheckBox_State_Write);
    addClassVar('TCustomCheckBox', 'OnChange', 'TNotifyEvent', @_LapeCustomCheckBox_OnChange_Read, @_LapeCustomCheckBox_OnChange_Write);

    addClass('TCheckBox', 'TCustomCheckBox');
    addGlobalFunc('procedure TCheckBox.Init(TheOwner: TComponent); override', @_LapeCheckBox_Init);

    addClass('TCustomLabel', 'TGraphicControl');
    addGlobalFunc('procedure TCustomLabel.Init(TheOwner: TComponent); override', @_LapeCustomLabel_Init);
    addGlobalFunc('function TCustomLabel.AdjustFontForOptimalFill: Boolean;', @_LapeCustomLabel_AdjustFontForOptimalFill);

    addClass('TLabel', 'TCustomLabel');
    addClassVar('TLabel', 'Alignment', 'TAlignment', @_LapeLabel_Alignment_Read, @_LapeLabel_Alignment_Write);
    addClassVar('TLabel', 'Layout', 'TTextLayout', @_LapeLabel_Layout_Read, @_LapeLabel_Layout_Write);
    addClassVar('TLabel', 'Transparent', 'Boolean', @_LapeLabel_Transparent_Read, @_LapeLabel_Transparent_Write);
    addClassVar('TLabel', 'WordWrap', 'Boolean', @_LapeLabel_WordWrap_Read, @_LapeLabel_WordWrap_Write);
    addClassVar('TLabel', 'OptimalFill', 'Boolean', @_LapeLabel_OptimalFill_Read, @_LapeLabel_OptimalFill_Write);
    addGlobalFunc('procedure TLabel.Init(TheOwner: TComponent); override', @_LapeLabel_Init);

    addClass('TCustomSpeedButton', 'TGraphicControl');
    addGlobalFunc('function TCustomSpeedButton.FindDownButton: TCustomSpeedButton;', @_LapeCustomSpeedButton_FindDownButton);
    addClassVar('TCustomSpeedButton', 'AllowAllUp', 'Boolean', @_LapeCustomSpeedButton_AllowAllUp_Read, @_LapeCustomSpeedButton_AllowAllUp_Write);
    addClassVar('TCustomSpeedButton', 'Down', 'Boolean', @_LapeCustomSpeedButton_Down_Read, @_LapeCustomSpeedButton_Down_Write);
    addClassVar('TCustomSpeedButton', 'Flat', 'Boolean', @_LapeCustomSpeedButton_Flat_Read, @_LapeCustomSpeedButton_Flat_Write);
    addClassVar('TCustomSpeedButton', 'Glyph', 'TBitmap', @_LapeCustomSpeedButton_Glyph_Read, @_LapeCustomSpeedButton_Glyph_Write);
    addClassVar('TCustomSpeedButton', 'Layout', 'TButtonLayout', @_LapeCustomSpeedButton_Layout_Read, @_LapeCustomSpeedButton_Layout_Write);
    addClassVar('TCustomSpeedButton', 'Margin', 'Integer', @_LapeCustomSpeedButton_Margin_Read, @_LapeCustomSpeedButton_Margin_Write);
    addClassVar('TCustomSpeedButton', 'ShowCaption', 'Boolean', @_LapeCustomSpeedButton_ShowCaption_Read, @_LapeCustomSpeedButton_ShowCaption_Write);
    addClassVar('TCustomSpeedButton', 'Spacing', 'Integer', @_LapeCustomSpeedButton_Spacing_Read, @_LapeCustomSpeedButton_Spacing_Write);
    addClassVar('TCustomSpeedButton', 'Transparent', 'Boolean', @_LapeCustomSpeedButton_Transparent_Read, @_LapeCustomSpeedButton_Transparent_Write);
    addGlobalFunc('procedure TCustomSpeedButton.Init(AOwner: TComponent); override', @_LapeCustomSpeedButton_Init);

    addClass('TSpeedButton', 'TCustomSpeedButton');
    addGlobalFunc('procedure TSpeedButton.Init(AOwner: TComponent); override', @_LapeSpeedButton_Init);

    addClass('TRadioButton', 'TCustomCheckBox');
    addGlobalFunc('procedure TRadioButton.Init(AOwner: TComponent); override', @_LapeRadioButton_Init);
  end;
end;

end.

