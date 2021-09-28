type
  PAlignment = ^TAlignment;
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
  PCustomScrollBar = ^TCustomScrollBar;
  PCustomSpeedButton = ^TCustomSpeedButton;
  PDrawItemEvent = ^TDrawItemEvent;
  PEdit = ^TEdit;
  PGroupBox = ^TGroupBox;
  PLabel = ^TLabel;
  PListBox = ^TCustomListBox;
  PListBoxStyle = ^TListBoxStyle;
  PMemo = ^TMemo;
  PMemoScrollbar = ^TMemoScrollbar;
  PRadioButton = ^TRadioButton;
  PScrollBar = ^TScrollBar;
  PScrollEvent = ^TScrollEvent;
  PScrollStyle = ^TScrollStyle;
  PSpeedButton = ^TSpeedButton;

procedure _LapeCustomScrollBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^ := TCustomScrollBar.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomScrollBar_SetParams(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.SetParams(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^, PInteger(Params^[4])^);
end;

procedure _LapeCustomScrollBar_SetParamsEx(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.SetParams(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeCustomScrollBar_Kind_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollBarKind(Result)^ := PCustomScrollBar(Params^[0])^.Kind;
end;

procedure _LapeCustomScrollBar_Kind_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.Kind := PScrollBarKind(Params^[1])^;
end;

procedure _LapeCustomScrollBar_LargeChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomScrollBar(Params^[0])^.LargeChange;
end;

procedure _LapeCustomScrollBar_LargeChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.LargeChange := Pinteger(Params^[1])^;
end;

procedure _LapeCustomScrollBar_Max_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.Max;
end;

procedure _LapeCustomScrollBar_Max_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.Max := PInteger(Params^[1])^;
end;

procedure _LapeCustomScrollBar_Min_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.Min;
end;

procedure _LapeCustomScrollBar_Min_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.Min := PInteger(Params^[1])^;
end;

procedure _LapeCustomScrollBar_PageSize_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.PageSize;
end;

procedure _LapeCustomScrollBar_PageSize_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.PageSize := PInteger(Params^[1])^;
end;

procedure _LapeCustomScrollBar_Position_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomScrollBar(Params^[0])^.Position;
end;

procedure _LapeCustomScrollBar_Position_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.Position := PInteger(Params^[1])^;
end;

procedure _LapeCustomScrollBar_SmallChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomScrollBar(Params^[0])^.SmallChange;
end;

procedure _LapeCustomScrollBar_SmallChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.SmallChange := Pinteger(Params^[1])^;
end;

procedure _LapeCustomScrollBar_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomScrollBar(Params^[0])^.OnChange;
end;

procedure _LapeCustomScrollBar_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomScrollBar_OnScroll_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollEvent(Result)^ := PCustomScrollBar(Params^[0])^.OnScroll;
end;

procedure _LapeCustomScrollBar_OnScroll_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.OnScroll := PScrollEvent(Params^[1])^;
end;

procedure _LapeCustomScrollBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomScrollBar(Params^[0])^.Free();
end;

procedure _LapeScrollBar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollBar(Params^[0])^ := TScrollBar.Create(PComponent(Params^[1])^);
end;

procedure _LapeScrollBar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollBar(Params^[0])^.Free();
end;

procedure _LapeCustomComboBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^ := TCustomComboBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomComboBox_IntfGetItems(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.IntfGetItems();
end;

procedure _LapeCustomComboBox_AddItem(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AddItem(PlpString(Params^[1])^, PObject(Params^[2])^);
end;

procedure _LapeCustomComboBox_AddHistoryItem(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AddHistoryItem(PlpString(Params^[1])^, Pinteger(Params^[2])^, Pboolean(Params^[3])^, Pboolean(Params^[4])^);
end;

procedure _LapeCustomComboBox_AddHistoryItemEx(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AddHistoryItem(PlpString(Params^[1])^, PObject(Params^[2])^, Pinteger(Params^[3])^, Pboolean(Params^[4])^, Pboolean(Params^[5])^);
end;

procedure _LapeCustomComboBox_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.Clear();
end;

procedure _LapeCustomComboBox_ClearSelection(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.ClearSelection();
end;

procedure _LapeCustomComboBox_DroppedDown_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.DroppedDown;
end;

procedure _LapeCustomComboBox_DroppedDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.DroppedDown := PBoolean(Params^[1])^;
end;

procedure _LapeCustomComboBox_SelectAll(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.SelectAll();
end;

procedure _LapeCustomComboBox_AutoComplete_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomComboBox(Params^[0])^.AutoComplete;
end;

procedure _LapeCustomComboBox_AutoComplete_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AutoComplete := Pboolean(Params^[1])^;
end;

procedure _LapeCustomComboBox_AutoDropDown_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoDropDown;
end;

procedure _LapeCustomComboBox_AutoDropDown_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AutoDropDown := PBoolean(Params^[1])^;
end;

procedure _LapeCustomComboBox_AutoSelect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoSelect;
end;

procedure _LapeCustomComboBox_AutoSelect_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AutoSelect := PBoolean(Params^[1])^;
end;

procedure _LapeCustomComboBox_AutoSelected_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.AutoSelected;
end;

procedure _LapeCustomComboBox_AutoSelected_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.AutoSelected := PBoolean(Params^[1])^;
end;

procedure _LapeCustomComboBox_ArrowKeysTraverseList_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.ArrowKeysTraverseList;
end;

procedure _LapeCustomComboBox_ArrowKeysTraverseList_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.ArrowKeysTraverseList := PBoolean(Params^[1])^;
end;

procedure _LapeCustomComboBox_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PCustomComboBox(Params^[0])^.Canvas;
end;

procedure _LapeCustomComboBox_DropDownCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomComboBox(Params^[0])^.DropDownCount;
end;

procedure _LapeCustomComboBox_DropDownCount_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.DropDownCount := PInteger(Params^[1])^;
end;

procedure _LapeCustomComboBox_Items_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PCustomComboBox(Params^[0])^.Items;
end;

procedure _LapeCustomComboBox_Items_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.Items := PStrings(Params^[1])^;
end;

procedure _LapeCustomComboBox_ItemIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomComboBox(Params^[0])^.ItemIndex;
end;

procedure _LapeCustomComboBox_ItemIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.ItemIndex := Pinteger(Params^[1])^;
end;

procedure _LapeCustomComboBox_ReadOnly_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomComboBox(Params^[0])^.ReadOnly;
end;

procedure _LapeCustomComboBox_ReadOnly_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.ReadOnly := PBoolean(Params^[1])^;
end;

procedure _LapeCustomComboBox_SelLength_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomComboBox(Params^[0])^.SelLength;
end;

procedure _LapeCustomComboBox_SelLength_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.SelLength := Pinteger(Params^[1])^;
end;

procedure _LapeCustomComboBox_SelStart_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomComboBox(Params^[0])^.SelStart;
end;

procedure _LapeCustomComboBox_SelStart_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.SelStart := Pinteger(Params^[1])^;
end;

procedure _LapeCustomComboBox_SelText_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomComboBox(Params^[0])^.SelText;
end;

procedure _LapeCustomComboBox_SelText_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.SelText := PlpString(Params^[1])^;
end;

procedure _LapeCustomComboBox_Style_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComboBoxStyle(Result)^ := PCustomComboBox(Params^[0])^.Style;
end;

procedure _LapeCustomComboBox_Style_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.Style := PComboBoxStyle(Params^[1])^;
end;

procedure _LapeCustomComboBox_Text_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomComboBox(Params^[0])^.Text;
end;

procedure _LapeCustomComboBox_Text_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.Text := PlpString(Params^[1])^;
end;

procedure _LapeCustomComboBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomComboBox(Params^[0])^.Free();
end;

procedure _LapeComboBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComboBox(Params^[0])^ := TComboBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomComboBox_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PComboBox(Params^[0])^.OnChange;
end;

procedure _LapeCustomComboBox_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComboBox(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeComboBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComboBox(Params^[0])^.Free();
end;

procedure _LapeCustomListBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^ := TCustomListBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomListBox_AddItem(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.AddItem(PlpString(Params^[1])^, PObject(Params^[2])^);
end;

procedure _LapeCustomListBox_Click(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Click();
end;

procedure _LapeCustomListBox_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Clear();
end;

procedure _LapeCustomListBox_ClearSelection(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ClearSelection();
end;

procedure _LapeCustomListBox_GetIndexAtXY(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.GetIndexAtXY(Pinteger(Params^[1])^, Pinteger(Params^[2])^);
end;

procedure _LapeCustomListBox_GetIndexAtY(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.GetIndexAtY(Pinteger(Params^[1])^);
end;

procedure _LapeCustomListBox_GetSelectedText(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomListBox(Params^[0])^.GetSelectedText();
end;

procedure _LapeCustomListBox_ItemAtPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.ItemAtPos(PPoint(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeCustomListBox_ItemRect(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRect(Result)^ := PCustomListBox(Params^[0])^.ItemRect(PInteger(Params^[1])^);
end;

procedure _LapeCustomListBox_ItemVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ItemVisible(PInteger(Params^[1])^);
end;

procedure _LapeCustomListBox_ItemFullyVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ItemFullyVisible(PInteger(Params^[1])^);
end;

procedure _LapeCustomListBox_LockSelectionChange(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.LockSelectionChange();
end;

procedure _LapeCustomListBox_MakeCurrentVisible(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.MakeCurrentVisible();
end;

procedure _LapeCustomListBox_MeasureItem(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.MeasureItem(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeCustomListBox_SelectAll(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.SelectAll();
end;

procedure _LapeCustomListBox_UnlockSelectionChange(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.UnlockSelectionChange();
end;

procedure _LapeCustomListBox_Canvas_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCanvas(Result)^ := PCustomListBox(Params^[0])^.Canvas;
end;

procedure _LapeCustomListBox_ClickOnSelChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ClickOnSelChange;
end;

procedure _LapeCustomListBox_ClickOnSelChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ClickOnSelChange := Pboolean(Params^[1])^;
end;

procedure _LapeCustomListBox_Columns_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.Columns;
end;

procedure _LapeCustomListBox_Columns_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Columns := PInteger(Params^[1])^;
end;

procedure _LapeCustomListBox_Count_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.Count;
end;

procedure _LapeCustomListBox_ExtendedSelect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.ExtendedSelect;
end;

procedure _LapeCustomListBox_ExtendedSelect_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ExtendedSelect := Pboolean(Params^[1])^;
end;

procedure _LapeCustomListBox_ItemHeight_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.ItemHeight;
end;

procedure _LapeCustomListBox_ItemHeight_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ItemHeight := PInteger(Params^[1])^;
end;

procedure _LapeCustomListBox_ItemIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.ItemIndex;
end;

procedure _LapeCustomListBox_ItemIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ItemIndex := Pinteger(Params^[1])^;
end;

procedure _LapeCustomListBox_Items_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PCustomListBox(Params^[0])^.Items;
end;

procedure _LapeCustomListBox_Items_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Items := PStrings(Params^[1])^;
end;

procedure _LapeCustomListBox_MultiSelect_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.MultiSelect;
end;

procedure _LapeCustomListBox_MultiSelect_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.MultiSelect := Pboolean(Params^[1])^;
end;

procedure _LapeCustomListBox_ScrollWidth_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.ScrollWidth;
end;

procedure _LapeCustomListBox_ScrollWidth_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.ScrollWidth := PInteger(Params^[1])^;
end;

procedure _LapeCustomListBox_SelCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomListBox(Params^[0])^.SelCount;
end;

procedure _LapeCustomListBox_Sorted_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomListBox(Params^[0])^.Sorted;
end;

procedure _LapeCustomListBox_Sorted_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Sorted := Pboolean(Params^[1])^;
end;

procedure _LapeCustomListBox_TopIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomListBox(Params^[0])^.TopIndex;
end;

procedure _LapeCustomListBox_TopIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.TopIndex := PInteger(Params^[1])^;
end;

procedure _LapeCustomListBox_Style_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PListBoxStyle(Result)^ := PCustomListBox(Params^[0])^.Style;
end;

procedure _LapeCustomListBox_Style_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Style := PListBoxStyle(Params^[1])^;
end;

procedure _LapeCustomListBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.Free();
end;

procedure _LapeCustomListBox_OnDrawItem_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomListBox(Params^[0])^.OnDrawItem := PDrawItemEvent(Params^[1])^;
end;

procedure _LapeListBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PListBox(Params^[0])^ := TListBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeListBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PListBox(Params^[0])^.Free();
end;

procedure _LapeCustomEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^ := TCustomEdit.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomEdit_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.Clear();
end;

procedure _LapeCustomEdit_SelectAll(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.SelectAll();
end;

procedure _LapeCustomEdit_ClearSelection(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.ClearSelection();
end;

procedure _LapeCustomEdit_CopyToClipboard(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.CopyToClipboard();
end;

procedure _LapeCustomEdit_CutToClipboard(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.CutToClipboard();
end;

procedure _LapeCustomEdit_PasteFromClipboard(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.PasteFromClipboard();
end;

procedure _LapeCustomEdit_Undo(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.Undo();
end;

procedure _LapeCustomEdit_CanUndo_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.CanUndo;
end;

procedure _LapeCustomEdit_CaretPos_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PPoint(Result)^ := PCustomEdit(Params^[0])^.CaretPos;
end;

procedure _LapeCustomEdit_CaretPos_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.CaretPos := PPoint(Params^[1])^;
end;

procedure _LapeCustomEdit_HideSelection_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.HideSelection;
end;

procedure _LapeCustomEdit_HideSelection_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.HideSelection := PBoolean(Params^[1])^;
end;

procedure _LapeCustomEdit_MaxLength_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomEdit(Params^[0])^.MaxLength;
end;

procedure _LapeCustomEdit_MaxLength_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.MaxLength := PInteger(Params^[1])^;
end;

procedure _LapeCustomEdit_Modified_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.Modified;
end;

procedure _LapeCustomEdit_Modified_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.Modified := PBoolean(Params^[1])^;
end;

procedure _LapeCustomEdit_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomEdit(Params^[0])^.OnChange;
end;

procedure _LapeCustomEdit_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomEdit_PasswordChar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PChar(Result)^ := PCustomEdit(Params^[0])^.PasswordChar;
end;

procedure _LapeCustomEdit_PasswordChar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.PasswordChar := PChar(Params^[1])^;
end;

procedure _LapeCustomEdit_ReadOnly_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomEdit(Params^[0])^.ReadOnly;
end;

procedure _LapeCustomEdit_ReadOnly_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.ReadOnly := PBoolean(Params^[1])^;
end;

procedure _LapeCustomEdit_SelLength_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomEdit(Params^[0])^.SelLength;
end;

procedure _LapeCustomEdit_SelLength_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.SelLength := Pinteger(Params^[1])^;
end;

procedure _LapeCustomEdit_SelStart_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomEdit(Params^[0])^.SelStart;
end;

procedure _LapeCustomEdit_SelStart_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.SelStart := Pinteger(Params^[1])^;
end;

procedure _LapeCustomEdit_SelText_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomEdit(Params^[0])^.SelText;
end;

procedure _LapeCustomEdit_SelText_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.SelText := PlpString(Params^[1])^;
end;

procedure _LapeCustomEdit_Text_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PCustomEdit(Params^[0])^.Text;
end;

procedure _LapeCustomEdit_Text_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.Text := PlpString(Params^[1])^;
end;

procedure _LapeCustomEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomEdit(Params^[0])^.Free();
end;

procedure _LapeEdit_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PEdit(Params^[0])^ := TEdit.Create(PComponent(Params^[1])^);
end;

procedure _LapeEdit_OnEditingDone_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PEdit(Params^[0])^.OnEditingDone;
end;

procedure _LapeEdit_OnEditingDone_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PEdit(Params^[0])^.OnEditingDone := PNotifyEvent(Params^[1])^;
end;

procedure _LapeEdit_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PEdit(Params^[0])^.Free();
end;

procedure _LapeCustomGroupBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomGroupBox(Params^[0])^ := TCustomGroupBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomGroupBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomGroupBox(Params^[0])^.Free();
end;

procedure _LapeGroupBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGroupBox(Params^[0])^ := TGroupBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeGroupBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PGroupBox(Params^[0])^.Free();
end;

procedure _LapeMemoScrollbar_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMemoScrollbar(Params^[0])^ := TMemoScrollbar.Create(PWinControl(Params^[1])^, PScrollBarKind(Params^[2])^);
end;

procedure _LapeMemoScrollbar_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMemoScrollbar(Params^[0])^.Free();
end;

procedure _LapeCustomMemo_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^ := TCustomMemo.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomMemo_Append(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.Append(PlpString(Params^[1])^);
end;

procedure _LapeCustomMemo_Lines_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PCustomMemo(Params^[0])^.Lines;
end;

procedure _LapeCustomMemo_Lines_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.Lines := PStrings(Params^[1])^;
end;

procedure _LapeCustomMemo_HorzScrollBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMemoScrollBar(Result)^ := PCustomMemo(Params^[0])^.HorzScrollBar;
end;

procedure _LapeCustomMemo_HorzScrollBar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.HorzScrollBar := PMemoScrollBar(Params^[1])^;
end;

procedure _LapeCustomMemo_VertScrollBar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMemoScrollBar(Result)^ := PCustomMemo(Params^[0])^.VertScrollBar;
end;

procedure _LapeCustomMemo_VertScrollBar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.VertScrollBar := PMemoScrollBar(Params^[1])^;
end;

procedure _LapeCustomMemo_ScrollBars_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PScrollStyle(Result)^ := PCustomMemo(Params^[0])^.ScrollBars;
end;

procedure _LapeCustomMemo_ScrollBars_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.ScrollBars := PScrollStyle(Params^[1])^;
end;

procedure _LapeCustomMemo_WantReturns_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomMemo(Params^[0])^.WantReturns;
end;

procedure _LapeCustomMemo_WantReturns_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.WantReturns := PBoolean(Params^[1])^;
end;

procedure _LapeCustomMemo_WantTabs_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomMemo(Params^[0])^.WantTabs;
end;

procedure _LapeCustomMemo_WantTabs_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.WantTabs := PBoolean(Params^[1])^;
end;

procedure _LapeCustomMemo_WordWrap_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomMemo(Params^[0])^.WordWrap;
end;

procedure _LapeCustomMemo_WordWrap_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.WordWrap := PBoolean(Params^[1])^;
end;

procedure _LapeCustomMemo_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomMemo(Params^[0])^.Free();
end;

procedure _LapeMemo_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMemo(Params^[0])^ := TMemo.Create(PComponent(Params^[1])^);
end;

procedure _LapeMemo_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMemo(Params^[0])^.Free();
end;

procedure _LapeButtonControl_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PButtonControl(Params^[0])^ := TButtonControl.Create(PComponent(Params^[1])^);
end;

procedure _LapeButtonControl_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PButtonControl(Params^[0])^.Free();
end;

procedure _LapeCustomButton_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^ := TCustomButton.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomButton_ExecuteDefaultAction(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.ExecuteDefaultAction();
end;

procedure _LapeCustomButton_ExecuteCancelAction(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.ExecuteCancelAction();
end;

procedure _LapeCustomButton_ActiveDefaultControlChanged(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.ActiveDefaultControlChanged(PControl(Params^[1])^);
end;

procedure _LapeCustomButton_UpdateRolesForForm(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.UpdateRolesForForm();
end;

procedure _LapeCustomButton_UseRightToLeftAlignment(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomButton(Params^[0])^.UseRightToLeftAlignment();
end;

procedure _LapeCustomButton_Active_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomButton(Params^[0])^.Active;
end;

procedure _LapeCustomButton_Default_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomButton(Params^[0])^.Default;
end;

procedure _LapeCustomButton_Default_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.Default := PBoolean(Params^[1])^;
end;

procedure _LapeCustomButton_Cancel_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomButton(Params^[0])^.Cancel;
end;

procedure _LapeCustomButton_Cancel_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.Cancel := PBoolean(Params^[1])^;
end;

procedure _LapeCustomButton_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomButton(Params^[0])^.Free();
end;

procedure _LapeButton_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PButton(Params^[0])^ := TButton.Create(PComponent(Params^[1])^);
end;

procedure _LapeButton_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PButton(Params^[0])^.Free();
end;

procedure _LapeCustomCheckBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckBox(Params^[0])^ := TCustomCheckBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomCheckBox_AllowGrayed_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomCheckBox(Params^[0])^.AllowGrayed;
end;

procedure _LapeCustomCheckBox_AllowGrayed_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckBox(Params^[0])^.AllowGrayed := PBoolean(Params^[1])^;
end;

procedure _LapeCustomCheckBox_State_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCheckBoxState(Result)^ := PCustomCheckBox(Params^[0])^.State;
end;

procedure _LapeCustomCheckBox_State_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckBox(Params^[0])^.State := PCheckBoxState(Params^[1])^;
end;

procedure _LapeCustomCheckBox_OnChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCustomCheckBox(Params^[0])^.OnChange;
end;

procedure _LapeCustomCheckBox_OnChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckBox(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCustomCheckBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomCheckBox(Params^[0])^.Free();
end;

procedure _LapeCheckBox_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCheckBox(Params^[0])^ := TCheckBox.Create(PComponent(Params^[1])^);
end;

procedure _LapeCheckBox_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCheckBox(Params^[0])^.Free();
end;

procedure _LapeCustomLabel_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomLabel(Params^[0])^ := TCustomLabel.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomLabel_ColorIsStored(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomLabel(Params^[0])^.ColorIsStored();
end;

procedure _LapeCustomLabel_AdjustFontForOptimalFill(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomLabel(Params^[0])^.AdjustFontForOptimalFill();
end;

procedure _LapeCustomLabel_Paint(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomLabel(Params^[0])^.Paint();
end;

procedure _LapeCustomLabel_SetBounds(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomLabel(Params^[0])^.SetBounds(Pinteger(Params^[1])^, Pinteger(Params^[2])^, Pinteger(Params^[3])^, Pinteger(Params^[4])^);
end;

procedure _LapeCustomLabel_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomLabel(Params^[0])^.Free();
end;

procedure _LapeLabel_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PLabel(Params^[0])^ := TLabel.Create(PComponent(Params^[1])^);
end;

procedure _LapeLabel_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PLabel(Params^[0])^.Free();
end;

procedure _LapeCustomSpeedButton_FindDownButton(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Result)^ := PCustomSpeedButton(Params^[0])^.FindDownButton();
end;

procedure _LapeCustomSpeedButton_LoadGlyphFromLazarusResource(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.LoadGlyphFromLazarusResource(PlpString(Params^[1])^);
end;

procedure _LapeCustomSpeedButton_AllowAllUp_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.AllowAllUp;
end;

procedure _LapeCustomSpeedButton_AllowAllUp_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.AllowAllUp := PBoolean(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Down_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.Down;
end;

procedure _LapeCustomSpeedButton_Down_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Down := PBoolean(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Flat_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.Flat;
end;

procedure _LapeCustomSpeedButton_Flat_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Flat := PBoolean(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Glyph_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Result)^ := PCustomSpeedButton(Params^[0])^.Glyph;
end;

procedure _LapeCustomSpeedButton_Glyph_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Glyph := PBitmap(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_GroupIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomSpeedButton(Params^[0])^.GroupIndex;
end;

procedure _LapeCustomSpeedButton_GroupIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.GroupIndex := PInteger(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Layout_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PButtonLayout(Result)^ := PCustomSpeedButton(Params^[0])^.Layout;
end;

procedure _LapeCustomSpeedButton_Layout_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Layout := PButtonLayout(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Margin_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpeedButton(Params^[0])^.Margin;
end;

procedure _LapeCustomSpeedButton_Margin_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Margin := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_NumGlyphs_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PCustomSpeedButton(Params^[0])^.NumGlyphs;
end;

procedure _LapeCustomSpeedButton_NumGlyphs_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.NumGlyphs := PInteger(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_ShowAccelChar_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomSpeedButton(Params^[0])^.ShowAccelChar;
end;

procedure _LapeCustomSpeedButton_ShowAccelChar_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.ShowAccelChar := Pboolean(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_ShowCaption_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCustomSpeedButton(Params^[0])^.ShowCaption;
end;

procedure _LapeCustomSpeedButton_ShowCaption_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.ShowCaption := Pboolean(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Spacing_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCustomSpeedButton(Params^[0])^.Spacing;
end;

procedure _LapeCustomSpeedButton_Spacing_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Spacing := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Transparent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PCustomSpeedButton(Params^[0])^.Transparent;
end;

procedure _LapeCustomSpeedButton_Transparent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Transparent := PBoolean(Params^[1])^;
end;

procedure _LapeCustomSpeedButton_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^ := TCustomSpeedButton.Create(PComponent(Params^[1])^);
end;

procedure _LapeCustomSpeedButton_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCustomSpeedButton(Params^[0])^.Free();
end;

procedure _LapeSpeedButton_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSpeedButton(Params^[0])^ := TSpeedButton.Create(PComponent(Params^[1])^);
end;

procedure _LapeSpeedButton_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PSpeedButton(Params^[0])^.Free();
end;

procedure _LapeRadioButton_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRadioButton(Params^[0])^ := TRadioButton.Create(PComponent(Params^[1])^);
end;

procedure _LapeRadioButton_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PRadioButton(Params^[0])^.Free();
end;

