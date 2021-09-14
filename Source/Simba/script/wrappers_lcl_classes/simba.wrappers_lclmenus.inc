procedure _LapeMenuItem_FCompStyle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PLongInt(Result)^ := PMenuItem(Params^[0])^.FCompStyle;
end;

procedure _LapeMenuItem_FCompStyle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.FCompStyle := PLongInt(Params^[1])^;
end;

procedure _LapeMenuItem_Find(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := PMenuItem(Params^[0])^.Find(PlpString(Params^[1])^);
end;

procedure _LapeMenuItem_GetParentMenu(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Result)^ := PMenuItem(Params^[0])^.GetParentMenu();
end;

procedure _LapeMenuItem_GetIsRightToLeft(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.GetIsRightToLeft();
end;

procedure _LapeMenuItem_HandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.HandleAllocated();
end;

procedure _LapeMenuItem_HasIcon(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.HasIcon();
end;

procedure _LapeMenuItem_InitiateAction(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.InitiateAction();
end;

procedure _LapeMenuItem_IntfDoSelect(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.IntfDoSelect();
end;

procedure _LapeMenuItem_IndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.IndexOf(PMenuItem(Params^[1])^);
end;

procedure _LapeMenuItem_IndexOfCaption(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.IndexOfCaption(PlpString(Params^[1])^);
end;

procedure _LapeMenuItem_VisibleIndexOf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.VisibleIndexOf(PMenuItem(Params^[1])^);
end;

procedure _LapeMenuItem_Add(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Add(PMenuItem(Params^[1])^);
end;

procedure _LapeMenuItem_AddSeparator(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.AddSeparator();
end;

procedure _LapeMenuItem_Click(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Click();
end;

procedure _LapeMenuItem_Delete(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeMenuItem_HandleNeeded(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.HandleNeeded();
end;

procedure _LapeMenuItem_Insert(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Insert(PInteger(Params^[1])^, PMenuItem(Params^[2])^);
end;

procedure _LapeMenuItem_RecreateHandle(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.RecreateHandle();
end;

procedure _LapeMenuItem_Remove(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Remove(PMenuItem(Params^[1])^);
end;

procedure _LapeMenuItem_IsCheckItem(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.IsCheckItem();
end;

procedure _LapeMenuItem_IsLine(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.IsLine();
end;

procedure _LapeMenuItem_IsInMenuBar(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.IsInMenuBar();
end;

procedure _LapeMenuItem_Clear(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Clear();
end;

procedure _LapeMenuItem_HasBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.HasBitmap();
end;

procedure _LapeMenuItem_RemoveHandlerOnDestroy(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.RemoveHandlerOnDestroy(PNotifyEvent(Params^[1])^);
end;

procedure _LapeMenuItem_Count_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.Count;
end;

procedure _LapeMenuItem_Items_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := PMenuItem(Params^[0])^.Items[PInteger(Params^[1])^];
end;

procedure _LapeMenuItem_MenuIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.MenuIndex;
end;

procedure _LapeMenuItem_MenuIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.MenuIndex := PInteger(Params^[1])^;
end;

procedure _LapeMenuItem_Menu_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Result)^ := PMenuItem(Params^[0])^.Menu;
end;

procedure _LapeMenuItem_Parent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := PMenuItem(Params^[0])^.Parent;
end;

procedure _LapeMenuItem_Command_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PWord(Result)^ := PMenuItem(Params^[0])^.Command;
end;

procedure _LapeMenuItem_MenuVisibleIndex(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PMenuItem(Params^[0])^.MenuVisibleIndex();
end;

procedure _LapeMenuItem_AutoCheck_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.AutoCheck;
end;

procedure _LapeMenuItem_AutoCheck_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.AutoCheck := Pboolean(Params^[1])^;
end;

procedure _LapeMenuItem_Default_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.Default;
end;

procedure _LapeMenuItem_Default_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Default := PBoolean(Params^[1])^;
end;

procedure _LapeMenuItem_Bitmap_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBitmap(Result)^ := PMenuItem(Params^[0])^.Bitmap;
end;

procedure _LapeMenuItem_Bitmap_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Bitmap := PBitmap(Params^[1])^;
end;

procedure _LapeMenuItem_GroupIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  pbyte(Result)^ := PMenuItem(Params^[0])^.GroupIndex;
end;

procedure _LapeMenuItem_GroupIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.GroupIndex := pbyte(Params^[1])^;
end;

procedure _LapeMenuItem_Hint_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PTranslateString(Result)^ := PMenuItem(Params^[0])^.Hint;
end;

procedure _LapeMenuItem_Hint_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Hint := PTranslateString(Params^[1])^;
end;

procedure _LapeMenuItem_RadioItem_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.RadioItem;
end;

procedure _LapeMenuItem_RadioItem_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.RadioItem := PBoolean(Params^[1])^;
end;

procedure _LapeMenuItem_RightJustify_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.RightJustify;
end;

procedure _LapeMenuItem_RightJustify_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.RightJustify := Pboolean(Params^[1])^;
end;

procedure _LapeMenuItem_OnClick_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PMenuItem(Params^[0])^.OnClick;
end;

procedure _LapeMenuItem_OnClick_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.OnClick := PNotifyEvent(Params^[1])^;
end;

procedure _LapeMenuItem_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^ := TMenuItem.Create(PComponent(Params^[1])^);
end;

procedure _LapeMenuItem_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Free();
end;

procedure _LapeMenuItem_Caption_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PMenuItem(Params^[0])^.Caption;
end;

procedure _LapeMenuItem_Caption_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Caption := PTranslateString(Params^[1])^;
end;

procedure _LapeMenuItem_Checked_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.Checked;
end;

procedure _LapeMenuItem_Checked_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Checked := PBoolean(Params^[1])^;
end;

procedure _LapeMenuItem_AddEx(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Params^[0])^.Add(PMenuItemArray(Params^[1])^);
end;

procedure _LapeMenuItem_AddMenu(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := TMenuItem.Create(PMenu(Params^[0])^);
  PMenuItem(Result)^.Caption := PLPString(Params^[1])^;
  PMenuItem(Params^[0])^.Add(PMenuItem(Result)^);
end;

procedure _LapeMenu_FCompStyle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PLongInt(Result)^ := PMenu(Params^[0])^.FCompStyle;
end;

procedure _LapeMenu_FCompStyle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.FCompStyle := PLongInt(Params^[1])^;
end;

procedure _LapeMenu_DestroyHandle(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.DestroyHandle();
end;

procedure _LapeMenu_HandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.HandleAllocated();
end;

procedure _LapeMenu_IsRightToLeft(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.IsRightToLeft();
end;

procedure _LapeMenu_UseRightToLeftAlignment(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.UseRightToLeftAlignment();
end;

procedure _LapeMenu_UseRightToLeftReading(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.UseRightToLeftReading();
end;

procedure _LapeMenu_HandleNeeded(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.HandleNeeded();
end;

procedure _LapeMenu_DispatchCommand(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.DispatchCommand(PWord(Params^[1])^);
end;

procedure _LapeMenu_Parent_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PComponent(Result)^ := PMenu(Params^[0])^.Parent;
end;

procedure _LapeMenu_Parent_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.Parent := PComponent(Params^[1])^;
end;

procedure _LapeMenu_ShortcutHandled_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PMenu(Params^[0])^.ShortcutHandled;
end;

procedure _LapeMenu_ShortcutHandled_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.ShortcutHandled := Pboolean(Params^[1])^;
end;

procedure _LapeMenu_ParentBidiMode_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.ParentBidiMode;
end;

procedure _LapeMenu_ParentBidiMode_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.ParentBidiMode := PBoolean(Params^[1])^;
end;

procedure _LapeMenu_Items_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := PMenu(Params^[0])^.Items;
end;

procedure _LapeMenu_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^ := TMenu.Create(PComponent(Params^[1])^);
end;

procedure _LapeMenu_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenu(Params^[0])^.Free();
end;

procedure _LapeMenu_AddMenu(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMenuItem(Result)^ := TMenuItem.Create(PMenu(Params^[0])^);
  PMenuItem(Result)^.Caption := PLPString(Params^[1])^;
  PMenu(Params^[0])^.Items.Add(PMenuItem(Result)^);
end;

procedure _LapeMainMenu_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMainMenu(Params^[0])^ := TMainMenu.Create(PComponent(Params^[1])^);
end;

procedure _LapeMainMenu_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PMainMenu(Params^[0])^.Free();
end;

