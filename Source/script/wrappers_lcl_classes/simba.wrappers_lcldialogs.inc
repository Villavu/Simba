type
  PColorDialog = ^TColorDialog;
  PCommonDialog = ^TCommonDialog;
  PFileDialog = ^TFileDialog;
  PLCLComponent = ^TLCLComponent;
  POpenDialog = ^TOpenDialog;
  POpenOption = ^TOpenOption;
  POpenOptions = ^TOpenOptions;

procedure _LapeLCLComponent_RemoveAllHandlersOfObject(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PLCLComponent(Params^[0])^.RemoveAllHandlersOfObject(PObject(Params^[1])^);
end;

procedure _LapeLCLComponent_IncLCLRefCount(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PLCLComponent(Params^[0])^.IncLCLRefCount();
end;

procedure _LapeLCLComponent_DecLCLRefCount(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PLCLComponent(Params^[0])^.DecLCLRefCount();
end;

procedure _LapeLCLComponent_LCLRefCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PLCLComponent(Params^[0])^.LCLRefCount;
end;

procedure _LapeLCLComponent_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PLCLComponent(Params^[0])^ := TLCLComponent.Create(PComponent(Params^[1])^);
end;

procedure _LapeLCLComponent_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PLCLComponent(Params^[0])^.Free();
end;

procedure _LapeCommonDialog_FCompStyle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PLongInt(Result)^ := PCommonDialog(Params^[0])^.FCompStyle;
end;

procedure _LapeCommonDialog_FCompStyle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.FCompStyle := PLongInt(Params^[1])^;
end;

procedure _LapeCommonDialog_Execute(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCommonDialog(Params^[0])^.Execute();
end;

procedure _LapeCommonDialog_Handle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PCommonDialog(Params^[0])^.Handle;
end;

procedure _LapeCommonDialog_Handle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.Handle := PHandle(Params^[1])^;
end;

procedure _LapeCommonDialog_UserChoice_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCommonDialog(Params^[0])^.UserChoice;
end;

procedure _LapeCommonDialog_UserChoice_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.UserChoice := Pinteger(Params^[1])^;
end;

procedure _LapeCommonDialog_Close(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.Close();
end;

procedure _LapeCommonDialog_DoShow(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.DoShow();
end;

procedure _LapeCommonDialog_DoCanClose(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.DoCanClose(PBoolean(Params^[1])^);
end;

procedure _LapeCommonDialog_DoClose(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.DoClose();
end;

procedure _LapeCommonDialog_HandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCommonDialog(Params^[0])^.HandleAllocated();
end;

procedure _LapeCommonDialog_OnClose_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCommonDialog(Params^[0])^.OnClose;
end;

procedure _LapeCommonDialog_OnClose_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.OnClose := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCommonDialog_OnCanClose_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCloseQueryEvent(Result)^ := PCommonDialog(Params^[0])^.OnCanClose;
end;

procedure _LapeCommonDialog_OnCanClose_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.OnCanClose := PCloseQueryEvent(Params^[1])^;
end;

procedure _LapeCommonDialog_OnShow_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCommonDialog(Params^[0])^.OnShow;
end;

procedure _LapeCommonDialog_OnShow_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

procedure _LapeCommonDialog_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCommonDialog(Params^[0])^.Width;
end;

procedure _LapeCommonDialog_Width_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.Width := Pinteger(Params^[1])^;
end;

procedure _LapeCommonDialog_Height_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCommonDialog(Params^[0])^.Height;
end;

procedure _LapeCommonDialog_Height_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.Height := Pinteger(Params^[1])^;
end;

procedure _LapeCommonDialog_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^ := TCommonDialog.Create(PComponent(Params^[1])^);
end;

procedure _LapeCommonDialog_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.Free();
end;

procedure _LapeFileDialog_DoTypeChange(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.DoTypeChange();
end;

procedure _LapeFileDialog_Execute(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PFileDialog(Params^[0])^.Execute();
end;

procedure _LapeFileDialog_Files_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PFileDialog(Params^[0])^.Files;
end;

procedure _LapeFileDialog_HistoryList_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PFileDialog(Params^[0])^.HistoryList;
end;

procedure _LapeFileDialog_HistoryList_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.HistoryList := PStrings(Params^[1])^;
end;

procedure _LapeFileDialog_IntfFileTypeChanged(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.IntfFileTypeChanged(PInteger(Params^[1])^);
end;

procedure _LapeFileDialog_DefaultExt_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PFileDialog(Params^[0])^.DefaultExt;
end;

procedure _LapeFileDialog_DefaultExt_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.DefaultExt := PString(Params^[1])^;
end;

procedure _LapeFileDialog_FileName_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PFileDialog(Params^[0])^.FileName;
end;

procedure _LapeFileDialog_FileName_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.FileName := PString(Params^[1])^;
end;

procedure _LapeFileDialog_Filter_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PFileDialog(Params^[0])^.Filter;
end;

procedure _LapeFileDialog_Filter_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.Filter := PString(Params^[1])^;
end;

procedure _LapeFileDialog_FilterIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PFileDialog(Params^[0])^.FilterIndex;
end;

procedure _LapeFileDialog_FilterIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.FilterIndex := PInteger(Params^[1])^;
end;

procedure _LapeFileDialog_InitialDir_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PString(Result)^ := PFileDialog(Params^[0])^.InitialDir;
end;

procedure _LapeFileDialog_InitialDir_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.InitialDir := PString(Params^[1])^;
end;

procedure _LapeFileDialog_OnHelpClicked_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PFileDialog(Params^[0])^.OnHelpClicked;
end;

procedure _LapeFileDialog_OnHelpClicked_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.OnHelpClicked := PNotifyEvent(Params^[1])^;
end;

procedure _LapeFileDialog_OnTypeChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PFileDialog(Params^[0])^.OnTypeChange;
end;

procedure _LapeFileDialog_OnTypeChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.OnTypeChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeFileDialog_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^ := TFileDialog.Create(PComponent(Params^[1])^);
end;

procedure _LapeFileDialog_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.Free();
end;

procedure _LapeOpenDialog_DoFolderChange(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.DoFolderChange();
end;

procedure _LapeOpenDialog_DoSelectionChange(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.DoSelectionChange();
end;

procedure _LapeOpenDialog_IntfSetOption(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.IntfSetOption(POpenOption(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeOpenDialog_Options_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenOptions(Result)^ := POpenDialog(Params^[0])^.Options;
end;

procedure _LapeOpenDialog_Options_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.Options := POpenOptions(Params^[1])^;
end;

procedure _LapeOpenDialog_OnFolderChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := POpenDialog(Params^[0])^.OnFolderChange;
end;

procedure _LapeOpenDialog_OnFolderChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.OnFolderChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeOpenDialog_OnSelectionChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := POpenDialog(Params^[0])^.OnSelectionChange;
end;

procedure _LapeOpenDialog_OnSelectionChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.OnSelectionChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeOpenDialog_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^ := TOpenDialog.Create(PComponent(Params^[1])^);
end;

procedure _LapeOpenDialog_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.Free();
end;

procedure _LapeColorDialog_Color_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PColor(Result)^ := PColorDialog(Params^[0])^.Color;
end;

procedure _LapeColorDialog_Color_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PColorDialog(Params^[0])^.Color := PColor(Params^[1])^;
end;

procedure _LapeColorDialog_CustomColors_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PColorDialog(Params^[0])^.CustomColors;
end;

procedure _LapeColorDialog_CustomColors_Write(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PColorDialog(Params^[0])^.CustomColors := PStrings(Params^[1])^;
end;

procedure _LapeColorDialog_Init(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PColorDialog(Params^[0])^ := TColorDialog.Create(PComponent(Params^[1])^);
end;

procedure _LapeColorDialog_Free(const Params: PParamArray); {$IFDEF Lape_CDECL} cdecl;{$ENDIF}
begin
  PColorDialog(Params^[0])^.Free();
end;

