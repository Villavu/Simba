unit simbascript.import_lcldialogs;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_LCLDialogs(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  stdctrls, forms, comctrls, dialogs, lclclasses;

type
  PHandle = ^THandle;
  PNotifyEvent = ^TNotifyEvent;
  PObject = ^TObject;
  PComponent = ^TComponent;
  PCommonDialog = ^TCommonDialog;
  PCloseQueryEvent = ^TCloseQueryEvent;
  PLCLComponent = ^TLCLComponent;
  PFileDialog = ^TFileDialog;
  POpenDialog = ^TOpenDialog;
  POpenOption = ^TOpenOption;
  POpenOptions = ^TOpenOptions;
  PColorDialog = ^TColorDialog;
  TColor = Integer;
  PColor = ^TColor;

//procedure RemoveAllHandlersOfObject(AnObject: TObject); virtual;
procedure Lape_TLCLComponent_RemoveAllHandlersOfObject(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLCLComponent(Params^[0])^.RemoveAllHandlersOfObject(PObject(Params^[1])^);
end;

//procedure IncLCLRefCount;
procedure Lape_TLCLComponent_IncLCLRefCount(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLCLComponent(Params^[0])^.IncLCLRefCount();
end;

//procedure DecLCLRefCount;
procedure Lape_TLCLComponent_DecLCLRefCount(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLCLComponent(Params^[0])^.DecLCLRefCount();
end;

//Read: property LCLRefCount: integer read FLCLRefCount;
procedure Lape_TLCLComponent_LCLRefCount_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PLCLComponent(Params^[0])^.LCLRefCount;
end;

//constructor Create();
procedure Lape_TLCLComponent_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
 PLCLComponent(Params^[0])^ := TLCLComponent.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TLCLComponent_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLCLComponent(Params^[0])^.Free();
end;

procedure Lape_Import_TLCLComponent(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TLCLComponent', 'TComponent');

    addGlobalFunc('procedure TLCLComponent.RemoveAllHandlersOfObject(AnObject: TObject); constref;', @Lape_TLCLComponent_RemoveAllHandlersOfObject);
    addGlobalFunc('procedure TLCLComponent.IncLCLRefCount(); constref;', @Lape_TLCLComponent_IncLCLRefCount);
    addGlobalFunc('procedure TLCLComponent.DecLCLRefCount(); constref;', @Lape_TLCLComponent_DecLCLRefCount);
    addClassVar('TLCLComponent', 'LCLRefCount', 'integer', @Lape_TLCLComponent_LCLRefCount_Read, nil);
    addGlobalFunc('procedure TLCLComponent.Init(TheOwner: TComponent); override;', @Lape_TLCLComponent_Init);
    //addGlobalFunc('procedure TLCLComponent.Free(); constref;', @Lape_TLCLComponent_Free);
  end;
end;


//Read: FCompStyle : LongInt;
Procedure Lape_TCommonDialog_FCompStyle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongInt(Result)^ := PCommonDialog(Params^[0])^.FCompStyle;
end;

//Write: FCompStyle : LongInt;
procedure Lape_TCommonDialog_FCompStyle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.FCompStyle := PLongInt(Params^[1])^;
end;

//function Execute: boolean; virtual;
procedure Lape_TCommonDialog_Execute(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCommonDialog(Params^[0])^.Execute();
end;

//Read: property Handle: THandle read FHandle write SetHandle;
procedure Lape_TCommonDialog_Handle_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PHandle(Result)^ := PCommonDialog(Params^[0])^.Handle;
end;

//Write: property Handle: THandle read FHandle write SetHandle;
procedure Lape_TCommonDialog_Handle_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.Handle := PHandle(Params^[1])^;
end;

//Read: property UserChoice: integer read FUserChoice write FUserChoice;
procedure Lape_TCommonDialog_UserChoice_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCommonDialog(Params^[0])^.UserChoice;
end;

//Write: property UserChoice: integer read FUserChoice write FUserChoice;
procedure Lape_TCommonDialog_UserChoice_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.UserChoice := Pinteger(Params^[1])^;
end;

//procedure Close; virtual;
procedure Lape_TCommonDialog_Close(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.Close();
End;

//procedure DoShow; virtual;
procedure Lape_TCommonDialog_DoShow(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.DoShow();
end;

//procedure DoCanClose(var CanClose: Boolean); virtual;
procedure Lape_TCommonDialog_DoCanClose(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.DoCanClose(PBoolean(Params^[1])^);
end;

//procedure DoClose; virtual;
procedure Lape_TCommonDialog_DoClose(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.DoClose();
end;

//function HandleAllocated: boolean;
procedure Lape_TCommonDialog_HandleAllocated(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pboolean(Result)^ := PCommonDialog(Params^[0])^.HandleAllocated();
end;

//Read: property OnClose: TNotifyEvent read FOnClose write FOnClose;
procedure Lape_TCommonDialog_OnClose_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCommonDialog(Params^[0])^.OnClose;
end;

//Write: property OnClose: TNotifyEvent read FOnClose write FOnClose;
procedure Lape_TCommonDialog_OnClose_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.OnClose := PNotifyEvent(Params^[1])^;
end;

//Read: property OnCanClose: TCloseQueryEvent read FOnCanClose write FOnCanClose;
procedure Lape_TCommonDialog_OnCanClose_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCloseQueryEvent(Result)^ := PCommonDialog(Params^[0])^.OnCanClose;
end;

//Write: property OnCanClose: TCloseQueryEvent read FOnCanClose write FOnCanClose;
procedure Lape_TCommonDialog_OnCanClose_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.OnCanClose := PCloseQueryEvent(Params^[1])^;
end;

//Read: property OnShow: TNotifyEvent read FOnShow write FOnShow;
procedure Lape_TCommonDialog_OnShow_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PCommonDialog(Params^[0])^.OnShow;
end;

//Write: property OnShow: TNotifyEvent read FOnShow write FOnShow;
procedure Lape_TCommonDialog_OnShow_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

//Read: property Width: integer read FWidth write SetWidth default 0;
procedure Lape_TCommonDialog_Width_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCommonDialog(Params^[0])^.Width;
end;

//Write: property Width: integer read FWidth write SetWidth default 0;
procedure Lape_TCommonDialog_Width_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.Width := Pinteger(Params^[1])^;
end;

//Read: property Height: integer read FHeight write SetHeight default 0;
procedure Lape_TCommonDialog_Height_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Pinteger(Result)^ := PCommonDialog(Params^[0])^.Height;
end;

//Write: property Height: integer read FHeight write SetHeight default 0;
procedure Lape_TCommonDialog_Height_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.Height := Pinteger(Params^[1])^;
end;

//constructor Create();
procedure Lape_TCommonDialog_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^ := TCommonDialog.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TCommonDialog_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PCommonDialog(Params^[0])^.Free();
end;

procedure Lape_Import_TCommonDialog(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TCommonDialog', 'TLCLComponent');

    addClassVar('TCommonDialog', 'FCompStyle', 'LongInt', @Lape_TCommonDialog_FCompStyle_Read, @Lape_TCommonDialog_FCompStyle_Write);
    addGlobalFunc('function TCommonDialog.Execute(): boolean; constref;', @Lape_TCommonDialog_Execute);
    addClassVar('TCommonDialog', 'Handle', 'THandle', @Lape_TCommonDialog_Handle_Read, @Lape_TCommonDialog_Handle_Write);
    addClassVar('TCommonDialog', 'UserChoice', 'integer', @Lape_TCommonDialog_UserChoice_Read, @Lape_TCommonDialog_UserChoice_Write);
    addGlobalFunc('procedure TCommonDialog.Close(); constref;', @Lape_TCommonDialog_Close);
    addGlobalFunc('procedure TCommonDialog.DoShow(); constref;', @Lape_TCommonDialog_DoShow);
    addGlobalFunc('procedure TCommonDialog.DoCanClose(var CanClose: Boolean); constref;', @Lape_TCommonDialog_DoCanClose);
    addGlobalFunc('procedure TCommonDialog.DoClose(); constref;', @Lape_TCommonDialog_DoClose);
    addGlobalFunc('function TCommonDialog.HandleAllocated(): boolean; constref;', @Lape_TCommonDialog_HandleAllocated);
    addClassVar('TCommonDialog', 'OnClose', 'TNotifyEvent', @Lape_TCommonDialog_OnClose_Read, @Lape_TCommonDialog_OnClose_Write);
    addClassVar('TCommonDialog', 'OnCanClose', 'TCloseQueryEvent', @Lape_TCommonDialog_OnCanClose_Read, @Lape_TCommonDialog_OnCanClose_Write);
    addClassVar('TCommonDialog', 'OnShow', 'TNotifyEvent', @Lape_TCommonDialog_OnShow_Read, @Lape_TCommonDialog_OnShow_Write);
    addClassVar('TCommonDialog', 'Width', 'integer', @Lape_TCommonDialog_Width_Read, @Lape_TCommonDialog_Width_Write);
    addClassVar('TCommonDialog', 'Height', 'integer', @Lape_TCommonDialog_Height_Read, @Lape_TCommonDialog_Height_Write);
    addGlobalFunc('procedure TCommonDialog.Init(AOwner: TComponent);', @Lape_TCommonDialog_Init);
    //addGlobalFunc('procedure TCommonDialog.Free(); constref;', @Lape_TCommonDialog_Free);
  end;
end;

//procedure DoTypeChange; virtual;
procedure Lape_TFileDialog_DoTypeChange(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.DoTypeChange();
end;

//function Execute: boolean; override;
procedure Lape_TFileDialog_Execute(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
Begin
  Pboolean(Result)^ := PFileDialog(Params^[0])^.Execute();
end;

//Read: property Files: TStrings read FFiles;
procedure Lape_TFileDialog_Files_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PFileDialog(Params^[0])^.Files;
end;

//Read: property HistoryList: TStrings read FHistoryList write SetHistoryList;
procedure Lape_TFileDialog_HistoryList_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PFileDialog(Params^[0])^.HistoryList;
end;

//Write: property HistoryList: TStrings read FHistoryList write SetHistoryList;
procedure Lape_TFileDialog_HistoryList_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.HistoryList := PStrings(Params^[1])^;
end;

//procedure IntfFileTypeChanged(NewFilterIndex: Integer);
procedure Lape_TFileDialog_IntfFileTypeChanged(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.IntfFileTypeChanged(PInteger(Params^[1])^);
end;

//Read: property DefaultExt: string read FDefaultExt write SetDefaultExt;
procedure Lape_TFileDialog_DefaultExt_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PFileDialog(Params^[0])^.DefaultExt;
end;

//Write: property DefaultExt: string read FDefaultExt write SetDefaultExt;
procedure Lape_TFileDialog_DefaultExt_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.DefaultExt := PlpString(Params^[1])^;
end;

//Read: property FileName: String read FFileName write SetFileName;
procedure Lape_TFileDialog_FileName_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PFileDialog(Params^[0])^.FileName;
end;

//Write: property FileName: String read FFileName write SetFileName;
procedure Lape_TFileDialog_FileName_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.FileName := PlpString(Params^[1])^;
end;

//Read: property Filter: String read FFilter write SetFilter;
procedure Lape_TFileDialog_Filter_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PFileDialog(Params^[0])^.Filter;
end;

//Write: property Filter: String read FFilter write SetFilter;
procedure Lape_TFileDialog_Filter_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.Filter := PlpString(Params^[1])^;
end;

//Read: property FilterIndex: Integer read GetFilterIndex write SetFilterIndex default 1;
procedure Lape_TFileDialog_FilterIndex_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PFileDialog(Params^[0])^.FilterIndex;
end;

//Write: property FilterIndex: Integer read GetFilterIndex write SetFilterIndex default 1;
procedure Lape_TFileDialog_FilterIndex_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.FilterIndex := PInteger(Params^[1])^;
end;

//Read: property InitialDir: string read FInitialDir write FInitialDir;
procedure Lape_TFileDialog_InitialDir_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PlpString(Result)^ := PFileDialog(Params^[0])^.InitialDir;
end;

//Write: property InitialDir: string read FInitialDir write FInitialDir;
procedure Lape_TFileDialog_InitialDir_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.InitialDir := PlpString(Params^[1])^;
end;

//Read: property OnHelpClicked: TNotifyEvent read FOnHelpClicked write FOnHelpClicked;
procedure Lape_TFileDialog_OnHelpClicked_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PFileDialog(Params^[0])^.OnHelpClicked;
end;

//Write: property OnHelpClicked: TNotifyEvent read FOnHelpClicked write FOnHelpClicked;
procedure Lape_TFileDialog_OnHelpClicked_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.OnHelpClicked := PNotifyEvent(Params^[1])^;
end;

//Read: property OnTypeChange: TNotifyEvent read FOnTypeChange write FOnTypeChange;
procedure Lape_TFileDialog_OnTypeChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := PFileDialog(Params^[0])^.OnTypeChange;
end;

//Write: property OnTypeChange: TNotifyEvent read FOnTypeChange write FOnTypeChange;
procedure Lape_TFileDialog_OnTypeChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.OnTypeChange := PNotifyEvent(Params^[1])^;
end;

//constructor Create();
procedure Lape_TFileDialog_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^ := TFileDialog.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TFileDialog_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PFileDialog(Params^[0])^.Free();
end;

procedure Lape_Import_TFileDialog(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    AddClass('TFileDialog', 'TCommonDialog');

    addGlobalFunc('procedure TFileDialog.DoTypeChange(); constref;', @Lape_TFileDialog_DoTypeChange);
    //addGlobalFunc('function TFileDialog.Execute(): boolean; constref;', @Lape_TFileDialog_Execute);
    addClassVar('TFileDialog', 'Files', 'TStrings', @Lape_TFileDialog_Files_Read, nil);
    addClassVar('TFileDialog', 'HistoryList', 'TStrings', @Lape_TFileDialog_HistoryList_Read, @Lape_TFileDialog_HistoryList_Write);
    addGlobalFunc('procedure TFileDialog.IntfFileTypeChanged(NewFilterIndex: Integer); constref;', @Lape_TFileDialog_IntfFileTypeChanged);
    addClassVar('TFileDialog', 'DefaultExt', 'string', @Lape_TFileDialog_DefaultExt_Read, @Lape_TFileDialog_DefaultExt_Write);
    addClassVar('TFileDialog', 'FileName', 'String', @Lape_TFileDialog_FileName_Read, @Lape_TFileDialog_FileName_Write);
    addClassVar('TFileDialog', 'Filter', 'String', @Lape_TFileDialog_Filter_Read, @Lape_TFileDialog_Filter_Write);
    addClassVar('TFileDialog', 'FilterIndex', 'Integer', @Lape_TFileDialog_FilterIndex_Read, @Lape_TFileDialog_FilterIndex_Write);
    addClassVar('TFileDialog', 'InitialDir', 'string', @Lape_TFileDialog_InitialDir_Read, @Lape_TFileDialog_InitialDir_Write);
    addClassVar('TFileDialog', 'OnHelpClicked', 'TNotifyEvent', @Lape_TFileDialog_OnHelpClicked_Read, @Lape_TFileDialog_OnHelpClicked_Write);
    addClassVar('TFileDialog', 'OnTypeChange', 'TNotifyEvent', @Lape_TFileDialog_OnTypeChange_Read, @Lape_TFileDialog_OnTypeChange_Write);
    addGlobalFunc('procedure TFileDialog.Init(AOwner: TComponent); override;', @Lape_TFileDialog_Init);
    //addGlobalFunc('procedure TFileDialog.Free(); constref;', @Lape_TFileDialog_Free);
  end;
end;

//procedure DoFolderChange; virtual;
procedure Lape_TOpenDialog_DoFolderChange(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.DoFolderChange();
end;

//procedure DoSelectionChange; virtual;
procedure Lape_TOpenDialog_DoSelectionChange(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.DoSelectionChange();
end;

//procedure IntfSetOption(const AOption: TOpenOption; const AValue: Boolean);
procedure Lape_TOpenDialog_IntfSetOption(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.IntfSetOption(POpenOption(Params^[1])^, PBoolean(Params^[2])^);
end;

//Read: property Options: TOpenOptions read FOptions write FOptions default DefaultOpenDialogOptions;
procedure Lape_TOpenDialog_Options_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POpenOptions(Result)^ := POpenDialog(Params^[0])^.Options;
end;

//Write: property Options: TOpenOptions read FOptions write FOptions default DefaultOpenDialogOptions;
procedure Lape_TOpenDialog_Options_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.Options := POpenOptions(Params^[1])^;
end;

//Read: property OnFolderChange: TNotifyEvent read FOnFolderChange write FOnFolderChange;
procedure Lape_TOpenDialog_OnFolderChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
Begin
  PNotifyEvent(Result)^ := POpenDialog(Params^[0])^.OnFolderChange;
end;

//Write: property OnFolderChange: TNotifyEvent read FOnFolderChange write FOnFolderChange;
procedure Lape_TOpenDialog_OnFolderChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.OnFolderChange := PNotifyEvent(Params^[1])^;
end;

//Read: property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
procedure Lape_TOpenDialog_OnSelectionChange_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PNotifyEvent(Result)^ := POpenDialog(Params^[0])^.OnSelectionChange;
end;

//Write: property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
procedure Lape_TOpenDialog_OnSelectionChange_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.OnSelectionChange := PNotifyEvent(Params^[1])^;
end;

//constructor Create();
procedure Lape_TOpenDialog_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^ := TOpenDialog.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TOpenDialog_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POpenDialog(Params^[0])^.Free();
end;

procedure Lape_Import_TOpenDialog(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    addClass('TOpenDialog', 'TFileDialog');

    addGlobalFunc('procedure TOpenDialog.DoFolderChange(); constref;', @Lape_TOpenDialog_DoFolderChange);
    addGlobalFunc('procedure TOpenDialog.DoSelectionChange(); constref;', @Lape_TOpenDialog_DoSelectionChange);
    addClassVar('TOpenDialog', 'Options', 'TOpenOptions', @Lape_TOpenDialog_Options_Read, @Lape_TOpenDialog_Options_Write);
    addClassVar('TOpenDialog', 'OnFolderChange', 'TNotifyEvent', @Lape_TOpenDialog_OnFolderChange_Read, @Lape_TOpenDialog_OnFolderChange_Write);
    addClassVar('TOpenDialog', 'OnSelectionChange', 'TNotifyEvent', @Lape_TOpenDialog_OnSelectionChange_Read, @Lape_TOpenDialog_OnSelectionChange_Write);
    addGlobalFunc('procedure TOpenDialog.Init(TheOwner: TComponent); override;', @Lape_TOpenDialog_Init);
    //addGlobalFunc('procedure TOpenDialog.Free(); constref;', @Lape_TOpenDialog_Free);
  end;
end;

//Read: property Color: TColor read FColor write FColor;
procedure Lape_TColorDialog_Color_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColor(Result)^ := PColorDialog(Params^[0])^.Color;
end;

//Write: property Color: TColor read FColor write FColor;
procedure Lape_TColorDialog_Color_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColorDialog(Params^[0])^.Color := PColor(Params^[1])^;
end;

//Read: property CustomColors: TStrings read FCustomColors write SetCustomColors;
procedure Lape_TColorDialog_CustomColors_Read(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PStrings(Result)^ := PColorDialog(Params^[0])^.CustomColors;
end;

//Write: property CustomColors: TStrings read FCustomColors write SetCustomColors;
procedure Lape_TColorDialog_CustomColors_Write(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColorDialog(Params^[0])^.CustomColors := PStrings(Params^[1])^;
end;

//constructor Create();
procedure Lape_TColorDialog_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColorDialog(Params^[0])^ := TColorDialog.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure Lape_TColorDialog_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PColorDialog(Params^[0])^.Free();
end;

procedure Lape_Import_TColorDialog(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
   with Compiler do
   begin
     addClass('TColorDialog', 'TCommonDialog');

     addClassVar('TColorDialog', 'Color', 'TColor', @Lape_TColorDialog_Color_Read, @Lape_TColorDialog_Color_Write);
     addClassVar('TColorDialog', 'CustomColors', 'TStrings', @Lape_TColorDialog_CustomColors_Read, @Lape_TColorDialog_CustomColors_Write);
     addGlobalFunc('procedure TColorDialog.Init(TheOwner: TComponent); override;', @Lape_TColorDialog_Init);
     //addGlobalFunc('procedure TColorDialog.Free(); constref;', @Lape_TColorDialog_Free);
   end;
end;

procedure Lape_Import_LCLDialogs(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
   begin
     addGlobalType('(ofReadOnly, ofOverwritePrompt,ofHideReadOnly,ofNoChangeDir,ofShowHelp,ofNoValidate,ofAllowMultiSelect,ofExtensionDifferent,ofPathMustExist,ofFileMustExist,ofCreatePrompt,ofShareAware,ofNoReadOnlyReturn,ofNoTestFileCreate,ofNoNetworkButton,ofNoLongNames,ofOldStyleDialog,ofNoDereferenceLinks,ofEnableIncludeNotify,ofEnableSizing,ofDontAddToRecent,ofForceShowHidden,ofViewDetail,ofAutoPreview)','TOpenOption');
     addGlobalType('set of TOpenOption', 'TOpenOptions');
   end;
  Lape_Import_TLCLComponent(Compiler);
  Lape_Import_TCommonDialog(Compiler);
  Lape_Import_TFileDialog(Compiler);
  Lape_Import_TOpenDialog(Compiler);
  Lape_Import_TColorDialog(Compiler);
end;

end.
