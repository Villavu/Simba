unit uPSR_mml;

interface
uses
  uPSRuntime;

procedure RIRegister_MML(cl: TPSRuntimeClassImporter);

implementation
uses
  SynRegExpr,bitmaps,dtm,mufasatypes,client,ocr,lcltype,classes,finder,files,iomanager,settingssandbox,mmltimer,
  {$IFDEF MSWINDOWS} os_windows {$ENDIF}
  {$IFDEF LINUX} os_linux {$ENDIF};

type
  TRegExp = class(SynRegExpr.TRegExpr);
procedure MBmp_Index_r(self : TMufasaBitmap; var Index : integer);begin;  Index := self.Index; end;
procedure MBmp_Width_r(self : TMufasaBitmap; var Width : integer);begin;  Width := self.Width; end;
procedure MBmp_Height_r(self : TMufasaBitmap; var Height : integer);begin;  Height := self.Height; end;
procedure MBmp_FData_r(self : TMufasaBitmap; var Data: PtrUInt);begin;  Data := PtrUInt(self.FData); end;
procedure MBmp_Name_r(self : TMufasaBitmap; var Name : String);begin;  Name := self.Name; end;
procedure MBmp_Name_w(self : TMufasaBitmap; const Name : String);begin; Self.name := name; end;
procedure MBmp_TransColorSet_r(Self : TMufasaBitmap; var IsSet : boolean); begin IsSet := self.TransparentColorSet; end;
procedure ERegExprCompilerErrorPos_W(Self: ERegExpr; const T: integer); Begin Self.CompilerErrorPos := T; end;
procedure ERegExprCompilerErrorPos_R(Self: ERegExpr; var T: integer);Begin T := Self.CompilerErrorPos; end;
procedure ERegExprErrorCode_W(Self: ERegExpr; const T: integer);Begin Self.ErrorCode := T; end;
procedure ERegExprErrorCode_R(Self: ERegExpr; var T: integer);Begin T := Self.ErrorCode; end;
procedure TRegExprInvertCase_W(Self: TRegExp; const T: TRegExprInvertCaseFunction);begin Self.InvertCase := T; end;
procedure TRegExprInvertCase_R(Self: TRegExp; var T: TRegExprInvertCaseFunction);begin T := Self.InvertCase; end;
procedure TRegExprLinePairedSeparator_W(Self: TRegExp; const T: RegExprString);begin Self.LinePairedSeparator := T; end;
procedure TRegExprLinePairedSeparator_R(Self: TRegExp; var T: RegExprString);begin T := Self.LinePairedSeparator; end;
procedure TRegExprLineSeparators_W(Self: TRegExp; const T: RegExprString);begin Self.LineSeparators := T; end;
procedure TRegExprLineSeparators_R(Self: TRegExp; var T: RegExprString);begin T := Self.LineSeparators; end;
procedure TRegExprWordChars_W(Self: TRegExp; const T: RegExprString);begin Self.WordChars := T; end;
procedure TRegExprWordChars_R(Self: TRegExp; var T: RegExprString);begin T := Self.WordChars; end;
procedure TRegExprSpaceChars_W(Self: TRegExp; const T: RegExprString);begin Self.SpaceChars := T; end;
procedure TRegExprSpaceChars_R(Self: TRegExp; var T: RegExprString);begin T := Self.SpaceChars; end;
procedure TRegExprCompilerErrorPos_R(Self: TRegExp; var T: integer);begin T := Self.CompilerErrorPos; end;
procedure TRegExprMatch_R(Self: TRegExp; var T: RegExprString; const t1: integer);begin T := Self.Match[t1]; end;
procedure TRegExprMatchLen_R(Self: TRegExp; var T: integer; const t1: integer);begin T := Self.MatchLen[t1]; end;
procedure TRegExprMatchPos_R(Self: TRegExp; var T: integer; const t1: integer);begin T := Self.MatchPos[t1]; end;
procedure TRegExprSubExprMatchCount_R(Self: TRegExp; var T: integer);begin T := Self.SubExprMatchCount; end;
Function TRegExprReplace2_P(Self: TRegExp;  AInputStr : RegExprString; AReplaceFunc : TRegExprReplaceFunction) : RegExprString;Begin Result := Self.Replace(AInputStr, AReplaceFunc); END;
Function TRegExprReplace_P(Self: TRegExp;  AInputStr : RegExprString; const AReplaceStr : RegExprString; AUseSubstitution : boolean) : RegExprString;Begin Result := Self.Replace(AInputStr, AReplaceStr, AUseSubstitution); END;
procedure TRegExprInputString_W(Self: TRegExp; const T: RegExprString);begin Self.InputString := T; end;
procedure TRegExprInputString_R(Self: TRegExp; var T: RegExprString);begin T := Self.InputString; end;
Function TRegExprExec_P(Self: TRegExp;  const AInputString : RegExprString) : boolean;Begin Result := Self.Exec(AInputString); END;
procedure TRegExprModifierX_W(Self: TRegExp; const T: boolean);begin Self.ModifierX := T; end;
procedure TRegExprModifierX_R(Self: TRegExp; var T: boolean);begin T := Self.ModifierX; end;
procedure TRegExprModifierM_W(Self: TRegExp; const T: boolean);begin Self.ModifierM := T; end;
procedure TRegExprModifierM_R(Self: TRegExp; var T: boolean);begin T := Self.ModifierM; end;
procedure TRegExprModifierG_W(Self: TRegExp; const T: boolean);begin Self.ModifierG := T; end;
procedure TRegExprModifierG_R(Self: TRegExp; var T: boolean);begin T := Self.ModifierG; end;
procedure TRegExprModifierS_W(Self: TRegExp; const T: boolean);begin Self.ModifierS := T; end;
procedure TRegExprModifierS_R(Self: TRegExp; var T: boolean);begin T := Self.ModifierS; end;
procedure TRegExprModifierR_W(Self: TRegExp; const T: boolean);begin Self.ModifierR := T; end;
procedure TRegExprModifierR_R(Self: TRegExp; var T: boolean);begin T := Self.ModifierR; end;
procedure TRegExprModifierI_W(Self: TRegExp; const T: boolean);begin Self.ModifierI := T; end;
procedure TRegExprModifierI_R(Self: TRegExp; var T: boolean);begin T := Self.ModifierI; end;
procedure TRegExprModifierStr_W(Self: TRegExp; const T: RegExprString);begin Self.ModifierStr := T; end;
procedure TRegExprModifierStr_R(Self: TRegExp; var T: RegExprString);begin T := Self.ModifierStr; end;
procedure TRegExprExpression_W(Self: TRegExp; const T: RegExprString);begin Self.Expression := T; end;
procedure TRegExprExpression_R(Self: TRegExp; var T: RegExprString);begin T := Self.Expression; end;
procedure TMDTMCount_W(Self: TMDTM; const T: Integer);begin Self.Count := T; end;
procedure TMDTMCount_R(Self: TMDTM; var T: Integer);begin T := Self.Count; end;
procedure TMDTMPoints_R(Self : TMDTM; var T : TMDTMPointArray); begin t := self.Points; end;
procedure TMDTMIndex_R(Self : TMDTM; var T : integer); begin t := self.Index; end;
procedure SettingsPrefix(self : TMMLSettingsSandbox; var Prefix : String);begin; Prefix := self.Prefix; end;
procedure TClientWritelnProc_W(Self: TClient; const T: TWritelnProc);Begin Self.WritelnProc := T; end;
procedure TClientWritelnProc_R(Self: TClient; var T: TWritelnProc);Begin T := Self.WritelnProc; end;
procedure TClientMOCR_W(Self: TClient; const T: TMOCR);Begin Self.MOCR := T; end;
procedure TClientMOCR_R(Self: TClient; var T: TMOCR); Begin T := Self.MOCR; end;
procedure TClientMDTMs_W(Self: TClient; const T: TMDTMS);Begin Self.MDTMs := T; end;
procedure TClientMDTMs_R(Self: TClient; var T: TMDTMS);Begin T := Self.MDTMs; end;
procedure TClientMBitmaps_W(Self: TClient; const T: TMBitmaps);Begin Self.MBitmaps := T; end;
procedure TClientMBitmaps_R(Self: TClient; var T: TMBitmaps);Begin T := Self.MBitmaps; end;
procedure TClientMFinder_W(Self: TClient; const T: TMFinder);Begin Self.MFinder := T; end;
procedure TClientMFinder_R(Self: TClient; var T: TMFinder);Begin T := Self.MFinder; end;
procedure TClientMFiles_W(Self: TClient; const T: TMFiles);Begin Self.MFiles := T; end;
procedure TClientMFiles_R(Self: TClient; var T: TMFiles);Begin T := Self.MFiles; end;
procedure TClientIOManager_W(Self: TClient; const T: TIOManager);Begin Self.IOManager := T; end;
procedure TClientIOManager_R(Self: TClient; var T: TIOManager);Begin T := Self.IOManager; end;
procedure TMFinderWarnOnly_W(Self: TMFinder; const T: boolean);Begin Self.WarnOnly := T; end;
procedure TMFinderWarnOnly_R(Self: TMFinder; var T: boolean);Begin T := Self.WarnOnly; end;
procedure TMDTMSDTM_R(Self: TMDTMS; var T: TMDTM; const t1: integer);begin T := Self.DTM[t1]; end;
Function TMDTMSAddMDTM_P(Self: TMDTMS;  const d : TMDTM) : Integer;Begin Result := Self.AddDTM(d); END;
Function TMDTMSAddSDTM_P(Self: TMDTMS;  const d : TSDTM) : Integer;Begin Result := Self.AddDTM(d); END;
Function TMBitmapsCreateBMPFromString_P(Self: TMBitmaps;  width, height : integer; Data : string) : integer;Begin Result := Self.CreateBMPFromString(width, height, Data); END;
procedure TMBitmapsBmp_R(Self: TMBitmaps; var T: TMufasaBitmap; const t1: integer);begin T := Self.Bmp[t1]; end;
Procedure TIOManager_AbstractGetKeyMouseTarget_P(Self: TIOManager_Abstract;  var idx : integer);Begin Self.GetKeyMouseTarget(idx); END;
Procedure TIOManager_AbstractGetImageTarget_P(Self: TIOManager_Abstract;  var idx : integer);Begin Self.GetImageTarget(idx); END;
Function TIOManager_AbstractExportKeyMouseTarget_P(Self: TIOManager_Abstract) : TTarget_Exported;Begin Result := Self.ExportKeyMouseTarget; END;
Function TIOManager_AbstractExportImageTarget_P(Self: TIOManager_Abstract) : TTarget_Exported;Begin Result := Self.ExportImageTarget; END;
Function TIOManager_AbstractGetKeyMouseTarget_P(Self: TIOManager_Abstract) : TTarget;Begin Result := Self.GetKeyMouseTarget; END;
Function TIOManager_AbstractGetImageTarget_P(Self: TIOManager_Abstract) : TTarget;Begin Result := Self.GetImageTarget; END;
Function TIOManager_AbstractSetTargetBmp_P(Self: TIOManager_Abstract;  bmp : TMufasaBitmap) : integer;Begin Result := Self.SetTarget(bmp); END;
Function TIOManager_AbstractSetTargetArr_P(Self: TIOManager_Abstract;  ArrPtr : Integer; Size : TPoint) : integer;Begin Result := Self.SetTarget(PRGB32(ArrPtr), Size); END;
{$ifdef MSWindows}
function TWindowCreate(handle : hwnd) : TWindow; begin result := TWindow.Create(handle); end;
{$endif}
function TIOManagerCreate(plugin_dir : string) : TIOManager; begin result := TIOManager.Create(plugin_dir); end;
function TIOManager_AbstractCreate(plugin_dir : string) : TIOManager_Abstract; begin result := TIOManager_Abstract.Create(plugin_dir); end;
Function TIOManagerSetTarget_P(Self: TIOManager;  target : TNativeWindow) : integer;Begin Result := Self.SetTarget(target); END;
procedure TMufasaBitmapCopyClientToBitmap(Self : TMufasaBitmap; MWindow : TObject; Resize : boolean;x,y : integer; xs, ys, xe, ye: Integer);begin self.CopyClientToBitmap(MWindow,Resize,x,y,xs,ys,xe,ye); end;
{TMMLTimer}
procedure TMMLTimer_ReadEnabled(Self: TMMLTimer; var Enabled: Boolean); begin Enabled := Self.Enabled; end;
procedure TMMLTimer_SetEnabled(Self: TMMLTimer; const Enabled: Boolean); begin Self.Enabled := Enabled; end;
procedure TMMLTimer_ReadInterval(Self: TMMLTimer; var Interval: Integer); begin Interval := Self.Interval; end;
procedure TMMLTimer_SetInterval(Self: TMMLTimer; const Interval: Integer); begin Self.Interval := Interval; end;
procedure TMMLTimer_ReadThreadPriority(Self: TMMLTimer; var ThreadPriority: TThreadPriority); begin ThreadPriority := Self.ThreadPriority; end;
procedure TMMLTimer_SetThreadPriority(Self: TMMLTimer; const ThreadPriority: TThreadPriority); begin Self.ThreadPriority := ThreadPriority; end;

procedure RIRegister_TMufasaBitmap(cl : TPSRuntimeClassImporter);
begin
  with cl.Add(TMufasaBitmap) do
  begin
    RegisterMethod(@TMufasaBitmap.ToTBitmap,'ToTBitmap');
    RegisterMethod(@TMufasaBitmap.SetSize,'SetSize');
    RegisterMethod(@TMufasaBitmap.StretchResize,'StretcRresize');
    RegisterMethod(@TMufasaBitmap.SetPersistentMemory, 'SetPersistentMemory');
    RegisterMethod(@TMufasaBitmap.ResetPersistentMemory, 'SetPersistentMemory');
    RegisterMethod(@TMufasaBitmap.FastSetPixel,'FastSetPixel');
    RegisterMethod(@TMufasaBitmap.FastSetPixels,'FastSetPixels');
    RegisterMethod(@TMufasaBitmap.DrawATPA,'DrawATPA');
    RegisterMethod(@TMufasaBitmap.DrawTPA,'DrawTPA');
    RegisterMethod(@TMufasaBitmap.DrawToCanvas, 'DrawToCanvas');
    RegisterMethod(@TMufasaBitmap.FloodFill,'FloodFill');
    RegisterMethod(@TMufasaBitmap.Rectangle,'Rectangle');
    RegisterMethod(@TMufasaBitmap.FastGetPixel,'FastGetPixel');
    RegisterMethod(@TMufasaBitmapCopyClientToBitmap,'CopyClientToBitmap');
    RegisterMethod(@TMufasaBitmap.SetTransparentColor,'SetTransparentColor');
    RegisterMethod(@TMufasaBitmap.GetTransparentColor,'GetTransparentColor');
    RegisterMethod(@TMufasaBitmap.FastDrawClear,'FastDrawClear');
    RegisterMethod(@TMufasaBitmap.FastDrawTransparent,'FastDrawTransparent');
    RegisterMethod(@TMufasaBitmap.FastReplaceColor,'FastReplaceColor');
    RegisterMethod(@TMufasaBitmap.RotateBitmap,'RotateBitmap');
    RegisterMethod(@TMufasaBitmap.Desaturate,'Desaturate');
    RegisterMethod(@TMufasaBitmap.GreyScale,'GreyScale');
    RegisterMethod(@TMufasaBitmap.Brightness,'Brightness');
    RegisterMethod(@TMufasaBitmap.Contrast,'Contrast');
    RegisterMethod(@TMufasaBitmap.Invert,'Invert');
    RegisterMethod(@TMufasaBitmap.Posterize,'Posterize');
    RegisterMethod(@TMufasaBitmap.Copy, 'Copy');
    RegisterMethod(@TMufasaBitmap.ToString,'ToString');
    RegisterMethod(@TMufasaBitmap.CreateTMask,'CreateTMask');
    RegisterPropertyHelper(@MBmp_TransColorSet_r,nil,'TransparentColorSet');
    RegisterPropertyHelper(@MBmp_Index_r,nil,'Index');
    RegisterPropertyHelper(@MBmp_Width_r,nil,'Width');
    RegisterPropertyHelper(@MBmp_Height_r,nil,'Height');
    RegisterPropertyHelper(@MBmp_FData_r,nil,'FData');
    RegisterPropertyHelper(@MBmp_Name_r,@MBmp_Name_w,'Name');
    RegisterConstructor(@TMufasaBitmap.Create,'Create');
    RegisterMethod(@TMufasaBitmap.free,'Free');
    RegisterMethod(@TMufasaBitmap.SaveToFile, 'SaveToFile');
    RegisterMethod(@TMufasaBitmap.LoadFromFile, 'LoadFormFile');
    RegisterMethod(@TMufasaBitmap.LoadFromTBitmap, 'LoadFromTBitmap');
  end;
end;

procedure RIRegister_TRegExp(cl : TPSRuntimeClassImporter);
begin
  with CL.Add(ERegExpr) do
  begin
    RegisterPropertyHelper(@ERegExprErrorCode_R,@ERegExprErrorCode_W,'ErrorCode');
    RegisterPropertyHelper(@ERegExprCompilerErrorPos_R,@ERegExprCompilerErrorPos_W,'CompilerErrorPos');
  end;
  with CL.Add(TRegExp) do
  begin
    RegisterConstructor(@TRegExp.Create, 'Create');
    RegisterMethod(@TRegExp.VersionMajor, 'VersionMajor');
    RegisterMethod(@TRegExp.VersionMinor, 'VersionMinor');
    RegisterPropertyHelper(@TRegExprExpression_R,@TRegExprExpression_W,'Expression');
    RegisterPropertyHelper(@TRegExprModifierStr_R,@TRegExprModifierStr_W,'ModifierStr');
    RegisterPropertyHelper(@TRegExprModifierI_R,@TRegExprModifierI_W,'ModifierI');
    RegisterPropertyHelper(@TRegExprModifierR_R,@TRegExprModifierR_W,'ModifierR');
    RegisterPropertyHelper(@TRegExprModifierS_R,@TRegExprModifierS_W,'ModifierS');
    RegisterPropertyHelper(@TRegExprModifierG_R,@TRegExprModifierG_W,'ModifierG');
    RegisterPropertyHelper(@TRegExprModifierM_R,@TRegExprModifierM_W,'ModifierM');
    RegisterPropertyHelper(@TRegExprModifierX_R,@TRegExprModifierX_W,'ModifierX');
    RegisterMethod(@TRegExprExec_P, 'Exec');
    RegisterMethod(@TRegExp.ExecNext, 'ExecNext');
    RegisterMethod(@TRegExp.ExecPos, 'ExecPos');
    RegisterPropertyHelper(@TRegExprInputString_R,@TRegExprInputString_W,'InputString');
    RegisterMethod(@TRegExp.Substitute, 'Substitute');
    RegisterMethod(@TRegExp.Split, 'Split');
    RegisterMethod(@TRegExprReplace_P, 'Replace');
    RegisterPropertyHelper(@TRegExprSubExprMatchCount_R,nil,'SubExprMatchCount');
    RegisterPropertyHelper(@TRegExprMatchPos_R,nil,'MatchPos');
    RegisterPropertyHelper(@TRegExprMatchLen_R,nil,'MatchLen');
    RegisterPropertyHelper(@TRegExprMatch_R,nil,'Match');
    RegisterMethod(@TRegExp.LastError, 'LastError');
    RegisterVirtualMethod(@TRegExp.ErrorMsg, 'ErrorMsg');
    RegisterPropertyHelper(@TRegExprCompilerErrorPos_R,nil,'CompilerErrorPos');
    RegisterPropertyHelper(@TRegExprSpaceChars_R,@TRegExprSpaceChars_W,'SpaceChars');
    RegisterPropertyHelper(@TRegExprWordChars_R,@TRegExprWordChars_W,'WordChars');
    RegisterPropertyHelper(@TRegExprLineSeparators_R,@TRegExprLineSeparators_W,'LineSeparators');
    RegisterPropertyHelper(@TRegExprLinePairedSeparator_R,@TRegExprLinePairedSeparator_W,'LinePairedSeparator');
    RegisterMethod(@TRegExp.InvertCaseFunction, 'InvertCaseFunction');
    RegisterPropertyHelper(@TRegExprInvertCase_R,@TRegExprInvertCase_W,'InvertCase');
    RegisterMethod(@TRegExp.Compile, 'Compile');
    RegisterMethod(@TRegExp.Dump, 'Dump');
  end;
end;
procedure RIRegister_TMDTM(cl : TPSRuntimeClassImporter);
begin
  with CL.Add(TMDTM) do
  begin
    RegisterConstructor(@TMDTM.Create,'Create');
    RegisterMethod(@TMDTM.Free,'Free');
    RegisterMethod(@TMDTM.ToString,'ToString');
    RegisterMethod(@TMDTM.LoadFromString,'LoadFromString');
    RegisterMethod(@TMDTM.Normalize,'Normalize');
    RegisterMethod(@TMDTM.Valid,'Valid');
    RegisterMethod(@TMDTM.DeletePoint,'DeletePoint');
    RegisterMethod(@TMDTM.SwapPoint,'SwapPoint');
    Registermethod(@TMDTM.MovePoint,'MovePoint');
    RegisterMethod(@TMDTM.AddPoint,'AddPoint');
    RegisterPropertyHelper(@TMDTMCount_R,@TMDTMCount_W,'Count');
    RegisterPropertyHelper(@TMDTMPoints_R,nil,'Points');
    RegisterPropertyHelper(@TMDTMIndex_r,nil,'Index');
  end;
end;
procedure RIRegister_TMMLSettingsSandbox(cl : TPSRuntimeClassImporter);
begin
  with cl.Add(TMMLSettingsSandbox) do
  begin
    RegisterMethod(@TMMLSettingsSandbox.IsKey,'IsKey');
    RegisterMethod(@TMMLSettingsSandbox.IsDirectory,'IsDirectory');
    RegisterMethod(@TMMLSettingsSandbox.SetKeyValue,'SetKeyValue');
    RegisterMethod(@TMMLSettingsSandbox.GetKeyValue,'GetKeyValue');
    RegisterMethod(@TMMLSettingsSandbox.GetKeyValueDef,'GetKeyValueDef');
    RegisterMethod(@TMMLSettingsSandbox.ListKeys,'ListKeys');
    RegisterMethod(@TMMLSettingsSandbox.DeleteKey,'DeleteKey');
    RegisterMethod(@TMMLSettingsSandbox.DeleteSubKeys,'DeleteSubKeys');
    RegisterPropertyHelper(@SettingsPrefix,nil,'Prefix');
  end;
end;

procedure RIRegister_TMDTMS(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TMDTMS) do
  begin
    RegisterMethod(@TMDTMSAddSDTM_P, 'AddSDTM');
    RegisterMethod(@TMDTMSAddMDTM_P, 'AddMDTM');
    RegisterMethod(@TMDTMS.GetDTM, 'GetDTM');
    RegisterMethod(@TMDTMS.FreeDTM, 'FreeDTM');
    RegisterMethod(@TMDTMS.StringToDTM, 'StringToDTM');
    RegisterPropertyHelper(@TMDTMSDTM_R,nil,'DTM');
    RegisterConstructor(@TMDTMS.Create, 'Create');
  end;
end;

procedure RIRegister_TMFinder(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TMFinder) do
  begin
    RegisterPropertyHelper(@TMFinderWarnOnly_R,@TMFinderWarnOnly_W,'WarnOnly');
    RegisterMethod(@TMFinder.DefaultOperations, 'DefaultOperations');
    RegisterMethod(@TMFinder.CountColorTolerance, 'CountColorTolerance');
    RegisterMethod(@TMFinder.CountColor, 'CountColor');
    RegisterMethod(@TMFinder.SimilarColors, 'SimilarColors');
    RegisterMethod(@TMFinder.FindColor, 'FindColor');
    RegisterMethod(@TMFinder.FindColorSpiral, 'FindColorSpiral');
    RegisterMethod(@TMFinder.FindColorSpiralTolerance, 'FindColorSpiralTolerance');
    RegisterMethod(@TMFinder.FindColorTolerance, 'FindColorTolerance');
    RegisterMethod(@TMFinder.FindColorsTolerance, 'FindColorsTolerance');
    RegisterMethod(@TMFinder.FindColorsSpiralTolerance, 'FindColorsSpiralTolerance');
    RegisterMethod(@TMFinder.FindColors, 'FindColors');
    RegisterMethod(@TMFinder.FindColoredArea, 'FindColoredArea');
    RegisterMethod(@TMFinder.FindColoredAreaTolerance, 'FindColoredAreaTolerance');
    RegisterMethod(@TMFinder.FindMaskTolerance, 'FindMaskTolerance');
    RegisterMethod(@TMFinder.CheckMask, 'CheckMask');
    RegisterMethod(@TMFinder.FindBitmap, 'FindBitmap');
    RegisterMethod(@TMFinder.FindBitmapIn, 'FindBitmapIn');
    RegisterMethod(@TMFinder.FindBitmapToleranceIn, 'FindBitmapToleranceIn');
    RegisterMethod(@TMFinder.FindBitmapSpiral, 'FindBitmapSpiral');
    RegisterMethod(@TMFinder.FindBitmapSpiralTolerance, 'FindBitmapSpiralTolerance');
    RegisterMethod(@TMFinder.FindBitmapsSpiralTolerance, 'FindBitmapsSpiralTolerance');
    RegisterMethod(@TMFinder.FindDeformedBitmapToleranceIn, 'FindDeformedBitmapToleranceIn');
    RegisterMethod(@TMFinder.FindDTM, 'FindDTM');
    RegisterMethod(@TMFinder.FindDTMs, 'FindDTMs');
    RegisterMethod(@TMFinder.FindDTMRotated, 'FindDTMRotated');
    RegisterMethod(@TMFinder.FindDTMsRotated, 'FindDTMsRotated');
    RegisterMethod(@TMFinder.GetColors, 'GetColors');
    RegisterMethod(@TMFinder.SetToleranceSpeed, 'SetToleranceSpeed');
    RegisterMethod(@TMFinder.GetToleranceSpeed, 'GetToleranceSpeed');
    RegisterMethod(@TMFinder.SetToleranceSpeed2Modifiers, 'SetToleranceSpeed2Modifiers');
    RegisterMethod(@TMFinder.GetToleranceSpeed2Modifiers, 'GetToleranceSpeed2Modifiers');
    RegisterConstructor(@TMFinder.Create, 'Create');
  end;
end;

procedure RIRegister_TMBitmaps(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TMBitmaps) do
  begin
    RegisterMethod(@TMBitmaps.GetBMP, 'GetBMP');
    RegisterPropertyHelper(@TMBitmapsBmp_R,nil,'Bmp');
    RegisterMethod(@TMBitmaps.CreateBMP, 'CreateBMP');
    RegisterMethod(@TMBitmaps.AddBMP, 'AddBMP');
    RegisterMethod(@TMBitmaps.CopyBMP, 'CopyBMP');
    RegisterMethod(@TMBitmaps.CreateMirroredBitmap, 'CreateMirroredBitmap');
    RegisterMethod(@TMBitmaps.CreateBMPFromFile, 'CreateBMPFromFile');
    RegisterMethod(@TMBitmapsCreateBMPFromString_P, 'CreateBMPFromString');
    RegisterMethod(@TMBitmaps.FreeBMP, 'FreeBMP');
    RegisterConstructor(@TMBitmaps.Create, 'Create');
  end;
end;

procedure RIRegister_TTarget(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TTarget) do
  begin
    RegisterVirtualMethod(@TTarget.GetTargetDimensions, 'GetTargetDimensions');
    RegisterVirtualMethod(@TTarget.GetColor, 'GetColor');
    RegisterVirtualMethod(@TTarget.ReturnData, 'ReturnData');
    RegisterVirtualMethod(@TTarget.FreeReturnData, 'FreeReturnData');
    RegisterVirtualMethod(@TTarget.ActivateClient, 'ActivateClient');
    RegisterVirtualMethod(@TTarget.TargetValid, 'TargetValid');
{    RegisterVirtualAbstractMethod(TTarget,@TTarget.GetError, 'GetError');
    RegisterVirtualAbstractMethod(TTarget,@TTarget.ReceivedError, 'ReceivedError');
    RegisterVirtualAbstractMethod(Ttarget,@TTarget.ResetError, 'ResetError');}
    RegisterVirtualMethod(@TTarget.GetMousePosition, 'GetMousePosition');
    RegisterVirtualMethod(@TTarget.MoveMouse, 'MoveMouse');
    RegisterVirtualMethod(@TTarget.ScrollMouse, 'ScrollMouse');
    RegisterVirtualMethod(@TTarget.HoldMouse, 'HoldMouse');
    RegisterVirtualMethod(@TTarget.ReleaseMouse, 'ReleaseMouse');
    RegisterVirtualMethod(@TTarget.IsMouseButtonHeld, 'IsMouseButtonHeld');
    RegisterVirtualMethod(@TTarget.SendString, 'SendString');
    RegisterVirtualMethod(@TTarget.HoldKey, 'HoldKey');
    RegisterVirtualMethod(@TTarget.ReleaseKey, 'ReleaseKey');
    RegisterVirtualMethod(@TTarget.IsKeyHeld, 'IsKeyHeld');
    RegisterVirtualMethod(@TTarget.GetKeyCode, 'GetKeyCode');
  end;
end;

procedure RIRegister_TRawTarget(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TRawTarget) do
  begin
    RegisterConstructor(@TRawTarget.Create, 'Create');
  end;
end;

procedure RIRegister_TBitmapTarget(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TBitmapTarget) do
  begin
    RegisterConstructor(@TBitmapTarget.Create, 'Create');
  end;
end;

procedure RIRegister_TWindow_Abstract(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TWindow_Abstract) do
  begin
  end;
end;

procedure RIRegister_TEIOS_Target(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TEIOS_Target) do
  begin
    RegisterConstructor(@TEIOS_Target.Create, 'Create');
  end;
end;

procedure RIRegister_TWindow(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TWindow) do
  begin
    {$ifdef MSWindows}
    RegisterConstructor(@TWindowCreate, 'Create');
    {$endif}
    RegisterMethod(@TWindow.GetNativeWindow, 'GetNativeWindow');
  end;
end;

procedure RIRegister_TIOManager_Abstract(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TIOManager_Abstract) do
  begin
    RegisterConstructor(@TIOManager_AbstractCreate, 'Create');
    RegisterMethod(@TIOManager_Abstract.GetError, 'GetError');
    RegisterMethod(@TIOManager_Abstract.ReceivedError, 'ReceivedError');
    RegisterMethod(@TIOManager_Abstract.ResetError, 'ResetError');
//    RegisterVirtualAbstractMethod(TIOManager_Abstract, @TIOManager_Abstract.SetDesktop, 'SetDesktop');
    RegisterMethod(@TIOManager_AbstractSetTargetArr_P, 'SetTargetArray');
    RegisterMethod(@TIOManager_AbstractSetTargetBmp_P, 'SetTargetBitmap');
    RegisterMethod(@TIOManager_Abstract.TargetValid, 'TargetValid');
    RegisterMethod(@TIOManager_Abstract.BitmapDestroyed, 'BitmapDestroyed');
    RegisterMethod(@TIOManager_Abstract.GetColor, 'GetColor');
    RegisterMethod(@TIOManager_Abstract.ReturnData, 'ReturnData');
    RegisterMethod(@TIOManager_Abstract.FreeReturnData, 'FreeReturnData');
    RegisterMethod(@TIOManager_Abstract.GetDimensions, 'GetDimensions');
    RegisterMethod(@TIOManager_Abstract.ActivateClient, 'ActivateClient');
    RegisterMethod(@TIOManager_Abstract.IsFrozen, 'IsFrozen');
    RegisterMethod(@TIOManager_Abstract.SetFrozen, 'SetFrozen');
    RegisterMethod(@TIOManager_Abstract.GetMousePos, 'GetMousePos');
    RegisterMethod(@TIOManager_Abstract.MoveMouse, 'MoveMouse');
    RegisterMethod(@TIOManager_Abstract.ScrollMouse, 'ScrollMouse');
    RegisterMethod(@TIOManager_Abstract.HoldMouse, 'HoldMouse');
    RegisterMethod(@TIOManager_Abstract.ReleaseMouse, 'ReleaseMouse');
    RegisterMethod(@TIOManager_Abstract.ClickMouse, 'ClickMouse');
    RegisterMethod(@TIOManager_Abstract.IsMouseButtonDown, 'IsMouseButtonDown');
    RegisterMethod(@TIOManager_Abstract.KeyUp, 'KeyUp');
    RegisterMethod(@TIOManager_Abstract.KeyDown, 'KeyDown');
    RegisterMethod(@TIOManager_Abstract.PressKey, 'PressKey');
    RegisterMethod(@TIOManager_Abstract.SendText, 'SendText');
    RegisterMethod(@TIOManager_Abstract.isKeyDown, 'isKeyDown');
    RegisterMethod(@TIOManager_Abstract.GetKeyCode, 'GetKeyCode');
    RegisterMethod(@TIOManager_AbstractGetImageTarget_P, 'GetImageTarget');
    RegisterMethod(@TIOManager_AbstractGetKeyMouseTarget_P, 'GetKeyMouseTarget');
    RegisterMethod(@TIOManager_AbstractExportImageTarget_P, 'ExportImageTarget');
    RegisterMethod(@TIOManager_AbstractExportKeyMouseTarget_P, 'ExportKeyMouseTarget');
    RegisterMethod(@TIOManager_AbstractGetImageTarget_P, 'GetImageTarget');
    RegisterMethod(@TIOManager_AbstractGetKeyMouseTarget_P, 'GetKeyMouseTarget');
    RegisterMethod(@TIOManager_Abstract.SetImageTarget, 'SetImageTarget');
    RegisterMethod(@TIOManager_Abstract.SetKeyMouseTarget, 'SetKeyMouseTarget');
    RegisterMethod(@TIOManager_Abstract.FreeTarget, 'FreeTarget');
    RegisterMethod(@TIOManager_Abstract.SetState, 'SetState');
  end;
end;

procedure RIRegister_TIOManager(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TIOManager) do
  begin
    RegisterConstructor(@TIOManagerCreate, 'Create');
    RegisterMethod(@TIOManager.SetDesktop,'SetDesktop');
    RegisterMethod(@TIOManagerSetTarget_P, 'SetTarget');
  end;
end;

procedure RIRegister_IOManager(CL: TPSRuntimeClassImporter);
begin
  RIRegister_TTarget(CL);
  RIRegister_TRawTarget(CL);
  RIRegister_TBitmapTarget(CL);
  RIRegister_TWindow_Abstract(CL);
  RIRegister_TEIOS_Target(CL);
  RIRegister_TWindow(cl);
  RIRegister_TIOManager_Abstract(CL);
  RIRegister_TIOManager(cl);
end;

procedure RIRegister_TClient(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TClient) do
  begin
    RegisterPropertyHelper(@TClientIOManager_R,@TClientIOManager_W,'IOManager');
    RegisterPropertyHelper(@TClientMFiles_R,@TClientMFiles_W,'MFiles');
    RegisterPropertyHelper(@TClientMFinder_R,@TClientMFinder_W,'MFinder');
    RegisterPropertyHelper(@TClientMBitmaps_R,@TClientMBitmaps_W,'MBitmaps');
    RegisterPropertyHelper(@TClientMDTMs_R,@TClientMDTMs_W,'MDTMs');
    RegisterPropertyHelper(@TClientMOCR_R,@TClientMOCR_W,'MOCR');
    RegisterPropertyHelper(@TClientWritelnProc_R,@TClientWritelnProc_W,'WritelnProc');
    RegisterMethod(@TClient.WriteLn, 'WriteLn');
    RegisterConstructor(@TClient.Create, 'Create');
  end;
end;

procedure RIRegister_TMMLTimer(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TMMLTimer) do
  begin
    RegisterConstructor(@TMMLTimer.Create, 'Create');
    RegisterPropertyHelper(@TMMLTimer_ReadEnabled, @TMMLTimer_SetEnabled, 'Enabled');
    RegisterPropertyHelper(@TMMLTimer_ReadInterval, @TMMLTimer_SetInterval, 'Interval');
    RegisterPropertyHelper(@TMMLTimer_ReadThreadPriority, @TMMLTimer_SetThreadPriority, 'ThreadPriority');
    RegisterMethod(@TMMLTimer.On, 'On');
    RegisterMethod(@TMMLTimer.Off, 'Off');
  end;
end;
procedure RIRegister_MML(cl: TPSRuntimeClassImporter);
begin;
  RIRegister_TMufasaBitmap(cl);
  RIRegister_TRegExp(cl);
  RIRegister_TMDTM(cl);
  RIRegister_TMMLSettingsSandbox(cl);
  RIRegister_TMDTMS(cl);
  RIRegister_TMFinder(cl);
  RIRegister_TMBitmaps(cl);
  RIRegister_IOManager(cl);
  RIRegister_TClient(cl);
  RIRegister_TMMLTimer(cl);
end;

end.
