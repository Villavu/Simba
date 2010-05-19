unit uPSR_mml;

interface
uses
  uPSRuntime;

procedure RIRegister_MML(cl: TPSRuntimeClassImporter);

implementation
uses
  SynRegExpr,bitmaps,dtm,mufasatypes,settingssandbox;

type
  TRegExp = class(SynRegExpr.TRegExpr);
procedure MBmp_Index_r(self : TMufasaBitmap; var Index : integer);begin;  Index := self.Index; end;
procedure MBmp_Width_r(self : TMufasaBitmap; var Width : integer);begin;  Width := self.Width; end;
procedure MBmp_Height_r(self : TMufasaBitmap; var Height : integer);begin;  Height := self.Height; end;
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
procedure SettingsPrefix(self : TMMLSettingsSandbox; var Prefix : String);begin; Prefix := self.Prefix; end;


procedure RIRegister_TMufasaBitmap(cl : TPSRuntimeClassImporter);
begin
  with cl.Add(TMufasaBitmap) do
  begin
    RegisterMethod(@TMufasaBitmap.ToTBitmap,'ToTBitmap');
    RegisterMethod(@TMufasaBitmap.SetSize,'SETSIZE');
    RegisterMethod(@TMufasaBitmap.StretchResize,'STRETCHRESIZE');
    RegisterMethod(@TMufasaBitmap.FastSetPixel,'FASTSETPIXEL');
    RegisterMethod(@TMufasaBitmap.FastSetPixels,'FASTSETPIXELS');
    RegisterMethod(@TMufasaBitmap.DrawATPA,'DRAWATPA');
    RegisterMethod(@TMufasaBitmap.DrawTPA,'DRAWTPA');
    RegisterMethod(@TMufasaBitmap.FloodFill,'FLOODFILL');
    RegisterMethod(@TMufasaBitmap.Rectangle,'RECTANGLE');
    RegisterMethod(@TMufasaBitmap.FastGetPixel,'FASTGETPIXEL');
    RegisterMethod(@TMufasaBitmap.SetTransparentColor,'SETTRANSPARENTCOLOR');
    RegisterMethod(@TMufasaBitmap.GetTransparentColor,'GETTRANSPARENTCOLOR');
    RegisterMethod(@TMufasaBitmap.FastDrawClear,'FASTDRAWCLEAR');
    RegisterMethod(@TMufasaBitmap.FastDrawTransparent,'FASTDRAWTRANSPARENT');
    RegisterMethod(@TMufasaBitmap.FastReplaceColor,'FASTREPLACECOLOR');
    RegisterMethod(@TMufasaBitmap.RotateBitmap,'ROTATEBITMAP');
    RegisterMethod(@TMufasaBitmap.Desaturate,'DESATURATE');
    RegisterMethod(@TMufasaBitmap.GreyScale,'GREYSCALE');
    RegisterMethod(@TMufasaBitmap.Brightness,'BRIGHTNESS');
    RegisterMethod(@TMufasaBitmap.Contrast,'CONTRAST');
    RegisterMethod(@TMufasaBitmap.Invert,'INVERT');
    RegisterMethod(@TMufasaBitmap.Posterize,'POSTERIZE');
    RegisterMethod(@TMufasaBitmap.Copy, 'COPY');
    RegisterMethod(@TMufasaBitmap.ToString,'TOSTRING');
    RegisterMethod(@TMufasaBitmap.CreateTMask,'CREATETMASK');
    RegisterPropertyHelper(@MBmp_TransColorSet_r,nil,'TRANSPARENTCOLORSET');
    RegisterPropertyHelper(@MBmp_Index_r,nil,'INDEX');
    RegisterPropertyHelper(@MBmp_Width_r,nil,'WIDTH');
    RegisterPropertyHelper(@MBmp_Height_r,nil,'HEIGHT');
    RegisterPropertyHelper(@MBmp_Name_r,@MBmp_Name_w,'NAME');
    RegisterConstructor(@TMufasaBitmap.Create,'CREATE');
    RegisterMethod(@TMufasaBitmap.free,'FREE');
    RegisterMethod(@TMufasaBitmap.SaveToFile, 'SAVETOFILE');
    RegisterMethod(@TMufasaBitmap.LoadFromFile, 'LOADFROMFILE');
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
    RegisterMethod(@TMDTM.Valid,'Valid');
    RegisterMethod(@TMDTM.DeletePoint,'DeletePoint');
    RegisterMethod(@TMDTM.SwapPoint,'SwapPoint');
    Registermethod(@TMDTM.MovePoint,'MovePoint');
    RegisterMethod(@TMDTM.AddPoint,'AddPoint');
    RegisterPropertyHelper(@TMDTMCount_R,@TMDTMCount_W,'Count');
    RegisterPropertyHelper(@TMDTMPoints_R,nil,'Points');
  end;
end;
procedure RIRegister_TMMLSettingsSandbox(cl : TPSRuntimeClassImporter);
begin
  with cl.Add(TMMLSettingsSandbox) do
  begin
    RegisterMethod(@TMMLSettingsSandbox.IsKey,'ISKEY');
    RegisterMethod(@TMMLSettingsSandbox.IsDirectory,'ISDIRECTORY');
    RegisterMethod(@TMMLSettingsSandbox.SetKeyValue,'SETKEYVALUE');
    RegisterMethod(@TMMLSettingsSandbox.GetKeyValue,'GETKEYVALUE');
    RegisterMethod(@TMMLSettingsSandbox.GetKeyValueDef,'GETKEYVALUEDEF');
    RegisterMethod(@TMMLSettingsSandbox.ListKeys,'LISTKEYS');
    RegisterMethod(@TMMLSettingsSandbox.DeleteKey,'DELETEKEY');
    RegisterMethod(@TMMLSettingsSandbox.DeleteSubKeys,'DELETESUBKEYS');
    RegisterPropertyHelper(@SettingsPrefix,nil,'Prefix');
  end;
end;

procedure RIRegister_MML(cl: TPSRuntimeClassImporter);
begin;
  RIRegister_TMufasaBitmap(cl);
  RIRegister_TRegExp(cl);
  RIRegister_TMDTM(cl);
  RIRegister_TMMLSettingsSandbox(cl);
end;

end.
