unit uPSC_mml;
interface
uses
  uPSCompiler;

procedure SIRegister_MML(cl: TPSPascalCompiler);

implementation

procedure SIRegister_TMufasaBitmap(cl : TPSPascalCompiler);
begin
  with cl.AddClassN(cl.FindClass('TObject'),'TMufasaBitmap') do
  begin;
    RegisterMethod('procedure SetSize(AWidth,AHeight : integer);');
    RegisterMethod('procedure StretchResize(AWidth,AHeight : integer);');
    RegisterMethod('procedure FastSetPixel(x,y : integer; Color : TColor);');
    RegisterMethod('procedure FastSetPixels(TPA : TPointArray; Colors : TIntegerArray);');
    RegisterMethod('procedure DrawATPA(ATPA : T2DPointArray; Colors : TIntegerArray);');
    RegisterMethod('procedure DrawTPA(TPA : TPointArray; Color : TColor);');
    RegisterMethod('function FastGetPixel(x,y : integer) : TColor;');
    RegisterMethod('procedure CopyClientToBitmap(Resize : boolean;x,y : integer; xs, ys, xe, ye: Integer);');
    RegisterMethod('procedure Rectangle(const Box : TBox; FillCol : TColor);');
    RegisterMethod('procedure FloodFill(const StartPT : TPoint; const SearchCol,ReplaceCol : TColor);');
//      function FastGetPixels(TPA : TPointArray) : TIntegerArray;
    RegisterMethod('procedure SetTransparentColor(Col : TColor);');
    RegisterMethod('function GetTransparentColor : TColor;');
    RegisterProperty('TransparentColorSet','Boolean',iptR);
    RegisterMethod('procedure FastDrawClear(Color : TColor);');
    RegisterMethod('procedure FastDrawTransparent(x, y: Integer; TargetBitmap: TMufasaBitmap);');
    RegisterMethod('procedure FastReplaceColor(OldColor, NewColor: TColor);');
    RegisterMethod('procedure RotateBitmap(angle: Extended;TargetBitmap : TMufasaBitmap );');
    RegisterMethod('procedure Desaturate(TargetBitmap : TMufasaBitmap);');
    RegisterMethod('procedure GreyScale(TargetBitmap : TMufasaBitmap);');
    RegisterMethod('procedure Brightness(TargetBitmap : TMufasaBitmap; br : integer);');
    RegisterMethod('procedure Contrast(TargetBitmap : TMufasaBitmap; co : Extended);');
    RegisterMethod('procedure Invert(TargetBitmap : TMufasaBitmap);');
    RegisterMethod('procedure Posterize(TargetBitmap : TMufasaBitmap; Po : integer);');
    RegisterMethod('function Copy(const xs,ys,xe,ye : integer) : TMufasaBitmap;');
    RegisterMethod('function ToString : string;');
    RegisterMethod('function ToTBitmap : TBitmap;');
    RegisterMethod('function CreateTMask : TMask;');
    RegisterMethod('constructor create');
    RegisterMethod('procedure Free');
    RegisterMethod('function SaveToFile(const FileName : string) :boolean;');
    RegisterMethod('procedure LoadFromFile(const FileName : string);');
    RegisterProperty('Width','Integer',iptR);
    RegisterProperty('Height','Integer',iptR);
    RegisterProperty('Index','Integer',iptR);
    RegisterProperty('Name','String',iptRW);
  end;
end;

procedure SIRegister_TRegExp(cl : TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('Exception'),'ERegExpr') do
  begin
    RegisterProperty('ErrorCode', 'integer', iptrw);
    RegisterProperty('CompilerErrorPos', 'integer', iptrw);
  end;
  with cl.AddClassN(cl.FindClass('TObject'),'TRegExp') do
  begin
    RegisterMethod('Constructor Create');
    RegisterMethod('Function VersionMajor : integer');
    RegisterMethod('Function VersionMinor : integer');
    RegisterProperty('Expression', 'String', iptrw);
    RegisterProperty('ModifierStr', 'String', iptrw);
    RegisterProperty('ModifierI', 'boolean', iptrw);
    RegisterProperty('ModifierR', 'boolean', iptrw);
    RegisterProperty('ModifierS', 'boolean', iptrw);
    RegisterProperty('ModifierG', 'boolean', iptrw);
    RegisterProperty('ModifierM', 'boolean', iptrw);
    RegisterProperty('ModifierX', 'boolean', iptrw);
    RegisterMethod('Function Exec( const AInputString : String) : boolean;');
    RegisterMethod('Function ExecNext : boolean');
    RegisterMethod('Function ExecPos( AOffset : integer) : boolean');
    RegisterProperty('InputString', 'String', iptrw);
    RegisterMethod('Function Substitute( const ATemplate : String) : String');
    RegisterMethod('Procedure Split( AInputStr : String; APieces : TStrings)');
    RegisterMethod('Function Replace( AInputStr : String; const AReplaceStr : String; AUseSubstitution : boolean) : String;');
    RegisterProperty('SubExprMatchCount', 'integer', iptr);
    RegisterProperty('MatchPos', 'integer integer', iptr);
    RegisterProperty('MatchLen', 'integer integer', iptr);
    RegisterProperty('Match', 'String integer', iptr);
    RegisterMethod('Function LastError : integer');
    RegisterMethod('Function ErrorMsg( AErrorID : integer) : String');
    RegisterProperty('CompilerErrorPos', 'integer', iptr);
    RegisterProperty('SpaceChars', 'String', iptrw);
    RegisterProperty('WordChars', 'String', iptrw);
    RegisterProperty('LineSeparators', 'String', iptrw);
    RegisterProperty('LinePairedSeparator', 'String', iptrw);
    RegisterMethod('Function InvertCaseFunction( const Ch : Char) : Char');
    RegisterProperty('InvertCase', 'TRegExprInvertCaseFunction', iptrw);
    RegisterMethod('Procedure Compile');
    RegisterMethod('Function Dump : String');
  end;
end;

procedure SIRegister_TMDTM(cl : TPSPascalCompiler);
begin
  with cl.AddClassN(cl.FindClass('TObject'),'TMDTM') do
  begin
    RegisterMethod('constructor create;');
    RegisterMethod('procedure free;');
    RegisterProperty('Name','String',iptrw);
    RegisterMethod('function ToString : string');
    RegisterMethod('function Valid:boolean');
    RegisterMethod('procedure DeletePoint( Point : integer);');
    RegisterMethod('procedure SwapPoint(p1,p2 : integer);');
    RegisterMethod('procedure MovePoint(fromIndex,toIndex : integer);');
    RegisterMethod('procedure AddPoint( Point : TMDTMPoint);');
    RegisterProperty('Count','Integer',iptrw);
    RegisterProperty('Points','TMDTMPointArray',iptr);
  end;
end;

procedure SIRegister_TMMLSettingsSandbox(CL : TPSPascalCompiler);
begin
  with cl.AddClassN(nil,'TMMLSettingsSandbox') do
  begin;
    RegisterMethod('function IsKey(const KeyName: String): Boolean;');
    RegisterMethod('function IsDirectory(const KeyName: String): Boolean;');
    RegisterMethod('function SetKeyValue(const Keyname, Value : string) : boolean;');
    RegisterMethod('function GetKeyValue(const KeyName: String): String;');
    RegisterMethod('function GetKeyValueDef(const KeyName, defVal: String): String;');
    RegisterMethod('function ListKeys(const KeyName: String; out Keys :TStringArray): boolean;');
    RegisterMethod('function DeleteKey(const KeyName: String): Boolean;');
    RegisterMethod('function DeleteSubKeys(const KeyName: String): Boolean;');
    RegisterProperty('Prefix','String',iptR);
  end;
end;

procedure SIRegister_MML(cl: TPSPascalCompiler);
begin
  SIRegister_TMufasaBitmap(cl);
  SIRegister_TRegExp(cl);
  SIRegister_TMDTM(cL);
  SIRegister_TMMLSettingsSandbox(cl);
end;

end.
