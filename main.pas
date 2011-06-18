unit Main;

{$I lape.inc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, SynEdit, SynHighlighterPas;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnGo: TButton;
    btnGo2: TButton;
    btnGo3: TButton;
    btnGo4: TButton;
    e: TSynEdit;
    m: TMemo;
    d: TMemo;
    Splitter1: TSplitter;
    PasSyn: TSynPasSyn;
    Splitter2: TSplitter;
    procedure btnGo2Click(Sender: TObject);
    procedure btnGo3Click(Sender: TObject);
    procedure btnGo4Click(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1;

implementation

uses
  lpparser, lpcompiler, lptypes, lpvartypes, lpeval, lpinterpreter, lpdisassembler, {_lpgenerateevalfunctions,}
  LCLIntf, Variants;

{$R *.lfm}

{ TForm1 }

type
  TPointArray = array of TPoint;
  T2DPointArray = array of TPointArray;

  _rec2 = record
    a: Integer;
    test: PInteger;
  end;
  _prec2 = ^_rec2;
  _rec = record
    x, y: string;
    z: _prec2;
    arr: TPointArray;
  end;

procedure TForm1.btnGoClick(Sender: TObject);
var
  i: Integer;
begin
  WriteLn(Ord(Low(opCode)), '..', Ord(High(opCode)));
  {$IFDEF Lape_TrackObjects}
  for i := 0 to lpgList.Count - 1 do
    WriteLn('unfreed: ', TLapeBaseClass(lpgList[i]).ClassName, ' -- [',  PtrInt(lpgList[i]), ']');
  {$ENDIF}
end;

procedure MyWrite(Params: PParamArray);
begin
  Form1.d.Text := Form1.d.Text + PlpString(Params^[0])^;
end;

procedure MyWriteLn(Params: PParamArray);
begin
  Form1.d.Text := Form1.d.Text + LineEnding;
end;

procedure MyRandom(const Params: PParamArray; const Result: Pointer);
begin
  PInt32(Result)^ := PInt32(Params^[0])^ + Random(PInt32(Params^[1])^ - PInt32(Params^[0])^ + 1);
end;

procedure MyIntToString(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := IntToStr(PInt32(Params^[0])^);
end;

procedure MyInt64ToString(const Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := IntToStr(PInt64(Params^[0])^);
end;

procedure MyStringToInt(const Params: PParamArray; const Result: Pointer);
begin
  PInt32(Result)^ := StrToInt(PlpString(Params^[0])^);
end;

procedure MyStupidProc(Params: PParamArray);
begin
  raise Exception.Create('Stupid Proc!!');
end;

var
  MyString: lpString;

procedure TForm1.btnGo2Click(Sender: TObject);

  function CombineDeclArray(a, b: TLapeDeclArray): TLapeDeclArray;
  var
    i, l: Integer;
  begin
    Result := a;
    l := Length(a);
    SetLength(Result, l + Length(b));
    for i := High(b) downto 0 do
      Result[l + i] := b[i];
  end;

var
  Parser: TLapeTokenizerString;
  Compiler: TLapeCompiler;
  {i,} di: Integer;
  dp: PInteger;
  t: Cardinal;
  rec, rec2, tp: TLapeType_Record;
  ttpa, t2dpa: TLapeType_DynArray;
  q: _rec;
  func1, func2, func3: TLapeType_Method;
  tpa: TPointArray;
  atpa: T2DPointArray;
  a: TLapeGlobalVar;
begin
  Parser := TLapeTokenizerString.Create(e.Lines.Text);
  Compiler := TLapeCompiler.Create(Parser);
  try
    di := 112233;
    dp := @di;
    MyString := 'a string';

    tp := TLapeType_Record.Create(Compiler, nil);
    tp.addField(Compiler.BaseTypes[ltInt32], 'x');
    tp.addField(Compiler.BaseTypes[ltInt32], 'y');
    ttpa := TLapeType_DynArray.Create(tp, Compiler);
    t2dpa := TLapeType_DynArray.Create(ttpa, Compiler);
    rec2 := TLapeType_Record.Create(Compiler, nil);
    rec2.addField(Compiler.BaseTypes[ltInt32], 'a');
    rec2.addField(Compiler.getPointerType(ltInt32), 'test');
    rec := TLapeType_Record.Create(Compiler, nil);
    rec.addField(Compiler.BaseTypes[ltString], 'x');
    rec.addField(Compiler.BaseTypes[ltString], 'y');
    rec.addField(Compiler.getPointerType(rec2), 'z');
    rec.addField(ttpa, 'arr');

    a := Compiler.addGlobalVar(123, 'a');

    func1 := TLapeType_Method.Create(Compiler, [Compiler.getBaseType(ltInt32), Compiler.getBaseType(ltInt32)], [lptNormal, lptNormal], [nil, a], Compiler.getBaseType(ltInt32));
    func2 := TLapeType_Method.Create(Compiler, [Compiler.getBaseType(ltInt32)], [lptNormal], [TLapeGlobalVar(nil)], Compiler.getBaseType(ltString));
    func3 := TLapeType_Method.Create(Compiler, [Compiler.getBaseType(ltString)], [lptNormal], [TLapeGlobalVar(nil)], Compiler.getBaseType(ltInt32));

    New(q.z);
    New(q.z^.test);
    SetLength(tpa, 10);
    tpa[0] := Point(123, 456);
    SetLength(atpa, 10, 10);
    atpa[0,0] := Point(123, 456);
    q.arr := tpa;

    //Compiler.addGlobalVar(proc1.NewGlobalVar(@MyWriteLn, 'WriteLn'));
    Compiler.addGlobalVar(func1.NewGlobalVar(@MyRandom, 'Random'));
    Compiler.addGlobalFunc('procedure _write(const s: string); override;', @MyWrite);
    Compiler.addGlobalFunc('procedure _writeln; override;', @MyWriteLn);
    Compiler.addGlobalFunc('function IntToStr(x: Int32): AnsiString; overload;', @MyIntToString);
    Compiler.addGlobalFunc('function IntToStr(x: Int64 = 123): AnsiString; overload;', @MyInt64ToString);
    Compiler.addGlobalFunc('procedure MyStupidProc', @MyStupidProc);
    Compiler.addGlobalVar(func3.NewGlobalVar(@MyStringToInt, 'StrToInt'));

    Compiler.addGlobalVar(Compiler.addGlobalType('record x, y: Int32; end', 'TPoint'), @tpa[0], 'myPoint');
    Compiler.addGlobalType('(enum1, enum2)', 'TMyEnum');
    Compiler.addGlobalVar(Compiler.getBaseType(ltString).NewGlobalVarP(@MyString, 'MyString'));
    Compiler.addGlobalVar(456, 'b');
    Compiler.addGlobalVar(789, 'c');
    Compiler.addGlobalVar(Compiler.getPointerType(ltInt32).NewGlobalVar(@di, 'd'));
    Compiler.addGlobalVar(Compiler.getPointerType(Compiler.getPointerType(ltInt32)).NewGlobalVar(@dp, 'dp'));
    Compiler.addGlobalVar('abc', 's');
    Compiler.addGlobalVar('cde', 't');
    Compiler.addGlobalVar(ttpa.NewGlobalVar(@tpa[0]), 'tpa');
    Compiler.addGlobalVar(t2dpa.NewGlobalVar(@atpa[0]), 'atpa');
    Compiler.addGlobalVar(rec.NewGlobalVarP(@q), 'q');
    Compiler.addGlobalVar(Compiler.getPointerType(rec).NewGlobalVar(@q, 'qp'));
    Compiler.addGlobalVar(Compiler.getPointerType(ttpa).NewGlobalVar(@tpa, 'tpap'));

    try
      t := getTickCount;
      if Compiler.Compile() then
      begin
        m.Lines.add('Compiling Time: ' + IntToStr(getTickCount - t) + 'ms.');
        DisassembleCode(Compiler.Emitter.Code, CombineDeclArray(Compiler.ManagedDeclarations.getByClass(TLapeGlobalVar), Compiler.GlobalDeclarations.getByClass(TLapeGlobalVar)));

        t := getTickCount;
        RunCode(Compiler.Emitter.Code);
        m.Lines.add('Running Time: ' + IntToStr(getTickCount - t) + 'ms.');
      end
      else
        m.Lines.add('Error!');
    except
      on E: Exception do
        m.Lines.add('Compilation error: "' + E.Message + '"');
    end;
  finally
    Dispose(q.z^.test);
    Dispose(q.z);

    Compiler.Free();
    rec2.Free();
    rec.Free();
    tp.Free();
    ttpa.Free();
    t2dpa.Free();
    func1.Free();
    func2.Free();
    func3.Free();
  end;
end;

procedure TForm1.btnGo3Click(Sender: TObject);
begin
  //LapePrintEvalRes;
end;

procedure TForm1.btnGo4Click(Sender: TObject);
begin
  //LapePrintEvalArr;
end;

end.

