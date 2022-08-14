program test_codetools;

{$i simba.inc}

uses
  classes, sysutils,
  simba.parser_misc;

var
  Failed, Passed: Integer;

procedure Assert(Test, Returned, Expected: String);
begin
  if (Returned <> Expected) then
  begin
    WriteLn('Failed :: "', Test, '"');
    WriteLn('Expected: "', Expected, '"');
    WriteLn('Returned: "', Returned, '"');
    WriteLn('');

    Failed := Failed + 1;
  end else
    Passed := Passed + 1;
end;

procedure Test_GetExpression(Test, Expected: String);
begin
  Assert(Test, GetExpression(Test, Length(Test)), Expected);
end;

begin
  Failed := 0;
  Passed := 0;

  Test_GetExpression('Foo(0, [5], Bar(1, [2,2], Foo())).', 'Foo().');
  Test_GetExpression('Foo([1,2])[1].', 'Foo()[].');
  Test_GetExpression('Foo([1,2])[1,3].', 'Foo()[,].');
  Test_GetExpression('Foo([1,2])[1,Abs(1),[X,Y]].', 'Foo()[,,].');
  Test_GetExpression('Foo([1,2])[1][3].', 'Foo()[][].');
  Test_GetExpression('Foo([1,2])[Abs(1),[1,2]].', 'Foo()[,].');
  Test_GetExpression('Foo([1,2])[Abs(1)][[1,2]].', 'Foo()[][].');

  Test_GetExpression('"hello world".', 'String.');
  Test_GetExpression(#39 + 'Test""' + #39 + '.', 'String.');

  WriteLn(IntToStr(Passed), ' tests passed');
  WriteLn(IntToStr(Failed), ' tests failed');

  if (Failed > 0) then
    ExitCode := 1;

  ReadLn;
end.

