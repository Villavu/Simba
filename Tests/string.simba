{$assertions on}
var
  TestString: String;

begin
  TestString := 'abcHelloxyz';
  Assert(ToString(TestString.Partition('Hello')) = '[abc, Hello, xyz]');

  TestString := 'HelloRemoveWorld';
  Assert(TestString.Remove('Remove') = True);
  Assert(TestString = 'HelloWorld');

  TestString := 'HelloRemoveRemoveWorld';
  Assert(TestString.RemoveAll('Remove') = 2);
  Assert(TestString = 'HelloWorld');

  TestString := '1.23';
  Assert(TestString.IsInteger = False);
  Assert(TestString.IsFloat   = True);
  Assert(ToString(TestString.ExtractFloat) = '1.23');

  TestString := '-9000';
  Assert(TestString.IsFloat   = True);
  Assert(TestString.IsInteger = True);
  Assert(ToString(TestString.ExtractInteger) = '-9000');
  Assert(ToString(TestString.ExtractFloat) = '-9000');

  TestString := 'Hello World 1,2,3';
  Assert(ToString(TestString.Split(' ')) = '[Hello, World, 1,2,3]');

  TestString := 'abc abc xyz xyz';
  Assert(TestString.Extract(['x', 'y', 'z']) = 'xyzxyz');

  TestString := 'hello123world456';
  Assert(ToString(TestString.ExtractInteger) = '123456');
  TestString := '-hello123world456';
  Assert(ToString(TestString.ExtractInteger) = '-123456');

  TestString := 'hello123world.456';
  Assert(ToString(TestString.ExtractFloat) = '123.456');
  TestString := '-hello123world.456';
  Assert(ToString(TestString.ExtractFloat) = '-123.456');

  Assert(String.UpperChars.IsUpper() = True);
  Assert(String.UpperChars.IsLower() = False);

  Assert(String.LowerChars.IsLower() = True);
  Assert(String.LowerChars.IsUpper() = False);

  Assert(String.AlphaChars.IsAlphaNum() = True);
end;
