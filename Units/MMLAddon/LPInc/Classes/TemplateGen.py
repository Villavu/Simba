import base64

test = b'''unit lp$ClassName;
//Depends: $Depends

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper;

procedure Register_$ClassName(Compiler: TLapeCompiler);

implementation

type
  $ClassNameP = ^$ClassName;

$Methods

procedure Register_$ClassName(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('$ClassName', '$ClassParent');

    $Registers
  end;
end;

end.
'''

print(base64.b64encode(test))