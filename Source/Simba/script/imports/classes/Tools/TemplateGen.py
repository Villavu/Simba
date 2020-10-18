import base64

test = b'''unit lp$ClassName;
//Depends: $Depends

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper;

procedure Register_$ClassName(Compiler: TScriptCompiler);

implementation

type
  $ClassNameP = ^$ClassName;

$Methods

procedure Register_$ClassName(Compiler: TScriptCompiler);
begin
  with Compiler do
  begin
    addClass(Compiler, '$ClassName', '$ClassParent');

    $Registers
  end;
end;

end.
'''

print(base64.b64encode(test))