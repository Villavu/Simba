unit simba.bitmap_finders;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.bitmap, simba.finder;

type
  TMufasaBitmapHelper = class helper for TMufasaBitmap
    function Finder: TSimbaFinder;
  end;

implementation

function TMufasaBitmapHelper.Finder: TSimbaFinder;
begin
  Result.Target.SetBitmap(Self);
end;

end.

