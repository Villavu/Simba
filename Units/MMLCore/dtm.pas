unit dtm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
    TMDTM = class(TObject)
          constructor Create(Owner: TObject);
          destructor Destroy; override;
    private
           Client: TObject;

    end;

{
 I am not sure wether I should simply copy and paste the old DTM implementation,
 or rewrite it from scratch.

 I recall there was something partially wrong with SCAR-alike DTM conversions
 to Mufasa DTM's...

 The old DTM system problaby doesn't perform that well, but seems to be quite
 stable and complete.

 If I would rewrite it from scratch, it would probably be faster, and
 hopefully more efficient.That won't be too hard, especially since I have
 direct data access now. (TClient FTW!)

 Rewrite from scratch it will be, I guess.
 And AreaShape will be turned into a {$I }, inline simply doesn't cut it.

 ~Wizz
}

implementation
uses
    Client;

constructor TMDTM.Create(Owner: TObject);
begin
  inherited Create;
  Self.Client := Owner;
end;

destructor TMDTM.Destroy;

begin
  //Something
  inherited Destroy;
end;

end.

