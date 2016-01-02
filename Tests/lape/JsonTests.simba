program new;

procedure testCreateString;
var
  json: TJSONObject;
begin
  json.Init('{exemplo:"valor", inteiro:1}');
  try
    if (json.opt('exemplo') <> nil) then
      WriteLn(json.optString('exemplo'));
  finally
    json.free;
  end;
end;

procedure testUnicodeCharacters;
var
  json: TJSONObject;
begin
  json.Init('{exemplo:"\u0046\u0041\u0042\u0049\u004F"}');
  try
    if (json.opt('exemplo') <> nil) then
      WriteLn(json.optString('exemplo'));
  finally
    json.free;
  end;
end;

procedure testOptInt;
var
  json: TJSONObject;
begin
  try
    json.Init('{exemplo:"valor", inteiro:"1"}');
    if (json.optInt('inteiro') = 1) then
      WriteLn('OptInt Success!');
  finally
    if Assigned(json) then
      json.free;
  end;
end;

procedure testNames;
var
  json: TJSONObject;
  a: TJSONArray;
begin
  try
    json.Init('{exemplo:"valor", inteiro:1}');
    a := json.names;
    if a.length = 2 then
      WriteLn('testNames success!');
  finally
    if Assigned(json) then
      json.free;
    if Assigned(a) then
      a.free;
  end;
end;

procedure testAssignTo;
var
  json, jo: TJSONObject;
begin
  try
    jo.Init('{exemplo:"valor", inteiro:1}');
    json.Init;
    jo.assignTo(json);
    if json.getInt('inteiro') = 1 then
      WriteLn('AssignTo Success!');
  finally
    if (Assigned(json)) then
      json.free;
    if (Assigned(jo)) then
      jo.free
  end;
end;

procedure testRSJsonAPI;
var
json,Obj: TJsonObject;
Arr: TJsonArray;
i: integer;
Len: integer;
begin
 try
 Json.Init(getPage('http://services.runescape.com/m=itemdb_rs/api/catalogue/category.json?category=1'));
 Arr := json.getJSONArray('alpha');
 Len := Arr.length();
 WriteLn('Array length = '+IntToStr(Len));
 //Obj := Arr.getJSONObject(1);
 //WriteLn(Obj.GetString('letter'));
 WriteLn(Arr.ToString);
 For i := 0 to Len-1 do
  begin
   Obj := Arr.getJSONObject(i);
   WriteLn('Letter' + Obj.GetString('letter'));
   WriteLn('Items count = ' + Obj.GetInt('items'));
  end;
 //WriteLn(Json.ToString);}
 finally
  if Assigned(Json) then
   Json.free;
  If Assigned(Arr) then
   Arr.Free;
 end;
end;

begin
  testCreateString;
  testUnicodeCharacters;
  testOptInt;
  testNames;
  testAssignTo;
  testRSJsonAPI;
end;
