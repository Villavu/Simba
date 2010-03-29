unit bzip2comn;

interface

const 
  max_groups     = 6;
  max_alpha_size = 258;
  max_code_len   = 23;
  group_size     = 50;
  iter_count     = 4;
  max_selectors  = 2+(900000 div group_size);

  mtfa_size      = 4096;
  mtfl_size      = 16;

type
  TCardinal_array = array [0..899999] of Cardinal;
  PCardinal_array = ^TCardinal_array;

  PCardinal  = ^Cardinal;
  Thuffarray = array[0..max_alpha_size] of Cardinal;
  Phuffarray = ^Thuffarray;

{A bzip2 stream starts with this:}
const bzip2_stream_magic='BZh';

{Error codes for stream errorinfo.}
const 
  bzip2_bad_header_magic        = 1;
  bzip2_bad_block_magic         = 2;
  bzip2_endoffile               = 3;
  bzip2_data_error              = 4;

procedure hb_create_decode_tables(var limit,base,perm:array of cardinal;
                                  var length:array of byte;
                                  minlen,maxlen:byte;alphasize:cardinal);


implementation

procedure hb_create_decode_tables(var limit,base,perm:array of cardinal;
                                  var length:array of byte;
                                  minlen,maxlen:byte;alphasize:cardinal);

var pp,i,j,vec:cardinal;

begin
  pp:=0;
  for i:=minlen to maxlen do
    for j:=0 to alphasize-1 do
      if length[j]=i then
        begin
          perm[pp]:=j;
          inc(pp);
        end;
  for i:=0 to max_code_len-1 do
    begin
      base[i]:=0;
      limit[i]:=0;
    end;
  for i:=0 to alphasize-1 do
    inc(base[length[i]+1]);
  for i:=1 to max_code_len-1 do
    inc(base[i],base[i-1]);
  vec:=0;
  for i:=minlen to maxlen do
    begin
      inc(vec,base[i+1]-base[i]);
      limit[i]:=vec-1;
      vec:=vec shl 1;
    end;
  for i:=minlen+1 to maxlen do
    base[i]:=((limit[i-1]+1) shl 1)-base[i];
end;

end.