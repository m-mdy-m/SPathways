program Aliases;
type 
  intptr = ^integer; 
var 
  x, y: intptr; 
begin
  new(x); 
  x^ := 1; 
  y := x; 
  y^ := 2; 
  writeln(x^); 
end.