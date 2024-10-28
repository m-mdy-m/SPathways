with Ada.Text_IO; use Ada.Text_IO;
procedure Hello is
begin
   Put_Line("Integer: " & Integer'Image(5));
   Put_Line("Float"& Float'Image(3.14));
   Put_Line("Character"&Character'Image('A'));
   Put_Line("String: Hello, World!");
end Hello;