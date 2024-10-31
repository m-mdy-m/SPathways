with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Calculator is
   first_num, second_num : Integer;
   sum,sub,div,mul : Integer;
begin
    Put("Enter first number: ");
    Get(first_num);
    Put("Enter second number: ");
    Get(second_num);
    sum := first_num + second_num;
    sub := first_num - second_num;
    mul := first_num * second_num;
    div := first_num / second_num;
    Put_Line(Integer'Image(first_num) & " + " & Integer'Image(second_num) & " = " & Integer'Image(sum));
    Put_Line(Integer'Image(first_num) & " - " & Integer'Image(second_num) & " = " & Integer'Image(sub));
    Put_Line(Integer'Image(first_num) & " * " & Integer'Image(second_num) & " = " & Integer'Image(mul));
    Put_Line(Integer'Image(first_num) & " / " & Integer'Image(second_num) & " = " & Integer'Image(div));
end Calculator;