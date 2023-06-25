with Ada.Strings.Fixed;	use Ada.Strings.Fixed;
with Ada.Text_IO;		use Ada.Text_IO;

procedure Main is
	--- Constants ---
	INPUT_FILE: constant String := "../input/input.txt";

	--- Variables ---
	F: File_Type;
	C1, C2 : Integer := 0;
begin
	--- Read and parse the input file ---
	Open(F, In_File, INPUT_FILE);
	while not End_Of_File(F) loop
		declare
			Line: String := Get_Line(F);
			I: Natural := Index(Source => Line, Pattern => ",");
			J: Natural := Index(Source => Line, Pattern => "-");
			K: Natural := Index(Source => Line, Pattern => "-", From => I);
			A1: Integer := Integer'Value(Line(1 .. J - 1));
			A2: Integer := Integer'Value(Line(J + 1 .. I - 1));
			B1: Integer := Integer'Value(Line(I + 1 .. K - 1));
			B2: Integer := Integer'Value(Line(K + 1 .. Line'Length));
		begin
			--- Puzzle 1 --
			if (A1 <= B1 and A2 >= B2) or else (A1 >= B1 and A2 <= B2) then
				C1 := C1 + 1;
			end if;

			--- Puzzle 2 ---
			if  (A1 >= B1 and A1 <= B2) or else
				(A2 >= B1 and A2 <= B2) or else
				(B1 >= A1 and B1 <= A2) or else
				(B2 >= A1 and B2 <= A2) then
				C2 := C2 + 1;
			end if;
		end;
	end loop;
	Close(F);

	--- Puzzle 1 ---
	Put_Line("Part 1. Total of ranges that fully contains another:" & Integer'Image(C1));

	--- Puzzle 2 ---
	Put_Line("Part 2. Total of ranges that overlaps:" & Integer'Image(C2));
end main;
