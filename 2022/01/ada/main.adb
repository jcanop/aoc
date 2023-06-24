with Ada.Containers.Vectors;
with Ada.Text_IO;				use Ada.Text_IO;

procedure Main is
	--- Constants ---
	INPUT_FILE: constant String := "../input/input.txt";

	--- Types ---
	package List is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Integer);
	package List_Sorting is new List.Generic_Sorting;

	--- Variables ---
	F: File_Type;
	C: Integer := 0;
	L: List.Vector;
begin
	--- Read and parse the input file ---
	Open(F, In_File, INPUT_FILE);
	while not End_Of_File(F) loop
		declare
			Line: String := Get_Line(F);
		begin
			if Line'length = 0 then
				L.Append(C);
				C := 0;
			else
				C := C + Integer'Value(Line);
			end if;
		end;
	end loop;
	Close(F);

	L.Append(C);
	List_Sorting.Sort(L);

	--- Puzzle 1 ---
	Put_Line("The Elf carrying the most Calories, is carrying" &
		Integer'Image(L.Last_Element) &
		" Calories");

	--- Puzzle 2 ---
	C := 0;
	for I in L.Last_Index - 2 .. L.Last_Index loop
		C := C + L(I);
	end loop;
	Put_Line("The top 3 Elves carrying the most Calores, are carrying" &
		Integer'Image(C) &
		" Calories");
end main;
