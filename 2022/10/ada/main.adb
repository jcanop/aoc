with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
	--- Constants ---
	INPUT_FILE: constant String := "../input/input.txt";

	--- Variables ---
	Registry: Integer := 1;
	Cycle: Positive := 1;
	Mark: Integer := 20;
	Total: Integer := 0;
	Buffer: String(1 .. 246);
	Idx: Integer := 1;
	F: File_Type;
begin
	--- Read and parse the input file ---
	Open(F, In_File, INPUT_FILE);
	while not End_Of_File(F) loop
		declare
			Line: String := Get_Line(F);
			C, P: Integer := 0;
		begin
			C := (if Line = "noop" then 1 else 2);
			for I in 1 .. C loop
				--- Puzzle 1 ---
				if Cycle = Mark then
					Total := Total + Registry * Mark;
					Mark := Mark + 40;
				end if;

				--- Puzzle 2 ---
				P := (Cycle - 1) mod 40;
				Buffer(Idx) := (if P >= Registry - 1 and P <= Registry + 1 then '#' else ' ');
				if Cycle mod 40 = 0 then
					Idx := Idx + 1;
					Buffer(Idx) := ASCII.LF;
				end if;
				Idx := Idx + 1;

				--- Both ---
				Cycle := Cycle + 1;
				if C = 2 and I = 2 then
					Registry := Registry + Integer'Value(Line(Line'First + 5 .. Line'Last));
				end if;
			end loop;
		end;
	end loop;
	Close(F);

	Put_Line("1. The sum of the signal strenghts is:" & Integer'Image(Total));
	Put_Line("2. Image on the CRT" & ASCII.LF & Buffer);
end main;
