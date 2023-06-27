with Ada.Text_IO;				use Ada.Text_IO;

procedure Main is
	--- Constants ---
	INPUT_FILE: constant String := "../input/input.txt";
	C_PACKET: constant Natural := 4;
	C_MESSAGE: constant Natural := 14;

	--- Functions ---
	function Unique(S: String) return Boolean is
	begin
		for I in S'First .. S'Last - 1 loop
			for J in I + 1 .. S'Last loop
				if S(I) = S(J) then
					return False;
				end if;
			end loop;
		end loop;
		return True;
	end Unique;

	--- Variables ---
	F: File_Type;
begin
	--- Read and parse the input file ---
	Open(F, In_File, INPUT_FILE);
	while not End_Of_File(F) loop
		declare
			Line: String := Get_Line(F);
		begin
			--- Puzzle 1 ---
			for I in Line'Range loop
				if Unique(Line(I .. I + C_PACKET - 1)) then
					Put_Line("Part 1. Start-of-packet marker:" & Integer'Image(I + C_PACKET - 1));
					exit;
				end if;
			end loop;

			--- Puzzle 2 ---
			for I in Line'Range loop
				if Unique(Line(I .. I + C_MESSAGE - 1)) then
					Put_Line("Part 2. Start-of-message marker:" & Integer'Image(I + C_MESSAGE - 1));
					exit;
				end if;
			end loop;
		end;
	end loop;
	Close(F);
end main;
