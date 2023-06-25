with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Text_IO;			use Ada.Text_IO;

procedure Main is
	--- Constants ---
	INPUT_FILE: constant String := "../input/input.txt";

	--- Types ---
	type Count is range 1 .. 3;

	--- Exceptions --
	Parse_Exception: exception;
	Not_Found_Exception: exception;

	--- Functions ---
	function Find_1(S: String) return Integer is
		N: Positive := S'Length / 2;
	begin
		if N = 0 then
			raise Parse_Exception with "Empty String";
		end if;

		for I in 1 .. N loop
			for J in N + 1 .. 2 * N loop
				if S(I) = S(J) then
					if S(I) >=  'a' and  S(I) <= 'z' then
						return Character'Pos(S(I)) - Character'Pos('a') + 1;
					elsif S(I) >= 'A' and S(I) <= 'Z' then
						return Character'Pos(S(I)) - Character'Pos('A') + 27;
					end if;
				end if;
			end loop;
		end loop;

		raise Not_Found_Exception with "Duplicated char not found";
	end Find_1;

	function Find_2(S1, S2, S3: String) return Integer is
	begin
		for I in 1 .. S1'Length loop
			for J in 1 .. S2'Length loop
				if S1(I) = S2(J) then
					for K in 1 .. S3'Length loop
						if S1(I) = S3(K) then
							if S1(I) >=  'a' and  S1(I) <= 'z' then
								return Character'Pos(S1(I)) - Character'Pos('a') + 1;
							elsif S1(I) >= 'A' and S1(I) <= 'Z' then
								return Character'Pos(S1(I)) - Character'Pos('A') + 27;
							end if;
						end if;
					end loop;
				end if;
			end loop;
		end loop;
		raise Not_Found_Exception with "Duplicated char not found";
	end Find_2;

	--- Variables ---
	F: File_Type;
	S1, S2, S3: Unbounded_String;
	N: Count := 1;
	C1, C2: Integer := 0;
begin
	--- Read and parse the input file ---
	Open(F, In_File, INPUT_FILE);
	while not End_Of_File(F) loop
		declare
			Line: String := Get_Line(F);
		begin
			--- Puzzle 1 ---
			C1 := C1 + Find_1(Line);

			--- Puzzle 2 ---
			case N is
				when 1 =>
					S1 := To_Unbounded_String(Line);
					N := 2;
				when 2 =>
					S2 := To_Unbounded_String(Line);
					N := 3;
				when 3 =>
					S3 := To_Unbounded_String(Line);
					C2 := C2 +  Find_2(To_String(S1), To_String(S2), To_String(S3));
					N := 1;
			end case;
		end;
	end loop;
	Close(F);

	--- Puzzle 1 ---
	Put_Line("Part 1. Total sum of the priorities:" & Integer'Image(C1));

	--- Puzzle 2 ---
	Put_Line("Part 2. Total sum of the item types:" & Integer'Image(C2));
end Main;
