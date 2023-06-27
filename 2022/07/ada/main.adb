with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
	--- Constants ---
	INPUT_FILE: constant String := "../input/input.txt";
	LIMIT: constant Natural := 100_000;
	DISK_SIZE: constant Natural := 70_000_000;
	UPDATE_SIZE: constant Natural := 30_000_000;

	--- Types ---
	package Maps is new Ada.Containers.Indefinite_Hashed_Maps(
		Key_Type => String,
		Element_Type => Natural,
		Hash => Ada.Strings.Hash,
		Equivalent_Keys => "=");
	use Maps;

	--- Variables ---
	F: File_Type;
	M: Map;
	Dirs: Unbounded_String := To_Unbounded_String("");
begin
	--- Read and parse the input file ---
	Open(F, In_File, INPUT_FILE);
	while not End_Of_File(F) loop
		declare
			Line: String := Get_Line(F);
		begin
			if Line = "$ cd .." then
				declare
					S: String := To_String(Dirs);
				begin
					for I in reverse S'Range loop
						if S(I) = '/' then
							Dirs := To_Unbounded_String(S(S'First .. I - 1));
							exit;
						end if;
					end loop;
				end;
			elsif Line(Line'First .. Line'First + 3) = "$ cd" then
				if Line = "$ cd /" then
					Dirs := Dirs & ".";
				else
					Dirs := Dirs & "/" & Line(Line'First + 5 .. Line'Last);
				end if;
				M.Insert(To_String(Dirs), 0);
			elsif Line = "$ ls" or else Line(Line'First .. Line'First + 3) = "dir " then
				null; -- Ignore ls and dir command
			else
				declare
					S: String := To_String(Dirs);
					N: Natural := 0;
				begin
					for I in Line'Range loop
						if Line(I) = ' ' then
							N := Integer'Value(Line(Line'First .. I - 1));
							exit;
						end if;
					end loop;

					M(S) := M(S) + N;
					for I in reverse S'Range loop
						if S(I) = '/' then
							M(S(S'First .. I - 1)) := M(S(S'First .. I - 1)) + N;
						end if;
					end loop;
				end;
			end if;
		end;
	end loop;
	Close(F);

	--- Puzzle 1 ---
	declare
		T: Natural := 0;
	begin
		for I in M.Iterate loop
			if M(I) <= LIMIT then
				T := T + M(I);
			end if;
		end loop;
		Put_Line("1. The sum of the total sizes of the directories is" & Integer'Image(T));
	end;

	--- Puzzle 2 ---
	declare
		Need: Natural := UPDATE_SIZE - DISK_SIZE + M(".");
		D: Natural := Natural'Last;
	begin
		for I in M.Iterate loop
			if M(I) >= Need and D > M(I) then
				D := M(I);
			end if;
		end loop;
		Put_Line("2. The total size of the smalles directory needed is" & Integer'Image(D));
	end;
end main;
