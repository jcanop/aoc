with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;
with Ada.Text_IO;			use Ada.Text_IO;
with Gnat.Regpat;			use Gnat.Regpat;

procedure Main is
	--- Constants ---
	INPUT_FILE: constant String := "../input/input.txt";

	--- Types ---
	package Queue is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Character);
	type Index is range 1 .. 9;
	type List is array(Index) of Queue.Vector;

	--- Exceptions ---
	Parse_Exception: Exception;

	--- Functions ---
	function RE_Search(Expression: Pattern_Matcher; S: String; First, Last: out Positive) return Boolean is
		Found: Boolean;
		Result: Match_Array(0 .. 1);
	begin
		Match(Expression, S, Result);
		Found := not "="(Result(1), No_Match);
		if Found then
			First := Result(1).First;
			Last := Result(1).Last;
		end if;
		return Found;
	end RE_Search;

	--- Variables ---
	F: File_Type;
	RE1: Pattern_Matcher := Compile("((\[\w\]|\s{3})\s?)");
	RE2: Pattern_Matcher := Compile("(\d+)");
	Parsing_Crates: Boolean := True;
	L1: List;
	L2: List;
begin
	--- Read and parse the input file ---
	Open(F, In_File, INPUT_FILE);
	while not End_Of_File(F) loop
		declare
			Line: String := Get_Line(F);
			I, First,Last: Positive;
			Move: Integer;
			From, To: Index;
			C: Integer := 1;
			E: Character;
		begin
			if Parsing_Crates then
				--- Parsing Crates ---
				I := Line'First;
				while RE_Search(RE1, Line(I .. Line'Last), First, Last) loop
					if Line(First + 1) /= ' ' then
						L1(Index(C)).append(Line(First + 1));
						L2(Index(C)).append(Line(First + 1));
					end if;
					I := Last + 1;
					C := C + 1;
				end loop;
				Parsing_Crates := Line /= "";
			else
				--- Parsing Moves ---
				if not RE_Search(RE2, Line, First, Last) then
					raise Parse_Exception with "Move value not found";
				end if;
				Move := Integer'Value(Line(First .. Last));
				if not RE_Search(RE2, Line(Last + 1 .. Line'Last), First, Last) then
					raise Parse_Exception with "From value not found";
				end if;
				From := Index'Value(Line(First .. Last));
				if not RE_Search(RE2, Line(Last + 1 .. Line'Last), First, Last) then
					raise Parse_Exception with "To value not found";
				end if;
				To := Index'Value(Line(First .. Last));

				--- Perform Moves ---
				for N in reverse 0 .. Move - 1 loop
					--- Puzzle 1 ---
					E := L1(From)(0);
					L1(From).Delete(0);
					L1(To).Prepend(E);

					--- Puzzle 2 ---
					E := L2(From)(N);
					L2(From).Delete(N);
					L2(To).Prepend(E);
				end loop;
			end if;
		end;
	end loop;
	Close(F);

	--- Print Results ---
	declare
		S1: Unbounded_String := To_Unbounded_String("");
		S2: Unbounded_String := To_Unbounded_String("");
	begin
		for I in Index'Range loop
			S1 := S1 & L1(I)(0);
			S2 := S2 & L2(I)(0);
		end loop;
		Put_Line("1. Crates on the top of each stack: " & To_String(S1));
		Put_Line("2. Crates on the top of each stack: " & To_String(S2));
	end;
end main;
