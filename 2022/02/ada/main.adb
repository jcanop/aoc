with Ada.Text_IO;	use Ada.Text_IO;

procedure Main is
	--- Constants ---
	INPUT_FILE: constant String := "../input/input.txt";

	--- Types ---
	type Result is (Lose, Draw, Win);
	type Shape is (Rock, Paper, Scissors);

	--- Exceptions ---
	Parse_Exception: exception;

	--- Functions ---
	function Value(R: Result) return Integer is
	begin
		case R is
			when Lose => return 0;
			when Draw => return 3;
			when Win => return 6;
		end case;
	end Value;

	function Value(S: Shape) return Integer is
	begin
		case S is
			when Rock => return 1;
			when Paper => return 2;
			when Scissors => return 3;
		end case;
	end Value;

	function Parse(C: Character) return Result is
	begin
		case C is
			when 'X' => return Lose;
			when 'Y' => return Draw;
			when 'Z' => return Win;
			when others => raise Parse_Exception with "Unknown Result";
		end case;
	end Parse;

	function Parse(C: Character) return Shape is
	begin
		case C is
			when 'X' | 'A' => return Rock;
			when 'Y' | 'B' => return Paper;
			when 'Z' | 'C' => return Scissors;
			when others => raise Parse_Exception with "Unknown Shape";
		end case;
	end Parse;

	function Shape_To(S: Shape; R: Result) return Shape is
	begin
		case R is
			when WIN =>
				case S is
					when Rock => return Paper;
					when Paper => return Scissors;
					when Scissors => return Rock;
				end case;
			when Lose =>
				case S is
					when Rock => return Scissors;
					when Paper => return Rock;
					when Scissors => return Paper;
				end case;
			when Draw => return S;
		end case;
	end Shape_To;

	function Match(M: Shape; O: Shape) return Result is
	begin
		if M = O then
			return Draw;
		end if;
		if  (M = Rock and O = Scissors) or
			(M = Scissors and O = Paper) or
			(M = Paper and O = Rock) then
			return Win;
		end if;
		return Lose;
	end Match;

	function Play(M: Shape; O: Shape) return Integer is
		C: Integer := Value(M);
	begin
		return C + Value(Match(M, O));
	end Play;

	--- Variables ---
	F: File_Type;
	R: Result;
	M, O: Shape;
	C1, C2: Integer := 0;
begin
	--- Read and parse the input file ---
	Open(F, In_File, INPUT_FILE);
	while not End_Of_File(F) loop
		declare
			Line: String := Get_Line(F);
		begin
			--- Puzzle 1 ---
			O := Parse(Line(1));
			M := Parse(Line(3));
			C1 := C1 + Play(M, O);

			--- Puzzle 2 ---
			R := Parse(Line(3));
			M := Shape_To(O, R);
			C2 := C2 + Play(M, O);
		end;
	end loop;
	Close(F);

	--- Puzzle 1 ---
	Put_Line("Part 1. Total score:" & Integer'Image(C1));

	--- Puzzle 2 ---
	Put_Line("Part 2. Total score:" & Integer'Image(C2));
end Main;
