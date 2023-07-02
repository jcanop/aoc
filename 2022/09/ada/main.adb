with Ada.Containers; use Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Hash;

procedure Main is
	--- Constants ---
	INPUT_FILE: constant String := "../input/input.txt";

	--- Types ---
	package Sets is new Ada.Containers.Indefinite_Hashed_Sets(
		Element_Type => String,
		Hash => Ada.Strings.Hash,
		Equivalent_Elements => "=");
	use Sets;

	type Direction is (Up, Down, Left, Right);
	type Point is record
		X, Y: Integer;
	end record;
	type Rope is array (Natural range <>) of Point;

	--- Exception ---
	ParseException: Exception;

	--- Functions ---
	function Signum(I: Integer)  return Integer is
	begin
		if I >= 1 then return 1; end if;
		if I <= -1 then return -1; end if;
		return 0;
	end Signum;

	function Parse(C: Character) return Direction is
	begin
		case C is
			when 'U' => return Up;
			when 'D' => return Down;
			when 'L' => return Left;
			when 'R' => return Right;
			when others => raise ParseException with "Usupported character";
		end case;
	end Parse;

	procedure Move(R: in out Rope; D: Direction) is
	begin
		case D is
			when Up    => R(0).Y := R(0).Y - 1;
			when Down  => R(0).Y := R(0).Y + 1;
			when Left  => R(0).X := R(0).X - 1;
			when Right => R(0).X := R(0).X + 1;
		end case;

		for I in 1 .. R'Last loop
			declare
				DX: Integer := R(I - 1).X - R(I).X;
				DY: Integer := R(I - 1).y - R(I).Y;
			begin
				exit when abs(DX) < 2 and abs(DY) < 2;
				R(I).X := R(I).X + Signum(DX);
				R(I).Y := R(I).Y + Signum(DY);
			end;
		end loop;
	end Move;

	--- Variables ---
	F: File_Type;
	R1: Rope(0 .. 1);
	R2: Rope(0 .. 9);
	T1: Point;
	T2: Point;
	S1, S2: Set;
begin
	for P of R1 loop P.X := 0; P.Y := 0; end loop;
	for P of R2 loop P.X := 0; P.Y := 0; end loop;

	--- Read and parse the input file ---
	Open(F, In_File, INPUT_FILE);
	while not End_Of_File(F) loop
		declare
			Line: String := Get_Line(F);
			D: Direction;
			S: Integer;
		begin
			D := Parse(Line(Line'First));
			S := Integer'Value(Line(Line'First + 2 .. Line'Last));
			for I in 1 .. S loop
				Move(R1, D);
				Move(R2, D);
				T1 := R1(R1'Last);
				T2 := R2(R2'Last);
				S1.Include(Integer'Image(T1.X) & "," & Integer'Image(T1.Y));
				S2.Include(Integer'Image(T2.X) & "," & Integer'Image(T2.Y));
			end loop;
		end;
	end loop;
	Close(F);

	Put_Line("1. Positions visited at least once:" & Count_Type'Image(S1.Length));
	Put_Line("2. Positions visited at least once:" & Count_Type'Image(S2.Length));
end main;
