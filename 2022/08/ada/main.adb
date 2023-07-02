with Ada.Containers.Vectors;
with Ada.Text_IO;				use Ada.Text_IO;

procedure Main is
	--- Constants ---
	INPUT_FILE: constant String := "../input/input.txt";

	--- Exception ---
	UnreachableException: exception;

	--- Variables ---
	MX, MY: Natural := 0;
	F: File_Type;
begin
	--- Find Matrix size ---
	Open(F, In_File, INPUT_FILE);
	while not End_Of_File(F) loop
		declare
			Line: String := Get_Line(F);
		begin
			if MX < Line'Length then
				MX := Line'Length;
			end if;
			MY := MY + 1;
		end;
	end loop;
	Close(F);

	declare
		--- Matrix Type ---
		type Tree is range 0 .. 9;
		type Matrix is array(0 .. MX - 1, 0 .. MY - 1) of Tree;

		--- Functions ---
		function Is_Visible(M: Matrix; X, Y: Natural) return Boolean is
		begin
			for I in Y + 1 .. MY - 1 loop
				if M(X, I) >= M(X, Y) then exit; end if;
				if I = MY - 1 then return True; end if;
			end loop;

			for I in reverse 0 .. Y - 1 loop
				if M(X, I) >= M(X, Y) then exit; end if;
				if I = 0 then return True; end if;
			end loop;

			for I in X + 1 .. MX - 1 loop
				if M(I, Y) >= M(X, Y) then exit; end if;
				if I = MX - 1 then return True; end if;
			end loop;

			for I in reverse 0 .. X - 1 loop
				if M(I, Y) >= M(X, Y) then return False; end if;
				if I = 0 then return True; end if;
			end loop;

			raise UnreachableException;
		end Is_Visible;

		function Score(M: Matrix; X, Y: Natural) return Integer is
			A, B, C, D: Integer := 1;
		begin
			for I in Y + 1 .. MY - 2 loop
				exit when  M(X, I) >= M(X, Y);
				A := A + 1;
			end loop;

			for I in reverse 1 .. Y - 1 loop
				exit when M(X, I) >= M(X, Y);
				B := B + 1;
			end loop;

			for I in X + 1 .. MX - 2 loop
				exit when M(I, Y) >= M(X, Y);
				C := C + 1;
			end loop;

			for I in reverse 1 .. X - 1 loop
				exit when M(I, Y) >= M(X, Y);
				D := D + 1;
			end loop;

			return A * B * C * D;
		end Score;

		--- Variables ---
		M: Matrix;
		X, Y: Natural := 0;
	begin
		--- Read and parse the input file ---
		Open(F, In_File, INPUT_FILE);
		while not End_Of_File(F) loop
			declare
				Line: String := Get_Line(F);
			begin
				X := 0;
				for C of Line loop
					M(X, Y) := Tree'Value((1 => C));
					X := X + 1;
				end loop;
				Y := Y + 1;
			end;
		end loop;
		Close(F);

		--- Puzzle 1 ---
		declare
			T: Integer := (MX + MY - 2) * 2;
		begin
			for X in 1 .. MX - 2 loop
				for Y in 1 .. MY - 2 loop
					if Is_Visible(M, X, Y) then
						T := T + 1;
					end if;
				end loop;
			end loop;
			Put_Line("1. Trees that are visible from aoutside the grid:" & Integer'Image(T));
		end;

		--- Puzzle 2 ---
		declare
			S, Max: Integer := 0;
		begin
			for X in 1 .. MX - 2 loop
				for Y in 1 .. MY - 2 loop
					S := Score(M, X, Y);
					if S > Max then Max := S; end if;
				end loop;
			end loop;
			Put_Line("2. The highest scenic score is:" & Integer'Image(Max));
		end;
	end;
end main;
