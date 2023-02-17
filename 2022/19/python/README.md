# Python/C Solution

## Introduction
Phyton is a powerful and versatile programming language but is slower than C or Rust. The script named `main.py` is a pure python implementation of the solution and can take a while to get the answer (20 min. in my testing machine).

Looking around the web, I tried a few tips to improve the performance of the code, but I only got marginal gains. So, I did what most python modules did to burst their performance: I used C code to do the heavy lifting. The script named `main_c.py` is the implementation using the C library.

## Running the Python/C version
You'll need to compile the C library for your system. In Linux/BSD/MacOS systems, you can use GCC and make for this.

```bash
# Go to the C solution
cd aoc/2022/19/c

# Compile the library
make lib

# Move the library to the Python solution
mv build/libc.so ../python

# Run the code
cd ../python
python3 main_c.py
```

