# ToolboxFortran
This repo contains numerical routines written in Fortran. I draw from a number of sources and give proper acknoledgments. If you see that something is not cited properly, please let me know and I will rectify the situation immediately.

**HOW TO COMPILE IT**

I tested the code with the Intel Fortran compiler on a Windows machine. Navigate to the folder where the source files are located. In order to compile the code and create an executable, type the following:

`ifort mod_numerical.f90 mytest.f90 -o run.exe  `

This generates a Windows executable called run.exe. To run the code, type:

`run.exe  `

Alternatively, one can use Visual Studio which is a nice IDE for Fortran on Windows.

**REMARKS**

Some of the programs use a toolbox that accompanies the excellent textbook 
* "Fehr and Kindermann (2018), Introduction to Computational Economics Using Fortran, Oxford University Press." 

Fehr and Kindermann's toolbox is available here: https://github.com/fabiankindermann/ce-fortran/

This code may contain bugs and errors. Pull requests and suggestions are more than welcome.

