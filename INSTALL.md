# Installation Manual

This manual outlines the steps to install the application from its GitHub repository,
and how to build the application on another device.

## Prerequisites
To run this application, you must ensure that your system:
- Already has OPAM, the OCaml Package Manager, installed
  - If your system does not already have OPAM installed, you can install it by following this [guide](https://cs3110.github.io/textbook/chapters/preface/install.html) 
- Already has an OPAM switch with the OCAML compiler and certain relevant packages
  - If your system does not already have the aforementioned installed, you can install it by following this [guide](https://cs3110.github.io/textbook/chapters/preface/install.html)
- Already has the Visual Studio Code editor, with OCaml support
  - If your system does not already have the aforementioned installed, you can install it by following this [guide](https://cs3110.github.io/textbook/chapters/preface/install.html)
- Is up to date
  - If your system is not up to date, you can update it by running the command ``` opam update ``` in the terminal
- Already has _dune_, the build system that the application uses, installed
  - If your system does not already have _dune_ installed, you can install _dune_ by running the command ``` opam install dune ``` in the terminal
- Already has installed the opam package ppx_inline_test in your switch.
  - If your system does not already have this package installed, you can install this package by running the command ``` opam install ppx_inline_test``` in the terminal 
## Installation Steps
To install the application:
1) Clone the repository of the application, linked [here](https://github.coecis.cornell.edu/ce248/final_project), into your computer
    - For instructions on how to clone a repository, consult this [guide](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository)
2) Change the location of your current working directory to the directory of the application, titled "final_project"

Having followed this instructions, you have completed the installation of this application.For instructions on how to use the application, consult the README.md file