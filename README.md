# [Final Project](https://github.com/cejiogu/final_project)
By: [Chimdi Ejiogu](https://github.com/cejiogu/), [Ori Wachman](https://github.com/cejiogu/), [Ilan Klimberg](https://github.coecis.cornell.edu/idk7)

### Introduction
This application is a Database Management System that allows the user to SQL to create and manage their own database. The program, primarily built in OCaml, utilizes a command-line interface (CLI), relying on the terminal to guide the user in how to use the application and to receive the inputs of the client.


### Features and Functionality
In this program, the user can:
- Create empty tables in their database
- Request a schema of their database
- Select specific columns from specific tables in their database
- Insert columns into already-existing tables in their database
- Insert values into already-existing tables in their database
- Select the maximum value in a column in a table in their database
- Select the minimum value in a column in a table in their database
- "Truncate" a given table in their database, thereby removing all data in the specified table
- Perform an inner join between two already-existing tables
- Select specific values from specified columns of a table


### Installation and Use

To install this application, consult the INSTALL.md file in the application's root directory. After having installed the application, you may then use the application. To use this application, the client must first build the application. 

#### Building Application

The following steps outline how to build the application

1) Run the command ``` make c ``` in the terminal
2) Run the command ``` make b ``` in the terminal

After building the application, the client must then run the application by running the command ``` make e ``` in the terminal.

#### Using Application
To make use of the functionalities of the application, apply any of the commands printed to the terminal.
