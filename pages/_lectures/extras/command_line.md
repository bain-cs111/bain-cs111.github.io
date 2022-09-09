### {{num}}. Review the Command Line Cheatsheet

For your convenience, we have made you a little cheatsheet to help you get familiar with the command line. <a href="https://tutorial.djangogirls.org/en/intro_to_command_line/" target="_blank">Django Girls</a> is a good resource. Note: you will not be tested on this or anything, but navigating the command line can be useful.  DO NOT ENTER THE `>` or `$` symbols! Those are there automatically in Command Prompt and Terminal respectively.

{:.instructions}
| | DOS (Windows) | Shell (Mac / Linux) |
|--|--|--|
| **What directory am I in?** | `> cd` | `$ pwd` |
| **Change directories** | `> cd {{your_file_path}}` | `$ cd {{your_file_path}}` |
| **List files & directories** | `> dir`<br> `> tree  # lists subdirectories` | `$ ls`<br>`$ ls -l` |
| **Navigate to parent directory** | `> cd ..` | `$ cd  ..` |
| **Navigate into child directory** | `> cd cs110` | `$ cd cs110` |
| **Navigate into descendant directory** | `> cd lectures\lecture_03` | `$ cd lectures/lecture_03` |
| **Navigate to sibling directory** | `> cd ..\lecture_02` | `$ cd  ../lecture_02` |
| **Navigate to ancestor directory** | `> cd ..\..\` | `$ cd  ../../` |
| **Navigate to home directory** |  | `$ cd` |
| **Command history** | `> doskey /HISTORY` | `$ history` |

Other optional commands you may find useful...

{:.instructions}
| | DOS (Windows) | Shell (Mac / Linux) |
|--|--|--|
| **Create a new file** | `> echo . > my_file.txt` | `$ echo . > my_file.txt`<br> `$ touch my_file.txt` |
| **Append to a file** | `> echo "some text" >> my_file.txt` | `$ echo "some text" > my_file.txt` |
| **Save history to a file** | `> doskey /HISTORY > my_history.txt` | `$ history > my_history.txt` |
| **Move a file** | `> move my_history.txt Documents/.` | `$ mv my_history.txt Documents/.` |
| **Make a folder** | `> mkdir my_folder` | `$ mkdir my_folder` |
| **Delete a file** | `> del my_history.txt` | `$ rm my_history.txt` |
| **Delete a folder** | `> rmdir my_folder` | `$ rm -r my_folder` |
