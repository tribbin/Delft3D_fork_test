The structure for developer documentation per project:

.\docs
|-<project>-mainpage.dox -> Contains main page information for the project.
|-<project>-doxyfile.dxg -> Doxygen settings for the project
|-\src\ -> Contains additional dox or tex files. Can have subfolders. Each new doc file could be a new page.
|-\inc\ -> Contains drawio/png/jpg/dot and other media source files that can be included in the doxygen documentation
|-\hdr\ -> COntains project specific logos, header images, etc.


..\result_doc -> Doxygen output directory per project. Not part of git.



Starting with Doxygen -> https://www.doxygen.nl/manual/starting.html


Using Pandoc to convert Word-example: .\pandoc.exe --extract-media=images -s .\input.docx -t markdown -o .\output.md