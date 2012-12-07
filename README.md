# beadgame

Implementation of the chainshot game written in Clojure!

All the ladies love Clojure!

## Usage


You can run the already built jar
    ` java -jar target/beadgame-3.0-standalone.jar `  

Or

To compile the code and package it into a jar, just run:  
    ` lein uberjar ` 

Then you can just run the standalone jar that's included in the target.  
    ` java -jar target/beadgame-3.0-standalone.jar `  


When you start the game it will ask you for an initial board file, this board file is a space delimited columns, new line delimited rows. Specify a color from A-Z (caps) if you want a visual guide.
Example initial boards are provided as startBoard, startBoard2 ... 
Test boards are under test-boards/

The game will explain the rest of the instructions

## Simple flow chart
Check flowchart.txt

## License

Distributed under GPL
