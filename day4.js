const fs = require('node:fs');

function processFileData(data) {
    let gridArr = []
    let lines = data.split('\n');
    lines.forEach(element => {
        lineArr = []
        for (var i = 0; i < element.length; ++i) {
            if (element[i] == '\r') continue;
            lineArr.push(element[i]);
        }
        gridArr.push(lineArr);
    });
    
    part1(gridArr);
    part2(gridArr);
}

function part2(gridArr) {
    var moved = true;
    var allMoved = 0;
    while(moved) {
        moved = false;
        for (var y = 0; y < gridArr.length; y++) {
            dispString = "";
            for (var x = 0; x < gridArr[0].length; x++) {
                if (gridArr[y][x] == '@') {
                    var border_count = 0;
                    // Check the row above
                    if (y > 0) {
                        if (x > 0 && gridArr[y - 1][x - 1] == "@") {
                            border_count++;
                        }
                        if (gridArr[y - 1][x] == "@") {
                            border_count++;
                        }
                        if (x < gridArr[0].length - 1 && gridArr[y - 1][x + 1] == "@") {
                            border_count++;
                        }
                    }
                    // Check left and right
                    if (x > 0 && gridArr[y][x - 1] == "@") {
                        border_count++;
                    }
                    if (x < gridArr[0].length - 1 && gridArr[y][x + 1] == "@") {
                        border_count++;
                    }           
                    // Check the row below
                    if (y < gridArr.length - 1) {
                        if (x > 0 && gridArr[y + 1][x - 1] == "@") {
                            border_count++;
                        }
                        if (gridArr[y + 1][x] == "@") {
                            border_count++;
                        }
                        if (x < gridArr[0].length - 1 && gridArr[y + 1][x + 1] == "@") {
                            border_count++;
                        }
                    }
    
                    if (border_count < 4) {
                        moved = true;
                        allMoved++;
                        dispString = dispString + 'x';
                        gridArr[y][x] = '.';
                    } else {
                        dispString = dispString + '@';
                    }
                } else
                {
                    dispString = dispString + gridArr[y][x]
                }            
            }
            console.log(dispString);
        }
        console.log("======================");
    }
    console.log("Part2: %d", allMoved);    

}

function part1(gridArr) {
    var allMovable = 0;

    for (var y = 0; y < gridArr.length; y++) {
        dispString = "";
        for (var x = 0; x < gridArr[0].length; x++) {
            if (gridArr[y][x] == '@') {
                var border_count = 0;
                // Check the row above
                if (y > 0) {
                    if (x > 0 && gridArr[y - 1][x - 1] == "@") {
                        border_count++;
                    }
                    if (gridArr[y - 1][x] == "@") {
                        border_count++;
                    }
                    if (x < gridArr[0].length - 1 && gridArr[y - 1][x + 1] == "@") {
                        border_count++;
                    }
                }
                // Check left and right
                if (x > 0 && gridArr[y][x - 1] == "@") {
                    border_count++;
                }
                if (x < gridArr[0].length - 1 && gridArr[y][x + 1] == "@") {
                    border_count++;
                }           
                // Check the row below
                if (y < gridArr.length - 1) {
                    if (x > 0 && gridArr[y + 1][x - 1] == "@") {
                        border_count++;
                    }
                    if (gridArr[y + 1][x] == "@") {
                        border_count++;
                    }
                    if (x < gridArr[0].length - 1 && gridArr[y + 1][x + 1] == "@") {
                        border_count++;
                    }
                }

                if (border_count < 4) {
                    allMovable++;
                    dispString = dispString + 'x';
                } else {
                    dispString = dispString + '@';
                }
            } else
            {
                dispString = dispString + gridArr[y][x]
            }            
        }
        console.log(dispString);
    }
    console.log("Part1: %d", allMovable);
}


fs.readFile('inputs/day4.txt', 'utf8', (err, fileData) => {
    if (err) {
      console.error(err);
      return;
    }
    processFileData(fileData);
  });