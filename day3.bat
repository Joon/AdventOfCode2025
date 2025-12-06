del day3data.db
sqlite3 day3data.db ".import inputs/day3.csv batteries"
echo "Answer Part 1:"
sqlite3 day3data.db < day3_part1.sql
echo "Answer Part 2:"
sqlite3 day3data.db < day3_part2.sql