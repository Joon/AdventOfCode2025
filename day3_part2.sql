WITH zero_to_nine AS (SELECT 0 as seq UNION SELECT 1 UNION SELECT 2 UNION SELECT 3 UNION SELECT 4 UNION SELECT 5 UNION SELECT 6 UNION SELECT 7 UNION SELECT 8 UNION SELECT 9), 
      one_to_ninety_nine AS (SELECT ones.seq + (tens.seq * 10) + (hundreds.seq * 100) AS num 
                              FROM zero_to_nine as ones 
                              CROSS JOIN zero_to_nine as tens 
                              CROSS JOIN zero_to_nine as hundreds 
                              WHERE  ones.seq + (tens.seq * 10) + (hundreds.seq * 100) > 0
                              AND    ones.seq + (tens.seq * 10) + (hundreds.seq * 100) < 110
                              ORDER BY num), 
      bank_batteries AS (SELECT bat.ROWID as bank_id, counter.num AS battery_seq, 
                                substr(banks, counter.num, 1) AS battery, bat.banks 
                         FROM batteries bat 
                         INNER JOIN one_to_ninety_nine counter 
                         ON counter.num <= length(bat.banks)),
      largest_char_1 AS (SELECT bank_id, MAX(battery) AS battery
                         FROM bank_batteries
                         WHERE battery_seq <= (100 - 11)
                         GROUP BY bank_id),
      char_1 AS (SELECT c1.bank_id, c1.battery, MIN(bats.battery_seq) battery_seq
                 FROM   largest_char_1 c1
                        INNER JOIN bank_batteries bats 
                        ON c1.bank_id = bats.bank_id
                        AND bats.battery = c1.battery
                 GROUP BY c1.bank_id),
      largest_char_2 AS (SELECT bat.bank_id, c1.battery_seq AS prev_seq, 
                                MAX(bat.battery) AS battery
                         FROM bank_batteries bat
                              INNER JOIN char_1 AS c1
                              ON c1.bank_id = bat.bank_id
                              AND bat.battery_seq > c1.battery_seq
                         WHERE bat.battery_seq <= (100 - 10)                         
                         GROUP BY bat.bank_id, c1.battery_seq),
      char_2 AS (SELECT c2.bank_id, c2.battery, MIN(bats.battery_seq) battery_seq
                 FROM largest_char_2 c2
                      INNER JOIN bank_batteries bats
                      ON c2.bank_id = bats.bank_id
                      AND bats.battery = c2.battery
                      AND bats.battery_seq > c2.prev_seq
                 GROUP BY c2.bank_id, c2.battery),
      largest_char_3 AS (SELECT bat.bank_id, c2.battery_seq AS prev_seq, 
                                MAX(bat.battery) AS battery
                         FROM bank_batteries bat
                              INNER JOIN char_2 AS c2
                              ON c2.bank_id = bat.bank_id
                              AND bat.battery_seq > c2.battery_seq
                         WHERE bat.battery_seq <= (100 - 9)                         
                         GROUP BY bat.bank_id, c2.battery_seq),
      char_3 AS (SELECT c3.bank_id, c3.battery, MIN(bats.battery_seq) battery_seq
                 FROM largest_char_3 c3
                      INNER JOIN bank_batteries bats
                      ON c3.bank_id = bats.bank_id
                      AND bats.battery = c3.battery
                      AND bats.battery_seq > c3.prev_seq
                 GROUP BY c3.bank_id, c3.battery),
      largest_char_4 AS (SELECT bat.bank_id, c3.battery_seq AS prev_seq, 
                                MAX(bat.battery) AS battery
                         FROM bank_batteries bat
                              INNER JOIN char_3 AS c3
                              ON c3.bank_id = bat.bank_id
                              AND bat.battery_seq > c3.battery_seq
                         WHERE bat.battery_seq <= (100 - 8)                         
                         GROUP BY bat.bank_id, c3.battery_seq),
      char_4 AS (SELECT c4.bank_id, c4.battery, MIN(bats.battery_seq) battery_seq
                 FROM largest_char_4 c4
                      INNER JOIN bank_batteries bats
                      ON c4.bank_id = bats.bank_id
                      AND bats.battery = c4.battery
                      AND bats.battery_seq > c4.prev_seq
                 GROUP BY c4.bank_id, c4.battery),
      largest_char_5 AS (SELECT bat.bank_id, c4.battery_seq AS prev_seq, 
                                MAX(bat.battery) AS battery
                         FROM bank_batteries bat
                              INNER JOIN char_4 AS c4
                              ON c4.bank_id = bat.bank_id
                              AND bat.battery_seq > c4.battery_seq
                         WHERE bat.battery_seq <= (100 - 7)                         
                         GROUP BY bat.bank_id, c4.battery_seq),
      char_5 AS (SELECT c5.bank_id, c5.battery, MIN(bats.battery_seq) battery_seq
                 FROM largest_char_5 c5
                      INNER JOIN bank_batteries bats
                      ON c5.bank_id = bats.bank_id
                      AND bats.battery = c5.battery
                      AND bats.battery_seq > c5.prev_seq
                 GROUP BY c5.bank_id, c5.battery),
      largest_char_6 AS (SELECT bat.bank_id, c5.battery_seq AS prev_seq, 
                                MAX(bat.battery) AS battery
                         FROM bank_batteries bat
                              INNER JOIN char_5 AS c5
                              ON c5.bank_id = bat.bank_id
                              AND bat.battery_seq > c5.battery_seq
                         WHERE bat.battery_seq <= (100 - 6)                         
                         GROUP BY bat.bank_id, c5.battery_seq),
      char_6 AS (SELECT c6.bank_id, c6.battery, MIN(bats.battery_seq) battery_seq
                 FROM largest_char_6 c6
                      INNER JOIN bank_batteries bats
                      ON c6.bank_id = bats.bank_id
                      AND bats.battery = c6.battery
                      AND bats.battery_seq > c6.prev_seq
                 GROUP BY c6.bank_id, c6.battery),
      largest_char_7 AS (SELECT bat.bank_id, c6.battery_seq AS prev_seq, 
                                MAX(bat.battery) AS battery
                         FROM bank_batteries bat
                              INNER JOIN char_6 AS c6
                              ON c6.bank_id = bat.bank_id
                              AND bat.battery_seq > c6.battery_seq
                         WHERE bat.battery_seq <= (100 - 5)                         
                         GROUP BY bat.bank_id, c6.battery_seq),
      char_7 AS (SELECT c7.bank_id, c7.battery, MIN(bats.battery_seq) battery_seq
                 FROM largest_char_7 c7
                      INNER JOIN bank_batteries bats
                      ON c7.bank_id = bats.bank_id
                      AND bats.battery = c7.battery
                      AND bats.battery_seq > c7.prev_seq
                 GROUP BY c7.bank_id, c7.battery),
      largest_char_8 AS (SELECT bat.bank_id, c7.battery_seq AS prev_seq, 
                                MAX(bat.battery) AS battery
                         FROM bank_batteries bat
                              INNER JOIN char_7 AS c7
                              ON c7.bank_id = bat.bank_id
                              AND bat.battery_seq > c7.battery_seq
                         WHERE bat.battery_seq <= (100 - 4)                         
                         GROUP BY bat.bank_id, c7.battery_seq),
      char_8 AS (SELECT c8.bank_id, c8.battery, MIN(bats.battery_seq) battery_seq
                 FROM largest_char_8 c8
                      INNER JOIN bank_batteries bats
                      ON c8.bank_id = bats.bank_id
                      AND bats.battery = c8.battery
                      AND bats.battery_seq > c8.prev_seq
                 GROUP BY c8.bank_id, c8.battery),
      largest_char_9 AS (SELECT bat.bank_id, c8.battery_seq AS prev_seq, 
                                MAX(bat.battery) AS battery
                         FROM bank_batteries bat
                              INNER JOIN char_8 AS c8
                              ON c8.bank_id = bat.bank_id
                              AND bat.battery_seq > c8.battery_seq
                         WHERE bat.battery_seq <= (100 - 3)                         
                         GROUP BY bat.bank_id, c8.battery_seq),
      char_9 AS (SELECT c9.bank_id, c9.battery, MIN(bats.battery_seq) battery_seq
                 FROM largest_char_9 c9
                      INNER JOIN bank_batteries bats
                      ON c9.bank_id = bats.bank_id
                      AND bats.battery = c9.battery
                      AND bats.battery_seq > c9.prev_seq
                 GROUP BY c9.bank_id, c9.battery),
      largest_char_10 AS (SELECT bat.bank_id, c9.battery_seq AS prev_seq, 
                                MAX(bat.battery) AS battery
                         FROM bank_batteries bat
                              INNER JOIN char_9 AS c9
                              ON c9.bank_id = bat.bank_id
                              AND bat.battery_seq > c9.battery_seq
                         WHERE bat.battery_seq <= (100 - 2)                         
                         GROUP BY bat.bank_id, c9.battery_seq),
      char_10 AS (SELECT c10.bank_id, c10.battery, MIN(bats.battery_seq) battery_seq
                 FROM largest_char_10 c10
                      INNER JOIN bank_batteries bats
                      ON c10.bank_id = bats.bank_id
                      AND bats.battery = c10.battery
                      AND bats.battery_seq > c10.prev_seq
                 GROUP BY c10.bank_id, c10.battery),
      largest_char_11 AS (SELECT bat.bank_id, c10.battery_seq AS prev_seq, 
                                MAX(bat.battery) AS battery
                         FROM bank_batteries bat
                              INNER JOIN char_10 AS c10
                              ON c10.bank_id = bat.bank_id
                              AND bat.battery_seq > c10.battery_seq
                         WHERE bat.battery_seq <= (100 - 1)                         
                         GROUP BY bat.bank_id, c10.battery_seq),
      char_11 AS (SELECT c11.bank_id, c11.battery, MIN(bats.battery_seq) battery_seq
                 FROM largest_char_11 c11
                      INNER JOIN bank_batteries bats
                      ON c11.bank_id = bats.bank_id
                      AND bats.battery = c11.battery
                      AND bats.battery_seq > c11.prev_seq
                 GROUP BY c11.bank_id, c11.battery),
      largest_char_12 AS (SELECT bat.bank_id, c11.battery_seq AS prev_seq, 
                                MAX(bat.battery) AS battery
                         FROM bank_batteries bat
                              INNER JOIN char_11 AS c11
                              ON c11.bank_id = bat.bank_id
                              AND bat.battery_seq > c11.battery_seq
                         GROUP BY bat.bank_id, c11.battery_seq),
      char_12 AS (SELECT c12.bank_id, c12.battery, MIN(bats.battery_seq) battery_seq
                 FROM largest_char_12 c12
                      INNER JOIN bank_batteries bats
                      ON c12.bank_id = bats.bank_id
                      AND bats.battery = c12.battery
                      AND bats.battery_seq > c12.prev_seq
                 GROUP BY c12.bank_id, c12.battery),
      bank_batteries_combined AS (SELECT c1.bank_id, c1.battery battery1, c2.battery battery2,
                                         c3.battery battery3, c4.battery battery4, 
                                         c5.battery battery5, c6.battery battery6,
                                         c7.battery battery7, c8.battery battery8,
                                         c9.battery battery9, c10.battery battery10,
                                         c11.battery battery11, c12.battery battery12
                                  FROM char_1 c1
                                       INNER JOIN char_2 c2
                                       ON c2.bank_id = c1.bank_id
                                       INNER JOIN char_3 c3
                                       ON c3.bank_id = c1.bank_id
                                       INNER JOIN char_4 c4
                                       ON c4.bank_id = c1.bank_id
                                       INNER JOIN char_5 c5
                                       ON c5.bank_id = c1.bank_id
                                       INNER JOIN char_6 c6
                                       ON c6.bank_id = c1.bank_id
                                       INNER JOIN char_7 c7
                                       ON c7.bank_id = c1.bank_id
                                       INNER JOIN char_8 c8
                                       ON c8.bank_id = c1.bank_id
                                       INNER JOIN char_9 c9
                                       ON c9.bank_id = c1.bank_id
                                       INNER JOIN char_10 c10
                                       ON c10.bank_id = c1.bank_id
                                       INNER JOIN char_11 c11
                                       ON c11.bank_id = c1.bank_id
                                       INNER JOIN char_12 c12
                                       ON c12.bank_id = c1.bank_id),
      largest_jolt_value AS (SELECT bank_id, (battery1 || battery2 || battery3 || battery4
                                    || battery5 || battery6 || battery7 || battery8 || battery9 
                                    || battery10 || battery11 || battery12) jolts 
                             FROM   bank_batteries_combined 
                             GROUP BY bank_id) 
 SELECT SUM(jolts) 
 FROM largest_jolt_value
 